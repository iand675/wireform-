{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.GlobalStore
Description : Global stores: bootstrapped before processing, then kept current

The JVM's @GlobalStreamThread@. Every instance holds a full copy of
each global store (a 'Kafka.Streams.GlobalKTable', or a store added
with 'Kafka.Streams.Topology.addGlobalStore'): a group-less reader
consumes every partition of the global topics.

On start the stores are /bootstrapped/: read from the checkpoint
(persistent stores) or the log start up to the end offsets, before
any task processes a record, so a join never misses a key that was
already in the topic. Then a background thread keeps applying new
records. Tasks see the stores read-only, through their engines'
shared stores.
-}
module Kafka.Streams.Runtime.GlobalStore (
  GlobalStore,
  startGlobalStore,
  globalStores,
  globalStoreFailure,
  stopGlobalStore,
) where

import Control.Concurrent.Async (Async, async, waitCatch)
import Kafka.Streams.Internal.Log qualified as SLog
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Headers qualified as KH
import Kafka.Streams.Errors (DeserializationHandler)
import Kafka.Streams.Internal.Engine (
  Engine,
  EngineOptions (..),
  StoreEntry,
  buildEngineWith,
  closeEngine,
  commitEngine,
  defaultEngineOptions,
  engineOwnStores,
  feedSourceH,
 )
import Kafka.Streams.Internal.RecordCollector (inMemoryCollector)
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Driver
import Kafka.Streams.Runtime.Plan (GlobalPlan (..))
import Kafka.Streams.Runtime.StateDir (readCheckpoint, writeCheckpoint)
import Kafka.Streams.State.Store (StoreName, unStoreName)
import Kafka.Streams.Time (Timestamp (..), nowMillis)
import Kafka.Streams.Types (topicName)
import System.FilePath ((</>))


data GlobalStore = GlobalStore
  { gsEngine :: !Engine
  , gsReader :: !Reader
  , gsPartitions :: ![KC.TopicPartition]
  , gsDir :: !FilePath
  , gsStop :: !(TVar Bool)
  , gsFailure :: !(TVar (Maybe Text))
  , gsThread :: !(IORef (Maybe (Async ())))
  }


{- | Build the global stores, bootstrap them, and start the thread
that keeps them current. @listener@ hears every bootstrapped batch
(store names, offset reached).
-}
startGlobalStore
  :: StreamDriver
  -> GlobalPlan
  -> Map Text Int
  -- ^ partition counts
  -> Text
  -- ^ application id
  -> FilePath
  -- ^ the application's state directory
  -> DeserializationHandler
  -> Int
  -- ^ poll timeout (ms)
  -> Int
  -- ^ commit interval (ms): how often stores are flushed and checkpointed
  -> (Text -> Int64 -> IO ())
  -> IO (Either String GlobalStore)
startGlobalStore drv plan counts appId appDir deser pollTimeout commitMs listener = do
  let dir = appDir </> "global"
      tps =
        concatMap
          (\t -> map (KC.TopicPartition t . fromIntegral) [0 .. Map.findWithDefault 1 t counts - 1])
          (globalPlanTopics plan)
      storeNames = T.intercalate "," (map unStoreName (globalPlanStores plan))
  collector <- inMemoryCollector
  engine <-
    buildEngineWith
      defaultEngineOptions {eoTaskDirectory = Just dir}
      (globalPlanTopology plan)
      (TaskId (-1) 0)
      appId
      collector
      deser
  eReader <- sdNewReader drv
  case eReader of
    Left e -> pure (Left ("global reader: " <> e))
    Right rd -> do
      saved <- readCheckpoint dir
      let start tp = Map.findWithDefault (-2) (tp.topic, tp.partition) saved
      eAssign <- rdAssign rd (map (\tp -> (tp, start tp)) tps)
      eEnds <- rdEndOffsets rd tps
      case (eAssign, eEnds) of
        (Left e, _) -> pure (Left ("global assign: " <> e))
        (_, Left e) -> pure (Left ("global end offsets: " <> e))
        (Right (), Right ends) -> do
          stop <- newTVarIO False
          failure <- newTVarIO Nothing
          thread <- newIORef Nothing
          let gs =
                GlobalStore
                  { gsEngine = engine
                  , gsReader = rd
                  , gsPartitions = tps
                  , gsDir = dir
                  , gsStop = stop
                  , gsFailure = failure
                  , gsThread = thread
                  }
          boot <- try (bootstrap gs ends storeNames)
          case boot of
            Left e -> pure (Left ("global bootstrap: " <> show (e :: SomeException)))
            Right (Left e) -> pure (Left ("global bootstrap: " <> e))
            Right (Right ()) -> do
              commitEngine engine
              saveCheckpoint gs
              a <- async (keepCurrent gs pollTimeout commitMs)
              writeIORef thread (Just a)
              pure (Right gs)
  where
    bootstrap gs ends storeNames = do
      let loop = do
            positions <- mapM (\tp -> (,) tp <$> rdPosition (gsReader gs) tp) (gsPartitions gs)
            let behind = filter (\(tp, p) -> either (const True) (< Map.findWithDefault 0 tp ends) p) positions
            if null behind
              then pure (Right ())
              else do
                eRecs <- rdPoll (gsReader gs) pollTimeout
                case eRecs of
                  Left e -> pure (Left e)
                  Right recs -> do
                    mapM_ (apply gs) recs
                    unless (null recs) $ listener storeNames (maximum (map drOffset recs))
                    loop
      loop


apply :: GlobalStore -> DriverRecord -> IO ()
apply gs r =
  feedSourceH
    (gsEngine gs)
    (topicName r.drTopic)
    r.drKey
    r.drValue
    (KH.fromList r.drHeaders)
    (Timestamp r.drTimestamp)
    (fromIntegral r.drPartition)
    r.drOffset


keepCurrent :: GlobalStore -> Int -> Int -> IO ()
keepCurrent gs pollTimeout commitMs = do
  lastCommit <- nowMillis >>= newIORef
  let loop = do
        stopping <- readTVarIO (gsStop gs)
        unless stopping $ do
          eRecs <- rdPoll (gsReader gs) (min pollTimeout 100)
          case eRecs of
            Left e -> atomically (writeTVar (gsFailure gs) (Just (T.pack e)))
            Right recs -> do
              res <- try (mapM_ (apply gs) recs)
              case res of
                Left e -> atomically (writeTVar (gsFailure gs) (Just (T.pack (show (e :: SomeException)))))
                Right () -> do
                  now <- nowMillis
                  before <- readIORef lastCommit
                  when (now - before >= fromIntegral commitMs) $ do
                    writeIORef lastCommit now
                    commitEngine (gsEngine gs)
                    saveCheckpoint gs
                  loop
  loop


saveCheckpoint :: GlobalStore -> IO ()
saveCheckpoint gs = do
  positions <- mapM (\tp -> (,) tp <$> rdPosition (gsReader gs) tp) (gsPartitions gs)
  writeCheckpoint
    (gsDir gs)
    (Map.fromList (concatMap (\(tp, p) -> either (const []) (\off -> [((tp.topic, tp.partition), off)]) p) positions))


-- | The global stores, for the tasks' engines.
globalStores :: GlobalStore -> IO (Map StoreName StoreEntry)
globalStores = engineOwnStores . gsEngine


-- | Set once the background thread has failed.
globalStoreFailure :: GlobalStore -> IO (Maybe Text)
globalStoreFailure = readTVarIO . gsFailure


-- | Stop the thread, flush, checkpoint and close.
stopGlobalStore :: GlobalStore -> IO ()
stopGlobalStore gs = do
  atomically (writeTVar (gsStop gs) True)
  ma <- readIORef (gsThread gs)
  forM_ ma waitCatch
  failed <- readTVarIO (gsFailure gs)
  r <- try $ do
    commitEngine (gsEngine gs)
    -- a failed thread may have applied part of a batch: don't certify it
    maybe (saveCheckpoint gs) (const (pure ())) failed
    closeEngine (gsEngine gs)
  either (\e -> SLog.logWarn ("closing global stores: " <> show (e :: SomeException))) pure r
  rdClose (gsReader gs)
