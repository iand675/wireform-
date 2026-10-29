{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.Tasks
Description : Active tasks: creation, restore, processing, commit, recovery

The JVM's @TaskManager@ + @StreamTask@ + @StoreChangelogReader@ for one
streams instance.

A /task/ is one subtopology on one partition number: its own engine
(processors, stores, stream time, punctuators), fed the records of
partition @p@ of every source topic of its subtopology, writing its
stores' changes to partition @p@ of their changelogs.

Lifecycle:

  1. /Assigned/ (the main consumer got one of its partitions): the
     engine is built; its source partitions are paused while every
     logged store is /restored/ from its changelog, from the task's
     checkpoint (persistent stores) or the log start, to the end
     offset (the last stable offset under exactly-once, so aborted
     writes never come back; for a store that reuses its source topic,
     the group's committed offset).
  2. /Running/: polled records are buffered per partition and
     processed in timestamp order across the task's partitions (so a
     join sees both sides interleaved by time), all tasks in parallel
     over @num.stream.threads@ workers between polls.
  3. /Commit/ (every @commit.interval.ms@, when a processor asks, before
     a rebalance and at close): each engine flushes its stores (and
     caches) and the producer is flushed. At least once: then the
     processed offsets are committed and persistent stores
     checkpointed. Exactly once: output, changelog writes and the
     offsets go out in one transaction.
  4. /Recovery/ (an aborted transaction, or a stream-thread failure the
     uncaught-exception handler answers with "replace thread"): every
     task is closed dirty, its state directory wiped, and it is rebuilt
     and restored from the changelogs while its partitions are rewound
     to the last committed offsets.
  5. /Revoked/: committed (before the rebalance), closed and
     checkpointed. /Lost/: closed dirty.

Standbys ('tmSetStandbys', from the assignment): the task's engine
built with its stores only, kept current from the changelogs by the
same reader that restores (after each commit their persistent stores
are checkpointed). A standby that becomes active is not rebuilt: its
processors are wired onto it ('activateEngine') and it restores only
what it hasn't applied yet.
-}
module Kafka.Streams.Runtime.Tasks (
  TaskManager,
  TaskManagerConfig (..),
  newTaskManager,
  tmOnAssigned,
  tmOnRevoked,
  tmOnLost,
  tmRoute,
  tmRestoreStep,
  tmRestoring,
  tmProcess,
  CommitResult (..),
  tmCommit,
  InstanceCounts (..),
  tmCounts,
  tmRecoverAll,
  tmCloseAll,
  tmSetPaused,
  tmEngines,
  tmSetStandbys,
  tmStandbyTasks,
  tmStandbyEngines,
  tmOffsetSums,
  StateLag (..),
  tmStateLags,
  tmTaskSnapshot,
  TaskSnapshot (..),
  tmTakeCommitRequest,
  tmSetThreads,
  tmThreads,
  tmAssigned,
  tmPlanOf,
  tmCarryOver,

  -- * Restore listener
  StateRestoreListener (..),
  defaultStateRestoreListener,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.Exception (
  SomeAsyncException,
  SomeException,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Data.Either (lefts)
import Data.IORef
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Internal.Text (decimalText)
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed.Mutable qualified as UMV
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP
import Kafka.Headers qualified as KH
import Kafka.Streams.Config (ProcessingGuarantee (..))
import Kafka.Streams.Consumed (AutoOffsetReset (..))
import Kafka.Streams.Errors (
  DeserializationHandler,
  ProcessingException (..),
  ProcessingExceptionHandler (..),
  ProcessingResponse (..),
  ProductionException (..),
  ProductionHandler (..),
  ProductionResponse (..),
  StreamsException (..),
 )
import Kafka.Streams.Internal.Engine (
  ChangelogTarget (..),
  Engine,
  EngineOptions (..),
  StoreEntry (..),
  activateEngine,
  buildEngineWith,
  closeEngine,
  commitEngine,
  defaultEngineOptions,
  engineOwnStores,
  feedSourceH,
  setPartitionCountLookup,
  syncWallClockToReal,
  takeCommitRequested,
 )
import Kafka.Streams.Internal.RecordCollector (
  CollectedRecord (..),
  RecordCollector (..),
 )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Commit
import Kafka.Streams.Runtime.Driver
import Kafka.Streams.Runtime.GroupAssignor (runningActive)
import Kafka.Streams.Runtime.Plan
import Kafka.Streams.Runtime.StateDir (readCheckpoint, writeCheckpoint)
import Kafka.Streams.State.Store (StateStore (..), AnyStateStore (..), KeyValueStore (..), SessionStore (..), StoreName, WindowStore (..), unStoreName)
import Kafka.Streams.Time (Timestamp (..), nowMillis)
import Kafka.Streams.Topology.Subtopology (ChangelogTopic (..))
import Kafka.Streams.Types (topicName, unTopicName)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)


----------------------------------------------------------------------
-- Restore listener
----------------------------------------------------------------------

{- | The JVM's @StateRestoreListener@: told when a store starts
restoring a changelog partition (with the start and end offsets),
after each batch (with its size), and when it is done.
'onRestoreSuspended' fires when the task is closed before finishing.
-}
data StateRestoreListener = StateRestoreListener
  { onRestoreStart :: !(KC.TopicPartition -> Text -> Int64 -> Int64 -> IO ())
  , onBatchRestored :: !(KC.TopicPartition -> Text -> Int64 -> Int64 -> IO ())
  -- ^ partition, store, last restored offset, records in the batch
  , onRestoreEnd :: !(KC.TopicPartition -> Text -> Int64 -> IO ())
  -- ^ partition, store, total records restored
  , onRestoreSuspended :: !(KC.TopicPartition -> Text -> Int64 -> IO ())
  }


defaultStateRestoreListener :: StateRestoreListener
defaultStateRestoreListener =
  StateRestoreListener
    { onRestoreStart = \_ _ _ _ -> pure ()
    , onBatchRestored = \_ _ _ _ -> pure ()
    , onRestoreEnd = \_ _ _ -> pure ()
    , onRestoreSuspended = \_ _ _ -> pure ()
    }


----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

data TaskManagerConfig = TaskManagerConfig
  { tmcAppId :: !Text
  , tmcAppDir :: !FilePath
  -- ^ @\<state.dir\>/\<application.id\>@
  , tmcGuarantee :: !ProcessingGuarantee
  , tmcThreads :: !Int
  , tmcMaxTaskIdleMs :: !Int
  , tmcMaxBufferedPerPartition :: !Int
  , tmcDeserHandler :: !DeserializationHandler
  , tmcProcessingHandler :: !(IO ProcessingExceptionHandler)
  , tmcProductionHandler :: !(IO ProductionHandler)
  , tmcRestoreListener :: !(IO StateRestoreListener)
  , tmcSharedStores :: !(Map StoreName StoreEntry)
  -- ^ The global stores, visible to every task
  , tmcParticipants :: ![CommitParticipant]
  -- ^ External systems committing with every cycle ("Kafka.Streams.Runtime.Commit")
  , tmcInstanceId :: !Text
  -- ^ Unique per run: names this instance's commit cycles
  }


data TaskState = Restoring | Running
  deriving stock (Eq, Show)


data Task = Task
  { taskId :: !TaskId
  , taskSub :: !SubPlan
  , taskDir :: !FilePath
  , taskWorker :: !Int
  , taskPartitions :: ![KC.TopicPartition]
  , taskEngine :: !(IORef Engine)
  , taskQueues :: !(IORef (Map KC.TopicPartition (Seq DriverRecord)))
  , taskConsumed :: !(IORef (Map KC.TopicPartition Int64))
  -- ^ Next offset to read, per partition, after what was processed
  , taskCommitted :: !(IORef (Map KC.TopicPartition Int64))
  , taskState :: !(IORef TaskState)
  , taskChangelogPos :: !(IORef (Map KC.TopicPartition Int64))
  {- ^ Per changelog partition, the offset the stores hold everything
  before: the restore end, then one past each acknowledged write
  -}
  , taskEmptySince :: !(IORef (Map KC.TopicPartition Int64))
  , taskProcessed :: !(IORef Int64)
  }


data RestoreTarget = RestoreTarget
  { rtTask :: !TaskId
  , rtStore :: !StoreName
  , rtApply :: !(Maybe (KC.TopicPartition -> DriverRecord -> IO (Either Text ())))
  , rtNext :: !Int64
  , rtEnd :: !Int64
  , rtRestored :: !Int64
  }


-- | A standby: a task's stores, kept current from its changelogs.
data Standby = Standby
  { sbTask :: !TaskId
  , sbDir :: !FilePath
  , sbEngine :: !Engine
  , sbPos :: !(IORef (Map KC.TopicPartition Int64))
  -- ^ The next changelog offset to apply, per partition (@-2@: the log start)
  , sbFeeds :: !(Map KC.TopicPartition (StoreName, Maybe (KC.TopicPartition -> DriverRecord -> IO (Either Text ()))))
  }


-- | Where a rebuilt task's restore starts.
data RestoreFrom
  = -- | Its checkpoint (persistent stores), else the log start
    FromCheckpoint
  | -- | The log start: the local state was wiped
    FromScratch
  | -- | Where its standby got to: the stores already hold everything before
    FromPositions !(Map KC.TopicPartition Int64)


data TaskManager = TaskManager
  { tmCfg :: !TaskManagerConfig
  , tmDriver :: !StreamDriver
  , tmPlan :: !RuntimePlan
  , tmReader :: !Reader
  , tmTasks :: !(IORef (Map TaskId Task))
  , tmAssignedRef :: !(IORef (Set KC.TopicPartition))
  , tmRestore :: !(IORef (Map KC.TopicPartition RestoreTarget))
  , tmReaderAssigned :: !(IORef (Set KC.TopicPartition))
  , tmSendErrors :: !(IORef [(Text, String)])
  , tmTxnOpen :: !(MVar Bool)
  , tmCommitRequest :: !(IORef Bool)
  , tmPaused :: !(IORef Bool)
  , tmBackpressured :: !(IORef (Set KC.TopicPartition))
  , tmLastPurge :: !(IORef Int64)
  , tmThreadsRef :: !(IORef Int)
  , tmCycle :: !(IORef Int64)
  , tmStandbys :: !(IORef (Map TaskId Standby))
  , tmCounters :: !(UMV.IOVector Int)
  -- ^ 'InstanceCounts', written by the stream loop's thread
  }


newTaskManager :: TaskManagerConfig -> StreamDriver -> RuntimePlan -> IO (Either String TaskManager)
newTaskManager cfg drv plan = do
  eReader <- sdNewReader drv
  case eReader of
    Left e -> pure (Left e)
    Right rd -> do
      tasks <- newIORef Map.empty
      asg <- newIORef Set.empty
      rs <- newIORef Map.empty
      ra <- newIORef Set.empty
      errs <- newIORef []
      txn <- newMVar False
      req <- newIORef False
      paused <- newIORef False
      bp <- newIORef Set.empty
      now <- nowMillis
      purge <- newIORef now
      threads <- newIORef (max 1 (tmcThreads cfg))
      cycleRef <- newIORef 0
      counters <- UMV.replicate countersSize 0
      standbys <- newIORef Map.empty
      pure . Right $
        TaskManager
          { tmCfg = cfg
          , tmDriver = drv
          , tmPlan = plan
          , tmReader = rd
          , tmTasks = tasks
          , tmAssignedRef = asg
          , tmRestore = rs
          , tmReaderAssigned = ra
          , tmSendErrors = errs
          , tmTxnOpen = txn
          , tmCommitRequest = req
          , tmPaused = paused
          , tmBackpressured = bp
          , tmLastPurge = purge
          , tmThreadsRef = threads
          , tmCycle = cycleRef
          , tmCounters = counters
          , tmStandbys = standbys
          }


eos :: TaskManager -> Bool
eos tm = tmcGuarantee (tmCfg tm) == ExactlyOnceV2


logLine :: String -> IO ()
logLine = hPutStrLn stderr . ("[streams] " <>)


taskName :: TaskId -> Text
taskName (TaskId s p) = decimalText s <> "_" <> decimalText p


tmPlanOf :: TaskManager -> RuntimePlan
tmPlanOf = tmPlan


{- | Hand what outlives a re-plan from @old@ to @new@: the counts, and
the commit-cycle sequence (cycle ids must keep increasing for the
instance, or recovery would mistake a new cycle for a committed one).
-}
tmCarryOver :: TaskManager -> TaskManager -> IO ()
tmCarryOver old new = do
  UMV.copy (tmCounters new) (tmCounters old)
  readIORef (tmCycle old) >>= writeIORef (tmCycle new)


tmAssigned :: TaskManager -> IO (Set KC.TopicPartition)
tmAssigned = readIORef . tmAssignedRef


----------------------------------------------------------------------
-- Building tasks
----------------------------------------------------------------------

-- | Under exactly-once, open the transaction before the first send of a commit interval.
ensureTransaction :: TaskManager -> IO (Either String ())
ensureTransaction tm
  | not (eos tm) = pure (Right ())
  | otherwise = modifyMVar (tmTxnOpen tm) $ \case
      True -> pure (True, Right ())
      False -> do
        r <- sdBeginTransaction (tmDriver tm)
        pure (either (const False) (const True) r, r)


recordSendError :: TaskManager -> Text -> String -> IO ()
recordSendError tm t e = atomicModifyIORef' (tmSendErrors tm) (\es -> ((t, e) : es, ()))


taskCollector :: TaskManager -> IORef (Map KC.TopicPartition Int64) -> RecordCollector
taskCollector tm clPos =
  RecordCollector
    { collectorSend = \cr -> do
        let t = unTopicName (crTopic cr)
        began <- ensureTransaction tm
        case began of
          Left e -> recordSendError tm t ("begin transaction: " <> e)
          Right () -> do
            r <- sdSend (tmDriver tm) (toProducerRecord cr) $ \case
              Left e -> recordSendError tm t e
              Right md ->
                -- a changelog write: the store now holds everything before it
                when (isJust (crPartition cr)) $
                  atomicModifyIORef' clPos $ \m ->
                    let tp = KC.TopicPartition md.topic md.partition
                    in if Map.member tp m then (Map.insertWith max tp (md.offset + 1) m, ()) else (m, ())
            either (recordSendError tm t) pure r
    , collectorFlush = pure ()
    , collectorClose = pure ()
    , collectorPeek = pure Map.empty
    , collectorTake = \_ -> pure []
    }


{- | The wire record for a sink or changelog emission. A negative
timestamp (a source record that had none) is left for the producer
to stamp, as the broker rejects negative ones.
-}
toProducerRecord :: CollectedRecord -> KP.ProducerRecord
toProducerRecord cr =
  KP.ProducerRecord
    { KP.topic = unTopicName (crTopic cr)
    , KP.key = crKey cr
    , KP.value = crValue cr
    , KP.headers = KH.toList (crHeaders cr)
    , KP.partition = fromIntegral <$> crPartition cr
    , KP.timestamp = case crTimestamp cr of
        Timestamp t
          | t >= 0 -> Just t
          | otherwise -> Nothing
    }


buildTaskEngine :: TaskManager -> SubPlan -> TaskId -> FilePath -> IORef (Map KC.TopicPartition Int64) -> Bool -> IO Engine
buildTaskEngine tm sub tid dir clPos storesOnly = do
  createDirectoryIfMissing True dir
  let TaskId _ p = tid
      cfg = tmCfg tm
      opts =
        defaultEngineOptions
          { eoChangelog = \sn -> case Map.lookup sn (subPlanChangelogs sub) of
              Just cl | clInternal cl -> Just (ChangelogTarget (clTopic cl) (fromIntegral p))
              _ -> Nothing
          , eoTaskDirectory = Just dir
          , eoSharedStores = tmcSharedStores cfg
          , eoStoresOnly = storesOnly
          }
  e <- buildEngineWith opts (subPlanTopology sub) tid (tmcAppId cfg) (taskCollector tm clPos) (tmcDeserHandler cfg)
  setPartitionCountLookup e (sdPartitionsFor (tmDriver tm))
  pure e


-- | The store entry is persistent (its state survives a restart, so a checkpoint helps).
isPersistent :: StoreEntry -> Bool
isPersistent se = case storeEntryAny se of
  AnyKeyValueStore s -> storePersistent (kvsBase s)
  AnyWindowStore s -> storePersistent (wsBase s)
  AnySessionStore s -> storePersistent (ssBase s)


{- | Register the restores a freshly built task needs; 'True' if there
is any (its partitions then stay paused until they finish).
-}
scheduleRestore :: TaskManager -> Task -> RestoreFrom -> IO (Either String Bool)
scheduleRestore tm t from = do
  engine <- readIORef (taskEngine t)
  stores <- engineOwnStores engine
  saved <- case from of
    FromCheckpoint -> readCheckpoint (taskDir t)
    _ -> pure Map.empty
  let TaskId _ p = taskId t
      changelogs = Map.toList (subPlanChangelogs (taskSub t))
      clTp cl = KC.TopicPartition (unTopicName (clTopic cl)) p
      internalTps = map (clTp . snd) (filter (clInternal . snd) changelogs)
      sourceTps = map (clTp . snd) (filter (not . clInternal . snd) changelogs)
  eEnds <- rdEndOffsets (tmReader tm) internalTps
  eCommitted <- if null sourceTps then pure (Right Map.empty) else sdCommitted (tmDriver tm) sourceTps
  case (eEnds, eCommitted) of
    (Left e, _) -> pure (Left ("changelog end offsets: " <> e))
    (_, Left e) -> pure (Left ("committed offsets: " <> e))
    (Right ends, Right committed) -> do
      listener <- tmcRestoreListener (tmCfg tm)
      targets <- fmap concat . forM changelogs $ \(sn, cl) -> do
        let tp = clTp cl
            end
              | clInternal cl = Map.findWithDefault 0 tp ends
              | otherwise = Map.findWithDefault 0 tp committed
            se = Map.lookup sn stores
            -- @-2@: the log start (whatever survived retention / purging)
            checkpointed = case from of
              FromPositions pos -> case Map.lookup tp pos of
                Just off | off >= 0 -> Just off
                _ -> Nothing
              _ -> case Map.lookup (tp.topic, tp.partition) saved of
                Just off | maybe False isPersistent se -> Just off
                _ -> Nothing
            start = fromMaybe 0 checkpointed
        -- remember where the stores stand, for the next checkpoint
        atomicModifyIORef' (taskChangelogPos t) (\m -> (Map.insert tp (max start end) m, ()))
        if start >= end
          then pure []
          else do
            onRestoreStart listener tp (unStoreName sn) start end
            pure
              [ ( tp
                , RestoreTarget
                    { rtTask = taskId t
                    , rtStore = sn
                    , rtApply = (\apply _ r -> maybe (pure (Right ())) (\k -> apply k (drValue r)) (drKey r)) <$> (se >>= storeEntryRestore)
                    , rtNext = fromMaybe (-2) checkpointed
                    , rtEnd = end
                    , rtRestored = 0
                    }
                )
              ]
      atomicModifyIORef' (tmRestore tm) (\m -> (Map.union (Map.fromList targets) m, ()))
      pure (Right (not (null targets)))


{- | Tasks for newly assigned partitions: build, start restoring, and
position the partitions without a committed offset per their
source's offset-reset policy.
-}
tmOnAssigned :: TaskManager -> [KC.TopicPartition] -> IO (Either String ())
tmOnAssigned tm tps = do
  bump tm ixRebalances
  atomicModifyIORef' (tmAssignedRef tm) (\s -> (Set.union s (Set.fromList tps), ()))
  let plan = tmPlan tm
      wanted =
        Set.fromList
          ( concatMap
              (\tp -> map (`TaskId` tp.partition) (Map.findWithDefault [] tp.topic (rpTopicSubtopologies plan)))
              tps
          )
  existing <- readIORef (tmTasks tm)
  eCommitted <- sdCommitted (tmDriver tm) tps
  case eCommitted of
    Left e -> pure (Left ("committed offsets: " <> e))
    Right committed -> do
      -- Where to start the partitions that have never been committed.
      resetErrs <- fmap concat . forM tps $ \tp ->
        if Map.member tp committed
          then pure []
          else do
            case resetPolicy plan tp of
              OffsetEarliest -> pure []
              OffsetLatest -> either (\e -> [e]) (const []) <$> sdSeek (tmDriver tm) tp (-1)
              OffsetNone ->
                pure ["no committed offset for " <> T.unpack tp.topic <> "-" <> show tp.partition <> " and its offset reset is none"]
      if not (null resetErrs)
        then pure (Left (unwords resetErrs))
        else do
          results <- forM (Set.toList (Set.difference wanted (Map.keysSet existing))) $ \tid ->
            createTask tm tid committed
          pure (either Left (const (Right ())) (sequence_ results))


-- | Where a partition without a committed offset starts.
resetPolicy :: RuntimePlan -> KC.TopicPartition -> AutoOffsetReset
resetPolicy plan tp =
  foldr
    (\sid acc -> fromMaybe acc (Map.lookup sid (rpSubtopologies plan) >>= Map.lookup tp.topic . subPlanOffsetReset))
    OffsetEarliest
    (Map.findWithDefault [] tp.topic (rpTopicSubtopologies plan))


createTask :: TaskManager -> TaskId -> Map KC.TopicPartition Int64 -> IO (Either String ())
createTask tm tid@(TaskId sid _) committed =
  case Map.lookup sid (rpSubtopologies (tmPlan tm)) of
    Nothing -> pure (Right ())
    Just sub -> do
      let parts = taskSourcePartitions (tmPlan tm) tid
          dir = tmcAppDir (tmCfg tm) </> taskDirName tid
      tasks <- readIORef (tmTasks tm)
      threads <- readIORef (tmThreadsRef tm)
      let load w = length (filter ((== w) . (`mod` threads) . taskWorker) (Map.elems tasks))
          worker = snd (minimum (map (\w -> (load w, w)) [0 .. threads - 1]))
      standby <- atomicModifyIORef' (tmStandbys tm) (\m -> (Map.delete tid m, Map.lookup tid m))
      (engine, clPos, from) <- case standby of
        Just sb -> do
          -- a warm standby: keep its stores, wire up the processors
          activateEngine (sbEngine sb)
          pos <- readIORef (sbPos sb)
          pure (sbEngine sb, sbPos sb, FromPositions pos)
        Nothing -> do
          ref <- newIORef Map.empty
          e <- buildTaskEngine tm sub tid dir ref False
          pure (e, ref, FromCheckpoint)
      engineRef <- newIORef engine
      queues <- newIORef Map.empty
      consumed <- newIORef (Map.restrictKeys committed (Set.fromList parts))
      committedRef <- newIORef (Map.restrictKeys committed (Set.fromList parts))
      st <- newIORef Restoring
      empty <- newIORef Map.empty
      processed <- newIORef 0
      let t =
            Task
              { taskId = tid
              , taskSub = sub
              , taskDir = dir
              , taskWorker = worker
              , taskPartitions = parts
              , taskEngine = engineRef
              , taskQueues = queues
              , taskConsumed = consumed
              , taskCommitted = committedRef
              , taskState = st
              , taskChangelogPos = clPos
              , taskEmptySince = empty
              , taskProcessed = processed
              }
      atomicModifyIORef' (tmTasks tm) (\m -> (Map.insert tid t m, ()))
      eRestore <- scheduleRestore tm t from
      case eRestore of
        Left e -> pure (Left e)
        Right True -> Right <$> sdPause (tmDriver tm) parts
        Right False -> Right <$> writeIORef st Running


----------------------------------------------------------------------
-- Restore
----------------------------------------------------------------------

tmRestoring :: TaskManager -> IO Bool
tmRestoring tm = not . Map.null <$> readIORef (tmRestore tm)


{- | One batch of restoration (finished tasks start running), and of
keeping the standbys current.
-}
tmRestoreStep :: TaskManager -> IO (Either String ())
tmRestoreStep tm = do
  targets <- readIORef (tmRestore tm)
  standbys <- readIORef (tmStandbys tm)
  let feeds = Map.unions (map (\sb -> Map.map ((,) sb) (sbFeeds sb)) (Map.elems standbys))
      wanted = Set.union (Map.keysSet targets) (Map.keysSet feeds)
  if Set.null wanted
    then pure (Right ())
    else do
      assigned <- readIORef (tmReaderAssigned tm)
      reassign <-
        if wanted == assigned
          then pure (Right ())
          else do
            writeIORef (tmReaderAssigned tm) wanted
            feedPos <- forM (Map.toList feeds) $ \(tp, (sb, _)) -> (,) tp . Map.findWithDefault (-2) tp <$> readIORef (sbPos sb)
            rdAssign (tmReader tm) (map (\(tp, rt) -> (tp, rtNext rt)) (Map.toList targets) <> feedPos)
      case reassign of
        Left e -> pure (Left ("assign restore reader: " <> e))
        Right () -> do
          eRecs <- rdPoll (tmReader tm) 0
          case eRecs of
            Left e -> pure (Left ("restore poll: " <> e))
            Right recs -> feedStandbys tm feeds recs >>= either (pure . Left) (const (restoreBatch targets recs))
  where
    restoreBatch targets recs = do
              listener <- tmcRestoreListener (tmCfg tm)
              -- apply, in order, per partition
              applied <- foldM (applyOne targets) Map.empty recs
              case sequence applied of
                Left e -> pure (Left e)
                Right counts -> do
                  addCount tm ixRestored (fromIntegral (sum (map fst (Map.elems counts))))
                  forM_ (Map.toList counts) $ \(tp, (n, lastOff)) ->
                    forM_ (Map.lookup tp targets) $ \rt ->
                      onBatchRestored listener tp (unStoreName (rtStore rt)) lastOff n
                  finished <- fmap concat . forM (Map.toList targets) $ \(tp, rt) -> do
                    ePos <- rdPosition (tmReader tm) tp
                    let pos = either (const (rtNext rt)) id ePos
                        restored = rtRestored rt + maybe 0 fst (either (const Nothing) (Map.lookup tp) (sequence applied))
                    if pos >= rtEnd rt
                      then do
                        onRestoreEnd listener tp (unStoreName (rtStore rt)) restored
                        pure [tp]
                      else do
                        atomicModifyIORef' (tmRestore tm) (\m -> (Map.adjust (\r -> r {rtNext = pos, rtRestored = restored}) tp m, ()))
                        pure []
                  atomicModifyIORef' (tmRestore tm) (\m -> (foldr Map.delete m finished, ()))
                  promoteRestored tm
                  pure (Right ())
    applyOne targets acc r = do
      let tp = KC.TopicPartition r.drTopic r.drPartition
      case acc of
        _ | Left _ <- sequence acc -> pure acc
        _ -> case Map.lookup tp targets of
          Nothing -> pure acc
          Just rt -> case rtApply rt of
            Nothing -> pure (Map.insert tp (Left ("store " <> T.unpack (unStoreName (rtStore rt)) <> " cannot be restored")) acc)
            Just apply -> do
              res <- apply tp r
              pure $ case res of
                Left e -> Map.insert tp (Left ("restoring " <> T.unpack (unStoreName (rtStore rt)) <> ": " <> T.unpack e)) acc
                Right () ->
                  Map.insertWith
                    (\_ old -> fmap (\(n, _) -> (n + 1, r.drOffset)) old)
                    tp
                    (Right (1, r.drOffset))
                    acc


-- | Apply the standbys' changelog records, and move their positions on.
feedStandbys
  :: TaskManager
  -> Map KC.TopicPartition (Standby, (StoreName, Maybe (KC.TopicPartition -> DriverRecord -> IO (Either Text ()))))
  -> [DriverRecord]
  -> IO (Either String ())
feedStandbys tm feeds recs
  | Map.null feeds = pure (Right ())
  | otherwise = do
      let go [] = pure (Right ())
          go (r : rest) = case Map.lookup (KC.TopicPartition r.drTopic r.drPartition) feeds of
            Nothing -> go rest
            Just (sb, (sn, mApply)) -> case mApply of
              Nothing -> pure (Left ("standby " <> T.unpack (taskName (sbTask sb)) <> ": store " <> T.unpack (unStoreName sn) <> " cannot be restored"))
              Just apply -> do
                res <- apply (KC.TopicPartition r.drTopic r.drPartition) r
                case res of
                  Left e -> pure (Left ("standby " <> T.unpack (taskName (sbTask sb)) <> ": " <> T.unpack e))
                  Right () -> go rest
      applied <- go recs
      case applied of
        Left e -> pure (Left e)
        Right () -> do
          forM_ (Map.toList feeds) $ \(tp, (sb, _)) -> do
            ePos <- rdPosition (tmReader tm) tp
            either (const (pure ())) (\pos -> atomicModifyIORef' (sbPos sb) (\m -> (Map.insert tp pos m, ()))) ePos
          addCount tm ixStandbyRecords (length (filter (\r -> Map.member (KC.TopicPartition r.drTopic r.drPartition) feeds) recs))
          pure (Right ())


----------------------------------------------------------------------
-- Standbys
----------------------------------------------------------------------

{- | Hold standbys for exactly @wanted@ (less the tasks running here):
close the others (checkpointed), build the new ones from their
checkpoints.
-}
tmSetStandbys :: TaskManager -> Set TaskId -> IO ()
tmSetStandbys tm wanted0 = do
  active <- Map.keysSet <$> readIORef (tmTasks tm)
  existing <- readIORef (tmStandbys tm)
  let wanted = Set.difference wanted0 active
  forM_ (Map.elems (Map.withoutKeys existing wanted)) $ \sb -> do
    atomicModifyIORef' (tmStandbys tm) (\m -> (Map.delete (sbTask sb) m, ()))
    closeStandby tm sb
  forM_ (Set.toList (Set.difference wanted (Map.keysSet existing))) (createStandby tm)


tmStandbyTasks :: TaskManager -> IO (Set TaskId)
tmStandbyTasks tm = Map.keysSet <$> readIORef (tmStandbys tm)


tmStandbyEngines :: TaskManager -> IO [(TaskId, Engine)]
tmStandbyEngines tm = map (\sb -> (sbTask sb, sbEngine sb)) . Map.elems <$> readIORef (tmStandbys tm)


createStandby :: TaskManager -> TaskId -> IO ()
createStandby tm tid@(TaskId sid p) =
  case Map.lookup sid (rpSubtopologies (tmPlan tm)) of
    Just sub | not (Map.null (subPlanChangelogs sub)) -> do
      let dir = tmcAppDir (tmCfg tm) </> taskDirName tid
      pos <- newIORef Map.empty
      engine <- buildTaskEngine tm sub tid dir pos True
      stores <- engineOwnStores engine
      saved <- readCheckpoint dir
      let feeds =
            Map.fromList
              ( map
                  ( \(sn, cl) ->
                      ( KC.TopicPartition (unTopicName (clTopic cl)) p
                      , (sn, (\apply _ r -> maybe (pure (Right ())) (\k -> apply k (drValue r)) (drKey r)) <$> (Map.lookup sn stores >>= storeEntryRestore))
                      )
                  )
                  (Map.toList (subPlanChangelogs sub))
              )
          start (tp, (sn, _)) = case Map.lookup (tp.topic, tp.partition) saved of
            Just off | maybe False isPersistent (Map.lookup sn stores) -> (tp, off)
            _ -> (tp, -2)
      writeIORef pos (Map.fromList (map start (Map.toList feeds)))
      atomicModifyIORef' (tmStandbys tm) (\m -> (Map.insert tid (Standby tid dir engine pos feeds) m, ()))
    _ -> pure ()


-- | Flush the standby's stores and checkpoint its persistent ones.
checkpointStandby :: Standby -> IO ()
checkpointStandby sb = do
  stores <- engineOwnStores (sbEngine sb)
  forM_ (Map.elems stores) storeEntryFlush
  pos <- readIORef (sbPos sb)
  let persistentTps =
        Set.fromList
          ( map
              fst
              (filter (\(_, (sn, _)) -> maybe False isPersistent (Map.lookup sn stores)) (Map.toList (sbFeeds sb)))
          )
      offs = Map.filter (>= 0) (Map.restrictKeys pos persistentTps)
  unless (Map.null offs) $
    writeCheckpoint (sbDir sb) (Map.mapKeys (\tp -> (tp.topic, tp.partition)) offs)


closeStandby :: TaskManager -> Standby -> IO ()
closeStandby _tm sb = do
  r <- try (checkpointStandby sb >> closeEngine (sbEngine sb)) :: IO (Either SomeException ())
  either (\e -> logLine ("closing standby " <> T.unpack (taskName (sbTask sb)) <> ": " <> show e)) pure r


-- | A task's local state against its changelogs.
data StateLag = StateLag
  { slTask :: !TaskId
  , slStandby :: !Bool
  , slPosition :: !Int64
  -- ^ Summed changelog offsets the state covers (the end, for a running task)
  , slEnd :: !Int64
  }
  deriving stock (Eq, Show)


{- | Every task with state here (running, restoring or standby) against
the current end of its changelogs.
-}
tmStateLags :: TaskManager -> IO (Either String [StateLag])
tmStateLags tm = do
  tasks <- readIORef (tmTasks tm)
  restore <- readIORef (tmRestore tm)
  standbys <- readIORef (tmStandbys tm)
  let cls = planTaskChangelogs (tmPlan tm)
      clsOf tid = Map.findWithDefault [] tid cls
      wanted = Map.keys (Map.filter (not . null) (Map.restrictKeys cls (Set.union (Map.keysSet tasks) (Map.keysSet standbys))))
      allTps = concatMap clsOf wanted
      internal = map fst (filter (clInternal . snd) allTps)
      reused = map fst (filter (not . clInternal . snd) allTps)
  eEnds <- if null internal then pure (Right Map.empty) else rdEndOffsets (tmReader tm) internal
  eCommitted <- if null reused then pure (Right Map.empty) else sdCommitted (tmDriver tm) reused
  case (eEnds, eCommitted) of
    (Left e, _) -> pure (Left ("changelog end offsets: " <> e))
    (_, Left e) -> pure (Left ("committed offsets: " <> e))
    (Right ends, Right committed) -> do
      let endOf (tp, cl) = Map.findWithDefault 0 tp (if clInternal cl then ends else committed)
          endSum tid = sum (map endOf (clsOf tid))
      fmap Right . forM wanted $ \tid -> case (Map.lookup tid tasks, Map.lookup tid standbys) of
        (Just t, _) -> do
          st <- readIORef (taskState t)
          pos <- readIORef (taskChangelogPos t)
          let at (tp, _) = case Map.lookup tp restore of
                Just rt -> max 0 (rtNext rt)
                Nothing -> max 0 (Map.findWithDefault 0 tp pos)
              current = if st == Running then endSum tid else sum (map at (clsOf tid))
          pure (StateLag tid False current (endSum tid))
        (Nothing, Just sb) -> do
          pos <- readIORef (sbPos sb)
          pure (StateLag tid True (sum (map (\(tp, _) -> max 0 (Map.findWithDefault 0 tp pos)) (clsOf tid))) (endSum tid))
        _ -> pure (StateLag tid False 0 (endSum tid))


{- | For the assignor: per task this instance holds state for, the
summed changelog offsets the state covers, or 'runningActive' for a
running task. Tasks not open here count by their checkpoints.
-}
tmOffsetSums :: TaskManager -> IO (Map TaskId Int64)
tmOffsetSums tm = do
  tasks <- readIORef (tmTasks tm)
  restore <- readIORef (tmRestore tm)
  standbys <- readIORef (tmStandbys tm)
  let plan = tmPlan tm
      clTps tid = maybe [] (map fst) (Map.lookup tid (planTaskChangelogs plan))
  activeSums <- forM (Map.elems tasks) $ \t -> do
    st <- readIORef (taskState t)
    case st of
      Running -> pure (taskId t, runningActive)
      Restoring -> do
        pos <- readIORef (taskChangelogPos t)
        let at tp = case Map.lookup tp restore of
              Just rt -> max 0 (rtNext rt)
              Nothing -> max 0 (Map.findWithDefault 0 tp pos)
        pure (taskId t, sum (map at (clTps (taskId t))))
  standbySums <- forM (Map.elems standbys) $ \sb -> do
    pos <- readIORef (sbPos sb)
    pure (sbTask sb, sum (map (max 0) (Map.elems pos)))
  let live = Set.union (Map.keysSet tasks) (Map.keysSet standbys)
      onDisk = filter (`Set.notMember` live) (Map.keys (planTaskChangelogs plan))
  diskSums <- forM onDisk $ \tid -> do
    saved <- readCheckpoint (tmcAppDir (tmCfg tm) </> taskDirName tid)
    pure (if Map.null saved then Nothing else Just (tid, sum (Map.elems saved)))
  pure (Map.fromList (activeSums <> standbySums <> catMaybes diskSums))


-- | Tasks without pending restores start running and resume their partitions.
promoteRestored :: TaskManager -> IO ()
promoteRestored tm = do
  pending <- Set.fromList . map rtTask . Map.elems <$> readIORef (tmRestore tm)
  tasks <- readIORef (tmTasks tm)
  paused <- readIORef (tmPaused tm)
  forM_ (Map.elems tasks) $ \t -> do
    st <- readIORef (taskState t)
    when (st == Restoring && Set.notMember (taskId t) pending) $ do
      writeIORef (taskState t) Running
      unless paused (sdResume (tmDriver tm) (taskPartitions t))


----------------------------------------------------------------------
-- Routing + processing
----------------------------------------------------------------------

-- | Buffer polled records in their tasks' queues.
tmRoute :: TaskManager -> [DriverRecord] -> IO ()
tmRoute tm recs = do
  tasks <- readIORef (tmTasks tm)
  let plan = tmPlan tm
      cap = tmcMaxBufferedPerPartition (tmCfg tm)
  full <- fmap concat . forM recs $ \r -> do
    let tp = KC.TopicPartition r.drTopic r.drPartition
        owners =
          mapMaybe
            (\sid -> Map.lookup (TaskId sid r.drPartition) tasks)
            (Map.findWithDefault [] r.drTopic (rpTopicSubtopologies plan))
    fmap concat . forM owners $ \t -> do
      n <- atomicModifyIORef' (taskQueues t) $ \m ->
        let q = Map.findWithDefault Seq.empty tp m Seq.|> r
        in (Map.insert tp q m, Seq.length q)
      pure (if n >= cap then [tp] else [])
  unless (null full) $ do
    atomicModifyIORef' (tmBackpressured tm) (\s -> (Set.union s (Set.fromList full), ()))
    sdPause (tmDriver tm) full


{- | Process what every running task has buffered (in parallel over
the workers), fire wall-clock punctuators, and lift back-pressure.
A failure that the processing handler doesn't absorb (or a
deserialization / partitioner / store failure) is rethrown once
every worker has stopped.
-}
tmProcess :: TaskManager -> IO ()
tmProcess tm = do
  paused <- readIORef (tmPaused tm)
  unless paused $ do
    tasks <- Map.elems <$> readIORef (tmTasks tm)
    running <- filterM (\t -> (== Running) <$> readIORef (taskState t)) tasks
    threads <- readIORef (tmThreadsRef tm)
    let byWorker = Map.elems (Map.fromListWith (flip (<>)) (map (\t -> (taskWorker t `mod` threads, [t])) running))
    results <- case byWorker of
      [] -> pure []
      [ts] -> (: []) <$> try (mapM_ (processTask tm) ts)
      _ -> mapConcurrently (try . mapM_ (processTask tm)) byWorker
    -- lift back-pressure on partitions that drained
    bp <- readIORef (tmBackpressured tm)
    unless (Set.null bp) $ do
      drained <- fmap concat . forM running $ \t -> do
        qs <- readIORef (taskQueues t)
        pure
          ( filter
              (\tp -> tp `elem` taskPartitions t && maybe True ((< tmcMaxBufferedPerPartition (tmCfg tm)) . Seq.length) (Map.lookup tp qs))
              (Set.toList bp)
          )
      unless (null drained) $ do
        atomicModifyIORef' (tmBackpressured tm) (\s -> (foldr Set.delete s drained, ()))
        sdResume (tmDriver tm) drained
    case lefts results of
      (e : _) -> throwIO (e :: SomeException)
      [] -> pure ()


processTask :: TaskManager -> Task -> IO ()
processTask tm t = do
  engine <- readIORef (taskEngine t)
  now <- nowMillis
  let idleMs = tmcMaxTaskIdleMs (tmCfg tm)
      loop !budget
        | budget <= 0 = pure ()
        | otherwise = do
            qs <- readIORef (taskQueues t)
            let heads = mapMaybe (\(tp, q) -> (,) tp <$> Seq.lookup 0 q) (Map.toList qs)
                emptyParts = filter (\tp -> maybe True Seq.null (Map.lookup tp qs)) (taskPartitions t)
            -- With max.task.idle.ms > 0 a task waits that long for an
            -- empty partition before running ahead on the others.
            waiting <-
              if idleMs > 0 && not (null emptyParts) && length (taskPartitions t) > 1
                then do
                  since <- readIORef (taskEmptySince t)
                  let since' = Map.union (Map.restrictKeys since (Set.fromList emptyParts)) (Map.fromList (map (\tp -> (tp, now)) emptyParts))
                  writeIORef (taskEmptySince t) since'
                  pure (any (\tp -> now - Map.findWithDefault now tp since' < fromIntegral idleMs) emptyParts)
                else pure False
            case sortOn (\(_, r) -> r.drTimestamp) heads of
              _ | waiting -> pure ()
              [] -> pure ()
              ((tp, r) : _) -> do
                atomicModifyIORef' (taskQueues t) (\m -> (Map.adjust (Seq.drop 1) tp m, ()))
                feedRecord tm t engine r
                atomicModifyIORef' (taskConsumed t) (\m -> (Map.insert tp (r.drOffset + 1) m, ()))
                modifyIORef' (taskProcessed t) (+ 1)
                loop (budget - 1 :: Int)
  loop 100_000
  syncWallClockToReal engine
  asked <- takeCommitRequested engine
  when asked (writeIORef (tmCommitRequest tm) True)


feedRecord :: TaskManager -> Task -> Engine -> DriverRecord -> IO ()
feedRecord tm t engine r = do
  res <-
    try $
      feedSourceH
        engine
        (topicName r.drTopic)
        r.drKey
        r.drValue
        (KH.fromList r.drHeaders)
        (Timestamp r.drTimestamp)
        (fromIntegral r.drPartition)
        r.drOffset
  case res of
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | Just (se :: StreamsException) <- fromException e -> throwIO se
      | otherwise -> do
          h <- tmcProcessingHandler (tmCfg tm)
          resp <-
            runProcessingExceptionHandler
              h
              ProcessingException
                { topic = r.drTopic
                , partition = r.drPartition
                , offset = fromIntegral r.drOffset
                , node = taskName (taskId t)
                , reason = T.pack (show e)
                }
          case resp of
            ProcessingContinue -> pure ()
            ProcessingFail ->
              throwIO
                StreamsException
                  { streamsErrorMessage =
                      "processing failed in task " <> taskName (taskId t) <> " at " <> r.drTopic <> "-" <> decimalText r.drPartition <> "@" <> decimalText r.drOffset <> ": " <> T.pack (show e)
                  , streamsErrorContext = Just "processing"
                  }


-- | Change how many workers process tasks (tasks are spread over them by index).
tmSetThreads :: TaskManager -> Int -> IO ()
tmSetThreads tm n = writeIORef (tmThreadsRef tm) (max 1 n)


tmThreads :: TaskManager -> IO Int
tmThreads = readIORef . tmThreadsRef


tmTakeCommitRequest :: TaskManager -> IO Bool
tmTakeCommitRequest tm = atomicModifyIORef' (tmCommitRequest tm) (\b -> (False, b))


----------------------------------------------------------------------
-- Commit
----------------------------------------------------------------------

data CommitResult
  = Committed
  | -- | At least once: the offset commit failed (typically a rebalance); nothing was lost
    CommitFailed !Text
  | -- | Exactly once: the transaction was aborted and every task recovered
    CommitAborted !Text


{- | Commit every running task. A failure the production handler
doesn't absorb, or a store that can't flush, is thrown.
-}
tmCommit :: TaskManager -> IO CommitResult
tmCommit tm = do
  r <- commitOnce tm
  bump tm $ case r of
    Committed -> ixCommits
    CommitFailed _ -> ixCommitsFailed
    CommitAborted _ -> ixCommitsAborted
  pure r


commitOnce :: TaskManager -> IO CommitResult
commitOnce tm = do
  tasks <- Map.elems <$> readIORef (tmTasks tm)
  running <- filterM (\t -> (== Running) <$> readIORef (taskState t)) tasks
  forM_ running $ \t -> readIORef (taskEngine t) >>= commitEngine
  offsets <- fmap concat . forM running $ \t -> do
    consumed <- readIORef (taskConsumed t)
    committed <- readIORef (taskCommitted t)
    pure (filter (\(tp, off) -> Map.lookup tp committed /= Just off) (Map.toList consumed))
  let drv = tmDriver tm
      done = do
        markCommitted running offsets
        unless (eos tm) $ do
          forM_ running (checkpoint tm)
          readIORef (tmStandbys tm) >>= mapM_ checkpointStandby . Map.elems
        purgeRepartitions tm running
        pure Committed
  if eos tm
    then do
      txnOpen <- readMVar (tmTxnOpen tm)
      if not txnOpen && null offsets
        then pure Committed
        else do
          began <- ensureTransaction tm
          fl <- sdFlush drv
          errs <- atomicModifyIORef' (tmSendErrors tm) (\es -> ([], es))
          case sendFailure began fl errs of
            Just why -> abortAndRecover Nothing why
            Nothing ->
              prepareCycle tm >>= \case
                Left why -> abortAndRecover Nothing why
                Right mCycle -> do
                  -- the cycle's marker rides in the transaction
                  marked <- markCycle tm mCycle
                  so <- case marked of
                    Left e -> pure (Left e)
                    Right () -> if null offsets then pure (Right ()) else sdSendOffsetsToTransaction drv offsets
                  case so of
                    Left e -> abortAndRecover mCycle ("send offsets to transaction: " <> T.pack e)
                    Right () -> do
                      c <- sdCommitTransaction drv
                      case c of
                        Left e -> abortAndRecover mCycle ("commit transaction: " <> T.pack e)
                        Right () -> do
                          _ <- swapMVar (tmTxnOpen tm) False
                          commitCycle tm mCycle
                          done
    else do
      fl <- sdFlush drv
      errs <- atomicModifyIORef' (tmSendErrors tm) (\es -> ([], es))
      case fl of
        Left e -> throwIO (StreamsException ("producer flush failed: " <> T.pack e) (Just "production"))
        Right () -> pure ()
      h <- tmcProductionHandler (tmCfg tm)
      forM_ (reverse errs) $ \(tp, e) -> do
        resp <- runProductionHandler h ProductionException {topic = tp, reason = T.pack e}
        case resp of
          ProdContinueProcessing -> logLine ("dropped a record for " <> T.unpack tp <> ": " <> e)
          ProdFailFast -> throwIO (StreamsException ("send to " <> tp <> " failed: " <> T.pack e) (Just "production"))
      prepareCycle tm >>= \case
        Left why -> abortAndRecover Nothing why
        Right mCycle -> do
          -- marker, then the participants' commit, then the offsets: a
          -- crash in between replays input (duplicates), never loses it
          marked <- markCycle tm mCycle
          flushed <- either (pure . Left) (const (sdFlush drv)) marked
          case flushed of
            Left e -> abortAndRecover mCycle ("record commit cycle: " <> T.pack e)
            Right () -> do
              commitCycle tm mCycle
              c <- if null offsets then pure (Right ()) else sdCommitOffsets drv offsets
              case c of
                Left e -> pure (CommitFailed (T.pack e))
                Right () -> done
  where
    sendFailure began fl errs = case (began, fl, errs) of
      (Left e, _, _) -> Just ("begin transaction: " <> T.pack e)
      (_, Left e, _) -> Just ("flush: " <> T.pack e)
      (_, _, (tp, e) : _) -> Just ("send to " <> tp <> ": " <> T.pack e)
      _ -> Nothing
    abortAndRecover mCycle why = do
      logLine ("commit aborted, recovering every task: " <> T.unpack why)
      forM_ mCycle $ \c -> forM_ (tmcParticipants (tmCfg tm)) (\p -> cpAbort p (renderCycleId c))
      tmRecoverAll tm
      pure (CommitAborted why)


-- | What the instance's task manager has done since it started.
data InstanceCounts = InstanceCounts
  { icCommits :: {-# UNPACK #-} !Int
  , icCommitsFailed :: {-# UNPACK #-} !Int
  -- ^ At least once: the offset commit failed (input is replayed)
  , icCommitsAborted :: {-# UNPACK #-} !Int
  -- ^ The commit was aborted and every task recovered
  , icRecoveries :: {-# UNPACK #-} !Int
  -- ^ Every task rewound to the last commit (aborts, replaced threads, lost partitions under EOS)
  , icRebalances :: {-# UNPACK #-} !Int
  -- ^ Assignments received
  , icRestoredRecords :: {-# UNPACK #-} !Int
  -- ^ Changelog records applied while restoring
  , icStandbyRecords :: {-# UNPACK #-} !Int
  -- ^ Changelog records applied to standbys
  }
  deriving stock (Eq, Show)


ixCommits, ixCommitsFailed, ixCommitsAborted, ixRecoveries, ixRebalances, ixRestored, ixStandbyRecords, countersSize :: Int
ixCommits = 0
ixCommitsFailed = 1
ixCommitsAborted = 2
ixRecoveries = 3
ixRebalances = 4
ixRestored = 5
ixStandbyRecords = 6
countersSize = 7


bump :: TaskManager -> Int -> IO ()
bump tm ix = addCount tm ix 1
{-# INLINE bump #-}


addCount :: TaskManager -> Int -> Int -> IO ()
addCount tm ix n = UMV.unsafeModify (tmCounters tm) (+ n) ix
{-# INLINE addCount #-}


tmCounts :: TaskManager -> IO InstanceCounts
tmCounts tm = do
  let c = tmCounters tm
      at = UMV.unsafeRead c
  InstanceCounts
    <$> at ixCommits
    <*> at ixCommitsFailed
    <*> at ixCommitsAborted
    <*> at ixRecoveries
    <*> at ixRebalances
    <*> at ixRestored
    <*> at ixStandbyRecords


-- | The internal topic holding each instance's last committed cycle.
cycleTopic :: TaskManager -> Text
cycleTopic tm = tmcAppId (tmCfg tm) <> "-twophase-commits"


{- | Have every participant prepare a new cycle; on a failure the ones
already prepared abort it. 'Nothing' when there are no participants.
-}
prepareCycle :: TaskManager -> IO (Either Text (Maybe CycleId))
prepareCycle tm = case tmcParticipants (tmCfg tm) of
  [] -> pure (Right Nothing)
  parts -> do
    n <- atomicModifyIORef' (tmCycle tm) (\c -> (c + 1, c + 1))
    let cyc = CycleId (tmcInstanceId (tmCfg tm)) n
        cid = renderCycleId cyc
        go _ [] = pure (Right (Just cyc))
        go prepared (p : rest) = do
          r <- cpPrepare p cid
          case r of
            Right () -> go (p : prepared) rest
            Left e -> do
              mapM_ (\q -> cpAbort q cid) (p : prepared)
              pure (Left ("prepare " <> cpName p <> ": " <> e))
    go [] parts


-- | Send the cycle's marker (inside the open transaction under exactly-once).
markCycle :: TaskManager -> Maybe CycleId -> IO (Either String ())
markCycle _ Nothing = pure (Right ())
markCycle tm (Just (CycleId inst n)) = do
  began <- ensureTransaction tm
  case began of
    Left e -> pure (Left e)
    Right () -> do
      failed <- newIORef Nothing
      r <-
        sdSend
          (tmDriver tm)
          KP.ProducerRecord
            { KP.topic = cycleTopic tm
            , KP.key = Just (TE.encodeUtf8 inst)
            , KP.value = Just (TE.encodeUtf8 (decimalText n))
            , KP.headers = []
            , KP.partition = Just 0
            , KP.timestamp = Nothing
            }
          (either (writeIORef failed . Just) (const (pure ())))
      fl <- sdFlush (tmDriver tm)
      f <- readIORef failed
      pure $ case (r, fl, f) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Just e) -> Left e
        _ -> Right ()


-- | The cycle committed in Kafka: make it visible everywhere.
commitCycle :: TaskManager -> Maybe CycleId -> IO ()
commitCycle _ Nothing = pure ()
commitCycle tm (Just cyc) =
  forM_ (tmcParticipants (tmCfg tm)) $ \p -> do
    r <- cpCommit p (renderCycleId cyc)
    case r of
      Right () -> pure ()
      -- committed in Kafka, not yet in the participant: retried by the
      -- recovery at the next start
      Left e -> throwIO (StreamsException ("commit " <> cpName p <> " for " <> renderCycleId cyc <> ": " <> e) (Just "two-phase commit"))


markCommitted :: [Task] -> [(KC.TopicPartition, Int64)] -> IO ()
markCommitted running offsets = do
  let m = Map.fromList offsets
  forM_ running $ \t ->
    atomicModifyIORef' (taskCommitted t) (\c -> (Map.union (Map.restrictKeys m (Set.fromList (taskPartitions t))) c, ()))


-- | Write the task's checkpoint, covering its persistent stores.
checkpoint :: TaskManager -> Task -> IO ()
checkpoint _tm t = do
  engine <- readIORef (taskEngine t)
  stores <- engineOwnStores engine
  pos <- readIORef (taskChangelogPos t)
  let TaskId _ p = taskId t
      persistentTps =
        Set.fromList
          ( map
              (\(_, cl) -> KC.TopicPartition (unTopicName (clTopic cl)) p)
              (filter (\(sn, _) -> maybe False isPersistent (Map.lookup sn stores)) (Map.toList (subPlanChangelogs (taskSub t))))
          )
  unless (Set.null persistentTps) $
    writeCheckpoint
      (taskDir t)
      (Map.mapKeys (\tp -> (tp.topic, tp.partition)) (Map.restrictKeys pos persistentTps))


{- | Drop consumed repartition records (at most every 30 s), like the
JVM's @repartition.purge.interval.ms@.
-}
purgeRepartitions :: TaskManager -> [Task] -> IO ()
purgeRepartitions tm running = do
  now <- nowMillis
  lastPurge <- readIORef (tmLastPurge tm)
  let repartition = Set.fromList (rpRepartitionTopics (tmPlan tm))
  when (now - lastPurge >= 30_000 && not (Set.null repartition)) $ do
    writeIORef (tmLastPurge tm) now
    offs <- fmap concat . forM running $ \t -> do
      c <- readIORef (taskCommitted t)
      pure (filter (\(tp, _) -> Set.member tp.topic repartition) (Map.toList c))
    unless (null offs) $ do
      r <- sdDeleteRecords (tmDriver tm) offs
      either (\e -> logLine ("purging repartition topics failed: " <> e)) pure r


----------------------------------------------------------------------
-- Recovery + close
----------------------------------------------------------------------

{- | Close every task dirty, wipe its state, rebuild it, restore it from
the changelogs and rewind its partitions to the last commit. Under
exactly-once the open transaction is aborted first.
-}
tmRecoverAll :: TaskManager -> IO ()
tmRecoverAll tm = do
  bump tm ixRecoveries
  let drv = tmDriver tm
  when (eos tm) $ do
    txnOpen <- readMVar (tmTxnOpen tm)
    when txnOpen $ do
      r <- sdAbortTransaction drv
      case r of
        Right () -> pure ()
        Left e -> do
          -- A producer that can't abort is re-initialised, which
          -- aborts the transaction on the broker.
          logLine ("abort failed (" <> e <> "); re-initialising the transactional producer")
          ri <- sdInitTransactions drv
          either (\e' -> throwIO (StreamsException ("cannot re-initialise transactions: " <> T.pack e') (Just "eos"))) pure ri
    _ <- swapMVar (tmTxnOpen tm) False
    pure ()
  writeIORef (tmSendErrors tm) []
  -- the input is rewound: what the participants staged comes again
  forM_ (tmcParticipants (tmCfg tm)) cpDiscard
  tasks <- Map.elems <$> readIORef (tmTasks tm)
  -- cancel in-flight restores; they are rescheduled from scratch
  writeIORef (tmRestore tm) Map.empty
  writeIORef (tmReaderAssigned tm) Set.empty
  forM_ tasks $ \t -> do
    old <- readIORef (taskEngine t)
    _ <- try (closeEngine old) :: IO (Either SomeException ())
    removePathForcibly (taskDir t)
    writeIORef (taskChangelogPos t) Map.empty
    engine <- buildTaskEngine tm (taskSub t) (taskId t) (taskDir t) (taskChangelogPos t) False
    writeIORef (taskEngine t) engine
    writeIORef (taskQueues t) Map.empty
    committed <- readIORef (taskCommitted t)
    writeIORef (taskConsumed t) committed
    forM_ (taskPartitions t) $ \tp ->
      sdSeek drv tp $ case (Map.lookup tp committed, resetPolicy (tmPlan tm) tp) of
        (Just off, _) -> off
        (Nothing, OffsetLatest) -> -1
        (Nothing, _) -> -2
    writeIORef (taskState t) Restoring
    sdPause drv (taskPartitions t)
    r <- scheduleRestore tm t FromScratch
    case r of
      Left e -> throwIO (StreamsException ("recovering task " <> taskName (taskId t) <> ": " <> T.pack e) (Just "restore"))
      Right _ -> pure ()
  writeIORef (tmBackpressured tm) Set.empty
  promoteRestored tm


-- | Already committed by the rebalance hook: close and checkpoint.
tmOnRevoked :: TaskManager -> [KC.TopicPartition] -> IO ()
tmOnRevoked tm tps = do
  atomicModifyIORef' (tmAssignedRef tm) (\s -> (foldr Set.delete s tps, ()))
  closeTasksFor tm tps True


-- | Taken while fenced: nothing of theirs may be committed.
tmOnLost :: TaskManager -> [KC.TopicPartition] -> IO ()
tmOnLost tm tps = do
  atomicModifyIORef' (tmAssignedRef tm) (\s -> (foldr Set.delete s tps, ()))
  closeTasksFor tm tps False
  -- Under exactly-once the open transaction holds every task's
  -- uncommitted output, so it is aborted and the tasks kept are
  -- rewound with it (what the participants staged goes too).
  when (eos tm) (tmRecoverAll tm)


closeTasksFor :: TaskManager -> [KC.TopicPartition] -> Bool -> IO ()
closeTasksFor tm tps clean = do
  tasks <- readIORef (tmTasks tm)
  let gone = Set.fromList tps
      closing = filter (any (`Set.member` gone) . taskPartitions) (Map.elems tasks)
  forM_ closing (closeTask tm clean)


closeTask :: TaskManager -> Bool -> Task -> IO ()
closeTask tm clean t = do
  listener <- tmcRestoreListener (tmCfg tm)
  pending <- Map.filter ((== taskId t) . rtTask) <$> readIORef (tmRestore tm)
  forM_ (Map.toList pending) $ \(tp, rt) -> onRestoreSuspended listener tp (unStoreName (rtStore rt)) (rtRestored rt)
  atomicModifyIORef' (tmRestore tm) (\m -> (Map.withoutKeys m (Map.keysSet pending), ()))
  st <- readIORef (taskState t)
  when (clean && st == Running) (checkpoint tm t)
  engine <- readIORef (taskEngine t)
  r <- try (closeEngine engine) :: IO (Either SomeException ())
  either (\e -> logLine ("closing task " <> T.unpack (taskName (taskId t)) <> ": " <> show e)) pure r
  -- dirty state must not be trusted by the next owner on this machine
  unless clean (removePathForcibly (taskDir t))
  atomicModifyIORef' (tmTasks tm) (\m -> (Map.delete (taskId t) m, ()))


-- | Close every task (after the final commit), checkpointing if @clean@.
tmCloseAll :: TaskManager -> Bool -> IO ()
tmCloseAll tm clean = do
  tasks <- Map.elems <$> readIORef (tmTasks tm)
  forM_ tasks (closeTask tm clean)
  standbys <- atomicModifyIORef' (tmStandbys tm) (\m -> (Map.empty, Map.elems m))
  forM_ standbys (closeStandby tm)
  rdClose (tmReader tm)


-- | Stop (or restart) fetching and processing.
tmSetPaused :: TaskManager -> Bool -> IO ()
tmSetPaused tm p = do
  was <- readIORef (tmPaused tm)
  when (was /= p) $ do
    writeIORef (tmPaused tm) p
    tasks <- Map.elems <$> readIORef (tmTasks tm)
    if p
      then sdPause (tmDriver tm) (concatMap taskPartitions tasks)
      else do
        bp <- readIORef (tmBackpressured tm)
        running <- filterM (\t -> (== Running) <$> readIORef (taskState t)) tasks
        sdResume (tmDriver tm) (filter (`Set.notMember` bp) (concatMap taskPartitions running))


----------------------------------------------------------------------
-- Introspection
----------------------------------------------------------------------

-- | Every task's engine (running or restoring), by task.
tmEngines :: TaskManager -> IO [(TaskId, Engine)]
tmEngines tm = do
  tasks <- Map.elems <$> readIORef (tmTasks tm)
  forM tasks $ \t -> (,) (taskId t) <$> readIORef (taskEngine t)


data TaskSnapshot = TaskSnapshot
  { tsTask :: !TaskId
  , tsWorker :: !Int
  , tsPartitions :: ![KC.TopicPartition]
  , tsRunning :: !Bool
  , tsProcessed :: !Int64
  , tsCommitted :: !(Map KC.TopicPartition Int64)
  }
  deriving stock (Eq, Show)


tmTaskSnapshot :: TaskManager -> IO [TaskSnapshot]
tmTaskSnapshot tm = do
  tasks <- Map.elems <$> readIORef (tmTasks tm)
  threads <- readIORef (tmThreadsRef tm)
  forM tasks $ \t -> do
    st <- readIORef (taskState t)
    n <- readIORef (taskProcessed t)
    c <- readIORef (taskCommitted t)
    pure
      TaskSnapshot
        { tsTask = taskId t
        , tsWorker = taskWorker t `mod` threads
        , tsPartitions = taskPartitions t
        , tsRunning = st == Running
        , tsProcessed = n
        , tsCommitted = c
        }

