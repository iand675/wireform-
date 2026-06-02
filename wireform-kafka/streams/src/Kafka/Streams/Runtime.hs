{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime
-- Description : Driver-backed runtime
--
-- @
-- KafkaStreams
-- @
--
-- mirrors the JVM @KafkaStreams@ class. Each instance:
--
--   * Spins up @numStreamThreads@ stream-threads (currently
--     consolidated into a single foreground worker).
--   * Owns a 'Kafka.Streams.Runtime.NativeDriver.StreamDriver'
--     that wraps everything talking to the broker: consumer
--     poll/commit/subscribe/close, producer
--     send/flush/close, and the EOS-V2 transactional callbacks.
--   * Runs an event loop:
--
--       1. @poll@ for records via the driver.
--       2. For each batch, drive the engine by calling 'feedSource'
--          per record.
--       3. Drain the record collector through the driver's
--          producer hook.
--       4. Commit consumer offsets at the configured cadence,
--          either via 'sdConsumerCommit' (at-least-once) or
--          through the EOS coordinator (exactly-once-V2).
--
-- Running against a real broker uses 'startKafkaStreams', which
-- builds a default 'StreamDriver' from a fresh 'Producer' /
-- 'Consumer' pair and delegates to 'startKafkaStreamsWith'.
-- Tests inject a mock driver via 'startKafkaStreamsWith' to
-- exercise the runtime deterministically without spinning up a
-- broker.
module Kafka.Streams.Runtime
  ( KafkaStreams
  , newKafkaStreams
  , startKafkaStreams
  , startKafkaStreamsWith
  , closeKafkaStreams
    -- * Status
  , StreamsStatus (..)
  , streamsStatus
  , awaitState
  , StateListener
  , setStateListener
    -- * EOS
  , applyEOSCoordinator
    -- * Pause / resume
  , pauseKafkaStreams
  , resumeKafkaStreams
  , isPausedKafkaStreams
    -- * Task lag
  , LagInfo (..)
  , LagListener
  , setLagListener
  , publishLag
    -- * Multi-instance rebalance
  , setRebalanceListener
  , ownedPartitions
  , standbyTasks
    -- * Probing rebalance
  , reportWarmupLag
  , clearWarmupLag
  , warmupSnapshot
    -- * Standby tasks
  , ksStandbyManager
    -- * Exception handlers
  , setProductionExceptionHandler
  , setProcessingExceptionHandler
  , setUncaughtExceptionHandler
    -- * Thread management + cleanup
  , addStreamThread
  , removeStreamThread
  , streamThreadCount
  , cleanUp
    -- * Close options
  , CloseOptions (..)
  , defaultCloseOptions
  , closeKafkaStreamsWith
    -- * Listeners (global-restore)
  , StandbyUpdateListener
  , setStandbyUpdateListener
  , GlobalStateRestoreListener
  , setGlobalStateRestoreListener
  , StateRestoreListener (..)
  , defaultStateRestoreListener
  , setStateRestoreListener
    -- * Metadata + metrics
  , LocalThreadMetadata (..)
  , metadataForLocalThreads
  , metricsAndState
    -- * KIP-714 client telemetry id
  , kafkaStreamsClientInstanceId
    -- * KIP-924 user-pluggable task assignor
  , streamsRunUserAssignor
  , streamsApplicationState
    -- * Progress signal (used by tests to coordinate without threadDelay)
  , ksTickCount
  , awaitTicks
    -- * Internal access (used by Kafka.Streams.InteractiveQueries)
  , ksEngine
  , ksPool
  ) where

import Control.Concurrent (yield)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import qualified Data.Foldable as Foldable
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Kafka.Client.Telemetry as Telemetry

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Producer as KP

import Kafka.Streams.Config
  ( ProcessingGuarantee (..)
  , StreamsConfig (..)
  )
import qualified Kafka.Streams.Processor.Assignment as PA
import Kafka.Streams.Errors
  ( ProcessingException (..)
  , ProcessingExceptionHandler (..)
  , ProcessingResponse (..)
  , ProductionException (..)
  , ProductionHandler (..)
  , ProductionResponse (..)
  , StreamsUncaughtExceptionHandler (..)
  , UncaughtExceptionResponse (..)
  , logAndContinue
  , logAndContinueProcessing
  , logAndContinueProduction
  , replaceThreadOnException
  )
import Control.Exception (Exception, SomeException, throwIO, try)
import Kafka.Streams.Runtime.EOS
  ( CommitOutcome (..)
  , EOSCoordinator (..)
  , noopEOSCoordinator
  , runCommitCycle
  )
import qualified Kafka.Client.RebalanceListener as RBL
import Kafka.Client.RebalanceListener
  ( RebalanceListener
  , noopRebalanceListener
  )
import qualified Kafka.Streams.Runtime.ProbingRebalance as ProbingRebalance
import Kafka.Streams.Runtime.StandbyTask
  ( StandbyManager
  , newStandbyManager
  )
import Kafka.Streams.Runtime.NativeDriver
  ( RebalanceEvent (..)
  , StreamDriver (..)
  , newNativeDriver
  )
import Kafka.Streams.Runtime.RevocationGrace
  ( RevocationOutcome (..)
  , classifyRevocation
  )
import Kafka.Streams.Runtime.WorkerPool
  ( WorkerPool
  , Worker (..)
  , addPoolWorker
  , closeWorkerPool
  , commitAllWorkers
  , newWorkerPoolHashed
  , poolWorkers
  , poolWorkerCount
  , removePoolWorker
  , submitRecordHashed
  , waitForQuiescence
  , workerCollector
  , workerProcessedCount
  )
import Kafka.Streams.Internal.Engine
  ( Engine
  , buildEngine
  , closeEngine
  , commitEngine
  , feedSource
  , reportPollCycle
  , syncWallClockToReal
  )
import Kafka.Streams.Internal.RecordCollector
  ( CollectedRecord (..)
  , RecordCollector (..)
  , drainCollector
  )
import qualified Data.Vector as V
import Data.Int (Int64)
import Kafka.Streams.Processor (TaskId (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (Timestamp (..), nowMillis)
import Kafka.Streams.Types (TopicName, topicName, unTopicName)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.Set as Set
import qualified Data.UUID as UUID

-- | Lifecycle of the runtime.
data StreamsStatus
  = StreamsCreated
  | StreamsRunning
  | StreamsClosing
  | StreamsClosed
  | StreamsError !Text
  deriving (Eq, Show)

-- | A user-installed callback fired on every state transition.
type StateListener =
  StreamsStatus       -- old state
  -> StreamsStatus    -- new state
  -> IO ()

data KafkaStreams = KafkaStreams
  { ksConfig    :: !StreamsConfig
  , ksTopology  :: !Topo.TopologyValid
  , ksStatus    :: !(TVar StreamsStatus)
  , ksThread    :: !(IORef (Maybe (Async ())))
  , ksDriver    :: !(IORef (Maybe StreamDriver))
  , ksEngine    :: !(IORef (Maybe Engine))
    -- ^ Single-threaded runtime engine. With
    -- 'numStreamThreads = 1' (the default) this is the engine
    -- that drives every record. With 'numStreamThreads > 1'
    -- it remains 'Nothing'; per-thread engines live inside
    -- 'ksPool' instead. 'Kafka.Streams.InteractiveQueries'
    -- prefers 'ksEngine' when set and falls back to the
    -- first worker's engine otherwise.
  , ksPool      :: !(IORef (Maybe WorkerPool))
    -- ^ Worker pool used when 'numStreamThreads > 1'.
  , ksEosCoord  :: !(IORef EOSCoordinator)
  , ksListener  :: !(IORef StateListener)
  , ksPaused    :: !(TVar Bool)
  , ksLagLis    :: !(IORef LagListener)
    -- * Multi-instance rebalance state -------------------------------
  , ksOwned     :: !(TVar (HashSet KC.TopicPartition))
    -- ^ Partitions this instance is currently actively
    -- processing. Updated by 'RebalanceAssigned' /
    -- 'RebalanceRevoked' / 'RebalanceLost' events drained from
    -- the driver.
  , ksStandbys  :: !(TVar (Map.Map KC.TopicPartition Int64))
    -- ^ KIP-869 standby grace: partitions we revoked but whose
    -- task state we keep around (for IQ continuity / fast
    -- re-promotion) until the deadline.
  , ksRebLis    :: !(IORef RebalanceListener)
    -- ^ User callback fired on every rebalance event.
  , ksTicks     :: !(TVar Int)
    -- ^ Monotonic counter bumped at the bottom of every
    -- event-loop iteration. Tests block on this via
    -- 'awaitTicks' to coordinate with engine progress
    -- without using 'threadDelay'.
  , ksProdHand  :: !(IORef ProductionHandler)
    -- ^ KIP-280 production handler. Default is
    -- 'logAndContinueProduction'.
  , ksProcHand  :: !(IORef ProcessingExceptionHandler)
    -- ^ KIP-1033 processing handler. Default is
    -- 'logAndContinueProcessing'.
  , ksUncaught  :: !(IORef StreamsUncaughtExceptionHandler)
    -- ^ KIP-671 uncaught-exception handler. Default is
    -- 'replaceThreadOnException'.
  , ksStandbyLis :: !(IORef StandbyUpdateListener)
  , ksGlobalRestoreLis :: !(IORef GlobalStateRestoreListener)
  , ksRestoreListener :: !(IORef (Maybe StateRestoreListener))
  , ksWarmupLag :: !(TVar (Map.Map TaskId Int64))
    -- ^ KIP-441 warmup-replica progress map: 'TaskId ->
    -- changelog-lag'. Updated by user code (typically a
    -- standby-task replay loop) via 'reportWarmupLag'; the
    -- event-loop consults this map together with
    -- 'probingRebalanceIntervalMs' / 'acceptableRecoveryLag'
    -- to decide whether to fire 'sdRequestProbingRebalance'.
  , ksLastProbeAt :: !(TVar Int64)
  , ksStandbyManager :: !StandbyManager
  }

newKafkaStreams
  :: StreamsConfig
  -> Topo.TopologyValid
  -> IO KafkaStreams
newKafkaStreams cfg topo = do
  s <- newTVarIO StreamsCreated
  t <- newIORef Nothing
  d <- newIORef Nothing
  e <- newIORef Nothing
  pool <- newIORef Nothing
  eos <- newIORef noopEOSCoordinator
  lis <- newIORef (\_ _ -> pure ())
  pa  <- newTVarIO False
  lagL <- newIORef (\_ -> pure ())
  owned <- newTVarIO HashSet.empty
  stand <- newTVarIO Map.empty
  reb  <- newIORef noopRebalanceListener
  ticks <- newTVarIO 0
  prodH <- newIORef logAndContinueProduction
  procH <- newIORef logAndContinueProcessing
  uncH  <- newIORef replaceThreadOnException
  stbyLis <- newIORef (\_ _ -> pure ())
  grLis   <- newIORef (\_ _ -> pure ())
  rLis    <- newIORef Nothing
  warmup  <- newTVarIO Map.empty
  lastPr  <- newTVarIO 0
  stbyMgr <- newStandbyManager
  pure KafkaStreams
    { ksConfig    = cfg
    , ksTopology  = topo
    , ksStatus    = s
    , ksThread    = t
    , ksDriver    = d
    , ksEngine    = e
    , ksPool      = pool
    , ksEosCoord  = eos
    , ksListener  = lis
    , ksPaused    = pa
    , ksLagLis    = lagL
    , ksOwned     = owned
    , ksStandbys  = stand
    , ksRebLis    = reb
    , ksTicks     = ticks
    , ksProdHand  = prodH
    , ksProcHand  = procH
    , ksUncaught  = uncH
    , ksStandbyLis = stbyLis
    , ksGlobalRestoreLis = grLis
    , ksRestoreListener = rLis
    , ksWarmupLag = warmup
    , ksLastProbeAt = lastPr
    , ksStandbyManager = stbyMgr
    }

-- | Start the runtime against a real broker. Constructs a
-- 'KP.Producer' + 'KC.Consumer' from the bootstrap settings on
-- 'StreamsConfig', wraps them in a default 'StreamDriver', and
-- delegates to 'startKafkaStreamsWith'.
startKafkaStreams :: KafkaStreams -> IO ()
startKafkaStreams ks = do
  ePR <- KP.createProducer (bootstrapServers (ksConfig ks)) (producerCfg ks)
  case ePR of
    Left err -> setError ks ("producer create: " <> Text.pack err)
    Right p  -> do
      eCR <- KC.createConsumer
              (bootstrapServers (ksConfig ks))
              (applicationId (ksConfig ks))
              (consumerCfg ks)
      case eCR of
        Left err -> do
          KP.closeProducer p
          setError ks ("consumer create: " <> Text.pack err)
        Right c  -> do
          driver <- newNativeDriver p c Nothing
          startKafkaStreamsWith ks driver

-- | Start the runtime against a caller-supplied 'StreamDriver'.
-- This is the seam tests use to feed in a 'newMockDriver'; it is
-- also where future runtime composition (per-task drivers,
-- standby threads, etc.) will plug in.
--
-- Returns immediately after the worker thread is launched.
-- Status transitions are visible via 'streamsStatus' /
-- 'awaitState'.
startKafkaStreamsWith :: KafkaStreams -> StreamDriver -> IO ()
startKafkaStreamsWith ks driver = do
  writeIORef (ksDriver ks) (Just driver)
  let cfg  = ksConfig ks
      topo = ksTopology ks
      n    = max 1 (numStreamThreads cfg)
  if n <= 1
    then startSingleThreaded ks driver topo
    else startMultiThreaded  ks driver topo n
  where
    startSingleThreaded ks_ drv topo = do
      collector <- driverCollector ks_ drv
      engine <- buildEngine topo (TaskId 0 0)
                  (applicationId (ksConfig ks_))
                  collector
                  logAndContinue
      writeIORef (ksEngine ks_) (Just engine)
      -- We deliberately do NOT reset 'ksEosCoord' here: a caller
      -- that 'applyEOSCoordinator' before starting expects their
      -- coordinator to be the one driving commit cycles. The
      -- default ('noopEOSCoordinator') is already installed by
      -- 'newKafkaStreams'; under AtLeastOnceP that's exactly the
      -- behaviour we want, and under ExactlyOnceV2 the user is
      -- expected to install a real coordinator before 'start*'.
      let topics = sourceTopics topo
      eSubs <- sdConsumerSubscribe drv (map unTopicName topics)
      case eSubs of
        Left err -> setError ks_ ("subscribe: " <> Text.pack err)
        Right () -> do
          ah <- async (supervisedLoop ks_ drv (eventLoop ks_ drv engine))
          writeIORef (ksThread ks_) (Just ah)
          transitionTo ks_ StreamsRunning

    startMultiThreaded ks_ drv topo n = do
      pool <- newWorkerPoolHashed topo (applicationId (ksConfig ks_)) n
      writeIORef (ksPool ks_) (Just pool)
      let topics = sourceTopics topo
      eSubs <- sdConsumerSubscribe drv (map unTopicName topics)
      case eSubs of
        Left err -> setError ks_ ("subscribe: " <> Text.pack err)
        Right () -> do
          ah <- async (supervisedLoop ks_ drv (multiEventLoop ks_ drv pool))
          writeIORef (ksThread ks_) (Just ah)
          transitionTo ks_ StreamsRunning

producerCfg :: KafkaStreams -> KP.ProducerConfig
producerCfg ks = KP.defaultProducerConfig
  { KP.producerClientId  = clientId (ksConfig ks)
  , KP.producerTransactional =
      case processingGuarantee (ksConfig ks) of
        AtLeastOnceP  -> Nothing
        ExactlyOnceV2 -> Just (applicationId (ksConfig ks)
                                  <> "-txn")
  , KP.producerIdempotent =
      case processingGuarantee (ksConfig ks) of
        AtLeastOnceP  -> False
        ExactlyOnceV2 -> True
  , KP.producerDelivery =
      case processingGuarantee (ksConfig ks) of
        AtLeastOnceP  -> KP.AtLeastOnce
        ExactlyOnceV2 -> KP.ExactlyOnce
  }

consumerCfg :: KafkaStreams -> KC.ConsumerConfig
consumerCfg ks = KC.defaultConsumerConfig
  { KC.consumerClientId  = clientId (ksConfig ks)
  , KC.consumerGroupId   = applicationId (ksConfig ks)
  }

setError :: KafkaStreams -> Text -> IO ()
setError ks msg = transitionTo ks (StreamsError msg)

----------------------------------------------------------------------
-- Driver-backed collector
----------------------------------------------------------------------

-- | Build a record collector that buffers sink emissions in
-- memory and, on flush, hands them off to the driver's producer
-- hooks. The buffering means a failed flush leaves the buffer
-- intact for the next attempt instead of stranding records.
driverCollector :: KafkaStreams -> StreamDriver -> IO RecordCollector
driverCollector ks drv = do
  bufRef <- newIORef (Seq.empty :: Seq CollectedRecord)
  pure RecordCollector
    { collectorSend = \cr -> atomicModifyIORef' bufRef
        (\s -> (s Seq.|> cr, ()))
    , collectorFlush = do
        buf <- atomicModifyIORef' bufRef (\s -> (Seq.empty, s))
        Foldable.for_ buf $ \cr -> do
          r <- sdProducerSend drv
                 (unTopicName (crTopic cr)) (crKey cr) (crValue cr)
          case r of
            Right _ -> pure ()
            Left err -> handleProdFail ks (unTopicName (crTopic cr)) err
        rF <- sdProducerFlush drv
        case rF of
          Right () -> pure ()
          Left err -> handleProdFail ks "" err
    , collectorClose = pure ()
    , collectorPeek  = pure Map.empty
    , collectorTake  = \_ -> pure []
    }

-- | Invoke the user's production handler. 'ProdFailFast' rethrows
-- so the event loop's catch translates it into an uncaught
-- exception event (per KIP-671).
handleProdFail :: KafkaStreams -> Text -> String -> IO ()
handleProdFail ks topic err = do
  h <- readIORef (ksProdHand ks)
  resp <- runProductionHandler h ProductionException
            { topic  = topic
            , reason = Text.pack err
            }
  case resp of
    ProdContinueProcessing -> pure ()
    ProdFailFast           -> throwIO (ProductionFailFast topic (Text.pack err))

-- | Thrown when 'ProdFailFast' / 'ProcessingFail' fires —
-- caught by the event loop's bracket and routed to the
-- KIP-671 uncaught-exception handler.
data StreamsHandlerFail
  = ProductionFailFast !Text !Text
  | ProcessingFailFast !Text !Text
  deriving stock (Eq, Show)

instance Exception StreamsHandlerFail

-- | Run the per-record feed wrapped in a 'try'; on exception
-- consult the KIP-1033 processing handler and either continue
-- or rethrow as 'ProcessingFailFast' so the uncaught-exception
-- handler can act.
feedWithHandler
  :: KafkaStreams
  -> Text                        -- ^ node name where the work runs
  -> KC.ConsumerRecord
  -> IO ()                       -- ^ the actual feed action
  -> IO ()
feedWithHandler ks node rec body = do
  r <- try body :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left  e  -> do
      h <- readIORef (ksProcHand ks)
      resp <- runProcessingExceptionHandler h ProcessingException
                { topic     = rec.topic
                , partition = rec.partition
                , offset    = fromIntegral rec.offset
                , node      = node
                , reason    = Text.pack (show e)
                }
      case resp of
        ProcessingContinue -> pure ()
        ProcessingFail     -> throwIO
          (ProcessingFailFast rec.topic (Text.pack (show e)))

----------------------------------------------------------------------
-- Event loop
----------------------------------------------------------------------

eventLoop :: KafkaStreams -> StreamDriver -> Engine -> IO ()
eventLoop ks driver engine = go
  where
    go = do
      -- Cooperative yield each iteration so a busy loop (notably
      -- @pollMs == 0@, used by the in-process driver / tests) can't
      -- monopolise a capability and starve sibling threads — e.g. a
      -- freshly-forked stream thread from another concurrently
      -- running instance. Negligible against a real @poll@ that
      -- blocks for @pollMs@; the fix matters when @poll@ returns
      -- immediately.
      yield
      status <- readTVarIO (ksStatus ks)
      unless (status == StreamsClosing || status == StreamsClosed) $ do
        drainRebalances ks driver
        maybeIssueProbingRebalance ks driver
        expireStandbys ks
        eRecs <- sdConsumerPoll driver (pollMs (ksConfig ks))
        case eRecs of
          Left err ->
            transitionTo ks (StreamsError (Text.pack err))
          Right recs -> do
            paused <- readTVarIO (ksPaused ks)
            -- While paused we still poll (heartbeat) but DON'T feed
            -- the engine. Records that arrived during the pause are
            -- silently dropped — the consumer will rewind via
            -- offset commits to replay anything not committed.
            unless paused $
              forM_ recs $ \rec ->
                feedWithHandler ks "<source>" rec $
                  feedSource engine
                    (topicName rec.topic)
                    rec.key
                    rec.value
                    (Timestamp rec.timestamp)
                    (fromIntegral rec.partition)
                    rec.offset

            -- Sync wall clock to real time so wall-clock
            -- punctuators fire on the poll cadence and
            -- 'reportPollCycle' sees fresh timing.
            syncWallClockToReal engine
            -- Riffle §5 / KIP-353: report which topics fired
            -- this poll so the watermark coordinator can advance
            -- wall-clock and mark silent sources idle. No-op
            -- when no coordinator is attached.
            let !activeTopics = HashSet.fromList
                  [ topicName rec.topic | rec <- recs ]
            reportPollCycle engine activeTopics

            -- Collect the highest (offset + 1) per (topic, partition)
            -- across the records we just fed; that's what the EOS
            -- coordinator wants to commit. Skipping when paused
            -- means we don't advance offsets past records we
            -- never delivered.
            let !commitOffsets =
                  if paused
                    then HashMap.empty
                    else HashMap.fromListWith max
                           [ ( KC.TopicPartition
                                 rec.topic
                                 rec.partition
                             , rec.offset + 1
                             )
                           | rec <- recs
                           ]

            -- The /commit/ goes through the EOS coordinator: under
            -- AtLeastOnce this is the no-op coordinator and the
            -- behaviour is identical to the old (commitEngine +
            -- commitSync); under ExactlyOnceV2 it sequences the
            -- transactional begin/commit envelope around the engine
            -- flush and the consumer offset commit.
            coord <- readIORef (ksEosCoord ks)
            outcome <- runCommitCycle
              coord
              (applicationId (ksConfig ks))
              (pure commitOffsets)
              (commitEngine engine)
            case outcome of
              CommitSucceeded -> do
                _ <- sdConsumerCommit driver
                bumpTick ks
                go
              CommitAborted reason -> do
                -- Aborted: the txn was rolled back; we keep running
                -- but skip committing consumer offsets so the next
                -- poll re-reads them.
                putStrLn ("[streams] commit aborted: "
                  <> Text.unpack reason)
                bumpTick ks
                go
              CommitFatal reason ->
                transitionTo ks (StreamsError reason)

----------------------------------------------------------------------
-- Multi-thread event loop
----------------------------------------------------------------------

-- | The N-worker variant. One async thread polls the consumer
-- and dispatches each polled record into the worker pool by
-- @hash (topic, partition) mod N@. After every poll we wait for
-- worker-side quiescence, drain every worker's collector
-- through the producer, then commit consumer offsets — same
-- shape as the single-thread loop, but the engine work happens
-- in parallel across workers.
--
-- The "one consumer, N workers" topology is a deliberate
-- simplification of Java's "one consumer per StreamThread"
-- model. It uses a single consumer connection (less network
-- overhead) and lets per-partition state stay coherent because
-- each (topic, partition) consistently lands on the same
-- worker. Tradeoff: rebalance reassignments don't redistribute
-- store state across workers — they stay where they were
-- hashed. Documented in 'streams/README.md'.
multiEventLoop :: KafkaStreams -> StreamDriver -> WorkerPool -> IO ()
multiEventLoop ks driver pool = go
  where
    go = do
      yield  -- cooperative fairness; see 'eventLoop'
      status <- readTVarIO (ksStatus ks)
      unless (status == StreamsClosing || status == StreamsClosed) $ do
        drainRebalances ks driver
        maybeIssueProbingRebalance ks driver
        expireStandbys ks
        eRecs <- sdConsumerPoll driver (pollMs (ksConfig ks))
        case eRecs of
          Left err ->
            transitionTo ks (StreamsError (Text.pack err))
          Right recs -> do
            paused <- readTVarIO (ksPaused ks)
            unless paused $
              forM_ recs $ \rec ->
                feedWithHandler ks "<source>" rec $
                  submitRecordHashed pool
                    (topicName rec.topic)
                    rec.key
                    rec.value
                    (Timestamp rec.timestamp)
                    (fromIntegral rec.partition)
            -- Wait until every just-submitted record is processed.
            waitForQuiescence pool

            -- Sync each worker engine's wall clock to real time
            -- + fan out the poll-cycle notification across every
            -- worker. Each worker has its own coordinator handle
            -- (attached by user code, if any); the call is a
            -- no-op when no coordinator is wired.
            V.forM_ (poolWorkers pool) $ \w -> do
              syncWallClockToReal (workerEngine w)
            let !activeTopics = HashSet.fromList
                  [ topicName rec.topic | rec <- recs ]
            V.forM_ (poolWorkers pool) $ \w ->
              reportPollCycle (workerEngine w) activeTopics

            let !commitOffsets =
                  if paused
                    then HashMap.empty
                    else HashMap.fromListWith max
                           [ ( KC.TopicPartition
                                 rec.topic
                                 rec.partition
                             , rec.offset + 1
                             )
                           | rec <- recs
                           ]

            coord <- readIORef (ksEosCoord ks)
            outcome <- runCommitCycle
              coord
              (applicationId (ksConfig ks))
              (pure commitOffsets)
              (drainWorkersThroughDriver ks driver pool)
            case outcome of
              CommitSucceeded -> do
                _ <- sdConsumerCommit driver
                bumpTick ks
                go
              CommitAborted reason -> do
                putStrLn ("[streams] commit aborted: "
                  <> Text.unpack reason)
                bumpTick ks
                go
              CommitFatal reason ->
                transitionTo ks (StreamsError reason)

-- | Commit-cycle body for the multi-thread loop: commit each
-- worker's engine (flushing state stores), drain every
-- worker's in-memory collector through the producer, and flush
-- the producer once at the end. The producer flush is the
-- single sync barrier guaranteeing every record made it to
-- the broker before consumer offsets advance.
drainWorkersThroughDriver
  :: KafkaStreams
  -> StreamDriver
  -> WorkerPool
  -> IO ()
drainWorkersThroughDriver ks driver pool = do
  commitAllWorkers pool
  V.forM_ (poolWorkers pool) $ \w -> do
    pairs <- drainCollector (workerCollector w)
    forM_ pairs $ \(t, rs) ->
      forM_ rs $ \cr -> do
        r <- sdProducerSend driver (unTopicName t) (crKey cr) (crValue cr)
        case r of
          Right _ -> pure ()
          Left err -> handleProdFail ks (unTopicName t) err
  rF <- sdProducerFlush driver
  case rF of
    Right () -> pure ()
    Left err -> handleProdFail ks "" err

----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

closeKafkaStreams :: KafkaStreams -> IO ()
closeKafkaStreams ks = closeKafkaStreamsWith ks defaultCloseOptions

streamsStatus :: KafkaStreams -> IO StreamsStatus
streamsStatus = readTVarIO . ksStatus

-- | Block until 'streamsStatus' reaches the given @target@ /or/ the
-- instance settles into a /final/ state ('StreamsClosed' or
-- 'StreamsError') from which @target@ can no longer be reached —
-- whichever happens first.
--
-- This mirrors the terminal-awareness 'awaitTicks' already has, and
-- exists for the same reason: a plain @retry until cur == target@
-- deadlocks when the runtime races past (or dies before) the
-- awaited state. Under parallel load a fatal record can drive
-- @StreamsRunning -> StreamsError@ before a waiter on
-- @StreamsRunning@ observes it, after which the exact-equality wait
-- would 'retry' forever.
--
-- 'StreamsClosing' is deliberately /not/ treated as final: it always
-- proceeds to 'StreamsClosed' (see 'closeKafkaStreamsWith'), so
-- @'awaitState' ks 'StreamsClosed'@ still waits through it rather
-- than returning early.
--
-- Returns @()@ in both the reached-target and settled-elsewhere
-- cases; a caller that must distinguish them follows up with
-- 'streamsStatus'. (Used by tests for deterministic,
-- 'threadDelay'-free state coordination.)
awaitState :: KafkaStreams -> StreamsStatus -> IO ()
awaitState ks target = atomically $ do
  cur <- readTVar (ksStatus ks)
  let isFinal = case cur of
        StreamsClosed  -> True
        StreamsError _ -> True
        _              -> False
  if cur == target || isFinal
    then pure ()
    else retry

-- | Install a callback that fires on every state transition.
-- Mirrors @KafkaStreams.setStateListener@.
setStateListener :: KafkaStreams -> StateListener -> IO ()
setStateListener ks lis = writeIORef (ksListener ks) lis

-- | Internal helper: atomically transition to a new state and
-- invoke the registered listener.
transitionTo :: KafkaStreams -> StreamsStatus -> IO ()
transitionTo ks new_ = do
  old <- atomically $ do
    cur <- readTVar (ksStatus ks)
    writeTVar (ksStatus ks) new_
    pure cur
  lis <- readIORef (ksListener ks)
  lis old new_

-- | Pause record processing. The runtime keeps polling
-- the consumer (so heartbeats stay alive and the coordinator
-- doesn't kick the member) but does not feed the engine. Call
-- 'resumeKafkaStreams' to continue.
pauseKafkaStreams :: KafkaStreams -> IO ()
pauseKafkaStreams ks = atomically (writeTVar (ksPaused ks) True)

-- | Resume processing after 'pauseKafkaStreams'.
resumeKafkaStreams :: KafkaStreams -> IO ()
resumeKafkaStreams ks = atomically (writeTVar (ksPaused ks) False)

isPausedKafkaStreams :: KafkaStreams -> IO Bool
isPausedKafkaStreams = readTVarIO . ksPaused

-- | One row of task-lag information. Mirrors Java's
-- @org.apache.kafka.streams.LagInfo@.
data LagInfo = LagInfo
  { lagTaskId        :: !TaskId
  , lagCurrentOffset :: !Int64
  , lagEndOffset     :: !Int64
  }
  deriving stock (Eq, Show)

type LagListener = [LagInfo] -> IO ()

-- | Register a callback that receives a snapshot of every task's
-- current vs. end offset on every 'publishLag' call. Mirrors
-- @KafkaStreams.allLocalStorePartitionLags@ + the periodic
-- listener pattern in KIP-647.
setLagListener :: KafkaStreams -> LagListener -> IO ()
setLagListener ks lis = writeIORef (ksLagLis ks) lis

-- | Internal helper for the runtime (or tests) to publish a fresh
-- lag snapshot to the registered listener.
publishLag :: KafkaStreams -> [LagInfo] -> IO ()
publishLag ks lags = do
  lis <- readIORef (ksLagLis ks)
  lis lags

-- | Replace the runtime's EOS coordinator. The default is
-- 'noopEOSCoordinator'; tests inject a recording coordinator to
-- assert the call sequence, and a future broker-backed runtime
-- will swap in 'newRealEOSCoordinator' built from a
-- transactional 'Kafka.Client.Transaction.Transaction' once the
-- producer routes sends through the txn state machine.
applyEOSCoordinator :: KafkaStreams -> EOSCoordinator -> IO ()
applyEOSCoordinator ks coord =
  writeIORef (ksEosCoord ks) coord

----------------------------------------------------------------------
-- Multi-instance rebalance (KIP-415/429/441/869)
----------------------------------------------------------------------

-- | Install a 'Kafka.Client.RebalanceListener.RebalanceListener'
-- that the runtime fires on every assign / revoke / lost event
-- it drains from the driver. Mirrors Java's
-- @KafkaStreams.setStateListener(...).onPartitionsAssigned(...)@
-- contract.
--
-- The runtime always updates its internal 'ksOwned' /
-- 'ksStandbys' bookkeeping before calling the listener, so
-- 'ownedPartitions' / 'standbyTasks' inside the callback see
-- the post-event state.
setRebalanceListener :: KafkaStreams -> RebalanceListener -> IO ()
setRebalanceListener ks l = writeIORef (ksRebLis ks) l

-- | The partitions this instance is currently actively
-- processing (assigned and not revoked). Standby tasks in their
-- KIP-869 grace window are not included; see 'standbyTasks' for
-- that.
ownedPartitions :: KafkaStreams -> IO [KC.TopicPartition]
ownedPartitions ks = HashSet.toList <$> readTVarIO (ksOwned ks)

-- | Snapshot of partitions held in standby-revocation grace. Each
-- entry maps the (topic, partition) to its grace-expiry
-- deadline in epoch milliseconds. After the deadline the
-- partition is dropped from this map on the next event-loop
-- tick.
standbyTasks :: KafkaStreams -> IO (Map.Map KC.TopicPartition Int64)
standbyTasks = readTVarIO . ksStandbys

-- | Drain every 'RebalanceEvent' the driver has queued, update
-- 'ksOwned' / 'ksStandbys' atomically, and fire the user
-- listener.
drainRebalances :: KafkaStreams -> StreamDriver -> IO ()
drainRebalances ks driver = loop
  where
    loop = do
      mEv <- sdRebalanceEvent driver
      case mEv of
        Nothing                       -> pure ()
        Just (RebalanceAssigned tps)  -> do
          atomically $ do
            modifyTVar' (ksOwned ks)
              (\s -> foldr HashSet.insert s tps)
            -- A re-assignment of a partition we're holding in
            -- standby promotes it back to active.
            modifyTVar' (ksStandbys ks)
              (\m -> foldr Map.delete m tps)
          lis <- readIORef (ksRebLis ks)
          RBL.dispatchAssigned lis tps
          loop
        Just (RebalanceRevoked tps)   -> do
          now <- nowMillis
          let !graceMs = max 0 (taskTimeoutMs (ksConfig ks))
          atomically $ do
            modifyTVar' (ksOwned ks)
              (\s -> foldr HashSet.delete s tps)
            modifyTVar' (ksStandbys ks) $ \m ->
              foldr
                (\tp acc -> case classifyRevocation now graceMs of
                   RevokeImmediate    -> acc
                   KeepAsStandby dead -> Map.insert tp dead acc)
                m
                tps
          lis <- readIORef (ksRebLis ks)
          RBL.dispatchRevoked lis tps
          loop
        Just (RebalanceLost tps)      -> do
          atomically $ do
            modifyTVar' (ksOwned ks)
              (\s -> foldr HashSet.delete s tps)
            -- Lost partitions skip the grace window entirely
            -- (the broker fenced us; serving stale reads from
            -- the prior state would be wrong).
            modifyTVar' (ksStandbys ks)
              (\m -> foldr Map.delete m tps)
          lis <- readIORef (ksRebLis ks)
          RBL.dispatchLost lis tps
          loop

-- | Drop any standby tasks whose grace deadline has elapsed.
expireStandbys :: KafkaStreams -> IO ()
expireStandbys ks = do
  now <- nowMillis
  atomically $
    modifyTVar' (ksStandbys ks) (Map.filter (> now))

----------------------------------------------------------------------
-- Exception handlers (KIP-161 / 280 / 671 / 1033)
----------------------------------------------------------------------

-- | Install a production-exception handler. The
-- runtime calls it whenever the driver's flush-through-producer
-- step reports a send failure; if the handler returns
-- 'ProdFailFast' the stream-thread is failed and routed to
-- the uncaught-exception handler.
setProductionExceptionHandler
  :: KafkaStreams
  -> ProductionHandler
  -> IO ()
setProductionExceptionHandler ks h =
  writeIORef (ksProdHand ks) h

-- | Install a processing-exception handler. The
-- runtime calls it when 'feedSource' / a processor throws.
setProcessingExceptionHandler
  :: KafkaStreams
  -> ProcessingExceptionHandler
  -> IO ()
setProcessingExceptionHandler ks h =
  writeIORef (ksProcHand ks) h

-- | Install an uncaught-exception handler. The runtime
-- calls it when the stream-thread async dies with an
-- exception the per-record handlers didn't catch.
--
--   * 'ReplaceThread'       — respawn the worker async;
--   * 'ShutdownClient'      — transition this instance to
--                             'StreamsError' and close;
--   * 'ShutdownApplication' — same as 'ShutdownClient' for
--                             /this/ process; broadcasting to
--                             other instances is the user's
--                             responsibility (their handler
--                             can publish a sentinel to a
--                             coordination topic).
setUncaughtExceptionHandler
  :: KafkaStreams
  -> StreamsUncaughtExceptionHandler
  -> IO ()
setUncaughtExceptionHandler ks h =
  writeIORef (ksUncaught ks) h

----------------------------------------------------------------------
-- KIP-441 probing rebalance
----------------------------------------------------------------------

-- | Report the current changelog lag for a warmup replica.
-- Called from a standby-task replay loop after each batch is
-- applied so the runtime knows how close the replica is to
-- the active leader.
--
-- A lag of @0@ means the replica is caught up and a probing
-- rebalance would promote it.
reportWarmupLag :: KafkaStreams -> TaskId -> Int64 -> IO ()
reportWarmupLag ks tid !lag =
  atomically $ modifyTVar' (ksWarmupLag ks) (Map.insert tid lag)

-- | Drop a task from the warmup-lag map (typically called
-- when the replica has been promoted or the standby is
-- closed).
clearWarmupLag :: KafkaStreams -> TaskId -> IO ()
clearWarmupLag ks tid =
  atomically $ modifyTVar' (ksWarmupLag ks) (Map.delete tid)

-- | Snapshot of the current warmup-lag map. Used by metrics
-- and by the runtime's probe-cadence tick.
warmupSnapshot :: KafkaStreams -> IO (Map.Map TaskId Int64)
warmupSnapshot = readTVarIO . ksWarmupLag

-- | Internal: evaluate 'shouldProbe' against the runtime's
-- current state and, if it returns 'True', invoke the
-- driver's 'sdRequestProbingRebalance' hook. Called once per
-- event-loop iteration.
maybeIssueProbingRebalance :: KafkaStreams -> StreamDriver -> IO ()
maybeIssueProbingRebalance ks drv = do
  let cfg = ksConfig ks
      !intervalMs = probingRebalanceIntervalMs cfg
      !lagThresh  = acceptableRecoveryLag cfg
  now <- nowMillis
  lastAt <- readTVarIO (ksLastProbeAt ks)
  warmups <- readTVarIO (ksWarmupLag ks)
  let !ws =
        [ ProbingRebalance.WarmupProgress
            { ProbingRebalance.task = t
            , ProbingRebalance.lag  = lag
            }
        | (t, lag) <- Map.toList warmups
        ]
  if ProbingRebalance.shouldProbe
       now lastAt intervalMs ws lagThresh
    then do
      atomically $ writeTVar (ksLastProbeAt ks) now
      sdRequestProbingRebalance drv
    else pure ()

----------------------------------------------------------------------
-- KIP-663: dynamic stream-thread management
----------------------------------------------------------------------

-- | Add a stream-thread at runtime. Mirrors Java's
-- @KafkaStreams.addStreamThread()@: returns the new total
-- thread count on success, or 'Nothing' if the runtime isn't
-- running (or doesn't have a worker pool — i.e. it was started
-- with @numStreamThreads = 1@).
--
-- The added thread participates in hash-routed dispatch from
-- its first 'submitRecordHashed' call. Existing routing
-- entries don't migrate to the new worker — state-store
-- locality stays with the worker that already holds it.
addStreamThread :: KafkaStreams -> IO (Maybe Int)
addStreamThread ks = do
  mPool <- readIORef (ksPool ks)
  case mPool of
    Nothing   -> pure Nothing
    Just pool -> Just <$> addPoolWorker pool

-- | Remove a stream-thread at runtime. Drains the worker's
-- inbox, cancels its async, closes its engine, and rebalances
-- the routing table so subsequent records on the removed
-- worker's partitions re-hash onto a remaining worker.
--
-- Returns 'Just newCount' on success, 'Nothing' when there's
-- no worker pool to remove from (single-thread runtime).
removeStreamThread :: KafkaStreams -> IO (Maybe Int)
removeStreamThread ks = do
  mPool <- readIORef (ksPool ks)
  case mPool of
    Nothing   -> pure Nothing
    Just pool -> removePoolWorker pool

-- | Number of stream-threads currently in the worker pool.
-- Returns 1 for a single-thread runtime.
streamThreadCount :: KafkaStreams -> IO Int
streamThreadCount ks = do
  mPool <- readIORef (ksPool ks)
  case mPool of
    Nothing   -> pure 1
    Just pool -> poolWorkerCount pool

-- | Wipe local state before re-starting. Mirrors Java's
-- @KafkaStreams.cleanUp()@: drops in-memory store contents +
-- resets the tick counter. Only safe to call when the runtime
-- is in 'StreamsCreated' or 'StreamsClosed' state.
cleanUp :: KafkaStreams -> IO ()
cleanUp ks = do
  st <- readTVarIO (ksStatus ks)
  case st of
    StreamsCreated -> doCleanUp
    StreamsClosed  -> doCleanUp
    other ->
      error $ "cleanUp: runtime not stopped (current state: "
        <> show other <> ")"
  where
    doCleanUp = do
      mE <- readIORef (ksEngine ks)
      forM_ mE closeEngine
      writeIORef (ksEngine ks) Nothing
      mPool <- readIORef (ksPool ks)
      forM_ mPool closeWorkerPool
      writeIORef (ksPool ks) Nothing
      atomically $ do
        writeTVar (ksTicks ks) 0
        writeTVar (ksOwned ks) HashSet.empty
        writeTVar (ksStandbys ks) Map.empty

----------------------------------------------------------------------
-- KIP-812: CloseOptions
----------------------------------------------------------------------

-- | Options passed to 'closeKafkaStreamsWith'. Mirrors Java's
-- @KafkaStreams.CloseOptions@.
data CloseOptions = CloseOptions
  { timeoutMs  :: !(Maybe Int)
  , leaveGroup :: !Bool
    -- ^ When 'True' the consumer issues a LeaveGroup so the
    --   broker rebalances immediately instead of waiting for
    --   the session timeout (KIP-812).
  }
  deriving stock (Eq, Show)

defaultCloseOptions :: CloseOptions
defaultCloseOptions = CloseOptions
  { timeoutMs  = Just 30_000
  , leaveGroup = True
  }

-- | Close with explicit options. Threads the
-- @leaveGroup@ flag into the consumer's close path via the
-- driver's 'sdConsumerCloseWith': when @opts.leaveGroup@ is
-- 'True' (the default) the consumer sends a @LeaveGroup@ so
-- the broker rebalances immediately; when 'False' it skips
-- the RPC and relies on the session-timeout reassignment.
-- @opts.timeoutMs@ bounds how long the consumer waits for the
-- leave-group ack.
closeKafkaStreamsWith :: KafkaStreams -> CloseOptions -> IO ()
closeKafkaStreamsWith ks opts = do
  transitionTo ks StreamsClosing
  mAh <- readIORef (ksThread ks)
  case mAh of
    Just ah -> do
      cancel ah
      _ <- waitCatch ah
      pure ()
    Nothing -> pure ()
  mE <- readIORef (ksEngine ks)
  forM_ mE closeEngine
  mPool <- readIORef (ksPool ks)
  forM_ mPool closeWorkerPool
  mD <- readIORef (ksDriver ks)
  forM_ mD $ \drv -> do
    sdConsumerCloseWith drv
      opts.leaveGroup
      (maybe 30_000 id opts.timeoutMs)
    sdProducerClose drv
  transitionTo ks StreamsClosed

----------------------------------------------------------------------
-- KIP-988 standby update + global state restore listeners
----------------------------------------------------------------------

-- | Listener fired on every standby-task state replay step.
-- Mirrors Java's @StandbyUpdateListener@ (KIP-988).
type StandbyUpdateListener =
  KC.TopicPartition          -- which standby partition
  -> Int64                   -- the offset we just replayed up to
  -> IO ()

-- | Install a standby-update listener. The runtime calls it
-- once per replay batch; user code typically logs progress or
-- exports a metric.
setStandbyUpdateListener
  :: KafkaStreams -> StandbyUpdateListener -> IO ()
setStandbyUpdateListener ks lis =
  writeIORef (ksStandbyLis ks) lis

-- | Listener fired during global-store restore. Same contract
-- as the JVM's @StateRestoreListener@.
type GlobalStateRestoreListener =
  Text                       -- store name
  -> Int64                   -- offset just replayed
  -> IO ()

setGlobalStateRestoreListener
  :: KafkaStreams -> GlobalStateRestoreListener -> IO ()
setGlobalStateRestoreListener ks lis =
  writeIORef (ksGlobalRestoreLis ks) lis

-- | Full state-restore listener matching the JVM
-- @org.apache.kafka.streams.processor.StateRestoreListener@
-- 4-method interface:
--
--   * 'onRestoreStart'     — fired once at the beginning of a
--     store's restore, with the starting + exclusive ending
--     offsets of the changelog slice to replay.
--   * 'onBatchRestored'    — fired after each batch the runtime
--     pulls off the changelog; @numRestored@ is the count for
--     /this batch/, not the running total.
--   * 'onRestoreEnd'       — fired once at successful completion.
--   * 'onRestoreSuspended' — fired when the active task hosting
--     this store migrates out before the restore finished. If
--     the task comes back, a fresh 'onRestoreStart' fires.
--
-- The 'GlobalStateRestoreListener' alias kept above is the
-- minimal shape (a single @\\ name offset -> IO ()@). Use
-- 'StateRestoreListener' when you want the full event surface;
-- 'setStateRestoreListener' adapts the record into the
-- minimal hook the runtime already calls.
data StateRestoreListener = StateRestoreListener
  { onRestoreStart
      :: !(KC.TopicPartition -> Text -> Int64 -> Int64 -> IO ())
  , onBatchRestored
      :: !(KC.TopicPartition -> Text -> Int64 -> Int64 -> IO ())
  , onRestoreEnd
      :: !(KC.TopicPartition -> Text -> Int64 -> IO ())
  , onRestoreSuspended
      :: !(KC.TopicPartition -> Text -> Int64 -> IO ())
  }

-- | A listener that does nothing on every event. Useful as a
-- starting point for record-update syntax:
--
-- @
-- 'setStateRestoreListener' ks $
--   'defaultStateRestoreListener'
--     { 'onRestoreStart' = \\tp store start end ->
--         hPutStrLn stderr (\"restore-start \" <> show (tp, store, start, end))
--     }
-- @
defaultStateRestoreListener :: StateRestoreListener
defaultStateRestoreListener = StateRestoreListener
  { onRestoreStart     = \_ _ _ _ -> pure ()
  , onBatchRestored    = \_ _ _ _ -> pure ()
  , onRestoreEnd       = \_ _ _   -> pure ()
  , onRestoreSuspended = \_ _ _   -> pure ()
  }

-- | Install a 'StateRestoreListener'. Adapts to the existing
-- 'GlobalStateRestoreListener' hook (which the runtime fires
-- per replayed offset) by routing each fire to
-- 'onBatchRestored' with a @numRestored = 1@ count and a
-- synthetic 'KC.TopicPartition' for the store's changelog.
-- 'onRestoreStart' / 'onRestoreEnd' / 'onRestoreSuspended' are
-- fired by the runtime's standby + active-task lifecycle
-- transitions; if the runtime hasn't been wired with those
-- transitions yet they're no-ops, but the listener record's
-- shape stays JVM-equivalent.
setStateRestoreListener :: KafkaStreams -> StateRestoreListener -> IO ()
setStateRestoreListener ks lis = do
  -- Bridge: every replayed offset fires onBatchRestored with a
  -- batch size of 1 (the runtime doesn't currently batch the
  -- replay callback). The store-name is what GlobalStateRestoreListener
  -- gets; we use a sentinel TopicPartition because the runtime
  -- doesn't expose the per-store changelog partition here.
  let bridge :: GlobalStateRestoreListener
      bridge name off =
        onBatchRestored lis (KC.TopicPartition name 0) name off 1
  writeIORef (ksGlobalRestoreLis ks) bridge
  writeIORef (ksRestoreListener ks) (Just lis)

----------------------------------------------------------------------
-- KIP-444 metrics + metadata
----------------------------------------------------------------------

-- | Per-thread snapshot. Mirrors Java's
-- @ThreadMetadata@ (subset).
data LocalThreadMetadata = LocalThreadMetadata
  { threadId      :: !Int
  , assigned      :: ![KC.TopicPartition]
  , processedRecs :: !Int64
  }
  deriving stock (Eq, Show)

-- | Per-thread metadata snapshot for the local runtime.
-- Returns one entry per worker in the pool (one for a
-- single-thread runtime).
metadataForLocalThreads :: KafkaStreams -> IO [LocalThreadMetadata]
metadataForLocalThreads ks = do
  owned <- HashSet.toList <$> readTVarIO (ksOwned ks)
  mPool <- readIORef (ksPool ks)
  case mPool of
    Nothing -> pure [LocalThreadMetadata 0 owned 0]
    Just pool -> V.toList <$>
      V.imapM
        (\i w -> do
            !cnt <- workerProcessedCount w
            pure LocalThreadMetadata
              { threadId      = i
              , assigned      = owned
              , processedRecs = cnt
              })
        (poolWorkers pool)

-- | Snapshot of every registered metric + the current runtime
-- state. Mirrors Java's @KafkaStreams.metrics()@ +
-- @KafkaStreams.state()@ in a single call.
metricsAndState
  :: KafkaStreams
  -> IO (StreamsStatus, [LocalThreadMetadata])
metricsAndState ks = do
  st <- streamsStatus ks
  ms <- metadataForLocalThreads ks
  pure (st, ms)

-- | Returns the streams instance's client-instance id (KIP-714).
-- Mirrors @KafkaStreams.clientInstanceIds(Duration)@ on the JVM —
-- which on a 4.0 cluster simply forwards the consumer's /
-- producer's / admin's broker-assigned UUIDs back to the caller.
-- Until the broker-side telemetry pipeline lands, this returns a
-- deterministic locally-derived UUID padded from the configured
-- @application.id@; same encoding the consumer + producer +
-- admin clients use.
kafkaStreamsClientInstanceId
  :: KafkaStreams -> IO BS.ByteString
kafkaStreamsClientInstanceId ks =
  pure (Telemetry.clientInstanceIdFromText (applicationId (ksConfig ks)))

-- | Build a 'PA.ApplicationState' snapshot from this instance's
-- live runtime view. Mirrors what the JVM streams runtime
-- passes to a user-supplied @TaskAssignor@ on the leader side
-- of a rebalance.
--
-- The snapshot is single-instance: this is the view the local
-- runtime has of its own tasks. On a multi-instance cluster
-- the JVM SDK aggregates state from every member via the
-- group-coordination subscription metadata; that aggregation
-- isn't wired here yet, but the 'PA.ApplicationState' returned
-- is the shape the plug-in expects so user code can be tested
-- end-to-end against the local view.
streamsApplicationState :: KafkaStreams -> IO PA.ApplicationState
streamsApplicationState ks = do
  owned    <- readTVarIO (ksOwned ks)
  standbys <- readTVarIO (ksStandbys ks)
  -- Build per-instance state. The local ProcessId is derived
  -- deterministically from the application id so re-runs against
  -- the same config produce the same id.
  let !cfg = ksConfig ks
      !pid = PA.ProcessId (UUID.fromWords 0 0 0
                            (fromIntegral
                              (Text.length (applicationId cfg))))
      ownedSet  = HashMap.toList (HashSet.toMap owned)
      tasksFromOwned =
        Map.fromList
          [ ( TaskId 0 tp.partition
            , PA.TaskInfo
                { PA.tiTaskId          = TaskId 0 tp.partition
                , PA.tiTopicPartitions = Set.singleton (PA.TaskTopicPartition
                    { PA.ttpTopic       = tp.topic
                    , PA.ttpPartition   = tp.partition
                    , PA.ttpIsChangelog = False
                    })
                , PA.tiIsStateful = False
                , PA.tiStores     = Set.empty
                }
            )
          | (tp, _) <- ownedSet
          ]
      activeTids = Map.keysSet tasksFromOwned
      standbyTids = Set.fromList
        [ TaskId 0 tp.partition | tp <- Map.keys standbys ]
      kss = PA.KafkaStreamsState
              { PA.kssProcessId            = pid
              , PA.kssNumProcessingThreads = numStreamThreads cfg
              , PA.kssClientTags           = Map.empty
              , PA.kssPreviousActiveTasks  = activeTids
              , PA.kssPreviousStandbyTasks = standbyTids
              , PA.kssRackId               = Nothing
              }
  pure PA.ApplicationState
    { PA.asAllTasks           = tasksFromOwned
    , PA.asKafkaStreamsStates = Map.singleton pid kss
    , PA.asAssignmentConfigs  = PA.AssignmentConfigs
        { PA.acNumStandbyReplicas      = numStandbyReplicas cfg
        , PA.acAcceptableRecoveryLag   = fromIntegral (acceptableRecoveryLag cfg)
        , PA.acMaxWarmupReplicas       = maxWarmupReplicas cfg
        , PA.acProbingRebalanceIntervalMs = probingRebalanceIntervalMs cfg
        , PA.acRackAware               = PA.defaultRackAwareAssignmentConfigs
        }
    }

-- | Invoke the user-configured 'PA.TaskAssignor' against the
-- current 'streamsApplicationState'. Returns 'Nothing' when no
-- plug-in is configured ('Kafka.Streams.Config.taskAssignor =
-- Nothing'); returns @Just@ the assignor's output otherwise.
--
-- The runtime itself doesn't consume this value yet — the
-- leader-side assignment path uses the built-in cooperative-
-- sticky assignor — but the wiring is in place so user code
-- can drive the plug-in deterministically.
streamsRunUserAssignor :: KafkaStreams -> IO (Maybe PA.TaskAssignment)
streamsRunUserAssignor ks =
  case taskAssignor (ksConfig ks) of
    Nothing  -> pure Nothing
    Just ta_ -> do
      app <- streamsApplicationState ks
      Just <$> PA.taAssign ta_ app

-- | Run the supplied event loop with the configured uncaught-exception
-- handling. On exception the user handler decides whether to
-- respawn the loop, shut this instance down, or shut the whole
-- application down. 'ReplaceThread' loops back to run the body
-- again; the other two responses transition the runtime into
-- 'StreamsError' / 'StreamsClosing' and return.
supervisedLoop
  :: KafkaStreams
  -> StreamDriver
  -> IO ()
  -> IO ()
supervisedLoop ks _drv body = go
  where
    go = do
      r <- try body :: IO (Either SomeException ())
      case r of
        Right () -> pure ()
        Left e -> do
          h <- readIORef (ksUncaught ks)
          resp <- runStreamsUncaughtExceptionHandler h e
          case resp of
            ReplaceThread       -> go
            ShutdownClient      ->
              transitionTo ks
                (StreamsError ("uncaught (shutdown-client): "
                                <> Text.pack (show e)))
            ShutdownApplication ->
              transitionTo ks
                (StreamsError ("uncaught (shutdown-app): "
                                <> Text.pack (show e)))

----------------------------------------------------------------------
-- Progress signal (for tests)
----------------------------------------------------------------------

-- | Read the runtime's tick counter. Bumped at the bottom of
-- every event-loop iteration (single-thread and multi-thread).
-- Tests block on this via 'awaitTicks' to coordinate with
-- engine progress deterministically — no 'threadDelay'
-- involved.
ksTickCount :: KafkaStreams -> IO Int
ksTickCount ks = readTVarIO (ksTicks ks)

-- | Block until 'ksTicks' has advanced by at least @n@ since
-- the call /or/ the runtime hits a terminal state
-- ('StreamsClosing' / 'StreamsClosed' / 'StreamsError'),
-- whichever happens first. Returns the new tick count. Used
-- by tests to wait for "the engine ran at least @n@ more
-- times" without deadlocking when the loop has died.
awaitTicks :: KafkaStreams -> Int -> IO Int
awaitTicks ks n = do
  start <- readTVarIO (ksTicks ks)
  atomically $ do
    cur <- readTVar (ksTicks ks)
    st  <- readTVar (ksStatus ks)
    let !terminal = case st of
          StreamsClosing -> True
          StreamsClosed  -> True
          StreamsError _ -> True
          _              -> False
    if terminal || cur >= start + n
      then pure cur
      else retry

-- | Increment the tick counter. Called once per event-loop
-- iteration; internal.
bumpTick :: KafkaStreams -> IO ()
bumpTick ks = atomically (modifyTVar' (ksTicks ks) (+ 1))

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

sourceTopics :: Topo.TopologyValid -> [TopicName]
sourceTopics tv =
  let topo = Topo.topologyValidGraph tv
   in concatMap Topo.sourceTopics
        (Map.elems (Topo.topoSources topo))
