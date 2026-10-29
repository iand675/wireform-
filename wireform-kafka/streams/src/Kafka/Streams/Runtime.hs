{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime
Description : The broker-backed Kafka Streams runtime

@KafkaStreams@ runs a topology against a cluster the way the JVM
client does:

  * At start it works out the subtopologies, their task counts and
    the internal topics ("Kafka.Streams.Runtime.Plan"), creates the
    repartition and changelog topics, bootstraps the global stores
    ("Kafka.Streams.Runtime.GlobalStore") and joins the consumer group
    named by @application.id@.
  * The group hands it partitions; each (subtopology, partition) is a
    task ("Kafka.Streams.Runtime.Tasks") whose stores are restored
    from their changelogs before it processes anything.
  * One thread polls; between polls the tasks process their records in
    parallel over @num.stream.threads@ workers. Every
    @commit.interval.ms@ (or when a processor asks, before a rebalance,
    and at close) it commits: at least once by committing offsets after
    flushing; exactly once (v2) in one producer transaction carrying
    the output, the changelog writes and the consumed offsets, fenced
    by the group generation. An aborted transaction rewinds every task
    to the last commit and restores it from the changelogs.
  * Errors go through the configured handlers: deserialization,
    processing (KIP-1033) and production (at least once) failures, and
    the uncaught-exception handler (KIP-671) for anything that stops
    the stream thread — @replaceThreadOnException@ recovers every task
    and carries on.

'startKafkaStreams' talks to the cluster in 'bootstrapServers';
'startKafkaStreamsWith' takes any 'StreamDriver', e.g. one over the
in-memory "Kafka.Streams.Runtime.MockBroker".
-}
module Kafka.Streams.Runtime (
  KafkaStreams,
  newKafkaStreams,
  startKafkaStreams,
  startKafkaStreamsWith,
  closeKafkaStreams,

  -- * Status
  StreamsStatus (..),
  streamsStatus,
  awaitState,
  StateListener,
  setStateListener,

  -- * Pause / resume
  pauseKafkaStreams,
  resumeKafkaStreams,
  isPausedKafkaStreams,

  -- * Task lag
  LagInfo (..),
  LagListener,
  setLagListener,
  publishLag,

  -- * Rebalance
  setRebalanceListener,
  ownedPartitions,

  -- * Probing rebalance
  reportWarmupLag,
  clearWarmupLag,
  warmupSnapshot,

  -- * Exception handlers
  setProductionExceptionHandler,
  setProcessingExceptionHandler,
  setUncaughtExceptionHandler,

  -- * Thread management + cleanup
  addStreamThread,
  removeStreamThread,
  streamThreadCount,
  cleanUp,

  -- * Close options
  CloseOptions (..),
  defaultCloseOptions,
  closeKafkaStreamsWith,

  -- * Restore listeners
  GlobalStateRestoreListener,
  setGlobalStateRestoreListener,
  StateRestoreListener (..),
  defaultStateRestoreListener,
  setStateRestoreListener,

  -- * Metadata + metrics
  LocalThreadMetadata (..),
  metadataForLocalThreads,
  metricsAndState,
  TaskSnapshot (..),
  localTasks,

  -- * KIP-714 client telemetry id
  kafkaStreamsClientInstanceId,

  -- * KIP-924 user-pluggable task assignor
  streamsRunUserAssignor,
  streamsApplicationState,

  -- * Progress signal (used by tests to coordinate without threadDelay)
  ksTickCount,
  awaitTicks,

  -- * Store access (used by Kafka.Streams.InteractiveQueries)
  StoreInstance (..),
  localStoreInstances,
) where

import Control.Concurrent (yield)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.RebalanceListener (
  RebalanceListener,
  noopRebalanceListener,
 )
import Kafka.Client.RebalanceListener qualified as RBL
import Kafka.Client.Telemetry qualified as Telemetry
import Kafka.Streams.Config (
  ProcessingGuarantee (..),
  StreamsConfig (..),
  effectiveCommitIntervalMs,
 )
import Kafka.Streams.Errors (
  ProcessingExceptionHandler,
  ProductionHandler,
  StreamsException (..),
  StreamsUncaughtExceptionHandler (..),
  UncaughtExceptionResponse (..),
  replaceThreadOnException,
 )
import Kafka.Streams.Internal.Engine (Engine, StoreEntry, engineOwnStores)
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Processor.Assignment qualified as PA
import Kafka.Streams.Runtime.Driver
import Kafka.Streams.Runtime.GlobalStore
import Kafka.Streams.Runtime.NativeDriver (newNativeDriver)
import Kafka.Streams.Runtime.Plan
import Kafka.Streams.Runtime.ProbingRebalance qualified as ProbingRebalance
import Kafka.Streams.Runtime.StateDir (markCleanShutdown, prepareStateDir)
import Kafka.Streams.Runtime.Tasks
import Kafka.Streams.State.Store (StoreName)
import Kafka.Streams.Time (nowMillis)
import Kafka.Streams.Topology qualified as Topo
import System.Directory (doesDirectoryExist, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)


-- | Lifecycle of the runtime.
data StreamsStatus
  = StreamsCreated
  | StreamsRebalancing
  -- ^ Started; restoring state or waiting for an assignment
  | StreamsRunning
  | StreamsClosing
  | StreamsClosed
  | StreamsError !Text
  deriving (Eq, Show)


-- | A user-installed callback fired on every state transition.
type StateListener =
  StreamsStatus -- old state
  -> StreamsStatus -- new state
  -> IO ()


{- | Listener fired as global stores bootstrap: the stores and the
offset just reached.
-}
type GlobalStateRestoreListener = Text -> Int64 -> IO ()


data Running = Running
  { rDriver :: !StreamDriver
  , rTasks :: !TaskManager
  , rGlobal :: !(Maybe GlobalStore)
  , rThread :: !(Async ())
  }


data KafkaStreams = KafkaStreams
  { ksConfig :: !StreamsConfig
  , ksTopology :: !Topo.TopologyValid
  , ksStatus :: !(TVar StreamsStatus)
  , ksRunning :: !(IORef (Maybe Running))
  , ksListener :: !(IORef StateListener)
  , ksPaused :: !(TVar Bool)
  , ksLagLis :: !(IORef LagListener)
  , ksRebLis :: !(IORef RebalanceListener)
  , ksTicks :: !(TVar Int)
  {- ^ Bumped at the bottom of every loop iteration. Tests block on
  this via 'awaitTicks' instead of sleeping.
  -}
  , ksProdHand :: !(IORef ProductionHandler)
  , ksProcHand :: !(IORef ProcessingExceptionHandler)
  , ksUncaught :: !(IORef StreamsUncaughtExceptionHandler)
  , ksGlobalRestoreLis :: !(IORef GlobalStateRestoreListener)
  , ksRestoreListener :: !(IORef StateRestoreListener)
  , ksWarmupLag :: !(TVar (Map.Map TaskId Int64))
  , ksLastProbeAt :: !(TVar Int64)
  , ksThreads :: !(IORef Int)
  , ksCleanStop :: !(IORef Bool)
  {- ^ Whether local state still matches the last commit: cleared by
  a failed commit or a stream-thread failure. Only then does close
  leave the clean-shutdown marker (see "Kafka.Streams.Runtime.StateDir").
  -}
  }


newKafkaStreams :: StreamsConfig -> Topo.TopologyValid -> IO KafkaStreams
newKafkaStreams cfg topo = do
  s <- newTVarIO StreamsCreated
  running <- newIORef Nothing
  lis <- newIORef (\_ _ -> pure ())
  pa <- newTVarIO False
  lagL <- newIORef (\_ -> pure ())
  reb <- newIORef noopRebalanceListener
  ticks <- newTVarIO 0
  prodH <- newIORef (defaultProductionHandler cfg)
  procH <- newIORef (defaultProcessingHandler cfg)
  uncH <- newIORef replaceThreadOnException
  grLis <- newIORef (\_ _ -> pure ())
  rLis <- newIORef defaultStateRestoreListener
  warmup <- newTVarIO Map.empty
  lastPr <- newTVarIO 0
  threads <- newIORef (max 1 (numStreamThreads cfg))
  cleanStop <- newIORef False
  pure
    KafkaStreams
      { ksConfig = cfg
      , ksTopology = topo
      , ksStatus = s
      , ksRunning = running
      , ksListener = lis
      , ksPaused = pa
      , ksLagLis = lagL
      , ksRebLis = reb
      , ksTicks = ticks
      , ksProdHand = prodH
      , ksProcHand = procH
      , ksUncaught = uncH
      , ksGlobalRestoreLis = grLis
      , ksRestoreListener = rLis
      , ksWarmupLag = warmup
      , ksLastProbeAt = lastPr
      , ksThreads = threads
      , ksCleanStop = cleanStop
      }


appDir :: StreamsConfig -> FilePath
appDir cfg = stateDir cfg </> Text.unpack (applicationId cfg)


-- | Start against the cluster in 'bootstrapServers'.
startKafkaStreams :: KafkaStreams -> IO ()
startKafkaStreams ks = do
  eDrv <- newNativeDriver (ksConfig ks)
  case eDrv of
    Left err -> setError ks ("connect: " <> Text.pack err)
    Right drv -> startKafkaStreamsWith ks drv


{- | Start against any 'StreamDriver'. Returns once the stream thread
is running (or the instance is in 'StreamsError'); restoring and
processing happen in the background.
-}
startKafkaStreamsWith :: KafkaStreams -> StreamDriver -> IO ()
startKafkaStreamsWith ks drv = do
  let cfg = ksConfig ks
      dir = appDir cfg
  -- Under exactly-once, state left by a run that didn't stop at a
  -- commit boundary may hold writes of aborted transactions: wipe it
  -- and restore from the changelogs.
  wiped <- prepareStateDir (processingGuarantee cfg) dir
  when wiped $
    logLine ("no clean shutdown recorded in " <> dir <> " under exactly-once: wiped local state")
  when (numStandbyReplicas cfg > 0) $
    logLine "num.standby.replicas > 0: standby replicas are not supported yet; tasks restore from changelogs on failover"
  transitionTo ks StreamsRebalancing
  r <- try (setUp dir)
  case r of
    Left (e :: SomeException) -> do
      sdClose drv
      setError ks (Text.pack (show e))
    Right (Left err) -> do
      sdClose drv
      setError ks err
    Right (Right ()) -> pure ()
  where
    setUp dir = do
      let cfg = ksConfig ks
      eInit <-
        if processingGuarantee cfg == ExactlyOnceV2
          then sdInitTransactions drv
          else pure (Right ())
      case eInit of
        Left e -> pure (Left ("init transactions: " <> Text.pack e))
        Right () -> do
          ePlan <- buildRuntimePlan (applicationId cfg) (replicationFactor cfg) drv (ksTopology ks)
          case ePlan of
            Left e -> pure (Left e)
            Right plan -> do
              grLis <- readIORef (ksGlobalRestoreLis ks)
              eGlobal <- case rpGlobal plan of
                Nothing -> pure (Right Nothing)
                Just gp ->
                  fmap Just
                    <$> startGlobalStore
                      drv
                      gp
                      (rpPartitionCounts plan)
                      (applicationId cfg)
                      dir
                      (defaultDeserHandler cfg)
                      (pollMs cfg)
                      (effectiveCommitIntervalMs cfg)
                      grLis
              case eGlobal of
                Left e -> pure (Left (Text.pack e))
                Right mGlobal -> do
                  shared <- maybe (pure Map.empty) globalStores mGlobal
                  threads <- readIORef (ksThreads ks)
                  eTm <-
                    newTaskManager
                      TaskManagerConfig
                        { tmcAppId = applicationId cfg
                        , tmcAppDir = dir
                        , tmcGuarantee = processingGuarantee cfg
                        , tmcThreads = threads
                        , tmcMaxTaskIdleMs = maxTaskIdleMs cfg
                        , tmcMaxBufferedPerPartition = max 1 (bufferedRecordsPerPartition cfg)
                        , tmcDeserHandler = defaultDeserHandler cfg
                        , tmcProcessingHandler = readIORef (ksProcHand ks)
                        , tmcProductionHandler = readIORef (ksProdHand ks)
                        , tmcRestoreListener = readIORef (ksRestoreListener ks)
                        , tmcSharedStores = shared
                        }
                      drv
                      plan
                  case eTm of
                    Left e -> do
                      mapM_ stopGlobalStore mGlobal
                      pure (Left ("task manager: " <> Text.pack e))
                    Right tm -> do
                      eSub <- sdSubscribe drv (rpSubscription plan) (callbacks tm)
                      case eSub of
                        Left e -> do
                          mapM_ stopGlobalStore mGlobal
                          pure (Left ("subscribe: " <> Text.pack e))
                        Right () -> do
                          writeIORef (ksCleanStop ks) True
                          a <- async (supervised ks drv tm mGlobal)
                          writeIORef
                            (ksRunning ks)
                            (Just Running {rDriver = drv, rTasks = tm, rGlobal = mGlobal, rThread = a})
                          pure (Right ())

    callbacks tm =
      RebalanceCallbacks
        { rcBeforeRejoin = do
            -- the last moment this generation may commit
            res <- tmCommit tm
            case res of
              Committed -> pure ()
              CommitFailed e -> do
                writeIORef (ksCleanStop ks) False
                logLine ("commit before rebalance failed: " <> Text.unpack e)
              CommitAborted e -> logLine ("transaction aborted before rebalance: " <> Text.unpack e)
        , rcRevoked = \tps -> do
            tmOnRevoked tm tps
            readIORef (ksRebLis ks) >>= \l -> RBL.dispatchRevoked l tps
        , rcAssigned = \tps -> do
            r <- tmOnAssigned tm tps
            either (\e -> throwIO (StreamsException (Text.pack e) (Just "assignment"))) pure r
            readIORef (ksRebLis ks) >>= \l -> RBL.dispatchAssigned l tps
        , rcLost = \tps -> do
            tmOnLost tm tps
            readIORef (ksRebLis ks) >>= \l -> RBL.dispatchLost l tps
        }


logLine :: String -> IO ()
logLine = hPutStrLn stderr . ("[streams] " <>)


setError :: KafkaStreams -> Text -> IO ()
setError ks msg = transitionTo ks (StreamsError msg)


----------------------------------------------------------------------
-- Stream thread
----------------------------------------------------------------------

{- | Run the loop under the uncaught-exception handler: "replace
thread" recovers every task (rewinding to the last commit) and runs
the loop again; the shutdown responses put the instance in
'StreamsError'.
-}
supervised :: KafkaStreams -> StreamDriver -> TaskManager -> Maybe GlobalStore -> IO ()
supervised ks drv tm mGlobal = go
  where
    go = do
      r <- try (streamLoop ks drv tm mGlobal)
      case r of
        Right () -> pure ()
        Left (e :: SomeException)
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> do
              writeIORef (ksCleanStop ks) False
              logLine ("stream thread failed: " <> show e)
              h <- readIORef (ksUncaught ks)
              resp <- runStreamsUncaughtExceptionHandler h e
              case resp of
                ReplaceThread -> do
                  rec_ <- try (tmRecoverAll tm)
                  case rec_ of
                    Right () -> go
                    Left (e' :: SomeException) -> setError ks ("recovery failed: " <> Text.pack (show e'))
                ShutdownClient -> setError ks ("uncaught (shutdown-client): " <> Text.pack (show e))
                ShutdownApplication -> setError ks ("uncaught (shutdown-app): " <> Text.pack (show e))


streamLoop :: KafkaStreams -> StreamDriver -> TaskManager -> Maybe GlobalStore -> IO ()
streamLoop ks drv tm mGlobal = do
  lastCommit <- nowMillis >>= newIORef
  let cfg = ksConfig ks
      interval = fromIntegral (effectiveCommitIntervalMs cfg)
      loop = do
        -- Fairness when polls return at once (pollMs = 0, tests).
        yield
        status <- readTVarIO (ksStatus ks)
        case status of
          StreamsClosing -> shutdown
          StreamsClosed -> pure ()
          StreamsError _ -> pure ()
          _ -> do
            gFail <- maybe (pure Nothing) globalStoreFailure mGlobal
            forM_ gFail $ \e -> throwIO (StreamsException ("global store thread failed: " <> e) (Just "global"))
            paused <- readTVarIO (ksPaused ks)
            tmSetPaused tm paused
            readIORef (ksThreads ks) >>= tmSetThreads tm
            restoring <- tmRestoring tm
            when restoring $ tmRestoreStep tm >>= either (\e -> throwIO (StreamsException (Text.pack e) (Just "restore"))) pure
            eRecs <- sdPoll drv (if restoring then 0 else pollMs cfg)
            recs <- either (\e -> throwIO (StreamsException ("poll: " <> Text.pack e) (Just "poll"))) pure eRecs
            tmRoute tm recs
            tmProcess tm
            stillRestoring <- tmRestoring tm
            assigned <- tmAssigned tm
            let target = if stillRestoring || Set.null assigned then StreamsRebalancing else StreamsRunning
            cur <- readTVarIO (ksStatus ks)
            when (cur /= target && (cur == StreamsRunning || cur == StreamsRebalancing)) (transitionTo ks target)
            maybeIssueProbingRebalance ks drv
            now <- nowMillis
            before <- readIORef lastCommit
            asked <- tmTakeCommitRequest tm
            when (asked || now - before >= interval) $ do
              writeIORef lastCommit now
              res <- tmCommit tm
              case res of
                Committed -> pure ()
                CommitFailed e -> logLine ("offset commit failed (records will be reprocessed): " <> Text.unpack e)
                CommitAborted e -> logLine ("transaction aborted, tasks rewound: " <> Text.unpack e)
            bumpTick ks
            loop
      -- Closing: commit what was processed, then close every task cleanly.
      shutdown = do
        res <- tmCommit tm
        clean <- case res of
          Committed -> pure True
          CommitFailed e -> logLine ("final commit failed: " <> Text.unpack e) >> pure False
          CommitAborted e -> logLine ("final transaction aborted: " <> Text.unpack e) >> pure True
        unless clean (writeIORef (ksCleanStop ks) False)
        tmCloseAll tm clean
  loop


----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

closeKafkaStreams :: KafkaStreams -> IO ()
closeKafkaStreams ks = closeKafkaStreamsWith ks defaultCloseOptions


streamsStatus :: KafkaStreams -> IO StreamsStatus
streamsStatus = readTVarIO . ksStatus


{- | Block until 'streamsStatus' reaches @target@ /or/ a final state
('StreamsClosed' or 'StreamsError') from which @target@ can no
longer be reached. 'StreamsClosing' is not final: it always proceeds
to 'StreamsClosed'. A caller that must tell the two outcomes apart
follows up with 'streamsStatus'.
-}
awaitState :: KafkaStreams -> StreamsStatus -> IO ()
awaitState ks target = atomically $ do
  cur <- readTVar (ksStatus ks)
  let isFinal = case cur of
        StreamsClosed -> True
        StreamsError _ -> True
        _ -> False
  if cur == target || isFinal
    then pure ()
    else retry


setStateListener :: KafkaStreams -> StateListener -> IO ()
setStateListener ks lis = writeIORef (ksListener ks) lis


transitionTo :: KafkaStreams -> StreamsStatus -> IO ()
transitionTo ks new_ = do
  old <- atomically $ do
    cur <- readTVar (ksStatus ks)
    writeTVar (ksStatus ks) new_
    pure cur
  when (old /= new_) $ do
    lis <- readIORef (ksListener ks)
    lis old new_


{- | Stop processing (and fetching) without leaving the group;
'resumeKafkaStreams' carries on from where it stopped.
-}
pauseKafkaStreams :: KafkaStreams -> IO ()
pauseKafkaStreams ks = atomically (writeTVar (ksPaused ks) True)


resumeKafkaStreams :: KafkaStreams -> IO ()
resumeKafkaStreams ks = atomically (writeTVar (ksPaused ks) False)


isPausedKafkaStreams :: KafkaStreams -> IO Bool
isPausedKafkaStreams = readTVarIO . ksPaused


-- | One task's position against its input. Mirrors @org.apache.kafka.streams.LagInfo@.
data LagInfo = LagInfo
  { lagTaskId :: !TaskId
  , lagCurrentOffset :: !Int64
  , lagEndOffset :: !Int64
  }
  deriving stock (Eq, Show)


type LagListener = [LagInfo] -> IO ()


setLagListener :: KafkaStreams -> LagListener -> IO ()
setLagListener ks lis = writeIORef (ksLagLis ks) lis


-- | Hand a lag snapshot to the registered listener.
publishLag :: KafkaStreams -> [LagInfo] -> IO ()
publishLag ks lags = do
  lis <- readIORef (ksLagLis ks)
  lis lags


----------------------------------------------------------------------
-- Rebalance
----------------------------------------------------------------------

{- | Callbacks fired after the runtime has handled each assignment
change (tasks created, or committed and closed).
-}
setRebalanceListener :: KafkaStreams -> RebalanceListener -> IO ()
setRebalanceListener ks l = writeIORef (ksRebLis ks) l


-- | The partitions the main consumer currently holds.
ownedPartitions :: KafkaStreams -> IO [KC.TopicPartition]
ownedPartitions ks =
  readIORef (ksRunning ks) >>= \case
    Nothing -> pure []
    Just r -> Set.toList <$> tmAssigned (rTasks r)


----------------------------------------------------------------------
-- Exception handlers
----------------------------------------------------------------------

{- | The handler consulted, at least once, for each record the producer
failed to send. ('ProdFailFast' stops the stream thread.) Under
exactly-once a failed send always aborts the transaction instead.
-}
setProductionExceptionHandler :: KafkaStreams -> ProductionHandler -> IO ()
setProductionExceptionHandler ks h = writeIORef (ksProdHand ks) h


-- | The handler consulted when a processor throws (KIP-1033).
setProcessingExceptionHandler :: KafkaStreams -> ProcessingExceptionHandler -> IO ()
setProcessingExceptionHandler ks h = writeIORef (ksProcHand ks) h


{- | The handler consulted when the stream thread stops on an exception
(KIP-671):

  * 'ReplaceThread' — close every task dirty, restore it from the
    changelogs, rewind to the last commit and carry on;
  * 'ShutdownClient' / 'ShutdownApplication' — this instance goes to
    'StreamsError'.
-}
setUncaughtExceptionHandler :: KafkaStreams -> StreamsUncaughtExceptionHandler -> IO ()
setUncaughtExceptionHandler ks h = writeIORef (ksUncaught ks) h


----------------------------------------------------------------------
-- KIP-441 probing rebalance
----------------------------------------------------------------------

-- | Report a warmup replica's changelog lag (0: caught up).
reportWarmupLag :: KafkaStreams -> TaskId -> Int64 -> IO ()
reportWarmupLag ks tid !lag =
  atomically $ modifyTVar' (ksWarmupLag ks) (Map.insert tid lag)


clearWarmupLag :: KafkaStreams -> TaskId -> IO ()
clearWarmupLag ks tid =
  atomically $ modifyTVar' (ksWarmupLag ks) (Map.delete tid)


warmupSnapshot :: KafkaStreams -> IO (Map.Map TaskId Int64)
warmupSnapshot = readTVarIO . ksWarmupLag


{- | Ask the group for a rebalance when a warmup is ready and the
probing interval has passed ('ProbingRebalance.shouldProbe').
-}
maybeIssueProbingRebalance :: KafkaStreams -> StreamDriver -> IO ()
maybeIssueProbingRebalance ks drv = do
  let cfg = ksConfig ks
  warmups <- readTVarIO (ksWarmupLag ks)
  unless (Map.null warmups) $ do
    now <- nowMillis
    lastAt <- readTVarIO (ksLastProbeAt ks)
    let ws =
          map
            (\(t, lag) -> ProbingRebalance.WarmupProgress {ProbingRebalance.task = t, ProbingRebalance.lag = lag})
            (Map.toList warmups)
    when (ProbingRebalance.shouldProbe now lastAt (probingRebalanceIntervalMs cfg) ws (acceptableRecoveryLag cfg)) $ do
      atomically $ writeTVar (ksLastProbeAt ks) now
      sdRequestRejoin drv


----------------------------------------------------------------------
-- KIP-663: stream-thread count
----------------------------------------------------------------------

{- | One more worker processing tasks (existing tasks are spread over
the workers by index). Returns the new count.
-}
addStreamThread :: KafkaStreams -> IO (Maybe Int)
addStreamThread ks = Just <$> atomicModifyIORef' (ksThreads ks) (\n -> (n + 1, n + 1))


-- | One worker fewer (never below one). Returns the new count.
removeStreamThread :: KafkaStreams -> IO (Maybe Int)
removeStreamThread ks = do
  n <- readIORef (ksThreads ks)
  if n <= 1
    then pure Nothing
    else Just <$> atomicModifyIORef' (ksThreads ks) (\m -> (m - 1, m - 1))


streamThreadCount :: KafkaStreams -> IO Int
streamThreadCount = readIORef . ksThreads


{- | Delete this application's local state (@\<state.dir\>/\<application.id\>@).
Mirrors @KafkaStreams.cleanUp()@: only while not running.
-}
cleanUp :: KafkaStreams -> IO ()
cleanUp ks = do
  st <- readTVarIO (ksStatus ks)
  case st of
    StreamsCreated -> wipe
    StreamsClosed -> wipe
    StreamsError _ -> wipe
    other -> throwIO (StreamsException ("cleanUp while " <> Text.pack (show other)) Nothing)
  where
    wipe = do
      let dir = appDir (ksConfig ks)
      exists <- doesDirectoryExist dir
      when exists (removePathForcibly dir)
      atomically (writeTVar (ksTicks ks) 0)


----------------------------------------------------------------------
-- Close
----------------------------------------------------------------------

-- | Mirrors @KafkaStreams.CloseOptions@.
data CloseOptions = CloseOptions
  { timeoutMs :: !(Maybe Int)
  , leaveGroup :: !Bool
  -- ^ Leave the group so it rebalances at once, rather than after the session timeout (KIP-812).
  }
  deriving stock (Eq, Show)


defaultCloseOptions :: CloseOptions
defaultCloseOptions = CloseOptions {timeoutMs = Just 30_000, leaveGroup = True}


{- | Close: the stream thread commits and closes its tasks at the next
loop boundary; if that takes longer than the timeout the thread is
cancelled and the state left uncertified. Then the global stores,
consumer and producer close.
-}
closeKafkaStreamsWith :: KafkaStreams -> CloseOptions -> IO ()
closeKafkaStreamsWith ks opts = do
  before <- readTVarIO (ksStatus ks)
  case before of
    StreamsClosed -> pure ()
    _ -> do
      transitionTo ks StreamsClosing
      let !tmoMs = maybe 30_000 id opts.timeoutMs
      mRun <- readIORef (ksRunning ks)
      atBoundary <- case mRun of
        Nothing -> pure True
        Just r -> do
          res <- timeout (max 0 tmoMs * 1000) (waitCatch (rThread r))
          case res of
            Just (Right ()) -> pure True
            Just (Left _) -> pure False
            Nothing -> do
              cancel (rThread r)
              _ <- waitCatch (rThread r)
              tmCloseAll (rTasks r) False
              pure False
      forM_ mRun $ \r -> do
        mapM_ stopGlobalStore (rGlobal r)
        sdCloseConsumer (rDriver r) opts.leaveGroup tmoMs
        sdCloseProducer (rDriver r)
        sdClose (rDriver r)
      writeIORef (ksRunning ks) Nothing
      cleanStop <- readIORef (ksCleanStop ks)
      let healthy = case before of
            StreamsError _ -> False
            _ -> True
      when (cleanStop && atBoundary && healthy) $
        markCleanShutdown (appDir (ksConfig ks))
      transitionTo ks StreamsClosed


----------------------------------------------------------------------
-- Restore listeners
----------------------------------------------------------------------

setGlobalStateRestoreListener :: KafkaStreams -> GlobalStateRestoreListener -> IO ()
setGlobalStateRestoreListener ks lis = writeIORef (ksGlobalRestoreLis ks) lis


-- | Told about every task store's restore (start, each batch, end, suspension).
setStateRestoreListener :: KafkaStreams -> StateRestoreListener -> IO ()
setStateRestoreListener ks lis = writeIORef (ksRestoreListener ks) lis


----------------------------------------------------------------------
-- Metadata
----------------------------------------------------------------------

-- | Per worker: its tasks' partitions and records processed. Mirrors Java's @ThreadMetadata@ (subset).
data LocalThreadMetadata = LocalThreadMetadata
  { threadId :: !Int
  , assigned :: ![KC.TopicPartition]
  , processedRecs :: !Int64
  }
  deriving stock (Eq, Show)


-- | The tasks on this instance.
localTasks :: KafkaStreams -> IO [TaskSnapshot]
localTasks ks =
  readIORef (ksRunning ks) >>= \case
    Nothing -> pure []
    Just r -> tmTaskSnapshot (rTasks r)


metadataForLocalThreads :: KafkaStreams -> IO [LocalThreadMetadata]
metadataForLocalThreads ks = do
  n <- readIORef (ksThreads ks)
  snaps <- localTasks ks
  pure $
    map
      ( \w ->
          let mine = filter ((== w) . tsWorker) snaps
          in LocalThreadMetadata
               { threadId = w
               , assigned = concatMap tsPartitions mine
               , processedRecs = sum (map tsProcessed mine)
               }
      )
      [0 .. n - 1]


metricsAndState :: KafkaStreams -> IO (StreamsStatus, [LocalThreadMetadata])
metricsAndState ks = (,) <$> streamsStatus ks <*> metadataForLocalThreads ks


{- | The instance's client-instance id (KIP-714), derived from
@application.id@ like the embedded clients'.
-}
kafkaStreamsClientInstanceId :: KafkaStreams -> IO BS.ByteString
kafkaStreamsClientInstanceId ks =
  pure (Telemetry.clientInstanceIdFromText (applicationId (ksConfig ks)))


{- | This instance's view as the 'PA.ApplicationState' a KIP-924
@TaskAssignor@ receives: its tasks, their partitions and stores.
-}
streamsApplicationState :: KafkaStreams -> IO PA.ApplicationState
streamsApplicationState ks = do
  snaps <- localTasks ks
  standbys <- Map.keysSet <$> readTVarIO (ksWarmupLag ks)
  let cfg = ksConfig ks
      pid = PA.ProcessId (UUID.fromWords 0 0 0 (fromIntegral (Text.length (applicationId cfg))))
      tasks =
        Map.fromList
          ( map
              ( \s ->
                  ( tsTask s
                  , PA.TaskInfo
                      { PA.tiTaskId = tsTask s
                      , PA.tiTopicPartitions =
                          Set.fromList
                            ( map
                                (\tp -> PA.TaskTopicPartition {PA.ttpTopic = tp.topic, PA.ttpPartition = tp.partition, PA.ttpIsChangelog = False})
                                (tsPartitions s)
                            )
                      , PA.tiIsStateful = False
                      , PA.tiStores = Set.empty
                      }
                  )
              )
              snaps
          )
      kss =
        PA.KafkaStreamsState
          { PA.kssProcessId = pid
          , PA.kssNumProcessingThreads = numStreamThreads cfg
          , PA.kssClientTags = Map.empty
          , PA.kssPreviousActiveTasks = Map.keysSet tasks
          , PA.kssPreviousStandbyTasks = standbys
          , PA.kssRackId = Nothing
          }
  pure
    PA.ApplicationState
      { PA.asAllTasks = tasks
      , PA.asKafkaStreamsStates = Map.singleton pid kss
      , PA.asAssignmentConfigs =
          PA.AssignmentConfigs
            { PA.acNumStandbyReplicas = numStandbyReplicas cfg
            , PA.acAcceptableRecoveryLag = fromIntegral (acceptableRecoveryLag cfg)
            , PA.acMaxWarmupReplicas = maxWarmupReplicas cfg
            , PA.acProbingRebalanceIntervalMs = probingRebalanceIntervalMs cfg
            , PA.acRackAware = PA.defaultRackAwareAssignmentConfigs
            }
      }


-- | Run the configured KIP-924 'PA.TaskAssignor' over 'streamsApplicationState'.
streamsRunUserAssignor :: KafkaStreams -> IO (Maybe PA.TaskAssignment)
streamsRunUserAssignor ks =
  case taskAssignor (ksConfig ks) of
    Nothing -> pure Nothing
    Just ta_ -> Just <$> (streamsApplicationState ks >>= PA.taAssign ta_)


----------------------------------------------------------------------
-- Store access
----------------------------------------------------------------------

-- | One local instance of a store: a task's, or the global copy.
data StoreInstance = StoreInstance
  { siTask :: !(Maybe TaskId)
  -- ^ 'Nothing' for a global store
  , siEngine :: !(Maybe Engine)
  , siEntry :: !StoreEntry
  }


-- | Every local instance of @sn@ (one per task holding it, or the global one).
localStoreInstances :: KafkaStreams -> StoreName -> IO [StoreInstance]
localStoreInstances ks sn =
  readIORef (ksRunning ks) >>= \case
    Nothing -> pure []
    Just r -> do
      engines <- tmEngines (rTasks r)
      taskOnes <- forM engines $ \(tid, e) -> do
        own <- engineOwnStores e
        pure ((\se -> StoreInstance (Just tid) (Just e) se) <$> Map.lookup sn own)
      global <- case rGlobal r of
        Nothing -> pure []
        Just g -> maybe [] (\se -> [StoreInstance Nothing Nothing se]) . Map.lookup sn <$> globalStores g
      pure (catMaybes taskOnes <> global)


----------------------------------------------------------------------
-- Progress signal (for tests)
----------------------------------------------------------------------

ksTickCount :: KafkaStreams -> IO Int
ksTickCount ks = readTVarIO (ksTicks ks)


{- | Block until the loop has run @n@ more times /or/ the instance is
closing, closed or failed. Returns the new count.
-}
awaitTicks :: KafkaStreams -> Int -> IO Int
awaitTicks ks n = do
  start <- readTVarIO (ksTicks ks)
  atomically $ do
    cur <- readTVar (ksTicks ks)
    st <- readTVar (ksStatus ks)
    let terminal = case st of
          StreamsClosing -> True
          StreamsClosed -> True
          StreamsError _ -> True
          _ -> False
    if terminal || cur >= start + n
      then pure cur
      else retry


bumpTick :: KafkaStreams -> IO ()
bumpTick ks = atomically (modifyTVar' (ksTicks ks) (+ 1))
