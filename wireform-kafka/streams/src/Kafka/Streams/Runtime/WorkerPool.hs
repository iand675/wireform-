{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime.WorkerPool
Description : numStreamThreads worker pool

Mirrors Java's @StreamThread@ pool: each worker owns its own
engine, its own slice of partitions, and a private input queue.
The pool fans records out to the correct worker by partition; a
dedicated 'async' thread per worker drains its queue and drives
the engine.

== Usage

@
pool <- newWorkerPool topo "app-id" perWorkerOwnership
mapM_ (submitRecord pool ...) records
waitForQuiescence pool
closeWorkerPool pool
@

The single-task 'TopologyTestDriver' is unchanged. The pool is a
separate, multi-thread-aware /driver/ used by tests and by the
broker-backed runtime when @numStreamThreads > 1@.
-}
module Kafka.Streams.Runtime.WorkerPool (
  -- * Pool
  WorkerPool,
  Worker (..),
  newWorkerPool,
  newWorkerPoolHashed,
  newWorkerPoolKeyGrouped,
  poolWorkers,
  poolWorkersSnapshot,
  poolWorkerCount,
  submitRecord,
  submitRecordHashed,
  submitRecordHashedWith,
  submitRecordKeyGrouped,
  setPoolPartitionCountLookup,
  waitForQuiescence,
  commitAllWorkers,
  closeWorkerPool,

  -- * Dynamic membership (KIP-663)
  addPoolWorker,
  removePoolWorker,

  -- * Routing introspection (KIP-535 partition-aware IQ)
  routingFor,
  workerById,

  -- * Per-worker state
  workerProcessedCount,

  -- * Key-group dispatch state (Riffle \xc2\xa72)
  poolKeyGroupAssignment,
  updateKeyGroupAssignment,
) where

import Control.Concurrent.Async (
  Async,
  async,
  cancel,
  waitCatch,
 )
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable qualified as Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (hash)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Int (Int32, Int64)
import Data.List qualified as Data.List
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Internal.Engine (
  Engine,
  buildEngine,
  closeEngine,
  commitEngine,
  feedSourceH,
  setPartitionCountLookup,
 )
import Kafka.Streams.Internal.RecordCollector (
  RecordCollector,
  inMemoryCollector,
 )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.KeyGroup (
  KeyGroupAssignment,
  KeyGroupConfig,
  KeyGroupId (..),
  emptyAssignment,
  keyGroupOfBytes,
 )
import Kafka.Streams.Runtime.KeyGroup qualified as KG
import Kafka.Streams.Time (Timestamp)
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Headers, TopicName, emptyHeaders)
import System.IO.Unsafe (unsafePerformIO)


----------------------------------------------------------------------
-- Worker
----------------------------------------------------------------------

{- | One worker. Each worker owns:

  * an 'Engine' built from the same topology as every other
    worker, but operating on its own state (its own stores,
    its own collector);
  * a 'Set' of (topic, partition) pairs it processes;
  * a private 'TQueue' inbox the pool writes to.
-}
data Worker = Worker
  { workerId :: !Int
  , workerEngine :: !Engine
  , workerCollector :: !RecordCollector
  , workerOwned :: !(HashSet (TopicName, Int32))
  , workerInbox :: !(TQueue WorkRecord)
  , workerStop :: !(TVar Bool)
  , workerProcessed :: !(TVar Int64)
  , workerSubmitted :: !(TVar Int64)
  {- ^ Tracks how many records have been submitted to this worker
  (via 'submitRecord'). 'waitForQuiescence' blocks until
  'workerProcessed' catches up to this number.
  -}
  , workerThread :: !(IORef (Maybe (Async ())))
  }


data WorkRecord = WorkRecord
  { wrTopic :: !TopicName
  , wrKey :: !(Maybe ByteString)
  , wrValue :: !ByteString
  , wrTimestamp :: !Timestamp
  , wrPartition :: !Int
  , wrOffset :: !Int64
  , wrHeaders :: !Headers
  }


----------------------------------------------------------------------
-- Pool
----------------------------------------------------------------------

data WorkerPool = WorkerPool
  { poolWorkersTV :: !(TVar (Vector Worker))
  {- ^ Live worker list. Mutable via 'addPoolWorker' /
  'removePoolWorker' so the pool can scale at runtime
  (KIP-663). Worker entries are stable for the lifetime
  of /that/ worker — never reused.
  -}
  , poolByIdxTV :: !(TVar (HashMap.HashMap Int Worker))
  {- ^ O(1) 'workerId -> Worker' lookup for the hot
  'submitRecord' path. Updated atomically with
  'poolWorkersTV'.
  -}
  , poolRouting :: !(TVar (HashMap.HashMap (TopicName, Int32) Int))
  , poolNextOff :: !(IORef Int64)
  , poolPartitionCount :: !(IORef (Maybe (Text -> IO (Either String Int))))
  {- ^ Sink partition-count lookup ('setPoolPartitionCountLookup'),
  kept so workers added later ('addPoolWorker') get it too.
  -}
  , poolTopo :: !Topo.TopologyValid
  , poolAppId :: !Text
  , poolNextWorkerId :: !(TVar Int)
  {- ^ Monotonic worker-id allocator. New workers get a fresh
  id even after removals, so routing-table entries never
  accidentally point at the wrong worker after churn.
  -}
  , poolKeyGroupCfg :: !(Maybe KeyGroupConfig)
  {- ^ Riffle \xc2\xa72 key-group dispatch. Present iff the pool was
  built via 'newWorkerPoolKeyGrouped'; otherwise the
  pool routes by partition (existing semantics).
  -}
  , poolKeyGroupTV :: !(TVar KeyGroupAssignment)
  {- ^ Live key-group ownership. The assignor pushes new
  assignments here in response to rebalance events; the
  hot path consults it on every 'submitRecordKeyGrouped'.
  -}
  , poolKeyGroupOwner :: !(TVar (HashMap.HashMap KeyGroupId Int))
  {- ^ KeyGroupId \xe2\x86\x92 workerId routing. Updated atomically
  whenever the assignment changes; the hot path is a
  single STM read.
  -}
  }


{- | Read the current worker vector. Reads see a consistent
snapshot (atomic 'TVar' read).
-}
poolWorkersSnapshot :: WorkerPool -> IO (Vector Worker)
poolWorkersSnapshot = readTVarIO . poolWorkersTV


-- | Worker count at this moment.
poolWorkerCount :: WorkerPool -> IO Int
poolWorkerCount = fmap V.length . poolWorkersSnapshot


{- | Legacy accessor. Kept for call sites that don't need a
consistent snapshot (e.g. when the runtime knows the pool
size won't change between read and use). For new code prefer
'poolWorkersSnapshot'.
-}
poolWorkers :: WorkerPool -> Vector Worker
poolWorkers pool =
  -- The 'unsafePerformIO' here is OK because 'readTVarIO' is
  -- pure-ish (an atomic snapshot), and call-sites that mutated
  -- the pool already synchronise via 'addPoolWorker' /
  -- 'removePoolWorker'.
  unsafePerformIO (poolWorkersSnapshot pool)
{-# NOINLINE poolWorkers #-}


{- | Build a pool of @length perWorkerOwnership@ workers. Each entry
in @perWorkerOwnership@ is the set of (topic, partition) pairs
that worker handles. The runtime's partition assignor decides
this in production; tests pass it in directly.
-}
newWorkerPool
  :: Topo.TopologyValid
  -> Text
  -- ^ application id
  -> [HashSet (TopicName, Int32)]
  -> IO WorkerPool
newWorkerPool topo appId perWorkerOwnership = do
  off <- newIORef 0
  partCount <- newIORef Nothing
  routing <- newTVarIO (buildRouting perWorkerOwnership)
  -- Build workers into a 'Vector' directly so we don't pay the
  -- 'fromList' cost on every iteration site. 'V.imapM' gives us
  -- the deterministic 0..n-1 worker id and the matching ownership
  -- entry in one pass.
  let !ownedV = V.fromList perWorkerOwnership
      !n = V.length ownedV
  workers <- V.imapM (buildWorker topo appId) ownedV
  workersTV <- newTVarIO workers
  byIdxTV <-
    newTVarIO $
      HashMap.fromList
        (V.toList (V.map (\w -> (workerId w, w)) workers))
  nextIdTV <- newTVarIO n
  kgTV <- newTVarIO emptyAssignment
  kgOwnTV <- newTVarIO HashMap.empty
  pure
    WorkerPool
      { poolWorkersTV = workersTV
      , poolByIdxTV = byIdxTV
      , poolRouting = routing
      , poolNextOff = off
      , poolPartitionCount = partCount
      , poolTopo = topo
      , poolAppId = appId
      , poolNextWorkerId = nextIdTV
      , poolKeyGroupCfg = Nothing
      , poolKeyGroupTV = kgTV
      , poolKeyGroupOwner = kgOwnTV
      }
  where
    buildWorker topology aId idx owned =
      mkWorker topology aId idx owned


{- | Internal: spin up a single worker with the given id and
start its event-loop async.
-}
mkWorker
  :: Topo.TopologyValid
  -> Text
  -> Int
  -> HashSet (TopicName, Int32)
  -> IO Worker
mkWorker topology aId idx owned = do
  inbox <- newTQueueIO
  stop <- newTVarIO False
  processed <- newTVarIO 0
  submitted <- newTVarIO 0
  coll <- inMemoryCollector
  eng <-
    buildEngine
      topology
      (TaskId 0 (fromIntegral idx))
      aId
      coll
      logAndContinue
  threadRef <- newIORef Nothing
  let !w =
        Worker
          { workerId = idx
          , workerEngine = eng
          , workerCollector = coll
          , workerOwned = owned
          , workerInbox = inbox
          , workerStop = stop
          , workerProcessed = processed
          , workerSubmitted = submitted
          , workerThread = threadRef
          }
  th <- async (workerLoop w)
  atomicModifyIORef' threadRef (\_ -> (Just th, ()))
  pure w


buildRouting :: [HashSet (TopicName, Int32)] -> HashMap.HashMap (TopicName, Int32) Int
buildRouting perWorker =
  HashMap.fromList
    [ (tp, idx)
    | (idx, owned) <- zip [0 ..] perWorker
    , tp <- HashSet.toList owned
    ]


{- | Build a pool of @n@ workers with no fixed partition
ownership. Records submitted via 'submitRecordHashed' are
dispatched by hashing @(topic, partition)@ modulo the worker
count, so the same (topic, partition) always lands on the
same worker (state-store consistency) even though the
partition set isn't known up-front.

Used by the multi-thread runtime, where partitions are
discovered as records are polled from the broker.
-}
newWorkerPoolHashed
  :: Topo.TopologyValid
  -> Text
  -- ^ application id
  -> Int
  -- ^ worker count (>= 1)
  -> IO WorkerPool
newWorkerPoolHashed topo appId n
  | n < 1 = error "newWorkerPoolHashed: worker count must be >= 1"
  | otherwise =
      newWorkerPool
        topo
        appId
        (replicate n HashSet.empty)


{- | Build a key-group-dispatched pool (Riffle \xc2\xa72). The runtime
routes records by hashing the key onto a 'KeyGroupId' via the
supplied 'KeyGroupConfig', then dispatches the record to the
worker that owns that key-group per the live
'KeyGroupAssignment'.

The pool starts with @n@ workers and an empty assignment. The
runtime (or the test) calls 'updateKeyGroupAssignment' to
publish the live ownership map; until then,
'submitRecordKeyGrouped' silently drops records the same way
'submitRecord' silently drops records for partitions whose
owner has been removed.

Decoupling parallelism from partition count: the worker count
and the key-group count are independent. Typical deployments
pick @kgcTotal@ as a power of two well above the maximum
anticipated parallelism (128 / 256 / 1024) and let the
assignor balance key-groups across workers.
-}
newWorkerPoolKeyGrouped
  :: Topo.TopologyValid
  -> Text
  -- ^ application id
  -> Int
  -- ^ worker count (>= 1)
  -> KeyGroupConfig
  -> IO WorkerPool
newWorkerPoolKeyGrouped topo appId n cfg
  | n < 1 = error "newWorkerPoolKeyGrouped: worker count must be >= 1"
  | otherwise = do
      pool <- newWorkerPool topo appId (replicate n HashSet.empty)
      pure pool {poolKeyGroupCfg = Just cfg}


----------------------------------------------------------------------
-- Submit
----------------------------------------------------------------------

{- | Enqueue a record onto the worker that owns its (topic,
partition). Records for unassigned partitions are silently
dropped (matches the broker's behaviour after a rebalance).
-}
submitRecord
  :: WorkerPool
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -> IO ()
submitRecord pool topic key val ts part = do
  off <- atomicModifyIORef' (poolNextOff pool) (\n -> (n + 1, n))
  routing <- readTVarIO (poolRouting pool)
  byIdx <- readTVarIO (poolByIdxTV pool)
  case HashMap.lookup (topic, fromIntegral part) routing of
    Nothing -> pure ()
    Just idx ->
      case HashMap.lookup idx byIdx of
        Nothing -> pure ()
        Just w ->
          atomically $ do
            writeTQueue
              (workerInbox w)
              (WorkRecord topic key val ts part off emptyHeaders)
            modifyTVar' (workerSubmitted w) (+ 1)


{- | Like 'submitRecord' but dispatch by @hash (topic, partition)
mod numWorkers@. Used by the broker-backed runtime where the
partition set isn't known until the consumer reports them.
Records that hash to the same worker preserve state-store
locality: stores live per-worker, so the same key (and the
same partition) always hit the same store.

The first time a (topic, partition) lands here we record the
chosen worker in 'poolRouting', so subsequent records on that
partition skip the hash and route through the table — matches
the JVM's sticky behaviour within a stream-thread.
-}
submitRecordHashed
  :: WorkerPool
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -> IO ()
submitRecordHashed pool topic key val ts part = do
  off <- atomicModifyIORef' (poolNextOff pool) (\n -> (n + 1, n))
  submitRecordHashedWith pool topic key val emptyHeaders ts part off


{- | 'submitRecordHashed' for a record polled off the broker: the
record's own headers and offset reach the topology (the plain
variant hands it empty headers and a pool-synthesised offset).
-}
submitRecordHashedWith
  :: WorkerPool
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Headers
  -> Timestamp
  -> Int
  -> Int64
  -> IO ()
submitRecordHashedWith pool topic key val hdrs ts part off = do
  -- Read the worker list + the by-id map in the SAME STM
  -- transaction as the routing so we don't race against
  -- 'addPoolWorker' / 'removePoolWorker' between picking an
  -- index and looking up the worker.
  let !tp = (topic, fromIntegral part :: Int32)
  mw <- atomically $ do
    ws <- readTVar (poolWorkersTV pool)
    byIdx <- readTVar (poolByIdxTV pool)
    let !workerIds = V.map workerId ws
        !nWorkers = V.length ws
    if nWorkers == 0
      then pure Nothing
      else do
        m <- readTVar (poolRouting pool)
        idx <- case HashMap.lookup tp m of
          Just i
            | i `elem` (V.toList workerIds) -> pure i
            | otherwise -> do
                -- The worker that used to own this tp has
                -- been removed; re-hash and rewrite the
                -- routing entry so subsequent records
                -- land deterministically.
                let !i' =
                      chooseHashed
                        workerIds
                        (abs (hash tp))
                writeTVar
                  (poolRouting pool)
                  (HashMap.insert tp i' m)
                pure i'
          Nothing -> do
            let !i = chooseHashed workerIds (abs (hash tp))
            writeTVar (poolRouting pool) (HashMap.insert tp i m)
            pure i
        case HashMap.lookup idx byIdx of
          Nothing -> pure Nothing
          Just w -> do
            writeTQueue
              (workerInbox w)
              (WorkRecord topic key val ts part off hdrs)
            modifyTVar' (workerSubmitted w) (+ 1)
            pure (Just w)
  _ <- pure mw
  pure ()


{- | Pick a worker id out of the current ID vector by hashing,
so subsequent records on the same tp land on the same
worker. The vector is the LIVE id vector — a worker that
has been removed isn't a candidate.
-}
chooseHashed :: V.Vector Int -> Int -> Int
chooseHashed ids h =
  let !n = V.length ids
  in if n == 0
       then 0
       else ids V.! (h `mod` n)


----------------------------------------------------------------------
-- Key-group dispatch (Riffle \xc2\xa72)
----------------------------------------------------------------------

{- | Enqueue a record onto the worker that owns the key-group
the record hashes into. Requires the pool to have been built
via 'newWorkerPoolKeyGrouped' and an assignment to have been
published via 'updateKeyGroupAssignment'. Otherwise the
record is silently dropped (matching the
partition-not-assigned semantics of 'submitRecord').
-}
submitRecordKeyGrouped
  :: WorkerPool
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -- ^ partition (carried through)
  -> IO ()
submitRecordKeyGrouped pool topic mKey val ts part =
  case poolKeyGroupCfg pool of
    Nothing -> pure ()
    Just cfg -> do
      off <-
        atomicModifyIORef'
          (poolNextOff pool)
          (\n -> (n + 1, n))
      let !kb = case mKey of
            Nothing -> BS.empty
            Just b -> b
          !kg = keyGroupOfBytes cfg kb
      mw <- atomically $ do
        owners <- readTVar (poolKeyGroupOwner pool)
        case HashMap.lookup kg owners of
          Nothing -> pure Nothing
          Just wid -> do
            byIdx <- readTVar (poolByIdxTV pool)
            case HashMap.lookup wid byIdx of
              Nothing -> pure Nothing
              Just w -> do
                writeTQueue
                  (workerInbox w)
                  (WorkRecord topic mKey val ts part off emptyHeaders)
                modifyTVar' (workerSubmitted w) (+ 1)
                pure (Just w)
      _ <- pure mw
      pure ()


{- | Read the live key-group assignment as the assignor last
published it.
-}
poolKeyGroupAssignment :: WorkerPool -> IO KeyGroupAssignment
poolKeyGroupAssignment = readTVarIO . poolKeyGroupTV


{- | Publish a new key-group assignment. The pool atomically
rebuilds the @KeyGroupId \xe2\x86\x92 workerId@ owner map by
distributing owned key-groups round-robin across the current
live worker set. Subsequent 'submitRecordKeyGrouped' calls
see the new assignment immediately.

The runtime would call this from the rebalance listener
whenever 'Kafka.Streams.Runtime.RebalanceProtocol.applyReconciliation'
changes the local instance's owned set.
-}
updateKeyGroupAssignment
  :: WorkerPool
  -> KeyGroupAssignment
  -> IO ()
updateKeyGroupAssignment pool asg = atomically $ do
  ws <- readTVar (poolWorkersTV pool)
  let !ids = V.toList (V.map workerId ws)
  let !owned = KG.kgaOwned asg
      !n = length ids
      !pairs =
        if n == 0
          then []
          else
            zipWith
              (\kg i -> (kg, ids !! (i `mod` n)))
              (Data.List.sort (toList owned))
              [0 :: Int ..]
  writeTVar (poolKeyGroupTV pool) asg
  writeTVar (poolKeyGroupOwner pool) (HashMap.fromList pairs)
  where
    toList s = HashSet.toList (HashSet.fromList (Data.Foldable.toList s))


{- | Block until every record submitted so far has finished
processing on the right worker.  Coordinated via per-worker
@workerSubmitted@ / @workerProcessed@ counters; no thread sleeps.
-}
waitForQuiescence :: WorkerPool -> IO ()
waitForQuiescence pool =
  atomically $ do
    ws <- readTVar (poolWorkersTV pool)
    V.forM_ ws $ \w -> do
      sub <- readTVar (workerSubmitted w)
      done <- readTVar (workerProcessed w)
      unless (done >= sub) retry


-- | Force a commit on every worker's engine.
commitAllWorkers :: WorkerPool -> IO ()
commitAllWorkers pool = do
  ws <- poolWorkersSnapshot pool
  V.mapM_ (commitEngine . workerEngine) ws


closeWorkerPool :: WorkerPool -> IO ()
closeWorkerPool pool = do
  -- Drain in-flight records first.
  waitForQuiescence pool
  ws <- poolWorkersSnapshot pool
  V.forM_ ws $ \w ->
    atomically (writeTVar (workerStop w) True)
  -- Cancel each worker thread.
  V.forM_ ws $ \w -> do
    mTh <- readIORef (workerThread w)
    case mTh of
      Just th -> do
        cancel th
        _ <- waitCatch th
        pure ()
      Nothing -> pure ()
  -- Close engines.
  V.mapM_ (closeEngine . workerEngine) ws


----------------------------------------------------------------------
-- KIP-663: dynamic add / remove
----------------------------------------------------------------------

{- | Add a fresh hash-routed worker. Returns the new worker
count. Existing routing entries don't migrate (state-store
locality would be lost); only newly-seen tps can hash onto
the added worker.

It's safe to call concurrently with 'submitRecordHashed' —
the routing read + worker lookup happen in one STM
transaction (see 'submitRecordHashed').
-}
{- | Install the sink partition-count lookup on every worker engine,
present and future (see 'setPartitionCountLookup').
-}
setPoolPartitionCountLookup
  :: WorkerPool -> (Text -> IO (Either String Int)) -> IO ()
setPoolPartitionCountLookup pool f = do
  writeIORef (poolPartitionCount pool) (Just f)
  ws <- poolWorkersSnapshot pool
  V.forM_ ws $ \w -> setPartitionCountLookup (workerEngine w) f


addPoolWorker :: WorkerPool -> IO Int
addPoolWorker pool = do
  -- Reserve a fresh id atomically so two concurrent calls
  -- don't pick the same.
  idx <- atomically $ do
    n <- readTVar (poolNextWorkerId pool)
    writeTVar (poolNextWorkerId pool) (n + 1)
    pure n
  w <- mkWorker (poolTopo pool) (poolAppId pool) idx HashSet.empty
  readIORef (poolPartitionCount pool)
    >>= mapM_ (setPartitionCountLookup (workerEngine w))
  atomically $ do
    modifyTVar' (poolWorkersTV pool) (`V.snoc` w)
    modifyTVar' (poolByIdxTV pool) (HashMap.insert idx w)
  V.length <$> poolWorkersSnapshot pool


{- | Remove one worker, draining + joining it cleanly. Picks
the highest-index worker (lifo) so the routing churn is
minimal. Returns 'Just newCount' on success, or 'Nothing'
if there are no workers left to remove.

Any (topic, partition) routes that pointed at the removed
worker are dropped from the routing table; the next record
on those partitions will re-hash onto a remaining worker.
-}
removePoolWorker :: WorkerPool -> IO (Maybe Int)
removePoolWorker pool = do
  mTarget <- atomically $ do
    ws <- readTVar (poolWorkersTV pool)
    if V.null ws
      then pure Nothing
      else do
        let !target = V.last ws
            !ws' = V.init ws
            !idx = workerId target
        writeTVar (poolWorkersTV pool) ws'
        modifyTVar' (poolByIdxTV pool) (HashMap.delete idx)
        -- Drop every routing entry pointing at the removed
        -- worker. Submitters will re-hash on the next record.
        modifyTVar'
          (poolRouting pool)
          (HashMap.filter (/= idx))
        -- Mark the worker stopped now so its loop exits as
        -- soon as the inbox drains.
        writeTVar (workerStop target) True
        pure (Just target)
  case mTarget of
    Nothing -> pure Nothing
    Just target -> do
      -- Wait for the worker to drain its inbox (processed
      -- catches up with submitted), then cancel + close.
      atomically $ do
        sub <- readTVar (workerSubmitted target)
        done <- readTVar (workerProcessed target)
        unless (done >= sub) retry
      mTh <- readIORef (workerThread target)
      case mTh of
        Just th -> do
          cancel th
          _ <- waitCatch th
          pure ()
        Nothing -> pure ()
      closeEngine (workerEngine target)
      Just . V.length <$> poolWorkersSnapshot pool


----------------------------------------------------------------------
-- Worker loop
----------------------------------------------------------------------

workerLoop :: Worker -> IO ()
workerLoop w = loop
  where
    loop = do
      -- Drain semantics:
      --
      --   * If the inbox is non-empty, take the next record
      --     (whether or not 'workerStop' is set). Otherwise
      --     'removePoolWorker' would deadlock waiting for
      --     'workerProcessed' to catch up to 'workerSubmitted'
      --     when records have been queued but not yet
      --     processed at the moment shutdown is requested.
      --
      --   * If the inbox is empty, only block when 'workerStop'
      --     is False; once stop is True the worker exits
      --     promptly so the pool can join it.
      next <- atomically $ do
        empty <- isEmptyTQueue (workerInbox w)
        if not empty
          then Just <$> readTQueue (workerInbox w)
          else do
            s <- readTVar (workerStop w)
            if s
              then pure Nothing
              else retry
      case next of
        Nothing -> pure ()
        Just r -> do
          -- Filter on ownership for safety, but only when the
          -- pool was constructed with an explicit ownership
          -- set. Hash-routed pools (built by
          -- 'newWorkerPoolHashed') have empty 'workerOwned' and
          -- skip the check; routing is decided by the
          -- 'submitRecordHashed' caller.
          let !tp = (wrTopic r, fromIntegral (wrPartition r))
              !ok =
                HashSet.null (workerOwned w)
                  || HashSet.member tp (workerOwned w)
          if ok
            then do
              feedSourceH
                (workerEngine w)
                (wrTopic r)
                (wrKey r)
                (Just (wrValue r))
                (wrHeaders r)
                (wrTimestamp r)
                (wrPartition r)
                (wrOffset r)
              atomically $ modifyTVar' (workerProcessed w) (+ 1)
            else
              -- Record is for a partition this worker doesn't
              -- own; drop it but still bump 'workerProcessed'
              -- so 'waitForQuiescence' can make progress.
              atomically $ modifyTVar' (workerProcessed w) (+ 1)
          loop


workerProcessedCount :: Worker -> IO Int64
workerProcessedCount = readTVarIO . workerProcessed


----------------------------------------------------------------------
-- Routing introspection
----------------------------------------------------------------------

{- | Look up which worker (by id) owns the supplied partition,
scanning the routing table for the first entry whose
@(_, partition)@ matches. Returns 'Nothing' if the
partition isn't routed anywhere (no record has touched it
yet, or the worker that owned it has been removed).
-}
routingFor :: WorkerPool -> Int -> IO (Maybe Int)
routingFor pool part = do
  m <- readTVarIO (poolRouting pool)
  pure $ case [ idx
              | ((_, p), idx) <- HashMap.toList m
              , p == fromIntegral part
              ] of
    (idx : _) -> Just idx
    [] -> Nothing


{- | O(1) lookup by 'workerId'. Returns 'Nothing' if the
worker has been removed (or never existed).
-}
workerById :: WorkerPool -> Int -> Maybe Worker
workerById pool wid =
  HashMap.lookup
    wid
    (unsafePerformIO (readTVarIO (poolByIdxTV pool)))
{-# NOINLINE workerById #-}
