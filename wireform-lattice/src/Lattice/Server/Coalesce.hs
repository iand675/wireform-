{- | Origin coalescing (spec §6.9): per-type accumulation windows over the
point-fetch load path.

Resource mode makes the origin see one plan's worth of work as a spray of
independent point fetches. Coalescing makes the two arrival patterns
execute identically: a fetch that misses origin-locally joins the current
accumulation window for its entity type; the window flushes into __one__
set-in map-out loader call when either 'ccMaxBatch' is reached or a
deadline expires, measured from the window's /first/ entrant
('Control.Concurrent.STM.TVar.registerDelay'), so steady arrival cannot
extend the window indefinitely.

== Semantics pinned here

* /Single-flight per (type, key):/ concurrent fetches of one key inside a
  window share one slot — one loader row serves every waiter. A fetch
  arriving after its key's window was dispatched joins the /next/ window
  (a fresh load; the row may have changed).
* /One loader call per flush:/ the flush hands the loader the window's
  distinct keys in one call — the same batched loaders query execution
  uses, so N+1 against storage is unrepresentable from either arrival
  pattern.
* /Loads are policy-free:/ nothing here sees claims, slices, or masks.
  Rendering (visibility, ETag, @ver@ pinning) happens per response,
  after the batch, in "Lattice.Server".
* /Failure isolation:/ a loader call that __throws__ fails every waiter in
  that flush — each waiter rethrows the exception on its own thread and
  renders its own @5xx@; waiters in other windows are untouched. A per-key
  'Lattice.Backend.BackendFailure' or an absent key affects only the
  waiters of that key.
* /Loader equivalence:/ the flush uses the loader supplied by the window's
  first entrant. Callers must pass equivalent loaders for a given type
  (in the origin wiring every call site closes over the one configured
  backend, so this holds by construction).

== Determinism hooks (no sleeping in tests)

'awaitPending' blocks (in STM) until a window holds a given number of
joins; 'flushNow' forces the open window for a type and returns only after
results are distributed and stats recorded; 'coalesceStats' exposes
per-flush batch sizes, waiter counts, and per-join window waits (the
@lattice.loader.batch_size@ / @lattice.coalesce.wait@ observability
signals of spec §19.3, in-process).
-}
module Lattice.Server.Coalesce (
  CoalesceConfig (..),
  Coalescer,
  newCoalescer,
  newCoalescerWith,
  Loader,
  coalescedLoad,
  coalescedLoadMany,

  -- * Deterministic hooks
  flushNow,
  awaitPending,
  CoalesceStats (..),
  FlushStat (..),
  coalesceStats,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (unless, when)
import Data.Foldable (for_, toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Traversable (for)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Lattice.Backend (BackendFailure, LoadResult)
import Lattice.Telemetry (LatticeTelemetry, noTelemetry, recordCoalesceWait, recordLoaderBatch, telemetryEnabled)
import Lattice.Types (TypeName, unTypeName)


-- | A batched row loader, shaped exactly like 'Lattice.Backend.beLoad'
-- applied to its type: distinct keys in, per-key verdicts out. Keys the
-- loader omits from the map are handed back as omitted (the caller's
-- absent-row semantics apply unchanged).
type Loader = [Text] -> IO (Map Text (Either BackendFailure LoadResult))


data CoalesceConfig = CoalesceConfig
  { ccWindowMicros :: Int
  -- ^ Deadline from the window's first entrant. Single-digit
  -- milliseconds by default per §6.9 (published in discovery as
  -- @coalesceWindowMs@).
  , ccMaxBatch :: Int
  -- ^ Flush immediately once a window holds this many distinct keys
  -- (clamped to at least 1).
  }
  deriving stock (Eq, Show)


data Coalescer = Coalescer
  { cConfig :: CoalesceConfig
  , cWindows :: TVar (Map TypeName Window)
  -- ^ The open (still joinable) window per type.
  , cFlushLog :: TVar (Seq FlushStat)
  -- ^ Bounded, chronological ('flushLogBound').
  , cTelemetry :: LatticeTelemetry
  -- ^ Feeds @lattice.loader.batch_size@ \/ @lattice.coalesce.wait@ (§19.3)
  -- at flush time; 'noTelemetry' via 'newCoalescer'.
  }


-- | One key's slot in a window: every join of that key awaits the same
-- result. Join timestamps (monotonic ns) feed the wait histogram.
data KeySlot = KeySlot
  { ksResult :: TMVar (Either SomeException (Maybe (Either BackendFailure LoadResult)))
  , ksJoins :: TVar [Word64]
  }


data Window = Window
  { wSlots :: TVar (Map Text KeySlot)
  , wClosed :: TVar Bool
  -- ^ Set (in the same transaction that removes the window from
  -- 'cWindows') by whichever of the three closers fires first: the
  -- max-batch join, 'flushNow', or the deadline. Doubles as the
  -- window's identity ('sameWindow').
  , wDone :: TMVar ()
  -- ^ Filled after results are distributed and stats recorded.
  , wLoader :: Loader
  }


sameWindow :: Window -> Window -> Bool
sameWindow a b = wClosed a == wClosed b


-- | How many 'FlushStat' entries 'cFlushLog' retains.
flushLogBound :: Int
flushLogBound = 1024


newCoalescer :: CoalesceConfig -> IO Coalescer
newCoalescer = newCoalescerWith noTelemetry


-- | 'newCoalescer' with a live telemetry handle (the origin wiring):
-- every flush records its batch size and per-join window waits.
newCoalescerWith :: LatticeTelemetry -> CoalesceConfig -> IO Coalescer
newCoalescerWith tel cfg =
  Coalescer cfg <$> newTVarIO Map.empty <*> newTVarIO Seq.empty <*> pure tel


-- | Coalesced load of one key: join the type's window, await the flush.
-- @Nothing@ = the loader omitted the key (caller-side absent semantics).
-- A loader exception is rethrown on this (the waiter's) thread.
coalescedLoad ::
  Coalescer ->
  TypeName ->
  Loader ->
  Text ->
  IO (Maybe (Either BackendFailure LoadResult))
coalescedLoad c ty loader key = enqueue c ty loader key >>= awaitSlot


{- | Drop-in 'Lattice.Backend.beLoad' wrapper: enqueue every key (so keys
of one call share windows and never wait on each other's results), then
await them all. Keys the underlying loader omitted are omitted here too,
byte-identical to the direct path.
-}
coalescedLoadMany ::
  Coalescer ->
  TypeName ->
  Loader ->
  [Text] ->
  IO (Map Text (Either BackendFailure LoadResult))
coalescedLoadMany c ty loader keys = do
  slots <- for keys $ \k -> (,) k <$> enqueue c ty loader k
  results <- for slots $ \(k, s) -> (,) k <$> awaitSlot s
  pure (Map.fromList (mapMaybe (\(k, r) -> (,) k <$> r) results))


{- | Force the open window for a type (if any) to flush immediately.
Returns after the flush has distributed results and recorded its
'FlushStat' — after 'flushNow' returns, 'coalesceStats' reflects the
flush. No open window = no-op.
-}
flushNow :: Coalescer -> TypeName -> IO ()
flushNow c ty = do
  mw <- atomically $ do
    wins <- readTVar (cWindows c)
    case Map.lookup ty wins of
      Nothing -> pure Nothing
      Just w -> do
        writeTVar (wClosed w) True
        writeTVar (cWindows c) (Map.delete ty wins)
        pure (Just w)
  for_ mw (atomically . readTMVar . wDone)


{- | Block until the open window for a type holds at least @n@ joins
(single-flight duplicates counted). The deterministic barrier for tests:
start @n@ fetches, 'awaitPending', 'flushNow' — no sleeping. Beware
deadline flushes emptying the window; pair with a generous
'ccWindowMicros'.
-}
awaitPending :: Coalescer -> TypeName -> Int -> STM ()
awaitPending c ty n = do
  wins <- readTVar (cWindows c)
  pending <- maybe (pure 0) windowJoins (Map.lookup ty wins)
  check (pending >= n)


data CoalesceStats = CoalesceStats
  { csFlushes :: [FlushStat]
  -- ^ Chronological; bounded to the last 'flushLogBound' flushes.
  , csPending :: Map TypeName Int
  -- ^ Joins waiting in each currently open window.
  }
  deriving stock (Eq, Show)


data FlushStat = FlushStat
  { fsType :: TypeName
  , fsBatch :: Int
  -- ^ Distinct keys handed to the loader (@lattice.loader.batch_size@).
  , fsWaiters :: Int
  -- ^ Joins served, single-flight duplicates included.
  , fsWaitMicros :: [Int]
  -- ^ Per join: window entry to flush dispatch (@lattice.coalesce.wait@).
  }
  deriving stock (Eq, Show)


coalesceStats :: Coalescer -> IO CoalesceStats
coalesceStats c = atomically $ do
  flushes <- readTVar (cFlushLog c)
  wins <- readTVar (cWindows c)
  pending <- traverse windowJoins wins
  pure CoalesceStats {csFlushes = toList flushes, csPending = pending}


windowJoins :: Window -> STM Int
windowJoins w = do
  slots <- readTVar (wSlots w)
  counts <- traverse (fmap length . readTVar . ksJoins) (Map.elems slots)
  pure (sum counts)


-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

{- | Join the open window for a type, creating (and installing) it when
none is open. The creator forks the window's flusher thread; losing an
installation race retries the join, so a fetch can never miss both the
existing window and the creation slot.
-}
enqueue :: Coalescer -> TypeName -> Loader -> Text -> IO KeySlot
enqueue c ty loader key = do
  now <- getMonotonicTimeNSec
  joined <- atomically (tryJoin c ty key now)
  case joined of
    Just ks -> pure ks
    Nothing -> do
      ks <- KeySlot <$> newEmptyTMVarIO <*> newTVarIO [now]
      w <-
        Window
          <$> newTVarIO (Map.singleton key ks)
          <*> newTVarIO False
          <*> newEmptyTMVarIO
          <*> pure loader
      -- A one-key batch cap closes the window before it ever opens for
      -- joining; it is flushed without touching 'cWindows'.
      let solo = ccMaxBatch (cConfig c) <= 1
      installed <- atomically $ do
        wins <- readTVar (cWindows c)
        case Map.lookup ty wins of
          Just _ -> pure False
          Nothing -> do
            if solo
              then writeTVar (wClosed w) True
              else writeTVar (cWindows c) (Map.insert ty w wins)
            pure True
      if installed
        then do
          deadline <-
            if solo
              then newTVarIO True
              else registerDelay (max 0 (ccWindowMicros (cConfig c)))
          _ <- forkIO (runFlush c ty w deadline)
          pure ks
        else enqueue c ty loader key


-- | Join the open window: share the key's slot when present (single
-- flight), otherwise add one; reaching 'ccMaxBatch' closes the window.
tryJoin :: Coalescer -> TypeName -> Text -> Word64 -> STM (Maybe KeySlot)
tryJoin c ty key now = do
  wins <- readTVar (cWindows c)
  case Map.lookup ty wins of
    Nothing -> pure Nothing
    Just w -> do
      slots <- readTVar (wSlots w)
      case Map.lookup key slots of
        Just ks -> do
          modifyTVar' (ksJoins ks) (now :)
          pure (Just ks)
        Nothing -> do
          ks <- KeySlot <$> newEmptyTMVar <*> newTVar [now]
          let slots' = Map.insert key ks slots
          writeTVar (wSlots w) slots'
          when (Map.size slots' >= max 1 (ccMaxBatch (cConfig c))) $ do
            writeTVar (wClosed w) True
            writeTVar (cWindows c) (Map.delete ty wins)
          pure (Just ks)


{- | The window's flusher (one thread per window, forked at creation):
wait until the window closes — max-batch join, 'flushNow', or this
thread's own deadline — then make the one loader call and distribute.
The deadline path closes the window itself, guarded by 'sameWindow' so a
racing closer's /successor/ window is never torn down.
-}
runFlush :: Coalescer -> TypeName -> Window -> TVar Bool -> IO ()
runFlush c ty w deadline = do
  atomically $ do
    closed <- readTVar (wClosed w)
    unless closed $ do
      expired <- readTVar deadline
      check expired
      writeTVar (wClosed w) True
      modifyTVar'
        (cWindows c)
        (Map.update (\cur -> if sameWindow cur w then Nothing else Just cur) ty)
  slots <- readTVarIO (wSlots w)
  flushedAt <- getMonotonicTimeNSec
  outcome <- try (wLoader w (Map.keys slots))
  joins <- atomically (traverse (readTVar . ksJoins) (Map.elems slots))
  let waits = map (\j -> fromIntegral ((flushedAt - min j flushedAt) `div` 1000)) (concat joins)
      stat =
        FlushStat
          { fsType = ty
          , fsBatch = Map.size slots
          , fsWaiters = length waits
          , fsWaitMicros = waits
          }
  atomically $ do
    for_ (Map.toList slots) $ \(k, ks) ->
      putTMVar (ksResult ks) $ case outcome of
        Left e -> Left e
        Right m -> Right (Map.lookup k m)
    modifyTVar' (cFlushLog c) $ \l ->
      Seq.drop (Seq.length l + 1 - flushLogBound) (l Seq.|> stat)
    putTMVar (wDone w) ()
  -- §19.3: the flush is a loader invocation (batch size) and the end of
  -- every join's window wait. No-op (and no loop) when telemetry is off.
  when (telemetryEnabled (cTelemetry c)) $ do
    recordLoaderBatch (cTelemetry c) (unTypeName ty) (fsBatch stat)
    for_ (fsWaitMicros stat) (recordCoalesceWait (cTelemetry c) (unTypeName ty))
  -- An async exception delivered mid-load has now failed every waiter;
  -- honor it on this thread too.
  case outcome of
    Left e | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
    _ -> pure ()


awaitSlot :: KeySlot -> IO (Maybe (Either BackendFailure LoadResult))
awaitSlot ks =
  atomically (readTMVar (ksResult ks)) >>= \case
    Left e -> throwIO e
    Right r -> pure r
