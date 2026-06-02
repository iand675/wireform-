{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.AsyncIO
-- Description : Bounded-concurrency, ordered\/unordered async I\/O operator
--
-- This module is part of the /Riffle/ extension tier (see
-- @wireform-kafka/streams/RIFFLE_SPEC.md@). It ships an
-- async-I\/O operator family that decouples per-record latency from
-- throughput on the stream thread, with bounded in-flight work,
-- explicit failure handling, EOS-compatible drain semantics, and
-- ordered\/unordered output modes.
--
-- == Why this is not @foreachAsync@
--
-- The 'Kafka.Streams.Topology.Free' haddock explains at length why
-- a fire-and-forget @foreachAsync@ is unsafe: no backpressure,
-- silent errors, EOS-incompatibility, lost per-key ordering. The
-- 'Kafka.Streams.AsyncIO.Config.AsyncIOConfig' record forces a
-- user-facing answer to each of those four problems before the
-- operator can be constructed.
--
-- == Threading model
--
-- @
--   stream thread (engine)              worker pool (N threads)
--   --------------------               -------------------------
--   procProcess(rec)
--   1. drain reorder buffer             dequeue from inFlight
--      -> ctxForward results            run user IO (with retry)
--   2. check failure TVar               deposit result in reorder
--      throw on failure                 buffer keyed by seqNo
--   3. enqueue rec on inFlight
--      (blocks STM-style when           wall-clock punctuator on
--      buffer full = backpressure)      stream thread also drains
-- @
--
-- Forwarding only happens on the stream thread — 'ctxForward' is
-- 'IORef'-backed and not thread-safe. Worker threads just deposit
-- results; the stream thread is responsible for sweeping them
-- downstream.
--
-- == Drain triggers
--
-- The sweep runs on:
--
--   * Every 'procProcess' entry (handles steady traffic naturally).
--   * A wall-clock punctuator registered in 'procInit', cadence
--     controlled by 'AsyncDrainTrigger' (handles input stalls).
--   * 'procClose' — final drain before the engine shuts the task
--     down (handles graceful shutdown).
--
-- The framework's EOS commit-cycle drain hook will additionally
-- block on the in-flight queue draining to empty before offsets
-- are committed; that wiring lives in
-- @Kafka.Streams.Runtime.EOS@ and is a separate PR.
module Kafka.Streams.AsyncIO
  ( -- * Re-exports from "Kafka.Streams.AsyncIO.Config"
    module Kafka.Streams.AsyncIO.Config
    -- * Smart constructors over 'KStream'
  --
  -- Each operation has two forms: a default-serde variant that
  -- resolves the new wire type's 'Serde' via 'HasSerde', and a
  -- @*With@ override that takes an explicit 'Serde' (useful for
  -- alternative codecs over the same Haskell type).
  , asyncMapValues
  , asyncMapValuesWith
  , asyncMapKeyValue
  , asyncMapKeyValueWith
  , asyncConcatMapValues
  , asyncConcatMapValuesWith
    -- * Processor builders
  --
  -- Used by the 'Kafka.Streams.Topology.Free.Prim' interpreter.
  -- Most user code should reach for the 'KStream'-shaped smart
  -- constructors above; the processor builders are exposed for
  -- callers that compose the imperative
  -- 'Kafka.Streams.Topology.Topology' graph by hand.
  , asyncMapValuesProc
  , asyncMapKeyValueProc
  , asyncConcatMapValuesProc
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.STM
import Control.Exception
  ( SomeException
  , throwIO
  , try
  )
import qualified Control.Exception as Exception
import Control.Monad (replicateM, unless, void, when)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))

import Kafka.Streams.AsyncIO.Config
import Kafka.Streams.KStream (KStream (..))
import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.Serde (HasSerde (..), Serde)
import Kafka.Streams.Processor
  ( Cancellable (..)
  , Processor (..)
  , ProcessorContext (..)
  , PunctuationType (WallClockTimePunctuation)
  , Punctuator (..)
  , forwardRecord
  , processorName
  )
import Kafka.Streams.Time (Duration, durationMillis)
import Kafka.Streams.Types (Record (..))

----------------------------------------------------------------------
-- Smart constructors over 'KStream'
----------------------------------------------------------------------

-- | Async analogue of 'Kafka.Streams.KStream.mapValuesM'. The
-- supplied @v -> IO v'@ runs on the configured worker pool; the
-- stream thread enqueues, drains completed results, and forwards
-- downstream.
--
-- Compared to 'Kafka.Streams.KStream.mapValuesM', this trades the
-- "synchronous on the stream thread" contract for bounded-
-- concurrency throughput. Backpressure is preserved by the
-- bounded in-flight queue; per-key ordering is preserved when
-- 'aioOutputMode' = 'OrderedOutput'.
--
-- @
-- enriched <- asyncMapValues cfg lookupExternal s
-- @
asyncMapValues
  :: forall k v v'
   . HasSerde v'
  => AsyncIOConfig
  -> (v -> IO v')
  -> KStream k v
  -> IO (KStream k v')
asyncMapValues = asyncMapValuesWith serde

-- | 'asyncMapValues' with an explicit downstream value 'Serde'.
asyncMapValuesWith
  :: forall k v v'
   . Serde v'
  -> AsyncIOConfig
  -> (v -> IO v')
  -> KStream k v
  -> IO (KStream k v')
asyncMapValuesWith vs' cfg f s =
  KS.attachProcessor s (aioName cfg)
    (asyncMapValuesProc cfg f)
    (kstreamKeySerde s)
    vs'

-- | Async analogue of 'Kafka.Streams.KStream.mapKeyValueM'. May
-- change both key and value; the new types' default serdes are
-- resolved via 'HasSerde'.
asyncMapKeyValue
  :: forall k v k' v'
   . (HasSerde k', HasSerde v')
  => AsyncIOConfig
  -> (k -> v -> IO (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
asyncMapKeyValue = asyncMapKeyValueWith serde serde

-- | 'asyncMapKeyValue' with explicit downstream key + value 'Serde's.
asyncMapKeyValueWith
  :: forall k v k' v'
   . Serde k'
  -> Serde v'
  -> AsyncIOConfig
  -> (k -> v -> IO (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
asyncMapKeyValueWith ks' vs' cfg f s =
  KS.attachProcessor s (aioName cfg)
    (asyncMapKeyValueProc cfg f)
    ks'
    vs'

-- | Async analogue of 'Kafka.Streams.KStream.concatMapValues' but
-- with effectful expansion: each input record yields zero or more
-- outputs, computed on the worker pool. Ordering across the
-- emitted list is preserved within a single input record;
-- ordering across input records follows 'aioOutputMode'.
asyncConcatMapValues
  :: forall k v v'
   . HasSerde v'
  => AsyncIOConfig
  -> (v -> IO [v'])
  -> KStream k v
  -> IO (KStream k v')
asyncConcatMapValues = asyncConcatMapValuesWith serde

-- | 'asyncConcatMapValues' with an explicit downstream value 'Serde'.
asyncConcatMapValuesWith
  :: forall k v v'
   . Serde v'
  -> AsyncIOConfig
  -> (v -> IO [v'])
  -> KStream k v
  -> IO (KStream k v')
asyncConcatMapValuesWith vs' cfg f s =
  KS.attachProcessor s (aioName cfg)
    (asyncConcatMapValuesProc cfg f)
    (kstreamKeySerde s)
    vs'

----------------------------------------------------------------------
-- Processor builders
----------------------------------------------------------------------

-- | Processor backing 'asyncMapValues'.
asyncMapValuesProc
  :: forall k v v'
   . AsyncIOConfig
  -> (v -> IO v')
  -> IO (Processor k v)
asyncMapValuesProc cfg f =
  buildAsyncProcessor cfg
    (\r -> do
       v' <- f (recordValue r)
       pure (Seq.singleton (recordWithValue r v')))

-- | Processor backing 'asyncMapKeyValue'.
asyncMapKeyValueProc
  :: forall k v k' v'
   . AsyncIOConfig
  -> (k -> v -> IO (k', v'))
  -> IO (Processor k v)
asyncMapKeyValueProc cfg f =
  buildAsyncProcessor cfg
    (\r -> do
       let mk = recordKey r
       case mk of
         Nothing -> pure Seq.empty
           -- The JVM convention for a 'Nothing' key on
           -- KeyValueMapper is to skip; we follow the same
           -- contract.
         Just k -> do
           (!k', !v') <- f k (recordValue r)
           pure (Seq.singleton (recordWithKeyValue r k' v')))

-- | Processor backing 'asyncConcatMapValues'.
asyncConcatMapValuesProc
  :: forall k v v'
   . AsyncIOConfig
  -> (v -> IO [v'])
  -> IO (Processor k v)
asyncConcatMapValuesProc cfg f =
  buildAsyncProcessor cfg
    (\r -> do
       vs <- f (recordValue r)
       pure (Seq.fromList (fmap (recordWithValue r) vs)))

----------------------------------------------------------------------
-- Generic builder
----------------------------------------------------------------------

-- | Build the processor for an async-I\/O operator. The supplied
-- @work@ function is what the worker pool runs per input record
-- and returns the (possibly empty, possibly multi-element)
-- sequence of records to forward downstream. The wrapper handles
-- queueing, ordering, retry, failure policy, draining, and
-- shutdown.
buildAsyncProcessor
  :: forall k v k' v'
   . AsyncIOConfig
  -> (Record k v -> IO (Seq (Record k' v')))
  -> IO (Processor k v)
buildAsyncProcessor cfg0 work = do
  let cfg = sanitiseConfig cfg0
  ctxRef       <- newIORef Nothing
  inFlight     <- newTBQueueIO (fromIntegral (aioBufferCapacity cfg))
  reorderRef   <- newTVarIO Map.empty
  unorderedQ   <- newTQueueIO
  nextInRef    <- newTVarIO 0
  nextOutRef   <- newTVarIO 0
  depositedRef <- newTVarIO 0
  failureRef   <- newTVarIO Nothing
  shutdownVar  <- newTVarIO False
  workersRef   <- newIORef ([] :: [Async ()])
  punCancel    <- newIORef Nothing

  let st = AsyncProcState
        { apsCfg         = cfg
        , apsInFlight    = inFlight
        , apsReorder     = reorderRef
        , apsUnordered   = unorderedQ
        , apsNextIn      = nextInRef
        , apsNextOut     = nextOutRef
        , apsDeposited   = depositedRef
        , apsFailure     = failureRef
        , apsShutdown    = shutdownVar
        , apsWorkersRef  = workersRef
        , apsPunCancel   = punCancel
        }

  pure Processor
    { procName    = processorName (aioName cfg)
    , procInit    = \ctx -> do
        writeIORef ctxRef (Just ctx)
        ws <- replicateM (aioWorkers cfg) $
                async (workerLoop st work)
        writeIORef workersRef ws
        case aioDrainTrigger cfg of
          DrainOnEntry -> pure ()
          DrainOnEntryAndPunctuator dur -> do
            let intervalMs = max 1 (fromIntegral (durationMillis dur)) :: Int
                pun = Punctuator $ \_now -> do
                  mctx2 <- readIORef ctxRef
                  case mctx2 of
                    Nothing  -> pure ()
                    Just c2  -> drainAndCheck st c2
            c <- ctxSchedule ctx intervalMs WallClockTimePunctuation pun
            writeIORef punCancel (Just c)
        -- Riffle: register the EOS pre-commit drain. The engine
        -- calls this on the stream thread before flushing stores
        -- and the record collector, so every in-flight async
        -- result lands in the same commit transaction as the
        -- source offsets it was produced from.
        ctxRegisterPreCommitDrain ctx (preCommitDrainAction st ctxRef)
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> do
            drainAndCheck st ctx
            sn <- atomically $ do
              n <- readTVar nextInRef
              writeTVar nextInRef (n + 1)
              writeTBQueue inFlight (Pending n r)
              pure n
            seq sn (pure ())
    , procClose   = closeProc st
    }

----------------------------------------------------------------------
-- Internal state
----------------------------------------------------------------------

type SeqNo = Int64

data Pending k v = Pending !SeqNo !(Record k v)

data AsyncProcState k v k' v' = AsyncProcState
  { apsCfg         :: !AsyncIOConfig
  , apsInFlight    :: !(TBQueue (Pending k v))
  , apsReorder     :: !(TVar (Map SeqNo (Maybe (Seq (Record k' v')))))
    -- ^ For 'OrderedOutput': @Just outs@ means the slot is
    --   complete; @Nothing@ means the slot was skipped (failure
    --   with 'DropAndContinue' \/ 'LogAndContinue' \/
    --   'CustomFailure'). Drain pops slots in 'SeqNo' order so
    --   downstream sees input order regardless of completion order.
  , apsUnordered   :: !(TQueue (Seq (Record k' v')))
    -- ^ For 'UnorderedOutput': completed batches in completion
    --   order. Skipped failures contribute nothing.
  , apsNextIn      :: !(TVar SeqNo)
    -- ^ Next sequence number to assign on enqueue. Equal to the
    --   total number of records submitted so far.
  , apsNextOut     :: !(TVar SeqNo)
  , apsDeposited   :: !(TVar SeqNo)
    -- ^ Total number of records the worker pool has deposited
    --   (success or skip) — used by 'preCommitDrain' to wait
    --   until every in-flight request has been resolved, so the
    --   EOS pre-commit hook can guarantee no async work is
    --   stranded across a commit boundary.
  , apsFailure     :: !(TVar (Maybe SomeException))
  , apsShutdown    :: !(TVar Bool)
  , apsWorkersRef  :: !(IORef [Async ()])
  , apsPunCancel   :: !(IORef (Maybe Cancellable))
  }

----------------------------------------------------------------------
-- Worker loop
----------------------------------------------------------------------

workerLoop
  :: AsyncProcState k v k' v'
  -> (Record k v -> IO (Seq (Record k' v')))
  -> IO ()
workerLoop st work = loop
  where
    cfg = apsCfg st
    loop = do
      mPending <- atomically $ do
        empty <- isEmptyTBQueue (apsInFlight st)
        if empty
          then do
            shut <- readTVar (apsShutdown st)
            if shut
              then pure Nothing
              else retry
          else Just <$> readTBQueue (apsInFlight st)
      case mPending of
        Nothing            -> pure ()
        Just (Pending n r) -> do
          result <- runWithRetry cfg (work r)
          handleResult st cfg n result
          loop

-- | Run the user-supplied IO with the configured retry strategy.
-- 'aioTimeout' is enforced by racing against a timer; whichever
-- finishes first wins. We model the timeout via STM
-- ('registerDelay') so we don't burn a thread on a sleep.
runWithRetry
  :: AsyncIOConfig
  -> IO a
  -> IO (Either SomeException a)
runWithRetry cfg act = go (retryAttempts (aioRetry cfg)) 0
  where
    go remaining attemptIdx = do
      result <- runOnce cfg act
      case result of
        Right v -> pure (Right v)
        Left e
          | remaining <= 0 -> pure (Left e)
          | otherwise      -> do
              sleepRetry (aioRetry cfg) attemptIdx
              go (remaining - 1) (attemptIdx + 1)

runOnce :: AsyncIOConfig -> IO a -> IO (Either SomeException a)
runOnce cfg act = do
  let timeoutMs = durationMillis (aioTimeout cfg)
  if timeoutMs <= 0
    then try act
    else do
      timerVar <- registerDelay (fromIntegral (timeoutMs * 1000))
      doneVar  <- newTVarIO (Nothing :: Maybe (Either SomeException a))
      runner <- async $ do
        r <- try act
        atomically (writeTVar doneVar (Just r))
      outcome <- atomically $ do
        d <- readTVar doneVar
        case d of
          Just r -> pure (Right r)
          Nothing -> do
            t <- readTVar timerVar
            if t then pure (Left ()) else retry
      case outcome of
        Right r -> pure r
        Left () -> do
          Async.cancel runner
          pure (Left timeoutException)

timeoutException :: SomeException
timeoutException = Exception.toException (Exception.ErrorCall "AsyncIO: request timeout")

retryAttempts :: AsyncRetryStrategy -> Int
retryAttempts = \case
  NoRetry            -> 0
  RetryFixed n _     -> max 0 n
  RetryBackoff n _ _ -> max 0 n

-- | Sleep before the next retry. @attemptIdx@ is zero-based: the
-- first retry sleeps with @attemptIdx = 0@.
sleepRetry :: AsyncRetryStrategy -> Int -> IO ()
sleepRetry s attemptIdx = case s of
  NoRetry                      -> pure ()
  RetryFixed _ dur             -> sleepDuration dur
  RetryBackoff _ initial mult  ->
    -- Pure 'Int64' math: delayMs = initialMs * mult ^ attemptIdx,
    -- clamped at 'maxBound :: Int64' so extreme inputs (e.g.
    -- @mult = 10 ^^ 20@) don't wrap around to a negative sleep.
    let !baseMs   = durationMillis initial
        !factor   = ipowClamped (fromIntegral (max 1 mult)) attemptIdx
        !delayMs  = saturatingMul baseMs factor
    in when (delayMs > 0)
         (threadDelay (saturatingToMicros delayMs))

sleepDuration :: Duration -> IO ()
sleepDuration d =
  let ms = durationMillis d
  in when (ms > 0) (threadDelay (saturatingToMicros ms))

-- | @ipowClamped base exp@: @base ^ exp@ in 'Int64', clamped at
-- 'maxBound' to avoid wrap-around on extreme inputs. Negative
-- exponents are treated as zero (caller bug; better than crashing
-- the worker thread).
ipowClamped :: Int64 -> Int -> Int64
ipowClamped _ e | e <= 0 = 1
ipowClamped b e          = go 1 e
  where
    !cap = maxBound :: Int64
    go !acc 0  = acc
    go !acc k
      | acc > cap `div` (max 1 b) = cap
      | otherwise                 = go (acc * b) (k - 1)

-- | Saturating multiplication on 'Int64' — returns 'maxBound' if
-- the unbounded product would overflow.
saturatingMul :: Int64 -> Int64 -> Int64
saturatingMul a b
  | a == 0 || b == 0      = 0
  | a > cap `div` b       = cap
  | otherwise             = a * b
  where
    !cap = maxBound :: Int64

-- | Convert a millisecond count to microseconds for
-- 'threadDelay', clamped at 'maxBound :: Int' so we never pass a
-- negative or wrapped value to the RTS.
saturatingToMicros :: Int64 -> Int
saturatingToMicros ms
  | ms <= 0                                            = 0
  | ms > fromIntegral (maxBound :: Int) `div` 1000     = maxBound
  | otherwise                                          = fromIntegral ms * 1000

----------------------------------------------------------------------
-- Result handling
----------------------------------------------------------------------

handleResult
  :: AsyncProcState k v k' v'
  -> AsyncIOConfig
  -> SeqNo
  -> Either SomeException (Seq (Record k' v'))
  -> IO ()
handleResult st cfg sn r = do
  -- During shutdown the engine no longer drains nor inspects
  -- 'apsFailure'; bumping deposited / firing the hook / setting
  -- failure flags would just be dead writes. Short-circuit so
  -- the worker can exit promptly when 'closeProc' has set the
  -- shutdown flag and cancelled its outstanding IO.
  shutdownNow <- readTVarIO (apsShutdown st)
  if shutdownNow
    then pure ()
    else do
      case r of
        Right batch ->
          depositSuccess st cfg sn batch
        Left e -> do
          keep <- applyFailurePolicy (aioOnFailure cfg) e
          if keep
            then
              -- Failure was non-fatal — record a skipped slot
              -- so the ordered drain doesn't stall on a hole.
              depositSkip st cfg sn
            else do
              -- Failure is fatal — surface to the stream
              -- thread on the next drain. We deposit a skipped
              -- slot too so the drain machinery can keep
              -- going, but the failure TVar is what the stream
              -- thread inspects.
              atomically $
                writeTVar (apsFailure st) (Just e)
              depositSkip st cfg sn
      -- Fire the post-deposit observability hook. Run AFTER
      -- the STM deposit so observers see only durable state.
      -- A throwing hook surfaces via the failure TVar; the
      -- worker keeps running so other in-flight work isn't
      -- lost.
      hookResult <- try (aioOnDeposit cfg) :: IO (Either SomeException ())
      case hookResult of
        Right () -> pure ()
        Left  he -> atomically $ writeTVar (apsFailure st) (Just he)

depositSuccess
  :: AsyncProcState k v k' v'
  -> AsyncIOConfig
  -> SeqNo
  -> Seq (Record k' v')
  -> IO ()
depositSuccess st cfg sn batch = atomically $ do
  case aioOutputMode cfg of
    OrderedOutput   ->
      modifyTVar' (apsReorder st) (Map.insert sn (Just batch))
    UnorderedOutput ->
      unless (Seq.null batch) $
        writeTQueue (apsUnordered st) batch
  modifyTVar' (apsDeposited st) (+ 1)

depositSkip
  :: AsyncProcState k v k' v'
  -> AsyncIOConfig
  -> SeqNo
  -> IO ()
depositSkip st cfg sn = atomically $ do
  case aioOutputMode cfg of
    OrderedOutput   ->
      modifyTVar' (apsReorder st) (Map.insert sn Nothing)
    UnorderedOutput ->
      -- Unordered: nothing to deposit. The seqNo is irrelevant
      -- because completion order drives output.
      pure ()
  modifyTVar' (apsDeposited st) (+ 1)

-- | Apply the failure policy. Returns 'True' when the failure is
-- non-fatal (downstream-skipped) and 'False' when the task should
-- shut down.
applyFailurePolicy :: AsyncFailurePolicy -> SomeException -> IO Bool
applyFailurePolicy p e = case p of
  FailTask          -> pure False
  DropAndContinue   -> pure True
  LogAndContinue    -> do
    -- Match the engine's logAndContinue style: write the
    -- exception to stderr-ish via 'putStrLn'. We don't depend on
    -- the engine's logger here to keep this module free of
    -- 'Engine' coupling.
    putStrLn ("[AsyncIO] dropping record: " <> show e)
    pure True
  CustomFailure h   -> do
    _ <- try (h e) :: IO (Either SomeException ())
    pure True

----------------------------------------------------------------------
-- Drain
----------------------------------------------------------------------

-- | EOS pre-commit drain: block on the stream thread until every
-- submitted request has been deposited by the worker pool, then
-- forward whatever's ready downstream.
--
-- Invoked by the engine's 'drainPreCommit' / 'commitEngine' just
-- before stores and the record collector are flushed. The
-- producer-side transaction therefore captures every async
-- output that corresponds to a source offset committed by the
-- same cycle.
--
-- Blocks indefinitely if the user IO hangs — that's the correct
-- EOS behaviour: the commit should not finalise while there is
-- still work that might be lost on a crash.
preCommitDrainAction
  :: AsyncProcState k v k' v'
  -> IORef (Maybe ProcessorContext)
  -> IO ()
preCommitDrainAction st ctxRef = do
  -- Wait for every in-flight request to be resolved (success or
  -- skip). After this barrier the reorder / unordered buffers
  -- hold everything the workers produced for in-flight work.
  atomically $ do
    submitted <- readTVar (apsNextIn st)
    deposited <- readTVar (apsDeposited st)
    unless (deposited >= submitted) retry
  -- Now drain on the stream thread — same path the periodic
  -- punctuator uses, including the failure-TVar re-throw.
  mctx <- readIORef ctxRef
  case mctx of
    Nothing  -> pure ()
    Just ctx -> drainAndCheck st ctx

drainAndCheck
  :: AsyncProcState k v k' v'
  -> ProcessorContext
  -> IO ()
drainAndCheck st ctx = do
  drainReady st ctx
  mErr <- atomically $ do
    e <- readTVar (apsFailure st)
    case e of
      Just _  -> writeTVar (apsFailure st) Nothing >> pure e
      Nothing -> pure Nothing
  case mErr of
    Nothing  -> pure ()
    Just exc -> throwIO exc

drainReady
  :: AsyncProcState k v k' v'
  -> ProcessorContext
  -> IO ()
drainReady st ctx = case aioOutputMode (apsCfg st) of
  OrderedOutput   -> drainOrdered st ctx
  UnorderedOutput -> drainUnordered st ctx

drainOrdered
  :: AsyncProcState k v k' v'
  -> ProcessorContext
  -> IO ()
drainOrdered st ctx = loop
  where
    loop = do
      batch <- atomically $ do
        next <- readTVar (apsNextOut st)
        m    <- readTVar (apsReorder st)
        case Map.lookup next m of
          Nothing      -> pure Nothing
          Just mBatch  -> do
            writeTVar (apsReorder st) (Map.delete next m)
            writeTVar (apsNextOut st) (next + 1)
            pure (Just mBatch)
      case batch of
        Nothing       -> pure ()
        Just Nothing  -> loop   -- skipped slot
        Just (Just b) -> do
          mapM_ (forwardRecord ctx) (toListSeq b)
          loop

drainUnordered
  :: AsyncProcState k v k' v'
  -> ProcessorContext
  -> IO ()
drainUnordered st ctx = loop
  where
    loop = do
      mBatch <- atomically (tryReadTQueue (apsUnordered st))
      case mBatch of
        Nothing -> pure ()
        Just b  -> do
          mapM_ (forwardRecord ctx) (toListSeq b)
          loop

toListSeq :: Seq a -> [a]
toListSeq = foldr (:) []

----------------------------------------------------------------------
-- Shutdown
----------------------------------------------------------------------

closeProc :: AsyncProcState k v k' v' -> IO ()
closeProc st = do
  mC <- readIORef (apsPunCancel st)
  case mC of
    Just c  -> cancel c       -- 'cancel' here is 'Cancellable.cancel'
    Nothing -> pure ()
  atomically $ writeTVar (apsShutdown st) True
  ws <- readIORef (apsWorkersRef st)
  -- Cancel each worker so any in-flight user 'IO' (typically
  -- blocked on a 'MVar', a socket, a database call) is broken
  -- out of via an async exception. Without this, a hung user
  -- handler would keep 'closeProc' (and therefore
  -- 'closeDriver' / 'closeEngine' / EOS shutdown) blocked
  -- indefinitely.
  --
  -- The worker's @try@ wrapper catches 'AsyncCancelled' as a
  -- regular failure; 'handleResult' short-circuits when
  -- 'apsShutdown' is set so the cancellation is /not/ routed
  -- through the configured failure policy.
  mapM_ Async.cancel ws
  mapM_ (void . waitCatch) ws
  -- Final drain attempts to forward anything that completed
  -- between the last drain and shutdown. We don't have a
  -- 'ProcessorContext' here (the engine has torn the chain down
  -- in 'closeEngine'), so we simply clear the reorder buffer to
  -- release memory. The pre-commit drain hook on commit is
  -- the mechanism that guarantees no in-flight work is lost
  -- across a commit boundary; close after a commit therefore
  -- has nothing left to forward.
  atomically $ do
    writeTVar (apsReorder st) Map.empty
    -- Drain unordered queue
    let drainUQ = do
          x <- tryReadTQueue (apsUnordered st)
          case x of
            Nothing -> pure ()
            Just _  -> drainUQ
    drainUQ

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

sanitiseConfig :: AsyncIOConfig -> AsyncIOConfig
sanitiseConfig c = c
  { aioBufferCapacity = max 1 (aioBufferCapacity c)
  , aioWorkers        = max 1 (aioWorkers c)
  }

recordWithValue :: Record k v -> v' -> Record k v'
recordWithValue r v' = Record
  { recordKey       = recordKey r
  , recordValue     = v'
  , recordTimestamp = recordTimestamp r
  , recordHeaders   = recordHeaders r
  }

recordWithKeyValue :: Record k v -> k' -> v' -> Record k' v'
recordWithKeyValue r k' v' = Record
  { recordKey       = Just k'
  , recordValue     = v'
  , recordTimestamp = recordTimestamp r
  , recordHeaders   = recordHeaders r
  }

