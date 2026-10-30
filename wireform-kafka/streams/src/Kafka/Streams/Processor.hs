{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Kafka.Streams.Processor
Description : Processor + ProcessorContext + Punctuator

The processor API is intentionally close to
@org.apache.kafka.streams.processor.api@:

  * 'ProcessorContext' is the dependency-injection seam every
    processor sees (forward, schedule, recordMetadata, getStore).
  * 'Processor' is a record of three callbacks:
    'procInit' / 'procProcess' / 'procClose'. We pass parents
    /typed/ so users do not have to type-cast at process time.
  * 'Punctuator' lets a processor schedule periodic callbacks driven
    either by 'StreamTimePunctuation' (advances with stream time,
    i.e. record timestamps) or 'WallClockTimePunctuation' (advances
    with the system clock).
-}
module Kafka.Streams.Processor (
  -- * Processor
  Processor (..),
  noopProcessor,
  statelessProcessor,

  -- * Fixed-key processor (KIP-820)
  FixedKeyProcessor (..),
  FixedKeyRecord,
  fixedKeyOf,
  liftFixedKeyProcessor,

  -- * Typed handles to processor identity
  ProcessorName (..),
  processorName,

  -- * Context
  ProcessorContext (..),
  currentRecordMetadata,
  forwardRecord,
  forwardTo,
  schedule,
  taskId,
  applicationIdC,
  streamTimeC,
  wallClockTimeC,
  getStateStore,
  effectiveTime,
  SinkEmit (..),
  currentHeaders,
  appendHeader,
  requestCommit,

  -- * Punctuators
  Punctuator (..),
  PunctuationType (..),
  Cancellable (..),
  cancelled,
  forwardingPunctuator,

  -- * Task identifiers
  TaskId (..),
  taskIdText,

  -- * Processor suppliers (KIP-820)
  ProcessorSupplier (..),
  supplierOf,
  supplierWithStores,
) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kafka.Streams.State.Store (AnyStateStore, StoreName)
import Kafka.Streams.Time (Timestamp)
import Kafka.Streams.Types (NodeName, Record (..), RecordMetadata)
import Kafka.Streams.Types qualified


{- | Logical processor name. Two processors with the same name belong
to the same node in the topology — no two distinct processor
instances may share a name.
-}
newtype ProcessorName = ProcessorName {unProcessorName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


processorName :: Text -> ProcessorName
processorName = ProcessorName


{- | Subtopology task identifier (matches Java's
@TaskId(subtopologyId, partition)@).
-}
data TaskId = TaskId
  { taskSubtopology :: !Int
  , taskPartition :: !Int32
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


taskIdText :: TaskId -> Text
taskIdText (TaskId sub p) =
  T.pack (show sub) <> "_" <> T.pack (show p)


-- | When does the punctuator fire?
data PunctuationType
  = -- | Stream-time (Java: @PunctuationType.STREAM_TIME@)
    StreamTimePunctuation
  | -- | Wall-clock (Java: @PunctuationType.WALL_CLOCK_TIME@)
    WallClockTimePunctuation
  deriving stock (Eq, Show, Generic)


-- | A user-supplied callback fired by 'schedule'.
newtype Punctuator = Punctuator
  { runPunctuator :: Timestamp -> IO ()
  }


-- | Token returned by 'schedule' for cancelling the punctuator.
newtype Cancellable = Cancellable
  { cancel :: IO ()
  }


-- | A no-op cancel token.
cancelled :: Cancellable
cancelled = Cancellable (pure ())


{- | Build a 'Punctuator' that forwards a record on every fire by
calling the supplied builder with the fire timestamp. Returning
'Nothing' from the builder skips that fire (no record forwarded).
-}
forwardingPunctuator
  :: ProcessorContext
  -> (Timestamp -> IO (Maybe (Record k v)))
  -> Punctuator
forwardingPunctuator ctx f = Punctuator $ \ts -> do
  m <- f ts
  case m of
    Nothing -> pure ()
    Just r -> ctxForward ctx r


{- | The full processor surface. Generic over the input @(k, v)@.

'procInit' is called once with the typed 'ProcessorContext' and is
the place to register stores / punctuators / read configuration.
'procProcess' is the per-record callback. 'procClose' is invoked on
task shutdown (after the last commit).
-}
data Processor k v = Processor
  { procName :: !ProcessorName
  , procInit :: !(ProcessorContext -> IO ())
  , procProcess :: !(Record k v -> IO ())
  , procClose :: !(IO ())
  }


-- | A processor that does nothing.
noopProcessor :: ProcessorName -> Processor k v
noopProcessor n =
  Processor
    { procName = n
    , procInit = \_ -> pure ()
    , procProcess = \_ -> pure ()
    , procClose = pure ()
    }


{- | Convenience constructor for stateless transformers that don't
need 'procInit'.
-}
statelessProcessor
  :: ProcessorName
  -> (Record k v -> IO ())
  -> Processor k v
statelessProcessor n p =
  Processor
    { procName = n
    , procInit = \_ -> pure ()
    , procProcess = p
    , procClose = pure ()
    }


{- | Dependency-injection record handed to every processor.

We pin the downstream forward type as 'forwardAny' (existentially
typed). The DSL erases the value's static type when crossing
processor boundaries; the runtime preserves type-safety at the
/edges/ (sources / sinks) and trusts the topology builder to wire
compatible pairs in between. This matches the Java erasure.
-}
data ProcessorContext = ProcessorContext
  { ctxApplicationId :: !Text
  , ctxTaskId :: !TaskId
  , ctxRecordMetadata :: !(IO (Maybe RecordMetadata))
  , ctxStreamTime :: !(IO Timestamp)
  , ctxWallClockTime :: !(IO Timestamp)
  , ctxForward :: !(forall k v. Record k v -> IO ())
  {- ^ Forward to all downstream nodes. The runtime tags every
  forward with the current (sourceNode, recordMetadata) so sink
  processors can attribute it correctly.
  -}
  , ctxForwardTo :: !(forall k v. NodeName -> Record k v -> IO ())
  -- ^ Forward to a single named downstream node. Used by 'branch'.
  , ctxSchedule :: !(Int -> PunctuationType -> Punctuator -> IO Cancellable)
  -- ^ @schedule intervalMs type pun@ — register a punctuator.
  , ctxGetStore :: !(StoreName -> IO (Maybe AnyStateStore))
  {- ^ Look up an attached store by name. Returns 'Nothing' if no
  such store was declared on this processor.
  -}
  , ctxEmitToTopic :: !(SinkEmit -> IO ())
  {- ^ Low-level: emit a record directly to the runtime's
  collector under an explicitly chosen topic. Used by
  'TopicNameExtractor' sinks; user code generally goes through
  'forwardRecord' instead.
  -}
  , ctxRecordHeaders :: !(IO (Maybe Kafka.Streams.Types.Headers))
  {- ^ Read the headers attached to the record currently being
  processed. Returns 'Nothing' when called from outside a
  record-processing context (e.g. from a punctuator).
  -}
  , ctxAddHeader :: !(Kafka.Streams.Types.Header -> IO ())
  {- ^ Append a header to the in-flight record. Subsequent
  'forwardRecord' calls (in the same 'procProcess' invocation)
  see the updated headers.
  -}
  , ctxRequestCommit :: !(IO ())
  {- ^ Request that the runtime commit at the next safe point.
  Mirrors @ProcessorContext.commit()@: the commit doesn't
  happen synchronously; the runtime picks it up at the end of
  the current commit window.
  -}
  , ctxRegisterPreCommitDrain :: !(IO () -> IO ())
  {- ^ Riffle: register a drain action that the engine will
  run /on the stream thread/ before the commit cycle
  flushes stores and the record collector. The action MUST
  block until any background work the processor depends on
  (e.g. an async-I\/O worker pool) has finished and been
  forwarded downstream — that's what makes the operator
  EOS-compatible with the producer's transactional commit.

  Processors that do not own background workers ignore this
  field. Drains run in registration order; an exception
  from any drain propagates back to 'commitEngine'
  (matching the JVM 'StateStore.flush' contract).
  -}
  , ctxCoordinatedWatermark :: !(IO (Maybe Timestamp))
  {- ^ Riffle \xc2\xa75: read the engine's cross-source effective
  watermark (min of every live source's per-source
  watermark, with idle-timeout skipping). Returns 'Nothing'
  when no coordinator is wired — the operator should fall
  back to 'ctxStreamTime' in that case. Operators that
  close windows or expire state (suppress, time-windowed
  aggregates) should prefer this when it's set so a
  per-task stream-time spike from one fast source doesn't
  close windows that the slow side hasn't reached yet.

  See 'effectiveTime' for the helper that combines this
  with 'ctxStreamTime'.
  -}
  }


{- | Bytes-already-serialised emission record. Defined here (rather
than in 'Kafka.Streams.Internal.RecordCollector') to avoid an
import cycle.
-}
data SinkEmit = SinkEmit
  { seTopic :: !Text
  , seKey :: !(Maybe ByteString)
  , seValue :: !ByteString
  , seTimestamp :: !Timestamp
  , seHeaders :: !Kafka.Streams.Types.Headers
  -- ^ Written onto the outgoing record verbatim.
  }
  deriving stock (Generic)


-- | Convenience access to the lookup function.
getStateStore :: ProcessorContext -> StoreName -> IO (Maybe AnyStateStore)
getStateStore = ctxGetStore


{- | Riffle \xc2\xa75: read the operator's effective event-time clock.
Returns the cross-source coordinated watermark when one is
wired (via 'Kafka.Streams.Internal.Engine.attachWatermarkCoordinator'),
falling back to the per-task 'ctxStreamTime' otherwise. This
is the recommended call for operators that close windows or
expire state — it makes them behave correctly under
mixed-rate sources without breaking single-source topologies
that don't wire a coordinator.
-}
effectiveTime :: ProcessorContext -> IO Timestamp
effectiveTime ctx = do
  mCoord <- ctxCoordinatedWatermark ctx
  case mCoord of
    Just t -> pure t
    Nothing -> ctxStreamTime ctx


currentRecordMetadata :: ProcessorContext -> IO (Maybe RecordMetadata)
currentRecordMetadata = ctxRecordMetadata


forwardRecord :: ProcessorContext -> Record k v -> IO ()
forwardRecord ctx r = ctxForward ctx r


forwardTo :: ProcessorContext -> NodeName -> Record k v -> IO ()
forwardTo ctx nm r = ctxForwardTo ctx nm r


schedule
  :: ProcessorContext
  -> Int
  -> PunctuationType
  -> Punctuator
  -> IO Cancellable
schedule = ctxSchedule


taskId :: ProcessorContext -> TaskId
taskId = ctxTaskId


applicationIdC :: ProcessorContext -> Text
applicationIdC = ctxApplicationId


streamTimeC :: ProcessorContext -> IO Timestamp
streamTimeC = ctxStreamTime


wallClockTimeC :: ProcessorContext -> IO Timestamp
wallClockTimeC = ctxWallClockTime


{- | Read the headers attached to the in-flight record. Returns
'Nothing' when called outside of a record-processing
context (e.g. from a punctuator).
-}
currentHeaders :: ProcessorContext -> IO (Maybe Kafka.Streams.Types.Headers)
currentHeaders = ctxRecordHeaders


{- | Append a header to the in-flight record. Mirrors Java's
@ProcessorContext.headers().add(...)@. Subsequent
'forwardRecord' calls in the same procProcess see the
updated headers.
-}
appendHeader :: ProcessorContext -> Kafka.Streams.Types.Header -> IO ()
appendHeader = ctxAddHeader


{- | Request that the runtime commit at the next safe point.
Mirrors Java's @ProcessorContext.commit()@. The commit
happens at the end of the current commit window, not
synchronously.
-}
requestCommit :: ProcessorContext -> IO ()
requestCommit = ctxRequestCommit


----------------------------------------------------------------------
-- KIP-820: Fixed-key processor
----------------------------------------------------------------------

{- | A 'FixedKeyRecord' is structurally the same as a 'Record'
but the type guarantees the processor cannot change the
key. Used as the input/output type for processors attached
via @processValues@. Mirrors Java's
@org.apache.kafka.streams.processor.api.FixedKeyRecord@.
-}
type FixedKeyRecord k v = Record k v


{- | Build a 'FixedKeyRecord' from a regular 'Record'. The
types are coincident in this port; the helper is here to
make the intent at the call site explicit.
-}
fixedKeyOf :: Record k v -> FixedKeyRecord k v
fixedKeyOf = id


{- | A fixed-key processor: like 'Processor' but the input and
output values share a key — the type makes that explicit.
The process function only forwards 'FixedKeyRecord' values
with the same key as the input.
-}
data FixedKeyProcessor k v v' = FixedKeyProcessor
  { name :: !ProcessorName
  , init :: !(ProcessorContext -> IO ())
  , process :: !(FixedKeyRecord k v -> IO (Maybe v'))
  , close :: !(IO ())
  }


{- | Convert a 'FixedKeyProcessor' into a regular 'Processor'
whose 'procProcess' forwards a record with the same key.
Used to bridge the typed surface into the existing engine,
which works in terms of 'Processor'.
-}
liftFixedKeyProcessor
  :: forall k v v'
   . FixedKeyProcessor k v v'
  -> IO (Processor k v)
liftFixedKeyProcessor fkp = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = fkp.name
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          fkp.init ctx
      , procProcess = \r -> do
          mctx <- readIORef ctxRef
          case mctx of
            Nothing -> pure ()
            Just ctx -> do
              mOut <- fkp.process (fixedKeyOf r)
              case mOut of
                Nothing -> pure ()
                Just v' ->
                  forwardRecord
                    ctx
                    ( ( Record
                          (recordKey r)
                          v'
                          (recordTimestamp r)
                          (recordHeaders r)
                      )
                        :: Record k v'
                    )
      , procClose = fkp.close
      }


----------------------------------------------------------------------
-- KIP-820: ProcessorSupplier with declared stores
----------------------------------------------------------------------

{- | A supplier of 'Processor' instances + a declaration of
which state stores the processor reads/writes. Mirrors
Java's @org.apache.kafka.streams.processor.api.ProcessorSupplier@
(which extends @Supplier<Processor>@ + @ConnectedStoreProvider@).
-}
data ProcessorSupplier k v = ProcessorSupplier
  { supply :: !(IO (Processor k v))
  , stores :: ![StoreName]
  {- ^ External state stores the processor declares it owns
  (or co-owns with a sibling). DSL helpers that accept
  a 'ProcessorSupplier' wire these into the topology
  automatically so callers don't have to call
  addStateStore + connectProcessorAndStateStores
  separately.
  -}
  }


-- | Stateless supplier: lift an @IO Processor k v@.
supplierOf :: IO (Processor k v) -> ProcessorSupplier k v
supplierOf m = ProcessorSupplier {supply = m, stores = []}


{- | Stateful supplier: lift an @IO Processor@ together with the
store names it depends on.
-}
supplierWithStores
  :: IO (Processor k v) -> [StoreName] -> ProcessorSupplier k v
supplierWithStores m ss =
  ProcessorSupplier
    { supply = m
    , stores = ss
    }
