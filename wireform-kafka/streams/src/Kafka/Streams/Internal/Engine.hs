{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Internal.Engine
Description : Topology engine — instantiates stores, processors and
              forwarders for one task

The engine is the implementation of the topology shape. It is shared
by 'Kafka.Streams.Driver' (the synchronous test driver) and
'Kafka.Streams.Runtime' (the broker-backed runtime).

A single engine corresponds to a single Kafka Streams /task/: a single
set of state stores, a single ProcessorContext, a single record
collector. The real 'Kafka.Streams.Runtime' instantiates one engine
per partition assignment.

== Type erasure

The Java engine has erasure for free; we don't. We thread records
through the engine as the existential type 'Erased' — backed by
@unsafeCoerce@ at every cross-node forwarder. The DSL builders
ensure that every parent of a node forwards records of the same
/actual/ type, so the coercion is sound. State-store accessors use
their own typed serdes and never rely on this trick.
-}
module Kafka.Streams.Internal.Engine (
  Engine,
  engineTopology,
  engineTaskId,
  engineAppId,
  engineCollector,
  engineStreamTime,
  engineWallClock,
  attachWatermarkCoordinator,
  setPartitionCountLookup,
  reportPollCycle,
  engineMetrics,
  buildEngine,
  feedSource,
  feedSourceH,
  advanceWallClock,
  syncWallClockToReal,
  advanceStreamTimeTo,
  triggerStreamTimePunctuators,
  triggerWallClockPunctuators,
  commitEngine,
  drainPreCommit,
  closeEngine,
  storeByName,
  streamTimeOfEngine,
  wallClockTimeOfEngine,
  takeCommitRequested,

  -- * Internals
  StoreEntry (..),
  Erased,
  erase,
  unsafeUnerase,
) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import GHC.Exts (Any)
import Kafka.Streams.Errors (
  DeserializationException (..),
  DeserializationHandler (..),
  DeserializationResponse (..),
  StreamsException (..),
 )
import Kafka.Streams.Internal.RecordCollector (
  CollectedRecord (..),
  RecordCollector (..),
 )
import Kafka.Streams.Metrics (
  MetricsRegistry,
  noopMetricsRegistry,
 )
import Kafka.Streams.Metrics qualified as Met
import Kafka.Streams.Processor (
  Cancellable (..),
  Processor (..),
  ProcessorContext (..),
  ProcessorName (..),
  PunctuationType (..),
  Punctuator (..),
  TaskId (..),
 )
import Kafka.Streams.Processor qualified
import Kafka.Streams.Produced (StreamPartitioner (..))
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueStore (..),
  SessionStore (..),
  StateStore (..),
  StoreBuilder (..),
  StoreBuilderKV (..),
  StoreBuilderS (..),
  StoreBuilderW (..),
  StoreName,
  WindowStore (..),
  unStoreName,
 )
import Kafka.Streams.Time (
  StreamTime (..),
  Timestamp (..),
  advanceStreamTime,
  initialStreamTime,
  runTimestampExtractor,
  utcTimeToTimestamp,
 )
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (
  Headers,
  Record (..),
  RecordMetadata (..),
  TopicName,
  emptyHeaders,
  unTopicName,
 )
import Kafka.Streams.Types qualified
import Kafka.Streams.Watermark qualified as Watermark
import Unsafe.Coerce (unsafeCoerce)


----------------------------------------------------------------------
-- Type erasure
----------------------------------------------------------------------

{- | An /erased/ value. We use 'GHC.Exts.Any' as the existential
carrier when threading records and serdes across the engine. The
DSL guarantees that the runtime types match before the engine
ever sees them.
-}
type Erased = Any


erase :: a -> Erased
erase = unsafeCoerce
{-# INLINE erase #-}


-- | The DSL guarantees the witness; the engine just does the cast.
unsafeUnerase :: Erased -> a
unsafeUnerase = unsafeCoerce
{-# INLINE unsafeUnerase #-}


-- | The universal forwarder shape used inside the engine.
type NodeForwarder = Record Erased Erased -> IO ()


----------------------------------------------------------------------
-- Engine state
----------------------------------------------------------------------

data StoreEntry = StoreEntry
  { storeEntryName :: !StoreName
  , storeEntryAny :: !AnyStateStore
  , storeEntryClose :: !(IO ())
  , storeEntryFlush :: !(IO ())
  }


data ProcessorRecord = ProcessorRecord
  { prClose :: !(IO ())
  , prName :: !ProcessorName
  }


data PunctuatorEntry = PunctuatorEntry
  { peType :: !PunctuationType
  , peIntervalMs :: !Int
  , peNextFireMs :: !Int64
  , pePunctuator :: !Punctuator
  , peCancelled :: !(IORef Bool)
  , peOwner :: !Topo.NodeName
  }


data SourceHandler = SourceHandler
  { shSourceName :: !Topo.NodeName
  , shTopics :: !(HashSet TopicName)
  -- ^ Source topics for this handler.
  , shHandler :: !(SourceInput -> IO ())
  }


data SourceInput = SourceInput
  { siTopic :: !TopicName
  , siPartition :: !Int
  , siOffset :: !Int64
  , siKey :: !(Maybe ByteString)
  , siValue :: !ByteString
  , siTimestamp :: !Timestamp
  , siHeaders :: !Headers
  }


data Engine = Engine
  { engineTopology :: !Topo.Topology
  , engineTaskId :: !TaskId
  , engineAppId :: !Text
  , engineCollector :: !RecordCollector
  , engineForwarders :: !(IORef (Map Topo.NodeName NodeForwarder))
  , engineSources :: !(IORef (Map Topo.NodeName SourceHandler))
  , engineStores :: !(IORef (Map StoreName StoreEntry))
  , engineProcessors :: !(IORef (Map Topo.NodeName ProcessorRecord))
  , engineStreamTime :: !(IORef StreamTime)
  , engineWallClock :: !(IORef Timestamp)
  , engineCurrentMd :: !(IORef (Maybe RecordMetadata))
  , engineCurrentHeaders :: !(IORef (Maybe Kafka.Streams.Types.Headers))
  , enginePunctuators :: !(IORef [PunctuatorEntry])
  , engineDeserHandler :: !DeserializationHandler
  , engineMetrics :: !MetricsRegistry
  , engineCommitRequested :: !(IORef Bool)
  , engineAsyncDrains :: !(IORef [IO ()])
  {- ^ Riffle: pre-commit drain registry. Processors that own
  background workers (today: 'Kafka.Streams.AsyncIO') register
  a drain action via 'ctxRegisterPreCommitDrain' during
  'procInit'. 'commitEngine' runs every registered drain
  /before/ flushing stores and the record collector so that:

  * In-flight async results are forwarded downstream and
    captured by the same commit transaction that captures
    the source-record offsets.
  * The producer's 'TxnOffsetCommit' is consistent with
    what the operator has emitted (the EOS contract).

  Drains run synchronously on the stream thread in
  registration order. An exception from any drain
  propagates as a 'commitEngine' failure.
  -}
  , enginePartitionCount :: !(IORef (Maybe (Text -> IO (Either String Int))))
  {- ^ Topic partition-count lookup for sinks that carry a
  'StreamPartitioner' (installed by the broker-backed runtime via
  'setPartitionCountLookup'). 'Nothing' (in-process drivers with
  no partition metadata) leaves every record's partition to the
  producer.
  -}
  , engineWatermarkCoord :: !(IORef (Maybe Watermark.WatermarkCoordinator))
  {- ^ Riffle \xc2\xa75: optional cross-source watermark coordinator.
  'Nothing' (the default) preserves per-task 'StreamTime'
  semantics. When set (via 'attachWatermarkCoordinator')
  'handleSource' calls
  'Kafka.Streams.Watermark.reportRecord' for every record
  whose source carries a 'sourceWatermarkStrategy'.
  -}
  }


----------------------------------------------------------------------
-- Build
----------------------------------------------------------------------

{- | Realise a topology into a runnable engine. Builds every store,
instantiates every processor (calling 'procInit' with the typed
'ProcessorContext'), wires forwarders, and registers every source
handler.
-}
buildEngine
  :: Topo.TopologyValid
  -> TaskId
  -> Text
  -> RecordCollector
  -> DeserializationHandler
  -> IO Engine
buildEngine validated tid appId collector deserHandler = do
  let topo = Topo.topologyValidGraph validated

  storeEntries <- forM (Map.toList (Topo.topoStores topo)) $ \(_nm, ab) ->
    realiseStore ab
  storesRef <-
    newIORef
      (Map.fromList [(storeEntryName se, se) | se <- storeEntries])

  forwardersRef <- newIORef Map.empty
  procsRef <- newIORef Map.empty
  sourcesRef <- newIORef Map.empty
  streamRef <- newIORef initialStreamTime
  wallRef <- nowAsTimestamp >>= newIORef
  currentMdRef <- newIORef Nothing
  currentHsRef <- newIORef Nothing
  pesRef <- newIORef []
  metrics <- noopMetricsRegistry
  commitReqRef <- newIORef False
  drainsRef <- newIORef []
  wmCoordRef <- newIORef Nothing
  partCountRef <- newIORef Nothing

  let engine =
        Engine
          { engineTopology = topo
          , engineTaskId = tid
          , engineAppId = appId
          , engineCollector = collector
          , engineForwarders = forwardersRef
          , engineSources = sourcesRef
          , engineStores = storesRef
          , engineProcessors = procsRef
          , engineStreamTime = streamRef
          , engineWallClock = wallRef
          , engineCurrentMd = currentMdRef
          , engineCurrentHeaders = currentHsRef
          , enginePunctuators = pesRef
          , engineDeserHandler = deserHandler
          , engineMetrics = metrics
          , engineCommitRequested = commitReqRef
          , engineAsyncDrains = drainsRef
          , enginePartitionCount = partCountRef
          , engineWatermarkCoord = wmCoordRef
          }

  -- Wire processors first so children-of references resolve.
  forM_ (Map.toList (Topo.topoProcessors topo)) $ \(nm, spec) ->
    instantiateProcessor engine nm spec
  forM_ (Map.toList (Topo.topoSinks topo)) $ \(nm, spec) ->
    instantiateSink engine nm spec
  forM_ (Map.toList (Topo.topoSources topo)) $ \(nm, spec) ->
    instantiateSource engine nm spec
  pure engine


----------------------------------------------------------------------
-- Stores
----------------------------------------------------------------------

realiseStore :: Topo.AnyStoreBuilder -> IO StoreEntry
realiseStore = \case
  Topo.AsKeyValueBuilder b -> do
    s <- sbKvBuild b
    pure
      StoreEntry
        { storeEntryName = sbKvName b
        , storeEntryAny = AnyKeyValueStore s
        , storeEntryClose = storeClose (kvsBase s)
        , storeEntryFlush = storeFlush (kvsBase s)
        }
  Topo.AsWindowBuilder b -> do
    s <- sbWBuild b
    pure
      StoreEntry
        { storeEntryName = sbWName b
        , storeEntryAny = AnyWindowStore s
        , storeEntryClose = storeClose (wsBase s)
        , storeEntryFlush = storeFlush (wsBase s)
        }
  Topo.AsSessionBuilder b -> do
    s <- sbSBuild b
    pure
      StoreEntry
        { storeEntryName = sbSName b
        , storeEntryAny = AnySessionStore s
        , storeEntryClose = storeClose (ssBase s)
        , storeEntryFlush = storeFlush (ssBase s)
        }
  Topo.AsRawBuilder b -> do
    s <- sbBuild b
    pure
      StoreEntry
        { storeEntryName = sbName b
        , storeEntryAny = AnyKeyValueStore (rawAsKeyValuePlaceholder s)
        , storeEntryClose = storeClose s
        , storeEntryFlush = storeFlush s
        }


rawAsKeyValuePlaceholder :: StateStore -> KeyValueStore () ()
rawAsKeyValuePlaceholder ss =
  KeyValueStore
    { kvsBase = ss
    , kvsGet = \_ -> opaque
    , kvsPut = \_ _ -> opaque
    , kvsPutIfAbsent = \_ _ -> opaque
    , kvsDelete = \_ -> opaque
    , kvsRange = \_ _ -> opaque
    , kvsAll = opaque
    , kvsApproxEntries = pure 0
    , kvsReverseRange = \_ _ -> opaque
    , kvsReverseAll = opaque
    }
  where
    opaque :: IO a
    opaque =
      error
        "Kafka.Streams.Internal.Engine: raw store has no typed key/value accessors"


----------------------------------------------------------------------
-- Processor instantiation
----------------------------------------------------------------------

instantiateProcessor :: Engine -> Topo.NodeName -> Topo.ProcessorSpec -> IO ()
instantiateProcessor engine nm spec =
  case Topo.processorSpecSupplier spec of
    Topo.AnyProcessor mkProc -> do
      proc_ <- mkProc
      let ctx = makeContext engine nm
      procInit proc_ ctx
      atomicModifyIORef' (engineForwarders engine) $ \m ->
        let !m' = Map.insert nm (forwardForProc proc_) m in (m', ())
      atomicModifyIORef' (engineProcessors engine) $ \m ->
        let !m' =
              Map.insert
                nm
                ProcessorRecord
                  { prClose = procClose proc_
                  , prName = procName proc_
                  }
                m
        in (m', ())


forwardForProc :: forall k v. Processor k v -> NodeForwarder
forwardForProc p = \rec ->
  -- Cross the erasure boundary: the DSL guarantees that incoming
  -- records have actual type @Record k v@ matching this processor.
  let !typed =
        Record
          { recordKey = unsafeUnerase <$> recordKey rec :: Maybe k
          , recordValue = unsafeUnerase (recordValue rec) :: v
          , recordTimestamp = recordTimestamp rec
          , recordHeaders = recordHeaders rec
          }
  in procProcess p typed
{-# INLINE forwardForProc #-}


----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

instantiateSink :: Engine -> Topo.NodeName -> Topo.SinkSpec -> IO ()
instantiateSink engine nm spec = do
  let !forwarder = sinkForwarder engine spec
  atomicModifyIORef' (engineForwarders engine) $ \m ->
    let !m' = Map.insert nm forwarder m in (m', ())


sinkForwarder :: Engine -> Topo.SinkSpec -> NodeForwarder
sinkForwarder engine spec =
  case (Topo.sinkKeySerde spec, Topo.sinkValueSerde spec) of
    (Topo.AnySerde ks, Topo.AnySerde vs) ->
      let !erasedK = unsafeCoerce ks :: Serde Erased
          !erasedV = unsafeCoerce vs :: Serde Erased
          !topic = Topo.sinkTopic spec
          -- Resolved once per sink, so a sink without a partitioner
          -- never touches the partition-count lookup.
          choosePartition :: Record Erased Erased -> IO (Maybe Int)
          choosePartition = case Topo.sinkPartitioner spec of
            Nothing -> \_ -> pure Nothing
            Just (Topo.AnyStreamPartitioner sp) ->
              sinkPartition
                engine
                (unTopicName topic)
                (unsafeCoerce sp :: StreamPartitioner Erased Erased)
      in \rec -> do
           let !keyB = serialize erasedK <$> recordKey rec
               !valB = serialize erasedV (recordValue rec)
               -- Stamp the serdes' header contributions (e.g. schema-identity
               -- headers) onto the outgoing record, mirroring the typed
               -- producer path ('Kafka.Client.Producer.publish'). Headers
               -- already on the record (carried from upstream, including any
               -- trace-context headers) are preserved and the serde headers
               -- appended; built-in serdes contribute 'mempty'.
               !keyHdrs = maybe mempty (serializeHeaders erasedK) (recordKey rec)
               !valHdrs = serializeHeaders erasedV (recordValue rec)
               !outHdrs = recordHeaders rec <> keyHdrs <> valHdrs
           !part <- choosePartition rec
           collectorSend
             (engineCollector engine)
             CollectedRecord
               { crTopic = topic
               , crKey = keyB
               , crValue = valB
               , crTimestamp = recordTimestamp rec
               , crHeaders = outHdrs
               , crPartition = part
               }


{- | Run a sink's 'StreamPartitioner' against the topic's current
partition count. Mirrors the JVM @RecordCollectorImpl.send@: no
partition metadata for the topic, or a partition outside
@[0, n)@, is a 'StreamsException' (routed through the
processing-exception handler like any other processor failure).
-}
sinkPartition
  :: Engine
  -> Text
  -> StreamPartitioner Erased Erased
  -> Record Erased Erased
  -> IO (Maybe Int)
sinkPartition engine topic sp rec = do
  mLookup <- readIORef (enginePartitionCount engine)
  case mLookup of
    Nothing -> pure Nothing
    Just lookupCount -> do
      eN <- lookupCount topic
      case eN of
        Left err ->
          sinkFail ("could not get partition information: " <> T.pack err)
        Right n
          | n <= 0 -> sinkFail "topic reports no partitions"
          | otherwise -> do
              mp <- runStreamPartitioner sp topic (recordKey rec) (recordValue rec) n
              case mp of
                Just p
                  | p < 0 || p >= n ->
                      sinkFail
                        ( "StreamPartitioner returned partition "
                            <> T.pack (show p)
                            <> " outside [0, "
                            <> T.pack (show n)
                            <> ")"
                        )
                _ -> pure mp
  where
    sinkFail msg =
      throwIO
        StreamsException
          { streamsErrorMessage = msg
          , streamsErrorContext = Just ("sink topic " <> topic)
          }


{- | Install the partition-count lookup sinks with a
'StreamPartitioner' consult (see 'enginePartitionCount').
-}
setPartitionCountLookup :: Engine -> (Text -> IO (Either String Int)) -> IO ()
setPartitionCountLookup engine f =
  writeIORef (enginePartitionCount engine) (Just f)


----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

instantiateSource :: Engine -> Topo.NodeName -> Topo.SourceSpec -> IO ()
instantiateSource engine nm spec = do
  -- Resolve children once so we don't pay for the lookup per record.
  fwds <- readIORef (engineForwarders engine)
  let kids = Topo.childrenOf (engineTopology engine) nm
  let childForwarders =
        [fw | k <- kids, Just fw <- [Map.lookup k fwds]]
  let handler = handleSource engine spec childForwarders
  atomicModifyIORef' (engineSources engine) $ \m ->
    let !m' =
          Map.insert
            nm
            SourceHandler
              { shSourceName = nm
              , shTopics = HashSet.fromList (Topo.sourceTopics spec)
              , shHandler = handler
              }
            m
    in (m', ())


handleSource
  :: Engine
  -> Topo.SourceSpec
  -> [NodeForwarder]
  -> SourceInput
  -> IO ()
handleSource engine spec children si = do
  let mErasedKey = decodeKeyErased (Topo.sourceKeySerde spec) (siKey si)
      eErasedVal = decodeValErased (Topo.sourceValueSerde spec) (siValue si)
  case (mErasedKey, eErasedVal) of
    (Right mk, Right v) -> do
      st <- readIORef (engineStreamTime engine)
      ts <-
        runErasedExtractor
          (Topo.sourceExtractor spec)
          mk
          v
          (siTimestamp si)
          st
      atomicModifyIORef' (engineStreamTime engine) $ \cur ->
        (advanceStreamTime ts cur, ())
      -- Riffle \xc2\xa75: if a watermark coordinator is wired and
      -- this source carries a 'WatermarkStrategy', report the
      -- record's extracted timestamp. The coordinator is
      -- responsible for running the strategy's generator and
      -- updating its per-source watermark.
      mWc <- readIORef (engineWatermarkCoord engine)
      case (mWc, Topo.sourceWatermarkStrategy spec) of
        (Just wc, Just _strat) -> do
          let sid =
                Watermark.SourceId
                  (Topo.unNodeName (Topo.sourceName spec))
          _ <- Watermark.reportRecord wc sid ts
          pure ()
        _ -> pure ()
      writeIORef
        (engineCurrentMd engine)
        ( Just
            RecordMetadata
              { rmTopic = siTopic si
              , rmPartition = fromIntegral (siPartition si)
              , rmOffset = siOffset si
              }
        )
      let rec =
            Record
              { recordKey = mk
              , recordValue = v
              , recordTimestamp = ts
              , recordHeaders = siHeaders si
              }
      writeIORef (engineCurrentHeaders engine) (Just (siHeaders si))
      Met.incCounter (engineMetrics engine) Met.processTotal
      -- Task-id-tagged variant for finer-grained inspection
      -- (mirrors the Java client's task-tagged metric names).
      Met.incCounter
        (engineMetrics engine)
        ( "stream-task-metrics:process-total{task="
            <> taskIdToText (engineTaskId engine)
            <> "}"
        )
      mapM_ (\fw -> fw rec) children
      writeIORef (engineCurrentMd engine) Nothing
      writeIORef (engineCurrentHeaders engine) Nothing
    _ -> do
      Met.incCounter (engineMetrics engine) Met.droppedRecordsTotal
      handleDeserError engine si mErasedKey eErasedVal


{- | Decode the source's key bytes via its 'AnySerde' into a fully
erased 'Maybe Erased'. The unsafeCoerce is sound because the
topology builder paired this 'Serde' with the same 'k' that
downstream processors will receive.
-}
decodeKeyErased
  :: Topo.AnySerde -> Maybe ByteString -> Either Text (Maybe Erased)
decodeKeyErased _ Nothing = Right Nothing
decodeKeyErased (Topo.AnySerde s) (Just kb) =
  case deserialize s kb of
    Left e -> Left e
    Right k -> Right (Just (erase k))


decodeValErased
  :: Topo.AnySerde -> ByteString -> Either Text Erased
decodeValErased (Topo.AnySerde s) bs =
  fmap erase (deserialize s bs)


{- | Run a type-erased 'TimestampExtractor'. Both the @Maybe k@ and
@v@ have already been erased; we coerce back to the extractor's
declared type at the boundary.
-}
runErasedExtractor
  :: Topo.AnyTimestampExtractor
  -> Maybe Erased
  -> Erased
  -> Timestamp
  -> StreamTime
  -> IO Timestamp
runErasedExtractor (Topo.AnyTimestampExtractor ex) mk v ts st =
  runTimestampExtractor
    ex
    (unsafeUnerase <$> mk)
    (unsafeUnerase v)
    ts
    st


handleDeserError
  :: forall a b
   . Engine
  -> SourceInput
  -> Either Text a
  -> Either Text b
  -> IO ()
handleDeserError engine si keyR valR = do
  let reason = case (keyR, valR) of
        (Left e, _) -> "key: " <> e
        (_, Left e) -> "value: " <> e
        _ -> "unknown"
      ex =
        DeserializationException
          { topic = unTopicName (siTopic si)
          , partition = fromIntegral (siPartition si)
          , offset = fromIntegral (siOffset si)
          , key = siKey si
          , value = siValue si
          , reason = reason
          }
  resp <- runDeserializationHandler (engineDeserHandler engine) ex
  case resp of
    DeserContinueProcessing -> pure ()
    DeserFailFast ->
      error $ "Kafka.Streams: deserialization failure: " <> T.unpack reason


----------------------------------------------------------------------
-- ProcessorContext
----------------------------------------------------------------------

makeContext :: Engine -> Topo.NodeName -> ProcessorContext
makeContext engine selfNm =
  ProcessorContext
    { ctxApplicationId = engineAppId engine
    , ctxTaskId = engineTaskId engine
    , ctxRecordMetadata = readIORef (engineCurrentMd engine)
    , ctxStreamTime = streamTimeOfEngine engine
    , ctxWallClockTime = wallClockTimeOfEngine engine
    , ctxForward = \rec -> do
        let !erased = eraseRecord rec
        fws <- childForwardersFor engine selfNm
        mapM_ ($ erased) fws
    , ctxForwardTo = \child rec -> do
        let !erased = eraseRecord rec
        fwds <- readIORef (engineForwarders engine)
        case Map.lookup child fwds of
          Just fw -> fw erased
          Nothing ->
            error $
              "Kafka.Streams: forward to unknown node "
                <> T.unpack (Topo.unNodeName child)
    , ctxSchedule = \intervalMs ptype pun -> do
        cancelRef <- newIORef False
        now <- case ptype of
          StreamTimePunctuation -> unTimestamp <$> streamTimeOfEngine engine
          WallClockTimePunctuation -> unTimestamp <$> wallClockTimeOfEngine engine
        let entry =
              PunctuatorEntry
                { peType = ptype
                , peIntervalMs = intervalMs
                , peNextFireMs = now + fromIntegral intervalMs
                , pePunctuator = pun
                , peCancelled = cancelRef
                , peOwner = selfNm
                }
        atomicModifyIORef' (enginePunctuators engine) $ \xs -> (entry : xs, ())
        pure Cancellable {cancel = writeIORef cancelRef True}
    , ctxGetStore = \sn -> do
        m <- readIORef (engineStores engine)
        pure (storeEntryAny <$> Map.lookup sn m)
    , ctxEmitToTopic = \emit ->
        collectorSend
          (engineCollector engine)
          CollectedRecord
            { crTopic =
                Kafka.Streams.Types.topicName
                  (Kafka.Streams.Processor.seTopic emit)
            , crKey = Kafka.Streams.Processor.seKey emit
            , crValue = Kafka.Streams.Processor.seValue emit
            , crTimestamp = Kafka.Streams.Processor.seTimestamp emit
            , crHeaders = Kafka.Streams.Processor.seHeaders emit
            , crPartition = Nothing
            }
    , ctxRecordHeaders = readIORef (engineCurrentHeaders engine)
    , ctxAddHeader = \h ->
        atomicModifyIORef' (engineCurrentHeaders engine) $ \mhs ->
          case mhs of
            Nothing -> (Just (Kafka.Streams.Types.headersFromList [h]), ())
            Just hs -> (Just (Kafka.Streams.Types.addHeader h hs), ())
    , ctxRequestCommit =
        writeIORef (engineCommitRequested engine) True
    , ctxRegisterPreCommitDrain = \action ->
        atomicModifyIORef'
          (engineAsyncDrains engine)
          (\xs -> (action : xs, ()))
    , ctxCoordinatedWatermark = do
        mCoord <- readIORef (engineWatermarkCoord engine)
        case mCoord of
          Nothing -> pure Nothing
          Just wc -> Just <$> Watermark.currentEffectiveWatermark wc
    }


eraseRecord :: Record k v -> Record Erased Erased
eraseRecord r =
  Record
    { recordKey = erase <$> recordKey r
    , recordValue = erase (recordValue r)
    , recordTimestamp = recordTimestamp r
    , recordHeaders = recordHeaders r
    }


childForwardersFor :: Engine -> Topo.NodeName -> IO [NodeForwarder]
childForwardersFor engine nm = do
  m <- readIORef (engineForwarders engine)
  let kids = Topo.childrenOf (engineTopology engine) nm
  pure [fw | k <- kids, Just fw <- [Map.lookup k m]]


----------------------------------------------------------------------
-- Driving
----------------------------------------------------------------------

feedSource
  :: Engine
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -> Int64
  -> IO ()
feedSource engine topic key value ts part off =
  feedSourceH engine topic key value emptyHeaders ts part off


{- | Like 'feedSource' but carries record headers from the source
into the topology. The driver's header-aware @pipeInput@ and the
internal repartition loop use this so headers survive ingestion
and through-topic hops.
-}
feedSourceH
  :: Engine
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Headers
  -> Timestamp
  -> Int
  -> Int64
  -> IO ()
feedSourceH engine topic key value hdrs ts part off = do
  shs <- readIORef (engineSources engine)
  let matching =
        filter (\sh -> HashSet.member topic (shTopics sh)) (Map.elems shs)
  case matching of
    [] -> pure ()
    _ -> do
      let si =
            SourceInput
              { siTopic = topic
              , siPartition = part
              , siOffset = off
              , siKey = key
              , siValue = value
              , siTimestamp = ts
              , siHeaders = hdrs
              }
      mapM_ (\sh -> shHandler sh si) matching
      triggerStreamTimePunctuators engine


advanceWallClock :: Engine -> Int64 -> IO ()
advanceWallClock engine deltaMs = do
  atomicModifyIORef' (engineWallClock engine) $ \(Timestamp t) ->
    (Timestamp (t + deltaMs), ())
  triggerWallClockPunctuators engine


{- | Sync the engine's wall clock to 'nowAsTimestamp' (real
'Data.Time.Clock' time). The runtime poll loop calls this
once per cycle so 'WallClockTimePunctuation' punctuators and
the watermark coordinator's idle-source detection
('reportPollCycle') see real time advance instead of staying
frozen at engine-construction time.

Mirrors JVM Kafka Streams' per-task @PunctuationType.WALL_CLOCK_TIME@
contract: a real-clock punctuator scheduled in a processor
fires when @System.currentTimeMillis@ advances past its next
target, /which only happens because the stream thread reads
the clock between record batches/. The mock 'Driver' path
continues to use 'advanceWallClock' for deterministic
timing — call sites that want pinned timing in tests should
avoid 'syncWallClockToReal' and use the delta-bump API.
-}
syncWallClockToReal :: Engine -> IO ()
syncWallClockToReal engine = do
  now <- nowAsTimestamp
  writeIORef (engineWallClock engine) now
  triggerWallClockPunctuators engine


{- | Riffle \xc2\xa75: wire a 'WatermarkCoordinator' to this engine. The
runtime calls this once at engine construction time after
building the per-app coordinator and before the first record
is fed; subsequent record ingestion in 'handleSource' reports
timestamps to the coordinator for every source whose
'sourceWatermarkStrategy' is 'Just'.

Idempotent: calling it again overwrites the previous
coordinator handle, useful for tests that rebind.
-}
attachWatermarkCoordinator
  :: Engine
  -> Watermark.WatermarkCoordinator
  -> IO ()
attachWatermarkCoordinator engine wc = do
  writeIORef (engineWatermarkCoord engine) (Just wc)
  -- Register every source that carries a strategy.
  forM_
    ( Map.toList
        ( Topo.topoSources
            (engineTopology engine)
        )
    )
    $ \(_nm, spec) ->
      case Topo.sourceWatermarkStrategy spec of
        Nothing -> pure ()
        Just strat -> do
          let sid =
                Watermark.SourceId
                  (Topo.unNodeName (Topo.sourceName spec))
              grp =
                fmap
                  Watermark.unAlignmentGroupId
                  (Watermark.wsAlignment strat)
          Watermark.registerSource wc sid strat grp


{- | Riffle §5 / KIP-353 idle-source heartbeat. The runtime calls
this once per poll cycle with the set of topic names whose
partitions produced at least one record. The engine then:

  * Propagates the engine's wall clock onto any attached
    'Kafka.Streams.Watermark.WatermarkCoordinator' so its
    'IdlenessConfig.idleTimeout' has fresh timing to compare
    against.
  * Calls 'markActive' for every source whose topics
    intersect the active set, and 'markIdle' for the others.
    The coordinator filters "idle enough" sources from the
    effective watermark based on each source's own
    'IdlenessConfig'.

Idempotent + cheap when no coordinator is attached (early
return). Safe to call on every poll, including empty ones --
that's the point: silence on a partition still has to be
reported so the coordinator can advance the effective
watermark of other sources past the silent one.
-}
reportPollCycle :: Engine -> HashSet.HashSet TopicName -> IO ()
reportPollCycle engine activeTopics = do
  mwc <- readIORef (engineWatermarkCoord engine)
  case mwc of
    Nothing -> pure ()
    Just wc -> do
      -- 1. Propagate the engine's wall clock to the coordinator.
      --    The runtime poll loop syncs the engine wall clock
      --    from 'nowAsTimestamp' once per cycle via
      --    'syncWallClockToReal', so in production this matches
      --    real time; the in-process test driver controls it
      --    via 'Kafka.Streams.Driver.advanceWallClockTime'.
      now <- readIORef (engineWallClock engine)
      Watermark.advanceWallClock wc now
      -- 2. Walk topology sources; classify by whether any of
      --    their topics fired this cycle.
      forM_ (Map.toList (Topo.topoSources (engineTopology engine))) $
        \(_nm, spec) ->
          case Topo.sourceWatermarkStrategy spec of
            Nothing -> pure ()
            Just _strat -> do
              let sid =
                    Watermark.SourceId
                      (Topo.unNodeName (Topo.sourceName spec))
                  active =
                    any
                      (`HashSet.member` activeTopics)
                      (Topo.sourceTopics spec)
              if active
                then Watermark.markActive wc sid
                else Watermark.markIdle wc sid


advanceStreamTimeTo :: Engine -> Timestamp -> IO ()
advanceStreamTimeTo engine ts = do
  atomicModifyIORef' (engineStreamTime engine) $ \cur ->
    (advanceStreamTime ts cur, ())
  triggerStreamTimePunctuators engine


triggerStreamTimePunctuators :: Engine -> IO ()
triggerStreamTimePunctuators engine = do
  StreamTime (Timestamp now) <- readIORef (engineStreamTime engine)
  fireDue engine StreamTimePunctuation now


triggerWallClockPunctuators :: Engine -> IO ()
triggerWallClockPunctuators engine = do
  Timestamp now <- readIORef (engineWallClock engine)
  fireDue engine WallClockTimePunctuation now


fireDue :: Engine -> PunctuationType -> Int64 -> IO ()
fireDue engine pt now = do
  pes <- readIORef (enginePunctuators engine)
  newPes <- forM pes $ \pe ->
    if peType pe /= pt
      then pure pe
      else do
        wasCancelled <- readIORef (peCancelled pe)
        if wasCancelled
          then pure pe
          else fireOnce pe
  writeIORef (enginePunctuators engine) newPes
  where
    -- Fire at most once per call (matches Java behaviour: the
    -- punctuator catches up by one tick on each record). Deferred
    -- catch-up across many ticks is acceptable for the tests we
    -- want to write and avoids unbounded firing on the first
    -- record (when stream time advances from minBound).
    fireOnce pe
      | peNextFireMs pe <= now = do
          runPunctuator (pePunctuator pe) (Timestamp now)
          Met.incCounter (engineMetrics engine) Met.punctuateTotal
          let !next = peNextFireMs pe + fromIntegral (peIntervalMs pe)
              -- If the next-fire is still behind now (because we
              -- jumped a huge gap in stream time), snap it to
              -- @now + interval@ so we don't emit a flood of
              -- back-fills next time.
              !target =
                if next < now
                  then now + fromIntegral (peIntervalMs pe)
                  else next
          pure pe {peNextFireMs = target}
      | otherwise = pure pe


----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

{- | Render a 'TaskId' as the canonical @"<topo>_<part>"@ form
used in metric names.
-}
taskIdToText :: TaskId -> Text
taskIdToText (TaskId topo part) =
  T.pack (show topo) <> "_" <> T.pack (show part)


{- | Atomically read and reset the "commit requested by processor"
flag. The runtime should call this at the top of each commit
window; if the result is 'True' the runtime SHOULD commit
regardless of whether the 'commit.interval.ms' boundary was hit.
-}
takeCommitRequested :: Engine -> IO Bool
takeCommitRequested engine =
  atomicModifyIORef' (engineCommitRequested engine) (\b -> (False, b))


commitEngine :: Engine -> IO ()
commitEngine engine = do
  -- Riffle: pre-commit drain runs FIRST. Any in-flight async work
  -- that completed after the last drain — or that's still in
  -- flight — is forwarded downstream now, so the records land in
  -- the collector and become part of the same commit transaction
  -- as the source offsets they were produced from.
  drainPreCommit engine
  m <- readIORef (engineStores engine)
  forM_ (Map.elems m) $ \se -> do
    eflush <- try (storeEntryFlush se) :: IO (Either SomeException ())
    case eflush of
      Left e -> putStrLn ("[streams] store flush failed: " <> show e)
      _ -> pure ()
  collectorFlush (engineCollector engine)
  Met.incCounter (engineMetrics engine) Met.commitTotal


{- | Run every registered pre-commit drain in /registration order/.

Drains run synchronously on the stream thread because the
forwarding they perform (via 'ProcessorContext.ctxForward') is
not thread-safe. Each drain blocks until its background workers
have caught up, then forwards any pending output downstream.

Called automatically as the first step of 'commitEngine'.
Exposed publicly for callers (and tests) that need an explicit
drain boundary outside of a full commit cycle — e.g. the
'Kafka.Streams.Runtime.EOS.EOSCoordinator' production runtime
can invoke this directly between @beginTxn@ and @commitOffsets@.
-}
drainPreCommit :: Engine -> IO ()
drainPreCommit engine = do
  drains <- readIORef (engineAsyncDrains engine)
  -- Drains were prepended at registration time; reverse so they
  -- run in registration order.
  mapM_ runOne (reverse drains)
  where
    runOne action = do
      r <- try action :: IO (Either SomeException ())
      case r of
        Right () -> pure ()
        Left e -> do
          putStrLn ("[streams] pre-commit drain failed: " <> show e)
          -- Re-throw so 'commitEngine' surfaces the failure to
          -- the runtime; EOS-mode runtimes will abort the
          -- transaction rather than commit an inconsistent
          -- state.
          throwIO e


closeEngine :: Engine -> IO ()
closeEngine engine = do
  ps <- readIORef (engineProcessors engine)
  forM_ (Map.toList ps) $ \(_nm, pr) -> do
    eclose <- try (prClose pr) :: IO (Either SomeException ())
    case eclose of
      Left e ->
        putStrLn $
          "[streams] processor close failed ("
            <> T.unpack (unProcessorName (prName pr))
            <> "): "
            <> show e
      _ -> pure ()
  ses <- readIORef (engineStores engine)
  forM_ (Map.elems ses) $ \se -> do
    eclose <- try (storeEntryClose se) :: IO (Either SomeException ())
    case eclose of
      Left e ->
        putStrLn $
          "[streams] store close failed ("
            <> T.unpack (unStoreName (storeEntryName se))
            <> "): "
            <> show e
      _ -> pure ()
  collectorClose (engineCollector engine)


storeByName :: Engine -> StoreName -> IO (Maybe StoreEntry)
storeByName engine sn = Map.lookup sn <$> readIORef (engineStores engine)


streamTimeOfEngine :: Engine -> IO Timestamp
streamTimeOfEngine engine =
  unStreamTime <$> readIORef (engineStreamTime engine)


wallClockTimeOfEngine :: Engine -> IO Timestamp
wallClockTimeOfEngine engine = readIORef (engineWallClock engine)


nowAsTimestamp :: IO Timestamp
nowAsTimestamp = utcTimeToTimestamp <$> Clock.getCurrentTime
