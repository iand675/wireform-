{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Replay
Description : Offline replay & backfill over the topology driver

Operational utilities for /reprocessing/ a recorded stream of input
records through a topology without a broker, built on the
'Kafka.Streams.Driver.TopologyTestDriver'. The two headline use
cases:

  * __Replay / reprocessing__ — re-run historical records through a
    (possibly fixed or changed) topology and capture the outputs and
    resulting state. Pair with a captured log
    ('encodeReplayLog' \/ 'decodeReplayLog') to snapshot production
    traffic and replay it offline.

  * __Backfill__ — populate a topology's state stores from a window
    of historical data before going live, optionally /time-shifting/
    the records so windowed aggregations land in the intended
    buckets. 'withReplayDriver' hands back the live driver so the
    freshly-built state can be inspected or snapshotted
    ('dumpKeyValueStore').

A 'ReplayPlan' selects an event-time window @[from, to)@ and applies
an optional time shift; records are fed in their original order
(faithful reprocessing), and stream time is advanced to the last
timestamp at the end so windowed / suppressed emits flush.

This is the offline (driver) layer. It is distinct from
/changelog replay/ ('Kafka.Streams.Runtime.Standby') and
/snapshot restore/ ('Kafka.Streams.State.KeyValue.Snapshot'), which
rebuild a single store's bytes rather than reprocessing input
through the whole topology.
-}
module Kafka.Streams.Replay (
  -- * Records
  ReplayRecord (..),
  replayRecord,
  replayRecordBytes,
  replayWithHeaders,
  replayWithOffset,

  -- * Plan
  ReplayPlan (..),
  defaultReplayPlan,
  selectForReplay,

  -- * Result
  ReplayResult (..),
  renderReplayResult,

  -- * Running
  runReplay,
  runReplayWith,
  withReplayDriver,

  -- * Rate control
  ReplayPacer,
  noPacing,
  gapPacer,
  runReplayPaced,

  -- * State inspection (backfill)
  dumpKeyValueStore,

  -- * Capture format (newline-delimited JSON)
  encodeReplayLog,
  decodeReplayLog,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kafka.Streams.Driver (
  CollectedRecord,
  TopologyTestDriver,
  advanceDriverStreamTime,
  closeDriver,
  getKeyValueStore,
  newDriverWith,
  pipeInputH,
  readOutputAll,
 )
import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Serde (Serde, serialize, serializeHeaders)
import Kafka.Streams.State.Store (
  StoreName,
  kvIteratorToList,
  kvsAll,
 )
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (
  Header (..),
  Headers,
  TopicName,
  emptyHeaders,
  headersFromList,
  headersToList,
  topicName,
  unTopicName,
 )
import Wireform.Base64 (decodeBase64, encodeBase64)


----------------------------------------------------------------------
-- Records
----------------------------------------------------------------------

{- | A single record to feed back through a topology. Mirrors the
arguments of 'Kafka.Streams.Driver.pipeInputH', including record
headers, which the driver now carries into the topology.
-}
data ReplayRecord = ReplayRecord
  { rrTopic :: !TopicName
  , rrKey :: !(Maybe ByteString)
  , rrValue :: !ByteString
  , rrTimestamp :: !Timestamp
  , rrPartition :: !Int
  , rrHeaders :: !Headers
  , rrOffset :: !(Maybe Int64)
  {- ^ The record's /original/ source offset, if captured. Enables
  offset-window selection ('replayFromOffset' \/ 'replayToOffset').
  -}
  }
  deriving stock (Eq, Show)


{- | Build a 'ReplayRecord' from typed key / value via serdes
(partition 0). The record's headers are seeded from the serdes'
'serializeHeaders' channels — @keySerde@ then @valueSerde@,
matching the typed producer path
('Kafka.Client.Producer.publish') — so schema-identity serdes
stamp their headers automatically. Append more (e.g. trace
context) with 'replayWithHeaders'.
-}
replayRecord
  :: Serde k
  -> Serde v
  -> TopicName
  -> Maybe k
  -> v
  -> Timestamp
  -> ReplayRecord
replayRecord ks vs topic mk v ts =
  ReplayRecord
    { rrTopic = topic
    , rrKey = fmap (serialize ks) mk
    , rrValue = serialize vs v
    , rrTimestamp = ts
    , rrPartition = 0
    , rrHeaders = keyHdrs <> serializeHeaders vs v
    , rrOffset = Nothing
    }
  where
    keyHdrs = maybe mempty (serializeHeaders ks) mk


{- | Build a 'ReplayRecord' straight from key / value bytes
(no headers, no offset). Attach with 'withHeaders' / 'withOffset'.
-}
replayRecordBytes
  :: TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -> ReplayRecord
replayRecordBytes topic key value ts part =
  ReplayRecord topic key value ts part emptyHeaders Nothing


{- | Append headers to a record (preserving any the serdes already
contributed via 'replayRecord'). Use for trace-context / OTel
headers that ride alongside schema-identity headers.
-}
replayWithHeaders :: Headers -> ReplayRecord -> ReplayRecord
replayWithHeaders hs r = r {rrHeaders = rrHeaders r <> hs}


{- | Attach an original source offset to a record (for offset-window
selection).
-}
replayWithOffset :: Int64 -> ReplayRecord -> ReplayRecord
replayWithOffset o r = r {rrOffset = Just o}


----------------------------------------------------------------------
-- Plan
----------------------------------------------------------------------

-- | How to select and rewrite records before replaying them.
data ReplayPlan = ReplayPlan
  { replayFrom :: !(Maybe Timestamp)
  {- ^ Inclusive lower bound on the /original/ event time. 'Nothing'
  means no lower bound.
  -}
  , replayTo :: !(Maybe Timestamp)
  {- ^ Exclusive upper bound on the /original/ event time. 'Nothing'
  means no upper bound.
  -}
  , replayFromOffset :: !(Maybe Int64)
  {- ^ Inclusive lower bound on the original source offset
  ('rrOffset'). Records without a captured offset are excluded
  whenever any offset bound is set. 'Nothing' means no bound.
  -}
  , replayToOffset :: !(Maybe Int64)
  -- ^ Exclusive upper bound on the original source offset.
  , replayTimeShiftMs :: !Int64
  {- ^ Milliseconds added to every selected record's timestamp
  /after/ window selection. Use to backfill a historical window
  into a different target window. Default: 0.
  -}
  , replayAdvanceStreamTimeAtEnd :: !Bool
  {- ^ After feeding all records, advance stream time to the last
  (shifted) timestamp so windowed / suppressed operators flush
  their pending emits. Default: 'True'.
  -}
  }
  deriving stock (Eq, Show)


defaultReplayPlan :: ReplayPlan
defaultReplayPlan =
  ReplayPlan
    { replayFrom = Nothing
    , replayTo = Nothing
    , replayFromOffset = Nothing
    , replayToOffset = Nothing
    , replayTimeShiftMs = 0
    , replayAdvanceStreamTimeAtEnd = True
    }


{- | Apply a plan to a record list: keep records whose original
timestamp falls in @[from, to)@, then add the time shift. Input
order is preserved (faithful reprocessing).
-}
selectForReplay :: ReplayPlan -> [ReplayRecord] -> [ReplayRecord]
selectForReplay plan = map shift . filter inWindow
  where
    inWindow r =
      aboveFrom (rrTimestamp r) && belowTo (rrTimestamp r) && offsetOk r
    aboveFrom ts = maybe True (ts >=) (replayFrom plan)
    belowTo ts = maybe True (ts <) (replayTo plan)
    offsetOk r =
      case (replayFromOffset plan, replayToOffset plan) of
        (Nothing, Nothing) -> True
        _ -> case rrOffset r of
          Just o ->
            maybe True (o >=) (replayFromOffset plan)
              && maybe True (o <) (replayToOffset plan)
          Nothing -> False
    shift r
      | replayTimeShiftMs plan == 0 = r
      | otherwise = r {rrTimestamp = bump (rrTimestamp r)}
    bump (Timestamp t) = Timestamp (t + replayTimeShiftMs plan)


----------------------------------------------------------------------
-- Result
----------------------------------------------------------------------

-- | Outcome of a replay run.
data ReplayResult = ReplayResult
  { replayConsumed :: !Int
  -- ^ Records actually fed through the topology.
  , replaySkipped :: !Int
  -- ^ Records dropped by the plan's window.
  , replayOutputs :: ![(TopicName, [CollectedRecord])]
  -- ^ Output records drained per sink topic.
  , replayMinTimestamp :: !(Maybe Timestamp)
  , replayMaxTimestamp :: !(Maybe Timestamp)
  }


-- | A short human-readable summary line.
renderReplayResult :: ReplayResult -> Text
renderReplayResult r =
  T.intercalate
    " "
    [ "consumed=" <> T.pack (show (replayConsumed r))
    , "skipped=" <> T.pack (show (replaySkipped r))
    , "outputTopics=" <> T.pack (show (length (replayOutputs r)))
    , "outputRecords=" <> T.pack (show outRecs)
    , "ts=" <> tsRange
    ]
  where
    outRecs = List.foldl' (\acc (_, rs) -> acc + length rs) 0 (replayOutputs r)
    tsRange = case (replayMinTimestamp r, replayMaxTimestamp r) of
      (Just (Timestamp lo), Just (Timestamp hi)) ->
        "[" <> T.pack (show lo) <> "," <> T.pack (show hi) <> "]"
      _ -> "[]"


----------------------------------------------------------------------
-- Running
----------------------------------------------------------------------

{- | Replay a record log through a fresh driver for the given
validated topology, returning the result. The driver is created and
closed internally.
-}
runReplay
  :: Topo.TopologyValid
  -> Text
  -> ReplayPlan
  -> [ReplayRecord]
  -> IO ReplayResult
runReplay topo appId plan records =
  bracket
    (newDriverWith topo appId logAndContinue)
    closeDriver
    (\d -> runReplayWith d plan records)


{- | Replay a record log through an /existing/ driver. Lets callers
chain multiple replays against the same accumulating state (e.g.
incremental backfill) before inspecting it.
-}
runReplayWith
  :: TopologyTestDriver -> ReplayPlan -> [ReplayRecord] -> IO ReplayResult
runReplayWith d plan = runReplayCore d plan noPacing


-- | Core runner with a 'ReplayPacer' between consecutive records.
runReplayCore
  :: TopologyTestDriver
  -> ReplayPlan
  -> ReplayPacer
  -> [ReplayRecord]
  -> IO ReplayResult
runReplayCore d plan pacer records = do
  let selected = selectForReplay plan records
  feedPaced Nothing selected
  case (replayAdvanceStreamTimeAtEnd plan, maxTs selected) of
    (True, Just mx) -> advanceDriverStreamTime d mx
    _ -> pure ()
  outs <- readOutputAll d
  pure
    ReplayResult
      { replayConsumed = length selected
      , replaySkipped = length records - length selected
      , replayOutputs = outs
      , replayMinTimestamp = minTs selected
      , replayMaxTimestamp = maxTs selected
      }
  where
    feedPaced _ [] = pure ()
    feedPaced mPrev (r : rest) = do
      case mPrev of
        Just prev -> pacer (gapMs prev r)
        Nothing -> pure ()
      feed r
      feedPaced (Just r) rest
    gapMs prev cur =
      max 0 (unTimestamp (rrTimestamp cur) - unTimestamp (rrTimestamp prev))
    feed r =
      pipeInputH
        d
        (rrTopic r)
        (rrKey r)
        (Just (rrValue r))
        (rrHeaders r)
        (rrTimestamp r)
        (rrPartition r)


{- | Replay into a fresh driver, then hand the live driver and the
result to a continuation so the freshly-built state can be
inspected or snapshotted. The driver is closed afterwards (even on
exception). This is the backfill entry point.
-}
withReplayDriver
  :: Topo.TopologyValid
  -> Text
  -> ReplayPlan
  -> [ReplayRecord]
  -> (TopologyTestDriver -> ReplayResult -> IO a)
  -> IO a
withReplayDriver topo appId plan records k =
  bracket
    (newDriverWith topo appId logAndContinue)
    closeDriver
    ( \d -> do
        res <- runReplayWith d plan records
        k d res
    )


----------------------------------------------------------------------
-- Rate control
----------------------------------------------------------------------

{- | A pacer is invoked between consecutive records with the
millisecond gap between their (shifted) timestamps. It decides
whether/how long to wait — enabling rate-controlled "live" replay.
-}
type ReplayPacer = Int64 -> IO ()


-- | As-fast-as-possible: never waits. The default for offline replay.
noPacing :: ReplayPacer
noPacing _ = pure ()


{- | Sleep for @gap / speedup@ milliseconds, reproducing the original
inter-record timing scaled by a speed factor (@1.0@ = real time,
@10.0@ = 10x faster). A non-positive @speedup@ disables waiting.
-}
gapPacer :: Double -> ReplayPacer
gapPacer speedup gap
  | speedup <= 0 = pure ()
  | otherwise =
      let micros = round (fromIntegral gap * 1000 / speedup) :: Int
      in if micros > 0 then threadDelay micros else pure ()


{- | Like 'runReplay' but paces records with the supplied 'ReplayPacer'
(e.g. @'gapPacer' 10@ for 10x-speed live replay).
-}
runReplayPaced
  :: Topo.TopologyValid
  -> Text
  -> ReplayPlan
  -> ReplayPacer
  -> [ReplayRecord]
  -> IO ReplayResult
runReplayPaced topo appId plan pacer records =
  bracket
    (newDriverWith topo appId logAndContinue)
    closeDriver
    (\d -> runReplayCore d plan pacer records)


minTs :: [ReplayRecord] -> Maybe Timestamp
minTs [] = Nothing
minTs rs = Just (List.foldl1' min (map rrTimestamp rs))


maxTs :: [ReplayRecord] -> Maybe Timestamp
maxTs [] = Nothing
maxTs rs = Just (List.foldl1' max (map rrTimestamp rs))


----------------------------------------------------------------------
-- State inspection (backfill)
----------------------------------------------------------------------

{- | Read every entry of a named key-value store as a list. The
key / value types are the caller's responsibility (same contract as
'getKeyValueStore'). Returns @[]@ if the store is absent.
-}
dumpKeyValueStore
  :: TopologyTestDriver -> StoreName -> IO [(k, v)]
dumpKeyValueStore d sn = do
  m <- getKeyValueStore d sn
  case m of
    Nothing -> pure []
    Just kvs -> kvsAll kvs >>= kvIteratorToList


----------------------------------------------------------------------
-- Capture format (newline-delimited JSON)
----------------------------------------------------------------------

{- | Encode a record log as newline-delimited JSON. Key / value bytes
are base64-encoded. Suitable for capturing production traffic to a
file and replaying it offline.
-}
encodeReplayLog :: [ReplayRecord] -> BL.ByteString
encodeReplayLog =
  BLC.intercalate "\n" . map (A.encode . toJson)


{- | Decode a newline-delimited JSON record log. Blank lines are
ignored. Returns the first parse error, if any.
-}
decodeReplayLog :: BL.ByteString -> Either String [ReplayRecord]
decodeReplayLog =
  traverse decodeLine . filter (not . BL.null) . BLC.lines
  where
    decodeLine ln = do
      v <- A.eitherDecode ln
      A.parseEither parseJson v


toJson :: ReplayRecord -> A.Value
toJson r =
  A.object
    [ "topic" .= unTopicName (rrTopic r)
    , "key" .= fmap b64 (rrKey r)
    , "value" .= b64 (rrValue r)
    , "timestamp" .= unTimestamp (rrTimestamp r)
    , "partition" .= rrPartition r
    , "offset" .= rrOffset r
    , "headers" .= map headerJson (headersToList (rrHeaders r))
    ]
  where
    b64 = TE.decodeUtf8 . encodeBase64
    headerJson h =
      A.object
        [ "key" .= headerKey h
        , "value" .= b64 (headerValue h)
        ]


parseJson :: A.Value -> A.Parser ReplayRecord
parseJson = A.withObject "ReplayRecord" $ \o -> do
  topic <- o .: "topic"
  mKeyT <- o .:? "key"
  valT <- o .: "value"
  ts <- o .: "timestamp"
  part <- o .: "partition"
  off <- o .:? "offset" .!= Nothing
  hdrsV <- o .:? "headers" .!= []
  key <- traverse decodeField mKeyT
  val <- decodeField valT
  hdrs <- traverse parseHeader hdrsV
  pure
    ReplayRecord
      { rrTopic = topicName topic
      , rrKey = key
      , rrValue = val
      , rrTimestamp = Timestamp ts
      , rrPartition = part
      , rrHeaders = headersFromList hdrs
      , rrOffset = off
      }
  where
    decodeField t =
      case decodeBase64 (TE.encodeUtf8 t) of
        Just bs -> pure bs
        Nothing -> fail "ReplayRecord: invalid base64 field"
    parseHeader = A.withObject "Header" $ \ho -> do
      k <- ho .: "key"
      v <- ho .: "value"
      bs <- decodeField v
      pure (Header k bs)
