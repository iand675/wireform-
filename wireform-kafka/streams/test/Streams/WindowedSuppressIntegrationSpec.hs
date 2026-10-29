{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Streams.WindowedSuppressIntegrationSpec
Description : Regression tests for the windowed-suppress pipeline

Pins down three previously-broken contracts on the
@reduceWindowed >>> streamFromWindowed (>>> suppressWindowed)@
chain:

  1. /Per-window dedup/. 'Kafka.Streams.Suppress.windowedAsStreamProc'
     used to re-forward every entry in the window store on
     every input. With the fix in place, downstream sees one
     record per actual @(key, window-start)@ /value change/
     rather than one record per input × per window.
  2. /Flush on stream-time advance/. 'suppressWindowedProc'
     now registers a stream-time punctuator; advancing the
     driver clock past @windowEnd + grace@ flushes buffered
     windows even when no further record arrives on the
     affected key.
  3. /No mutual recursion for @emitOnWindowClose@/. The
     'streamFromWindowedHandle' / 'suppressWindowedHandle'
     pair used to delegate to each other for handles tagged
     @OnWindowClose@, which compiled and then looped forever.
     The fix routes both through 'doStreamFromWindowedHandle';
     this test makes sure the path is exercised end-to-end.
-}
module Streams.WindowedSuppressIntegrationSpec (tests) where

import Control.Arrow ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams.Grouped qualified as Grouped
import Kafka.Streams.Imperative
import Kafka.Streams.Materialized qualified as Mat
import Kafka.Streams.Serde.Windowed (windowedSerde)
import Kafka.Streams.Suppress qualified as Suppress
import Kafka.Streams.Time qualified as Time
import Kafka.Streams.TimeWindowedKStream qualified as TWKS
import Kafka.Streams.Topology.Free qualified as F
import Kafka.Streams.Window qualified as Win
import Test.Syd


tests :: Spec
tests =
  describe "Windowed suppress pipeline (regression)" $
    sequence_
      [ stream_from_windowed_dedupes_per_window
      , stream_from_windowed_emits_old_window_only_on_value_change
      , suppress_flushes_on_stream_time_advance_alone
      , emit_on_window_close_dispatch_does_not_recurse
      , suppress_until_time_limit_flushes_on_stream_time_advance
      , suppress_windowed_shed_flushes_on_stream_time_advance
      ]


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


-- | Simple value-decoder shared by every test.
readD :: BSC.ByteString -> Double
readD bs = case deserialize doubleSerde bs of
  Right d -> d
  Left e -> error ("decode failure: " <> T.unpack e)


{- | Topology under test:

  source(in, Text, Double)
    >>> groupByKey
    >>> windowedByTime tumbling(5s)
    >>> reduceWindowed max
    >>> streamFromWindowed
    >>> sink(out, WindowedKey Text, Double)

Matches the @Temperature@ example, with grace = 0 and the
'emitOnWindowUpdate' default. The optional 'F.withEmitStrategy'
in 'closeStrategy' flips the windowed handle into
'emitOnWindowClose' so the OnWindowClose code path is exercised.
-}
data EmitMode = EmitOnUpdate | EmitOnClose
  deriving (Eq)


reduceMaxTopology :: EmitMode -> F.Topology Void ()
reduceMaxTopology emitMode =
  reducePipelineWithStrategy
    >>> F.streamFromWindowed
    >>> F.sink "out"
  where
    maxMat :: Materialized Text Double
    maxMat =
      Mat.withValueSerde doubleSerde $
        Mat.withKeySerde textSerde $
          Mat.materialized
    reducePipeline
      :: F.Topology Void (TWKS.WindowedTableHandle Text Double)
    reducePipeline =
      F.source @Text @Double "in"
        >>> F.groupByKey
        >>> F.windowedByTime (Win.tumblingWindows (Time.seconds 5))
        >>> F.reduceWindowed max maxMat
    reducePipelineWithStrategy
      :: F.Topology Void (TWKS.WindowedTableHandle Text Double)
    reducePipelineWithStrategy = case emitMode of
      EmitOnUpdate -> reducePipeline
      EmitOnClose ->
        F.withEmitStrategy TWKS.emitOnWindowClose reducePipeline


-- | Decode a windowed sink record into @(inner-key, ts, value)@.
decodeWindowed
  :: CollectedRecord
  -> Either Text (Maybe Text, Int, Double)
decodeWindowed r = do
  k <- case crKey r of
    Just bs -> case deserialize (windowedSerde textSerde) bs of
      Right (WindowedKey k (Timestamp ts)) ->
        Right (Just k, fromIntegral ts)
      Left e -> Left e
    Nothing -> Right (Nothing, 0)
  let v = readD (crValueBytes r)
      (mk, ts) = k
  pure (mk, ts, v)


----------------------------------------------------------------------
-- 1. Per-window dedup
----------------------------------------------------------------------

{- | The aggregator forwards a record /every time/ an input
falls into a window. With the @windowedAsStreamProc@ dedup,
downstream only sees a record when the aggregate /value/
actually changes for that (key, window-start). So a max-reduce
that sees @20, 5, 20, 7@ in the same window emits two records,
not four.
-}
stream_from_windowed_dedupes_per_window :: Spec
stream_from_windowed_dedupes_per_window =
  it "windowedAsStreamProc: one emit per (key, window-start) value change" $ do
    (_h, topo) <- F.compile (reduceMaxTopology EmitOnUpdate)
    driver <- newDriver topo "wd-dedupe"

    -- One key, one window [0..5000). The max walks 20 -> 20 -> 20 -> 20:
    -- only the first record bumps the aggregate; the next three
    -- leave it unchanged and must NOT re-emit downstream.
    mapM_
      ( \(ts, v) ->
          pipeInput
            driver
            (topicName "in")
            (Just (bytes "k"))
            (serialize doubleSerde v)
            (Timestamp ts)
            0
      )
      [ (100 :: Int64, 20.0 :: Double) -- max = 20.0
      , (1500, 5.0) -- max stays 20.0
      , (2500, 20.0) -- max stays 20.0
      , (4500, 7.0) -- max stays 20.0
      ]
    out <- readOutput driver (topicName "out")
    map (fmap (\(_, _, v) -> v) . decodeWindowed) out
      `shouldBe` [Right 20.0]
    closeDriver driver


stream_from_windowed_emits_old_window_only_on_value_change :: Spec
stream_from_windowed_emits_old_window_only_on_value_change =
  it "old windows are not re-emitted just because a new window updates" $ do
    (_h, topo) <- F.compile (reduceMaxTopology EmitOnUpdate)
    driver <- newDriver topo "wd-dedupe-2"

    -- Same key, three windows.
    --   window 0: max goes 10 -> 20  (2 emits)
    --   window 1: max goes 5  -> 9   (2 emits)
    --   window 2: max stays 1        (1 emit)
    -- Total downstream: 5 emits. Without the dedup fix the
    -- adapter re-forwards ALL prior entries on each input and
    -- the suppress-free path would amplify this to N×W records.
    let inputs :: [(Int64, Double)]
        inputs =
          [ (100, 10.0) -- W0 = 10
          , (1500, 20.0) -- W0 = 20
          , (2500, 5.0) -- W0 stays 20 (no emit)
          , (5500, 5.0) -- W1 = 5
          , (6500, 9.0) -- W1 = 9
          , (9999, 3.0) -- W1 stays 9 (no emit)
          , (10500, 1.0) -- W2 = 1
          ]
    mapM_
      ( \(ts, v) ->
          pipeInput
            driver
            (topicName "in")
            (Just (bytes "k"))
            (serialize doubleSerde v)
            (Timestamp ts)
            0
      )
      inputs

    out <- readOutput driver (topicName "out")
    let decoded =
          [ (ts, v)
          | r <- out
          , Right (_, ts, v) <- [decodeWindowed r]
          ]
    -- Each (window-start, value) appears at most once.
    Set.size (Set.fromList decoded) `shouldBe` length decoded
    -- And we see exactly the expected (window, value) updates.
    Set.fromList decoded
      `shouldBe` Set.fromList
        [ (0, 10.0)
        , (0, 20.0)
        , (5000, 5.0)
        , (5000, 9.0)
        , (10000, 1.0)
        ]
    closeDriver driver


----------------------------------------------------------------------
-- 2. Suppress flushes on stream-time advance alone
----------------------------------------------------------------------

{- | Topology with a downstream @suppress(untilWindowCloses)@:

  source -> groupByKey -> windowedByTime tumbling(5s)
  -> reduceWindowed max -> streamFromWindowed
  -> suppressWindowed grace=0 windowSize=5s
  -> sink
-}
suppressedReduceMaxTopology :: F.Topology Void ()
suppressedReduceMaxTopology =
  F.source @Text @Double "in"
    >>> F.groupByKey
    >>> F.windowedByTime (Win.tumblingWindows (Time.seconds 5))
    >>> F.reduceWindowed max maxMat
    >>> F.streamFromWindowed
    >>> F.suppressWindowed (Time.millis 0) (Time.durationMillis (Time.seconds 5))
    >>> F.sink "sup-out"
  where
    maxMat :: Materialized Text Double
    maxMat =
      Mat.withValueSerde doubleSerde $
        Mat.withKeySerde textSerde $
          Mat.materialized


suppress_flushes_on_stream_time_advance_alone :: Spec
suppress_flushes_on_stream_time_advance_alone =
  it "suppressWindowedProc: stream-time advance flushes due windows without a new record" $ do
    (_h, topo) <- F.compile suppressedReduceMaxTopology
    driver <- newDriver topo "wd-sup-flush"

    -- Buffer two windows worth of data on the same key; nothing
    -- can flush yet because stream time is < windowEnd.
    mapM_
      ( \(ts, v) ->
          pipeInput
            driver
            (topicName "in")
            (Just (bytes "k"))
            (serialize doubleSerde v)
            (Timestamp ts)
            0
      )
      [ (100 :: Int64, 1.0 :: Double)
      , (4500, 2.0)
      , (5500, 7.0)
      , (9000, 9.0)
      ]
    pre <- readOutput driver (topicName "sup-out")
    -- Nothing flushed: stream time is at 9000, window 0 closes at
    -- 5000 (would have flushed when ts=5500 arrived).
    let preDecoded =
          [ (ts, v)
          | r <- pre
          , Right (_, ts, v) <- [decodeWindowed r]
          ]
    preDecoded `shouldBe` [(0, 2.0)]

    -- Advance the driver clock past window 1's close + grace
    -- (10000 + 0) with NO further input. Before the punctuator
    -- fix, this was a no-op and window 1 sat in the suppress
    -- buffer forever.
    advanceDriverStreamTime driver (Timestamp 10_001)
    post <- readOutput driver (topicName "sup-out")
    let postDecoded =
          [ (ts, v)
          | r <- post
          , Right (_, ts, v) <- [decodeWindowed r]
          ]
    postDecoded `shouldBe` [(5000, 9.0)]
    closeDriver driver


----------------------------------------------------------------------
-- 3. emitOnWindowClose dispatch does not loop
----------------------------------------------------------------------

emit_on_window_close_dispatch_does_not_recurse :: Spec
emit_on_window_close_dispatch_does_not_recurse =
  it "wthEmit = OnWindowClose -> streamFromWindowed compiles and emits at close" $ do
    -- The mere fact that 'F.compile' returns without diverging
    -- guarantees the recursion bug is gone. We then drive a few
    -- records + advance the clock to confirm the operator still
    -- behaves like "emit one final value per window" via the
    -- internal suppressWindowed it now wraps with.
    (_h, topo) <- F.compile (reduceMaxTopology EmitOnClose)
    driver <- newDriver topo "wd-close-dispatch"

    mapM_
      ( \(ts, v) ->
          pipeInput
            driver
            (topicName "in")
            (Just (bytes "k"))
            (serialize doubleSerde v)
            (Timestamp ts)
            0
      )
      [ (100 :: Int64, 1.0 :: Double)
      , (4500, 9.0)
      , (5500, 7.0)
      ]
    advanceDriverStreamTime driver (Timestamp 10_001)

    out <- readOutput driver (topicName "out")
    let decoded =
          [ (ts, v)
          | r <- out
          , Right (_, ts, v) <- [decodeWindowed r]
          ]
    -- Exactly one record per (already-closed) window — the
    -- "emit once at close" KIP-825 contract.
    Set.fromList decoded
      `shouldBe` Set.fromList
        [ (0, 9.0) -- max for window [0, 5000)
        , (5000, 7.0) -- max for window [5000, 10000)
        ]
    -- Doubly defensive: no record appears twice.
    Set.size (Set.fromList decoded) `shouldBe` length decoded
    -- And every record carries the inner key 'k'.
    let keys =
          [ k
          | r <- out
          , Right (k, _, _) <- [decodeWindowed r]
          ]
    (all (== Just "k") keys) `shouldBe` True
    closeDriver driver


----------------------------------------------------------------------
-- 4. suppressUntilTimeLimit flushes on stream-time advance
----------------------------------------------------------------------

{- | Per-key debounce: at most one emission per key per
'limitMs'. The bug fix here mirrors @suppressWindowedProc@ —
without a stream-time punctuator the buffered debounce value
only flushes when another record arrives on the affected key,
so a key that goes silent forever after one update would
never deliver.
-}
suppress_until_time_limit_flushes_on_stream_time_advance :: Spec
suppress_until_time_limit_flushes_on_stream_time_advance =
  it "suppressUntilTimeLimit: stream-time advance flushes silent debounce buffers" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.suppressUntilTimeLimit (Time.millis 1000)
            >>> F.sink "out"
    (_h, topo) <- F.compile topology
    driver <- newDriver topo "wd-tl-flush"
    pipeInput
      driver
      (topicName "in")
      (Just (bytes "k"))
      (bytes "v0")
      (Timestamp 0)
      0
    pre <- readOutput driver (topicName "out")
    -- Stream time is 0; debounce window ends at 1000. Buffer
    -- holds the value; nothing emitted yet.
    length pre `shouldBe` 0

    -- Advance past the debounce limit with NO further input.
    -- The punctuator must flush the buffered value.
    advanceDriverStreamTime driver (Timestamp 1500)
    post <- readOutput driver (topicName "out")
    map
      ( \r ->
          ( fmap (T.pack . BSC.unpack) (crKey r)
          , T.pack (BSC.unpack (crValueBytes r))
          )
      )
      post
      `shouldBe` [(Just "k", "v0")]
    closeDriver driver


----------------------------------------------------------------------
-- 5. suppressWindowedShed flushes on stream-time advance
----------------------------------------------------------------------

{- | Same bug, third venue: the shed-to-DLQ variant of the
bounded suppress operator. Buffer two windows worth of data
on distinct keys (within the record cap so no shedding
happens), then advance stream time past their close + grace
and check that the main downstream receives them.
-}
suppress_windowed_shed_flushes_on_stream_time_advance :: Spec
suppress_windowed_shed_flushes_on_stream_time_advance =
  it "suppressWindowedShed: stream-time advance flushes buffered windows" $ do
    let shelf =
          Suppress.DeadLetterShelf
            { Suppress.dlsTopic = topicName "shed-dlq"
            , Suppress.dlsKeySerde = windowedSerde textSerde
            , Suppress.dlsValueSerde = doubleSerde
            , Suppress.dlsRecordCap = 8 -- high enough that we never shed
            }
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Double "in"
            >>> F.groupByKey
            >>> F.windowedByTime (Win.tumblingWindows (Time.seconds 5))
            >>> F.reduceWindowed max maxMat
            >>> F.streamFromWindowed
            >>> F.suppressWindowedShed
              (Time.millis 0)
              (Time.durationMillis (Time.seconds 5))
              shelf
            >>> F.sink "shed-out"
        maxMat :: Materialized Text Double
        maxMat =
          Mat.withValueSerde doubleSerde $
            Mat.withKeySerde textSerde $
              Mat.materialized
    (_h, topo) <- F.compile topology
    driver <- newDriver topo "wd-shed-flush"
    -- Two distinct keys, both in window [0, 5000). After all
    -- four inputs stream time is 4500, still before W0 closes.
    mapM_
      ( \(k, ts, v) ->
          pipeInput
            driver
            (topicName "in")
            (Just (bytes k))
            (serialize doubleSerde v)
            (Timestamp ts)
            0
      )
      [ ("a" :: Text, 100 :: Int64, 1.0 :: Double)
      , ("a", 4500, 2.0)
      , ("b", 200, 3.0)
      , ("b", 4400, 4.0)
      ]
    preMain <- readOutput driver (topicName "shed-out")
    preDlq <- readOutput driver (topicName "shed-dlq")
    let preDecoded =
          [ (k, ts, v)
          | r <- preMain
          , Right (k, ts, v) <- [decodeWindowed r]
          ]
    preDecoded `shouldBe` []
    length preDlq `shouldBe` 0 -- no shedding either (under the cap)

    -- Stream-time advance: W0 closes at 5000 + 0. With the
    -- punctuator fix the suppress flush fires.
    advanceDriverStreamTime driver (Timestamp 5001)
    postMain <- readOutput driver (topicName "shed-out")
    let postDecoded =
          [ (k, ts, v)
          | r <- postMain
          , Right (k, ts, v) <- [decodeWindowed r]
          ]
    -- Both keys flushed; max(a) = 2.0; max(b) = 4.0.
    Set.fromList postDecoded
      `shouldBe` Set.fromList
        [ (Just "a", 0, 2.0)
        , (Just "b", 0, 4.0)
        ]
    closeDriver driver
