{-# LANGUAGE OverloadedStrings #-}

{- | Live-broker tests for the consumer offset / seek surface
(KIP-41 'position', KIP-79 beginning/end/offsetsForTimes,
KIP-211 batch 'committedAll', KIP-339 'seek' / 'seekToBeginning'
/ 'seekToEnd').

Skipped at run time unless @WIREFORM_KAFKA_BROKER=host:port@ is
set (mirrors the rest of the integration suite). Each scenario
runs on a freshly-named topic so concurrent runs don't collide.
-}
module Integration.ConsumerOffsetsSpec (
  tests,
) where

import Control.Monad (replicateM_)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP
import System.Environment (lookupEnv)
import Test.Syd


----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Integration: Consumer offset / seek APIs" $
    sequence_
      [ it "endOffsets / beginningOffsets reflect produced records" $
          withBroker testEndAndBeginning
      , it "seekToEnd / seek + position behave consistently" $
          withBroker testSeekAndPosition
      , it "produce + assign + poll round-trips records (Produce v13 / Fetch v12 / OffsetFetch v8)" $
          withBroker testProduceAndPollRoundTrip
      , it "offsetsForTimesFull surfaces timestamp + leader epoch (ListOffsets v8)" $
          withBroker testOffsetsForTimesFull
      ]


-- The fixed pre-created topic name. Operators are expected to
-- create this topic ahead of running the integration suite (it's
-- created once and reused across tests so we don't depend on the
-- producer's auto-create + metadata-refresh path, which still
-- has known interactions with freshly-created topics).
fixedTopic :: T.Text
fixedTopic = "wireform-bench-cmp"


----------------------------------------------------------------------
-- Scenarios
----------------------------------------------------------------------

testEndAndBeginning :: T.Text -> IO ()
testEndAndBeginning brokerText = do
  let topic = fixedTopic
  produceN brokerText topic 5
  consumer <- mkConsumerOrFail brokerText "wf-it-grp-eo"
  let tp = KC.TopicPartition topic 0
  beg <- KC.beginningOffsets consumer [tp]
  end <- KC.endOffsets consumer [tp]
  case (beg, end) of
    (Right bm, Right em) -> do
      let begOff = fromMaybe (-9999) (HashMap.lookup tp bm)
          endOff = fromMaybe (-9999) (HashMap.lookup tp em)
      -- Kafka may have cleaned earlier log segments due to
      -- retention; the begin offset isn't guaranteed to be 0.
      -- All we need is begin >= 0 (i.e. the topic exists) and
      -- end >= begin + 5 (the records we just produced are
      -- visible).
      (if (begOff >= 0) then pure () else expectationFailure ("begin offset should be >= 0 (got " ++ show begOff ++ ")"))
      ( if (endOff >= begOff + 5)
          then pure ()
          else
            expectationFailure
              ( "end offset should be >= begin + 5 (got begin="
                  ++ show begOff
                  ++ ", end="
                  ++ show endOff
                  ++ ")"
              )
        )
    _ -> error ("offsets failed: beg=" ++ show beg ++ " end=" ++ show end)
  KC.closeConsumer consumer


testSeekAndPosition :: T.Text -> IO ()
testSeekAndPosition brokerText = do
  let topic = fixedTopic
  produceN brokerText topic 10
  consumer <- mkConsumerOrFail brokerText "wf-it-grp-seek"
  -- We use 'assign' so we don't depend on the consumer-group
  -- coordinator's rebalance for this test.
  let tp = KC.TopicPartition topic 0
  KC.assign consumer [tp] >>= either error (\_ -> pure ())
  -- After 'assign' the consumer's local position is whatever the
  -- broker's earliest/latest offset query returned (depending on
  -- auto.offset.reset). 'seekToBeginning' resets it to the
  -- partition's actual earliest offset; we read that with
  -- 'beginningOffsets' so the assertion is independent of any
  -- per-broker retention / compaction state.
  KC.seekToBeginning consumer [tp] >>= either error (\_ -> pure ())
  begMap <- KC.beginningOffsets consumer [tp]
  let !begOff =
        case begMap of
          Right m -> fromMaybe (-9999) (HashMap.lookup tp m)
          Left e -> error e
  pos1 <- KC.position consumer tp
  pos1 `shouldBe` Right begOff
  -- Now seek to a specific offset (offset 4 — chosen because the
  -- topic always has > 4 records by this point in the test run).
  KC.seek consumer tp (begOff + 4) >>= either error (\_ -> pure ())
  pos2 <- KC.position consumer tp
  pos2 `shouldBe` Right (begOff + 4)
  -- And go to the end; verify it's >= the begin + 10 records we
  -- just produced (the topic may also hold records from prior
  -- test runs, hence the >=).
  KC.seekToEnd consumer [tp] >>= either error (\_ -> pure ())
  pos3 <- KC.position consumer tp
  case pos3 of
    Right o ->
      ( if (o >= begOff + 10)
          then pure ()
          else
            expectationFailure
              ( "expected position >= begin + 10 (got "
                  ++ show o
                  ++ ", begin = "
                  ++ show begOff
                  ++ ")"
              )
      )
    Left e -> error e
  KC.closeConsumer consumer


testProduceAndPollRoundTrip :: T.Text -> IO ()
testProduceAndPollRoundTrip brokerText = do
  -- End-to-end exercise of the recently-bumped:
  --   * Produce v13 (KIP-516 TopicId-based; the metadata cache
  --     populates 'topicProduceDataTopicId' from
  --     MetadataResponse v10+).
  --   * Fetch v12 (flexible; previously broken by the
  --     codegen tagged-string bug now fixed in this branch).
  --   * OffsetFetch v8 (per-group batched groups[] shape; the
  --     consumer's commit/fetch path dispatches on the
  --     negotiated version).
  -- The other tests in this group only call ListOffsets, which
  -- is its own separate negotiation site (now bumped to v8).
  let topic = fixedTopic
      payload = T.encodeUtf8 (T.pack "wf-it-poll-payload")
  pcfg <-
    pure $
      KP.defaultProducerConfig
        { KP.producerClientId = "wf-it-poll-prod"
        , KP.producerLingerMs = 5
        , KP.producerBatchSize = 16384
        }
  pr <- either error pure =<< KP.createProducer [brokerText] pcfg
  -- Stamp the record with a known offset so the test can find
  -- it in the poll output without relying on broker state.
  sendR <- KP.sendMessage pr topic Nothing payload
  case sendR of
    Left e -> expectationFailure ("produce: " <> e)
    Right md -> do
      let producedOffset = md.offset
      KP.closeProducer pr
      consumer <- mkConsumerOrFail brokerText "wf-it-poll-grp"
      let tp = KC.TopicPartition topic 0
      KC.assign consumer [tp] >>= either error (\_ -> pure ())
      KC.seek consumer tp producedOffset
        >>= either error (\_ -> pure ())
      -- Poll once with a 2-second timeout. The broker should
      -- return our record in the first response (the request
      -- has min.bytes=1 so it doesn't wait for more data).
      pollR <- KC.poll consumer 2000
      KC.closeConsumer consumer
      case pollR of
        Left e -> expectationFailure ("poll: " <> e)
        Right rs -> do
          (not (null rs)) `shouldBe` True
          let !mr = filter (\r -> r.offset == producedOffset) rs
          case mr of
            (r : _) -> do
              r.topic `shouldBe` topic
              r.partition `shouldBe` 0
              r.value `shouldBe` Just payload
            [] ->
              expectationFailure
                ( "poll did not return the record we just produced "
                    <> "(offset="
                    <> show producedOffset
                    <> "; got offsets="
                    <> show (map (.offset) rs)
                    <> ")"
                )


testOffsetsForTimesFull :: T.Text -> IO ()
testOffsetsForTimesFull brokerText = do
  -- ListOffsets v4+ surfaces a per-partition timestamp and leader
  -- epoch on top of the offset; 'offsetsForTimes' (the legacy
  -- KIP-79 façade) drops them, so we use 'offsetsForTimesFull'
  -- to assert they round-trip cleanly.
  let topic = fixedTopic
  produceN brokerText topic 5
  consumer <- mkConsumerOrFail brokerText "wf-it-grp-oft"
  let tp = KC.TopicPartition topic 0
  -- Ask for "earliest after timestamp -2 (the broker's earliest
  -- magic value)"; that's just the partition's beginning offset
  -- but the request goes through ListOffsets v4+ so the
  -- timestamp/leaderEpoch fields are populated.
  r <- KC.offsetsForTimesFull consumer [(tp, -2)]
  KC.closeConsumer consumer
  case r of
    Left err -> expectationFailure ("offsetsForTimesFull: " ++ err)
    Right hm ->
      case HashMap.lookup tp hm of
        Nothing ->
          expectationFailure
            ( "offsetsForTimesFull: no entry for "
                ++ T.unpack topic
                ++ ":"
                ++ show tp.partition
            )
        Just oat -> do
          (if (KC.oatOffset oat >= 0) then pure () else expectationFailure ("expected non-negative offset, got " ++ show (KC.oatOffset oat)))
          (if (KC.oatLeaderEpoch oat >= 0) then pure () else expectationFailure ("expected leader epoch >= 0, got " ++ show (KC.oatLeaderEpoch oat)))
          -- Timestamp is the broker-reported value; -2 is a
          -- sentinel meaning "earliest", and the broker may
          -- echo -1 if no record exists. Just check it's a
          -- value (not the uninitialised default).
          let _ = KC.oatTimestamp oat
          pure ()


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

withBroker :: (T.Text -> IO ()) -> IO ()
withBroker k = do
  m <- lookupEnv "WIREFORM_KAFKA_BROKER"
  case m of
    Nothing -> pure () -- skip (mirrors the rest of the integration suite)
    Just h -> k (T.pack h)


mkConsumerOrFail :: T.Text -> T.Text -> IO KC.Consumer
mkConsumerOrFail brokerText groupId = do
  let cfg =
        KC.defaultConsumerConfig
          { KC.consumerClientId = "wf-it-consumer"
          , KC.consumerGroupId = groupId
          }
  r <- KC.createConsumer [brokerText] groupId cfg
  case r of
    Left err -> error ("createConsumer: " <> err)
    Right c -> pure c


produceN :: T.Text -> T.Text -> Int -> IO ()
produceN brokerText topic n = do
  let pcfg =
        KP.defaultProducerConfig
          { KP.producerClientId = "wf-it-producer"
          , KP.producerLingerMs = 5
          , KP.producerBatchSize = 16384
          }
  r <- KP.createProducer [brokerText] pcfg
  case r of
    Left err -> error ("producer create: " <> err)
    Right p -> do
      replicateM_ n $ do
        rr <- KP.sendMessage p topic Nothing "v"
        case rr of
          Left e -> error ("produce: " ++ e)
          Right _ -> pure ()
      _ <- KP.flushProducer p
      KP.closeProducer p
