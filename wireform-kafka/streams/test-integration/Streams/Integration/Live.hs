{-# LANGUAGE OverloadedStrings #-}

{- | A 'KafkaStreams' application run end to end against a live broker
on the native driver: a word count over a two-partition topic, once
per processing guarantee. The instance is stopped and a fresh one
(new state directory) started half way, so the second half also
exercises restoring the count store from its changelog. The output is
read back with a read-committed consumer; every key's latest count
must equal the number of rounds produced.

Each run uses fresh application and topic names, so runs against the
same broker don't see each other's data.
-}
module Streams.Integration.Live (tests) where

import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Kafka.Client.AdminClient qualified as Admin
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP
import Kafka.Streams.Imperative
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


tests :: String -> Spec
tests brokers =
  describe "Live runtime" $
    sequence_
      [ it "word count, at least once, across a restart" (wordCountRun brokers AtLeastOnceP "alo")
      , it "word count, exactly once v2, across a restart" (wordCountRun brokers ExactlyOnceV2 "eos")
      ]


keys :: [Text]
keys = map (\i -> "k" <> T.pack (show i)) [0 .. 9 :: Int]


wordCountRun :: String -> ProcessingGuarantee -> Text -> IO ()
wordCountRun brokers guarantee tag = do
  stamp <- (round . (* 1000) <$> getPOSIXTime) :: IO Int64
  let suffix = tag <> "-" <> T.pack (show stamp)
      app = "kstreams-live-" <> suffix
      inT = "kstreams-live-in-" <> suffix
      outT = "kstreams-live-out-" <> suffix
      bs = [T.pack brokers]
  created <-
    Admin.withAdminClient bs Admin.defaultAdminClientConfig $ \admin ->
      Admin.createTopics admin [Admin.NewTopic inT 2 1 [], Admin.NewTopic outT 2 1 []]
  either (expectationFailure . ("createTopics: " <>)) (mapM_ (either (expectationFailure . ("createTopics: " <>)) pure . snd)) created
  topo <- countTopology inT outT
  let cfgFor dir =
        defaultStreamsConfig
          { applicationId = app
          , bootstrapServers = bs
          , processingGuarantee = guarantee
          , commitIntervalMs = Just 100
          , replicationFactor = 1
          , stateDir = dir
          }
      -- One instance on its own state directory, run until the
      -- output shows @rounds@ for every key.
      runUntil rounds =
        withSystemTempDirectory "kstreams-live" $ \dir -> do
          ks <- newKafkaStreams (cfgFor dir) topo
          ( do
              startKafkaStreams ks
              reached <- awaitCounts ks bs outT (fromIntegral rounds)
              unless reached $ do
                st <- streamsStatus ks
                expectationFailure ("counts never reached " <> show rounds <> "; instance " <> show st)
            )
            `finally` closeKafkaStreams ks
  produceRounds bs inT [1, 2, 3]
  runUntil (3 :: Int)
  -- A fresh instance with an empty state directory: the counts come
  -- back from the changelog, and carry on.
  produceRounds bs inT [4, 5]
  runUntil 5


countTopology :: Text -> Text -> IO TopologyValid
countTopology inT outT = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName inT) (consumed textSerde textSerde)
  counts <- countStream (materializedAs (storeName "counts")) (groupByKey (grouped textSerde textSerde) s)
  out <- toKStreamFromKTable counts
  toTopic (topicName outT) (produced textSerde int64Serde) out
  topo <- buildTopology b
  either (error . show) pure (validateTopology topo)


produceRounds :: [Text] -> Text -> [Int] -> IO ()
produceRounds bs topic rounds = do
  r <- KP.withProducer bs KP.defaultProducerConfig $ \p -> do
    forM_ rounds $ \_ ->
      forM_ keys $ \k ->
        KP.sendMessage p topic (Just (TE.encodeUtf8 k)) "x"
          >>= either (expectationFailure . ("produce: " <>)) (const (pure ()))
    KP.flushProducer p
  either (expectationFailure . ("flush: " <>)) pure r


{- | Read @topic@ from the start with a read-committed consumer until
every key's latest count is @n@ (or the instance stops running, or
the budget of polls runs out).
-}
awaitCounts :: KafkaStreams -> [Text] -> Text -> Int64 -> IO Bool
awaitCounts ks bs topic n = do
  let cfg =
        KC.defaultConsumerConfig
          { KC.consumerIsolationLevel = KC.ReadCommitted
          , KC.consumerAutoOffsetReset = KC.Earliest
          }
  rc <- KC.createConsumer bs "" cfg
  case rc of
    Left err -> expectationFailure ("createConsumer: " <> err) >> pure False
    Right c ->
      ( do
          _ <- KC.assign c [KC.TopicPartition topic 0, KC.TopicPartition topic 1]
          go c Map.empty (600 :: Int)
      )
        `finally` KC.closeConsumer c
  where
    done m = all (\k -> Map.lookup (TE.encodeUtf8 k) m == Just n) keys
    go _ m 0 = pure (done m)
    go c m budget
      | done m = pure True
      | otherwise = do
          st <- streamsStatus ks
          case st of
            StreamsError _ -> pure False
            StreamsClosed -> pure False
            _ -> do
              r <- KC.poll c 100
              case r of
                Left err -> expectationFailure ("poll: " <> err) >> pure False
                Right recs -> go c (foldl latest m recs) (budget - 1)
    latest :: Map ByteString Int64 -> KC.ConsumerRecord -> Map ByteString Int64
    latest m r = case (r.key, r.value) of
      (Just k, Just v) -> either (const m) (\c -> Map.insert k c m) (deserialize int64Serde v)
      _ -> m
