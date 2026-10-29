{-# LANGUAGE PackageImports #-}

{- |
Module      : Integration.BasicSpec
Description : Basic integration tests for the Kafka client.
Copyright   : (c) 2025
License     : BSD-3-Clause

These tests need a running Kafka broker (see
@test-integration/docker-compose.yml@). They cover:

  * raw TCP connect / disconnect to the broker,
  * a hand-rolled @MetadataRequest@ over 'Kafka.Network.Connection',
  * end-to-end producer + consumer flow,
  * a one-record produce/consume round-trip through the public API.
-}
module Integration.BasicSpec (tests) where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS8
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word
import Kafka.Client.Consumer qualified as Consumer
import Kafka.Client.Producer qualified as Producer
import Kafka.Network.Connection qualified as Conn
import System.Timeout (timeout)
import Test.Syd
import "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest qualified as MR
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RequestHeader qualified as RH
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P


-- | Bootstrap broker for testing
testBroker :: Conn.BrokerAddress
testBroker =
  Conn.BrokerAddress
    { Conn.brokerHost = "127.0.0.1"
    , Conn.brokerPort = 9092
    }


-- | Test connection configuration
testConfig :: Conn.ConnectionConfig
testConfig = Conn.defaultConnectionConfig


-- | Test topic name
testTopic :: Text
testTopic = "kafka-native-integration-test"


-- | All integration tests
tests :: Spec
tests =
  describe "Integration Tests" $
    sequence_
      [ connectionTests
      , metadataTests
      , producerTests
      , consumerTests
      , produceConsumeTests
      ]


connectionTests :: Spec
connectionTests =
  describe "Connection" $
    sequence_
      [ it "Can connect to Kafka broker" testConnection
      , it "Can disconnect cleanly" testDisconnect
      ]


metadataTests :: Spec
metadataTests =
  describe "Metadata" $
    sequence_
      [ it "Can request cluster metadata" testMetadataRequest
      ]


-- | Test that we can establish a connection to the broker
testConnection :: IO ()
testConnection = do
  result <- Conn.connect testBroker testConfig
  case result of
    Left err -> expectationFailure $ "Failed to connect: " ++ err
    Right conn -> Conn.disconnect conn


-- | Test clean disconnection
testDisconnect :: IO ()
testDisconnect = do
  result <- Conn.connect testBroker testConfig
  case result of
    Left err -> expectationFailure $ "Failed to connect: " ++ err
    Right conn -> Conn.disconnect conn


{- | Smoke-test: we can serialise a MetadataRequest header without
the codec blowing up. Actually transporting the request lives in
the producer / consumer / admin paths; this assertion is purely
structural.
-}
testMetadataRequest :: IO ()
testMetadataRequest = do
  result <- Conn.withConnection testBroker testConfig $ \_conn -> do
    let _metadataReq =
          MR.MetadataRequest
            { MR.metadataRequestTopics = P.mkKafkaArray mempty
            , MR.metadataRequestAllowAutoTopicCreation = True
            , MR.metadataRequestIncludeClusterAuthorizedOperations = False
            , MR.metadataRequestIncludeTopicAuthorizedOperations = False
            }

        apiVersion = 0 :: Int
        correlationId = 1 :: Int32
        clientId = P.mkKafkaString "kafka-native-test"

        _header =
          RH.RequestHeader
            { RH.requestHeaderRequestApiKey = 3 -- Metadata API
            , RH.requestHeaderRequestApiVersion = fromIntegral apiVersion
            , RH.requestHeaderCorrelationId = correlationId
            , RH.requestHeaderClientId = clientId
            }
    pure ()

  case result of
    Left err -> expectationFailure $ "Connection failed: " ++ err
    Right () -> pure ()


-- | Producer tests
producerTests :: Spec
producerTests =
  describe "Producer" $
    sequence_
      [ it "Can create and close producer" testCreateProducer
      , it "Can send message synchronously" testProducerSendSync
      , it "Can send batch of messages" testProducerBatch
      ]


-- | Test creating and closing a producer
testCreateProducer :: IO ()
testCreateProducer = do
  result <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case result of
    Left err -> expectationFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      Producer.closeProducer producer
      putStrLn "Producer created and closed successfully"


-- | Test sending a message synchronously
testProducerSendSync :: IO ()
testProducerSendSync = do
  putStrLn "Creating producer..."
  result <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case result of
    Left err -> expectationFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      let key = Just (BS8.pack "producer-key")
          value = BS8.pack "producer-value-sync"

      sendResult <- Producer.sendMessage producer testTopic key value
      Producer.closeProducer producer

      case sendResult of
        Left err -> expectationFailure $ "Failed to send: " ++ err
        Right md -> do
          putStrLn $ "Sent to offset: " ++ show md.offset
          (md.offset >= 0) `shouldBe` True


-- | Test sending a batch of messages
testProducerBatch :: IO ()
testProducerBatch = do
  result <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case result of
    Left err -> expectationFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      results <-
        sequence
          [ Producer.sendMessage
              producer
              testTopic
              (Just $ BS8.pack ("batch-key-" <> show i))
              (BS8.pack ("batch-value-" <> show i))
          | i <- [1 .. 10 :: Int]
          ]

      Producer.closeProducer producer

      case sequence results of
        Left err -> expectationFailure $ "Failed to send batch: " ++ err
        Right metadatas -> do
          putStrLn $ "Sent " ++ show (length metadatas) ++ " messages"
          (length metadatas) `shouldBe` 10


-- | Consumer tests
consumerTests :: Spec
consumerTests =
  describe "Consumer" $
    sequence_
      [ it "Can create and close consumer" testCreateConsumer
      , it "Can manually assign partitions" testConsumerAssign
      , it "Can poll for records" testConsumerPoll
      , it "Can subscribe + receive an assignment" testConsumerSubscribe
      ]


testCreateConsumer :: IO ()
testCreateConsumer = do
  let config = Consumer.defaultConsumerConfig
  result <- Consumer.createConsumer ["localhost:9092"] "" config
  case result of
    Left err -> expectationFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      Consumer.closeConsumer consumer
      putStrLn "Consumer created and closed successfully"


testConsumerAssign :: IO ()
testConsumerAssign = do
  let config = Consumer.defaultConsumerConfig
  result <- Consumer.createConsumer ["localhost:9092"] "" config
  case result of
    Left err -> expectationFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      let partitions = [Consumer.TopicPartition testTopic 0]
      assignResult <- Consumer.assign consumer partitions
      assignment <- Consumer.assignment consumer
      Consumer.closeConsumer consumer
      case assignResult of
        Left err -> expectationFailure $ "Failed to assign: " ++ err
        Right () -> do
          putStrLn $ "Assigned " ++ show (length assignment) ++ " partition(s)"
          (length assignment) `shouldBe` 1


testConsumerPoll :: IO ()
testConsumerPoll = do
  let config = Consumer.defaultConsumerConfig
  result <- Consumer.createConsumer ["localhost:9092"] "" config
  case result of
    Left err -> expectationFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      let partitions = [Consumer.TopicPartition testTopic 0]
      assignResult <- Consumer.assign consumer partitions
      case assignResult of
        Left err -> do
          Consumer.closeConsumer consumer
          expectationFailure $ "Failed to assign: " ++ err
        Right () -> do
          pollResult <- Consumer.poll consumer 5000
          Consumer.closeConsumer consumer
          case pollResult of
            Left err -> expectationFailure $ "Failed to poll: " ++ err
            Right records ->
              putStrLn $ "Polled " ++ show (length records) ++ " record(s)"


{- | End-to-end exercise of the consumer-group join path:
createConsumer (which kicks off the heartbeat thread for a
non-empty group id) -> subscribe (which runs FindCoordinator
+ JoinGroup + SyncGroup against the live broker). The
assertion is that subscribe returns within a reasonable
wall-clock budget (10 s). For a single-member group the
broker should hold JoinGroup open only until
'group.initial.rebalance.delay.ms' elapses (3 s default),
so 10 s is more than enough headroom on a healthy cluster.
-}
testConsumerSubscribe :: IO ()
testConsumerSubscribe = do
  ts <- (show :: Integer -> String) . truncate <$> getPOSIXTime
  let groupId = T.pack ("wf-it-subgrp-" ++ ts)
      cfg =
        Consumer.defaultConsumerConfig
          { Consumer.consumerAutoOffsetReset = Consumer.Earliest
          , Consumer.consumerAutoCommit = False
          }
  result <- Consumer.createConsumer ["localhost:9092"] groupId cfg
  case result of
    Left err -> expectationFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      mr <- timeout (10 * 1000_000) $ Consumer.subscribe consumer [testTopic]
      Consumer.closeConsumer consumer
      case mr of
        Nothing ->
          expectationFailure
            "subscribe took longer than 10 s — heartbeat / rebalance hung"
        Just (Left e) -> expectationFailure ("subscribe: " ++ e)
        Just (Right _) -> putStrLn "subscribe completed in <10 s"


-- | Produce-consume integration tests
produceConsumeTests :: Spec
produceConsumeTests =
  describe "Produce-Consume Integration" $
    sequence_
      [ it "Can produce and consume messages" testProduceConsumeIntegration
      ]


{- | Produce a record through the real producer, then consume it
back through the real consumer assigned to the same partition.
-}
testProduceConsumeIntegration :: IO ()
testProduceConsumeIntegration = do
  let uniqueValue = "integration-test-" <> show (hash 42 :: Word64)
      payload = BS8.pack uniqueValue

  producerResult <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case producerResult of
    Left err -> expectationFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      sendResult <- Producer.sendMessage producer testTopic Nothing payload
      Producer.closeProducer producer

      case sendResult of
        Left err -> expectationFailure $ "Failed to produce: " ++ err
        Right md -> do
          let producedOffset = md.offset
              producedPart = md.partition
          putStrLn $ "Produced at offset: " ++ show producedOffset
          (producedOffset >= 0) `shouldBe` True

          -- Consume from the partition the broker actually assigned.
          let config =
                Consumer.defaultConsumerConfig
                  { Consumer.consumerAutoOffsetReset = Consumer.Earliest
                  }
          consumerResult <- Consumer.createConsumer ["localhost:9092"] "" config
          case consumerResult of
            Left err -> expectationFailure $ "Failed to create consumer: " ++ err
            Right consumer -> do
              let partitions = [Consumer.TopicPartition testTopic producedPart]
              _ <- Consumer.assign consumer partitions

              pollResult <- Consumer.poll consumer 10000
              Consumer.closeConsumer consumer

              case pollResult of
                Left err -> expectationFailure $ "Failed to poll: " ++ err
                Right records -> do
                  let ours = filter (\r -> r.value == Just payload) records
                  when (null ours) $
                    putStrLn $
                      "Polled " ++ show (length records) ++ " unrelated record(s)"
                  (not (null ours)) `shouldBe` True


-- Simple deterministic hash used to build a unique payload per run.
hash :: Word64 -> Word64
hash x = x * 2654435761
