{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the producer / consumer interceptor APIs. The
functional cases run against the in-process accumulator +
record-batch encoder; the network
side is exercised by 'producerOnAcknowledgement' through the
broker-rejection path of @sendMessage@ (already covered by
the lifecycle tests).
-}
module Client.InterceptorSpec (tests) where

import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kafka.Client.Consumer qualified as Consumer
import Kafka.Client.Producer qualified as Producer
import Test.Syd


tests :: Spec
tests =
  describe "Interceptor APIs" $
    sequence_
      [ it
          "ProducerConfig: default interceptor is identity"
          defaultProducerInterceptorIsIdentity
      , it
          "ProducerConfig: default onAcknowledgement is no-op"
          defaultProducerOnAckIsNoOp
      , it
          "ProducerConfig: interceptor can rewrite topic / key / value"
          interceptorCanRewriteRecord
      , it
          "ConsumerConfig: default interceptor is identity"
          defaultConsumerInterceptorIsIdentity
      , it
          "ConsumerConfig: default onCommit is no-op"
          defaultConsumerOnCommitIsNoOp
      , it
          "ConsumerConfig: interceptor can drop / rewrite records"
          consumerInterceptorCanDropRecords
      , it
          "ConsumerConfig: onCommit receives the offsets passed in"
          consumerOnCommitReceivesOffsets
      ]


defaultProducerInterceptorIsIdentity :: IO ()
defaultProducerInterceptorIsIdentity = do
  let cfg = Producer.defaultProducerConfig
      rec_ =
        Producer.ProducerRecord
          { topic = "t"
          , key = Just "k"
          , value = Just "v"
          , headers = []
          , partition = Nothing
          , timestamp = Nothing
          }
  out <- Producer.producerInterceptor cfg rec_
  out `shouldBe` rec_


defaultProducerOnAckIsNoOp :: IO ()
defaultProducerOnAckIsNoOp = do
  let cfg = Producer.defaultProducerConfig
      rec_ = Producer.ProducerRecord "t" Nothing (Just "v") [] Nothing Nothing
  -- Has to not throw and to return ()
  Producer.producerOnAcknowledgement cfg rec_ (Left "broker timeout")


interceptorCanRewriteRecord :: IO ()
interceptorCanRewriteRecord = do
  let cfg =
        Producer.defaultProducerConfig
          { Producer.producerInterceptor = \r ->
              -- DuplicateRecordFields makes the bare @r { topic =
              -- ... }@ ambiguous because multiple types in scope
              -- have a 'topic' field. Rebuild the
              -- 'Producer.ProducerRecord' explicitly so the type
              -- of every field site is unambiguous.
              pure
                Producer.ProducerRecord
                  { Producer.topic = r.topic <> "-suffix"
                  , Producer.key = r.key
                  , Producer.value = r.value
                  , Producer.headers = r.headers ++ [("trace-id", "abc")]
                  , Producer.partition = r.partition
                  , Producer.timestamp = r.timestamp
                  }
          }
      input =
        Producer.ProducerRecord
          { Producer.topic = "events"
          , Producer.key = Just "k"
          , Producer.value = Just "v"
          , Producer.headers = []
          , Producer.partition = Nothing
          , Producer.timestamp = Nothing
          }
  out <- Producer.producerInterceptor cfg input
  out.topic `shouldBe` "events-suffix"
  out.headers `shouldBe` [("trace-id", "abc")]


defaultConsumerInterceptorIsIdentity :: IO ()
defaultConsumerInterceptorIsIdentity = do
  let cfg = Consumer.defaultConsumerConfig
  out <- Consumer.consumerInterceptor cfg sampleRecords
  out `shouldBe` sampleRecords


defaultConsumerOnCommitIsNoOp :: IO ()
defaultConsumerOnCommitIsNoOp = do
  let cfg = Consumer.defaultConsumerConfig
  -- Just verifies the call doesn't throw.
  Consumer.consumerOnCommit
    cfg
    [(Consumer.TopicPartition "t" 0, 7)]


consumerInterceptorCanDropRecords :: IO ()
consumerInterceptorCanDropRecords = do
  let cfg =
        Consumer.defaultConsumerConfig
          { Consumer.consumerInterceptor = \rs ->
              pure (filter (\r -> r.key /= Just "drop") rs)
          }
      input =
        [ sampleRec "k1" "v1"
        , sampleRec "drop" "x"
        , sampleRec "k2" "v2"
        ]
  out <- Consumer.consumerInterceptor cfg input
  map (.key) out `shouldBe` [Just "k1", Just "k2"]


consumerOnCommitReceivesOffsets :: IO ()
consumerOnCommitReceivesOffsets = do
  ref <- newIORef []
  let cfg =
        Consumer.defaultConsumerConfig
          { Consumer.consumerOnCommit = \os ->
              atomicModifyIORef' ref (\acc -> (acc ++ os, ()))
          }
      offsets =
        [ (Consumer.TopicPartition "t" 0, 7)
        , (Consumer.TopicPartition "t" 1, 9)
        ]
  Consumer.consumerOnCommit cfg offsets
  got <- readIORef ref
  got `shouldBe` offsets


sampleRecords :: [Consumer.ConsumerRecord]
sampleRecords = [sampleRec "k" "v"]


sampleRec :: String -> String -> Consumer.ConsumerRecord
sampleRec k v =
  Consumer.ConsumerRecord
    { topic = "t"
    , partition = 0
    , offset = 0
    , timestamp = 0
    , key = Just (toBs k)
    , value = Just (toBs v)
    , headers = []
    }
  where
    toBs = TE.encodeUtf8 . T.pack
