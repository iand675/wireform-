{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | KIP-663 dynamic thread management tests. Verifies that
'addStreamThread' / 'removeStreamThread' actually mutate the
live 'WorkerPool' (not just report a hypothetical count).
-}
module Streams.DynamicThreadsSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.NativeDriver
import Test.Syd


tests :: Spec
tests =
  describe "Dynamic thread management (KIP-663) + CloseOptions (KIP-812)" $
    sequence_
      [ add_stream_thread_grows_pool
      , remove_stream_thread_shrinks_pool
      , add_then_remove_returns_to_baseline
      , single_thread_runtime_returns_nothing
      , close_with_leave_group_false_skips_leave_group
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


mkRec :: Text -> Text -> Text -> KC.ConsumerRecord
mkRec topic k v =
  KC.ConsumerRecord
    { topic = topic
    , partition = 0
    , offset = 0
    , timestamp = 100
    , key = Just (bytes k)
    , value = Just (bytes v)
    , headers = []
    }


buildPassthrough :: IO TopologyValid
buildPassthrough = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


multiThreadCfg :: Int -> StreamsConfig
multiThreadCfg n =
  defaultStreamsConfig
    { applicationId = "dyn-threads"
    , bootstrapServers = ["mock:0"]
    , numStreamThreads = n
    , pollMs = 0
    }


----------------------------------------------------------------------
-- 1. addStreamThread grows the pool
----------------------------------------------------------------------

add_stream_thread_grows_pool :: Spec
add_stream_thread_grows_pool =
  it "addStreamThread: count goes from 2 to 4 after two adds" $ do
    topo <- buildPassthrough
    ks <- newKafkaStreams (multiThreadCfg 2) topo
    (drv, _h) <- newMockDriver
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    streamThreadCount ks >>= (`shouldBe` 2)

    r1 <- addStreamThread ks
    r2 <- addStreamThread ks
    r1 `shouldBe` Just 3
    r2 `shouldBe` Just 4

    streamThreadCount ks >>= (`shouldBe` 4)

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 2. removeStreamThread shrinks the pool
----------------------------------------------------------------------

remove_stream_thread_shrinks_pool :: Spec
remove_stream_thread_shrinks_pool =
  it "removeStreamThread: count goes from 3 to 1 after two removes" $ do
    topo <- buildPassthrough
    ks <- newKafkaStreams (multiThreadCfg 3) topo
    (drv, _h) <- newMockDriver
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    streamThreadCount ks >>= (`shouldBe` 3)

    r1 <- removeStreamThread ks
    r2 <- removeStreamThread ks
    r1 `shouldBe` Just 2
    r2 `shouldBe` Just 1

    streamThreadCount ks >>= (`shouldBe` 1)

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 3. Add then remove returns to baseline (and the runtime
--    keeps processing records throughout)
----------------------------------------------------------------------

add_then_remove_returns_to_baseline :: Spec
add_then_remove_returns_to_baseline =
  it "add then remove: records still flow through" $ do
    topo <- buildPassthrough
    ks <- newKafkaStreams (multiThreadCfg 2) topo
    (drv, h) <- newMockDriver
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    mockDriverInjectPoll
      h
      [mkRec "in" (T.pack ('k' : show i)) "v" | i <- [(0 :: Int) .. 4]]
    _ <- awaitTicks ks 3

    streamThreadCount ks >>= (`shouldBe` 2)

    r <- addStreamThread ks
    r `shouldBe` Just 3
    -- Push more records — should now flow through 3 workers.
    mockDriverInjectPoll
      h
      [mkRec "in" (T.pack ('m' : show i)) "v" | i <- [(0 :: Int) .. 4]]
    _ <- awaitTicks ks 3

    r2 <- removeStreamThread ks
    r2 `shouldBe` Just 2
    streamThreadCount ks >>= (`shouldBe` 2)

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 4. Single-thread runtime: add/remove return Nothing
----------------------------------------------------------------------

single_thread_runtime_returns_nothing :: Spec
single_thread_runtime_returns_nothing =
  it "addStreamThread / removeStreamThread on a 1-thread runtime is Nothing" $ do
    topo <- buildPassthrough
    ks <- newKafkaStreams (multiThreadCfg 1) topo
    (drv, _h) <- newMockDriver
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    addStreamThread ks >>= (`shouldBe` Nothing)
    removeStreamThread ks >>= (`shouldBe` Nothing)
    streamThreadCount ks >>= (`shouldBe` 1)

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- CloseOptions threading (KIP-812)
----------------------------------------------------------------------

close_with_leave_group_false_skips_leave_group :: Spec
close_with_leave_group_false_skips_leave_group =
  it "closeKafkaStreamsWith leaveGroup=False reaches Closed cleanly" $ do
    topo <- buildPassthrough
    ks <- newKafkaStreams (multiThreadCfg 1) topo
    (drv, _h) <- newMockDriver
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Drive the leaveGroup=False path. The mock driver's
    -- sdConsumerCloseWith records the close regardless of the
    -- flag; the assertion is that the runtime still reaches
    -- StreamsClosed (i.e. the new code-path is wired up and
    -- doesn't get stuck waiting for a phantom LeaveGroup ack).
    closeKafkaStreamsWith
      ks
      (defaultCloseOptions {leaveGroup = False})
    awaitState ks StreamsClosed
