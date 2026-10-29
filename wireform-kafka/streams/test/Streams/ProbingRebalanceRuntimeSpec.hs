{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | KIP-441 probing-rebalance: runtime-side wiring.

Verifies that the streams runtime invokes
'sdRequestProbingRebalance' on the mock driver when:

  * the probing interval has elapsed since the last probe,
  * AND at least one warmup replica is "ready" (lag <=
    acceptable.recovery.lag).

The driver's count is observable via 'mockDriverProbeRequests'.
-}
module Streams.ProbingRebalanceRuntimeSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Streams.Imperative
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.NativeDriver
import Test.Syd


tests :: Spec
tests =
  describe "Probing rebalance runtime wiring (KIP-441)" $
    sequence_
      [ ready_warmup_with_zero_interval_does_not_probe
      , no_warmups_means_no_probe
      , ready_warmup_with_short_interval_eventually_probes
      ]


mkRec :: Text -> Text -> KC.ConsumerRecord
mkRec k v =
  KC.ConsumerRecord
    { topic = "in"
    , partition = 0
    , offset = 0
    , timestamp = 0
    , key = Just (BSC.pack (T.unpack k))
    , value = Just (BSC.pack (T.unpack v))
    , headers = []
    }


buildPassthroughTopo :: IO TopologyValid
buildPassthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


cfgWith :: Int -> StreamsConfig
cfgWith intervalMs =
  defaultStreamsConfig
    { applicationId = "probe-runtime"
    , bootstrapServers = ["mock:0"]
    , numStreamThreads = 1
    , pollMs = 0
    , probingRebalanceIntervalMs = intervalMs
    , acceptableRecoveryLag = 100
    }


----------------------------------------------------------------------
-- 1. interval = 0 disables probing
----------------------------------------------------------------------

ready_warmup_with_zero_interval_does_not_probe :: Spec
ready_warmup_with_zero_interval_does_not_probe =
  it "probingRebalanceIntervalMs=0 disables probing even when warmups are ready" $ do
    topo <- buildPassthroughTopo
    ks <- newKafkaStreams (cfgWith 0) topo
    (drv, h) <- newMockDriver
    -- Mark a warmup as fully caught up.
    reportWarmupLag ks (TaskId 0 0) 0
    mockDriverInjectPoll h [mkRec "k" "v"]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning
    _ <- awaitTicks ks 3
    probes <- mockDriverProbeRequests h
    probes `shouldBe` 0
    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 2. No warmups -> no probe
----------------------------------------------------------------------

no_warmups_means_no_probe :: Spec
no_warmups_means_no_probe =
  it "no registered warmups: probe never fires" $ do
    topo <- buildPassthroughTopo
    ks <- newKafkaStreams (cfgWith 1) topo -- 1 ms — trivially elapsed
    (drv, h) <- newMockDriver
    mockDriverInjectPoll h [mkRec "k" "v"]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning
    _ <- awaitTicks ks 5
    probes <- mockDriverProbeRequests h
    probes `shouldBe` 0
    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 3. Ready warmup + short interval -> at least one probe
----------------------------------------------------------------------

ready_warmup_with_short_interval_eventually_probes :: Spec
ready_warmup_with_short_interval_eventually_probes =
  it "ready warmup + short interval: probe fires at least once" $ do
    topo <- buildPassthroughTopo
    ks <- newKafkaStreams (cfgWith 1) topo
    (drv, h) <- newMockDriver
    -- Mark task (0, 0) as caught up (lag <= acceptableRecoveryLag).
    reportWarmupLag ks (TaskId 0 0) 0
    -- Drive a few poll cycles so the event-loop runs the probe
    -- check multiple times.
    mockDriverInjectPoll h [mkRec "k1" "v"]
    mockDriverInjectPoll h [mkRec "k2" "v"]
    mockDriverInjectPoll h [mkRec "k3" "v"]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning
    _ <- awaitTicks ks 5
    probes <- mockDriverProbeRequests h
    (if (probes >= 1) then pure () else expectationFailure ("expected >= 1 probe; got " <> show probes))
    closeKafkaStreams ks
    awaitState ks StreamsClosed
