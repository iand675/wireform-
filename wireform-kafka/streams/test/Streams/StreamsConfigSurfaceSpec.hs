{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the new fields on 'StreamsConfig' added to honour
the JVM client's full configuration surface (KIP-892 +
KIP-441 family).
-}
module Streams.StreamsConfigSurfaceSpec (tests) where

import Data.Map.Strict qualified as Map
import Kafka.Streams.Config qualified as C
import Test.Syd


tests :: Spec
tests =
  describe "StreamsConfig: new full-surface fields" $
    sequence_
      [ it
          "defaults match the JVM 3.x defaults"
          defaults_match
      , it
          "commit.interval.ms and buffered.records.per.partition are honoured"
          commit_and_buffer_parsed
      , it
          "acceptable.recovery.lag is honoured"
          arl_parsed
      , it
          "max.warmup.replicas is honoured"
          mwr_parsed
      , it
          "probing.rebalance.interval.ms is honoured"
          pri_parsed
      , it
          "the exactly-once commit interval defaults to 100 ms"
          eos_commit_default
      ]


defaults_match :: IO ()
defaults_match = do
  C.acceptableRecoveryLag C.defaultStreamsConfig `shouldBe` 10_000
  C.maxWarmupReplicas C.defaultStreamsConfig `shouldBe` 2
  C.probingRebalanceIntervalMs C.defaultStreamsConfig `shouldBe` 600_000
  C.bufferedRecordsPerPartition C.defaultStreamsConfig `shouldBe` 1000
  C.effectiveCommitIntervalMs C.defaultStreamsConfig `shouldBe` 30_000


commit_and_buffer_parsed :: IO ()
commit_and_buffer_parsed = do
  let cfg = C.streamsConfigFromMap (Map.fromList [("commit.interval.ms", "250"), ("buffered.records.per.partition", "64")])
  C.commitIntervalMs cfg `shouldBe` Just 250
  C.bufferedRecordsPerPartition cfg `shouldBe` 64


arl_parsed :: IO ()
arl_parsed =
  C.acceptableRecoveryLag
    (C.streamsConfigFromMap (Map.singleton "acceptable.recovery.lag" "5000"))
    `shouldBe` 5_000


mwr_parsed :: IO ()
mwr_parsed =
  C.maxWarmupReplicas
    (C.streamsConfigFromMap (Map.singleton "max.warmup.replicas" "8"))
    `shouldBe` 8


pri_parsed :: IO ()
pri_parsed =
  C.probingRebalanceIntervalMs
    (C.streamsConfigFromMap (Map.singleton "probing.rebalance.interval.ms" "30000"))
    `shouldBe` 30_000


eos_commit_default :: IO ()
eos_commit_default = do
  C.effectiveCommitIntervalMs C.defaultStreamsConfig {C.processingGuarantee = C.ExactlyOnceV2} `shouldBe` 100
  C.effectiveCommitIntervalMs C.defaultStreamsConfig {C.processingGuarantee = C.ExactlyOnceV2, C.commitIntervalMs = Just 7}
    `shouldBe` 7
