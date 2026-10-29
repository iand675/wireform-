{-# LANGUAGE OverloadedStrings #-}

{- | The native driver's admin operations against a live broker: topic
creation and description, listing (pattern sources) and partition
racks (rack-aware assignment). With @WIREFORM_KAFKA_RACKS@ set to the
cluster's racks (comma-separated), every partition's replicas must be
on those racks and none may come back without one.
-}
module Streams.Integration.NativeDriver (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.Driver (NewInternalTopic (..), StreamDriver (..))
import Kafka.Streams.Runtime.NativeDriver (newNativeDriver)
import System.Environment (lookupEnv)
import Test.Syd


tests :: String -> Spec
tests brokers =
  describe "NativeDriver" $
    it "creates, lists and describes topics, and reads partition racks" $ do
      let topic = "kstreams-it-racks"
          cfg =
            defaultStreamsConfig
              { applicationId = "kstreams-it-native"
              , bootstrapServers = [T.pack brokers]
              , clientId = "kstreams-it-native"
              }
      drv <- newNativeDriver cfg >>= either (expectationFailure . ("driver: " <>)) pure
      sdCreateTopics drv [NewInternalTopic topic 2 (-1) []] >>= (`shouldBe` Right ())
      sdDescribeTopics drv [topic] >>= (`shouldBe` Right (Map.singleton topic 2))
      listed <- sdListTopics drv
      fmap (topic `elem`) listed `shouldBe` Right True
      let tps = map (KC.TopicPartition topic) [0, 1]
      racks <- sdPartitionRacks drv tps >>= either (expectationFailure . ("racks: " <>)) pure
      Map.keysSet racks `shouldBe` Set.fromList tps
      expected <- lookupEnv "WIREFORM_KAFKA_RACKS"
      case expected of
        Nothing -> pure ()
        Just rs -> do
          let known = Set.fromList (map T.strip (T.splitOn "," (T.pack rs)))
          mapM_ (\r -> do Set.null r `shouldBe` False; r `shouldSatisfy` (`Set.isSubsetOf` known)) (Map.elems racks)
      sdClose drv
