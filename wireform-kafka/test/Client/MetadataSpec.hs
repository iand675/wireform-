{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Client.MetadataSpec (tests) where

import Control.Concurrent.STM
import Control.Monad (replicateM)
import Data.HashMap.Strict qualified as Map
import Data.Int
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.Metadata
import Kafka.Network.Connection (BrokerAddress (..))
import Test.Syd
import Test.Syd.Hedgehog ()
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P


-- | Generate a broker metadata
genBrokerMetadata :: Gen BrokerMetadata
genBrokerMetadata = do
  nodeId <- Gen.int32 (Range.linear 0 10)
  host <- Gen.string (Range.linear 5 15) Gen.alphaNum
  port <- Gen.integral (Range.linear 9000 9999)
  return $ BrokerMetadata nodeId (BrokerAddress host port)


-- | Generate partition metadata
genPartitionMetadata :: Gen PartitionMetadata
genPartitionMetadata = do
  partId <- Gen.int32 (Range.linear 0 10)
  leader <- Gen.int32 (Range.linear 0 5)
  numReplicas <- Gen.int (Range.linear 1 3)
  replicas <- replicateM numReplicas $ Gen.int32 (Range.linear 0 5)
  numIsrs <- Gen.int (Range.linear 1 numReplicas)
  isrs <- take numIsrs <$> Gen.shuffle replicas
  return $ PartitionMetadata partId leader replicas isrs


-- | Generate topic metadata
genTopicMetadata :: Gen TopicMetadata
genTopicMetadata = do
  name <- Gen.text (Range.linear 5 20) Gen.alphaNum
  numPartitions <- Gen.int (Range.linear 1 5)
  partitions <- replicateM numPartitions genPartitionMetadata
  let partMap =
        IntMap.fromList $
          map (\p -> (fromIntegral (partitionMetaId p), p)) partitions
  errorCode <- Gen.int16 (Range.linear 0 10)
  isInternal <- Gen.bool
  return $ TopicMetadata name partMap errorCode isInternal P.nullUuid


-- | Generate cluster metadata
genClusterMetadata :: Gen ClusterMetadata
genClusterMetadata = do
  numBrokers <- Gen.int (Range.linear 1 5)
  brokers <- replicateM numBrokers genBrokerMetadata
  let brokerMap =
        IntMap.fromList $
          map (\b -> (fromIntegral (brokerMetaNodeId b), b)) brokers

  numTopics <- Gen.int (Range.linear 1 3)
  topics <- replicateM numTopics genTopicMetadata
  let topicMap = Map.fromList $ map (\t -> (topicMetaName t, t)) topics

  controllerId <- Gen.int32 (Range.linear 0 (fromIntegral numBrokers - 1))
  -- KIP-78 cluster id; sometimes Nothing to exercise both paths.
  cId <- Gen.maybe (Gen.text (Range.linear 8 16) Gen.alphaNum)
  return $ ClusterMetadata brokerMap topicMap controllerId cId


-- | Test creating an empty metadata cache
unit_createMetadataCache :: IO ()
unit_createMetadataCache = do
  cache <- createMetadataCache

  -- Query should return Nothing for empty cache
  result <- atomically $ getPartitionLeader cache "test-topic" 0
  result `shouldBe` Nothing


-- | Test querying partition leader from empty cache returns Nothing
prop_getPartitionLeaderEmpty :: Property
prop_getPartitionLeaderEmpty = property $ do
  cache <- evalIO createMetadataCache
  topic <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
  partition <- forAll $ Gen.int32 (Range.linear 0 10)

  -- Query empty cache
  leaderM <- evalIO $ atomically $ getPartitionLeader cache topic partition
  leaderM === Nothing


-- | Test querying for non-existent topic partition
prop_getNonExistentTopicPartition :: Property
prop_getNonExistentTopicPartition = property $ do
  cache <- evalIO createMetadataCache
  topic <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
  partition <- forAll $ Gen.int32 (Range.linear 0 10)

  -- Query for non-existent topic-partition
  leaderM <- evalIO $ atomically $ getPartitionLeader cache topic partition
  leaderM === Nothing


-- | Test getting topic partitions from empty cache
prop_getTopicPartitionsEmpty :: Property
prop_getTopicPartitionsEmpty = property $ do
  cache <- evalIO createMetadataCache
  topic <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum

  -- Query empty cache
  partsM <- evalIO $ atomically $ getTopicPartitions cache topic
  partsM === Nothing


-- | Test getting all brokers from empty cache
prop_getAllBrokersEmpty :: Property
prop_getAllBrokersEmpty = property $ do
  cache <- evalIO createMetadataCache

  -- Query empty cache
  brokersM <- evalIO $ atomically $ getAllBrokers cache
  brokersM === Nothing


-- Note: More comprehensive tests would require exposing a way to populate the cache
-- or implementing refreshMetadata functionality fully

-- | Test that empty cache returns Nothing for all queries
prop_emptyCacheReturnsNothing :: Property
prop_emptyCacheReturnsNothing = property $ do
  cache <- evalIO createMetadataCache
  topic <- forAll $ Gen.text (Range.linear 1 10) Gen.alphaNum
  partition <- forAll $ Gen.int32 (Range.linear 0 10)

  -- All queries should return Nothing
  leaderM <- evalIO $ atomically $ getPartitionLeader cache topic partition
  partsM <- evalIO $ atomically $ getTopicPartitions cache topic
  brokersM <- evalIO $ atomically $ getAllBrokers cache

  leaderM === Nothing
  partsM === Nothing
  brokersM === Nothing


-- | All tests for metadata caching
tests :: Spec
tests =
  describe "Metadata" $
    sequence_
      [ describe "applyMetadataResponse" $
          sequence_
            [ it "a refresh for some topics keeps the others, the response's topics winning" prop_scopedRefreshMerges
            , it "a refresh for all topics replaces the cache" prop_fullRefreshReplaces
            ]
      , describe "Metadata Cache" $
          sequence_
            [ it "Create empty cache" unit_createMetadataCache
            , it "Empty cache returns Nothing" prop_emptyCacheReturnsNothing
            ]
      , describe "Partition Leader Lookup" $
          sequence_
            [ it "Get partition leader from empty cache" prop_getPartitionLeaderEmpty
            , it "Get topic partition from empty cache" prop_getNonExistentTopicPartition
            ]
      , describe "Topic and Broker Queries" $
          sequence_
            [ it "Get topic partitions from empty cache" prop_getTopicPartitionsEmpty
            , it "Get all brokers from empty cache" prop_getAllBrokersEmpty
            ]
      ]


prop_scopedRefreshMerges :: Property
prop_scopedRefreshMerges = property $ do
  old <- forAll genClusterMetadata
  new <- forAll genClusterMetadata
  let got = applyMetadataResponse True new (Just old)
  clusterBrokers got === clusterBrokers new
  clusterControllerId got === clusterControllerId new
  -- every topic the response carries, as it carries it
  Map.intersection (clusterTopics got) (clusterTopics new) === clusterTopics new
  -- and every other topic the cache had, untouched
  Map.difference (clusterTopics got) (clusterTopics new) === Map.difference (clusterTopics old) (clusterTopics new)


prop_fullRefreshReplaces :: Property
prop_fullRefreshReplaces = property $ do
  old <- forAll genClusterMetadata
  new <- forAll genClusterMetadata
  applyMetadataResponse False new (Just old) === new
  applyMetadataResponse True new Nothing === new
