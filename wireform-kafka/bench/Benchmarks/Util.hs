{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{- |
Module      : Benchmarks.Util
Description : Shared utilities for benchmarking
Copyright   : (c) 2025
License     : BSD-3-Clause

This module provides helper functions for generating test data and
constructing protocol messages for benchmarking purposes.
-}
module Benchmarks.Util (
  -- * Test Data Generation
  mkBenchData,
  randomishBytes,

  -- * Protocol Message Generators
  createProduceRequest,
  createFetchRequest,
  createMetadataRequest,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FetchRequest qualified as Fetch
import "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest qualified as Metadata
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ProduceRequest qualified as Produce
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P


-- -----------------------------------------------------------------------------
-- Test Data Generation
-- -----------------------------------------------------------------------------

{- | Generate deterministic test data of a given size.
Uses a repeating pattern to ensure consistent memory usage and cache
behavior. The previous implementation used
@BS.take n (BS.concat (repeat pattern))@ which tries to materialise
an infinite list of bytestrings before truncating — that's a textbook
accidental @O(infinity)@ heap blowup; with the criterion harness it
pinned a CPU at 1 TiB virtual memory and never returned.
-}
mkBenchData :: Int -> ByteString
mkBenchData n
  | n <= 0 = BS.empty
  | otherwise = BS.take n (BS.concat (replicate copies pat))
  where
    pat = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#$%^&*()-_=+[]{}|;:,.<>?/~`"
    pLen = BS.length pat
    copies = (n + pLen - 1) `quot` pLen


{- | Generate pseudo-random bytes of a given size.
Not cryptographically secure, but good enough for benchmarking.
Uses a deterministic pattern seeded by position to avoid actual randomness overhead.
-}
randomishBytes :: Int -> ByteString
randomishBytes n = BS.pack $ take n $ cycle $ map fromIntegral [0 .. 255 :: Int]


-- -----------------------------------------------------------------------------
-- Protocol Message Generators
-- -----------------------------------------------------------------------------

{- | Create a ProduceRequest with the specified number of topics and partitions per topic.
Generates realistic but minimal data for benchmarking.
-}
createProduceRequest :: Int -> Int -> Produce.ProduceRequest
createProduceRequest numTopics numPartitions =
  Produce.ProduceRequest
    { Produce.produceRequestTransactionalId = P.mkKafkaString ""
    , Produce.produceRequestAcks = 1
    , Produce.produceRequestTimeoutMs = 30000
    , Produce.produceRequestTopicData = P.mkKafkaArray $ V.fromList $ map mkTopicData [1 .. numTopics]
    }
  where
    mkTopicData :: Int -> Produce.TopicProduceData
    mkTopicData topicNum =
      Produce.TopicProduceData
        { Produce.topicProduceDataName = mkTopicName topicNum
        , Produce.topicProduceDataTopicId = P.nullUuid
        , Produce.topicProduceDataPartitionData = P.mkKafkaArray $ V.fromList $ map mkPartitionData [0 .. numPartitions - 1]
        }

    mkPartitionData :: Int -> Produce.PartitionProduceData
    mkPartitionData partNum =
      Produce.PartitionProduceData
        { Produce.partitionProduceDataIndex = fromIntegral partNum
        , -- Empty records for now - in real benchmarks we'd add RecordBatch data
          Produce.partitionProduceDataRecords = P.mkKafkaBytes BS.empty
        }

    mkTopicName :: Int -> P.KafkaString
    mkTopicName n = P.mkKafkaString $ T.pack $ "benchmark-topic-" ++ show n


-- | Create a FetchRequest with the specified number of topics and partitions per topic.
createFetchRequest :: Int -> Int -> Fetch.FetchRequest
createFetchRequest numTopics numPartitions =
  Fetch.FetchRequest
    { Fetch.fetchRequestClusterId = P.mkKafkaString ""
    , Fetch.fetchRequestReplicaId = -1
    , Fetch.fetchRequestReplicaState =
        Fetch.ReplicaState
          { Fetch.replicaStateReplicaId = -1
          , Fetch.replicaStateReplicaEpoch = -1
          }
    , Fetch.fetchRequestMaxWaitMs = 500
    , Fetch.fetchRequestMinBytes = 1
    , Fetch.fetchRequestMaxBytes = 52428800
    , Fetch.fetchRequestIsolationLevel = 0
    , Fetch.fetchRequestSessionId = 0
    , Fetch.fetchRequestSessionEpoch = -1
    , Fetch.fetchRequestTopics = P.mkKafkaArray $ V.fromList $ map mkTopic [1 .. numTopics]
    , Fetch.fetchRequestForgottenTopicsData = P.mkKafkaArray V.empty
    , Fetch.fetchRequestRackId = P.mkKafkaString ""
    }
  where
    mkTopic :: Int -> Fetch.FetchTopic
    mkTopic topicNum =
      Fetch.FetchTopic
        { Fetch.fetchTopicTopic = mkTopicName topicNum
        , Fetch.fetchTopicTopicId = P.nullUuid
        , Fetch.fetchTopicPartitions = P.mkKafkaArray $ V.fromList $ map mkPartition [0 .. numPartitions - 1]
        }

    mkPartition :: Int -> Fetch.FetchPartition
    mkPartition partNum =
      Fetch.FetchPartition
        { Fetch.fetchPartitionPartition = fromIntegral partNum
        , Fetch.fetchPartitionCurrentLeaderEpoch = -1
        , Fetch.fetchPartitionFetchOffset = 0
        , Fetch.fetchPartitionLastFetchedEpoch = -1
        , Fetch.fetchPartitionLogStartOffset = -1
        , Fetch.fetchPartitionHighWatermark = maxBound
        , Fetch.fetchPartitionPartitionMaxBytes = 1048576
        , Fetch.fetchPartitionReplicaDirectoryId = P.nullUuid
        }

    mkTopicName :: Int -> P.KafkaString
    mkTopicName n = P.mkKafkaString $ T.pack $ "benchmark-topic-" ++ show n


-- | Create a MetadataRequest with the specified number of topics.
createMetadataRequest :: Int -> Metadata.MetadataRequest
createMetadataRequest numTopics =
  Metadata.MetadataRequest
    { Metadata.metadataRequestTopics =
        if numTopics == 0
          then P.mkKafkaArray V.empty -- Request metadata for all topics
          else P.mkKafkaArray $ V.fromList $ map mkTopic [1 .. numTopics]
    , Metadata.metadataRequestAllowAutoTopicCreation = False
    , Metadata.metadataRequestIncludeClusterAuthorizedOperations = False
    , Metadata.metadataRequestIncludeTopicAuthorizedOperations = False
    }
  where
    mkTopic :: Int -> Metadata.MetadataRequestTopic
    mkTopic topicNum =
      Metadata.MetadataRequestTopic
        { Metadata.metadataRequestTopicTopicId = P.nullUuid
        , Metadata.metadataRequestTopicName = mkTopicName topicNum
        }

    mkTopicName :: Int -> P.KafkaString
    mkTopicName n = P.mkKafkaString $ T.pack $ "benchmark-topic-" ++ show n
