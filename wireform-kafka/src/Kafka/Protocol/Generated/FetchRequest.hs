{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchRequest
Description : Kafka FetchRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 1.



Valid versions: 4-19
Flexible versions: 12+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchRequest
  (
    FetchRequest(..),
    ReplicaState(..),
    FetchTopic(..),
    FetchPartition(..),
    ForgottenTopic(..),
    maxFetchRequestVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The state of the replica in the follower.
data ReplicaState = ReplicaState
  {

  -- | The replica ID of the follower, or -1 if this request is from a consumer.

  -- Versions: 15+
  replicaStateReplicaId :: !(Int32)
,

  -- | The epoch of this follower, or -1 if not available.

  -- Versions: 15+
  replicaStateReplicaEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | The partitions to fetch.
data FetchPartition = FetchPartition
  {

  -- | The partition index.

  -- Versions: 0+
  fetchPartitionPartition :: !(Int32)
,

  -- | The current leader epoch of the partition.

  -- Versions: 9+
  fetchPartitionCurrentLeaderEpoch :: !(Int32)
,

  -- | The message offset.

  -- Versions: 0+
  fetchPartitionFetchOffset :: !(Int64)
,

  -- | The epoch of the last fetched record or -1 if there is none.

  -- Versions: 12+
  fetchPartitionLastFetchedEpoch :: !(Int32)
,

  -- | The earliest available offset of the follower replica.  The field is only used when the request is s

  -- Versions: 5+
  fetchPartitionLogStartOffset :: !(Int64)
,

  -- | The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be ho

  -- Versions: 0+
  fetchPartitionPartitionMaxBytes :: !(Int32)
,

  -- | The directory id of the follower fetching.

  -- Versions: 17+
  fetchPartitionReplicaDirectoryId :: !(KafkaUuid)
,

  -- | The high-watermark known by the replica. -1 if the high-watermark is not known and 92233720368547758

  -- Versions: 18+
  fetchPartitionHighWatermark :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | The topics to fetch.
data FetchTopic = FetchTopic
  {

  -- | The name of the topic to fetch.

  -- Versions: 0-12
  fetchTopicTopic :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 13+
  fetchTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions to fetch.

  -- Versions: 0+
  fetchTopicPartitions :: !(KafkaArray (FetchPartition))

  }
  deriving (Eq, Show, Generic)

-- | In an incremental fetch request, the partitions to remove.
data ForgottenTopic = ForgottenTopic
  {

  -- | The topic name.

  -- Versions: 7-12
  forgottenTopicTopic :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 13+
  forgottenTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions indexes to forget.

  -- Versions: 7+
  forgottenTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


data FetchRequest = FetchRequest
  {

  -- | The clusterId if known. This is used to validate metadata fetches prior to broker registration.

  -- Versions: 12+
  fetchRequestClusterId :: !(KafkaString)
,

  -- | The broker ID of the follower, of -1 if this request is from a consumer.

  -- Versions: 0-14
  fetchRequestReplicaId :: !(Int32)
,

  -- | The state of the replica in the follower.

  -- Versions: 15+
  fetchRequestReplicaState :: !(ReplicaState)
,

  -- | The maximum time in milliseconds to wait for the response.

  -- Versions: 0+
  fetchRequestMaxWaitMs :: !(Int32)
,

  -- | The minimum bytes to accumulate in the response.

  -- Versions: 0+
  fetchRequestMinBytes :: !(Int32)
,

  -- | The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.

  -- Versions: 3+
  fetchRequestMaxBytes :: !(Int32)
,

  -- | This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_lev

  -- Versions: 4+
  fetchRequestIsolationLevel :: !(Int8)
,

  -- | The fetch session ID.

  -- Versions: 7+
  fetchRequestSessionId :: !(Int32)
,

  -- | The fetch session epoch, which is used for ordering requests in a session.

  -- Versions: 7+
  fetchRequestSessionEpoch :: !(Int32)
,

  -- | The topics to fetch.

  -- Versions: 0+
  fetchRequestTopics :: !(KafkaArray (FetchTopic))
,

  -- | In an incremental fetch request, the partitions to remove.

  -- Versions: 7+
  fetchRequestForgottenTopicsData :: !(KafkaArray (ForgottenTopic))
,

  -- | Rack ID of the consumer making this request.

  -- Versions: 11+
  fetchRequestRackId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchRequest.
maxFetchRequestVersion :: Int16
maxFetchRequestVersion = 19

-- | KafkaMessage instance for FetchRequest.
instance KafkaMessage FetchRequest where
  messageApiKey = 1
  messageMinVersion = 4
  messageMaxVersion = 19
  messageFlexibleVersion = Just 12

-- | Worst-case wire size of a ReplicaState.
wireMaxSizeReplicaState :: Int -> ReplicaState -> Int
wireMaxSizeReplicaState _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for ReplicaState.
wirePokeReplicaState :: Int -> Ptr Word8 -> ReplicaState -> IO (Ptr Word8)
wirePokeReplicaState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 15 then W.pokeInt32BE p0 (replicaStateReplicaId msg) else pure p0)
  p2 <- (if version >= 15 then W.pokeInt64BE p1 (replicaStateReplicaEpoch msg) else pure p1)
  if version >= 12 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReplicaState.
wirePeekReplicaState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaState, Ptr Word8)
wirePeekReplicaState version _fp _basePtr p0 endPtr = do
  (f0_replicaid, p1) <- (if version >= 15 then W.peekInt32BE p0 endPtr else pure (-1, p0))
  (f1_replicaepoch, p2) <- (if version >= 15 then W.peekInt64BE p1 endPtr else pure (-1, p1))
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReplicaState { replicaStateReplicaId = f0_replicaid, replicaStateReplicaEpoch = f1_replicaepoch }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultReplicaState :: ReplicaState
defaultReplicaState = ReplicaState { replicaStateReplicaId = -1, replicaStateReplicaEpoch = -1 }

-- | Worst-case wire size of a FetchPartition.
wireMaxSizeFetchPartition :: Int -> FetchPartition -> Int
wireMaxSizeFetchPartition _version msg =
  0
  + 4
  + 4
  + 8
  + 4
  + 8
  + 4
  + (if _version >= 17 && (fetchPartitionReplicaDirectoryId msg) /= P.nullUuid then 16 + 10 else 0)
  + (if _version >= 18 then 8 + 10 else 0)
  + 1

-- | Direct-poke encoder for FetchPartition.
wirePokeFetchPartition :: Int -> Ptr Word8 -> FetchPartition -> IO (Ptr Word8)
wirePokeFetchPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (fetchPartitionPartition msg)
  p2 <- (if version >= 9 then W.pokeInt32BE p1 (fetchPartitionCurrentLeaderEpoch msg) else pure p1)
  p3 <- W.pokeInt64BE p2 (fetchPartitionFetchOffset msg)
  p4 <- (if version >= 12 then W.pokeInt32BE p3 (fetchPartitionLastFetchedEpoch msg) else pure p3)
  p5 <- (if version >= 5 then W.pokeInt64BE p4 (fetchPartitionLogStartOffset msg) else pure p4)
  p6 <- W.pokeInt32BE p5 (fetchPartitionPartitionMaxBytes msg)
  if version >= 12 then do
    let !_taggedEntries = (if version >= 17 && (fetchPartitionReplicaDirectoryId msg) /= P.nullUuid then [(0, W.runWirePut (fetchPartitionReplicaDirectoryId msg))] else []) ++ (if version >= 18 then [(1, W.runWirePut (fetchPartitionHighWatermark msg))] else [])
    WP.pokeTaggedFieldEntries p6 _taggedEntries
  else pure p6

-- | Direct-poke decoder for FetchPartition.
wirePeekFetchPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchPartition, Ptr Word8)
wirePeekFetchPartition version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_currentleaderepoch, p2) <- (if version >= 9 then W.peekInt32BE p1 endPtr else pure (-1, p1))
  (f2_fetchoffset, p3) <- W.peekInt64BE p2 endPtr
  (f3_lastfetchedepoch, p4) <- (if version >= 12 then W.peekInt32BE p3 endPtr else pure (-1, p3))
  (f4_logstartoffset, p5) <- (if version >= 5 then W.peekInt64BE p4 endPtr else pure (-1, p4))
  (f5_partitionmaxbytes, p6) <- W.peekInt32BE p5 endPtr
  (_taggedMap, pTagsEnd) <- if version >= 12 then WP.peekTaggedFieldsMap p6 endPtr else pure (Data.Map.Strict.empty, p6)
  let !_tag_replicadirectoryid = if version >= 17 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGet :: Data.ByteString.ByteString -> Either String P.KafkaUuid) _bs of { Right _v -> _v ; Left _ -> P.nullUuid}; Nothing -> P.nullUuid} else P.nullUuid
  let !_tag_highwatermark = if version >= 18 then case Data.Map.Strict.lookup 1 _taggedMap of { Just _bs -> case (W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Int.Int64) _bs of { Right _v -> _v ; Left _ -> 9223372036854775807}; Nothing -> 9223372036854775807} else 9223372036854775807
  pure (FetchPartition { fetchPartitionPartition = f0_partition, fetchPartitionCurrentLeaderEpoch = f1_currentleaderepoch, fetchPartitionFetchOffset = f2_fetchoffset, fetchPartitionLastFetchedEpoch = f3_lastfetchedepoch, fetchPartitionLogStartOffset = f4_logstartoffset, fetchPartitionPartitionMaxBytes = f5_partitionmaxbytes, fetchPartitionReplicaDirectoryId = _tag_replicadirectoryid, fetchPartitionHighWatermark = _tag_highwatermark }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultFetchPartition :: FetchPartition
defaultFetchPartition = FetchPartition { fetchPartitionPartition = 0, fetchPartitionCurrentLeaderEpoch = -1, fetchPartitionFetchOffset = 0, fetchPartitionLastFetchedEpoch = -1, fetchPartitionLogStartOffset = -1, fetchPartitionPartitionMaxBytes = 0, fetchPartitionReplicaDirectoryId = P.nullUuid, fetchPartitionHighWatermark = 9223372036854775807 }

-- | Worst-case wire size of a FetchTopic.
wireMaxSizeFetchTopic :: Int -> FetchTopic -> Int
wireMaxSizeFetchTopic _version msg =
  0
  + WP.dualStringMaxSize (fetchTopicTopic msg)
  + 16
  + (5 + (case P.unKafkaArray (fetchTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFetchPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FetchTopic.
wirePokeFetchTopic :: Int -> Ptr Word8 -> FetchTopic -> IO (Ptr Word8)
wirePokeFetchTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 12 then (if version >= 12 then WP.pokeCompactString p0 (P.toCompactString (fetchTopicTopic msg)) else WP.pokeKafkaString p0 (fetchTopicTopic msg)) else pure p0)
  p2 <- (if version >= 13 then WP.pokeKafkaUuid p1 (fetchTopicTopicId msg) else pure p1)
  p3 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchPartition version p x) p2 (fetchTopicPartitions msg)
  if version >= 12 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for FetchTopic.
wirePeekFetchTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchTopic, Ptr Word8)
wirePeekFetchTopic version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (if version <= 12 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 13 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitions, p3) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchPartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (FetchTopic { fetchTopicTopic = f0_topic, fetchTopicTopicId = f1_topicid, fetchTopicPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultFetchTopic :: FetchTopic
defaultFetchTopic = FetchTopic { fetchTopicTopic = P.KafkaString Null, fetchTopicTopicId = P.nullUuid, fetchTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ForgottenTopic.
wireMaxSizeForgottenTopic :: Int -> ForgottenTopic -> Int
wireMaxSizeForgottenTopic _version msg =
  0
  + WP.dualStringMaxSize (forgottenTopicTopic msg)
  + 16
  + (5 + (case P.unKafkaArray (forgottenTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ForgottenTopic.
wirePokeForgottenTopic :: Int -> Ptr Word8 -> ForgottenTopic -> IO (Ptr Word8)
wirePokeForgottenTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 7 && version <= 12 then (if version >= 12 then WP.pokeCompactString p0 (P.toCompactString (forgottenTopicTopic msg)) else WP.pokeKafkaString p0 (forgottenTopicTopic msg)) else pure p0)
  p2 <- (if version >= 13 then WP.pokeKafkaUuid p1 (forgottenTopicTopicId msg) else pure p1)
  p3 <- (if version >= 7 then WP.pokeVersionedArray version 12 W.pokeInt32BE p2 (forgottenTopicPartitions msg) else pure p2)
  if version >= 12 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ForgottenTopic.
wirePeekForgottenTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ForgottenTopic, Ptr Word8)
wirePeekForgottenTopic version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (if version >= 7 && version <= 12 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 13 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitions, p3) <- (if version >= 7 then WP.peekVersionedArray version 12 W.peekInt32BE p2 endPtr else pure (P.mkKafkaArray V.empty, p2))
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ForgottenTopic { forgottenTopicTopic = f0_topic, forgottenTopicTopicId = f1_topicid, forgottenTopicPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultForgottenTopic :: ForgottenTopic
defaultForgottenTopic = ForgottenTopic { forgottenTopicTopic = P.KafkaString Null, forgottenTopicTopicId = P.nullUuid, forgottenTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a FetchRequest.
wireMaxSizeFetchRequest :: Int -> FetchRequest -> Int
wireMaxSizeFetchRequest _version msg =
  0
  + (if _version >= 12 then WP.dualStringMaxSize (fetchRequestClusterId msg) + 10 else 0)
  + 4
  + (if _version >= 15 then wireMaxSizeReplicaState _version (fetchRequestReplicaState msg) + 10 else 0)
  + 4
  + 4
  + 4
  + 1
  + 4
  + 4
  + (5 + (case P.unKafkaArray (fetchRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFetchTopic _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (fetchRequestForgottenTopicsData msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeForgottenTopic _version x ) v); P.Null -> 0 }))
  + WP.dualStringMaxSize (fetchRequestRackId msg)
  + 1

-- | Direct-poke encoder for FetchRequest.
wirePokeFetchRequest :: Int -> Ptr Word8 -> FetchRequest -> IO (Ptr Word8)
wirePokeFetchRequest version basePtr msg
  | version == 11 = do
    p0 <- pure basePtr
    p1 <- (if version <= 14 then W.pokeInt32BE p0 (fetchRequestReplicaId msg) else pure p0)
    p2 <- W.pokeInt32BE p1 (fetchRequestMaxWaitMs msg)
    p3 <- W.pokeInt32BE p2 (fetchRequestMinBytes msg)
    p4 <- (if version >= 3 then W.pokeInt32BE p3 (fetchRequestMaxBytes msg) else pure p3)
    p5 <- (if version >= 4 then W.pokeWord8 p4 (fromIntegral (fetchRequestIsolationLevel msg)) else pure p4)
    p6 <- (if version >= 7 then W.pokeInt32BE p5 (fetchRequestSessionId msg) else pure p5)
    p7 <- (if version >= 7 then W.pokeInt32BE p6 (fetchRequestSessionEpoch msg) else pure p6)
    p8 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchTopic version p x) p7 (fetchRequestTopics msg)
    p9 <- (if version >= 7 then WP.pokeVersionedArray version 12 (\p x -> wirePokeForgottenTopic version p x) p8 (fetchRequestForgottenTopicsData msg) else pure p8)
    p10 <- (if version >= 11 then (if version >= 12 then WP.pokeCompactString p9 (P.toCompactString (fetchRequestRackId msg)) else WP.pokeKafkaString p9 (fetchRequestRackId msg)) else pure p9)
    pure p10
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version <= 14 then W.pokeInt32BE p0 (fetchRequestReplicaId msg) else pure p0)
    p2 <- W.pokeInt32BE p1 (fetchRequestMaxWaitMs msg)
    p3 <- W.pokeInt32BE p2 (fetchRequestMinBytes msg)
    p4 <- (if version >= 3 then W.pokeInt32BE p3 (fetchRequestMaxBytes msg) else pure p3)
    p5 <- (if version >= 4 then W.pokeWord8 p4 (fromIntegral (fetchRequestIsolationLevel msg)) else pure p4)
    p6 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchTopic version p x) p5 (fetchRequestTopics msg)
    pure p6
  | version >= 12 && version <= 14 = do
    p0 <- pure basePtr
    p1 <- (if version <= 14 then W.pokeInt32BE p0 (fetchRequestReplicaId msg) else pure p0)
    p2 <- W.pokeInt32BE p1 (fetchRequestMaxWaitMs msg)
    p3 <- W.pokeInt32BE p2 (fetchRequestMinBytes msg)
    p4 <- (if version >= 3 then W.pokeInt32BE p3 (fetchRequestMaxBytes msg) else pure p3)
    p5 <- (if version >= 4 then W.pokeWord8 p4 (fromIntegral (fetchRequestIsolationLevel msg)) else pure p4)
    p6 <- (if version >= 7 then W.pokeInt32BE p5 (fetchRequestSessionId msg) else pure p5)
    p7 <- (if version >= 7 then W.pokeInt32BE p6 (fetchRequestSessionEpoch msg) else pure p6)
    p8 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchTopic version p x) p7 (fetchRequestTopics msg)
    p9 <- (if version >= 7 then WP.pokeVersionedArray version 12 (\p x -> wirePokeForgottenTopic version p x) p8 (fetchRequestForgottenTopicsData msg) else pure p8)
    p10 <- (if version >= 11 then (if version >= 12 then WP.pokeCompactString p9 (P.toCompactString (fetchRequestRackId msg)) else WP.pokeKafkaString p9 (fetchRequestRackId msg)) else pure p9)
    let !_taggedEntries = (if version >= 12 then [(0, W.runWirePut (P.toCompactString (fetchRequestClusterId msg)))] else []) ++ (if version >= 15 then [(1, W.runWirePokeWith (wireMaxSizeReplicaState version (fetchRequestReplicaState msg)) (\p -> wirePokeReplicaState version p (fetchRequestReplicaState msg)))] else [])
    WP.pokeTaggedFieldEntries p10 _taggedEntries
  | version >= 7 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- (if version <= 14 then W.pokeInt32BE p0 (fetchRequestReplicaId msg) else pure p0)
    p2 <- W.pokeInt32BE p1 (fetchRequestMaxWaitMs msg)
    p3 <- W.pokeInt32BE p2 (fetchRequestMinBytes msg)
    p4 <- (if version >= 3 then W.pokeInt32BE p3 (fetchRequestMaxBytes msg) else pure p3)
    p5 <- (if version >= 4 then W.pokeWord8 p4 (fromIntegral (fetchRequestIsolationLevel msg)) else pure p4)
    p6 <- (if version >= 7 then W.pokeInt32BE p5 (fetchRequestSessionId msg) else pure p5)
    p7 <- (if version >= 7 then W.pokeInt32BE p6 (fetchRequestSessionEpoch msg) else pure p6)
    p8 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchTopic version p x) p7 (fetchRequestTopics msg)
    p9 <- (if version >= 7 then WP.pokeVersionedArray version 12 (\p x -> wirePokeForgottenTopic version p x) p8 (fetchRequestForgottenTopicsData msg) else pure p8)
    pure p9
  | version >= 15 && version <= 19 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (fetchRequestMaxWaitMs msg)
    p2 <- W.pokeInt32BE p1 (fetchRequestMinBytes msg)
    p3 <- (if version >= 3 then W.pokeInt32BE p2 (fetchRequestMaxBytes msg) else pure p2)
    p4 <- (if version >= 4 then W.pokeWord8 p3 (fromIntegral (fetchRequestIsolationLevel msg)) else pure p3)
    p5 <- (if version >= 7 then W.pokeInt32BE p4 (fetchRequestSessionId msg) else pure p4)
    p6 <- (if version >= 7 then W.pokeInt32BE p5 (fetchRequestSessionEpoch msg) else pure p5)
    p7 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchTopic version p x) p6 (fetchRequestTopics msg)
    p8 <- (if version >= 7 then WP.pokeVersionedArray version 12 (\p x -> wirePokeForgottenTopic version p x) p7 (fetchRequestForgottenTopicsData msg) else pure p7)
    p9 <- (if version >= 11 then (if version >= 12 then WP.pokeCompactString p8 (P.toCompactString (fetchRequestRackId msg)) else WP.pokeKafkaString p8 (fetchRequestRackId msg)) else pure p8)
    let !_taggedEntries = (if version >= 12 then [(0, W.runWirePut (P.toCompactString (fetchRequestClusterId msg)))] else []) ++ (if version >= 15 then [(1, W.runWirePokeWith (wireMaxSizeReplicaState version (fetchRequestReplicaState msg)) (\p -> wirePokeReplicaState version p (fetchRequestReplicaState msg)))] else [])
    WP.pokeTaggedFieldEntries p9 _taggedEntries
  | otherwise = error $ "wirePoke FetchRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for FetchRequest.
wirePeekFetchRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchRequest, Ptr Word8)
wirePeekFetchRequest version _fp _basePtr p0 endPtr
  | version == 11 = do
    (f0_replicaid, p1) <- (if version <= 14 then W.peekInt32BE p0 endPtr else pure (-1, p0))
    (f1_maxwaitms, p2) <- W.peekInt32BE p1 endPtr
    (f2_minbytes, p3) <- W.peekInt32BE p2 endPtr
    (f3_maxbytes, p4) <- (if version >= 3 then W.peekInt32BE p3 endPtr else pure (2147483647, p3))
    (f4_isolationlevel, p5) <- (if version >= 4 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p4 endPtr else pure (0, p4))
    (f5_sessionid, p6) <- (if version >= 7 then W.peekInt32BE p5 endPtr else pure (0, p5))
    (f6_sessionepoch, p7) <- (if version >= 7 then W.peekInt32BE p6 endPtr else pure (-1, p6))
    (f7_topics, p8) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchTopic version _fp _basePtr p e) p7 endPtr
    (f8_forgottentopicsdata, p9) <- (if version >= 7 then WP.peekVersionedArray version 12 (\p e -> wirePeekForgottenTopic version _fp _basePtr p e) p8 endPtr else pure (P.mkKafkaArray V.empty, p8))
    (f9_rackid, p10) <- (if version >= 11 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p9 endPtr else WP.peekKafkaString p9 endPtr) else pure (P.KafkaString Null, p9))
    pure (FetchRequest { fetchRequestClusterId = P.KafkaString Null, fetchRequestReplicaId = f0_replicaid, fetchRequestReplicaState = defaultReplicaState, fetchRequestMaxWaitMs = f1_maxwaitms, fetchRequestMinBytes = f2_minbytes, fetchRequestMaxBytes = f3_maxbytes, fetchRequestIsolationLevel = f4_isolationlevel, fetchRequestSessionId = f5_sessionid, fetchRequestSessionEpoch = f6_sessionepoch, fetchRequestTopics = f7_topics, fetchRequestForgottenTopicsData = f8_forgottentopicsdata, fetchRequestRackId = f9_rackid }, p10)
  | version >= 4 && version <= 6 = do
    (f0_replicaid, p1) <- (if version <= 14 then W.peekInt32BE p0 endPtr else pure (-1, p0))
    (f1_maxwaitms, p2) <- W.peekInt32BE p1 endPtr
    (f2_minbytes, p3) <- W.peekInt32BE p2 endPtr
    (f3_maxbytes, p4) <- (if version >= 3 then W.peekInt32BE p3 endPtr else pure (2147483647, p3))
    (f4_isolationlevel, p5) <- (if version >= 4 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p4 endPtr else pure (0, p4))
    (f5_topics, p6) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchTopic version _fp _basePtr p e) p5 endPtr
    pure (FetchRequest { fetchRequestClusterId = P.KafkaString Null, fetchRequestReplicaId = f0_replicaid, fetchRequestReplicaState = defaultReplicaState, fetchRequestMaxWaitMs = f1_maxwaitms, fetchRequestMinBytes = f2_minbytes, fetchRequestMaxBytes = f3_maxbytes, fetchRequestIsolationLevel = f4_isolationlevel, fetchRequestSessionId = 0, fetchRequestSessionEpoch = -1, fetchRequestTopics = f5_topics, fetchRequestForgottenTopicsData = P.mkKafkaArray V.empty, fetchRequestRackId = P.KafkaString Null }, p6)
  | version >= 12 && version <= 14 = do
    (f0_replicaid, p1) <- (if version <= 14 then W.peekInt32BE p0 endPtr else pure (-1, p0))
    (f1_maxwaitms, p2) <- W.peekInt32BE p1 endPtr
    (f2_minbytes, p3) <- W.peekInt32BE p2 endPtr
    (f3_maxbytes, p4) <- (if version >= 3 then W.peekInt32BE p3 endPtr else pure (2147483647, p3))
    (f4_isolationlevel, p5) <- (if version >= 4 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p4 endPtr else pure (0, p4))
    (f5_sessionid, p6) <- (if version >= 7 then W.peekInt32BE p5 endPtr else pure (0, p5))
    (f6_sessionepoch, p7) <- (if version >= 7 then W.peekInt32BE p6 endPtr else pure (-1, p6))
    (f7_topics, p8) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchTopic version _fp _basePtr p e) p7 endPtr
    (f8_forgottentopicsdata, p9) <- (if version >= 7 then WP.peekVersionedArray version 12 (\p e -> wirePeekForgottenTopic version _fp _basePtr p e) p8 endPtr else pure (P.mkKafkaArray V.empty, p8))
    (f9_rackid, p10) <- (if version >= 11 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p9 endPtr else WP.peekKafkaString p9 endPtr) else pure (P.KafkaString Null, p9))
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p10 endPtr
    let !_tag_clusterid = if version >= 12 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (\b -> fmap P.fromCompactString ((W.runWireGet :: Data.ByteString.ByteString -> Either String P.CompactString) b)) _bs of { Right _v -> _v ; Left _ -> P.KafkaString Null}; Nothing -> P.KafkaString Null} else P.KafkaString Null
    let !_tag_replicastate = if version >= 15 then case Data.Map.Strict.lookup 1 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekReplicaState version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> defaultReplicaState}; Nothing -> defaultReplicaState} else defaultReplicaState
    pure (FetchRequest { fetchRequestClusterId = _tag_clusterid, fetchRequestReplicaId = f0_replicaid, fetchRequestReplicaState = _tag_replicastate, fetchRequestMaxWaitMs = f1_maxwaitms, fetchRequestMinBytes = f2_minbytes, fetchRequestMaxBytes = f3_maxbytes, fetchRequestIsolationLevel = f4_isolationlevel, fetchRequestSessionId = f5_sessionid, fetchRequestSessionEpoch = f6_sessionepoch, fetchRequestTopics = f7_topics, fetchRequestForgottenTopicsData = f8_forgottentopicsdata, fetchRequestRackId = f9_rackid }, pTagsEnd)
  | version >= 7 && version <= 10 = do
    (f0_replicaid, p1) <- (if version <= 14 then W.peekInt32BE p0 endPtr else pure (-1, p0))
    (f1_maxwaitms, p2) <- W.peekInt32BE p1 endPtr
    (f2_minbytes, p3) <- W.peekInt32BE p2 endPtr
    (f3_maxbytes, p4) <- (if version >= 3 then W.peekInt32BE p3 endPtr else pure (2147483647, p3))
    (f4_isolationlevel, p5) <- (if version >= 4 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p4 endPtr else pure (0, p4))
    (f5_sessionid, p6) <- (if version >= 7 then W.peekInt32BE p5 endPtr else pure (0, p5))
    (f6_sessionepoch, p7) <- (if version >= 7 then W.peekInt32BE p6 endPtr else pure (-1, p6))
    (f7_topics, p8) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchTopic version _fp _basePtr p e) p7 endPtr
    (f8_forgottentopicsdata, p9) <- (if version >= 7 then WP.peekVersionedArray version 12 (\p e -> wirePeekForgottenTopic version _fp _basePtr p e) p8 endPtr else pure (P.mkKafkaArray V.empty, p8))
    pure (FetchRequest { fetchRequestClusterId = P.KafkaString Null, fetchRequestReplicaId = f0_replicaid, fetchRequestReplicaState = defaultReplicaState, fetchRequestMaxWaitMs = f1_maxwaitms, fetchRequestMinBytes = f2_minbytes, fetchRequestMaxBytes = f3_maxbytes, fetchRequestIsolationLevel = f4_isolationlevel, fetchRequestSessionId = f5_sessionid, fetchRequestSessionEpoch = f6_sessionepoch, fetchRequestTopics = f7_topics, fetchRequestForgottenTopicsData = f8_forgottentopicsdata, fetchRequestRackId = P.KafkaString Null }, p9)
  | version >= 15 && version <= 19 = do
    (f0_maxwaitms, p1) <- W.peekInt32BE p0 endPtr
    (f1_minbytes, p2) <- W.peekInt32BE p1 endPtr
    (f2_maxbytes, p3) <- (if version >= 3 then W.peekInt32BE p2 endPtr else pure (2147483647, p2))
    (f3_isolationlevel, p4) <- (if version >= 4 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr else pure (0, p3))
    (f4_sessionid, p5) <- (if version >= 7 then W.peekInt32BE p4 endPtr else pure (0, p4))
    (f5_sessionepoch, p6) <- (if version >= 7 then W.peekInt32BE p5 endPtr else pure (-1, p5))
    (f6_topics, p7) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchTopic version _fp _basePtr p e) p6 endPtr
    (f7_forgottentopicsdata, p8) <- (if version >= 7 then WP.peekVersionedArray version 12 (\p e -> wirePeekForgottenTopic version _fp _basePtr p e) p7 endPtr else pure (P.mkKafkaArray V.empty, p7))
    (f8_rackid, p9) <- (if version >= 11 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p8 endPtr else WP.peekKafkaString p8 endPtr) else pure (P.KafkaString Null, p8))
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p9 endPtr
    let !_tag_clusterid = if version >= 12 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (\b -> fmap P.fromCompactString ((W.runWireGet :: Data.ByteString.ByteString -> Either String P.CompactString) b)) _bs of { Right _v -> _v ; Left _ -> P.KafkaString Null}; Nothing -> P.KafkaString Null} else P.KafkaString Null
    let !_tag_replicastate = if version >= 15 then case Data.Map.Strict.lookup 1 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekReplicaState version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> defaultReplicaState}; Nothing -> defaultReplicaState} else defaultReplicaState
    pure (FetchRequest { fetchRequestClusterId = _tag_clusterid, fetchRequestReplicaId = -1, fetchRequestReplicaState = _tag_replicastate, fetchRequestMaxWaitMs = f0_maxwaitms, fetchRequestMinBytes = f1_minbytes, fetchRequestMaxBytes = f2_maxbytes, fetchRequestIsolationLevel = f3_isolationlevel, fetchRequestSessionId = f4_sessionid, fetchRequestSessionEpoch = f5_sessionepoch, fetchRequestTopics = f6_topics, fetchRequestForgottenTopicsData = f7_forgottentopicsdata, fetchRequestRackId = f8_rackid }, pTagsEnd)
  | otherwise = error $ "wirePeek FetchRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec FetchRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFetchRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFetchRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFetchRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}