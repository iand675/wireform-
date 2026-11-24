{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchSnapshotResponse
Description : Kafka FetchSnapshotResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 59.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchSnapshotResponse
  (
    FetchSnapshotResponse(..),
    TopicSnapshot(..),
    PartitionSnapshot(..),
    SnapshotId(..),
    LeaderIdAndEpoch(..),
    NodeEndpoint(..),
    maxFetchSnapshotResponseVersion
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


-- | The snapshot endOffset and epoch fetched.
data SnapshotId = SnapshotId
  {

  -- | The snapshot end offset.

  -- Versions: 0+
  snapshotIdEndOffset :: !(Int64)
,

  -- | The snapshot epoch.

  -- Versions: 0+
  snapshotIdEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The leader of the partition at the time of the snapshot.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The partitions to fetch.
data PartitionSnapshot = PartitionSnapshot
  {

  -- | The partition index.

  -- Versions: 0+
  partitionSnapshotIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no fetch error.

  -- Versions: 0+
  partitionSnapshotErrorCode :: !(Int16)
,

  -- | The snapshot endOffset and epoch fetched.

  -- Versions: 0+
  partitionSnapshotSnapshotId :: !(SnapshotId)
,

  -- | The leader of the partition at the time of the snapshot.

  -- Versions: 0+
  partitionSnapshotCurrentLeader :: !(LeaderIdAndEpoch)
,

  -- | The total size of the snapshot.

  -- Versions: 0+
  partitionSnapshotSize :: !(Int64)
,

  -- | The starting byte position within the snapshot included in the Bytes field.

  -- Versions: 0+
  partitionSnapshotPosition :: !(Int64)
,

  -- | Snapshot data in records format which may not be aligned on an offset boundary.

  -- Versions: 0+
  partitionSnapshotUnalignedRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | The topics to fetch.
data TopicSnapshot = TopicSnapshot
  {

  -- | The name of the topic to fetch.

  -- Versions: 0+
  topicSnapshotName :: !(KafkaString)
,

  -- | The partitions to fetch.

  -- Versions: 0+
  topicSnapshotPartitions :: !(KafkaArray (PartitionSnapshot))

  }
  deriving (Eq, Show, Generic)

-- | Endpoints for all current-leaders enumerated in PartitionSnapshot.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 1+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 1+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 1+
  nodeEndpointPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


data FetchSnapshotResponse = FetchSnapshotResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  fetchSnapshotResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  fetchSnapshotResponseErrorCode :: !(Int16)
,

  -- | The topics to fetch.

  -- Versions: 0+
  fetchSnapshotResponseTopics :: !(KafkaArray (TopicSnapshot))
,

  -- | Endpoints for all current-leaders enumerated in PartitionSnapshot.

  -- Versions: 1+
  fetchSnapshotResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchSnapshotResponse.
maxFetchSnapshotResponseVersion :: Int16
maxFetchSnapshotResponseVersion = 1

-- | KafkaMessage instance for FetchSnapshotResponse.
instance KafkaMessage FetchSnapshotResponse where
  messageApiKey = 59
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a SnapshotId.
wireMaxSizeSnapshotId :: Int -> SnapshotId -> Int
wireMaxSizeSnapshotId _version msg =
  0
  + 8
  + 4
  + 1

-- | Direct-poke encoder for SnapshotId.
wirePokeSnapshotId :: Int -> Ptr Word8 -> SnapshotId -> IO (Ptr Word8)
wirePokeSnapshotId version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (snapshotIdEndOffset msg)
  p2 <- W.pokeInt32BE p1 (snapshotIdEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for SnapshotId.
wirePeekSnapshotId :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SnapshotId, Ptr Word8)
wirePeekSnapshotId version _fp _basePtr p0 endPtr = do
  (f0_endoffset, p1) <- W.peekInt64BE p0 endPtr
  (f1_epoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (SnapshotId { snapshotIdEndOffset = f0_endoffset, snapshotIdEpoch = f1_epoch }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultSnapshotId :: SnapshotId
defaultSnapshotId = SnapshotId { snapshotIdEndOffset = 0, snapshotIdEpoch = 0 }

-- | Worst-case wire size of a LeaderIdAndEpoch.
wireMaxSizeLeaderIdAndEpoch :: Int -> LeaderIdAndEpoch -> Int
wireMaxSizeLeaderIdAndEpoch _version msg =
  0
  + 4
  + 4
  + 1

-- | Direct-poke encoder for LeaderIdAndEpoch.
wirePokeLeaderIdAndEpoch :: Int -> Ptr Word8 -> LeaderIdAndEpoch -> IO (Ptr Word8)
wirePokeLeaderIdAndEpoch version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (leaderIdAndEpochLeaderId msg)
  p2 <- W.pokeInt32BE p1 (leaderIdAndEpochLeaderEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for LeaderIdAndEpoch.
wirePeekLeaderIdAndEpoch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderIdAndEpoch, Ptr Word8)
wirePeekLeaderIdAndEpoch version _fp _basePtr p0 endPtr = do
  (f0_leaderid, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = f0_leaderid, leaderIdAndEpochLeaderEpoch = f1_leaderepoch }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultLeaderIdAndEpoch :: LeaderIdAndEpoch
defaultLeaderIdAndEpoch = LeaderIdAndEpoch { leaderIdAndEpochLeaderId = 0, leaderIdAndEpochLeaderEpoch = 0 }

-- | Worst-case wire size of a PartitionSnapshot.
wireMaxSizePartitionSnapshot :: Int -> PartitionSnapshot -> Int
wireMaxSizePartitionSnapshot _version msg =
  0
  + 4
  + 2
  + wireMaxSizeSnapshotId _version (partitionSnapshotSnapshotId msg)
  + (if _version >= 0 then wireMaxSizeLeaderIdAndEpoch _version (partitionSnapshotCurrentLeader msg) + 10 else 0)
  + 8
  + 8
  + WP.dualBytesMaxSize (partitionSnapshotUnalignedRecords msg)
  + 1

-- | Direct-poke encoder for PartitionSnapshot.
wirePokePartitionSnapshot :: Int -> Ptr Word8 -> PartitionSnapshot -> IO (Ptr Word8)
wirePokePartitionSnapshot version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionSnapshotIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionSnapshotErrorCode msg)
  p3 <- wirePokeSnapshotId version p2 (partitionSnapshotSnapshotId msg)
  p4 <- W.pokeInt64BE p3 (partitionSnapshotSize msg)
  p5 <- W.pokeInt64BE p4 (partitionSnapshotPosition msg)
  p6 <- (if version >= 0 then WP.pokeCompactBytes p5 (P.toCompactBytes (partitionSnapshotUnalignedRecords msg)) else WP.pokeKafkaBytes p5 (partitionSnapshotUnalignedRecords msg))
  if version >= 0 then do
    let !_taggedEntries = (if version >= 0 then [(0, W.runWirePokeWith (wireMaxSizeLeaderIdAndEpoch version (partitionSnapshotCurrentLeader msg)) (\p -> wirePokeLeaderIdAndEpoch version p (partitionSnapshotCurrentLeader msg)))] else [])
    WP.pokeTaggedFieldEntries p6 _taggedEntries
  else pure p6

-- | Direct-poke decoder for PartitionSnapshot.
wirePeekPartitionSnapshot :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionSnapshot, Ptr Word8)
wirePeekPartitionSnapshot version _fp _basePtr p0 endPtr = do
  (f0_index, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_snapshotid, p3) <- wirePeekSnapshotId version _fp _basePtr p2 endPtr
  (f3_size, p4) <- W.peekInt64BE p3 endPtr
  (f4_position, p5) <- W.peekInt64BE p4 endPtr
  (f5_unalignedrecords, p6) <- (if version >= 0 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p5 endPtr else WP.peekKafkaBytes p5 endPtr)
  (_taggedMap, pTagsEnd) <- if version >= 0 then WP.peekTaggedFieldsMap p6 endPtr else pure (Data.Map.Strict.empty, p6)
  let !_tag_currentleader = if version >= 0 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekLeaderIdAndEpoch version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> defaultLeaderIdAndEpoch}; Nothing -> defaultLeaderIdAndEpoch} else defaultLeaderIdAndEpoch
  pure (PartitionSnapshot { partitionSnapshotIndex = f0_index, partitionSnapshotErrorCode = f1_errorcode, partitionSnapshotSnapshotId = f2_snapshotid, partitionSnapshotCurrentLeader = _tag_currentleader, partitionSnapshotSize = f3_size, partitionSnapshotPosition = f4_position, partitionSnapshotUnalignedRecords = f5_unalignedrecords }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionSnapshot :: PartitionSnapshot
defaultPartitionSnapshot = PartitionSnapshot { partitionSnapshotIndex = 0, partitionSnapshotErrorCode = 0, partitionSnapshotSnapshotId = defaultSnapshotId, partitionSnapshotCurrentLeader = defaultLeaderIdAndEpoch, partitionSnapshotSize = 0, partitionSnapshotPosition = 0, partitionSnapshotUnalignedRecords = P.KafkaBytes Null }

-- | Worst-case wire size of a TopicSnapshot.
wireMaxSizeTopicSnapshot :: Int -> TopicSnapshot -> Int
wireMaxSizeTopicSnapshot _version msg =
  0
  + WP.dualStringMaxSize (topicSnapshotName msg)
  + (5 + (case P.unKafkaArray (topicSnapshotPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionSnapshot _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicSnapshot.
wirePokeTopicSnapshot :: Int -> Ptr Word8 -> TopicSnapshot -> IO (Ptr Word8)
wirePokeTopicSnapshot version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (topicSnapshotName msg)) else WP.pokeKafkaString p0 (topicSnapshotName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionSnapshot version p x) p1 (topicSnapshotPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicSnapshot.
wirePeekTopicSnapshot :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicSnapshot, Ptr Word8)
wirePeekTopicSnapshot version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionSnapshot version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicSnapshot { topicSnapshotName = f0_name, topicSnapshotPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicSnapshot :: TopicSnapshot
defaultTopicSnapshot = TopicSnapshot { topicSnapshotName = P.KafkaString Null, topicSnapshotPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a NodeEndpoint.
wireMaxSizeNodeEndpoint :: Int -> NodeEndpoint -> Int
wireMaxSizeNodeEndpoint _version msg =
  0
  + 4
  + WP.dualStringMaxSize (nodeEndpointHost msg)
  + 2
  + 1

-- | Direct-poke encoder for NodeEndpoint.
wirePokeNodeEndpoint :: Int -> Ptr Word8 -> NodeEndpoint -> IO (Ptr Word8)
wirePokeNodeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 1 then W.pokeInt32BE p0 (nodeEndpointNodeId msg) else pure p0)
  p2 <- (if version >= 1 then (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg)) else WP.pokeKafkaString p1 (nodeEndpointHost msg)) else pure p1)
  p3 <- (if version >= 1 then W.pokeWord16BE p2 (nodeEndpointPort msg) else pure p2)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_host, p2) <- (if version >= 1 then (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_port, p3) <- (if version >= 1 then W.peekWord16BE p2 endPtr else pure (0, p2))
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultNodeEndpoint :: NodeEndpoint
defaultNodeEndpoint = NodeEndpoint { nodeEndpointNodeId = 0, nodeEndpointHost = P.KafkaString Null, nodeEndpointPort = 0 }

-- | Worst-case wire size of a FetchSnapshotResponse.
wireMaxSizeFetchSnapshotResponse :: Int -> FetchSnapshotResponse -> Int
wireMaxSizeFetchSnapshotResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (fetchSnapshotResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicSnapshot _version x ) v); P.Null -> 0 }))
  + (if _version >= 1 then (5 + (case P.unKafkaArray (fetchSnapshotResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 })) + 10 else 0)
  + 1

-- | Direct-poke encoder for FetchSnapshotResponse.
wirePokeFetchSnapshotResponse :: Int -> Ptr Word8 -> FetchSnapshotResponse -> IO (Ptr Word8)
wirePokeFetchSnapshotResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (fetchSnapshotResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (fetchSnapshotResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicSnapshot version p x) p2 (fetchSnapshotResponseTopics msg)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (fetchSnapshotResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (fetchSnapshotResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p3 _taggedEntries
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (fetchSnapshotResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (fetchSnapshotResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicSnapshot version p x) p2 (fetchSnapshotResponseTopics msg)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (fetchSnapshotResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (fetchSnapshotResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p3 _taggedEntries
  | otherwise = error $ "wirePoke FetchSnapshotResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for FetchSnapshotResponse.
wirePeekFetchSnapshotResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchSnapshotResponse, Ptr Word8)
wirePeekFetchSnapshotResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicSnapshot version _fp _basePtr p e) p2 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p3 endPtr
    let !_tag_nodeendpoints = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (FetchSnapshotResponse { fetchSnapshotResponseThrottleTimeMs = f0_throttletimems, fetchSnapshotResponseErrorCode = f1_errorcode, fetchSnapshotResponseTopics = f2_topics, fetchSnapshotResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicSnapshot version _fp _basePtr p e) p2 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p3 endPtr
    let !_tag_nodeendpoints = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (FetchSnapshotResponse { fetchSnapshotResponseThrottleTimeMs = f0_throttletimems, fetchSnapshotResponseErrorCode = f1_errorcode, fetchSnapshotResponseTopics = f2_topics, fetchSnapshotResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek FetchSnapshotResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec FetchSnapshotResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFetchSnapshotResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFetchSnapshotResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFetchSnapshotResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}