{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndQuorumEpochResponse
Description : Kafka EndQuorumEpochResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 54.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndQuorumEpochResponse
  (
    EndQuorumEpochResponse(..),
    TopicData(..),
    PartitionData(..),
    NodeEndpoint(..),
    maxEndQuorumEpochResponseVersion
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


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition level error code.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The topic data.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The partition data.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)

-- | Endpoints for all leaders enumerated in PartitionData.
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


data EndQuorumEpochResponse = EndQuorumEpochResponse
  {

  -- | The top level error code.

  -- Versions: 0+
  endQuorumEpochResponseErrorCode :: !(Int16)
,

  -- | The topic data.

  -- Versions: 0+
  endQuorumEpochResponseTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for all leaders enumerated in PartitionData.

  -- Versions: 1+
  endQuorumEpochResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndQuorumEpochResponse.
maxEndQuorumEpochResponseVersion :: Int16
maxEndQuorumEpochResponseVersion = 1

-- | KafkaMessage instance for EndQuorumEpochResponse.
instance KafkaMessage EndQuorumEpochResponse where
  messageApiKey = 54
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + 4
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderId msg)
  p4 <- W.pokeInt32BE p3 (partitionDataLeaderEpoch msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataLeaderId = f2_leaderid, partitionDataLeaderEpoch = f3_leaderepoch }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataErrorCode = 0, partitionDataLeaderId = 0, partitionDataLeaderEpoch = 0 }

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + WP.dualStringMaxSize (topicDataTopicName msg)
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg)) else WP.pokeKafkaString p0 (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicData :: TopicData
defaultTopicData = TopicData { topicDataTopicName = P.KafkaString Null, topicDataPartitions = P.mkKafkaArray V.empty }

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
  p2 <- (if version >= 1 then (if version >= 1 then WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg)) else WP.pokeKafkaString p1 (nodeEndpointHost msg)) else pure p1)
  p3 <- (if version >= 1 then W.pokeWord16BE p2 (nodeEndpointPort msg) else pure p2)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_host, p2) <- (if version >= 1 then (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_port, p3) <- (if version >= 1 then W.peekWord16BE p2 endPtr else pure (0, p2))
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultNodeEndpoint :: NodeEndpoint
defaultNodeEndpoint = NodeEndpoint { nodeEndpointNodeId = 0, nodeEndpointHost = P.KafkaString Null, nodeEndpointPort = 0 }

-- | Worst-case wire size of a EndQuorumEpochResponse.
wireMaxSizeEndQuorumEpochResponse :: Int -> EndQuorumEpochResponse -> Int
wireMaxSizeEndQuorumEpochResponse _version msg =
  0
  + 2
  + (5 + (case P.unKafkaArray (endQuorumEpochResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + (if _version >= 1 then (5 + (case P.unKafkaArray (endQuorumEpochResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 })) + 10 else 0)
  + 1

-- | Direct-poke encoder for EndQuorumEpochResponse.
wirePokeEndQuorumEpochResponse :: Int -> Ptr Word8 -> EndQuorumEpochResponse -> IO (Ptr Word8)
wirePokeEndQuorumEpochResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (endQuorumEpochResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (endQuorumEpochResponseTopics msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (endQuorumEpochResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (endQuorumEpochResponseTopics msg)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (endQuorumEpochResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (endQuorumEpochResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p2 _taggedEntries
  | otherwise = error $ "wirePoke EndQuorumEpochResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for EndQuorumEpochResponse.
wirePeekEndQuorumEpochResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EndQuorumEpochResponse, Ptr Word8)
wirePeekEndQuorumEpochResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pure (EndQuorumEpochResponse { endQuorumEpochResponseErrorCode = f0_errorcode, endQuorumEpochResponseTopics = f1_topics, endQuorumEpochResponseNodeEndpoints = P.mkKafkaArray V.empty }, p2)
  | version == 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p2 endPtr
    let !_tag_nodeendpoints = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (EndQuorumEpochResponse { endQuorumEpochResponseErrorCode = f0_errorcode, endQuorumEpochResponseTopics = f1_topics, endQuorumEpochResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek EndQuorumEpochResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec EndQuorumEpochResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEndQuorumEpochResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEndQuorumEpochResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEndQuorumEpochResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}