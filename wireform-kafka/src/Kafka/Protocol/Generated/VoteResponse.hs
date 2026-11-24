{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.VoteResponse
Description : Kafka VoteResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 52.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.VoteResponse
  (
    VoteResponse(..),
    TopicData(..),
    PartitionData(..),
    NodeEndpoint(..),
    maxVoteResponseVersion
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


-- | The results for each partition.
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
,

  -- | True if the vote was granted and false otherwise.

  -- Versions: 0+
  partitionDataVoteGranted :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | The results for each topic.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The results for each partition.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)

-- | Endpoints for all current-leaders enumerated in PartitionData.
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


data VoteResponse = VoteResponse
  {

  -- | The top level error code.

  -- Versions: 0+
  voteResponseErrorCode :: !(Int16)
,

  -- | The results for each topic.

  -- Versions: 0+
  voteResponseTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for all current-leaders enumerated in PartitionData.

  -- Versions: 1+
  voteResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for VoteResponse.
maxVoteResponseVersion :: Int16
maxVoteResponseVersion = 2

-- | KafkaMessage instance for VoteResponse.
instance KafkaMessage VoteResponse where
  messageApiKey = 52
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + 4
  + 4
  + 1
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderId msg)
  p4 <- W.pokeInt32BE p3 (partitionDataLeaderEpoch msg)
  p5 <- W.pokeWord8 p4 (if (partitionDataVoteGranted msg) then 1 else 0)
  if version >= 0 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_votegranted, p5) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataLeaderId = f2_leaderid, partitionDataLeaderEpoch = f3_leaderepoch, partitionDataVoteGranted = f4_votegranted }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataErrorCode = 0, partitionDataLeaderId = 0, partitionDataLeaderEpoch = 0, partitionDataVoteGranted = False }

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
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg)) else WP.pokeKafkaString p0 (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
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

-- | Worst-case wire size of a VoteResponse.
wireMaxSizeVoteResponse :: Int -> VoteResponse -> Int
wireMaxSizeVoteResponse _version msg =
  0
  + 2
  + (5 + (case P.unKafkaArray (voteResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + (if _version >= 1 then (5 + (case P.unKafkaArray (voteResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 })) + 10 else 0)
  + 1

-- | Direct-poke encoder for VoteResponse.
wirePokeVoteResponse :: Int -> Ptr Word8 -> VoteResponse -> IO (Ptr Word8)
wirePokeVoteResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (voteResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p1 (voteResponseTopics msg)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (voteResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (voteResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p2 _taggedEntries
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (voteResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p1 (voteResponseTopics msg)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (voteResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (voteResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p2 _taggedEntries
  | otherwise = error $ "wirePoke VoteResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for VoteResponse.
wirePeekVoteResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (VoteResponse, Ptr Word8)
wirePeekVoteResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p2 endPtr
    let !_tag_nodeendpoints = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (VoteResponse { voteResponseErrorCode = f0_errorcode, voteResponseTopics = f1_topics, voteResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p2 endPtr
    let !_tag_nodeendpoints = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (VoteResponse { voteResponseErrorCode = f0_errorcode, voteResponseTopics = f1_topics, voteResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek VoteResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec VoteResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeVoteResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeVoteResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekVoteResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}