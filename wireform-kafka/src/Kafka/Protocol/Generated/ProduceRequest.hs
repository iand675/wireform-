{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ProduceRequest
Description : Kafka ProduceRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 0.



Valid versions: 3-14
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ProduceRequest
  (
    ProduceRequest(..),
    TopicProduceData(..),
    PartitionProduceData(..),
    maxProduceRequestVersion
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


-- | Each partition to produce to.
data PartitionProduceData = PartitionProduceData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionProduceDataIndex :: !(Int32)
,

  -- | The record data to be produced.

  -- Versions: 0+
  partitionProduceDataRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Each topic to produce to.
data TopicProduceData = TopicProduceData
  {

  -- | The topic name.

  -- Versions: 0-12
  topicProduceDataName :: !(KafkaString)
,

  -- | The unique topic ID

  -- Versions: 13+
  topicProduceDataTopicId :: !(KafkaUuid)
,

  -- | Each partition to produce to.

  -- Versions: 0+
  topicProduceDataPartitionData :: !(KafkaArray (PartitionProduceData))

  }
  deriving (Eq, Show, Generic)


data ProduceRequest = ProduceRequest
  {

  -- | The transactional ID, or null if the producer is not transactional.

  -- Versions: 3+
  produceRequestTransactionalId :: !(KafkaString)
,

  -- | The number of acknowledgments the producer requires the leader to have received before considering a

  -- Versions: 0+
  produceRequestAcks :: !(Int16)
,

  -- | The timeout to await a response in milliseconds.

  -- Versions: 0+
  produceRequestTimeoutMs :: !(Int32)
,

  -- | Each topic to produce to.

  -- Versions: 0+
  produceRequestTopicData :: !(KafkaArray (TopicProduceData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ProduceRequest.
maxProduceRequestVersion :: Int16
maxProduceRequestVersion = 14

-- | KafkaMessage instance for ProduceRequest.
instance KafkaMessage ProduceRequest where
  messageApiKey = 0
  messageMinVersion = 3
  messageMaxVersion = 14
  messageFlexibleVersion = Just 9

-- | Worst-case wire size of a PartitionProduceData.
wireMaxSizePartitionProduceData :: Int -> PartitionProduceData -> Int
wireMaxSizePartitionProduceData _version msg =
  0
  + 4
  + WP.dualBytesMaxSize (partitionProduceDataRecords msg)
  + 1

-- | Direct-poke encoder for PartitionProduceData.
wirePokePartitionProduceData :: Int -> Ptr Word8 -> PartitionProduceData -> IO (Ptr Word8)
wirePokePartitionProduceData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionProduceDataIndex msg)
  p2 <- (if version >= 9 then WP.pokeCompactBytes p1 (P.toCompactBytes (partitionProduceDataRecords msg)) else WP.pokeKafkaBytes p1 (partitionProduceDataRecords msg))
  if version >= 9 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for PartitionProduceData.
wirePeekPartitionProduceData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionProduceData, Ptr Word8)
wirePeekPartitionProduceData version _fp _basePtr p0 endPtr = do
  (f0_index, p1) <- W.peekInt32BE p0 endPtr
  (f1_records, p2) <- (if version >= 9 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr else WP.peekKafkaBytes p1 endPtr)
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (PartitionProduceData { partitionProduceDataIndex = f0_index, partitionProduceDataRecords = f1_records }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionProduceData :: PartitionProduceData
defaultPartitionProduceData = PartitionProduceData { partitionProduceDataIndex = 0, partitionProduceDataRecords = P.KafkaBytes Null }

-- | Worst-case wire size of a TopicProduceData.
wireMaxSizeTopicProduceData :: Int -> TopicProduceData -> Int
wireMaxSizeTopicProduceData _version msg =
  0
  + WP.dualStringMaxSize (topicProduceDataName msg)
  + 16
  + (5 + (case P.unKafkaArray (topicProduceDataPartitionData msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionProduceData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicProduceData.
wirePokeTopicProduceData :: Int -> Ptr Word8 -> TopicProduceData -> IO (Ptr Word8)
wirePokeTopicProduceData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 12 then (if version >= 9 then WP.pokeCompactString p0 (P.toCompactString (topicProduceDataName msg)) else WP.pokeKafkaString p0 (topicProduceDataName msg)) else pure p0)
  p2 <- (if version >= 13 then WP.pokeKafkaUuid p1 (topicProduceDataTopicId msg) else pure p1)
  p3 <- WP.pokeVersionedArray version 9 (\p x -> wirePokePartitionProduceData version p x) p2 (topicProduceDataPartitionData msg)
  if version >= 9 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TopicProduceData.
wirePeekTopicProduceData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicProduceData, Ptr Word8)
wirePeekTopicProduceData version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version <= 12 then (if version >= 9 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 13 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitiondata, p3) <- WP.peekVersionedArray version 9 (\p e -> wirePeekPartitionProduceData version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TopicProduceData { topicProduceDataName = f0_name, topicProduceDataTopicId = f1_topicid, topicProduceDataPartitionData = f2_partitiondata }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicProduceData :: TopicProduceData
defaultTopicProduceData = TopicProduceData { topicProduceDataName = P.KafkaString Null, topicProduceDataTopicId = P.nullUuid, topicProduceDataPartitionData = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ProduceRequest.
wireMaxSizeProduceRequest :: Int -> ProduceRequest -> Int
wireMaxSizeProduceRequest _version msg =
  0
  + WP.dualStringMaxSize (produceRequestTransactionalId msg)
  + 2
  + 4
  + (5 + (case P.unKafkaArray (produceRequestTopicData msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicProduceData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ProduceRequest.
wirePokeProduceRequest :: Int -> Ptr Word8 -> ProduceRequest -> IO (Ptr Word8)
wirePokeProduceRequest version basePtr msg
  | version >= 3 && version <= 8 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then (if version >= 9 then WP.pokeCompactString p0 (P.toCompactString (produceRequestTransactionalId msg)) else WP.pokeKafkaString p0 (produceRequestTransactionalId msg)) else pure p0)
    p2 <- W.pokeInt16BE p1 (produceRequestAcks msg)
    p3 <- W.pokeInt32BE p2 (produceRequestTimeoutMs msg)
    p4 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceData version p x) p3 (produceRequestTopicData msg)
    pure p4
  | version >= 9 && version <= 14 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then (if version >= 9 then WP.pokeCompactString p0 (P.toCompactString (produceRequestTransactionalId msg)) else WP.pokeKafkaString p0 (produceRequestTransactionalId msg)) else pure p0)
    p2 <- W.pokeInt16BE p1 (produceRequestAcks msg)
    p3 <- W.pokeInt32BE p2 (produceRequestTimeoutMs msg)
    p4 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeTopicProduceData version p x) p3 (produceRequestTopicData msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke ProduceRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ProduceRequest.
wirePeekProduceRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ProduceRequest, Ptr Word8)
wirePeekProduceRequest version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 8 = do
    (f0_transactionalid, p1) <- (if version >= 3 then (if version >= 9 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_acks, p2) <- W.peekInt16BE p1 endPtr
    (f2_timeoutms, p3) <- W.peekInt32BE p2 endPtr
    (f3_topicdata, p4) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceData version _fp _basePtr p e) p3 endPtr
    pure (ProduceRequest { produceRequestTransactionalId = f0_transactionalid, produceRequestAcks = f1_acks, produceRequestTimeoutMs = f2_timeoutms, produceRequestTopicData = f3_topicdata }, p4)
  | version >= 9 && version <= 14 = do
    (f0_transactionalid, p1) <- (if version >= 3 then (if version >= 9 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_acks, p2) <- W.peekInt16BE p1 endPtr
    (f2_timeoutms, p3) <- W.peekInt32BE p2 endPtr
    (f3_topicdata, p4) <- WP.peekVersionedArray version 9 (\p e -> wirePeekTopicProduceData version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ProduceRequest { produceRequestTransactionalId = f0_transactionalid, produceRequestAcks = f1_acks, produceRequestTimeoutMs = f2_timeoutms, produceRequestTopicData = f3_topicdata }, pTagsEnd)
  | otherwise = error $ "wirePeek ProduceRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ProduceRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeProduceRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeProduceRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekProduceRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}