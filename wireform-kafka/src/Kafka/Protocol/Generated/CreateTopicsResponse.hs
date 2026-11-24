{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateTopicsResponse
Description : Kafka CreateTopicsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 19.



Valid versions: 2-7
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateTopicsResponse
  (
    CreateTopicsResponse(..),
    CreatableTopicResult(..),
    CreatableTopicConfigs(..),
    maxCreateTopicsResponseVersion
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


-- | Configuration of the topic.
data CreatableTopicConfigs = CreatableTopicConfigs
  {

  -- | The configuration name.

  -- Versions: 5+
  creatableTopicConfigsName :: !(KafkaString)
,

  -- | The configuration value.

  -- Versions: 5+
  creatableTopicConfigsValue :: !(KafkaString)
,

  -- | True if the configuration is read-only.

  -- Versions: 5+
  creatableTopicConfigsReadOnly :: !(Bool)
,

  -- | The configuration source.

  -- Versions: 5+
  creatableTopicConfigsConfigSource :: !(Int8)
,

  -- | True if this configuration is sensitive.

  -- Versions: 5+
  creatableTopicConfigsIsSensitive :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Results for each topic we tried to create.
data CreatableTopicResult = CreatableTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  creatableTopicResultName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 7+
  creatableTopicResultTopicId :: !(KafkaUuid)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  creatableTopicResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 1+
  creatableTopicResultErrorMessage :: !(KafkaString)
,

  -- | Optional topic config error returned if configs are not returned in the response.

  -- Versions: 5+
  creatableTopicResultTopicConfigErrorCode :: !(Int16)
,

  -- | Number of partitions of the topic.

  -- Versions: 5+
  creatableTopicResultNumPartitions :: !(Int32)
,

  -- | Replication factor of the topic.

  -- Versions: 5+
  creatableTopicResultReplicationFactor :: !(Int16)
,

  -- | Configuration of the topic.

  -- Versions: 5+
  creatableTopicResultConfigs :: !(KafkaArray (CreatableTopicConfigs))

  }
  deriving (Eq, Show, Generic)


data CreateTopicsResponse = CreateTopicsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  createTopicsResponseThrottleTimeMs :: !(Int32)
,

  -- | Results for each topic we tried to create.

  -- Versions: 0+
  createTopicsResponseTopics :: !(KafkaArray (CreatableTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateTopicsResponse.
maxCreateTopicsResponseVersion :: Int16
maxCreateTopicsResponseVersion = 7

-- | KafkaMessage instance for CreateTopicsResponse.
instance KafkaMessage CreateTopicsResponse where
  messageApiKey = 19
  messageMinVersion = 2
  messageMaxVersion = 7
  messageFlexibleVersion = Just 5

-- | Worst-case wire size of a CreatableTopicConfigs.
wireMaxSizeCreatableTopicConfigs :: Int -> CreatableTopicConfigs -> Int
wireMaxSizeCreatableTopicConfigs _version msg =
  0
  + WP.dualStringMaxSize (creatableTopicConfigsName msg)
  + WP.dualStringMaxSize (creatableTopicConfigsValue msg)
  + 1
  + 1
  + 1
  + 1

-- | Direct-poke encoder for CreatableTopicConfigs.
wirePokeCreatableTopicConfigs :: Int -> Ptr Word8 -> CreatableTopicConfigs -> IO (Ptr Word8)
wirePokeCreatableTopicConfigs version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 5 then (if version >= 5 then WP.pokeCompactString p0 (P.toCompactString (creatableTopicConfigsName msg)) else WP.pokeKafkaString p0 (creatableTopicConfigsName msg)) else pure p0)
  p2 <- (if version >= 5 then (if version >= 5 then WP.pokeCompactString p1 (P.toCompactString (creatableTopicConfigsValue msg)) else WP.pokeKafkaString p1 (creatableTopicConfigsValue msg)) else pure p1)
  p3 <- (if version >= 5 then W.pokeWord8 p2 (if (creatableTopicConfigsReadOnly msg) then 1 else 0) else pure p2)
  p4 <- (if version >= 5 then W.pokeWord8 p3 (fromIntegral (creatableTopicConfigsConfigSource msg)) else pure p3)
  p5 <- (if version >= 5 then W.pokeWord8 p4 (if (creatableTopicConfigsIsSensitive msg) then 1 else 0) else pure p4)
  if version >= 5 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for CreatableTopicConfigs.
wirePeekCreatableTopicConfigs :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableTopicConfigs, Ptr Word8)
wirePeekCreatableTopicConfigs version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 5 then (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_value, p2) <- (if version >= 5 then (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_readonly, p3) <- (if version >= 5 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
  (f3_configsource, p4) <- (if version >= 5 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr else pure (-1, p3))
  (f4_issensitive, p5) <- (if version >= 5 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr else pure (False, p4))
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (CreatableTopicConfigs { creatableTopicConfigsName = f0_name, creatableTopicConfigsValue = f1_value, creatableTopicConfigsReadOnly = f2_readonly, creatableTopicConfigsConfigSource = f3_configsource, creatableTopicConfigsIsSensitive = f4_issensitive }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCreatableTopicConfigs :: CreatableTopicConfigs
defaultCreatableTopicConfigs = CreatableTopicConfigs { creatableTopicConfigsName = P.KafkaString Null, creatableTopicConfigsValue = P.KafkaString Null, creatableTopicConfigsReadOnly = False, creatableTopicConfigsConfigSource = -1, creatableTopicConfigsIsSensitive = False }

-- | Worst-case wire size of a CreatableTopicResult.
wireMaxSizeCreatableTopicResult :: Int -> CreatableTopicResult -> Int
wireMaxSizeCreatableTopicResult _version msg =
  0
  + WP.dualStringMaxSize (creatableTopicResultName msg)
  + 16
  + 2
  + WP.dualStringMaxSize (creatableTopicResultErrorMessage msg)
  + (if _version >= 5 then 2 + 10 else 0)
  + 4
  + 2
  + (5 + (case P.unKafkaArray (creatableTopicResultConfigs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatableTopicConfigs _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreatableTopicResult.
wirePokeCreatableTopicResult :: Int -> Ptr Word8 -> CreatableTopicResult -> IO (Ptr Word8)
wirePokeCreatableTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 5 then WP.pokeCompactString p0 (P.toCompactString (creatableTopicResultName msg)) else WP.pokeKafkaString p0 (creatableTopicResultName msg))
  p2 <- (if version >= 7 then WP.pokeKafkaUuid p1 (creatableTopicResultTopicId msg) else pure p1)
  p3 <- W.pokeInt16BE p2 (creatableTopicResultErrorCode msg)
  p4 <- (if version >= 1 then (if version >= 5 then WP.pokeCompactString p3 (P.toCompactString (creatableTopicResultErrorMessage msg)) else WP.pokeKafkaString p3 (creatableTopicResultErrorMessage msg)) else pure p3)
  p5 <- (if version >= 5 then W.pokeInt32BE p4 (creatableTopicResultNumPartitions msg) else pure p4)
  p6 <- (if version >= 5 then W.pokeInt16BE p5 (creatableTopicResultReplicationFactor msg) else pure p5)
  p7 <- (if version >= 5 then WP.pokeVersionedNullableArray version 5 (\p x -> wirePokeCreatableTopicConfigs version p x) p6 (creatableTopicResultConfigs msg) else pure p6)
  if version >= 5 then do
    let !_taggedEntries = (if version >= 5 then [(0, W.runWirePut (creatableTopicResultTopicConfigErrorCode msg))] else [])
    WP.pokeTaggedFieldEntries p7 _taggedEntries
  else pure p7

-- | Direct-poke decoder for CreatableTopicResult.
wirePeekCreatableTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatableTopicResult, Ptr Word8)
wirePeekCreatableTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_topicid, p2) <- (if version >= 7 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  (f3_errormessage, p4) <- (if version >= 1 then (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
  (f4_numpartitions, p5) <- (if version >= 5 then W.peekInt32BE p4 endPtr else pure (-1, p4))
  (f5_replicationfactor, p6) <- (if version >= 5 then W.peekInt16BE p5 endPtr else pure (-1, p5))
  (f6_configs, p7) <- (if version >= 5 then WP.peekVersionedNullableArray version 5 (\p e -> wirePeekCreatableTopicConfigs version _fp _basePtr p e) p6 endPtr else pure (P.KafkaArray P.Null, p6))
  (_taggedMap, pTagsEnd) <- if version >= 5 then WP.peekTaggedFieldsMap p7 endPtr else pure (Data.Map.Strict.empty, p7)
  let !_tag_topicconfigerrorcode = if version >= 5 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Int.Int16) _bs of { Right _v -> _v ; Left _ -> 0}; Nothing -> 0} else 0
  pure (CreatableTopicResult { creatableTopicResultName = f0_name, creatableTopicResultTopicId = f1_topicid, creatableTopicResultErrorCode = f2_errorcode, creatableTopicResultErrorMessage = f3_errormessage, creatableTopicResultTopicConfigErrorCode = _tag_topicconfigerrorcode, creatableTopicResultNumPartitions = f4_numpartitions, creatableTopicResultReplicationFactor = f5_replicationfactor, creatableTopicResultConfigs = f6_configs }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCreatableTopicResult :: CreatableTopicResult
defaultCreatableTopicResult = CreatableTopicResult { creatableTopicResultName = P.KafkaString Null, creatableTopicResultTopicId = P.nullUuid, creatableTopicResultErrorCode = 0, creatableTopicResultErrorMessage = P.KafkaString Null, creatableTopicResultTopicConfigErrorCode = 0, creatableTopicResultNumPartitions = -1, creatableTopicResultReplicationFactor = -1, creatableTopicResultConfigs = P.KafkaArray P.Null }

-- | Worst-case wire size of a CreateTopicsResponse.
wireMaxSizeCreateTopicsResponse :: Int -> CreateTopicsResponse -> Int
wireMaxSizeCreateTopicsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (createTopicsResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatableTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreateTopicsResponse.
wirePokeCreateTopicsResponse :: Int -> Ptr Word8 -> CreateTopicsResponse -> IO (Ptr Word8)
wirePokeCreateTopicsResponse version basePtr msg
  | version >= 2 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then W.pokeInt32BE p0 (createTopicsResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeCreatableTopicResult version p x) p1 (createTopicsResponseTopics msg)
    pure p2
  | version >= 5 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then W.pokeInt32BE p0 (createTopicsResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeCreatableTopicResult version p x) p1 (createTopicsResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke CreateTopicsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for CreateTopicsResponse.
wirePeekCreateTopicsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreateTopicsResponse, Ptr Word8)
wirePeekCreateTopicsResponse version _fp _basePtr p0 endPtr
  | version >= 2 && version <= 4 = do
    (f0_throttletimems, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_topics, p2) <- WP.peekVersionedArray version 5 (\p e -> wirePeekCreatableTopicResult version _fp _basePtr p e) p1 endPtr
    pure (CreateTopicsResponse { createTopicsResponseThrottleTimeMs = f0_throttletimems, createTopicsResponseTopics = f1_topics }, p2)
  | version >= 5 && version <= 7 = do
    (f0_throttletimems, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_topics, p2) <- WP.peekVersionedArray version 5 (\p e -> wirePeekCreatableTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (CreateTopicsResponse { createTopicsResponseThrottleTimeMs = f0_throttletimems, createTopicsResponseTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek CreateTopicsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec CreateTopicsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreateTopicsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreateTopicsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreateTopicsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}