{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ApiVersionsRequest
Description : Kafka ApiVersionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 18.



Valid versions: 0-6
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ApiVersionsRequest
  (
    ApiVersionsRequest(..),
    maxApiVersionsRequestVersion
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




data ApiVersionsRequest = ApiVersionsRequest
  {

  -- | The name of the client.

  -- Versions: 3+
  apiVersionsRequestClientSoftwareName :: !(KafkaString)
,

  -- | The version of the client.

  -- Versions: 3+
  apiVersionsRequestClientSoftwareVersion :: !(KafkaString)
,

  -- | The cluster ID the client intends to connect to. Provide both ClusterId and NodeId, if known.

  -- Versions: 5+
  apiVersionsRequestClusterId :: !(KafkaString)
,

  -- | The node ID the client intends to connect to. Provide both ClusterId and NodeId, if known.

  -- Versions: 5+
  apiVersionsRequestNodeId :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ApiVersionsRequest.
maxApiVersionsRequestVersion :: Int16
maxApiVersionsRequestVersion = 6

-- | KafkaMessage instance for ApiVersionsRequest.
instance KafkaMessage ApiVersionsRequest where
  messageApiKey = 18
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 3


-- | Worst-case wire size of a ApiVersionsRequest.
wireMaxSizeApiVersionsRequest :: Int -> ApiVersionsRequest -> Int
wireMaxSizeApiVersionsRequest _version msg =
  0
  + WP.dualStringMaxSize (apiVersionsRequestClientSoftwareName msg)
  + WP.dualStringMaxSize (apiVersionsRequestClientSoftwareVersion msg)
  + WP.dualStringMaxSize (apiVersionsRequestClusterId msg)
  + 4
  + 1

-- | Direct-poke encoder for ApiVersionsRequest.
wirePokeApiVersionsRequest :: Int -> Ptr Word8 -> ApiVersionsRequest -> IO (Ptr Word8)
wirePokeApiVersionsRequest version basePtr msg
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (apiVersionsRequestClientSoftwareName msg)) else WP.pokeKafkaString p0 (apiVersionsRequestClientSoftwareName msg)) else pure p0)
    p2 <- (if version >= 3 then (if version >= 3 then WP.pokeCompactString p1 (P.toCompactString (apiVersionsRequestClientSoftwareVersion msg)) else WP.pokeKafkaString p1 (apiVersionsRequestClientSoftwareVersion msg)) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version >= 5 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (apiVersionsRequestClientSoftwareName msg)) else WP.pokeKafkaString p0 (apiVersionsRequestClientSoftwareName msg)) else pure p0)
    p2 <- (if version >= 3 then (if version >= 3 then WP.pokeCompactString p1 (P.toCompactString (apiVersionsRequestClientSoftwareVersion msg)) else WP.pokeKafkaString p1 (apiVersionsRequestClientSoftwareVersion msg)) else pure p1)
    p3 <- (if version >= 5 then (if version >= 3 then WP.pokeCompactString p2 (P.toCompactString (apiVersionsRequestClusterId msg)) else WP.pokeKafkaString p2 (apiVersionsRequestClusterId msg)) else pure p2)
    p4 <- (if version >= 5 then W.pokeInt32BE p3 (apiVersionsRequestNodeId msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    pure p0
  | otherwise = error $ "wirePoke ApiVersionsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ApiVersionsRequest.
wirePeekApiVersionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ApiVersionsRequest, Ptr Word8)
wirePeekApiVersionsRequest version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 4 = do
    (f0_clientsoftwarename, p1) <- (if version >= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_clientsoftwareversion, p2) <- (if version >= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ApiVersionsRequest { apiVersionsRequestClientSoftwareName = f0_clientsoftwarename, apiVersionsRequestClientSoftwareVersion = f1_clientsoftwareversion, apiVersionsRequestClusterId = P.KafkaString Null, apiVersionsRequestNodeId = -1 }, pTagsEnd)
  | version >= 5 && version <= 6 = do
    (f0_clientsoftwarename, p1) <- (if version >= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_clientsoftwareversion, p2) <- (if version >= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
    (f2_clusterid, p3) <- (if version >= 5 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_nodeid, p4) <- (if version >= 5 then W.peekInt32BE p3 endPtr else pure (-1, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ApiVersionsRequest { apiVersionsRequestClientSoftwareName = f0_clientsoftwarename, apiVersionsRequestClientSoftwareVersion = f1_clientsoftwareversion, apiVersionsRequestClusterId = f2_clusterid, apiVersionsRequestNodeId = f3_nodeid }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    pure (ApiVersionsRequest { apiVersionsRequestClientSoftwareName = P.KafkaString Null, apiVersionsRequestClientSoftwareVersion = P.KafkaString Null, apiVersionsRequestClusterId = P.KafkaString Null, apiVersionsRequestNodeId = -1 }, p0)
  | otherwise = error $ "wirePeek ApiVersionsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ApiVersionsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeApiVersionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeApiVersionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekApiVersionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}