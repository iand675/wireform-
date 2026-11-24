{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EnvelopeRequest
Description : Kafka EnvelopeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 58.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EnvelopeRequest
  (
    EnvelopeRequest(..),
    maxEnvelopeRequestVersion
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




data EnvelopeRequest = EnvelopeRequest
  {

  -- | The embedded request header and data.

  -- Versions: 0+
  envelopeRequestRequestData :: !(KafkaBytes)
,

  -- | Value of the initial client principal when the request is redirected by a broker.

  -- Versions: 0+
  envelopeRequestRequestPrincipal :: !(KafkaBytes)
,

  -- | The original client's address in bytes.

  -- Versions: 0+
  envelopeRequestClientHostAddress :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EnvelopeRequest.
maxEnvelopeRequestVersion :: Int16
maxEnvelopeRequestVersion = 1

-- | KafkaMessage instance for EnvelopeRequest.
instance KafkaMessage EnvelopeRequest where
  messageApiKey = 58
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a EnvelopeRequest.
wireMaxSizeEnvelopeRequest :: Int -> EnvelopeRequest -> Int
wireMaxSizeEnvelopeRequest _version msg =
  0
  + WP.dualBytesMaxSize (envelopeRequestRequestData msg)
  + WP.dualBytesMaxSize (envelopeRequestRequestPrincipal msg)
  + WP.dualBytesMaxSize (envelopeRequestClientHostAddress msg)
  + 1

-- | Direct-poke encoder for EnvelopeRequest.
wirePokeEnvelopeRequest :: Int -> Ptr Word8 -> EnvelopeRequest -> IO (Ptr Word8)
wirePokeEnvelopeRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactBytes p0 (P.toCompactBytes (envelopeRequestRequestData msg)) else WP.pokeKafkaBytes p0 (envelopeRequestRequestData msg))
    p2 <- (if version >= 0 then WP.pokeCompactBytes p1 (P.toCompactBytes (envelopeRequestRequestPrincipal msg)) else WP.pokeKafkaBytes p1 (envelopeRequestRequestPrincipal msg))
    p3 <- (if version >= 0 then WP.pokeCompactBytes p2 (P.toCompactBytes (envelopeRequestClientHostAddress msg)) else WP.pokeKafkaBytes p2 (envelopeRequestClientHostAddress msg))
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke EnvelopeRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for EnvelopeRequest.
wirePeekEnvelopeRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EnvelopeRequest, Ptr Word8)
wirePeekEnvelopeRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_requestdata, p1) <- (if version >= 0 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr else WP.peekKafkaBytes p0 endPtr)
    (f1_requestprincipal, p2) <- (if version >= 0 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr else WP.peekKafkaBytes p1 endPtr)
    (f2_clienthostaddress, p3) <- (if version >= 0 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (EnvelopeRequest { envelopeRequestRequestData = f0_requestdata, envelopeRequestRequestPrincipal = f1_requestprincipal, envelopeRequestClientHostAddress = f2_clienthostaddress }, pTagsEnd)
  | otherwise = error $ "wirePeek EnvelopeRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec EnvelopeRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEnvelopeRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEnvelopeRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEnvelopeRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}