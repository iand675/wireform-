{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EnvelopeResponse
Description : Kafka EnvelopeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 58.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EnvelopeResponse
  (
    EnvelopeResponse(..),
    maxEnvelopeResponseVersion
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




data EnvelopeResponse = EnvelopeResponse
  {

  -- | The embedded response header and data.

  -- Versions: 0+
  envelopeResponseResponseData :: !(KafkaBytes)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  envelopeResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EnvelopeResponse.
maxEnvelopeResponseVersion :: Int16
maxEnvelopeResponseVersion = 1

-- | KafkaMessage instance for EnvelopeResponse.
instance KafkaMessage EnvelopeResponse where
  messageApiKey = 58
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a EnvelopeResponse.
wireMaxSizeEnvelopeResponse :: Int -> EnvelopeResponse -> Int
wireMaxSizeEnvelopeResponse _version msg =
  0
  + WP.dualBytesMaxSize (envelopeResponseResponseData msg)
  + 2
  + 1

-- | Direct-poke encoder for EnvelopeResponse.
wirePokeEnvelopeResponse :: Int -> Ptr Word8 -> EnvelopeResponse -> IO (Ptr Word8)
wirePokeEnvelopeResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactBytes p0 (P.toCompactBytes (envelopeResponseResponseData msg)) else WP.pokeKafkaBytes p0 (envelopeResponseResponseData msg))
    p2 <- W.pokeInt16BE p1 (envelopeResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke EnvelopeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for EnvelopeResponse.
wirePeekEnvelopeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EnvelopeResponse, Ptr Word8)
wirePeekEnvelopeResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_responsedata, p1) <- (if version >= 0 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr else WP.peekKafkaBytes p0 endPtr)
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (EnvelopeResponse { envelopeResponseResponseData = f0_responsedata, envelopeResponseErrorCode = f1_errorcode }, pTagsEnd)
  | otherwise = error $ "wirePeek EnvelopeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec EnvelopeResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEnvelopeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEnvelopeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEnvelopeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}