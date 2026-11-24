{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerHeartbeatRequest
Description : Kafka BrokerHeartbeatRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 63.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerHeartbeatRequest
  (
    BrokerHeartbeatRequest(..),
    maxBrokerHeartbeatRequestVersion
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




data BrokerHeartbeatRequest = BrokerHeartbeatRequest
  {

  -- | The broker ID.

  -- Versions: 0+
  brokerHeartbeatRequestBrokerId :: !(Int32)
,

  -- | The broker epoch.

  -- Versions: 0+
  brokerHeartbeatRequestBrokerEpoch :: !(Int64)
,

  -- | The highest metadata offset which the broker has reached.

  -- Versions: 0+
  brokerHeartbeatRequestCurrentMetadataOffset :: !(Int64)
,

  -- | True if the broker wants to be fenced, false otherwise.

  -- Versions: 0+
  brokerHeartbeatRequestWantFence :: !(Bool)
,

  -- | True if the broker wants to be shut down, false otherwise.

  -- Versions: 0+
  brokerHeartbeatRequestWantShutDown :: !(Bool)
,

  -- | Log directories that failed and went offline.

  -- Versions: 1+
  brokerHeartbeatRequestOfflineLogDirs :: !(KafkaArray (KafkaUuid))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerHeartbeatRequest.
maxBrokerHeartbeatRequestVersion :: Int16
maxBrokerHeartbeatRequestVersion = 1

-- | KafkaMessage instance for BrokerHeartbeatRequest.
instance KafkaMessage BrokerHeartbeatRequest where
  messageApiKey = 63
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a BrokerHeartbeatRequest.
wireMaxSizeBrokerHeartbeatRequest :: Int -> BrokerHeartbeatRequest -> Int
wireMaxSizeBrokerHeartbeatRequest _version msg =
  0
  + 4
  + 8
  + 8
  + 1
  + 1
  + (if _version >= 1 then (5 + (case P.unKafkaArray (brokerHeartbeatRequestOfflineLogDirs msg) of { P.NotNull v -> sum (fmap (\x -> 16 ) v); P.Null -> 0 })) + 10 else 0)
  + 1

-- | Direct-poke encoder for BrokerHeartbeatRequest.
wirePokeBrokerHeartbeatRequest :: Int -> Ptr Word8 -> BrokerHeartbeatRequest -> IO (Ptr Word8)
wirePokeBrokerHeartbeatRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerHeartbeatRequestBrokerId msg)
    p2 <- W.pokeInt64BE p1 (brokerHeartbeatRequestBrokerEpoch msg)
    p3 <- W.pokeInt64BE p2 (brokerHeartbeatRequestCurrentMetadataOffset msg)
    p4 <- W.pokeWord8 p3 (if (brokerHeartbeatRequestWantFence msg) then 1 else 0)
    p5 <- W.pokeWord8 p4 (if (brokerHeartbeatRequestWantShutDown msg) then 1 else 0)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (brokerHeartbeatRequestOfflineLogDirs msg) of { P.NotNull v -> sum (fmap (\x -> 16) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray WP.pokeKafkaUuid p (brokerHeartbeatRequestOfflineLogDirs msg)))] else [])
    WP.pokeTaggedFieldEntries p5 _taggedEntries
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerHeartbeatRequestBrokerId msg)
    p2 <- W.pokeInt64BE p1 (brokerHeartbeatRequestBrokerEpoch msg)
    p3 <- W.pokeInt64BE p2 (brokerHeartbeatRequestCurrentMetadataOffset msg)
    p4 <- W.pokeWord8 p3 (if (brokerHeartbeatRequestWantFence msg) then 1 else 0)
    p5 <- W.pokeWord8 p4 (if (brokerHeartbeatRequestWantShutDown msg) then 1 else 0)
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (brokerHeartbeatRequestOfflineLogDirs msg) of { P.NotNull v -> sum (fmap (\x -> 16) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray WP.pokeKafkaUuid p (brokerHeartbeatRequestOfflineLogDirs msg)))] else [])
    WP.pokeTaggedFieldEntries p5 _taggedEntries
  | otherwise = error $ "wirePoke BrokerHeartbeatRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for BrokerHeartbeatRequest.
wirePeekBrokerHeartbeatRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BrokerHeartbeatRequest, Ptr Word8)
wirePeekBrokerHeartbeatRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokerepoch, p2) <- W.peekInt64BE p1 endPtr
    (f2_currentmetadataoffset, p3) <- W.peekInt64BE p2 endPtr
    (f3_wantfence, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    (f4_wantshutdown, p5) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p5 endPtr
    let !_tag_offlinelogdirs = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray WP.peekKafkaUuid p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (BrokerHeartbeatRequest { brokerHeartbeatRequestBrokerId = f0_brokerid, brokerHeartbeatRequestBrokerEpoch = f1_brokerepoch, brokerHeartbeatRequestCurrentMetadataOffset = f2_currentmetadataoffset, brokerHeartbeatRequestWantFence = f3_wantfence, brokerHeartbeatRequestWantShutDown = f4_wantshutdown, brokerHeartbeatRequestOfflineLogDirs = _tag_offlinelogdirs }, pTagsEnd)
  | version == 1 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokerepoch, p2) <- W.peekInt64BE p1 endPtr
    (f2_currentmetadataoffset, p3) <- W.peekInt64BE p2 endPtr
    (f3_wantfence, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    (f4_wantshutdown, p5) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p5 endPtr
    let !_tag_offlinelogdirs = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray WP.peekKafkaUuid p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (BrokerHeartbeatRequest { brokerHeartbeatRequestBrokerId = f0_brokerid, brokerHeartbeatRequestBrokerEpoch = f1_brokerepoch, brokerHeartbeatRequestCurrentMetadataOffset = f2_currentmetadataoffset, brokerHeartbeatRequestWantFence = f3_wantfence, brokerHeartbeatRequestWantShutDown = f4_wantshutdown, brokerHeartbeatRequestOfflineLogDirs = _tag_offlinelogdirs }, pTagsEnd)
  | otherwise = error $ "wirePeek BrokerHeartbeatRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec BrokerHeartbeatRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeBrokerHeartbeatRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeBrokerHeartbeatRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekBrokerHeartbeatRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}