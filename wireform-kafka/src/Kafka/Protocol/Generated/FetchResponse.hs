{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchResponse
Description : Kafka FetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 1.



Valid versions: 4-19
Flexible versions: 12+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchResponse
  (
    FetchResponse(..),
    FetchableTopicResponse(..),
    PartitionData(..),
    EpochEndOffset(..),
    LeaderIdAndEpoch(..),
    SnapshotId(..),
    AbortedTransaction(..),
    NodeEndpoint(..),
    maxFetchResponseVersion
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


-- | In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to div
data EpochEndOffset = EpochEndOffset
  {

  -- | The largest epoch.

  -- Versions: 12+
  epochEndOffsetEpoch :: !(Int32)
,

  -- | The end offset of the epoch.

  -- Versions: 12+
  epochEndOffsetEndOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | The current leader of the partition.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 12+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 12+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
data SnapshotId = SnapshotId
  {

  -- | The end offset of the epoch.

  -- Versions: 0+
  snapshotIdEndOffset :: !(Int64)
,

  -- | The largest epoch.

  -- Versions: 0+
  snapshotIdEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The aborted transactions.
data AbortedTransaction = AbortedTransaction
  {

  -- | The producer id associated with the aborted transaction.

  -- Versions: 4+
  abortedTransactionProducerId :: !(Int64)
,

  -- | The first offset in the aborted transaction.

  -- Versions: 4+
  abortedTransactionFirstOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | The topic partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no fetch error.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The current high water mark.

  -- Versions: 0+
  partitionDataHighWatermark :: !(Int64)
,

  -- | The last stable offset (or LSO) of the partition. This is the last offset such that the state of all

  -- Versions: 4+
  partitionDataLastStableOffset :: !(Int64)
,

  -- | The current log start offset.

  -- Versions: 5+
  partitionDataLogStartOffset :: !(Int64)
,

  -- | In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, thi

  -- Versions: 12+
  partitionDataDivergingEpoch :: !(EpochEndOffset)
,

  -- | The current leader of the partition.

  -- Versions: 12+
  partitionDataCurrentLeader :: !(LeaderIdAndEpoch)
,

  -- | In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch tha

  -- Versions: 12+
  partitionDataSnapshotId :: !(SnapshotId)
,

  -- | The aborted transactions.

  -- Versions: 4+
  partitionDataAbortedTransactions :: !(KafkaArray (AbortedTransaction))
,

  -- | The preferred read replica for the consumer to use on its next fetch request.

  -- Versions: 11+
  partitionDataPreferredReadReplica :: !(Int32)
,

  -- | The record data.

  -- Versions: 0+
  partitionDataRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | The response topics.
data FetchableTopicResponse = FetchableTopicResponse
  {

  -- | The topic name.

  -- Versions: 0-12
  fetchableTopicResponseTopic :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 13+
  fetchableTopicResponseTopicId :: !(KafkaUuid)
,

  -- | The topic partitions.

  -- Versions: 0+
  fetchableTopicResponsePartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)

-- | Endpoints for all current-leaders enumerated in PartitionData, with errors NOT_LEADER_OR_FOLLOWER & FENCED_LEADER_EPOCH.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 16+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 16+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 16+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 16+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data FetchResponse = FetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  fetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 7+
  fetchResponseErrorCode :: !(Int16)
,

  -- | The fetch session ID, or 0 if this is not part of a fetch session.

  -- Versions: 7+
  fetchResponseSessionId :: !(Int32)
,

  -- | The response topics.

  -- Versions: 0+
  fetchResponseResponses :: !(KafkaArray (FetchableTopicResponse))
,

  -- | Endpoints for all current-leaders enumerated in PartitionData, with errors NOT_LEADER_OR_FOLLOWER & 

  -- Versions: 16+
  fetchResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchResponse.
maxFetchResponseVersion :: Int16
maxFetchResponseVersion = 19

-- | KafkaMessage instance for FetchResponse.
instance KafkaMessage FetchResponse where
  messageApiKey = 1
  messageMinVersion = 4
  messageMaxVersion = 19
  messageFlexibleVersion = Just 12

-- | Worst-case wire size of a EpochEndOffset.
wireMaxSizeEpochEndOffset :: Int -> EpochEndOffset -> Int
wireMaxSizeEpochEndOffset _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for EpochEndOffset.
wirePokeEpochEndOffset :: Int -> Ptr Word8 -> EpochEndOffset -> IO (Ptr Word8)
wirePokeEpochEndOffset version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 12 then W.pokeInt32BE p0 (epochEndOffsetEpoch msg) else pure p0)
  p2 <- (if version >= 12 then W.pokeInt64BE p1 (epochEndOffsetEndOffset msg) else pure p1)
  if version >= 12 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for EpochEndOffset.
wirePeekEpochEndOffset :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EpochEndOffset, Ptr Word8)
wirePeekEpochEndOffset version _fp _basePtr p0 endPtr = do
  (f0_epoch, p1) <- (if version >= 12 then W.peekInt32BE p0 endPtr else pure (-1, p0))
  (f1_endoffset, p2) <- (if version >= 12 then W.peekInt64BE p1 endPtr else pure (-1, p1))
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (EpochEndOffset { epochEndOffsetEpoch = f0_epoch, epochEndOffsetEndOffset = f1_endoffset }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultEpochEndOffset :: EpochEndOffset
defaultEpochEndOffset = EpochEndOffset { epochEndOffsetEpoch = -1, epochEndOffsetEndOffset = -1 }

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
  p1 <- (if version >= 12 then W.pokeInt32BE p0 (leaderIdAndEpochLeaderId msg) else pure p0)
  p2 <- (if version >= 12 then W.pokeInt32BE p1 (leaderIdAndEpochLeaderEpoch msg) else pure p1)
  if version >= 12 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for LeaderIdAndEpoch.
wirePeekLeaderIdAndEpoch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderIdAndEpoch, Ptr Word8)
wirePeekLeaderIdAndEpoch version _fp _basePtr p0 endPtr = do
  (f0_leaderid, p1) <- (if version >= 12 then W.peekInt32BE p0 endPtr else pure (-1, p0))
  (f1_leaderepoch, p2) <- (if version >= 12 then W.peekInt32BE p1 endPtr else pure (-1, p1))
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = f0_leaderid, leaderIdAndEpochLeaderEpoch = f1_leaderepoch }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultLeaderIdAndEpoch :: LeaderIdAndEpoch
defaultLeaderIdAndEpoch = LeaderIdAndEpoch { leaderIdAndEpochLeaderId = -1, leaderIdAndEpochLeaderEpoch = -1 }

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
  if version >= 12 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for SnapshotId.
wirePeekSnapshotId :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SnapshotId, Ptr Word8)
wirePeekSnapshotId version _fp _basePtr p0 endPtr = do
  (f0_endoffset, p1) <- W.peekInt64BE p0 endPtr
  (f1_epoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (SnapshotId { snapshotIdEndOffset = f0_endoffset, snapshotIdEpoch = f1_epoch }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultSnapshotId :: SnapshotId
defaultSnapshotId = SnapshotId { snapshotIdEndOffset = -1, snapshotIdEpoch = -1 }

-- | Worst-case wire size of a AbortedTransaction.
wireMaxSizeAbortedTransaction :: Int -> AbortedTransaction -> Int
wireMaxSizeAbortedTransaction _version msg =
  0
  + 8
  + 8
  + 1

-- | Direct-poke encoder for AbortedTransaction.
wirePokeAbortedTransaction :: Int -> Ptr Word8 -> AbortedTransaction -> IO (Ptr Word8)
wirePokeAbortedTransaction version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 4 then W.pokeInt64BE p0 (abortedTransactionProducerId msg) else pure p0)
  p2 <- (if version >= 4 then W.pokeInt64BE p1 (abortedTransactionFirstOffset msg) else pure p1)
  if version >= 12 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AbortedTransaction.
wirePeekAbortedTransaction :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AbortedTransaction, Ptr Word8)
wirePeekAbortedTransaction version _fp _basePtr p0 endPtr = do
  (f0_producerid, p1) <- (if version >= 4 then W.peekInt64BE p0 endPtr else pure (0, p0))
  (f1_firstoffset, p2) <- (if version >= 4 then W.peekInt64BE p1 endPtr else pure (0, p1))
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AbortedTransaction { abortedTransactionProducerId = f0_producerid, abortedTransactionFirstOffset = f1_firstoffset }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAbortedTransaction :: AbortedTransaction
defaultAbortedTransaction = AbortedTransaction { abortedTransactionProducerId = 0, abortedTransactionFirstOffset = 0 }

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + 8
  + 8
  + 8
  + (if _version >= 12 then wireMaxSizeEpochEndOffset _version (partitionDataDivergingEpoch msg) + 10 else 0)
  + (if _version >= 12 then wireMaxSizeLeaderIdAndEpoch _version (partitionDataCurrentLeader msg) + 10 else 0)
  + (if _version >= 12 then wireMaxSizeSnapshotId _version (partitionDataSnapshotId msg) + 10 else 0)
  + (5 + (case P.unKafkaArray (partitionDataAbortedTransactions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAbortedTransaction _version x ) v); P.Null -> 0 }))
  + 4
  + WP.dualBytesMaxSize (partitionDataRecords msg)
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- W.pokeInt64BE p2 (partitionDataHighWatermark msg)
  p4 <- (if version >= 4 then W.pokeInt64BE p3 (partitionDataLastStableOffset msg) else pure p3)
  p5 <- (if version >= 5 then W.pokeInt64BE p4 (partitionDataLogStartOffset msg) else pure p4)
  p6 <- (if version >= 4 then WP.pokeVersionedNullableArray version 12 (\p x -> wirePokeAbortedTransaction version p x) p5 (partitionDataAbortedTransactions msg) else pure p5)
  p7 <- (if version >= 11 then W.pokeInt32BE p6 (partitionDataPreferredReadReplica msg) else pure p6)
  p8 <- (if version >= 12 then WP.pokeCompactBytes p7 (P.toCompactBytes (partitionDataRecords msg)) else WP.pokeKafkaBytes p7 (partitionDataRecords msg))
  if version >= 12 then do
    let !_taggedEntries = (if version >= 12 then [(0, W.runWirePokeWith (wireMaxSizeEpochEndOffset version (partitionDataDivergingEpoch msg)) (\p -> wirePokeEpochEndOffset version p (partitionDataDivergingEpoch msg)))] else []) ++ (if version >= 12 then [(1, W.runWirePokeWith (wireMaxSizeLeaderIdAndEpoch version (partitionDataCurrentLeader msg)) (\p -> wirePokeLeaderIdAndEpoch version p (partitionDataCurrentLeader msg)))] else []) ++ (if version >= 12 then [(2, W.runWirePokeWith (wireMaxSizeSnapshotId version (partitionDataSnapshotId msg)) (\p -> wirePokeSnapshotId version p (partitionDataSnapshotId msg)))] else [])
    WP.pokeTaggedFieldEntries p8 _taggedEntries
  else pure p8

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_highwatermark, p3) <- W.peekInt64BE p2 endPtr
  (f3_laststableoffset, p4) <- (if version >= 4 then W.peekInt64BE p3 endPtr else pure (-1, p3))
  (f4_logstartoffset, p5) <- (if version >= 5 then W.peekInt64BE p4 endPtr else pure (-1, p4))
  (f5_abortedtransactions, p6) <- (if version >= 4 then WP.peekVersionedNullableArray version 12 (\p e -> wirePeekAbortedTransaction version _fp _basePtr p e) p5 endPtr else pure (P.KafkaArray P.Null, p5))
  (f6_preferredreadreplica, p7) <- (if version >= 11 then W.peekInt32BE p6 endPtr else pure (-1, p6))
  (f7_records, p8) <- (if version >= 12 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p7 endPtr else WP.peekKafkaBytes p7 endPtr)
  (_taggedMap, pTagsEnd) <- if version >= 12 then WP.peekTaggedFieldsMap p8 endPtr else pure (Data.Map.Strict.empty, p8)
  let !_tag_divergingepoch = if version >= 12 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekEpochEndOffset version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> defaultEpochEndOffset}; Nothing -> defaultEpochEndOffset} else defaultEpochEndOffset
  let !_tag_currentleader = if version >= 12 then case Data.Map.Strict.lookup 1 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekLeaderIdAndEpoch version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> defaultLeaderIdAndEpoch}; Nothing -> defaultLeaderIdAndEpoch} else defaultLeaderIdAndEpoch
  let !_tag_snapshotid = if version >= 12 then case Data.Map.Strict.lookup 2 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> wirePeekSnapshotId version _fp _bp p e)) _bs of { Right _v -> _v ; Left _ -> defaultSnapshotId}; Nothing -> defaultSnapshotId} else defaultSnapshotId
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataHighWatermark = f2_highwatermark, partitionDataLastStableOffset = f3_laststableoffset, partitionDataLogStartOffset = f4_logstartoffset, partitionDataDivergingEpoch = _tag_divergingepoch, partitionDataCurrentLeader = _tag_currentleader, partitionDataSnapshotId = _tag_snapshotid, partitionDataAbortedTransactions = f5_abortedtransactions, partitionDataPreferredReadReplica = f6_preferredreadreplica, partitionDataRecords = f7_records }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataErrorCode = 0, partitionDataHighWatermark = 0, partitionDataLastStableOffset = -1, partitionDataLogStartOffset = -1, partitionDataDivergingEpoch = defaultEpochEndOffset, partitionDataCurrentLeader = defaultLeaderIdAndEpoch, partitionDataSnapshotId = defaultSnapshotId, partitionDataAbortedTransactions = P.KafkaArray P.Null, partitionDataPreferredReadReplica = -1, partitionDataRecords = P.KafkaBytes Null }

-- | Worst-case wire size of a FetchableTopicResponse.
wireMaxSizeFetchableTopicResponse :: Int -> FetchableTopicResponse -> Int
wireMaxSizeFetchableTopicResponse _version msg =
  0
  + WP.dualStringMaxSize (fetchableTopicResponseTopic msg)
  + 16
  + (5 + (case P.unKafkaArray (fetchableTopicResponsePartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FetchableTopicResponse.
wirePokeFetchableTopicResponse :: Int -> Ptr Word8 -> FetchableTopicResponse -> IO (Ptr Word8)
wirePokeFetchableTopicResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 12 then (if version >= 12 then WP.pokeCompactString p0 (P.toCompactString (fetchableTopicResponseTopic msg)) else WP.pokeKafkaString p0 (fetchableTopicResponseTopic msg)) else pure p0)
  p2 <- (if version >= 13 then WP.pokeKafkaUuid p1 (fetchableTopicResponseTopicId msg) else pure p1)
  p3 <- WP.pokeVersionedArray version 12 (\p x -> wirePokePartitionData version p x) p2 (fetchableTopicResponsePartitions msg)
  if version >= 12 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for FetchableTopicResponse.
wirePeekFetchableTopicResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchableTopicResponse, Ptr Word8)
wirePeekFetchableTopicResponse version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (if version <= 12 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 13 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitions, p3) <- WP.peekVersionedArray version 12 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (FetchableTopicResponse { fetchableTopicResponseTopic = f0_topic, fetchableTopicResponseTopicId = f1_topicid, fetchableTopicResponsePartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultFetchableTopicResponse :: FetchableTopicResponse
defaultFetchableTopicResponse = FetchableTopicResponse { fetchableTopicResponseTopic = P.KafkaString Null, fetchableTopicResponseTopicId = P.nullUuid, fetchableTopicResponsePartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a NodeEndpoint.
wireMaxSizeNodeEndpoint :: Int -> NodeEndpoint -> Int
wireMaxSizeNodeEndpoint _version msg =
  0
  + 4
  + WP.dualStringMaxSize (nodeEndpointHost msg)
  + 4
  + WP.dualStringMaxSize (nodeEndpointRack msg)
  + 1

-- | Direct-poke encoder for NodeEndpoint.
wirePokeNodeEndpoint :: Int -> Ptr Word8 -> NodeEndpoint -> IO (Ptr Word8)
wirePokeNodeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 16 then W.pokeInt32BE p0 (nodeEndpointNodeId msg) else pure p0)
  p2 <- (if version >= 16 then (if version >= 12 then WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg)) else WP.pokeKafkaString p1 (nodeEndpointHost msg)) else pure p1)
  p3 <- (if version >= 16 then W.pokeInt32BE p2 (nodeEndpointPort msg) else pure p2)
  p4 <- (if version >= 16 then (if version >= 12 then WP.pokeCompactString p3 (P.toCompactString (nodeEndpointRack msg)) else WP.pokeKafkaString p3 (nodeEndpointRack msg)) else pure p3)
  if version >= 12 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- (if version >= 16 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_host, p2) <- (if version >= 16 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_port, p3) <- (if version >= 16 then W.peekInt32BE p2 endPtr else pure (0, p2))
  (f3_rack, p4) <- (if version >= 16 then (if version >= 12 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port, nodeEndpointRack = f3_rack }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultNodeEndpoint :: NodeEndpoint
defaultNodeEndpoint = NodeEndpoint { nodeEndpointNodeId = 0, nodeEndpointHost = P.KafkaString Null, nodeEndpointPort = 0, nodeEndpointRack = P.KafkaString Null }

-- | Worst-case wire size of a FetchResponse.
wireMaxSizeFetchResponse :: Int -> FetchResponse -> Int
wireMaxSizeFetchResponse _version msg =
  0
  + 4
  + 2
  + 4
  + (5 + (case P.unKafkaArray (fetchResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFetchableTopicResponse _version x ) v); P.Null -> 0 }))
  + (if _version >= 16 then (5 + (case P.unKafkaArray (fetchResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 })) + 10 else 0)
  + 1

-- | Direct-poke encoder for FetchResponse.
wirePokeFetchResponse :: Int -> Ptr Word8 -> FetchResponse -> IO (Ptr Word8)
wirePokeFetchResponse version basePtr msg
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (fetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchableTopicResponse version p x) p1 (fetchResponseResponses msg)
    pure p2
  | version >= 12 && version <= 15 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (fetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version >= 7 then W.pokeInt16BE p1 (fetchResponseErrorCode msg) else pure p1)
    p3 <- (if version >= 7 then W.pokeInt32BE p2 (fetchResponseSessionId msg) else pure p2)
    p4 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchableTopicResponse version p x) p3 (fetchResponseResponses msg)
    let !_taggedEntries = (if version >= 16 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (fetchResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (fetchResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p4 _taggedEntries
  | version >= 16 && version <= 19 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (fetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version >= 7 then W.pokeInt16BE p1 (fetchResponseErrorCode msg) else pure p1)
    p3 <- (if version >= 7 then W.pokeInt32BE p2 (fetchResponseSessionId msg) else pure p2)
    p4 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchableTopicResponse version p x) p3 (fetchResponseResponses msg)
    let !_taggedEntries = (if version >= 16 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (fetchResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeNodeEndpoint version p_ x) p (fetchResponseNodeEndpoints msg)))] else [])
    WP.pokeTaggedFieldEntries p4 _taggedEntries
  | version >= 7 && version <= 11 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (fetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version >= 7 then W.pokeInt16BE p1 (fetchResponseErrorCode msg) else pure p1)
    p3 <- (if version >= 7 then W.pokeInt32BE p2 (fetchResponseSessionId msg) else pure p2)
    p4 <- WP.pokeVersionedArray version 12 (\p x -> wirePokeFetchableTopicResponse version p x) p3 (fetchResponseResponses msg)
    pure p4
  | otherwise = error $ "wirePoke FetchResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for FetchResponse.
wirePeekFetchResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchResponse, Ptr Word8)
wirePeekFetchResponse version _fp _basePtr p0 endPtr
  | version >= 4 && version <= 6 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_responses, p2) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchableTopicResponse version _fp _basePtr p e) p1 endPtr
    pure (FetchResponse { fetchResponseThrottleTimeMs = f0_throttletimems, fetchResponseErrorCode = 0, fetchResponseSessionId = 0, fetchResponseResponses = f1_responses, fetchResponseNodeEndpoints = P.mkKafkaArray V.empty }, p2)
  | version >= 12 && version <= 15 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- (if version >= 7 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_sessionid, p3) <- (if version >= 7 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_responses, p4) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchableTopicResponse version _fp _basePtr p e) p3 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p4 endPtr
    let !_tag_nodeendpoints = if version >= 16 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (FetchResponse { fetchResponseThrottleTimeMs = f0_throttletimems, fetchResponseErrorCode = f1_errorcode, fetchResponseSessionId = f2_sessionid, fetchResponseResponses = f3_responses, fetchResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | version >= 16 && version <= 19 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- (if version >= 7 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_sessionid, p3) <- (if version >= 7 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_responses, p4) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchableTopicResponse version _fp _basePtr p e) p3 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p4 endPtr
    let !_tag_nodeendpoints = if version >= 16 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekNodeEndpoint version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    pure (FetchResponse { fetchResponseThrottleTimeMs = f0_throttletimems, fetchResponseErrorCode = f1_errorcode, fetchResponseSessionId = f2_sessionid, fetchResponseResponses = f3_responses, fetchResponseNodeEndpoints = _tag_nodeendpoints }, pTagsEnd)
  | version >= 7 && version <= 11 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- (if version >= 7 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_sessionid, p3) <- (if version >= 7 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_responses, p4) <- WP.peekVersionedArray version 12 (\p e -> wirePeekFetchableTopicResponse version _fp _basePtr p e) p3 endPtr
    pure (FetchResponse { fetchResponseThrottleTimeMs = f0_throttletimems, fetchResponseErrorCode = f1_errorcode, fetchResponseSessionId = f2_sessionid, fetchResponseResponses = f3_responses, fetchResponseNodeEndpoints = P.mkKafkaArray V.empty }, p4)
  | otherwise = error $ "wirePeek FetchResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec FetchResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFetchResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFetchResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFetchResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}