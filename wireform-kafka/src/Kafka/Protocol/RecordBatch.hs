{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- \| This module holds the on-the-wire data types + constants + the
-- attribute bit packers. Encoders / decoders live in
-- "Kafka.Protocol.RecordBatchWire" — see 'RBW.encodeRecordBatchWire'
-- and 'RBW.decodeRecordBatchWireWithDecompression'.
--
-- Callers (Wire codec, tests, mock clusters) manipulate batches
-- via the data types here without rebuilding the type
-- system.

{- |
Module      : Kafka.Protocol.RecordBatch
Description : Kafka RecordBatch v2 format encoding and decoding
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the Kafka RecordBatch v2 format (magic byte 2), which is
the current standard format for storing and transmitting records in Kafka.

= RecordBatch Format

The RecordBatch v2 format has the following structure:

@
RecordBatch =>
  BaseOffset => Int64
  Length => Int32
  PartitionLeaderEpoch => Int32
  Magic => Int8 (value = 2)
  CRC => Uint32
  Attributes => Int16
  LastOffsetDelta => Int32
  BaseTimestamp => Int64
  MaxTimestamp => Int64
  ProducerId => Int64
  ProducerEpoch => Int16
  BaseSequence => Int32
  RecordsCount => Int32
  Records => [Record]
@

The CRC covers the data from the attributes to the end of the batch (i.e., all
bytes following the CRC). It uses the CRC-32C (Castagnoli) polynomial.

= Attributes Field

The attributes field (16 bits) contains:

* Bits 0-2: Compression type (0=none, 1=gzip, 2=snappy, 3=lz4, 4=zstd)
* Bit 3: Timestamp type (0=CreateTime, 1=LogAppendTime)
* Bit 4: Transactional flag (0=non-transactional, 1=transactional)
* Bit 5: Control flag (0=normal, 1=control)
* Bit 6: Delete horizon flag (0=no, 1=yes)
* Bits 7-15: Unused (must be 0)

= Records

Individual records within a batch use a different format from the batch header.
See 'Record' for details.
-}
module Kafka.Protocol.RecordBatch (
  -- * RecordBatch Types
  RecordBatch (..),
  Record (..),
  RecordHeader (..),

  -- * Batch Construction
  mkRecordBatch,
  mkSimpleBatch,

  -- * Batch Attributes
  TimestampType (..),
  Attributes (..),
  mkAttributes,
  defaultAttributes,
  encodeAttributes,
  decodeAttributes,

  -- * Constants
  magicV2,
  noProducerId,
  noProducerEpoch,
  noSequence,
  noPartitionLeaderEpoch,
  noTimestamp,

  -- * Utilities
  calculateBatchSize,
  recordBatchOverhead,
) where

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int
import Data.Vector qualified as V
import Data.Word
import GHC.Generics (Generic)
import Kafka.Compression.Types qualified as Compression


-- | Magic byte value for RecordBatch v2 format.
magicV2 :: Int8
magicV2 = 2


-- | Value indicating no producer ID is set.
noProducerId :: Int64
noProducerId = -1


-- | Value indicating no producer epoch is set.
noProducerEpoch :: Int16
noProducerEpoch = -1


-- | Value indicating no sequence number is set.
noSequence :: Int32
noSequence = -1


-- | Value indicating no partition leader epoch is set.
noPartitionLeaderEpoch :: Int32
noPartitionLeaderEpoch = -1


-- | Value indicating no timestamp is set.
noTimestamp :: Int64
noTimestamp = -1


-- | Fixed overhead of the RecordBatch header (61 bytes).
recordBatchOverhead :: Int
recordBatchOverhead = 61


-- -----------------------------------------------------------------------------
-- Timestamp Type
-- -----------------------------------------------------------------------------

-- | Timestamp type for records.
data TimestampType
  = -- | Timestamp set by the producer
    CreateTime
  | -- | Timestamp set by the broker when appended
    LogAppendTime
  deriving (Eq, Show, Ord, Generic)


-- -----------------------------------------------------------------------------
-- Attributes
-- -----------------------------------------------------------------------------

-- | Attributes field of a RecordBatch.
data Attributes = Attributes
  { attrCompressionType :: !Compression.CompressionCodec
  -- ^ Compression codec used for the records
  , attrTimestampType :: !TimestampType
  -- ^ How timestamps are assigned
  , attrIsTransactional :: !Bool
  -- ^ Whether this batch is part of a transaction
  , attrIsControl :: !Bool
  -- ^ Whether this is a control batch
  , attrHasDeleteHorizon :: !Bool
  -- ^ Whether the delete horizon flag is set
  }
  deriving (Eq, Show, Generic)


-- | Create attributes with the specified values.
mkAttributes
  :: Compression.CompressionCodec
  -> TimestampType
  -> Bool
  -- ^ Transactional
  -> Bool
  -- ^ Control
  -> Bool
  -- ^ Delete horizon
  -> Attributes
mkAttributes = Attributes


-- | Default attributes: no compression, create time, non-transactional, non-control.
defaultAttributes :: Attributes
defaultAttributes =
  Attributes
    { attrCompressionType = Compression.NoCompression
    , attrTimestampType = CreateTime
    , attrIsTransactional = False
    , attrIsControl = False
    , attrHasDeleteHorizon = False
    }


-- | Encode attributes to 16-bit integer.
encodeAttributes :: Attributes -> Int16
encodeAttributes Attributes {..} =
  let compressionBits = fromIntegral (Compression.codecId attrCompressionType) .&. 0x07
      timestampBit = if attrTimestampType == LogAppendTime then 0x08 else 0x00
      transactionalBit = if attrIsTransactional then 0x10 else 0x00
      controlBit = if attrIsControl then 0x20 else 0x00
      deleteHorizonBit = if attrHasDeleteHorizon then 0x40 else 0x00
  in compressionBits .|. timestampBit .|. transactionalBit .|. controlBit .|. deleteHorizonBit


-- | Decode attributes from 16-bit integer.
decodeAttributes :: Int16 -> Either String Attributes
decodeAttributes attrs =
  let compressionId = fromIntegral (attrs .&. 0x07) :: Int8
      timestampType = if (attrs .&. 0x08) /= 0 then LogAppendTime else CreateTime
      isTransactional = (attrs .&. 0x10) /= 0
      isControl = (attrs .&. 0x20) /= 0
      hasDeleteHorizon = (attrs .&. 0x40) /= 0
      compressionCodec = case compressionId of
        0 -> Compression.NoCompression
        1 -> Compression.Gzip
        2 -> Compression.Snappy
        3 -> Compression.Lz4
        4 -> Compression.Zstd
        _ -> Compression.NoCompression -- Unknown codec, default to none
  in Right $ Attributes compressionCodec timestampType isTransactional isControl hasDeleteHorizon


-- -----------------------------------------------------------------------------
-- Record Header
-- -----------------------------------------------------------------------------

-- | A header key-value pair within a record.
data RecordHeader = RecordHeader
  { headerKey :: !ByteString
  -- ^ Header key
  , headerValue :: !(Maybe ByteString)
  -- ^ Header value (nullable)
  }
  deriving (Eq, Show, Generic)


-- -----------------------------------------------------------------------------
-- Record
-- -----------------------------------------------------------------------------

{- | An individual record within a RecordBatch.

Records are encoded with variable-length integers using ZigZag encoding:

@
Record =>
  Length => VarInt
  Attributes => Int8
  TimestampDelta => VarLong
  OffsetDelta => VarInt
  KeyLength => VarInt (-1 for null)
  Key => Bytes
  ValueLength => VarInt (-1 for null)
  Value => Bytes
  HeadersCount => VarInt
  Headers => [RecordHeader]
@
-}
data Record = Record
  { recordTimestampDelta :: !Int64
  -- ^ Timestamp delta from batch base timestamp
  , recordOffsetDelta :: !Int32
  -- ^ Offset delta from batch base offset
  , recordKey :: !(Maybe ByteString)
  -- ^ Record key (nullable)
  , recordValue :: !(Maybe ByteString)
  -- ^ Record value; 'Nothing' is a null value (a tombstone)
  -- ^ Record value
  , recordHeaders :: ![RecordHeader]
  -- ^ Record headers
  }
  deriving (Eq, Show, Generic)


-- -----------------------------------------------------------------------------
-- RecordBatch
-- -----------------------------------------------------------------------------

-- | A RecordBatch in v2 format (magic byte 2).
data RecordBatch = RecordBatch
  { batchBaseOffset :: !Int64
  -- ^ Base offset of this batch
  , batchPartitionLeaderEpoch :: !Int32
  -- ^ Partition leader epoch (set by broker, use 'noPartitionLeaderEpoch' for producer)
  , batchAttributes :: !Attributes
  -- ^ Batch attributes
  , batchLastOffsetDelta :: !Int32
  -- ^ Offset delta of the last record in the batch
  , batchBaseTimestamp :: !Int64
  -- ^ Base timestamp for the batch
  , batchMaxTimestamp :: !Int64
  -- ^ Maximum timestamp in the batch
  , batchProducerId :: !Int64
  -- ^ Producer ID (for idempotent/transactional producers)
  , batchProducerEpoch :: !Int16
  -- ^ Producer epoch
  , batchBaseSequence :: !Int32
  -- ^ Base sequence number
  , batchRecords :: !(V.Vector Record)
  -- ^ Records in this batch
  }
  deriving (Eq, Show, Generic)


-- | Create a RecordBatch with specified parameters.
mkRecordBatch
  :: Int64
  -- ^ Base offset
  -> Int32
  -- ^ Partition leader epoch
  -> Attributes
  -- ^ Attributes
  -> Int64
  -- ^ Base timestamp
  -> Int64
  -- ^ Producer ID
  -> Int16
  -- ^ Producer epoch
  -> Int32
  -- ^ Base sequence
  -> V.Vector Record
  -- ^ Records
  -> RecordBatch
mkRecordBatch baseOffset leaderEpoch attrs baseTimestamp producerId producerEpoch baseSequence records =
  let lastOffsetDelta = if V.null records then 0 else fromIntegral (V.length records) - 1
      maxTimestamp =
        if V.null records
          then baseTimestamp
          else maximum (baseTimestamp : V.toList (V.map (\r -> baseTimestamp + recordTimestampDelta r) records))
  in RecordBatch
       { batchBaseOffset = baseOffset
       , batchPartitionLeaderEpoch = leaderEpoch
       , batchAttributes = attrs
       , batchLastOffsetDelta = lastOffsetDelta
       , batchBaseTimestamp = baseTimestamp
       , batchMaxTimestamp = maxTimestamp
       , batchProducerId = producerId
       , batchProducerEpoch = producerEpoch
       , batchBaseSequence = baseSequence
       , batchRecords = records
       }


-- | Create a simple RecordBatch with default values for non-idempotent producer.
mkSimpleBatch
  :: Int64
  -- ^ Base offset
  -> Int64
  -- ^ Base timestamp
  -> V.Vector Record
  -- ^ Records
  -> RecordBatch
mkSimpleBatch baseOffset baseTimestamp records =
  mkRecordBatch
    baseOffset
    noPartitionLeaderEpoch
    defaultAttributes
    baseTimestamp
    noProducerId
    noProducerEpoch
    noSequence
    records


-- -----------------------------------------------------------------------------
-- CRC32C Checksum
-- -----------------------------------------------------------------------------

{- | Calculate CRC32C checksum using Castagnoli polynomial.

-----------------------------------------------------------------------------
Worst-case batch-size estimator
-----------------------------------------------------------------------------
-}

{- | Worst-case upper bound on the wire size of a 'RecordBatch'.
Sums the per-record overhead + payload bytes; uses the worst-
case varint width (5 bytes for 'VarInt', 10 for 'VarLong') so
the result is a permissive over-estimate (typically a handful
of bytes per record). Used by buffer pre-sizing on the caller
side, so an over-estimate is exactly the right call.
-}
calculateBatchSize :: RecordBatch -> Int
calculateBatchSize RecordBatch {..} =
  let !recordsBytes = sum (fmap recordWireUpperBound batchRecords)
  in recordBatchOverhead + recordsBytes
  where
    recordWireUpperBound :: Record -> Int
    recordWireUpperBound Record {..} =
      let !keyLen = maybe 0 BS.length recordKey
          !valLen = maybe 0 BS.length recordValue
          !hdrBytes =
            sum
              [ 5
                  + BS.length headerKey
                  + 5
                  + maybe 0 BS.length headerValue
              | RecordHeader {..} <- recordHeaders
              ]
      in 5 -- outer length varint
           + 1 -- record attributes byte
           + 10 -- timestamp delta varint (worst case)
           + 5 -- offset delta varint (worst case)
           + 5
           + keyLen
           + 5
           + valLen
           + 5
           + hdrBytes
