-- | Avro schemas for Apache Iceberg manifest files and manifest lists.
--
-- The Iceberg specification defines manifest entries and manifest file
-- (manifest list) entries as Avro records. This module constructs the
-- standard 'AvroType' schemas for these structures.
--
-- Also provides SIMD-accelerated helpers for manifest file pruning
-- (comparing serialized partition bounds against filter predicates).
module Iceberg.Manifest
  ( manifestEntrySchema
  , manifestFileSchema
  , filterBoundsMask
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import Wireform.FFI (compareBoundsBS)

-- | Avro schema for @manifest_entry@ records, as defined by the Iceberg
-- specification. Each manifest entry describes a single data or delete file.
--
-- The numeric @field-id@ values are spec-mandated (Iceberg Table Spec
-- 'Manifests' section). Every nested field in the manifest schema
-- must carry its 'field-id' Avro property; engines like
-- Spark / Trino reject manifests that lack them.
manifestEntrySchema :: AvroType
manifestEntrySchema = AvroRecord
  { avroRecordName      = "manifest_entry"
  , avroRecordNamespace  = Just "org.apache.iceberg"
  , avroRecordDoc        = Just "Entry in an Iceberg manifest file"
  , avroRecordAliases    = V.empty
  , avroRecordProps      = Map.empty
  , avroRecordFields     = V.fromList
      [ mkField    0 "status"               (AvroPrimitive AvroInt)  (Just "File status: 0=existing, 1=added, 2=deleted")
      , mkFieldOpt 1 "snapshot_id"          (AvroPrimitive AvroLong) (Just "Snapshot that added the file")
      , mkFieldOpt 3 "sequence_number"      (AvroPrimitive AvroLong) (Just "Data sequence number")
      , mkFieldOpt 4 "file_sequence_number" (AvroPrimitive AvroLong) (Just "File sequence number")
      , mkField    2 "data_file"            dataFileSchema           Nothing
      ]
  }

-- | Avro schema for the @data_file@ struct embedded in
-- @manifest_entry@. Wire order matches the Iceberg Java reference
-- writer (the test fixtures in @test\/Test\/Iceberg.hs@ depend on
-- this); 'content' is appended at the end since it was added in v2.
dataFileSchema :: AvroType
dataFileSchema = AvroRecord
  { avroRecordName      = "data_file"
  , avroRecordNamespace  = Just "org.apache.iceberg"
  , avroRecordDoc        = Just "Description of a data file"
  , avroRecordAliases    = V.empty
  , avroRecordProps      = Map.empty
  , avroRecordFields     = V.fromList
      [ mkField    100 "file_path"        (AvroPrimitive AvroString) (Just "Full URI of data file")
      , mkField    101 "file_format"      (AvroPrimitive AvroString) (Just "File format: avro, parquet, or orc")
      , mkField    102 "partition"
          (AvroRecord
            { avroRecordName      = "partition_data"
            , avroRecordNamespace  = Just "org.apache.iceberg"
            , avroRecordDoc        = Nothing
            , avroRecordAliases    = V.empty
            , avroRecordProps      = Map.empty
            , avroRecordFields     = V.empty
            }) Nothing
      , mkField    103 "record_count"        (AvroPrimitive AvroLong) (Just "Number of records in file")
      , mkField    104 "file_size_in_bytes"  (AvroPrimitive AvroLong) (Just "Total file size in bytes")
      , mkField    105 "block_size_in_bytes" (AvroPrimitive AvroLong) Nothing
      , mkFieldOpt 108 "column_sizes"
          (AvroMap { avroMapValues = AvroPrimitive AvroLong })  (Just "Map from column id to size")
      , mkFieldOpt 109 "value_counts"
          (AvroMap { avroMapValues = AvroPrimitive AvroLong })  (Just "Map from column id to value count")
      , mkFieldOpt 110 "null_value_counts"
          (AvroMap { avroMapValues = AvroPrimitive AvroLong })  (Just "Map from column id to null count")
      , mkFieldOpt 125 "lower_bounds"
          (AvroMap { avroMapValues = AvroPrimitive AvroBytes }) (Just "Map from column id to lower bound")
      , mkFieldOpt 128 "upper_bounds"
          (AvroMap { avroMapValues = AvroPrimitive AvroBytes }) (Just "Map from column id to upper bound")
      -- v2 added 'content' (0 = data, 1 = position deletes, 2 = equality deletes).
      -- Append at the end to match the Iceberg Java writer's wire order.
      , mkField 134 "content"
          (AvroPrimitive AvroInt) (Just "0=data, 1=position deletes, 2=equality deletes")
      ]
  }

-- | Avro schema for manifest list entries (@manifest_file@ records),
-- as defined by the Iceberg specification. Field IDs are
-- spec-mandated (Iceberg Table Spec 'Manifest Lists' section).
manifestFileSchema :: AvroType
manifestFileSchema = AvroRecord
  { avroRecordName      = "manifest_file"
  , avroRecordNamespace  = Just "org.apache.iceberg"
  , avroRecordDoc        = Just "Entry in an Iceberg manifest list"
  , avroRecordAliases    = V.empty
  , avroRecordProps      = Map.empty
  , avroRecordFields     = V.fromList
      [ mkField    500 "manifest_path"               (AvroPrimitive AvroString) (Just "Location of the manifest file")
      , mkField    501 "manifest_length"             (AvroPrimitive AvroLong)   (Just "Length of the manifest file")
      , mkField    502 "partition_spec_id"           (AvroPrimitive AvroInt)    (Just "ID of the partition spec")
      , mkField    517 "content"                     (AvroPrimitive AvroInt)    (Just "0=data, 1=deletes")
      , mkField    515 "sequence_number"             (AvroPrimitive AvroLong)   (Just "Sequence number when manifest was written")
      , mkField    516 "min_sequence_number"         (AvroPrimitive AvroLong)   (Just "Minimum data sequence number in manifest")
      , mkField    503 "added_snapshot_id"           (AvroPrimitive AvroLong)   (Just "Snapshot ID that added the manifest")
      , mkFieldOpt 504 "added_data_files_count"      (AvroPrimitive AvroInt)    Nothing
      , mkFieldOpt 505 "existing_data_files_count"   (AvroPrimitive AvroInt)    Nothing
      , mkFieldOpt 506 "deleted_data_files_count"    (AvroPrimitive AvroInt)    Nothing
      , mkFieldOpt 512 "added_rows_count"            (AvroPrimitive AvroLong)   Nothing
      , mkFieldOpt 513 "existing_rows_count"         (AvroPrimitive AvroLong)   Nothing
      , mkFieldOpt 514 "deleted_rows_count"          (AvroPrimitive AvroLong)   Nothing
      ]
  }

-- ============================================================
-- Helpers
-- ============================================================

-- | Build a required field with its spec-mandated Iceberg @field-id@.
mkField :: Int -> String -> AvroType -> Maybe String -> AvroField
mkField fid name ty doc = AvroField
  { avroFieldName    = T.pack name
  , avroFieldType    = ty
  , avroFieldDefault = Nothing
  , avroFieldOrder   = Nothing
  , avroFieldAliases = V.empty
  , avroFieldDoc     = fmap T.pack doc
  , avroFieldProps   = Map.singleton "field-id" (T.pack (show fid))
  }

-- | Build an optional ([null, T] union) field with the spec-mandated
-- Iceberg @field-id@.
mkFieldOpt :: Int -> String -> AvroType -> Maybe String -> AvroField
mkFieldOpt fid name ty doc = AvroField
  { avroFieldName    = T.pack name
  , avroFieldType    = AvroUnion { avroUnionBranches = V.fromList [AvroPrimitive AvroNull, ty] }
  , avroFieldDefault = Just AvroNull
  , avroFieldOrder   = Nothing
  , avroFieldAliases = V.empty
  , avroFieldDoc     = fmap T.pack doc
  , avroFieldProps   = Map.singleton "field-id" (T.pack (show fid))
  }

-- | Bulk-compare a search value against @N@ serialized partition bounds
-- (all the same @width@ bytes each, packed contiguously in @bounds@).
--
-- Returns a bitmask where bit @i@ is set if @bounds[i] <= search@.
-- For 4-byte LE int32 bounds (common for Iceberg int32 columns), this
-- uses SSE2 to compare 4 bounds at once.
--
-- @bounds@ must contain exactly @count * width@ bytes.
-- @search@ must contain exactly @width@ bytes.
filterBoundsMask
  :: ByteString  -- ^ Packed serialized bounds, @count * width@ bytes
  -> Int         -- ^ Number of bounds
  -> Int         -- ^ Width of each bound in bytes
  -> ByteString  -- ^ Search value, @width@ bytes
  -> Int         -- ^ Bitmask result
filterBoundsMask = compareBoundsBS
{-# INLINE filterBoundsMask #-}
