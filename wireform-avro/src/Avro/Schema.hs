-- | Avro schema abstract syntax tree.
--
-- Represents the full Avro schema specification including primitive types,
-- complex types (records, enums, arrays, maps, unions, fixed), and logical
-- types (decimal, date, time, timestamp, duration, uuid).
--
-- Data types use strict fields with @UNPACK@ where appropriate for
-- cache-friendly, allocation-light representations.  All names use 'Text'.
module Avro.Schema
  ( -- * Schema types
    AvroSchema (..)
  , AvroField (..)
  , AvroType (..)
  , SortOrder (..)
  , LogicalType (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson

-- | Sort order for record fields, used by Avro's sort-order comparison.
data SortOrder
  = Ascending
  | Descending
  | Ignore
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Avro logical types layered on top of primitive or fixed types.
data LogicalType
  = DecimalLogical {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    -- ^ @decimal(precision, scale)@ — annotates @bytes@ or @fixed@
  | DateLogical
    -- ^ @date@ — days since Unix epoch, annotates @int@
  | TimeMillisLogical
    -- ^ @time-millis@ — milliseconds since midnight, annotates @int@
  | TimeMicrosLogical
    -- ^ @time-micros@ — microseconds since midnight, annotates @long@
  | TimestampMillisLogical
    -- ^ @timestamp-millis@ — milliseconds since Unix epoch, annotates @long@
  | TimestampMicrosLogical
    -- ^ @timestamp-micros@ — microseconds since Unix epoch, annotates @long@
  | DurationLogical
    -- ^ @duration@ — annotates a 12-byte @fixed@ (months, days, millis as 3 LE uint32s)
  | UuidLogical
    -- ^ @uuid@ — annotates @string@
  | CustomLogical !Text
    -- ^ An unknown\/user-defined logical type, preserved for registry-driven codegen.
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | A single field within an Avro record schema.
--
-- 'avroFieldDefault' is the field's declared default JSON value (Avro
-- 1.11 \§schema-record), used by schema resolution when the writer
-- omits this field. The previous representation was a useless
-- @Maybe AvroSchema@ tag that always evaluated to a hard-coded zero;
-- 'Maybe Aeson.Value' keeps the real JSON literal so the resolver can
-- interpret it under the field's reader type.
data AvroField = AvroField
  { avroFieldName    :: !Text
  , avroFieldType    :: !AvroType
  , avroFieldDefault :: !(Maybe Aeson.Value)
  , avroFieldOrder   :: !(Maybe SortOrder)
  , avroFieldAliases :: !(Vector Text)
  , avroFieldDoc     :: !(Maybe Text)
  , avroFieldProps   :: !(Map Text Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Core Avro schema AST.
--
-- A schema is either an 'AvroPrimitive' type reference, a 'AvroLogical' type
-- wrapping a base type, or one of the complex types (record, enum, array,
-- map, union, fixed).  The 'AvroSchemaRef' constructor handles named type
-- references within unions and recursive schemas.
data AvroSchema
  = AvroNull
    -- ^ The null type: no value, zero bytes on the wire.
  | AvroBool
    -- ^ Boolean: single byte 0x00 or 0x01.
  | AvroInt
    -- ^ 32-bit signed integer (ZigZag + varint on the wire).
  | AvroLong
    -- ^ 64-bit signed integer (ZigZag + varint on the wire).
  | AvroFloat
    -- ^ 32-bit IEEE 754 float, little-endian.
  | AvroDouble
    -- ^ 64-bit IEEE 754 double, little-endian.
  | AvroBytes
    -- ^ Arbitrary byte sequence, length-prefixed.
  | AvroString
    -- ^ UTF-8 string, length-prefixed.
  | AvroSchemaRef !Text
    -- ^ Named reference to another schema (for recursive types).
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Avro type, encompassing primitives, complex types, and logical types.
data AvroType
  = AvroPrimitive !AvroSchema
    -- ^ A primitive schema used as a type.
  | AvroRecord
      { avroRecordName      :: !Text
      , avroRecordNamespace :: !(Maybe Text)
      , avroRecordDoc       :: !(Maybe Text)
      , avroRecordAliases   :: !(Vector Text)
      , avroRecordFields    :: !(Vector AvroField)
      , avroRecordProps     :: !(Map Text Text)
      }
    -- ^ Record: ordered list of named, typed fields.
  | AvroEnum
      { avroEnumName      :: !Text
      , avroEnumNamespace :: !(Maybe Text)
      , avroEnumDoc       :: !(Maybe Text)
      , avroEnumAliases   :: !(Vector Text)
      , avroEnumSymbols   :: !(Vector Text)
      , avroEnumDefault   :: !(Maybe Text)
      }
    -- ^ Enum: ordered list of symbolic names, encoded as a varint index.
  | AvroArray
      { avroArrayItems :: !AvroType
      }
    -- ^ Array: homogeneous sequence, encoded as blocks of (count, items).
  | AvroMap
      { avroMapValues :: !AvroType
      }
    -- ^ Map: string-keyed, encoded as blocks of (count, key-value pairs).
  | AvroUnion
      { avroUnionBranches :: !(Vector AvroType)
      }
    -- ^ Union: discriminated by a varint branch index on the wire.
  | AvroFixed
      { avroFixedName      :: !Text
      , avroFixedNamespace :: !(Maybe Text)
      , avroFixedSize      :: {-# UNPACK #-} !Int
      , avroFixedAliases   :: !(Vector Text)
      }
    -- ^ Fixed: fixed number of bytes, no length prefix on the wire.
  | AvroLogical
      { avroLogicalBase :: !AvroType
      , avroLogicalType :: !LogicalType
      }
    -- ^ Logical type layered on a base type.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
