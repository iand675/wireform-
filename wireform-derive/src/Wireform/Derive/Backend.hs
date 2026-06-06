-- | Identifier for a serialization backend.
--
-- A 'Backend' is a textual tag attached to per-backend modifier overrides
-- (e.g. @forBackend backendProto (rename "n")@) and consulted by each
-- per-format deriver to decide which modifiers apply.
--
-- New backends can be defined by downstream packages: just construct a
-- 'Backend' with a fresh identifier. The set of \"standard\" backends
-- defined here is open-ended and exists only as a convenience so that
-- common cases compose cleanly.
module Wireform.Derive.Backend
  ( Backend (..)
    -- * Standard backends
  , backendProto
  , backendCBOR
  , backendMsgPack
  , backendThrift
  , backendJSON
  , backendEDN
  , backendEDI
  , backendTOML
  , backendYAML
  , backendXML
  , backendHTML
  , backendCSV
  , backendNDJSON
  , backendBinary
  , backendTextFormat

    -- * Schema-driven backends
  , backendASN1
  , backendAvro
  , backendBond
  , backendFlatBuffers
  , backendCapnProto

    -- * Document / object stores
  , backendBSON
  , backendBencode
  , backendION

    -- * Cross-language object serialization
  , backendFory

    -- * Columnar / tabular
  , backendArrow
  , backendParquet
  , backendOrc
  , backendIceberg
  ) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Open identifier for a serialization backend.
--
-- 'Backend' has an 'IsString' instance, so backends can be written
-- inline as string literals, e.g. @forBackend "proto" (rename "n")@.
-- Prefer the named constants below where they exist so that typos are
-- caught at compile time.
newtype Backend = Backend { unBackend :: Text }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving newtype (IsString, Hashable, NFData)

-- | Protocol Buffers wire encoding.
backendProto :: Backend
backendProto = Backend "proto"

-- | RFC 8949 CBOR.
backendCBOR :: Backend
backendCBOR = Backend "cbor"

-- | MessagePack.
backendMsgPack :: Backend
backendMsgPack = Backend "msgpack"

-- | Apache Thrift binary protocol.
backendThrift :: Backend
backendThrift = Backend "thrift"

-- | JSON (used by both Aeson-style and Proto3 JSON mappings;
-- Proto-JSON-specific overrides should additionally tag with
-- 'backendProto').
backendJSON :: Backend
backendJSON = Backend "json"

-- | Extensible Data Notation (Clojure / EDN).
backendEDN :: Backend
backendEDN = Backend "edn"

-- | Electronic Data Interchange segment streams.
backendEDI :: Backend
backendEDI = Backend "edi"

-- | TOML.
backendTOML :: Backend
backendTOML = Backend "toml"

-- | YAML.
backendYAML :: Backend
backendYAML = Backend "yaml"

-- | XML.
backendXML :: Backend
backendXML = Backend "xml"

-- | CSV / TSV header rows.
backendCSV :: Backend
backendCSV = Backend "csv"

-- | HTML serialization (sub-vocabulary of XML; element/attribute split).
backendHTML :: Backend
backendHTML = Backend "html"

-- | Newline-delimited JSON. One record per line.
backendNDJSON :: Backend
backendNDJSON = Backend "ndjson"

-- | Catch-all for hand-rolled binary formats that do not fit one of the
-- standard backends above.
backendBinary :: Backend
backendBinary = Backend "binary"

-- | Protobuf text format (@google.protobuf.TextFormat@ / @pbtxt@).
backendTextFormat :: Backend
backendTextFormat = Backend "textformat"

-- ---------------------------------------------------------------------------
-- Schema-driven binary formats
-- ---------------------------------------------------------------------------

-- | ASN.1 BER / DER / PER family.
backendASN1 :: Backend
backendASN1 = Backend "asn1"

-- | Apache Avro.
backendAvro :: Backend
backendAvro = Backend "avro"

-- | Microsoft Bond.
backendBond :: Backend
backendBond = Backend "bond"

-- | FlatBuffers.
backendFlatBuffers :: Backend
backendFlatBuffers = Backend "flatbuffers"

-- | Cap\'n Proto.
backendCapnProto :: Backend
backendCapnProto = Backend "capnproto"

-- ---------------------------------------------------------------------------
-- Document / object stores
-- ---------------------------------------------------------------------------

-- | BSON (MongoDB binary JSON).
backendBSON :: Backend
backendBSON = Backend "bson"

-- | BitTorrent's Bencode.
backendBencode :: Backend
backendBencode = Backend "bencode"

-- | Amazon ION (rich-typed JSON superset).
backendION :: Backend
backendION = Backend "ion"

-- ---------------------------------------------------------------------------
-- Cross-language object serialization
-- ---------------------------------------------------------------------------

-- | Apache Fory (formerly Fury) xlang serialization. Field names
-- are converted to @snake_case@ before being written as the
-- spec\'s meta-string field name.
backendFory :: Backend
backendFory = Backend "fory"

-- ---------------------------------------------------------------------------
-- Columnar / tabular
-- ---------------------------------------------------------------------------

-- | Apache Arrow in-memory columnar layout.
backendArrow :: Backend
backendArrow = Backend "arrow"

-- | Apache Parquet.
backendParquet :: Backend
backendParquet = Backend "parquet"

-- | Apache ORC.
backendOrc :: Backend
backendOrc = Backend "orc"

-- | Apache Iceberg table format.
backendIceberg :: Backend
backendIceberg = Backend "iceberg"
