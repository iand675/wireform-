-- | Real Apache Arrow IPC framing — binary-compatible with the
-- reference implementations (arrow-cpp, arrow-rs, pyarrow).
--
-- "Arrow.IPC" uses a simplified flatbuffer-shaped encoding that
-- only self-round-trips; this module constructs Arrow's metadata
-- tables as actual FlatBuffers (per @format/Schema.fbs@ and
-- @format/Message.fbs@) and emits the encapsulated-message framing
-- so pyarrow / arrow-rs / arrow-cpp can consume the output.
--
-- The /generic/ flatbuffer primitives (back-to-front 'Builder',
-- vtable dedup, soffset chains, reader peek + table resolution)
-- live in "FlatBuffers.Builder" and "FlatBuffers.Reader" inside
-- @wireform-flatbuffers@. This module only owns the
-- Arrow-specific layout — the @Schema@, @Field@, @Type@,
-- @RecordBatch@, @Message@, @Tensor@, @SparseTensor@ tables, plus
-- the encapsulated-frame / file-format glue. That split lets
-- "FlatBuffers.Encode" / "FlatBuffers.Decode" stay focused on
-- value-shaped use cases while the spec-precise encoder is
-- shared rather than reimplemented per call site.
--
-- The encoder is standards-compliant:
--
--   * Buffer is built back-to-front.
--   * Tables carry a signed int32 soffset to their vtable at offset 0.
--   * Vtables share when structurally identical (via a deduplication map).
--   * Scalars are aligned to their width; vectors/strings/tables
--     are 4-aligned.
--   * The root offset at byte 0 is an unsigned uoffset_t pointing to
--     the root table.
--
-- The implementation is split across "Arrow.FlatBufferIPC.Write"
-- (encoder), "Arrow.FlatBufferIPC.Read" (decoder) and
-- "Arrow.FlatBufferIPC.Common" (types both sides share); this
-- module re-exports their public API.
module Arrow.FlatBufferIPC
  ( -- * Top-level builders
    buildSchemaMessage
  , buildRecordBatchMessage
    -- * Encapsulated-message framing
  , encapsulateMessage
    -- * Stream / file writers
  , writeArrowStreamFB
  , writeArrowFileFB
  , writeArrowFileFBWithDicts
    -- * Column-based convenience writer
  , buildRecordBatchBytes
  , buildRecordBatchBytesWith
  , writeArrowStreamFBFromColumns
    -- * Body compression helpers
  , compressBody
  , compressBufferEither
  , bodyCompressionAvailable
  , decompressBody
    -- * Reader (parses pyarrow / arrow-cpp output)
  , readArrowStreamFB
  , readArrowFileFB
  , readArrowFileFBWithDicts
  , decodeSchemaMessage
  , decodeRecordBatchMessage
  , decodeDictionaryBatchMessage
  , decodeRecordBatch
  , decodeDictionaryBatch
    -- * Dictionary support
  , DictBatch (..)
  , readArrowStreamFBWithDicts
  , readArrowStreamFBInterleaved
  , StreamFrame (..)
  , buildDictionaryBatchMessage
    -- * Tensor / SparseTensor
  , Tensor (..)
  , TensorDim (..)
  , buildTensorMessage
  , decodeTensorMessage
  , encodeTensorFrame
  , decodeTensorFrame
  , SparseTensor (..)
  , buildSparseTensorMessageCOO
  , encodeSparseTensorFrame
  , decodeSparseTensorFrame
  , writeArrowStreamFBWithDicts
    -- * Single-message frames
  , encodeMessageFrame
  , decodeMessageFrame
  , readFrameHeader
  ) where

import Arrow.FlatBufferIPC.Common
import Arrow.FlatBufferIPC.Read
import Arrow.FlatBufferIPC.Write
