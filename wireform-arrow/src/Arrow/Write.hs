{- | Apache Arrow IPC whole-stream and whole-file writers. Both emit
the standard Arrow IPC format (FlatBuffers metadata, encapsulated
messages, spec footer for files) that pyarrow, arrow-cpp and arrow-rs
read, using 'Arrow.Stream.defaultWriteOptions' (no body compression,
one dictionary batch per dictionary id). Use "Arrow.Stream" directly
for compression, dictionary-replacement options, or the lazy
(zero-copy body) writers.
-}
module Arrow.Write (
  writeArrowStream,
  writeArrowFile,

  -- * Record batch layout (see "Arrow.FlatBufferIPC")
  validateColumns,
  BatchPlan (..),
  planBatch,
) where

import Arrow.Column (ColumnArray)
import Arrow.Stream (defaultWriteOptions, encodeArrowFile, encodeArrowStream)
import Arrow.Types (Schema)
import Arrow.Write.Columns (BatchPlan (..), planBatch, validateColumns)
import Data.ByteString (ByteString)
import Data.Vector qualified as V


{- | Write a complete Arrow IPC stream (schema, dictionaries, record
batches, end-of-stream marker). 'Left' when a batch does not fit the
schema or a dictionary cannot be written (see 'Arrow.Stream.encodeArrowStream').
-}
writeArrowStream :: Schema -> V.Vector (V.Vector ColumnArray) -> Either String ByteString
writeArrowStream schema batches = encodeArrowStream defaultWriteOptions schema (V.toList batches)


{- | Write a complete Arrow IPC file (magic, stream payload, FlatBuffers
footer, magic). 'Left' as for 'writeArrowStream'.
-}
writeArrowFile :: Schema -> V.Vector (V.Vector ColumnArray) -> Either String ByteString
writeArrowFile schema batches = encodeArrowFile defaultWriteOptions schema (V.toList batches)
