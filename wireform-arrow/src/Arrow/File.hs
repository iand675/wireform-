{-# LANGUAGE BangPatterns #-}

{- | Apache Arrow IPC file and stream readers for the standard Arrow
format (FlatBuffers metadata; files carry the @ARROW1@ magic and a
FlatBuffers footer), as written by pyarrow, arrow-cpp, arrow-rs,
"Arrow.Stream" and "Arrow.Write".

'readArrowFileColumns' is the convenience entry point: it returns the
schema and every record batch materialized, with body compression
undone and dictionary columns resolved. 'readArrowFile' and
'readArrowStream' expose the raw @(RecordBatchDef, body)@ pairs in wire
layout (decode them with 'Arrow.FlatBufferIPC.materializeRecordBatchFB'
after any body decompression).
-}
module Arrow.File (
  ArrowFile (..),
  ArrowStream (..),
  readArrowFile,
  readArrowFileColumns,
  readArrowStream,
  readIPCMessage,
) where

import Arrow.Column (ColumnArray)
import Arrow.FlatBufferIPC (decodeMessageFrame, readArrowFileFBWithDicts, readArrowStreamFBWithDicts, readFrameHeader)
import Arrow.Stream (decodeArrowFile)
import Arrow.Types
import Data.Bits (complement, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V


-- | Schema and record batches of an Arrow IPC file, in wire layout.
data ArrowFile = ArrowFile
  { afSchema :: !Schema
  , afBatches :: !(V.Vector (RecordBatchDef, ByteString))
  }
  deriving stock (Show, Eq)


-- | Schema and record batches of an Arrow IPC stream, in wire layout.
data ArrowStream = ArrowStream
  { asSchema :: !Schema
  , asBatches :: !(V.Vector (RecordBatchDef, ByteString))
  }
  deriving stock (Show, Eq)


{- | Read the encapsulated message starting at the given byte offset.
Returns @(message, body, offset of the next message)@. The
end-of-stream marker is reported as 'Left'.
-}
readIPCMessage :: ByteString -> Int -> Either String (Message, ByteString, Int)
readIPCMessage bs !off
  | off < 0 || off > BS.length bs = Left "Arrow.File: message offset out of range"
  | otherwise = do
      let !input = BS.drop off bs
      (msg, bodyLen) <- decodeMessageFrame input
      (_, _, rest) <- readFrameHeader input
      let !metaEnd = off + (BS.length input - BS.length rest)
          !n = fromIntegral bodyLen :: Int
      if bodyLen < 0 || n > BS.length rest
        then Left "Arrow.File: message body runs past the end of the input"
        else
          let !padded = min (BS.length rest) ((n + 7) .&. complement 7)
          in Right (msg, BS.take n rest, metaEnd + padded)


-- | Read an Arrow IPC stream (schema, dictionary and record batches up to the end-of-stream marker).
readArrowStream :: ByteString -> Either String ArrowStream
readArrowStream bs = do
  (schema, _, batches) <- readArrowStreamFBWithDicts bs
  Right ArrowStream {asSchema = schema, asBatches = V.fromList batches}


-- | Read an Arrow IPC file.
readArrowFile :: ByteString -> Either String ArrowFile
readArrowFile bs = do
  (schema, _, batches) <- readArrowFileFBWithDicts bs
  Right ArrowFile {afSchema = schema, afBatches = V.fromList batches}


{- | Read an Arrow IPC file and materialize every record batch
(decompressed, dictionaries resolved).
-}
readArrowFileColumns :: ByteString -> Either String (Schema, V.Vector (V.Vector ColumnArray))
readArrowFileColumns bs = do
  (schema, batches) <- decodeArrowFile bs
  Right (schema, V.fromList batches)
