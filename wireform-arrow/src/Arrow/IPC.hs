{- | Apache Arrow IPC message framing.

An encapsulated Arrow IPC message is a continuation marker
(@0xFFFFFFFF@), a 4-byte little-endian metadata length, the
FlatBuffers @Message@ table (padded to 8 bytes) and then the body.
'encodeIPCMessage' / 'decodeIPCMessage' handle the metadata frame for
one 'Message' in exactly the format pyarrow, arrow-cpp and arrow-rs use
(they delegate to "Arrow.FlatBufferIPC"); bodies are appended / read by
the caller. "Arrow.Stream" and "Arrow.File" build whole streams and
files on top.
-}
module Arrow.IPC (
  encodeIPCMessage,
  decodeIPCMessage,
  validateRecordBatchBuffers,
) where

import Arrow.FlatBufferIPC (decodeMessageFrame, encodeMessageFrame)
import Arrow.Read.Columns (validateRecordBatchBuffers)
import Arrow.Types (Message)
import Data.ByteString (ByteString)


{- | Encapsulated metadata frame for one message. For record and
dictionary batches the declared body length is the 8-aligned extent of
the batch's buffers; append that many body bytes to form the full
message.
-}
encodeIPCMessage :: Message -> ByteString
encodeIPCMessage = encodeMessageFrame


{- | Decode the metadata frame at the start of the input (bytes after
the frame, such as the body, are ignored).
-}
decodeIPCMessage :: ByteString -> Either String Message
decodeIPCMessage bs = fst <$> decodeMessageFrame bs
