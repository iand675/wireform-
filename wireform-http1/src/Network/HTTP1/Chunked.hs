{- | Chunked-transfer-encoding helpers (RFC 9112 § 7.1).

The encoder writes @<hex-size>\\r\\n<bytes>\\r\\n@ per chunk and a
terminating @0\\r\\n\\r\\n@ at end-of-stream. The decoder is exposed via
'Network.HTTP1.Parser.parseChunkHeader' + a streaming pull from the
recv buffer.

Both sides go through @Wireform.Builder@ so the encoded form lands in
the connection's send buffer without intermediate allocation.
-}
module Network.HTTP1.Chunked (
  encodeChunk,
  encodeChunkPrefix,
  encodeLastChunk,
  encodeLastChunkWithTrailers,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8, Word64)
import Network.HTTP1.Headers (Header)
import Wireform.Builder qualified as B


{- | Encode a non-terminating data chunk.

Wire form: @<hex-size> CRLF <data> CRLF@.
-}
{-# INLINE encodeChunk #-}
encodeChunk :: ByteString -> B.Builder
encodeChunk bs =
  encodeChunkPrefix (fromIntegral (BS.length bs))
    <> B.byteString bs
    <> crlfB


-- | Encode a chunk size and CRLF before a separately written payload.
{-# INLINE encodeChunkPrefix #-}
encodeChunkPrefix :: Word64 -> B.Builder
encodeChunkPrefix size = hexSize size <> crlfB


{- | Encode the terminating zero-size chunk with no trailers.
Wire form: @0 CRLF CRLF@.
-}
{-# INLINE encodeLastChunk #-}
encodeLastChunk :: B.Builder
encodeLastChunk = B.byteString "0\r\n\r\n"


{- | Encode the terminating zero-size chunk with a trailer-fields
section.  Wire form: @0 CRLF (field-name COLON SP field-value CRLF)*
CRLF@.
-}
encodeLastChunkWithTrailers :: [Header] -> B.Builder
encodeLastChunkWithTrailers [] = encodeLastChunk
encodeLastChunkWithTrailers trls =
  B.byteString "0\r\n"
    <> foldMap emitHdr trls
    <> crlfB
  where
    emitHdr (k, v) = B.byteString k <> colonSp <> B.byteString v <> crlfB
    colonSp = B.byteString ": "


------------------------------------------------------------------------

{-# INLINE crlfB #-}
crlfB :: B.Builder
crlfB = B.byteString "\r\n"


{- | Lowercase ASCII hex of a 'Word64'. Worst case is 16 nibbles for a
64-bit chunk; we emit only as many as needed. We deliberately avoid
the @bytestring-builder@ decimal/hex helpers because they pull in
'String' formatting via @reads@ on the slow path.
-}
hexSize :: Word64 -> B.Builder
hexSize 0 = B.word8 0x30 -- '0'
hexSize w0 = go w0 mempty
  where
    go 0 acc = acc
    go w acc =
      let !d = fromIntegral (w .&. 0xf) :: Word8
          !c = if d < 10 then d + 0x30 else d - 10 + 0x61
      in go (w `shiftR` 4) (B.word8 c <> acc)
