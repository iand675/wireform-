{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
-- | ORC stream-level decompression.
--
-- ORC's stream framing wraps each (potentially compressed)
-- payload in a 3-byte header
-- @[len_lo, len_mid, len_hi_isOriginal]@ where the high bit
-- of the third byte is the @isOriginal@ flag (1 = data is
-- raw / not compressed) and the remaining 23 bits are the
-- chunk's compressed length. A stream is a back-to-back
-- sequence of these chunks; each chunk decompresses to at
-- most the file's @compressionBlockSize@ (PostScript
-- field 4).
--
-- Both data streams /and/ the file footer / metadata
-- protobufs are compressed under this same framing, so the
-- footer reader needs the same decompressor as the column
-- reader.
--
-- This module owns the logic so both 'ORC.Footer' (footer
-- decoding) and 'ORC.Read' (column decoding) can reuse it
-- without forming a cycle.
module ORC.Compress
  ( decompressORCStream
  , decompressORCStreamSized
  , compressORCStream
  , compressORCStreamSized
  , defaultORCCompressionBlockSize
  ) where

import Control.Exception (try, evaluate, SomeException)
import qualified Codec.Compression.Zlib.Raw as ZlibRaw
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word64)
import System.IO.Unsafe (unsafePerformIO)

#ifdef HAVE_ZSTD
import Codec.Compression.Zstd (Decompress (..), compress, decompress)
#endif

#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif

#ifdef HAVE_LZ4
import qualified Columnar.LZ4 as LZ4
#endif

import ORC.Types (CompressionKind (..))

-- | The ORC default for @compressionBlockSize@, 256 KiB.
-- Every upstream writer (Java ORC, C++ ORC, arrow-rs) uses
-- this value; older callers that don't thread the
-- postscript-supplied size through fall back to it.
defaultORCCompressionBlockSize :: Int
defaultORCCompressionBlockSize = 262144

{-# INLINE decompressORCStream #-}
decompressORCStream :: CompressionKind -> ByteString -> Either String ByteString
decompressORCStream =
  decompressORCStreamSized defaultORCCompressionBlockSize

-- | Like 'decompressORCStream' but the caller supplies the
-- file's @compressionBlockSize@ from the postscript. LZ4 /
-- LZO need this to size their output buffer correctly;
-- Zlib / Snappy / ZSTD ignore it (they decompress to whatever
-- the input expands to).
decompressORCStreamSized
  :: Int -> CompressionKind -> ByteString -> Either String ByteString
decompressORCStreamSized _   CompressionNone bs = Right bs
decompressORCStreamSized blk kind            bs = decompressChunks blk kind bs 0 []

decompressChunks
  :: Int -> CompressionKind -> ByteString -> Int -> [ByteString]
  -> Either String ByteString
decompressChunks !blk kind bs !off !acc
  | off >= BS.length bs = Right $! BS.concat (reverse acc)
  | off + 3 > BS.length bs = Left "ORC.Compress: truncated compression header"
  | otherwise = do
      let !b0     = fromIntegral (BS.index bs off)       :: Word64
          !b1     = fromIntegral (BS.index bs (off + 1)) :: Word64
          !b2     = fromIntegral (BS.index bs (off + 2)) :: Word64
          !header = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
          !isOrig = header .&. 1 == 1
          !cLen   = fromIntegral (header `shiftR` 1) :: Int
      if off + 3 + cLen > BS.length bs
        then Left "ORC.Compress: compression chunk extends past stream end"
        else do
          let !chunk = BS.take cLen (BS.drop (off + 3) bs)
          decoded <- if isOrig
            then Right chunk
            else decompressBlock blk kind chunk
          decompressChunks blk kind bs (off + 3 + cLen) (decoded : acc)

decompressBlock
  :: Int -> CompressionKind -> ByteString -> Either String ByteString
decompressBlock _   CompressionNone bs = Right bs
decompressBlock _   CompressionZlib bs = tryZlibRaw bs
decompressBlock _   CompressionSnappy bs = trySnappy bs
#ifdef HAVE_ZSTD
decompressBlock _   CompressionZstd bs = tryZstd bs
#endif
#ifdef HAVE_LZ4
decompressBlock blk CompressionLZ4 bs = tryLZ4 blk bs
#endif
decompressBlock _ CompressionLZO bs =
  -- ORC's LZO codec is the legacy Hadoop variant that requires
  -- a JNI-bound C lib; no pure-Haskell decoder is available
  -- today. Real-world files written this century almost never
  -- use LZO, so we surface a clear error instead of silently
  -- pretending.
  Left $ "ORC.Compress: LZO decompression not implemented (no pure-"
       ++ "Haskell decoder); the file is otherwise readable for "
       ++ "metadata. Length=" ++ show (BS.length bs)
decompressBlock _ c _ =
  Left $
    "ORC.Compress: compression "
      ++ show c
      ++ " not supported (use None, Zlib, Snappy with -fsnappy"
#ifdef HAVE_ZSTD
      ++ ", Zstandard with -fzstd"
#endif
#ifdef HAVE_LZ4
      ++ ", LZ4 with -flz4"
#endif
      ++ ")"

tryZlibRaw :: ByteString -> Either String ByteString
tryZlibRaw bs =
  unsafePerformIO $ do
    er <- try @SomeException $ evaluate $ BL.toStrict $ ZlibRaw.decompress $ BL.fromStrict bs
    case er of
      Left e  -> pure $ Left $ "ORC.Compress: zlib decompress failed: " ++ show e
      Right x -> pure $ Right x

trySnappy :: ByteString -> Either String ByteString
#ifdef HAVE_SNAPPY
trySnappy bs = Right (Snappy.decompress bs)
#else
trySnappy _ =
  Left "ORC.Compress: Snappy requires building wireform with -fsnappy"
#endif

#ifdef HAVE_ZSTD
tryZstd :: ByteString -> Either String ByteString
tryZstd bs =
  case decompress bs of
    Decompress out -> Right out
    Skip           -> Left "ORC.Compress: zstd decompress skipped"
    Error msg      -> Left $ "ORC.Compress: zstd decompress failed: " ++ msg
#endif

#ifdef HAVE_LZ4
-- | Decompress one ORC LZ4 chunk via 'Columnar.LZ4'. The
-- output is bounded by the file's compressionBlockSize.
tryLZ4 :: Int -> ByteString -> Either String ByteString
tryLZ4 !blockSize bs = case LZ4.decompress blockSize bs of
  Right out -> Right out
  Left e    -> Left $
    "ORC.Compress: LZ4 decompress failed (input " ++ show (BS.length bs)
      ++ " bytes, max output " ++ show blockSize ++ " bytes); "
      ++ e
#endif

-- ============================================================
-- Compression (writer side)
-- ============================================================

-- | Wrap @bs@ in ORC's compression envelope using the default
-- block size. Each block of up to 'defaultORCCompressionBlockSize'
-- input bytes becomes a 3-byte header + (compressed or
-- original, whichever is smaller) block bytes. When the codec
-- is 'CompressionNone' the input is returned verbatim.
{-# INLINE compressORCStream #-}
compressORCStream :: CompressionKind -> ByteString -> Either String ByteString
compressORCStream =
  compressORCStreamSized defaultORCCompressionBlockSize

-- | Like 'compressORCStream' but the caller picks the block
-- size. Block size matters for two reasons: it bounds the
-- decompressor's per-chunk buffer (some codecs need this for
-- output sizing), and it's the unit of the per-chunk
-- 'isOriginal' fallback (so smaller blocks let the writer
-- choose 'isOriginal' more often when compression doesn't
-- pay).
compressORCStreamSized
  :: Int -> CompressionKind -> ByteString -> Either String ByteString
compressORCStreamSized _   CompressionNone bs = Right bs
compressORCStreamSized blk kind            bs
  | blk <= 0  = Left "ORC.Compress: non-positive compression block size"
  | otherwise = compressChunks blk kind bs 0 []

compressChunks
  :: Int -> CompressionKind -> ByteString -> Int -> [ByteString]
  -> Either String ByteString
compressChunks !blk !kind !bs !off !acc
  | off >= BS.length bs = Right $! BS.concat (reverse acc)
  | otherwise = do
      let !remaining = BS.length bs - off
          !take_     = min blk remaining
          !chunk     = BS.take take_ (BS.drop off bs)
      compressedM <- compressBlock kind chunk
      let !packed = packChunkEnvelope chunk compressedM
      compressChunks blk kind bs (off + take_) (packed : acc)

-- | Build the 3-byte header + payload for one chunk. Falls
-- back to @isOriginal=1@ when compression doesn't shrink the
-- block (writers typically check this; otherwise readers
-- decompress only to discover the block was the same size).
packChunkEnvelope :: ByteString -> Maybe ByteString -> ByteString
packChunkEnvelope original mCompressed =
  let !use = case mCompressed of
        Just cBs | BS.length cBs < BS.length original -> Right cBs
        _                                             -> Left  original
      !payloadLen = case use of
        Right cBs -> BS.length cBs
        Left  o   -> BS.length o
      !isOrig    = case use of
        Right _ -> 0 :: Int
        Left  _ -> 1
      !header = (payloadLen `shiftL` 1) .|. isOrig
      !h0     = fromIntegral (header .&. 0xFF)        :: Word8
      !h1     = fromIntegral ((header `shiftR` 8)  .&. 0xFF) :: Word8
      !h2     = fromIntegral ((header `shiftR` 16) .&. 0xFF) :: Word8
      !payload = either id id use
  in  BS.pack [h0, h1, h2] <> payload

-- | Compress a single block under the given codec. Returns
-- 'Nothing' if the codec isn't enabled or fails — the caller
-- then falls back to @isOriginal=1@ (the block goes through
-- raw).
compressBlock :: CompressionKind -> ByteString -> Either String (Maybe ByteString)
compressBlock CompressionNone _ = Right Nothing
compressBlock CompressionZlib bs =
  Right (Just (BL.toStrict (ZlibRaw.compress (BL.fromStrict bs))))
#ifdef HAVE_SNAPPY
compressBlock CompressionSnappy bs = Right (Just (Snappy.compress bs))
#else
compressBlock CompressionSnappy _ =
  Left "ORC.Compress: Snappy requires building wireform with -fsnappy"
#endif
#ifdef HAVE_ZSTD
compressBlock CompressionZstd bs = Right (Just (compress 3 bs))
#endif
#ifdef HAVE_LZ4
compressBlock CompressionLZ4 bs = Right (Just (LZ4.compress bs))
#endif
compressBlock c _ = Left $
  "ORC.Compress: codec " ++ show c
    ++ " not supported by the writer (use None, Zlib, Snappy/-fsnappy"
#ifdef HAVE_ZSTD
    ++ ", Zstandard/-fzstd"
#endif
#ifdef HAVE_LZ4
    ++ ", LZ4/-flz4"
#endif
    ++ ")"
