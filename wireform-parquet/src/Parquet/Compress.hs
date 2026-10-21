{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Page-data compression for the Parquet writer.

The reader's 'Parquet.Read.decompressChunk' supports @Uncompressed@,
@GZip@, @Snappy@ (with @-fsnappy@), @ZSTD@ (with @-fzstd@), and
@LZ4_RAW@ (with @-flz4@). This module exposes the symmetric writer
side: 'compressPageBytes' takes a 'Compression' codec and a page body
and returns the compressed bytes.

The codec set behind cabal flags is identical to the reader's; if the
user requests a codec that wasn't enabled at build time, the writer
returns @Left@ rather than silently emitting uncompressed data.
-}
module Parquet.Compress (
  compressPageBytes,
) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL


#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif

#ifdef HAVE_ZSTD
import qualified Codec.Compression.Zstd as Zstd
#endif

#ifdef HAVE_LZ4
import qualified Columnar.LZ4 as LZ4
#endif

#ifdef HAVE_BROTLI
import qualified Codec.Compression.Brotli as Brotli
#endif

import Parquet.Types (Compression (..))


{- | Compress a page-body byte string with the given codec.

@Uncompressed@ is always available; @GZip@ uses the built-in @zlib@
dependency; @Snappy@\/@ZSTD@\/@LZ4@ require the matching @-f@ flag at
build time. Returns @Left@ for codecs we cannot encode (e.g. the
deprecated Hadoop-flavoured @LZ4@).
-}
compressPageBytes :: Compression -> ByteString -> Either String ByteString
compressPageBytes Uncompressed bs = Right bs
compressPageBytes GZip bs =
  Right (BL.toStrict (GZip.compress (BL.fromStrict bs)))

#ifdef HAVE_SNAPPY
compressPageBytes Snappy bs = Right (Snappy.compress bs)
#else
compressPageBytes Snappy _ =
  Left "Parquet.Compress: Snappy requires building wireform with -fsnappy"
#endif

#ifdef HAVE_ZSTD
compressPageBytes ZSTD bs =
  Right (Zstd.compress 3 bs)  -- level 3 is the parquet-mr default.
#else
compressPageBytes ZSTD _ =
  Left "Parquet.Compress: ZSTD requires building wireform with -fzstd"
#endif

#ifdef HAVE_LZ4
-- LZ4_RAW (Parquet codec id 7): the raw LZ4 block format with
-- no framing. 'Columnar.LZ4.compress' is a direct FFI call to
-- liblz4's @LZ4_compress_default@ — output is byte-identical
-- to what arrow-cpp / parquet-mr / pyarrow emit, so any
-- modern Parquet reader accepts it.
-- A page above liblz4's raw-block limit cannot be compressed at all:
-- 'LZ4.compress' would return an empty payload, which would be written
-- as a valid but empty page. Reject it instead (Parquet pages are
-- bounded by the page size, far below the limit).
compressPageBytes LZ4Raw bs
  | LZ4.compressBound (BS.length bs) < 0 =
      Left "Parquet.Compress: LZ4_RAW input exceeds LZ4_MAX_INPUT_SIZE"
  | otherwise = Right (LZ4.compress bs)
#else
compressPageBytes LZ4Raw _ =
  Left "Parquet.Compress: LZ4_RAW requires building wireform with -flz4"
#endif


compressPageBytes LZ4 _ =
  Left $
    "Parquet.Compress: deprecated Hadoop LZ4 (codec 5) is not supported; "
      ++ "use LZ4_RAW (codec 7) with -flz4"

#ifdef HAVE_BROTLI
compressPageBytes Brotli bs =
  Right (BL.toStrict (Brotli.compress (BL.fromStrict bs)))
#else
compressPageBytes Brotli _ =
  Left "Parquet.Compress: Brotli requires building wireform with -fbrotli"
#endif


-- LZO is intentionally unsupported: the Parquet spec lists it (codec 3)
-- for historical compatibility with Hadoop writers, but none of the
-- mainstream modern writers (parquet-mr, arrow-cpp, pyarrow) emit LZO
-- pages and parquet-cpp ships with it disabled by default. We surface a
-- clear error rather than pretending otherwise.
compressPageBytes LZO _ =
  Left "Parquet.Compress: LZO (codec 3) is not supported; it's a legacy Hadoop codec not emitted by modern writers"
