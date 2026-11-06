{-# LANGUAGE BangPatterns #-}

{- | Strict, optionally page-bounded GZip decoding through zlib's incremental
API. Output is checked before requesting the next chunk; concatenated GZip
members, CRCs, end-of-input and trailing bytes all remain validated.
-}
module Parquet.GZip (decompress, decompressSized) where

import Codec.Compression.Zlib.Internal qualified as Z
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import System.IO.Unsafe (unsafeDupablePerformIO)


decompress :: ByteString -> Either String ByteString
decompress = decode Nothing


decompressSized :: Int -> ByteString -> Either String ByteString
decompressSized expected
  | expected < 0 || expected >= maxBound || expected > fromIntegral (maxBound :: Int32) = const (Left "Parquet.Read: invalid GZip uncompressed size")
  | otherwise = decode (Just expected)


decode :: Maybe Int -> ByteString -> Either String ByteString
decode expected input = unsafeDupablePerformIO (walk False False 0 [] initial)
  where
    limit = maybe (fromIntegral (maxBound :: Int32)) id expected
    parameters =
      Z.defaultDecompressParams
        { Z.decompressBufferSize = maybe 32768 (\n -> n + 1) expected
        , Z.decompressAllMembers = True
        }
    initial = Z.decompressIO Z.gzipFormat parameters
    walk !fed !eof !written !chunks stream = case stream of
      Z.DecompressInputRequired supply
        | not fed -> supply input >>= walk True (BS.null input) written chunks
        | not eof -> supply BS.empty >>= walk True True written chunks
        | otherwise -> pure (Left "Parquet.Read: GZip truncated input")
      Z.DecompressOutputAvailable chunk next
        | BS.length chunk > limit - written -> pure (Left "Parquet.Read: GZip output exceeds page size")
        | otherwise -> next >>= walk fed eof (written + BS.length chunk) (chunk : chunks)
      Z.DecompressStreamEnd unused
        | not (BS.null unused) -> pure (Left "Parquet.Read: GZip trailing compressed input")
        | maybe False (/= written) expected -> pure (Left "Parquet.Read: GZip decompressed size differs from page header")
        | otherwise -> pure (Right (finish chunks))
      Z.DecompressStreamError problem -> pure (Left ("Parquet.Read: GZip decompress failed: " ++ show problem))
    finish [] = BS.empty
    finish [chunk] = chunk
    finish chunks = BS.concat (reverse chunks)
