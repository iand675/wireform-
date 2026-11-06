{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module CodecBoundaryRegression (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import GHC.Conc (getAllocationCounter)
import Parquet.Compress qualified as Compress
import Parquet.Page qualified as Page
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import Parquet.Write qualified as Write
import System.Mem (performGC)


check :: String -> Bool -> IO ()
check name ok = unless ok (fail ("Codec/page regression: " ++ name))


pageV1 :: Int -> BS.ByteString -> BS.ByteString
pageV1 expected body = Write.encodePageHeader (Page.PageHeader (Page.PtDataPage (Page.DataPageHeader 1 0)) (Just (fromIntegral expected)) (Just (fromIntegral (BS.length body)))) <> body


pageV2 :: Bool -> Int -> BS.ByteString -> BS.ByteString
pageV2 compressed expected body = Write.encodePageHeader (Page.PageHeader (Page.PtDataPageV2 (Page.DataPageHeaderV2 1 1 1 0 2 0 compressed)) (Just (fromIntegral expected + 2)) (Just (fromIntegral (BS.length body) + 2))) <> BS.pack [2, 0] <> body


tests :: IO ()
tests = do
  forM_ [False, True] $ \compressed -> forM_ [P.Uncompressed, P.GZip, P.Snappy, P.ZSTD, P.Brotli, P.LZ4Raw, P.LZO] $ \codec -> do
    check "all-null V2 does not ignore non-empty invalid payload" (isLeft (Read.decompressDataPageV2Body codec 0 1 (pageV2 compressed 0 "invalid codec bytes") 0))
    check "all-null V2 with absent values remains readable" (case Read.decompressDataPageV2Body codec 0 1 (pageV2 compressed 0 BS.empty) 0 of Right (_, _, _, _, values, _) -> BS.null values; Left _ -> False)
  let available = [P.Uncompressed, P.GZip, P.Snappy, P.ZSTD, P.LZ4Raw, P.LZO, P.Brotli]
  forM_ available $ \codec -> case Compress.validateCompression codec of
    Left _ -> pure ()
    Right () -> do
      emptyValues <- either fail pure (Compress.compressPageBytes codec BS.empty)
      check ("enabled codec empty V1 bytes: " ++ show codec) (case Read.decompressDataPageBody codec (pageV1 0 emptyValues) 0 of Right (_, _, value, _) -> BS.null value; Left _ -> False)
      check ("enabled codec empty V2 values: " ++ show codec) (case Read.decompressDataPageV2Body codec 0 1 (pageV2 True 0 emptyValues) 0 of Right (_, _, _, _, value, _) -> BS.null value; Left _ -> False)
      let raw = BS.pack [0 .. 255]
      encoded <- either fail pure (Compress.compressPageBytes codec raw)
      check "every enabled codec validates declared V1 output size" (isLeft (Read.decompressDataPageBody codec (pageV1 (BS.length raw + 1) encoded) 0))
      check "every enabled codec preserves exact V1 bytes" (case Read.decompressDataPageBody codec (pageV1 (BS.length raw) encoded) 0 of Right (_, _, value, _) -> value == raw; Left _ -> False)
      let large = BS.replicate (16 * 1024 * 1024) 0
      bomb <- either fail pure (Compress.compressPageBytes codec large)
      let framed = pageV1 8 bomb
      _ <- evaluate (BS.length framed)
      performGC
      before <- getAllocationCounter
      rejected <- evaluate (isLeft (Read.decompressDataPageBody codec framed 0))
      after <- getAllocationCounter
      check ("enabled codec expansion guard: " ++ show codec ++ " allocated=" ++ show (before - after)) (rejected && before - after < 1024 * 1024)
  -- Forged frame-size headers must be rejected before requesting gigabytes
  -- from the allocator. The compressed payload itself need not be valid.
  forM_ [(P.Snappy, BS.pack [128, 128, 128, 128, 8]), (P.ZSTD, BS.pack [40, 181, 47, 253, 224, 0, 0, 0, 128, 0, 0, 0, 0])] $ \(codec, payload) ->
    case Compress.validateCompression codec of
      Left _ -> pure ()
      Right () -> do
        before <- getAllocationCounter
        rejected <- evaluate (isLeft (Read.decompressChunk codec payload))
        after <- getAllocationCounter
        check "raw codec size ceiling is checked before allocation" (rejected && before - after < 1024 * 1024)
  a <- either fail pure (Compress.compressPageBytes P.GZip "first")
  b <- either fail pure (Compress.compressPageBytes P.GZip "second")
  check "concatenated GZip members retain exact semantics" (Read.decompressChunk P.GZip (a <> b) == Right "firstsecond")
  check "GZip trailing compressed bytes rejected" (isLeft (Read.decompressChunk P.GZip (a <> "garbage")))
#ifdef HAVE_BROTLI
  forM_ [BS.empty, "x", BS.pack [0..255], BS.replicate 32768 97, BS.replicate 131072 0] $ \input -> do
    encoded <- either fail pure (Compress.compressPageBytes P.Brotli input)
    check "unsized complete stream" (Read.decompressChunk P.Brotli encoded == Right input)
    check "exact bounded page" (case Read.decompressDataPageBody P.Brotli (pageV1 (BS.length input) encoded) 0 of Right (_,_,raw,_) -> raw == input; Left _ -> False)
    forM_ [0, BS.length input + 1] $ \wrong ->
      if wrong == BS.length input then pure () else check "declared output size mismatch" (isLeft (Read.decompressDataPageBody P.Brotli (pageV1 wrong encoded) 0))
    check "trailing compressed bytes" (isLeft (Read.decompressChunk P.Brotli (encoded <> "trailing")))
    check "bounded trailing compressed bytes" (isLeft (Read.decompressDataPageBody P.Brotli (pageV1 (BS.length input) (encoded <> "trailing")) 0))
    forM_ [0 .. BS.length encoded - 1] $ \prefix -> do
      check "truncated compressed stream" (isLeft (Read.decompressChunk P.Brotli (BS.take prefix encoded)))
      check "bounded truncated compressed stream" (isLeft (Read.decompressDataPageBody P.Brotli (pageV1 (BS.length input) (BS.take prefix encoded)) 0))
  empty <- either fail pure (Compress.compressPageBytes P.Brotli BS.empty)
  check "compressed empty all-null V2 stream" (case Read.decompressDataPageV2Body P.Brotli 0 1 (pageV2 True 0 empty) 0 of Right (_,_,_,_,raw,_) -> BS.null raw; Left _ -> False)
  let large = BS.replicate (16*1024*1024) 0
  bomb <- either fail pure (Compress.compressPageBytes P.Brotli large)
  _ <- evaluate (BS.length bomb)
  performGC
  start <- getAllocationCounter
  rejected <- evaluate (isLeft (Read.decompressDataPageBody P.Brotli (pageV1 8 bomb) 0))
  performGC
  end <- getAllocationCounter
  check "advertised size prevents full expansion" (rejected && start - end < 1024*1024)
#else
  check "Brotli remains explicitly unavailable without its optional flag" (isLeft (Compress.validateCompression P.Brotli) && isLeft (Compress.compressPageBytes P.Brotli "x") && isLeft (Read.decompressChunk P.Brotli "x"))
#endif
