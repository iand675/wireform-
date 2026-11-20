{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Main (main) where

import Columnar.LZO qualified as LZO
import Control.DeepSeq (NFData (..))
import Control.Monad (forM_, unless)
import Criterion.Main
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Word (Word32)
import System.Environment (getArgs)
#ifdef LZO_REFERENCE
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import System.Environment (withArgs)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
foreign import ccall unsafe "hs_bench_lzo_work_size" workSize :: IO CSize
foreign import ccall unsafe "hs_bench_lzo_compress"
  encodeRef :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt
foreign import ccall safe "hs_bench_lzo_compress"
  encodeRefSafe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt
foreign import ccall unsafe "hs_bench_lzo_decompress"
  decodeRef :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall safe "hs_bench_lzo_decompress"
  decodeRefSafe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall unsafe "hs_columnar_lzo1x_measure"
  measureOwn :: Ptr Word8 -> CSize -> CSize -> Ptr CSize -> IO CInt
foreign import ccall unsafe "hs_columnar_lzo1x_decompress"
  decodeOwn :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall safe "hs_columnar_lzo1x_decompress"
  decodeOwnSafe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
#endif

data Input = Input
  { plainBytes :: !BS.ByteString
  , packedBytes :: !BS.ByteString
#ifdef LZO_REFERENCE
  , referenceWork :: !(ForeignPtr Word8)
#endif
  }
instance NFData Input where
  rnf input = rnf (plainBytes input) `seq` rnf (packedBytes input)
#ifdef LZO_REFERENCE
    `seq` referenceWork input `seq` ()
#endif

freshEncode :: BS.ByteString -> BS.ByteString
freshEncode input = either (error . show) id (LZO.compress input)
{-# NOINLINE freshEncode #-}
freshDecode :: Int -> BS.ByteString -> BS.ByteString
freshDecode limit input = either (error . show) id (LZO.decompress limit input)
{-# NOINLINE freshDecode #-}

#ifdef LZO_REFERENCE
referenceEncode :: ForeignPtr Word8 -> BS.ByteString -> BS.ByteString
referenceEncode work input = unsafePerformIO $ withForeignPtr work $ \scratch ->
  BSU.unsafeUseAsCStringLen input $ \(src, n) -> alloca $ \written -> do
    let cap = LZO.compressBound n
    out <- mallocForeignPtrBytes cap
    withForeignPtr out $ \dst -> do
      let call = if n <= 65536 && cap <= 65536 then encodeRef else encodeRefSafe
      rc <- call (castPtr src) (fromIntegral n) dst (fromIntegral cap) scratch written
      size <- peek written
      if rc /= 0 || size > fromIntegral cap then fail "liblzo2 encode"
        else pure (BSI.fromForeignPtr out 0 (fromIntegral size))
{-# NOINLINE referenceEncode #-}
referenceDecode :: Int -> BS.ByteString -> BS.ByteString
referenceDecode cap input = unsafePerformIO $
  BSU.unsafeUseAsCStringLen input $ \(src, n) -> alloca $ \written -> do
    out <- mallocForeignPtrBytes cap
    withForeignPtr out $ \dst -> do
      let call = if n <= 65536 && cap <= 65536 then decodeRef else decodeRefSafe
      rc <- call (castPtr src) (fromIntegral n) dst (fromIntegral cap) written
      size <- peek written
      if rc /= 0 || size /= fromIntegral cap then fail "liblzo2 decode"
        else pure (BSI.fromForeignPtr out 0 cap)
{-# NOINLINE referenceDecode #-}

-- Trusted, pre-sized benchmark fixtures isolate C decoding from the public
-- API's mandatory validation-before-allocation pass. This is diagnostic only.
checkedDecode :: Int -> BS.ByteString -> BS.ByteString
checkedDecode cap input = unsafePerformIO $
  BSU.unsafeUseAsCStringLen input $ \(src, n) -> alloca $ \written -> do
    out <- mallocForeignPtrBytes cap
    withForeignPtr out $ \dst -> do
      let call = if n <= 65536 && cap <= 65536 then decodeOwn else decodeOwnSafe
      rc <- call (castPtr src) (fromIntegral n) dst (fromIntegral cap) written
      size <- peek written
      if rc /= 0 || size /= fromIntegral cap then fail "checked LZO decode"
        else pure (BSI.fromForeignPtr out 0 cap)
{-# NOINLINE checkedDecode #-}

measureSize :: Int -> BS.ByteString -> Int
measureSize cap input = unsafePerformIO $
  BSU.unsafeUseAsCStringLen input $ \(src, n) -> alloca $ \written -> do
    rc <- measureOwn (castPtr src) (fromIntegral n) (fromIntegral cap) written
    size <- peek written
    if rc /= 0 || size /= fromIntegral cap then fail "LZO size validation"
      else pure (fromIntegral size)
{-# NOINLINE measureSize #-}

validationDiagnostics :: [Benchmark]
validationDiagnostics = map (\n -> env (setup "mixed" n) $ \input -> bgroup (show n ++ " bytes")
  [ bench "validate only" $ nf (measureSize n) (packedBytes input)
  , bench "checked C decode" $ nf (checkedDecode n) (packedBytes input)
  , bench "public decode" $ nf (freshDecode n) (packedBytes input)
  , bench "liblzo2 decode" $ nf (referenceDecode n) (packedBytes input)
  ]) [1024,65536]
#endif

payload :: String -> Int -> BS.ByteString
payload family n = case family of
  "zero" -> BS.replicate n 0
  "periodic" -> BS.pack (map (fromIntegral . (`rem` 7)) [0..n-1])
  "random" -> fst (BS.unfoldrN n (\s -> let next=s*1664525+1013904223 in Just (fromIntegral (next `shiftR` 24),next)) (7123 :: Word32))
  _ -> BS.pack (map (\i -> if i `rem` 16 < 12 then fromIntegral (i `rem` 3)
                           else fromIntegral (((fromIntegral i * 1664525+1013904223) :: Word32) `shiftR` 24)) [0..n-1])

setup :: String -> Int -> IO Input
setup family n = do
  let plain = payload family n
#ifdef LZO_REFERENCE
  bytes <- workSize
  unless (bytes > 0) (fail "liblzo2 initialization")
  work <- mallocForeignPtrBytes (fromIntegral bytes)
  let packed = referenceEncode work plain
  BS.length packed `seq` pure (Input plain packed work)
#else
  let packed = freshEncode plain
  BS.length packed `seq` pure (Input plain packed)
#endif

families :: [String]
families = ["zero","periodic","random","mixed"]
sizes :: [Int]
sizes = [17,1024,65536]

checkCases :: IO ()
checkCases = forM_ families $ \family -> forM_ sizes $ \n -> do
  input <- setup family n
  let ours = freshEncode (plainBytes input)
  unless (freshDecode n (packedBytes input) == plainBytes input) (fail "decode reference data")
  unless (freshDecode n ours == plainBytes input) (fail "roundtrip")
#ifdef LZO_REFERENCE
  unless (referenceDecode n ours == plainBytes input) (fail "independent readback")
  unless (checkedDecode n (packedBytes input) == plainBytes input
    && measureSize n (packedBytes input) == n) (fail "validation diagnostic fixtures")
  putStrLn (family ++ " " ++ show n ++ ": wireform=" ++ show (BS.length ours) ++ ", liblzo2=" ++ show (BS.length (packedBytes input)))
#endif

main :: IO ()
main = do
  args <- getArgs
  case args of
#ifdef LZO_REFERENCE
    _ | "--validation-cost" `elem` args ->
      withArgs (filter (/= "--validation-cost") args) (defaultMain validationDiagnostics)
#endif
    ["--check"] -> checkCases
#ifdef LZO_REFERENCE
    ["--reference-compress", source, output] -> do
      size <- workSize
      unless (size > 0) (fail "liblzo2 initialization")
      work <- mallocForeignPtrBytes (fromIntegral size)
      bytes <- BS.readFile source
      BS.writeFile output (referenceEncode work bytes)
    ["--reference-decompress", count, source, output] -> case readMaybe count of
      Just n | n >= 0 && n <= 16777216 -> do
        ready <- workSize
        unless (ready > 0) (fail "liblzo2 initialization")
        bytes <- BS.readFile source
        BS.writeFile output (referenceDecode n bytes)
      _ -> fail "test oracle output limit is 16MiB"
#endif
    _ -> defaultMain $
      map (\family -> bgroup family $
        map (\n -> env (setup family n) $ \input -> bgroup (show n ++ " bytes")
          [ bgroup "compress"
              [ bench "wireform" $ nf freshEncode (plainBytes input)
#ifdef LZO_REFERENCE
              , bench "liblzo2" $ nf (referenceEncode (referenceWork input)) (plainBytes input)
#endif
              ]
          , bgroup "decompress"
              [ bench "wireform" $ nf (freshDecode n) (packedBytes input)
#ifdef LZO_REFERENCE
              , bench "liblzo2" $ nf (referenceDecode n) (packedBytes input)
#endif
              ]
          ]) sizes) families
