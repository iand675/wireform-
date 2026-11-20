{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Native Brotli calls. Parquet supplies the decoded page size, so the normal
reader allocates only that destination plus one overflow-detection byte.
A complete stream and complete consumption of its input are both required.
-}
module Parquet.Brotli (compress, decompress, decompressSized) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as U
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafeDupablePerformIO)


data Decoder


type Alloc = Ptr () -> CSize -> IO (Ptr ())


type Free = Ptr () -> Ptr () -> IO ()


foreign import ccall unsafe "BrotliDecoderCreateInstance"
  createDecoder :: FunPtr Alloc -> FunPtr Free -> Ptr () -> IO (Ptr Decoder)


foreign import ccall unsafe "BrotliDecoderDestroyInstance"
  destroyDecoder :: Ptr Decoder -> IO ()


foreign import ccall safe "BrotliDecoderDecompressStream"
  decodeStream :: Ptr Decoder -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt


foreign import ccall unsafe "BrotliEncoderMaxCompressedSize"
  maximumCompressedSize :: CSize -> IO CSize


foreign import ccall safe "BrotliEncoderCompress"
  encode :: CInt -> CInt -> CInt -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr Word8 -> IO CInt


-- The decoder state is destroyed before any continuation runs, including a
-- retry with a larger destination. All FFI byte counts use size_t/CSize.
decodeAtCapacity :: Int -> ByteString -> (ByteString -> IO a) -> IO a -> (String -> IO a) -> IO a
decodeAtCapacity capacity input complete full failed = do
  output <- BSI.mallocByteString capacity
  withForeignPtr output $ \destination -> U.unsafeUseAsCStringLen input $ \(source, count) ->
    alloca $ \availableInput -> alloca $ \nextInput -> alloca $ \availableOutput -> alloca $ \nextOutput -> do
      poke availableInput (fromIntegral count)
      poke nextInput (castPtr source)
      poke availableOutput (fromIntegral capacity)
      poke nextOutput destination
      status <-
        bracket
          (createDecoder nullFunPtr nullFunPtr nullPtr)
          (\decoder -> when (decoder /= nullPtr) (destroyDecoder decoder))
          (\decoder -> if decoder == nullPtr then pure 0 else decodeStream decoder availableInput nextInput availableOutput nextOutput nullPtr)
      leftInput <- peek availableInput
      leftOutput <- peek availableOutput
      let written = capacity - fromIntegral leftOutput
      case status of
        1
          | leftInput /= 0 -> failed "trailing compressed input"
          | leftOutput > fromIntegral capacity -> failed "invalid decoder output count"
          | otherwise -> complete (BSI.fromForeignPtr output 0 written)
        2 -> failed "truncated input"
        3 -> full
        _ -> failed "invalid stream or decoder allocation failure"


{- | Exact page-sized decoding. One extra byte distinguishes a completed empty
tail from a stream that actually expands past its advertised destination.
-}
decompressSized :: Int -> ByteString -> Either String ByteString
decompressSized expected input
  | expected < 0 || expected >= maxBound || expected > fromIntegral (maxBound :: Int32) = Left "Parquet.Read: invalid Brotli uncompressed size"
  | otherwise =
      unsafeDupablePerformIO $
        decodeAtCapacity
          (expected + 1)
          input
          (\bytes -> pure $ if BS.length bytes == expected then Right bytes else Left "Parquet.Read: Brotli decompressed size differs from page header")
          (pure (Left "Parquet.Read: Brotli output exceeds page header size"))
          (pure . Left . ("Parquet.Read: Brotli decompress failed: " ++))


{- | Compatibility API for callers without a page header. Grow geometrically
up to Parquet's signed INT32 page-size ceiling. File readers use the exact
bounded path above instead of discovering the output size this way.
-}
decompress :: ByteString -> Either String ByteString
decompress input = unsafeDupablePerformIO (go 32768)
  where
    limit = fromIntegral (maxBound :: Int32)
    go !capacity =
      decodeAtCapacity
        capacity
        input
        (pure . Right)
        ( if capacity == limit
            then pure (Left "Parquet.Read: Brotli output exceeds INT32 page-size limit")
            else go (if capacity > limit `quot` 2 then limit else capacity * 2)
        )
        (pure . Left . ("Parquet.Read: Brotli decompress failed: " ++))


{- | Native default quality11, window22, generic mode. The destination is sized
using the encoder's own bound; counts are checked before converting to Int.
-}
compress :: ByteString -> Either String ByteString
compress input = unsafeDupablePerformIO $ do
  bound <- maximumCompressedSize (fromIntegral (BS.length input))
  if bound == 0 || bound > fromIntegral (maxBound :: Int)
    then pure (Left "Parquet.Compress: Brotli output bound exceeds indexing range")
    else do
      output <- BSI.mallocByteString (fromIntegral bound)
      withForeignPtr output $ \destination -> U.unsafeUseAsCStringLen input $ \(source, count) -> alloca $ \size -> do
        poke size bound
        status <- encode 11 22 0 (fromIntegral count) (castPtr source) size destination
        actual <- peek size
        pure $
          if status == 1 && actual <= bound
            then
              let bytes = BSI.fromForeignPtr output 0 (fromIntegral actual)
                  -- Keep incompressible data without an extra copy, but do not
                  -- retain an input-sized destination for a tiny encoded page.
                  trimmed = if actual < bound `quot` 2 then BS.copy bytes else bytes
              in Right $! trimmed
            else Left "Parquet.Compress: Brotli compression failed"
