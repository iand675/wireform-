{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Raw LZO1X blocks. Validation measures output before allocating it. The
codec has no Hadoop/lzop framing and retains full size_t counts across FFI.
-}
module Columnar.LZO (
  compress,
  decompress,
  compressBound,
  LZOError (..),
  prettyLZOError,
) where

import Control.Exception (mask_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)


foreign import ccall unsafe "hs_columnar_lzo1x_compress_bound"
  c_bound :: CSize -> CSize


foreign import ccall unsafe "hs_columnar_lzo1x_measure"
  c_measure :: Ptr Word8 -> CSize -> CSize -> Ptr CSize -> IO CInt


foreign import ccall safe "hs_columnar_lzo1x_measure"
  c_measure_safe :: Ptr Word8 -> CSize -> CSize -> Ptr CSize -> IO CInt


foreign import ccall unsafe "hs_columnar_lzo1x_decompress"
  c_decode :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt


foreign import ccall safe "hs_columnar_lzo1x_decompress"
  c_decode_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt


foreign import ccall unsafe "hs_columnar_lzo1x_compress"
  c_encode :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt


foreign import ccall safe "hs_columnar_lzo1x_compress"
  c_encode_safe :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt


data LZOError
  = LZOInvalidOutputLimit
  | LZOLengthTooLarge
  | LZOFailure {-# UNPACK #-} !Int
  deriving (Show, Eq)


prettyLZOError :: LZOError -> String
prettyLZOError LZOInvalidOutputLimit = "LZO: negative output limit"
prettyLZOError LZOLengthTooLarge = "LZO: input/output length exceeds the codec or host bound"
prettyLZOError (LZOFailure code) =
  "LZO: " ++ case code of
    1 -> "truncated input"
    2 -> "output budget exceeded"
    3 -> "invalid match history"
    4 -> "invalid end marker or trailing input"
    5 -> "length overflow"
    _ -> "codec failure (status " ++ show code ++ ")"


{- | Standard LZO worst-case capacity, or -1 if it cannot be represented.
The encoder's dictionary positions cover at most UINT32_MAX input bytes.
-}
compressBound :: Int -> Int
compressBound n
  | n < 0 = -1
  | bound == 0 || bound > fromIntegral (maxBound :: Int) = -1
  | otherwise = fromIntegral bound
  where
    bound = c_bound (fromIntegral n)
{-# INLINE compressBound #-}


-- | Compress one block. Empty input has the canonical three-byte end marker.
compress :: ByteString -> Either LZOError ByteString
compress input
  | capacity < 0 = Left LZOLengthTooLarge
  | otherwise = unsafePerformIO $
      mask_ $
        BSU.unsafeUseAsCStringLen input $ \(source, len) ->
          alloca $ \written -> do
            output <- mallocForeignPtrBytes capacity
            withForeignPtr output $ \destination -> do
              let encode = if len <= 65536 && capacity <= 65536 then c_encode else c_encode_safe
              status <- encode (castPtr source) (fromIntegral len) destination (fromIntegral capacity) written
              if status /= 0
                then pure (Left (LZOFailure (fromIntegral status)))
                else do
                  actual <- peek written
                  if actual > fromIntegral capacity
                    then pure (Left LZOLengthTooLarge)
                    else pure (Right (BSI.fromForeignPtr output 0 (fromIntegral actual)))
  where
    capacity = compressBound (BS.length input)
{-# NOINLINE compress #-}


{- | Decode one complete raw block, permitting at most the supplied output
bytes. The measurement pass is O(compressed length), does not touch output
memory and rejects malformed/oversized streams before allocation. Larger
FFI calls permit GC; source and destination owners span every call.
-}
decompress :: Int -> ByteString -> Either LZOError ByteString
decompress limit input
  | limit < 0 = Left LZOInvalidOutputLimit
  | otherwise = unsafePerformIO $
      mask_ $
        BSU.unsafeUseAsCStringLen input $ \(source, len) ->
          alloca $ \written -> do
            let measure = if len <= 65536 then c_measure else c_measure_safe
            status <- measure (castPtr source) (fromIntegral len) (fromIntegral limit) written
            if status /= 0
              then pure (Left (LZOFailure (fromIntegral status)))
              else do
                measured <- peek written
                if measured > fromIntegral limit
                  then pure (Left LZOLengthTooLarge)
                  else
                    if measured == 0
                      then pure (Right BS.empty)
                      else do
                        output <- mallocForeignPtrBytes (fromIntegral measured)
                        withForeignPtr output $ \destination -> do
                          let decode = if len <= 65536 && measured <= 65536 then c_decode else c_decode_safe
                          result <- decode (castPtr source) (fromIntegral len) destination measured written
                          actual <- peek written
                          if result /= 0
                            then pure (Left (LZOFailure (fromIntegral result)))
                            else
                              if actual /= measured
                                then pure (Left LZOLengthTooLarge)
                                else pure (Right (BSI.fromForeignPtr output 0 (fromIntegral measured)))
{-# NOINLINE decompress #-}
