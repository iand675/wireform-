{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Checked PLAIN layout shared by nested auxiliary-data generators.
module Parquet.Nested.Plain (
  PlainLayout (..),
  plainLayout,
  validPlainValues,
  lengthAt,
  word16At,
  word32At,
  word64At,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Word (Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (readWord8OffAddrAsWord16#, readWord8OffAddrAsWord32#, readWord8OffAddrAsWord64#)
import GHC.IO (IO (..))
import GHC.Ptr (Ptr (..))
import GHC.Word (Word16 (W16#), Word32 (W32#), Word64 (W64#))
import Parquet.Types (ParquetType (..), SchemaElement (..))


data PlainLayout = BooleanValues | VariableValues | FixedValues {-# UNPACK #-} !Int


plainLayout :: SchemaElement -> Either String PlainLayout
plainLayout field = case seType field of
  Just PTBoolean -> Right BooleanValues
  Just PTByteArray -> Right VariableValues
  Just PTInt32 -> Right (FixedValues 4)
  Just PTInt64 -> Right (FixedValues 8)
  Just PTInt96 -> Right (FixedValues 12)
  Just PTFloat -> Right (FixedValues 4)
  Just PTDouble -> Right (FixedValues 8)
  Just PTFixedLenByteArray -> case seTypeLength field of
    Just width | width > 0 -> Right (FixedValues (fromIntegral width))
    _ -> invalid
  _ -> invalid
  where
    invalid = Left "Parquet.Nested: missing or invalid physical PLAIN layout"


validPlainValues :: PlainLayout -> Int -> ByteString -> Bool
validPlainValues layout count bytes
  | count < 0 = False
  | otherwise = case layout of
      BooleanValues -> BS.length bytes == count `quot` 8 + if count `rem` 8 == 0 then 0 else 1
      FixedValues width -> toInteger count * toInteger width == toInteger (BS.length bytes)
      VariableValues -> go 0 0
  where
    !size = BS.length bytes
    go !index !offset
      | index == count = offset == size
      | size - offset < 4 = False
      | otherwise =
          let !length_ = lengthAt bytes offset
          in length_ >= 0 && length_ <= 2147483647 && length_ <= size - offset - 4 && go (index + 1) (offset + 4 + length_)


-- Only call these readers within a checked logical slice. They read exactly
-- the named bytes, require no alignment, and are portable across host order.
word16At :: ByteString -> Int -> Word16
word16At bytes offset = BSI.accursedUnutterablePerformIO $ do
  let (fp, start, _) = BSI.toForeignPtr bytes
  value <- case unsafeForeignPtrToPtr fp `plusPtr` (start + offset) of
    Ptr address -> IO $ \s -> case readWord8OffAddrAsWord16# address 0# s of
      (# next, word #) -> (# next, W16# word #)
  touchForeignPtr fp
  pure (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap16 value)
{-# INLINE word16At #-}


word32At :: ByteString -> Int -> Word32
word32At bytes offset = BSI.accursedUnutterablePerformIO $ do
  let (fp, start, _) = BSI.toForeignPtr bytes
  value <- case unsafeForeignPtrToPtr fp `plusPtr` (start + offset) of
    Ptr address -> IO $ \s -> case readWord8OffAddrAsWord32# address 0# s of
      (# next, word #) -> (# next, W32# word #)
  touchForeignPtr fp
  pure (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap32 value)
{-# INLINE word32At #-}


word64At :: ByteString -> Int -> Word64
word64At bytes offset = BSI.accursedUnutterablePerformIO $ do
  let (fp, start, _) = BSI.toForeignPtr bytes
  value <- case unsafeForeignPtrToPtr fp `plusPtr` (start + offset) of
    Ptr address -> IO $ \s -> case readWord8OffAddrAsWord64# address 0# s of
      (# next, word #) -> (# next, W64# word #)
  touchForeignPtr fp
  pure (case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap64 value)
{-# INLINE word64At #-}


lengthAt :: ByteString -> Int -> Int
lengthAt bytes offset = fromIntegral (word32At bytes offset)
{-# INLINE lengthAt #-}
