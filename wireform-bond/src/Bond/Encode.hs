{-# LANGUAGE BangPatterns #-}
-- | Bond Compact Binary v1 encoder.
--
-- Implements the Microsoft Bond Compact Binary protocol v1 wire format.
-- Field headers use packed delta/type encoding when possible.
-- Signed integers use ZigZag + LEB128 varint encoding.
--
-- Uses direct buffer writes via 'Proto.Encode.Direct.directEncode' to
-- avoid Builder allocation overhead.
module Bond.Encode
  ( encode
  ) where

import Data.Bits (countLeadingZeros, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word64)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import Foreign.ForeignPtr (withForeignPtr)
import qualified Data.ByteString.Internal as BSI
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Bond.Value
import Wireform.Encode.Direct (directEncode)

btStop :: Word8
btStop = 0

btStopBase :: Word8
btStopBase = 1

encode :: Value -> ByteString
encode val = directEncode (bondSize val) (writeBond val)
{-# INLINE encode #-}

-- Size computation

bondSize :: Value -> Int
bondSize (Bool _)     = 1
bondSize (Int8 n)     = zigZagVarintSize (fromIntegral n :: Int64)
bondSize (Int16 n)    = zigZagVarintSize (fromIntegral n :: Int64)
bondSize (Int32 n)    = zigZagVarintSize (fromIntegral n :: Int64)
bondSize (Int64 n)    = zigZagVarintSize n
bondSize (UInt8 n)    = varintSize (fromIntegral n :: Word64)
bondSize (UInt16 n)   = varintSize (fromIntegral n :: Word64)
bondSize (UInt32 n)   = varintSize (fromIntegral n :: Word64)
bondSize (UInt64 n)   = varintSize n
bondSize (Float _)    = 4
bondSize (Double _)   = 8
bondSize (String t)   = let !bs = TE.encodeUtf8 t
                            !len = BS.length bs
                        in varintSize (fromIntegral len) + len
-- WString is UTF-16-LE per the Bond compact-binary spec ('count' is
-- the number of 16-bit code units, /not/ bytes; 'characters' are
-- 2-byte UTF-16-LE code units).  Encode via 'utf16BytesLength' so
-- supplementary-plane code points expand to surrogate pairs.
bondSize (WString t)  = let !codeUnits = utf16CodeUnits t
                            !byteLen   = codeUnits * 2
                        in varintSize (fromIntegral codeUnits) + byteLen
bondSize (Blob bs)    = BS.length bs
bondSize (List et vs) = containerHeaderSize et (V.length vs) + V.foldl' (\s v -> s + bondSize v) 0 vs
bondSize (Set et vs)  = containerHeaderSize et (V.length vs) + V.foldl' (\s v -> s + bondSize v) 0 vs
bondSize (Map kt vt kvs) = 1 + containerHeaderSize vt (V.length kvs)
                           + V.foldl' (\s (k, v) -> s + bondSize k + bondSize v) 0 kvs
bondSize (Struct bases fields) = structSize bases fields
bondSize (Nullable Nothing)  = 1
bondSize (Nullable (Just v)) = 1 + bondSize v
bondSize (Enum n)     = zigZagVarintSize (fromIntegral n :: Int64)

structSize :: V.Vector Value -> V.Vector (Word16, BondType, Value) -> Int
structSize bases fields =
  let !baseSize = V.foldl' (\s baseVal -> s + basePartSize baseVal) 0 bases
      !fieldSize = fieldsSizeFrom 0 (sortFieldsForSize fields) 0
  in baseSize + fieldSize + 1
{-# INLINE structSize #-}

basePartSize :: Value -> Int
basePartSize (Struct bases fields) =
  let !bs = V.foldl' (\s baseVal -> s + basePartSize baseVal) 0 bases
      !fs = fieldsSizeFrom 0 (sortFieldsForSize fields) 0
  in bs + fs + 1
basePartSize _ = 1

-- | Local copy of 'sortFields' for the size pass: fieldsSizeFrom
-- depends on the previous field ID (delta encoding), so we must walk
-- in the same order as the writer.
sortFieldsForSize
  :: V.Vector (Word16, BondType, Value)
  -> V.Vector (Word16, BondType, Value)
sortFieldsForSize v
  | V.length v <= 1 = v
  | V.and (V.zipWith (\(a, _, _) (b, _, _) -> a <= b) v (V.tail v)) = v
  | otherwise = V.fromList
      (Data.List.sortBy
         (\(a, _, _) (b, _, _) -> compare a b)
         (V.toList v))

fieldsSizeFrom :: Word16 -> V.Vector (Word16, BondType, Value) -> Int -> Int
fieldsSizeFrom !_ fields !acc | V.null fields = acc
fieldsSizeFrom !prevId fields !acc =
  let (fid, bt, val) = V.head fields
      !rest = V.tail fields
      !hdrSz = fieldHeaderSize prevId fid bt
      !valSz = bondSize val
  in fieldsSizeFrom fid rest (acc + hdrSz + valSz)

fieldHeaderSize :: Word16 -> Word16 -> BondType -> Int
fieldHeaderSize prevId fieldId bt =
  let !delta = fieldId - prevId
      !_ = bondTypeId bt
  in if delta >= 1 && delta <= 5
     then 1
     else 1 + varintSize (fromIntegral fieldId :: Word64)
{-# INLINE fieldHeaderSize #-}

containerHeaderSize :: BondType -> Int -> Int
containerHeaderSize _bt count =
  if count >= 1 && count <= 7
  then 1
  else 1 + varintSize (fromIntegral count :: Word64)
{-# INLINE containerHeaderSize #-}

varintSize :: Word64 -> Int
varintSize !n =
  let !bits = 64 - countLeadingZeros (n .|. 1)
  in (bits + 6) `quot` 7
{-# INLINE varintSize #-}

zigZagVarintSize :: Int64 -> Int
zigZagVarintSize !n = varintSize (zigZagEncode n)
{-# INLINE zigZagVarintSize #-}

zigZagEncode :: Int64 -> Word64
zigZagEncode !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZagEncode #-}

-- Offset-based writers

writeBond :: Value -> Ptr Word8 -> Int -> IO Int
writeBond val p off = writeValue val p off
{-# INLINE writeBond #-}

writeValue :: Value -> Ptr Word8 -> Int -> IO Int
writeValue (Bool b)     p off = do pokeByteOff p off (if b then 1 :: Word8 else 0); pure $! off + 1
writeValue (Int8 n)     p off = writeZigZagVarint p off (fromIntegral n :: Int64)
writeValue (Int16 n)    p off = writeZigZagVarint p off (fromIntegral n :: Int64)
writeValue (Int32 n)    p off = writeZigZagVarint p off (fromIntegral n :: Int64)
writeValue (Int64 n)    p off = writeZigZagVarint p off n
writeValue (UInt8 n)    p off = writeVarint p off (fromIntegral n :: Word64)
writeValue (UInt16 n)   p off = writeVarint p off (fromIntegral n :: Word64)
writeValue (UInt32 n)   p off = writeVarint p off (fromIntegral n :: Word64)
writeValue (UInt64 n)   p off = writeVarint p off n
writeValue (Float f)    p off = do pokeByteOff p off (castFloatToWord32 f); pure $! off + 4
writeValue (Double d)   p off = do pokeByteOff p off (castDoubleToWord64 d); pure $! off + 8
writeValue (String t)   p off = writeTextValue p off t
writeValue (WString t)  p off = writeWStringValue p off t
writeValue (Blob bs)    p off = writeRawBytes p off bs
writeValue (List et vs) p off = do
  off1 <- writeContainerHeader p off et (V.length vs)
  V.foldM' (\o v -> writeValue v p o) off1 vs
writeValue (Set et vs)  p off = do
  off1 <- writeContainerHeader p off et (V.length vs)
  V.foldM' (\o v -> writeValue v p o) off1 vs
writeValue (Map kt vt kvs) p off = do
  pokeByteOff p off (bondTypeId kt)
  off1 <- writeContainerHeader p (off + 1) vt (V.length kvs)
  V.foldM' (\o (k, v) -> do o1 <- writeValue k p o; writeValue v p o1) off1 kvs
writeValue (Struct bases fields) p off = writeStruct p off bases fields
writeValue (Nullable Nothing)  p off = do pokeByteOff p off (0 :: Word8); pure $! off + 1
writeValue (Nullable (Just v)) p off = do pokeByteOff p off (1 :: Word8); writeValue v p (off + 1)
writeValue (Enum n)     p off = writeZigZagVarint p off (fromIntegral n :: Int64)

writeTextValue :: Ptr Word8 -> Int -> Text -> IO Int
writeTextValue p off t = do
  let !bs = TE.encodeUtf8 t
      !len = BS.length bs
  off1 <- writeVarint p off (fromIntegral len :: Word64)
  writeRawBytes p off1 bs
{-# INLINE writeTextValue #-}

-- | Encode a Bond @wstring@: varint count of UTF-16 code units, then
-- the code units themselves as 2-byte little-endian words.  BMP
-- code points encode to one code unit; supplementary-plane code
-- points (U+10000..U+10FFFF) encode to a high+low surrogate pair.
writeWStringValue :: Ptr Word8 -> Int -> Text -> IO Int
writeWStringValue p off t = do
  let !codeUnits = utf16CodeUnits t
  off1 <- writeVarint p off (fromIntegral codeUnits :: Word64)
  writeUtf16LE p off1 t
{-# INLINE writeWStringValue #-}

-- | Number of UTF-16 code units required to encode @t@. Each BMP
-- code point is one code unit; supplementary code points are two.
utf16CodeUnits :: Text -> Int
utf16CodeUnits = T.foldl' step 0
  where
    step !acc c = if Char.ord c < 0x10000 then acc + 1 else acc + 2
{-# INLINE utf16CodeUnits #-}

-- | Write @t@ as little-endian UTF-16 code units. Returns the
-- updated offset.
writeUtf16LE :: Ptr Word8 -> Int -> Text -> IO Int
writeUtf16LE p off0 t = T.foldl' step (pure off0) t >>= \o -> pure o
  where
    step :: IO Int -> Char -> IO Int
    step ioOff c = do
      !o <- ioOff
      let !cp = Char.ord c
      if cp < 0x10000
        then do
          pokeByteOff p o       (fromIntegral (cp .&. 0xFF) :: Word8)
          pokeByteOff p (o + 1) (fromIntegral ((cp `shiftR` 8) .&. 0xFF) :: Word8)
          pure $! o + 2
        else do
          let !n  = cp - 0x10000
              !hi = 0xD800 + (n `shiftR` 10)        -- 10 high bits of n
              !lo = 0xDC00 + (n .&. 0x3FF)          -- 10 low bits of n
          pokeByteOff p o       (fromIntegral (hi .&. 0xFF) :: Word8)
          pokeByteOff p (o + 1) (fromIntegral ((hi `shiftR` 8) .&. 0xFF) :: Word8)
          pokeByteOff p (o + 2) (fromIntegral (lo .&. 0xFF) :: Word8)
          pokeByteOff p (o + 3) (fromIntegral ((lo `shiftR` 8) .&. 0xFF) :: Word8)
          pure $! o + 4

writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes p off (BSI.BS fp len) = do
  withForeignPtr fp $ \src -> BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}

writeStruct :: Ptr Word8 -> Int -> V.Vector Value -> V.Vector (Word16, BondType, Value) -> IO Int
writeStruct p off bases fields = do
  off1 <- V.foldM' (\o baseVal -> writeBase p o baseVal) off bases
  off2 <- writeFields p off1 0 (sortFields fields)
  pokeByteOff p off2 btStop
  pure $! off2 + 1

writeBase :: Ptr Word8 -> Int -> Value -> IO Int
writeBase p off (Struct bases fields) = do
  off1 <- V.foldM' (\o baseVal -> writeBase p o baseVal) off bases
  off2 <- writeFields p off1 0 (sortFields fields)
  pokeByteOff p off2 btStopBase
  pure $! off2 + 1
writeBase p off _ = do
  pokeByteOff p off btStopBase
  pure $! off + 1

-- | Bond compact-binary v1 uses delta-encoded field IDs and the
-- decoder assumes ascending order. Sort by ID before emitting so
-- callers don't have to care; duplicate IDs are kept in their
-- original order (sortBy is stable enough — we use Vector
-- modifyM-style sort via list).
sortFields
  :: V.Vector (Word16, BondType, Value)
  -> V.Vector (Word16, BondType, Value)
sortFields v
  | V.length v <= 1 = v
  | isSorted v      = v
  | otherwise       = V.fromList (Data.List.sortBy
                                    (\(a, _, _) (b, _, _) -> compare a b)
                                    (V.toList v))
  where
    isSorted vs = V.and (V.zipWith (\(a, _, _) (b, _, _) -> a <= b) vs (V.tail vs))

writeFields :: Ptr Word8 -> Int -> Word16 -> V.Vector (Word16, BondType, Value) -> IO Int
writeFields _p off !_ fields | V.null fields = pure off
writeFields p off !prevId fields = do
  let (fid, bt, val) = V.head fields
      !rest = V.tail fields
  off1 <- writeFieldHeader p off prevId fid bt
  off2 <- writeValue val p off1
  writeFields p off2 fid rest

writeFieldHeader :: Ptr Word8 -> Int -> Word16 -> Word16 -> BondType -> IO Int
writeFieldHeader p off prevId fieldId bt = do
  let !delta = fieldId - prevId
      !tid = bondTypeId bt
  if delta >= 1 && delta <= 5
    then do pokeByteOff p off (fromIntegral (delta `shiftL` 5) .|. tid :: Word8); pure $! off + 1
    else do pokeByteOff p off tid; writeVarint p (off + 1) (fromIntegral fieldId :: Word64)
{-# INLINE writeFieldHeader #-}

writeContainerHeader :: Ptr Word8 -> Int -> BondType -> Int -> IO Int
writeContainerHeader p off bt count = do
  let !tid = bondTypeId bt
  if count >= 1 && count <= 7
    then do pokeByteOff p off (fromIntegral (count `shiftL` 5) .|. fromIntegral tid :: Word8); pure $! off + 1
    else do pokeByteOff p off tid; writeVarint p (off + 1) (fromIntegral count :: Word64)
{-# INLINE writeContainerHeader #-}

writeZigZagVarint :: Ptr Word8 -> Int -> Int64 -> IO Int
writeZigZagVarint p off n = writeVarint p off (zigZagEncode n)
{-# INLINE writeZigZagVarint #-}

writeVarint :: Ptr Word8 -> Int -> Word64 -> IO Int
writeVarint !p !off !n
  | n < 0x80 = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure $! off + 1
  | n < 0x4000 = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 1) (fromIntegral (n `shiftR` 7) :: Word8)
      pure $! off + 2
  | n < 0x200000 = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 1) (fromIntegral ((n `shiftR` 7) .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 2) (fromIntegral (n `shiftR` 14) :: Word8)
      pure $! off + 3
  | otherwise = writeVarintSlow p off n
{-# INLINE writeVarint #-}

writeVarintSlow :: Ptr Word8 -> Int -> Word64 -> IO Int
writeVarintSlow !p !off !n
  | n < 0x80 = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure $! off + 1
  | otherwise = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      writeVarintSlow p (off + 1) (n `shiftR` 7)
