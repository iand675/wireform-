{-# LANGUAGE BangPatterns #-}
-- | High-level Thrift encoding for Binary and Compact protocols.
--
-- Converts a 'Thrift.Value.Value' tree into a wire-format 'ByteString'.
-- Uses direct buffer writes via 'Proto.Encode.Direct.directEncode' to
-- avoid Builder allocation overhead.
--
-- @
-- import Thrift.Encode (encodeBinary, encodeCompact)
-- import qualified Thrift.Value as T
-- import qualified Data.Vector as V
--
-- let person = T.Struct (V.fromList [(1, T.String \"Alice\")])
-- let binBytes = encodeBinary person
-- let compactBytes = encodeCompact person
-- @
module Thrift.Encode
  ( encodeBinary
  , encodeCompact
  , encodeCompactSorted
  ) where

import Data.Bits (countLeadingZeros, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int16, Int32, Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64)

import qualified Thrift.Value as TV
import Thrift.Wire (ThriftType(..), thriftTypeToBin, thriftTypeToCompact)
import Wireform.Encode.Direct (directEncode)

--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

encodeBinary :: TV.Value -> ByteString
encodeBinary val = directEncode (binValueSize val) (writeBin val)
{-# INLINE encodeBinary #-}

binValueSize :: TV.Value -> Int
binValueSize = \case
  TV.Bool _   -> 1
  TV.Byte _   -> 1
  TV.I16 _    -> 2
  TV.I32 _    -> 4
  TV.I64 _    -> 8
  TV.Double _ -> 8
  TV.String t -> let !bs = TE.encodeUtf8 t in 4 + BS.length bs
  TV.Binary b -> 4 + BS.length b
  TV.UUID _   -> 16
  TV.Struct fields ->
    let sorted = sortBy (comparing fst) (V.toList fields)
        fieldsSize = sum [3 + binValueSize v | (_fid, v) <- sorted]
    in fieldsSize + 1
  TV.Map _kt _vt entries ->
    6 + V.foldl' (\acc (k, v) -> acc + binValueSize k + binValueSize v) 0 entries
  TV.List _et elems ->
    5 + V.foldl' (\acc v -> acc + binValueSize v) 0 elems
  TV.Set _et elems ->
    5 + V.foldl' (\acc v -> acc + binValueSize v) 0 elems

writeBin :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeBin = writeBinValue
{-# INLINE writeBin #-}

writeBinValue :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeBinValue val p off = case val of
  TV.Bool b -> do
    pokeByteOff p off (if b then 1 :: Word8 else 0)
    pure $! off + 1
  TV.Byte v -> do
    pokeByteOff p off (fromIntegral v :: Word8)
    pure $! off + 1
  TV.I16 v -> writeBE16 p off (fromIntegral v)
  TV.I32 v -> writeBE32 p off (fromIntegral v)
  TV.I64 v -> writeBE64 p off (fromIntegral v)
  TV.Double d -> writeBE64 p off (castDoubleToWord64 d)
  TV.String t -> do
    let !bs = TE.encodeUtf8 t
    writeBinBytes p off bs
  TV.Binary b -> writeBinBytes p off b
  TV.UUID b -> writeRaw p off b

  TV.Struct fields -> do
    let sorted = sortBy (comparing fst) (V.toList fields)
    off1 <- writeBinStructFields p off sorted
    pokeByteOff p off1 (0x00 :: Word8)
    pure $! off1 + 1

  TV.Map kt vt entries -> do
    pokeByteOff p off (thriftTypeToBin kt)
    pokeByteOff p (off + 1) (thriftTypeToBin vt)
    off1 <- writeBE32 p (off + 2) (fromIntegral (V.length entries) :: Word32)
    V.foldM' (\o (k, v) -> do o1 <- writeBinValue k p o; writeBinValue v p o1) off1 entries

  TV.List et elems -> do
    pokeByteOff p off (thriftTypeToBin et)
    off1 <- writeBE32 p (off + 1) (fromIntegral (V.length elems) :: Word32)
    V.foldM' (\o v -> writeBinValue v p o) off1 elems

  TV.Set et elems -> do
    pokeByteOff p off (thriftTypeToBin et)
    off1 <- writeBE32 p (off + 1) (fromIntegral (V.length elems) :: Word32)
    V.foldM' (\o v -> writeBinValue v p o) off1 elems

writeBinStructFields :: Ptr Word8 -> Int -> [(Int16, TV.Value)] -> IO Int
writeBinStructFields _ off [] = pure off
writeBinStructFields p off ((fid, v) : rest) = do
  pokeByteOff p off (thriftTypeToBin (TV.thriftTypeOf v))
  off1 <- writeBE16 p (off + 1) (fromIntegral fid :: Word16)
  off2 <- writeBinValue v p off1
  writeBinStructFields p off2 rest

writeBinBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeBinBytes p off bs = do
  off1 <- writeBE32 p off (fromIntegral (BS.length bs) :: Word32)
  writeRaw p off1 bs
{-# INLINE writeBinBytes #-}

--------------------------------------------------------------------------------
-- Compact Protocol
--------------------------------------------------------------------------------

encodeCompact :: TV.Value -> ByteString
encodeCompact val = directEncode (compValueSize val) (writeComp val)
{-# INLINE encodeCompact #-}

-- | Like 'encodeCompact' but assumes that every 'TV.Struct' in the
-- value tree (transitively) already has its fields stored in
-- ascending @fid@ order. This skips the per-struct
-- @sortBy . V.toList@ that 'encodeCompact' otherwise performs once
-- for the size pass and once for the write pass.
--
-- Wireform's own @ColumnChunk@ / @ColumnMetaData@ / @Statistics@ /
-- @KeyValue@ / @PageEncodingStats@ / @SchemaElement@ / @RowGroup@ /
-- @FileMetaData@ encoders all build their field vectors in ascending
-- order via "Parquet.Footer.Build", so the Parquet writer uses this
-- entry point. Callers whose data may be unsorted should keep using
-- 'encodeCompact'; passing unsorted fields here will produce
-- malformed (but parseable) output because the delta-tag header
-- relies on monotonically increasing @fid@.
encodeCompactSorted :: TV.Value -> ByteString
encodeCompactSorted val =
  directEncode (compValueSizeSorted val) (writeCompSorted val)
{-# INLINE encodeCompactSorted #-}

compValueSize :: TV.Value -> Int
compValueSize val = case val of
  TV.Struct fields -> compStructSize fields
  _                -> compPrimitiveSize val

compValueSizeSorted :: TV.Value -> Int
compValueSizeSorted val = case val of
  TV.Struct fields -> compStructSizeSorted fields
  _                -> compPrimitiveSizeSorted val

compStructSize :: V.Vector (Int16, TV.Value) -> Int
compStructSize fields =
  let sorted = sortBy (comparing fst) (V.toList fields)
  in goStructSize 0 sorted + 1

-- | Sorted-fast-path: walk the 'Vector' in place, no list copy, no sort.
compStructSizeSorted :: V.Vector (Int16, TV.Value) -> Int
compStructSizeSorted fields = goSorted 0 0 (V.length fields) + 1
  where
    goSorted !lastFid !i !n
      | i >= n = 0
      | otherwise =
          let (!fid, !v) = V.unsafeIndex fields i
              !delta = fid - lastFid
              !hdrSz = if delta > 0 && delta <= 15 then 1
                       else 1 + compVarintSize (fromIntegral (zigZagEncode32 (fromIntegral fid)))
              !valSz = case v of
                         TV.Bool _ -> 0
                         _         -> compPrimitiveSizeSorted v
          in hdrSz + valSz + goSorted fid (i + 1) n

goStructSize :: Int16 -> [(Int16, TV.Value)] -> Int
goStructSize _ [] = 0
goStructSize !lastFid ((fid, v) : rest) =
  let !delta = fid - lastFid
      !hdrSz = if delta > 0 && delta <= 15 then 1
               else 1 + compVarintSize (fromIntegral (zigZagEncode32 (fromIntegral fid)))
      !valSz = case v of
                 TV.Bool _ -> 0
                 _         -> compPrimitiveSize v
  in hdrSz + valSz + goStructSize fid rest

compPrimitiveSize :: TV.Value -> Int
compPrimitiveSize = \case
  TV.Bool _   -> 1
  TV.Byte _   -> 1
  TV.I16 v    -> compVarintSize (fromIntegral (zigZagEncode32 (fromIntegral v)))
  TV.I32 v    -> compVarintSize (fromIntegral (zigZagEncode32 v))
  TV.I64 v    -> compVarintSize (zigZagEncode64 v)
  TV.Double _ -> 8
  TV.String t -> let !bs = TE.encodeUtf8 t in compVarintSize (fromIntegral (BS.length bs)) + BS.length bs
  TV.Binary b -> compVarintSize (fromIntegral (BS.length b)) + BS.length b
  TV.UUID _   -> 16
  TV.Struct fields -> compStructSize fields
  TV.Map _kt _vt entries ->
    if V.null entries
    then 1
    else compVarintSize (fromIntegral (V.length entries))
         + 1
         + V.foldl' (\acc (k, v) -> acc + compPrimitiveSize k + compPrimitiveSize v) 0 entries
  TV.List _et elems ->
    let !sz = V.length elems
        !hdrSz = if sz < 15 then 1 else 1 + compVarintSize (fromIntegral sz)
    in hdrSz + V.foldl' (\acc v -> acc + compPrimitiveSize v) 0 elems
  TV.Set _et elems ->
    let !sz = V.length elems
        !hdrSz = if sz < 15 then 1 else 1 + compVarintSize (fromIntegral sz)
    in hdrSz + V.foldl' (\acc v -> acc + compPrimitiveSize v) 0 elems

-- | Sorted-fast-path mirror of 'compPrimitiveSize'. Recurses through
-- 'TV.Struct' via 'compStructSizeSorted' so nested structs also skip
-- the sort.
compPrimitiveSizeSorted :: TV.Value -> Int
compPrimitiveSizeSorted = \case
  TV.Struct fields -> compStructSizeSorted fields
  TV.Map _kt _vt entries ->
    if V.null entries
    then 1
    else compVarintSize (fromIntegral (V.length entries))
         + 1
         + V.foldl' (\acc (k, v) -> acc
                       + compPrimitiveSizeSorted k
                       + compPrimitiveSizeSorted v) 0 entries
  TV.List _et elems ->
    let !sz = V.length elems
        !hdrSz = if sz < 15 then 1 else 1 + compVarintSize (fromIntegral sz)
    in hdrSz + V.foldl' (\acc v -> acc + compPrimitiveSizeSorted v) 0 elems
  TV.Set _et elems ->
    let !sz = V.length elems
        !hdrSz = if sz < 15 then 1 else 1 + compVarintSize (fromIntegral sz)
    in hdrSz + V.foldl' (\acc v -> acc + compPrimitiveSizeSorted v) 0 elems
  -- Leaf primitives don't carry sub-structs; reuse the existing
  -- (already-no-sort) implementation.
  v -> compPrimitiveSize v

compVarintSize :: Word64 -> Int
compVarintSize !n =
  let !bits = 64 - countLeadingZeros (n .|. 1)
  in (bits + 6) `quot` 7
{-# INLINE compVarintSize #-}

zigZagEncode32 :: Int32 -> Word32
zigZagEncode32 !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))
{-# INLINE zigZagEncode32 #-}

zigZagEncode64 :: Int64 -> Word64
zigZagEncode64 !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZagEncode64 #-}

writeComp :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeComp val p off = case val of
  TV.Struct fields -> writeCompStruct p off fields
  _                -> writeCompValue val p off
{-# INLINE writeComp #-}

-- | Sorted-fast-path mirror of 'writeComp'. Skips the
-- 'sortBy . V.toList' that 'writeCompStruct' otherwise performs,
-- assuming the caller built each 'TV.Struct' with ascending @fid@s.
writeCompSorted :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeCompSorted val p off = case val of
  TV.Struct fields -> writeCompStructSorted p off fields
  _                -> writeCompValueSorted val p off
{-# INLINE writeCompSorted #-}

writeCompStruct :: Ptr Word8 -> Int -> V.Vector (Int16, TV.Value) -> IO Int
writeCompStruct p off fields = do
  let sorted = sortBy (comparing fst) (V.toList fields)
  off1 <- goWriteCompStruct p off 0 sorted
  pokeByteOff p off1 (0x00 :: Word8)
  pure $! off1 + 1

-- | Walk the 'Vector' directly, no copy, no sort. Recurses through
-- nested structs via 'writeCompValueSorted'.
writeCompStructSorted
  :: Ptr Word8 -> Int -> V.Vector (Int16, TV.Value) -> IO Int
writeCompStructSorted p off fields = do
  off1 <- goSorted 0 0 (V.length fields) off
  pokeByteOff p off1 (0x00 :: Word8)
  pure $! off1 + 1
  where
    goSorted !lastFid !i !n !cursor
      | i >= n = pure cursor
      | otherwise = do
          let (!fid, !v) = V.unsafeIndex fields i
              !delta = fid - lastFid
              !ctype = case v of
                         TV.Bool b -> if b then 1 else 2
                         _         -> thriftTypeToCompact (TV.thriftTypeOf v)
          c1 <- if delta > 0 && delta <= 15
            then do
              pokeByteOff p cursor (fromIntegral delta `shiftL` 4 .|. ctype :: Word8)
              pure $! cursor + 1
            else do
              pokeByteOff p cursor ctype
              writeVarint p (cursor + 1) (fromIntegral (zigZagEncode32 (fromIntegral fid)))
          c2 <- case v of
            TV.Bool _ -> pure c1
            _         -> writeCompValueSorted v p c1
          goSorted fid (i + 1) n c2

goWriteCompStruct :: Ptr Word8 -> Int -> Int16 -> [(Int16, TV.Value)] -> IO Int
goWriteCompStruct _ off _ [] = pure off
goWriteCompStruct p off !lastFid ((fid, v) : rest) = do
  let !delta = fid - lastFid
      !ctype = case v of
                 TV.Bool b -> if b then 1 else 2
                 _         -> thriftTypeToCompact (TV.thriftTypeOf v)
  off1 <- if delta > 0 && delta <= 15
    then do
      pokeByteOff p off (fromIntegral delta `shiftL` 4 .|. ctype :: Word8)
      pure $! off + 1
    else do
      pokeByteOff p off ctype
      writeVarint p (off + 1) (fromIntegral (zigZagEncode32 (fromIntegral fid)))
  off2 <- case v of
    TV.Bool _ -> pure off1
    _         -> writeCompValue v p off1
  goWriteCompStruct p off2 fid rest

writeCompValue :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeCompValue val p off = case val of
  TV.Bool b -> do
    pokeByteOff p off (if b then 1 :: Word8 else 0)
    pure $! off + 1
  TV.Byte v -> do
    pokeByteOff p off (fromIntegral v :: Word8)
    pure $! off + 1
  TV.I16 v -> writeVarint p off (fromIntegral (zigZagEncode32 (fromIntegral v)))
  TV.I32 v -> writeVarint p off (fromIntegral (zigZagEncode32 v))
  TV.I64 v -> writeVarint p off (zigZagEncode64 v)
  TV.Double d -> writeLE64 p off (castDoubleToWord64 d)
  TV.String t -> do
    let !bs = TE.encodeUtf8 t
    writeCompBytes p off bs
  TV.Binary b -> writeCompBytes p off b
  TV.UUID b -> writeRaw p off b
  TV.Struct fields -> writeCompStruct p off fields
  TV.Map kt vt entries ->
    if V.null entries
    then do pokeByteOff p off (0x00 :: Word8); pure $! off + 1
    else do
      off1 <- writeVarint p off (fromIntegral (V.length entries))
      pokeByteOff p off1 (thriftTypeToCompact kt `shiftL` 4 .|. thriftTypeToCompact vt :: Word8)
      V.foldM' (\o (k, v) -> do o1 <- writeCompValue k p o; writeCompValue v p o1) (off1 + 1) entries
  TV.List et elems -> do
    off1 <- writeCompListHeader p off et (V.length elems)
    V.foldM' (\o v -> writeCompValue v p o) off1 elems
  TV.Set et elems -> do
    off1 <- writeCompListHeader p off et (V.length elems)
    V.foldM' (\o v -> writeCompValue v p o) off1 elems

-- | Sorted-fast-path mirror of 'writeCompValue'. Recurses into
-- 'TV.Struct' / 'TV.List' / 'TV.Set' / 'TV.Map' via the sorted
-- variants so nested structs also skip the sort.
writeCompValueSorted :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeCompValueSorted val p off = case val of
  TV.Struct fields -> writeCompStructSorted p off fields
  TV.Map kt vt entries ->
    if V.null entries
    then do pokeByteOff p off (0x00 :: Word8); pure $! off + 1
    else do
      off1 <- writeVarint p off (fromIntegral (V.length entries))
      pokeByteOff p off1 (thriftTypeToCompact kt `shiftL` 4 .|. thriftTypeToCompact vt :: Word8)
      V.foldM' (\o (k, v) -> do o1 <- writeCompValueSorted k p o
                                writeCompValueSorted v p o1) (off1 + 1) entries
  TV.List et elems -> do
    off1 <- writeCompListHeader p off et (V.length elems)
    V.foldM' (\o v -> writeCompValueSorted v p o) off1 elems
  TV.Set et elems -> do
    off1 <- writeCompListHeader p off et (V.length elems)
    V.foldM' (\o v -> writeCompValueSorted v p o) off1 elems
  -- Leaf primitives are identical to the unsorted path.
  v -> writeCompValue v p off

writeCompBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeCompBytes p off bs = do
  off1 <- writeVarint p off (fromIntegral (BS.length bs))
  writeRaw p off1 bs
{-# INLINE writeCompBytes #-}

writeCompListHeader :: Ptr Word8 -> Int -> ThriftType -> Int -> IO Int
writeCompListHeader p off et sz = do
  let !ctype = thriftTypeToCompact et
  if sz < 15
    then do pokeByteOff p off (fromIntegral sz `shiftL` 4 .|. ctype :: Word8); pure $! off + 1
    else do pokeByteOff p off (0xF0 .|. ctype :: Word8); writeVarint p (off + 1) (fromIntegral sz)
{-# INLINE writeCompListHeader #-}

--------------------------------------------------------------------------------
-- Shared write helpers
--------------------------------------------------------------------------------

writeBE16 :: Ptr Word8 -> Int -> Word16 -> IO Int
writeBE16 p off w = do
  pokeByteOff p off (fromIntegral (w `shiftR` 8) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral (w .&. 0xFF) :: Word8)
  pure $! off + 2
{-# INLINE writeBE16 #-}

writeBE32 :: Ptr Word8 -> Int -> Word32 -> IO Int
writeBE32 p off w = do
  pokeByteOff p off (fromIntegral (w `shiftR` 24) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral (w .&. 0xFF) :: Word8)
  pure $! off + 4
{-# INLINE writeBE32 #-}

writeBE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeBE64 p off w = do
  pokeByteOff p off (fromIntegral (w `shiftR` 56) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 48) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 40) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 32) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral (w .&. 0xFF) :: Word8)
  pure $! off + 8
{-# INLINE writeBE64 #-}

writeLE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeLE64 p off w = do
  pokeByteOff p off w
  pure $! off + 8
{-# INLINE writeLE64 #-}

writeRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRaw p off (BSI.BS fp len) = do
  withForeignPtr fp $ \src -> BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRaw #-}

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
