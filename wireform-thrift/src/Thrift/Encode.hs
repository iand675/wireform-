{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | High-level Thrift encoding for Binary and Compact protocols.

Converts a 'Thrift.Value.Value' tree into a wire-format 'ByteString'.
Uses direct buffer writes via 'Proto.Encode.Direct.directEncode' to
avoid Builder allocation overhead.

@
import Thrift.Encode (encodeBinary, encodeCompact)
import qualified Thrift.Value as T
import qualified Data.Vector as V

let person = T.Struct (V.fromList [(1, T.String \"Alice\")])
let binBytes = encodeBinary person
let compactBytes = encodeCompact person
@
-}
module Thrift.Encode (
  encodeBinary,
  encodeBinaryBounded,
  encodeCompact,
  encodeCompactBounded,
  binaryEncodedSizeBounded,
  compactEncodedSizeBounded,
) where

import Data.Bits (countLeadingZeros, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int16, Int32, Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text.Encoding qualified as TE
import Data.Text.Foreign qualified as TF
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Int#, writeWord8OffAddrAsWord64#, (+#), (-#), (<#), (==#), (>#))
import GHC.Float (castDoubleToWord64)
import GHC.IO (IO (..))
import GHC.Ptr (Ptr (Ptr))
import GHC.Word (Word64 (W64#))
import Thrift.Value qualified as TV
import Thrift.Wire (ThriftType (..), thriftTypeToBin, thriftTypeToCompact)
import Wireform.Encode.Direct (directEncode)


--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

encodeBinary :: TV.Value -> ByteString
encodeBinary = either error id . encodeBinaryBounded maxBound
{-# INLINE encodeBinary #-}


-- | Validate the complete value and byte budget before allocating output.
encodeBinaryBounded :: Int -> TV.Value -> Either String ByteString
encodeBinaryBounded limit value = do
  size <- binaryEncodedSizeBounded limit value
  Right (directEncode size (writeBin value))


-- | Measure exactly without encoding strings or allocating output buffers.
binaryEncodedSizeBounded :: Int -> TV.Value -> Either String Int
binaryEncodedSizeBounded = checkedSize measureBinary#


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
encodeCompact = either error id . encodeCompactBounded maxBound
{-# INLINE encodeCompact #-}


-- | Encode only after exact sizing and validation fit the requested budget.
encodeCompactBounded :: Int -> TV.Value -> Either String ByteString
encodeCompactBounded limit value = do
  size <- compactEncodedSizeBounded limit value
  Right (directEncode size (writeComp value))


compactEncodedSizeBounded :: Int -> TV.Value -> Either String Int
compactEncodedSizeBounded = checkedSize measureCompact#


type Measure = Int# -> TV.Value -> (# String | Int# #)


-- Subtract from a remaining budget instead of adding sizes that can overflow.
-- Recursive workers return raw counters/results, never a per-value Either.
checkedSize :: Measure -> Int -> TV.Value -> Either String Int
checkedSize measure (I# limit) value = case limit <# 0# of
  1# -> Left "Thrift.Encode: negative size limit"
  _ -> case measure limit value of
    (# message | #) -> Left message
    (# | remaining #) -> Right (I# (limit -# remaining))
{-# INLINE checkedSize #-}


spend# :: Int -> Int# -> (# String | Int# #)
spend# (I# bytes) remaining = case bytes <# 0# of
  1# -> (# "Thrift.Encode: negative size" | #)
  _ -> case bytes ># remaining of
    1# -> (# "Thrift.Encode: encoded value exceeds size limit" | #)
    _ -> (# | remaining -# bytes #)
{-# INLINE spend# #-}


countFits :: Word64 -> Int -> Bool
countFits maximumCount count = count >= 0 && fromIntegral count <= maximumCount
{-# INLINE countFits #-}


measurePayload# :: Word64 -> Int -> Int -> Int# -> (# String | Int# #)
measurePayload# maximumCount prefixSize size remaining
  | not (countFits maximumCount size) = (# "Thrift.Encode: byte length exceeds protocol limit" | #)
  | otherwise = case spend# prefixSize remaining of
      (# message | #) -> (# message | #)
      (# | afterPrefix #) -> spend# size afterPrefix
{-# INLINE measurePayload# #-}


measureUUID# :: ByteString -> Int# -> (# String | Int# #)
measureUUID# bytes remaining
  | BS.length bytes /= 16 = (# "Thrift.Encode: UUID must contain exactly 16 bytes" | #)
  | otherwise = spend# 16 remaining
{-# INLINE measureUUID# #-}


measureBinary# :: Measure
measureBinary# remaining value = case value of
  TV.Bool _ -> spend# 1 remaining
  TV.Byte _ -> spend# 1 remaining
  TV.I16 _ -> spend# 2 remaining
  TV.I32 _ -> spend# 4 remaining
  TV.I64 _ -> spend# 8 remaining
  TV.Double _ -> spend# 8 remaining
  TV.String text -> measurePayload# 0x7fffffff 4 (TF.lengthWord8 text) remaining
  TV.Binary bytes -> measurePayload# 0x7fffffff 4 (BS.length bytes) remaining
  TV.UUID bytes -> measureUUID# bytes remaining
  TV.Struct fields -> case spend# 1 remaining of
    (# message | #) -> (# message | #)
    (# | rest #) ->
      if V.length fields > I# rest `quot` 3
        then (# "Thrift.Encode: encoded value exceeds size limit" | #)
        else measureFields# False 0 (sortBy (comparing fst) (V.toList fields)) rest
  TV.Map keyType valueType entries -> measureMap# False keyType valueType entries remaining
  TV.List elementType elements -> measureSequence# False elementType elements remaining
  TV.Set elementType elements -> measureSequence# False elementType elements remaining


measureCompact# :: Measure
measureCompact# remaining value = case value of
  TV.Bool _ -> spend# 1 remaining
  TV.Byte _ -> spend# 1 remaining
  TV.I16 n -> spend# (compVarintSize (fromIntegral (zigZagEncode32 (fromIntegral n)))) remaining
  TV.I32 n -> spend# (compVarintSize (fromIntegral (zigZagEncode32 n))) remaining
  TV.I64 n -> spend# (compVarintSize (zigZagEncode64 n)) remaining
  TV.Double _ -> spend# 8 remaining
  TV.String text -> bytes (TF.lengthWord8 text)
  TV.Binary valueBytes -> bytes (BS.length valueBytes)
  TV.UUID valueBytes -> measureUUID# valueBytes remaining
  TV.Struct fields -> case spend# 1 remaining of
    (# message | #) -> (# message | #)
    (# | rest #) ->
      if V.length fields > I# rest
        then (# "Thrift.Encode: encoded value exceeds size limit" | #)
        else measureFields# True 0 (sortBy (comparing fst) (V.toList fields)) rest
  TV.Map keyType valueType entries -> measureMap# True keyType valueType entries remaining
  TV.List elementType elements -> measureSequence# True elementType elements remaining
  TV.Set elementType elements -> measureSequence# True elementType elements remaining
  where
    bytes size = measurePayload# 0xffffffff (compVarintSize (fromIntegral size)) size remaining


measureFields# :: Bool -> Int16 -> [(Int16, TV.Value)] -> Int# -> (# String | Int# #)
measureFields# _ _ [] remaining = (# | remaining #)
measureFields# compact lastFid ((fid, value) : fields) remaining =
  let !delta = fromIntegral fid - fromIntegral lastFid :: Int
      !headerSize = if not compact then 3 else if delta > 0 && delta <= 15 then 1 else 1 + compVarintSize (fromIntegral (zigZagEncode32 (fromIntegral fid)))
  in case spend# headerSize remaining of
       (# message | #) -> (# message | #)
       (# | rest #) -> case value of
         TV.Bool _ | compact -> measureFields# compact fid fields rest
         _ -> case (if compact then measureCompact# else measureBinary#) rest value of
           (# message | #) -> (# message | #)
           (# | afterValue #) -> measureFields# compact fid fields afterValue


measureSequence# :: Bool -> ThriftType -> V.Vector TV.Value -> Int# -> (# String | Int# #)
measureSequence# compact elementType elements remaining
  | not (countFits (if compact then 0xffffffff else 0x7fffffff) count) = (# "Thrift.Encode: container length exceeds protocol limit" | #)
  | otherwise = case spend# headerSize remaining of
      (# message | #) -> (# message | #)
      (# | rest #) ->
        if count > I# rest
          then (# "Thrift.Encode: encoded value exceeds size limit" | #)
          else
            let go index budget = case index ==# count# of
                  1# -> (# | budget #)
                  _ ->
                    let value = V.unsafeIndex elements (I# index)
                    in if TV.thriftTypeOf value /= elementType
                         then (# "Thrift.Encode: container element type mismatch" | #)
                         else case (if compact then measureCompact# else measureBinary#) budget value of
                           (# message | #) -> (# message | #)
                           (# | next #) -> go (index +# 1#) next
            in go 0# rest
  where
    !count@(I# count#) = V.length elements
    !headerSize = if not compact then 5 else if count < 15 then 1 else 1 + compVarintSize (fromIntegral count)


measureMap# :: Bool -> ThriftType -> ThriftType -> V.Vector (TV.Value, TV.Value) -> Int# -> (# String | Int# #)
measureMap# compact keyType valueType entries remaining
  | not (countFits (if compact then 0xffffffff else 0x7fffffff) count) = (# "Thrift.Encode: container length exceeds protocol limit" | #)
  | otherwise = case spend# headerSize remaining of
      (# message | #) -> (# message | #)
      (# | rest #) ->
        if count > I# rest `quot` 2
          then (# "Thrift.Encode: encoded value exceeds size limit" | #)
          else
            let go index budget = case index ==# count# of
                  1# -> (# | budget #)
                  _ ->
                    let (key, value) = V.unsafeIndex entries (I# index)
                    in if TV.thriftTypeOf key /= keyType || TV.thriftTypeOf value /= valueType
                         then (# "Thrift.Encode: map key/value type mismatch" | #)
                         else case (if compact then measureCompact# else measureBinary#) budget key of
                           (# message | #) -> (# message | #)
                           (# | afterKey #) -> case (if compact then measureCompact# else measureBinary#) afterKey value of
                             (# message | #) -> (# message | #)
                             (# | next #) -> go (index +# 1#) next
            in go 0# rest
  where
    !count@(I# count#) = V.length entries
    !headerSize = if not compact then 6 else if count == 0 then 1 else 1 + compVarintSize (fromIntegral count)


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
  _ -> writeCompValue val p off
{-# INLINE writeComp #-}


writeCompStruct :: Ptr Word8 -> Int -> V.Vector (Int16, TV.Value) -> IO Int
writeCompStruct p off fields = do
  let sorted = sortBy (comparing fst) (V.toList fields)
  off1 <- goWriteCompStruct p off 0 sorted
  pokeByteOff p off1 (0x00 :: Word8)
  pure $! off1 + 1


goWriteCompStruct :: Ptr Word8 -> Int -> Int16 -> [(Int16, TV.Value)] -> IO Int
goWriteCompStruct _ off _ [] = pure off
goWriteCompStruct p off !lastFid ((fid, v) : rest) = do
  let !delta = fid - lastFid
      !ctype = case v of
        TV.Bool b -> if b then 1 else 2
        _ -> thriftTypeToCompact (TV.thriftTypeOf v)
  off1 <-
    if delta > 0 && delta <= 15
      then do
        pokeByteOff p off (fromIntegral delta `shiftL` 4 .|. ctype :: Word8)
        pure $! off + 1
      else do
        pokeByteOff p off ctype
        writeVarint p (off + 1) (fromIntegral (zigZagEncode32 (fromIntegral fid)))
  off2 <- case v of
    TV.Bool _ -> pure off1
    _ -> writeCompValue v p off1
  goWriteCompStruct p off2 fid rest


writeCompValue :: TV.Value -> Ptr Word8 -> Int -> IO Int
writeCompValue val p off = case val of
  TV.Bool b -> do
    pokeByteOff p off (if b then 1 :: Word8 else 2)
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
writeLE64 (Ptr address) (I# offset) value =
  let !little = case targetByteOrder of LittleEndian -> value; BigEndian -> byteSwap64 value
  in case little of
       W64# word -> IO $ \state ->
         case writeWord8OffAddrAsWord64# address offset word state of
           next -> (# next, I# (offset +# 8#) #)
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
