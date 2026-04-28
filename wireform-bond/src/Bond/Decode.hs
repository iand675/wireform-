{-# LANGUAGE BangPatterns #-}
-- | Bond Compact Binary v1 decoder.
--
-- Implements the Microsoft Bond Compact Binary protocol v1 wire format decoder.
-- Field headers use packed delta/type encoding.
-- Signed integers use ZigZag + LEB128 varint encoding.
module Bond.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.Char as Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Bond.Value
import Wireform.FFI (decodeTextFast)

btStop :: Word8
btStop = 0

btStopBase :: Word8
btStopBase = 1

type Offset = Int

decode :: BondType -> ByteString -> Either String Value
decode bt bs = case decodeValue bt bs 0 of
  Left err        -> Left err
  Right (val, _)  -> Right val

decodeValue :: BondType -> ByteString -> Offset -> Either String (Value, Offset)
decodeValue BT_BOOL   bs off = decodeBool bs off
decodeValue BT_INT8   bs off = do { (w, o) <- decodeVarint bs off; Right (Int8 (fromIntegral (zigZagDecode w)), o) }
decodeValue BT_INT16  bs off = do { (w, o) <- decodeVarint bs off; Right (Int16 (fromIntegral (zigZagDecode w)), o) }
decodeValue BT_INT32  bs off = do { (w, o) <- decodeVarint bs off; Right (Int32 (fromIntegral (zigZagDecode w)), o) }
decodeValue BT_INT64  bs off = do { (w, o) <- decodeVarint bs off; Right (Int64 (zigZagDecode w), o) }
decodeValue BT_UINT8  bs off = do { (w, o) <- decodeVarint bs off; Right (UInt8 (fromIntegral w), o) }
decodeValue BT_UINT16 bs off = do { (w, o) <- decodeVarint bs off; Right (UInt16 (fromIntegral w), o) }
decodeValue BT_UINT32 bs off = do { (w, o) <- decodeVarint bs off; Right (UInt32 (fromIntegral w), o) }
decodeValue BT_UINT64 bs off = do { (w, o) <- decodeVarint bs off; Right (UInt64 w, o) }
decodeValue BT_FLOAT  bs off = decodeBondFloat bs off
decodeValue BT_DOUBLE bs off = decodeBondDouble bs off
decodeValue BT_STRING bs off = decodeBondString String bs off
decodeValue BT_WSTRING bs off = decodeBondWString bs off
decodeValue BT_LIST   bs off = decodeList bs off
decodeValue BT_SET    bs off = decodeSet bs off
decodeValue BT_MAP    bs off = decodeMap bs off
decodeValue BT_STRUCT bs off = decodeStruct bs off

decodeBool :: ByteString -> Offset -> Either String (Value, Offset)
decodeBool bs off = do
  b <- peekByte bs off
  Right (Bool (b /= 0), off + 1)

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readLE32BS :: ByteString -> Int -> Word32
readLE32BS bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE32BS #-}

readLE64BS :: ByteString -> Int -> Word64
readLE64BS bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE64BS #-}

decodeBondFloat :: ByteString -> Offset -> Either String (Value, Offset)
decodeBondFloat bs off = do
  checkLen bs off 4
  let !w = readLE32BS bs off
  Right (Float (castWord32ToFloat w), off + 4)

decodeBondDouble :: ByteString -> Offset -> Either String (Value, Offset)
decodeBondDouble bs off = do
  checkLen bs off 8
  let !w = readLE64BS bs off
  Right (Double (castWord64ToDouble w), off + 8)

decodeBondString :: (T.Text -> Value) -> ByteString -> Offset -> Either String (Value, Offset)
decodeBondString ctor bs off = do
  (len, off') <- decodeVarint bs off
  n <- word64ToInt "string" len
  -- Reject lengths that exceed the bytes left in the input before
  -- the BS.take/drop slice runs (DoS prevention).
  if n > BS.length bs - off'
    then Left "Bond.Decode: string length exceeds remaining input"
    else do
      let !raw = BS.take n (BS.drop off' bs)
      case decodeTextFast raw of
        Left e  -> Left $ "Bond.Decode: " ++ e
        Right t -> Right (ctor t, off' + n)

-- | Convert a Bond varint length to a host Int, rejecting values
-- that would silently wrap on 32-bit hosts.
word64ToInt :: String -> Word64 -> Either String Int
word64ToInt what n
  | n <= fromIntegral (maxBound :: Int) = Right (fromIntegral n)
  | otherwise = Left $
      "Bond.Decode: " ++ what ++ " length overflows host Int"

-- | Bond compact-binary @wstring@: varint count of UTF-16 /code
-- units/, followed by @count * 2@ bytes of little-endian UTF-16 code
-- units.  Per the official Bond spec.  Surrogate pairs in the wire
-- bytes are decoded back into supplementary-plane code points.
decodeBondWString :: ByteString -> Offset -> Either String (Value, Offset)
decodeBondWString bs off = do
  (lenW, off') <- decodeVarint bs off
  codeUnits <- word64ToInt "wstring code-unit count" lenW
  -- Reject impossible lengths before computing 'codeUnits * 2', which
  -- would otherwise overflow on hostile input.
  if codeUnits > (BS.length bs - off') `div` 2
    then Left "Bond.Decode: wstring length exceeds remaining input"
    else do
      let !byteLen = codeUnits * 2
      case decodeUtf16LE bs off' codeUnits of
        Left e  -> Left $ "Bond.Decode: " ++ e
        Right t -> Right (WString t, off' + byteLen)

-- | Decode @n@ UTF-16-LE code units starting at @off@ into 'Text'.
decodeUtf16LE
  :: ByteString -> Offset -> Int -> Either String T.Text
decodeUtf16LE bs off0 n = go 0 off0 []
  where
    go !i !o !acc
      | i >= n =
          Right (T.pack (reverse acc))
      | otherwise =
          let !w = fromIntegral (BS.index bs o)
                  .|.
                  (fromIntegral (BS.index bs (o + 1)) `shiftL` 8)
                  :: Int
          in if w >= 0xD800 && w <= 0xDBFF
               then if i + 1 >= n
                      then Left "wstring: truncated surrogate pair"
                      else
                        let !lo = fromIntegral (BS.index bs (o + 2))
                                  .|.
                                  (fromIntegral (BS.index bs (o + 3)) `shiftL` 8)
                                  :: Int
                        in if lo < 0xDC00 || lo > 0xDFFF
                             then Left "wstring: invalid low surrogate"
                             else
                               let !cp = 0x10000
                                       + ((w - 0xD800) `shiftL` 10)
                                       + (lo - 0xDC00)
                               in go (i + 2) (o + 4) (Char.chr cp : acc)
               else if w >= 0xDC00 && w <= 0xDFFF
                 then Left "wstring: orphan low surrogate"
                 else go (i + 1) (o + 2) (Char.chr w : acc)

decodeList :: ByteString -> Offset -> Either String (Value, Offset)
decodeList bs off = do
  (et, count, off') <- decodeContainerHeader bs off
  (vs, off'') <- decodeItems et count bs off'
  Right (List et vs, off'')

decodeSet :: ByteString -> Offset -> Either String (Value, Offset)
decodeSet bs off = do
  (et, count, off') <- decodeContainerHeader bs off
  (vs, off'') <- decodeItems et count bs off'
  Right (Set et vs, off'')

decodeMap :: ByteString -> Offset -> Either String (Value, Offset)
decodeMap bs off = do
  kb <- peekByte bs off
  kt <- typeFromId kb
  (vt, count, off') <- decodeContainerHeader bs (off + 1)
  (kvs, off'') <- decodeMapItems kt vt count bs off'
  Right (Map kt vt kvs, off'')

-- | Decode a struct. Field groups separated by BT_STOP_BASE are base structs.
-- The final group (ending with BT_STOP) contains the struct's own fields.
decodeStruct :: ByteString -> Offset -> Either String (Value, Offset)
decodeStruct bs off = do
  (bases, ownFields, off') <- decodeStructGroups bs off
  Right (Struct bases ownFields, off')

data Terminator = Stop | StopBase

decodeStructGroups :: ByteString -> Offset -> Either String (V.Vector Value, V.Vector (Word16, BondType, Value), Offset)
decodeStructGroups bs off = go V.empty bs off
  where
    go !bases !input !o = do
      (fields, terminator, o') <- readFieldGroup 0 input o
      case terminator of
        StopBase -> do
          let base = Struct V.empty fields
          go (V.snoc bases base) input o'
        Stop ->
          Right (bases, fields, o')

readFieldGroup :: Word16 -> ByteString -> Offset -> Either String (V.Vector (Word16, BondType, Value), Terminator, Offset)
readFieldGroup !prevId bs off = do
  b <- peekByte bs off
  let !tid = b .&. 0x1F
      !delta = fromIntegral (b `shiftR` 5) :: Word16
  if tid == btStop
    then Right (V.empty, Stop, off + 1)
  else if tid == btStopBase
    then Right (V.empty, StopBase, off + 1)
  else do
    bt <- typeFromId tid
    (off', fieldId) <- if delta >= 1 && delta <= 5
      then Right (off + 1, prevId + delta)
      else do
        (fid, o) <- decodeVarint bs (off + 1)
        Right (o, fromIntegral fid)
    (val, off'') <- decodeValue bt bs off'
    (rest, term, off''') <- readFieldGroup fieldId bs off''
    Right (V.cons (fieldId, bt, val) rest, term, off''')

decodeContainerHeader :: ByteString -> Offset -> Either String (BondType, Int, Offset)
decodeContainerHeader bs off = do
  b <- peekByte bs off
  let !tid = b .&. 0x1F
      !countOrFlag = fromIntegral (b `shiftR` 5) :: Int
  bt <- typeFromId tid
  if countOrFlag >= 1 && countOrFlag <= 7
    then Right (bt, countOrFlag, off + 1)
    else do
      (cnt, off') <- decodeVarint bs (off + 1)
      n <- word64ToInt "container count" cnt
      -- Reject impossibly-large counts up-front; each element is at
      -- least one byte, so a count larger than the remaining input
      -- can never represent a valid payload.
      if n > BS.length bs - off'
        then Left "Bond.Decode: container count exceeds remaining input"
        else Right (bt, n, off')

decodeItems :: BondType -> Int -> ByteString -> Offset -> Either String (V.Vector Value, Offset)
decodeItems bt count bs off = go 0 V.empty off
  where
    go !i !acc !o
      | i >= count = Right (acc, o)
      | otherwise = do
          (v, o') <- decodeValue bt bs o
          go (i + 1) (V.snoc acc v) o'

decodeMapItems :: BondType -> BondType -> Int -> ByteString -> Offset -> Either String (V.Vector (Value, Value), Offset)
decodeMapItems kt vt count bs off = go 0 V.empty off
  where
    go !i !acc !o
      | i >= count = Right (acc, o)
      | otherwise = do
          (k, o1) <- decodeValue kt bs o
          (v, o2) <- decodeValue vt bs o1
          go (i + 1) (V.snoc acc (k, v)) o2

-- ============================================================
-- Varint / ZigZag
-- ============================================================

decodeVarint :: ByteString -> Offset -> Either String (Word64, Offset)
decodeVarint bs off = go off 0 0
  where
    go !o !acc !shift
      | shift > 63 = Left "varint too long"
      | o >= BS.length bs = Left "unexpected end of input in varint"
      | otherwise =
          let !b = BS.index bs o
              !val = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
             then Right (val, o + 1)
             else go (o + 1) val (shift + 7)

zigZagDecode :: Word64 -> Int64
zigZagDecode !n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE zigZagDecode #-}

-- ============================================================
-- Helpers
-- ============================================================

peekByte :: ByteString -> Offset -> Either String Word8
peekByte bs off
  | off >= BS.length bs = Left "unexpected end of input"
  | otherwise = Right (BS.index bs off)
{-# INLINE peekByte #-}

checkLen :: ByteString -> Offset -> Int -> Either String ()
checkLen bs off n
  | off + n > BS.length bs = Left "unexpected end of input"
  | otherwise = Right ()
{-# INLINE checkLen #-}

typeFromId :: Word8 -> Either String BondType
typeFromId  2 = Right BT_BOOL
typeFromId  3 = Right BT_INT8
typeFromId  4 = Right BT_INT16
typeFromId  5 = Right BT_INT32
typeFromId  6 = Right BT_INT64
typeFromId  7 = Right BT_UINT8
typeFromId  8 = Right BT_UINT16
typeFromId  9 = Right BT_UINT32
typeFromId 10 = Right BT_UINT64
typeFromId 11 = Right BT_FLOAT
typeFromId 12 = Right BT_DOUBLE
typeFromId 13 = Right BT_STRING
typeFromId 14 = Right BT_WSTRING
typeFromId 15 = Right BT_LIST
typeFromId 16 = Right BT_SET
typeFromId 17 = Right BT_MAP
typeFromId 18 = Right BT_STRUCT
typeFromId n  = Left $ "unknown Bond type id: " ++ show n
