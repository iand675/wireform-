{-# LANGUAGE BangPatterns #-}
-- | Amazon Ion binary decoding.
--
-- Decodes an Amazon Ion binary 'ByteString' into an 'Ion.Value.Value'.
-- Validates the Binary Version Marker, then parses the (optional) local
-- symbol table that follows so that subsequent @Symbol@ values and
-- struct field-name SIDs can be resolved back to their textual form.
module Ion.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word8, Word64, byteSwap64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Ion.Value as I
import Ion.SymbolTable (SymbolTable)
import qualified Ion.SymbolTable as ST
import Wireform.FFI (decodeTextFast)

------------------------------------------------------------------------
-- Top level
------------------------------------------------------------------------

decode :: ByteString -> Either String I.Value
decode !bs
  | BS.length bs < 4 = Left "Ion.Decode: input too short"
  | BSU.unsafeIndex bs 0 /= 0xE0
    || BSU.unsafeIndex bs 1 /= 0x01
    || BSU.unsafeIndex bs 2 /= 0x00
    || BSU.unsafeIndex bs 3 /= 0xEA = Left "Ion.Decode: invalid BVM"
  | BS.length bs == 4 = Left "Ion.Decode: no data after BVM"
  | otherwise = do
      (st, off1) <- consumeSymbolTables bs 4 ST.empty
      case decodeValue st bs off1 of
        Left err -> Left err
        Right (val, off2)
          | off2 == BS.length bs -> Right val
          | otherwise -> Left $ "Ion.Decode: " ++ show (BS.length bs - off2) ++ " trailing bytes"

------------------------------------------------------------------------
-- Symbol table: consume any leading $ion_symbol_table annotation envelopes
-- and merge them into the user symbol table.
------------------------------------------------------------------------

systemSidIonSymbolTable :: Int
systemSidIonSymbolTable = 3

systemSidSymbols :: Int
systemSidSymbols = 7

consumeSymbolTables :: ByteString -> Int -> SymbolTable -> Either String (SymbolTable, Int)
consumeSymbolTables !bs !off !st
  | off >= BS.length bs = Right (st, off)
  | otherwise =
      let !td = rdByte bs off
          !typeNibble = td `shiftR` 4
      in if typeNibble /= 0x0E
           then Right (st, off)
           else case peekAnnotationSid bs off of
                  Just (sid, _) | sid == systemSidIonSymbolTable -> do
                    (st', off') <- parseSymbolTable bs off st
                    consumeSymbolTables bs off' st'
                  _ -> Right (st, off)

-- | Look at an annotation envelope's first annotation SID without
-- consuming the value.  Returns @(sid, end-of-envelope)@.
peekAnnotationSid :: ByteString -> Int -> Maybe (Int, Int)
peekAnnotationSid !bs !off = case ensureBytes bs off 1 of
  Left _ -> Nothing
  Right () ->
    let !td = rdByte bs off
        !typeNibble = td `shiftR` 4
        !lenNibble  = td .&. 0x0F
    in if typeNibble /= 0x0E then Nothing
       else case readLength bs (off + 1) lenNibble of
         Left _ -> Nothing
         Right (totalLen, off1) -> case readVarUInt bs off1 of
           Left _ -> Nothing
           Right (_annsBytes, off2) -> case readVarUInt bs off2 of
             Left _ -> Nothing
             Right (sid, _off3) -> Just (sid, off1 + totalLen)

parseSymbolTable :: ByteString -> Int -> SymbolTable -> Either String (SymbolTable, Int)
parseSymbolTable !bs !off !st = do
  ensure bs off 1
  let !td = rdByte bs off
      !lenNibble  = td .&. 0x0F
  (totalLen, off1) <- readLength bs (off + 1) lenNibble
  let !envEnd = off1 + totalLen
  (annsBytes, off2) <- readVarUInt bs off1
  let !annsEnd = off2 + annsBytes
  -- Skip the annotation SIDs (we only require the first one to be
  -- $ion_symbol_table; any additional annotations are ignored).
  let !innerStart = annsEnd
  (innerVal, _innerEnd) <- decodeValueRaw bs innerStart
  st' <- case innerVal of
    RStruct kvs -> mergeSymbols kvs st
    _ -> Right st
  Right (st', envEnd)

-- | Add the @symbols: [...]@ list of a parsed LST struct to the table.
mergeSymbols :: [(Int, RawValue)] -> SymbolTable -> Either String SymbolTable
mergeSymbols kvs st0 = go st0 kvs
  where
    go !st [] = Right st
    go !st ((sid, v) : rest)
      | sid == systemSidSymbols = case v of
          RList items ->
            let !st' = foldl addStr st items
            in go st' rest
          _ -> go st rest
      | otherwise = go st rest

    addStr !st (RString t) = snd (ST.internSymbol t st)
    addStr !st _           = st

------------------------------------------------------------------------
-- Raw value parser (used only inside symbol tables)
------------------------------------------------------------------------

-- | A trimmed-down value representation that exposes the wire-level
-- shape of a struct (field-name SIDs preserved) and lists. We use it
-- only to parse local symbol tables, where the structure is fixed and
-- we don't yet have a populated symbol table to resolve text symbols.
data RawValue
  = RNull
  | RBool !Bool
  | RInt !Int64
  | RFloat !Double
  | RString !T.Text
  | RBlob !ByteString
  | RClob !ByteString
  | RList ![RawValue]
  | RStruct ![(Int, RawValue)]
  | RSymbol !Int
  | RAnnotation !Int !RawValue
  deriving stock (Show, Eq)

decodeValueRaw :: ByteString -> Int -> Either String (RawValue, Int)
decodeValueRaw bs off = do
  ensure bs off 1
  let !td = rdByte bs off
      !typeNibble = td `shiftR` 4
      !lenNibble  = td .&. 0x0F
  case typeNibble of
    0x00 -> Right (RNull, off + 1)
    0x01 -> Right (RBool (lenNibble /= 0), off + 1)
    0x02 -> rDecInt bs (off + 1) lenNibble False
    0x03 -> rDecInt bs (off + 1) lenNibble True
    0x04 -> rDecFloat bs (off + 1) lenNibble
    0x07 -> rDecSymbol bs (off + 1) lenNibble
    0x08 -> rDecString bs (off + 1) lenNibble
    0x09 -> rDecClob bs (off + 1) lenNibble
    0x0A -> rDecBlob bs (off + 1) lenNibble
    0x0B -> rDecList bs (off + 1) lenNibble
    0x0D -> rDecStruct bs (off + 1) lenNibble
    0x0E -> rDecAnnotation bs (off + 1) lenNibble
    _    -> Left $ "Ion.Decode (raw): unsupported type nibble: " ++ show typeNibble

rDecInt :: ByteString -> Int -> Word8 -> Bool -> Either String (RawValue, Int)
rDecInt bs off lenNibble isNeg
  | lenNibble == 0 = Right (RInt 0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      (mag, off2) <- readMagnitude bs off1 len
      let !val = if isNeg then negate (fromIntegral mag) :: Int64 else fromIntegral mag
      Right (RInt val, off2)

rDecFloat :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecFloat bs off lenNibble
  | lenNibble == 0 = Right (RFloat 0.0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      case len of
        8 -> do
          ensure bs off1 8
          let !w = readBE64 bs off1
          Right (RFloat (castWord64ToDouble w), off1 + 8)
        _ -> Left $ "Ion.Decode (raw): unsupported float size: " ++ show len

rDecString :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecString bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  case decodeTextFast raw of
    Left _  -> Left "Ion.Decode: invalid UTF-8 in string"
    Right t -> Right (RString t, off1 + len)

rDecBlob :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecBlob bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  Right (RBlob raw, off1 + len)

rDecClob :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecClob bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  Right (RClob raw, off1 + len)

rDecSymbol :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecSymbol bs off lenNibble
  | lenNibble == 0 = Right (RSymbol 0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      (mag, off2) <- readMagnitude bs off1 len
      Right (RSymbol (fromIntegral mag), off2)

rDecList :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecList bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  items <- collectRawItems bs off1 end
  Right (RList items, end)

collectRawItems :: ByteString -> Int -> Int -> Either String [RawValue]
collectRawItems bs off end
  | off >= end = Right []
  | otherwise = do
      (v, off1) <- decodeValueRaw bs off
      rest <- collectRawItems bs off1 end
      Right (v : rest)

rDecStruct :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecStruct bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  fields <- collectRawFields bs off1 end
  Right (RStruct fields, end)

collectRawFields :: ByteString -> Int -> Int -> Either String [(Int, RawValue)]
collectRawFields bs off end
  | off >= end = Right []
  | otherwise = do
      (sid, off1) <- readVarUInt bs off
      (v, off2) <- decodeValueRaw bs off1
      rest <- collectRawFields bs off2 end
      Right ((sid, v) : rest)

rDecAnnotation :: ByteString -> Int -> Word8 -> Either String (RawValue, Int)
rDecAnnotation bs off lenNibble = do
  (totalLen, off1) <- readLength bs off lenNibble
  let !end = off1 + totalLen
  (_annBytes, off2) <- readVarUInt bs off1
  (sid, off3) <- readVarUInt bs off2
  (inner, _off4) <- decodeValueRaw bs off3
  Right (RAnnotation sid inner, end)

------------------------------------------------------------------------
-- Symbol-aware value parser
------------------------------------------------------------------------

decodeValue :: SymbolTable -> ByteString -> Int -> Either String (I.Value, Int)
decodeValue !st bs off = do
  ensure bs off 1
  let !td = rdByte bs off
      !typeNibble = td `shiftR` 4
      !lenNibble  = td .&. 0x0F
  case typeNibble of
    0x00 -> Right (I.Null, off + 1)
    0x01 -> Right (I.Bool (lenNibble /= 0), off + 1)
    0x02 -> decodeInt bs (off + 1) lenNibble False
    0x03 -> decodeInt bs (off + 1) lenNibble True
    0x04 -> decodeIonFloat bs (off + 1) lenNibble
    0x07 -> decodeSymbol st bs (off + 1) lenNibble
    0x08 -> decodeString bs (off + 1) lenNibble
    0x09 -> decodeClob bs (off + 1) lenNibble
    0x0A -> decodeBlob bs (off + 1) lenNibble
    0x0B -> decodeList st bs (off + 1) lenNibble
    0x0D -> decodeStruct st bs (off + 1) lenNibble
    0x0E -> decodeAnnotation st bs (off + 1) lenNibble
    _    -> Left $ "Ion.Decode: unsupported type nibble: " ++ show typeNibble

decodeInt :: ByteString -> Int -> Word8 -> Bool -> Either String (I.Value, Int)
decodeInt bs off lenNibble isNeg
  | lenNibble == 0 = Right (I.Int 0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      (mag, off2) <- readMagnitude bs off1 len
      let !val = if isNeg then negate (fromIntegral mag) :: Int64 else fromIntegral mag
      Right (I.Int val, off2)

decodeIonFloat :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeIonFloat bs off lenNibble
  | lenNibble == 0 = Right (I.Float 0.0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      case len of
        8 -> do
          ensure bs off1 8
          let !w = readBE64 bs off1
          Right (I.Float (castWord64ToDouble w), off1 + 8)
        _ -> Left $ "Ion.Decode: unsupported float size: " ++ show len

decodeString :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeString bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  case decodeTextFast raw of
    Left _  -> Left "Ion.Decode: invalid UTF-8 in string"
    Right t -> Right (I.String t, off1 + len)

decodeSymbol :: SymbolTable -> ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeSymbol st bs off lenNibble
  | lenNibble == 0 = Right (I.Symbol T.empty, off)  -- $0
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      (mag, off2) <- readMagnitude bs off1 len
      let !sid = fromIntegral mag :: Int
      case ST.lookupText sid st of
        Just t  -> Right (I.Symbol t, off2)
        Nothing -> Left $ "Ion.Decode: undefined symbol SID: " ++ show sid

decodeBlob :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeBlob bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  Right (I.Blob raw, off1 + len)

decodeClob :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeClob bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  Right (I.Clob raw, off1 + len)

decodeList :: SymbolTable -> ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeList st bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  (items, _) <- readItems st bs off1 end
  Right (I.List (V.fromList items), end)

readItems :: SymbolTable -> ByteString -> Int -> Int -> Either String ([I.Value], Int)
readItems st bs off end
  | off >= end = Right ([], off)
  | otherwise = do
      (v, off1) <- decodeValue st bs off
      (rest, off2) <- readItems st bs off1 end
      Right (v : rest, off2)

decodeStruct :: SymbolTable -> ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeStruct st bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  (fields, _) <- readFields st bs off1 end
  Right (I.Struct (V.fromList fields), end)

readFields :: SymbolTable -> ByteString -> Int -> Int -> Either String ([(T.Text, I.Value)], Int)
readFields st bs off end
  | off >= end = Right ([], off)
  | otherwise = do
      (sid, off1) <- readVarUInt bs off
      k <- case ST.lookupText sid st of
        Just t  -> Right t
        Nothing -> Left $ "Ion.Decode: undefined struct field SID: " ++ show sid
      (v, off2) <- decodeValue st bs off1
      (rest, off3) <- readFields st bs off2 end
      Right ((k, v) : rest, off3)

decodeAnnotation :: SymbolTable -> ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeAnnotation st bs off lenNibble = do
  (totalLen, off1) <- readLength bs off lenNibble
  let !end = off1 + totalLen
  (_annsBytes, off2) <- readVarUInt bs off1
  (sid, off3) <- readVarUInt bs off2
  ann <- case ST.lookupText sid st of
    Just t  -> Right t
    Nothing -> Left $ "Ion.Decode: undefined annotation SID: " ++ show sid
  (inner, _off4) <- decodeValue st bs off3
  Right (I.Annotation ann inner, end)

------------------------------------------------------------------------
-- Wire helpers
------------------------------------------------------------------------

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "Ion.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

ensureBytes :: ByteString -> Int -> Int -> Either String ()
ensureBytes = ensure
{-# INLINE ensureBytes #-}

readVarUInt :: ByteString -> Int -> Either String (Int, Int)
readVarUInt bs off = go off 0
  where
    go !i !acc
      | i >= BS.length bs = Left "Ion.Decode: truncated VarUInt"
      | otherwise =
          let !b = rdByte bs i
          in if b >= 0x80
             then Right (acc `shiftL` 7 .|. fromIntegral (b .&. 0x7F), i + 1)
             else go (i + 1) (acc `shiftL` 7 .|. fromIntegral b)

readMagnitude :: ByteString -> Int -> Int -> Either String (Word64, Int)
readMagnitude bs off nbytes
  | nbytes == 0 = Right (0, off)
  | otherwise = do
      ensure bs off nbytes
      let go !i !acc
            | i >= nbytes = Right (acc, off + nbytes)
            | otherwise =
                let !b = fromIntegral (rdByte bs (off + i)) :: Word64
                in go (i + 1) ((acc `shiftL` 8) .|. b)
      go 0 0

readLength :: ByteString -> Int -> Word8 -> Either String (Int, Int)
readLength bs off lenNibble
  | lenNibble < 0x0E = Right (fromIntegral lenNibble, off)
  | lenNibble == 0x0E = readVarUInt bs off
  | otherwise = Left "Ion.Decode: reserved length nibble 0x0F"

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readBE64 :: ByteString -> Int -> Word64
readBE64 bs off = withBSPtrOff bs off $ \p ->
  byteSwap64 <$> (peekByteOff p 0 :: IO Word64)
{-# INLINE readBE64 #-}
