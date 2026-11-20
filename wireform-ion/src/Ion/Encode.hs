{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Amazon Ion binary encoding.

Encodes an 'Ion.Value.Value' to Amazon Ion binary format. The output
begins with the Ion Binary Version Marker (BVM: @0xE0 0x01 0x00 0xEA@)
followed by the encoded value. Uses 'Wireform.Encode.Direct.directEncode'
for direct buffer writes with pre-computed sizes.
-}
module Ion.Encode (
  encode,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Text qualified as T
import Data.Text.Foreign qualified as TF
import Data.Vector qualified as V
import Data.Word (Word64, Word8, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (Int (I#), Int#)
import GHC.Float (castDoubleToWord64)
import Ion.Internal.SymbolPlan qualified as Plan
import Ion.Internal.UTF8 qualified as UTF8
import Ion.SymbolTable qualified as ST
import Ion.Value qualified as I
import Wireform.Encode.Direct (directEncode)


{- | Emit an Ion 1.0 datagram containing one user value. Field names,
annotations and symbol values use SIDs; local text is declared once in
a preceding local symbol table. Ordinary strings are never interned.
-}
encode :: I.Value -> ByteString
encode !val = case Plan.build ST.emptyEncoder val of
  (# plan, initial #) -> encodePlanned plan initial val
{-# NOINLINE encode #-}


encodePlanned :: Plan.Plan -> ST.EncoderSymbols -> I.Value -> ByteString
encodePlanned plan initial !val = case prepare plan initial val of
  (# valSize#, symbols #) ->
    let !valSize = I# valSize#
        !locals = ST.encoderSymbols symbols
        table =
          I.Annotation
            "$ion_symbol_table"
            (I.Struct (V.singleton ("symbols", I.List (V.map I.String locals))))
        !tableSize = if V.null locals then 0 else valueSize symbols table
        !sz = 4 + tableSize + valSize
    in directEncode sz $ \p off -> do
         valueStart <- writeBack plan symbols p sz val
         tableStart <- if V.null locals then pure valueStart else writeBack Plan.Plain symbols p valueStart table
         _ <- writeBVM p off
         if tableStart == off + 4 then pure sz else error "Ion.Encode: size invariant"
{-# NOINLINE encodePlanned #-}


-- One traversal both interns symbols and computes exact sizes. A strict
-- unboxed result avoids allocating a state/size record for every node.
prepare :: Plan.Plan -> ST.EncoderSymbols -> I.Value -> (# Int#, ST.EncoderSymbols #)
prepare plan symbols = \case
  I.Symbol text -> case Plan.intern plan text symbols of
    (# sid, next #) ->
      let !len = magnitudeBytes (fromIntegral sid)
      in sized (tdSize 7 len + len) next
  I.List vs -> items 0 0 symbols
    where
      !child = Plan.listItem plan
      items !i !size !st
        | i >= V.length vs = sized (tdSize 11 size + size) st
        | otherwise = case prepare child st (V.unsafeIndex vs i) of
            (# size'#, next #) -> items (i + 1) (size + I# size'#) next
  I.Struct fields -> pairs 0 0 symbols
    where
      pairs !i !size !st
        | i >= V.length fields = sized (tdSize 13 size + size) st
        | otherwise = case V.unsafeIndex fields i of
            (key, v) ->
              let !hint = Plan.fieldAt plan i
                  !child = Plan.fieldItem hint
              in case Plan.internField hint key st of
                   (# sid, st' #) -> case prepare child st' v of
                     (# size'#, next #) -> pairs (i + 1) (size + varUIntSize sid + I# size'#) next
  annotated@I.Annotation {} -> case annotations plan symbols 0 annotated of
    (# idsSize, innerSize#, next #) ->
      let !len = varUIntSize idsSize + idsSize + I# innerSize#
      in sized (tdSize 14 len + len) next
    where
      annotations hint !st !ids (I.Annotation ann v) = case Plan.intern hint ann st of
        (# sid, next #) ->
          let !child = Plan.annotationItem hint
          in annotations child next (ids + varUIntSize sid) v
      annotations hint !st !ids v = case prepare hint st v of
        (# size#, next #) -> (# ids, size#, next #)
  scalar -> sized (valueSize symbols scalar) symbols


sized :: Int -> ST.EncoderSymbols -> (# Int#, ST.EncoderSymbols #)
sized (I# size#) symbols = (# size#, symbols #)
{-# INLINE sized #-}


writeBVM :: Ptr Word8 -> Int -> IO Int
writeBVM p off = do
  pokeByteOff p off (0xE0 :: Word8)
  pokeByteOff p (off + 1) (0x01 :: Word8)
  pokeByteOff p (off + 2) (0x00 :: Word8)
  pokeByteOff p (off + 3) (0xEA :: Word8)
  pure $! off + 4


valueSize :: ST.EncoderSymbols -> I.Value -> Int
valueSize symbols = \case
  I.Null -> 1
  I.Bool _ -> 1
  I.Int n
    | n == 0 -> 1
    | n > 0 -> 1 + magnitudeBytes (fromIntegral n)
    | otherwise -> 1 + magnitudeBytes (fromIntegral (negate n))
  I.Float d
    | d == 0 && not (isNegativeZero d) -> 1
    | otherwise -> 1 + 8
  I.String t ->
    let !len = TF.lengthWord8 t
    in tdSize 8 len + len
  I.Blob bs ->
    let !len = BS.length bs
    in tdSize 10 len + len
  I.Clob bs ->
    let !len = BS.length bs
    in tdSize 9 len + len
  I.List vs ->
    let !payloadSz = V.foldl' (\acc v -> acc + valueSize symbols v) 0 vs
    in tdSize 11 payloadSz + payloadSz
  I.Struct fields ->
    let !payloadSz = V.foldl' (\acc (k, v) -> acc + fieldSize symbols k v) 0 fields
    in tdSize 13 payloadSz + payloadSz
  I.Symbol t ->
    let !len = magnitudeBytes (fromIntegral (ST.symbolID symbols t))
    in tdSize 7 len + len
  annotated@I.Annotation {} ->
    let !idsSize = annotationIDsSize symbols annotated
        !innerSize = valueSize symbols (annotationValue annotated)
        !wrapLen = varUIntSize idsSize + idsSize + innerSize
    in tdSize 14 wrapLen + wrapLen
{-# INLINE valueSize #-}


fieldSize :: ST.EncoderSymbols -> T.Text -> I.Value -> Int
fieldSize symbols k v = varUIntSize (ST.symbolID symbols k) + valueSize symbols v


-- Nested constructors express ordered annotations, encoded as one wrapper.
annotationIDsSize :: ST.EncoderSymbols -> I.Value -> Int
annotationIDsSize symbols = go 0
  where
    go !size (I.Annotation ann v) = go (size + varUIntSize (ST.symbolID symbols ann)) v
    go !size _ = size
{-# INLINE annotationIDsSize #-}


annotationValue :: I.Value -> I.Value
annotationValue (I.Annotation _ v) = annotationValue v
annotationValue v = v
{-# INLINE annotationValue #-}


tdSize :: Int -> Int -> Int
tdSize _typeCode len
  | len < 14 = 1
  | otherwise = 1 + varUIntSize len
{-# INLINE tdSize #-}


varUIntSize :: Int -> Int
varUIntSize = go 1
  where
    go !size !n
      | n <= 0x7F = size
      | otherwise = go (size + 1) (n `shiftR` 7)
{-# INLINE varUIntSize #-}


magnitudeBytes :: Word64 -> Int
magnitudeBytes n
  | n <= 0xFF = 1
  | n <= 0xFFFF = 2
  | n <= 0xFFFFFF = 3
  | n <= 0xFFFFFFFF = 4
  | n <= 0xFFFFFFFFFF = 5
  | n <= 0xFFFFFFFFFFFF = 6
  | n <= 0xFFFFFFFFFFFFFF = 7
  | otherwise = 8
{-# INLINE magnitudeBytes #-}


-- Write a VarUInt immediately before end. Sizing it separately would
-- repeat the magnitude walk; the reverse writer already knows the end.
writeVarUIntBack :: Ptr Word8 -> Int -> Int -> IO Int
writeVarUIntBack p end n = do
  pokeByteOff p (end - 1) (fromIntegral (n .&. 0x7f .|. 0x80) :: Word8)
  go (end - 1) (n `shiftR` 7)
  where
    go !start !remaining
      | remaining == 0 = pure start
      | otherwise = do
          pokeByteOff p (start - 1) (fromIntegral (remaining .&. 0x7f) :: Word8)
          go (start - 1) (remaining `shiftR` 7)
{-# INLINE writeVarUIntBack #-}


writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}


writeMagnitude :: Ptr Word8 -> Int -> Word64 -> Int -> IO Int
writeMagnitude p off mag nbytes = go off 0
  where
    go !o !i
      | i >= nbytes = pure o
      | otherwise = do
          let !byteIdx = nbytes - 1 - i
              !b = fromIntegral ((mag `shiftR` (byteIdx * 8)) .&. 0xFF) :: Word8
          pokeByteOff p o b
          go (o + 1) (i + 1)


{- | Backward filling avoids a size traversal at every container. The
initial size pass and this write pass each visit every value once.
-}
writeBack :: Plan.Plan -> ST.EncoderSymbols -> Ptr Word8 -> Int -> I.Value -> IO Int
writeBack plan symbols p end = \case
  I.List vs -> do
    start <- items (V.length vs - 1) end
    finish 11 start
    where
      !child = Plan.listItem plan
      items !i !position
        | i < 0 = pure position
        | otherwise = writeBack child symbols p position (V.unsafeIndex vs i) >>= items (i - 1)
  I.Struct fields -> do
    start <- pairs (V.length fields - 1) end
    finish 13 start
    where
      pairs !i !position
        | i < 0 = pure position
        | otherwise = do
            let (key, v) = V.unsafeIndex fields i
                !hint = Plan.fieldAt plan i
                !sid = Plan.fieldID hint key symbols
                !child = Plan.fieldItem hint
            valueStart <- writeBack child symbols p position v
            start <- writeVarUIntBack p valueStart sid
            pairs (i - 1) start
  annotated@I.Annotation {} -> do
    let !child = innerPlan plan annotated
    innerStart <- writeBack child symbols p end (annotationValue annotated)
    idsStart <- writeAnnotationsBack plan symbols p innerStart annotated
    start <- writeVarUIntBack p idsStart (innerStart - idsStart)
    finish 14 start
  I.Null -> byte 0x0f
  I.Bool b -> byte (if b then 0x11 else 0x10)
  I.Int 0 -> byte 0x20
  I.Int n -> do
    let !mag = if n < 0 then fromIntegral (negate n) else fromIntegral n
        !len = magnitudeBytes mag
        !start = end - len
    _ <- writeMagnitude p start mag len
    finish (if n < 0 then 3 else 2) start
  I.Float d
    | d == 0 && not (isNegativeZero d) -> byte 0x40
    | otherwise -> do
        _ <- writeBE64 p (end - 8) (castDoubleToWord64 d)
        finish 4 (end - 8)
  I.String text -> do
    let !start = end - TF.lengthWord8 text
    UTF8.copyText text (p `plusPtr` start)
    finish 8 start
  I.Blob bytes -> do
    let !start = end - BS.length bytes
    _ <- writeRawBytes p start bytes
    finish 10 start
  I.Clob bytes -> do
    let !start = end - BS.length bytes
    _ <- writeRawBytes p start bytes
    finish 9 start
  I.Symbol text -> do
    let !sid = fromIntegral (Plan.lookupID plan text symbols)
        !len = magnitudeBytes sid
        !start = end - len
    _ <- writeMagnitude p start sid len
    finish 7 start
  where
    byte !b = pokeByteOff p (end - 1) (b :: Word8) >> pure (end - 1)
    finish !code !start
      | len < 14 = do
          pokeByteOff p (start - 1) ((code `shiftL` 4) .|. fromIntegral len :: Word8)
          pure (start - 1)
      | otherwise = do
          lengthStart <- writeVarUIntBack p start len
          pokeByteOff p (lengthStart - 1) ((code `shiftL` 4) .|. 0x0e :: Word8)
          pure (lengthStart - 1)
      where
        !len = end - start


-- The AST stores annotations as a chain. Walk to the last annotation,
-- then fill their VarUInt IDs backwards, preserving their original order.
writeAnnotationsBack :: Plan.Plan -> ST.EncoderSymbols -> Ptr Word8 -> Int -> I.Value -> IO Int
writeAnnotationsBack plan symbols p end (I.Annotation ann v) = do
  let !child = Plan.annotationItem plan
  after <- writeAnnotationsBack child symbols p end v
  writeVarUIntBack p after (Plan.lookupID plan ann symbols)
writeAnnotationsBack _ _ _ end _ = pure end


writeBE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeBE64 p off w = do
  pokeByteOff p off (byteSwap64 w)
  pure $! off + 8
{-# INLINE writeBE64 #-}


innerPlan :: Plan.Plan -> I.Value -> Plan.Plan
innerPlan plan (I.Annotation _ value) = innerPlan (Plan.annotationItem plan) value
innerPlan plan _ = plan
