{-# LANGUAGE BangPatterns #-}
-- | Incremental\/streaming decode for CBOR values.
--
-- CBOR values are self-delimiting, so the streaming decoder reads
-- one complete value at a time. When the input is incomplete, it
-- returns 'Partial' requesting more bytes.
-- Uses mutable vectors for definite-length arrays/maps and growing
-- vectors for indefinite-length containers.
module CBOR.Stream
  ( DecodeStep(..)
  , streamDecode
  , feedMore
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import qualified CBOR.Value as C

-- | Result of an incremental decode step.
data DecodeStep a
  = Done !a !ByteString
  | Partial (ByteString -> DecodeStep a)
  | Fail !String

instance Show a => Show (DecodeStep a) where
  show (Done a bs) = "Done " ++ show a ++ " (" ++ show (BS.length bs) ++ " leftover)"
  show (Partial _) = "Partial _"
  show (Fail e)    = "Fail " ++ show e

-- | Begin streaming decode of a single CBOR value.
streamDecode :: ByteString -> DecodeStep C.Value
streamDecode = tryDecode BS.empty

-- | Feed more bytes into a 'Partial' continuation.
feedMore :: DecodeStep a -> ByteString -> DecodeStep a
feedMore (Partial k) bs = k bs
feedMore step _         = step

tryDecode :: ByteString -> ByteString -> DecodeStep C.Value
tryDecode !accum !new =
  let !buf = if BS.null accum then new else accum <> new
  in if BS.null buf
     then Partial $ \more ->
            if BS.null more
            then Fail "CBOR.Stream: unexpected end of input"
            else tryDecode buf more
     else case decodeOneWithLeftover buf of
            Right (val, leftover) -> Done val leftover
            Left _ -> Partial $ \more ->
              if BS.null more
              then Fail "CBOR.Stream: incomplete CBOR value"
              else tryDecode buf more

decodeOneWithLeftover :: ByteString -> Either String (C.Value, ByteString)
decodeOneWithLeftover bs
  | BS.null bs = Left "empty input"
  | otherwise = case decodeOne bs 0 of
      Left e -> Left e
      Right (val, off) -> Right (val, BS.drop off bs)

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BS.index bs off
{-# INLINE rdByte #-}

ensureBytes :: ByteString -> Int -> Int -> Either String ()
ensureBytes !bs !off !n
  | off + n > BS.length bs = Left "unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensureBytes #-}

readArg :: ByteString -> Int -> Word8 -> Either String (Word64, Int)
readArg !bs !off !info
  | info <= 23 = Right (fromIntegral info, off)
  | info == 24 = do
      ensureBytes bs off 1
      Right (fromIntegral (rdByte bs off), off + 1)
  | info == 25 = do
      ensureBytes bs off 2
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
      Right ((b0 `shiftL` 8) .|. b1, off + 2)
  | info == 26 = do
      ensureBytes bs off 4
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
      Right ((b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3, off + 4)
  | info == 27 = do
      ensureBytes bs off 8
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
          !b4 = fromIntegral (rdByte bs (off + 4)) :: Word64
          !b5 = fromIntegral (rdByte bs (off + 5)) :: Word64
          !b6 = fromIntegral (rdByte bs (off + 6)) :: Word64
          !b7 = fromIntegral (rdByte bs (off + 7)) :: Word64
      Right ( (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40)
              .|. (b3 `shiftL` 32) .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16)
              .|. (b6 `shiftL` 8) .|. b7
            , off + 8)
  | info == 31 = Left "indefinite length not supported in stream decoder"
  | otherwise  = Left $ "reserved additional info: " ++ show info

decodeOne :: ByteString -> Int -> Either String (C.Value, Int)
decodeOne !bs !off
  | off >= BS.length bs = Left "unexpected end of input"
  | otherwise =
    let !ib    = rdByte bs off
        !major = ib `shiftR` 5
        !info  = ib .&. 0x1f
    in case major of
      0 -> do (n, off') <- readArg bs (off + 1) info; Right (C.UInt n, off')
      1 -> do (n, off') <- readArg bs (off + 1) info; Right (C.NInt n, off')
      2 -> decodeBStr bs (off + 1) info
      3 -> decodeTStr bs (off + 1) info
      4 -> decodeArray bs (off + 1) info
      5 -> decodeMap bs (off + 1) info
      6 -> decodeTag bs (off + 1) info
      7 -> decodeSimpleOrFloat bs (off + 1) info
      _ -> Left $ "invalid major type: " ++ show major

decodeBStr :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeBStr bs off info
  | info == 31 = decodeIndefiniteBStr bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      ensureBytes bs off' len
      let !chunk = BS.take len (BS.drop off' bs)
      Right (C.ByteString chunk, off' + len)

decodeTStr :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeTStr bs off info
  | info == 31 = decodeIndefiniteTStr bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      ensureBytes bs off' len
      let !raw = BS.take len (BS.drop off' bs)
      case TE.decodeUtf8' raw of
        Left _  -> Left "invalid UTF-8 in text string"
        Right t -> Right (C.TextString t, off' + len)

decodeIndefiniteBStr :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteBStr bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "unexpected end of input in indefinite byte string"
      | rdByte bs off == 0xff = Right (C.ByteString (BS.concat (reverse acc)), off + 1)
      | otherwise = do
          let !ib = rdByte bs off
              !m  = ib `shiftR` 5
              !ai = ib .&. 0x1f
          if m /= 2
            then Left "non-byte-string chunk in indefinite byte string"
            else do
              (len64, off') <- readArg bs (off + 1) ai
              let !len = fromIntegral len64
              ensureBytes bs off' len
              let !chunk = BS.take len (BS.drop off' bs)
              go (off' + len) (chunk : acc)

decodeIndefiniteTStr :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteTStr bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "unexpected end of input in indefinite text string"
      | rdByte bs off == 0xff = do
          let !combined = BS.concat (reverse acc)
          case TE.decodeUtf8' combined of
            Left _  -> Left "invalid UTF-8 in indefinite text string"
            Right t -> Right (C.TextString t, off + 1)
      | otherwise = do
          let !ib = rdByte bs off
              !m  = ib `shiftR` 5
              !ai = ib .&. 0x1f
          if m /= 3
            then Left "non-text-string chunk in indefinite text string"
            else do
              (len64, off') <- readArg bs (off + 1) ai
              let !len = fromIntegral len64
              ensureBytes bs off' len
              let !chunk = BS.take len (BS.drop off' bs)
              go (off' + len) (chunk : acc)

decodeArray :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeArray bs off info
  | info == 31 = decodeIndefiniteArray bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      decodeNItems bs off' len
{-# INLINE decodeArray #-}

decodeNItems :: ByteString -> Int -> Int -> Either String (C.Value, Int)
decodeNItems _bs !off 0 = Right (C.Array V.empty, off)
decodeNItems bs !off !n = runST $ do
  mv <- MV.new n
  go mv 0 off
  where
    go :: MV.MVector s C.Value -> Int -> Int -> ST s (Either String (C.Value, Int))
    go !mv !i !o
      | i >= n = do
          vec <- V.unsafeFreeze mv
          pure $! Right (C.Array vec, o)
      | otherwise = case decodeOne bs o of
          Left e -> pure $! Left e
          Right (v, o') -> do
            MV.unsafeWrite mv i v
            go mv (i + 1) o'
{-# INLINE decodeNItems #-}

decodeIndefiniteArray :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteArray bs off0 = runST $ do
  mv <- MV.new 8
  go mv 0 8 off0
  where
    go :: MV.MVector s C.Value -> Int -> Int -> Int -> ST s (Either String (C.Value, Int))
    go !mv !i !cap !off
      | off >= BS.length bs = pure $! Left "unexpected end of input in indefinite array"
      | rdByte bs off == 0xff = do
          vec <- V.unsafeFreeze (MV.take i mv)
          pure $! Right (C.Array vec, off + 1)
      | otherwise = case decodeOne bs off of
          Left e -> pure $! Left e
          Right (v, off') -> do
            mv' <- if i >= cap
              then MV.grow mv cap
              else pure mv
            let !cap' = if i >= cap then cap * 2 else cap
            MV.unsafeWrite mv' i v
            go mv' (i + 1) cap' off'

decodeMap :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeMap bs off info
  | info == 31 = decodeIndefiniteMap bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      decodeNPairs bs off' len
{-# INLINE decodeMap #-}

decodeNPairs :: ByteString -> Int -> Int -> Either String (C.Value, Int)
decodeNPairs _bs !off 0 = Right (C.Map V.empty, off)
decodeNPairs bs !off !n = runST $ do
  mv <- MV.new n
  go mv 0 off
  where
    go :: MV.MVector s (C.Value, C.Value) -> Int -> Int -> ST s (Either String (C.Value, Int))
    go !mv !i !o
      | i >= n = do
          vec <- V.unsafeFreeze mv
          pure $! Right (C.Map vec, o)
      | otherwise = case decodeOne bs o of
          Left e -> pure $! Left e
          Right (k, o') -> case decodeOne bs o' of
            Left e -> pure $! Left e
            Right (v, o'') -> do
              MV.unsafeWrite mv i (k, v)
              go mv (i + 1) o''
{-# INLINE decodeNPairs #-}

decodeIndefiniteMap :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteMap bs off0 = runST $ do
  mv <- MV.new 8
  go mv 0 8 off0
  where
    go :: MV.MVector s (C.Value, C.Value) -> Int -> Int -> Int -> ST s (Either String (C.Value, Int))
    go !mv !i !cap !off
      | off >= BS.length bs = pure $! Left "unexpected end of input in indefinite map"
      | rdByte bs off == 0xff = do
          vec <- V.unsafeFreeze (MV.take i mv)
          pure $! Right (C.Map vec, off + 1)
      | otherwise = case decodeOne bs off of
          Left e -> pure $! Left e
          Right (k, off') -> case decodeOne bs off' of
            Left e -> pure $! Left e
            Right (v, off'') -> do
              mv' <- if i >= cap
                then MV.grow mv cap
                else pure mv
              let !cap' = if i >= cap then cap * 2 else cap
              MV.unsafeWrite mv' i (k, v)
              go mv' (i + 1) cap' off''

decodeTag :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeTag bs off info = do
  (tagNum, off') <- readArg bs off info
  (content, off'') <- decodeOne bs off'
  Right (C.Tag tagNum content, off'')

decodeSimpleOrFloat :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeSimpleOrFloat bs off info
  | info <= 19 = Right (C.Simple info, off)
  | info == 20 = Right (C.Bool False, off)
  | info == 21 = Right (C.Bool True, off)
  | info == 22 = Right (C.Null, off)
  | info == 23 = Right (C.Undefined, off)
  | info == 24 = do
      ensureBytes bs off 1
      let !sv = rdByte bs off
      Right (C.Simple sv, off + 1)
  | info == 25 = do
      ensureBytes bs off 2
      let !b0 = rdByte bs off
          !b1 = rdByte bs (off + 1)
          !halfBits = (fromIntegral b0 :: Word16) `shiftL` 8
                      .|. fromIntegral b1
          !f = halfToFloat halfBits
      Right (C.Float16 f, off + 2)
  | info == 26 = do
      ensureBytes bs off 4
      let !b0 = fromIntegral (rdByte bs off) :: Word32
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word32
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word32
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word32
          !w  = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
      Right (C.Float32 (castWord32ToFloat w), off + 4)
  | info == 27 = do
      ensureBytes bs off 8
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
          !b4 = fromIntegral (rdByte bs (off + 4)) :: Word64
          !b5 = fromIntegral (rdByte bs (off + 5)) :: Word64
          !b6 = fromIntegral (rdByte bs (off + 6)) :: Word64
          !b7 = fromIntegral (rdByte bs (off + 7)) :: Word64
          !w  = (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40)
                .|. (b3 `shiftL` 32) .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16)
                .|. (b6 `shiftL` 8) .|. b7
      Right (C.Float64 (castWord64ToDouble w), off + 8)
  | info == 31 = Left "break at top level is not valid"
  | otherwise  = Left $ "reserved simple value info: " ++ show info

-- See 'CBOR.Decode.halfToFloat' for the rationale; this is the same
-- conversion duplicated for the streaming decoder so that both
-- entry points produce the same Float for the same 2-byte input.
halfToFloat :: Word16 -> Float
halfToFloat !h =
  let !w   = fromIntegral h :: Word32
      !sign = w `shiftR` 15
      !expo = (w `shiftR` 10) .&. 0x1f
      !mant = w .&. 0x03ff
      !signBit = sign `shiftL` 31
  in if expo == 0
     then if mant == 0
          then castWord32ToFloat signBit
          else let !f = (fromIntegral mant :: Float) / 1024.0 * (2 ** (-14))
               in if sign /= 0 then negate f else f
     else if expo == 0x1f
     then castWord32ToFloat
            (signBit .|. 0x7f800000 .|. (mant `shiftL` 13))
     else castWord32ToFloat
            (signBit .|. ((expo + 112) `shiftL` 23) .|. (mant `shiftL` 13))
