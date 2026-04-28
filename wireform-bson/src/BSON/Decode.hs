{-# LANGUAGE BangPatterns #-}
-- | BSON binary decoding.
--
-- Decodes a BSON wire-format 'ByteString' into a 'BSON.Value.Value'.
-- Uses unsafe indexing for performance on validated input. Supports
-- all standard BSON element types.
-- Uses growing mutable vectors for document element accumulation.
module BSON.Decode
  ( decode
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Float (castWord64ToDouble)

import qualified BSON.Value as B
import Wireform.FFI (findNulBS, decodeTextFast)

decode :: ByteString -> Either String B.Value
decode !bs
  | BS.length bs < 5 = Left "BSON.Decode: input too short"
  | otherwise = case decodeDocument bs 0 of
      Left err -> Left err
      Right (val, _) -> Right val

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word32
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word32
      !b2 = fromIntegral (rdByte bs (off + 2)) :: Word32
      !b3 = fromIntegral (rdByte bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
{-# INLINE readLE32 #-}

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word64
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
      !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
      !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
      !b4 = fromIntegral (rdByte bs (off + 4)) :: Word64
      !b5 = fromIntegral (rdByte bs (off + 5)) :: Word64
      !b6 = fromIntegral (rdByte bs (off + 6)) :: Word64
      !b7 = fromIntegral (rdByte bs (off + 7)) :: Word64
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
       .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
{-# INLINE readLE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off < 0 || n < 0 = Left "BSON.Decode: negative offset/length"
  | n > BS.length bs - off =
      Left "BSON.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

readCString :: ByteString -> Int -> Either String (Text, Int)
readCString bs off =
  case findNulBS bs off of
    Nothing -> Left "BSON.Decode: unterminated cstring"
    Just !i ->
      let !raw = BSU.unsafeTake (i - off) (BSU.unsafeDrop off bs)
      in case decodeTextFast raw of
           Left _  -> Left "BSON.Decode: invalid UTF-8 in cstring"
           Right t -> Right (t, i + 1)

readBSONString :: ByteString -> Int -> Either String (Text, Int)
readBSONString bs off = do
  ensure bs off 4
  let !len = fromIntegral (readLE32 bs off) :: Int
  ensure bs (off + 4) len
  let !raw = BSU.unsafeTake (len - 1) (BSU.unsafeDrop (off + 4) bs)
  case decodeTextFast raw of
    Left _  -> Left "BSON.Decode: invalid UTF-8 in string"
    Right t -> Right (t, off + 4 + len)

decodeDocument :: ByteString -> Int -> Either String (B.Value, Int)
decodeDocument bs off = do
  ensure bs off 4
  let !docSz = fromIntegral (readLE32 bs off) :: Int
      !docEnd = off + docSz
  ensure bs off docSz
  if rdByte bs (docEnd - 1) /= 0x00
    then Left "BSON.Decode: document missing terminator"
    else do
      (vec, _) <- readElements bs (off + 4) (docEnd - 1)
      Right (B.Document vec, docEnd)

readElements :: ByteString -> Int -> Int -> Either String (V.Vector (Text, B.Value), Int)
readElements bs off end = runST $ do
  mv <- MV.new 8
  go mv 0 8 off
  where
    go :: MV.MVector s (Text, B.Value) -> Int -> Int -> Int -> ST s (Either String (V.Vector (Text, B.Value), Int))
    go !mv !i !cap !o
      | o >= end = do
          vec <- V.unsafeFreeze (MV.take i mv)
          pure $! Right (vec, o)
      | otherwise = case readOneElement bs o of
          Left e -> pure $! Left e
          Right (kv, o') -> do
            mv' <- if i >= cap
              then MV.grow mv cap
              else pure mv
            let !cap' = if i >= cap then cap * 2 else cap
            MV.unsafeWrite mv' i kv
            go mv' (i + 1) cap' o'
{-# INLINE readElements #-}

readOneElement :: ByteString -> Int -> Either String ((Text, B.Value), Int)
readOneElement bs off = do
  ensure bs off 1
  let !tag = rdByte bs off
  (key, off1) <- readCString bs (off + 1)
  (val, off2) <- readValue bs off1 tag
  Right ((key, val), off2)
{-# INLINE readOneElement #-}

readValue :: ByteString -> Int -> Word8 -> Either String (B.Value, Int)
readValue bs off tag = case tag of
  0x01 -> do
    ensure bs off 8
    let !w = readLE64 bs off
    Right (B.Double (castWord64ToDouble w), off + 8)
  0x02 -> do
    (t, off1) <- readBSONString bs off
    Right (B.String t, off1)
  0x03 -> decodeDocument bs off
  0x04 -> do
    result <- decodeDocument bs off
    case result of
      (B.Document fields, off1) -> Right (B.Array (V.map snd fields), off1)
      (_, _off1) -> Left "BSON.Decode: expected document for array"
  0x05 -> do
    ensure bs off 5
    let !len = fromIntegral (readLE32 bs off) :: Int
        !sub = rdByte bs (off + 4)
        !off1 = off + 4 + 1
    ensure bs off1 len
    let !dat = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
    Right (B.Binary sub dat, off1 + len)
  0x06 -> Right (B.Undefined, off)
  0x07 -> do
    ensure bs off 12
    let !dat = BSU.unsafeTake 12 (BSU.unsafeDrop off bs)
    Right (B.ObjectId dat, off + 12)
  0x08 -> do
    ensure bs off 1
    let !b = rdByte bs off /= 0x00
    Right (B.Bool b, off + 1)
  0x09 -> do
    ensure bs off 8
    let !ms = fromIntegral (readLE64 bs off) :: Int64
    Right (B.DateTime ms, off + 8)
  0x0A -> Right (B.Null, off)
  0x0B -> do
    (pat, off1) <- readCString bs off
    (opts, off2) <- readCString bs off1
    Right (B.Regex pat opts, off2)
  0x0D -> do
    (code, off1) <- readBSONString bs off
    Right (B.JavaScript code, off1)
  0x0C -> do
    -- Deprecated DBPointer: UTF-8 namespace followed by a 12-byte ObjectId.
    (ns, off1) <- readBSONString bs off
    ensure bs off1 12
    let !oid = BSU.unsafeTake 12 (BSU.unsafeDrop off1 bs)
    Right (B.DBPointer ns oid, off1 + 12)
  0x0E -> do
    (t, off1) <- readBSONString bs off
    Right (B.Symbol t, off1)
  0x0F -> do
    ensure bs off 4
    let !_totalSz = fromIntegral (readLE32 bs off) :: Int
    (code, off1) <- readBSONString bs (off + 4)
    case decodeDocument bs off1 of
      Left err -> Left err
      Right (scope, off2) -> Right (B.JavaScriptScope code scope, off2)
  0x11 -> do
    ensure bs off 8
    let !w = readLE64 bs off
    Right (B.Timestamp w, off + 8)
  0x13 -> do
    ensure bs off 16
    let !dat = BSU.unsafeTake 16 (BSU.unsafeDrop off bs)
    Right (B.Decimal128 dat, off + 16)
  0x10 -> do
    ensure bs off 4
    let !n = fromIntegral (readLE32 bs off) :: Int32
    Right (B.Int32 n, off + 4)
  0x12 -> do
    ensure bs off 8
    let !n = fromIntegral (readLE64 bs off) :: Int64
    Right (B.Int64 n, off + 8)
  0x7F -> Right (B.MaxKey, off)
  0xFF -> Right (B.MinKey, off)
  _ -> Left $ "BSON.Decode: unknown type tag: " ++ show tag
