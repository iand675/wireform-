{-# LANGUAGE BangPatterns #-}

-- | Decode an unpacked, single-segment Cap'n Proto message.
module CapnProto.Decode (decode) where

import CapnProto.Value qualified as C
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Bits ((.&.), shiftR, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64)
import Foreign.Storable (Storable, peekByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)


-- | Struct data words are returned as 'C.UInt64', since field types and
-- bit offsets require a schema. Pointer sections retain their wire shape.
decode :: ByteString -> Either String C.Value
decode bytes
  | BS.length bytes < 8 = Left "CapnProto.Decode: input too short for segment table"
  | (readAt bytes 0 :: Word32) /= 0 = Left "CapnProto.Decode: only single-segment messages supported"
  | segmentBytes > BS.length bytes - 8 = Left "CapnProto.Decode: segment truncated"
  | segmentBytes < 8 = Left "CapnProto.Decode: missing root struct pointer"
  | root .&. 3 /= 0 = Left "CapnProto.Decode: root pointer is not a struct pointer"
  | root == 0 = Right C.Void
  | otherwise = evalStateT
      (decodeStruct segment 0 (pointerTarget 0 root) (dataWords root) (pointerWords root))
      (8 * 1024 * 1024)
  where
    segmentBytes = fromIntegral (readAt bytes 4 :: Word32) * 8
    segment = BS.take segmentBytes (BS.drop 8 bytes)
    root = readAt segment 0 :: Word64


readAt :: Storable a => ByteString -> Int -> a
readAt (BSI.BS fp _) offset =
  unsafeDupablePerformIO $ BSI.unsafeWithForeignPtr fp $ \ptr ->
    peekByteOff ptr offset
{-# INLINE readAt #-}


type Decoder = StateT Int (Either String)


inSegment :: ByteString -> Int -> Int -> Decoder ()
inSegment bytes start size =
  unless (start >= 0 && size >= 0 && start <= BS.length bytes - size) $
    lift (Left "CapnProto.Decode: object exceeds declared segment")
{-# INLINE inSegment #-}


-- The wire permits zero-width list elements. Charge for the materialized
-- values as well as the data words so those lists share the same budget.
spendWords :: Int -> Decoder ()
spendWords wordsUsed = do
  remaining <- get
  when (wordsUsed > remaining) $ lift (Left "CapnProto.Decode: traversal limit exceeded")
  put $! remaining - wordsUsed
{-# INLINE spendWords #-}


decodePointer :: ByteString -> Int -> Int -> Decoder C.Value
decodePointer bytes depth slot = do
  when (depth == 64) $ lift (Left "CapnProto.Decode: pointer nesting exceeds 64")
  inSegment bytes slot 8
  let !pointer = readAt bytes slot :: Word64
      !target = pointerTarget slot pointer
  if pointer == 0
    then pure C.Void
    else case pointer .&. 3 of
      0 -> decodeStruct bytes depth target (dataWords pointer) (pointerWords pointer)
      1 -> decodeList bytes depth target pointer
      _ -> lift (Left "CapnProto.Decode: unsupported pointer type")


pointerTarget :: Int -> Word64 -> Int
pointerTarget slot pointer =
  let low = fromIntegral ((pointer `shiftR` 2) .&. 0x3fffffff)
      offset = if testBit low 29 then low - 0x40000000 else low
  in slot + 8 * (offset + 1)
{-# INLINE pointerTarget #-}


dataWords :: Word64 -> Int
dataWords word = fromIntegral ((word `shiftR` 32) .&. 0xffff)
{-# INLINE dataWords #-}


pointerWords :: Word64 -> Int
pointerWords word = fromIntegral (word `shiftR` 48)
{-# INLINE pointerWords #-}


decodeStruct :: ByteString -> Int -> Int -> Int -> Int -> Decoder C.Value
decodeStruct bytes depth start dw pw = do
  inSegment bytes start (8 * (dw + pw))
  spendWords (max 1 (dw + pw))
  let !dat = V.generate dw (\i -> C.UInt64 (readAt bytes (start + 8 * i)))
  pointers <- V.generateM pw (\i -> decodePointer bytes (depth + 1) (start + 8 * (dw + i)))
  pure (C.Struct dat pointers)


decodeList :: ByteString -> Int -> Int -> Word64 -> Decoder C.Value
decodeList bytes depth start pointer =
  case code of
    0 -> dataList 0 (const C.Void)
    1 -> dataList ((count + 7) `quot` 8) $ \i ->
      let (byte, bit) = i `quotRem` 8
      in C.Bool (testBit (BSU.unsafeIndex bytes (start + byte)) bit)
    2 -> do
      inSegment bytes start count
      spendWords ((count + 7) `quot` 8)
      let raw = BS.take count (BS.drop start bytes)
      pure $ case BS.unsnoc raw of
        Just (body, 0) | Right text <- TE.decodeUtf8' body -> C.Text text
        _ -> C.Data raw
    3 -> dataList (2 * count) (\i -> C.UInt16 (readAt bytes (start + 2 * i)))
    4 -> dataList (4 * count) (\i -> C.UInt32 (readAt bytes (start + 4 * i)))
    5 -> dataList (8 * count) (\i -> C.UInt64 (readAt bytes (start + 8 * i)))
    6 -> do
      inSegment bytes start (8 * count)
      spendWords count
      C.List <$> V.generateM count (\i -> decodePointer bytes (depth + 1) (start + 8 * i))
    _ -> do
      inSegment bytes start (8 * (count + 1))
      let !tag = readAt bytes start :: Word64
          !items = fromIntegral ((tag `shiftR` 2) .&. 0x3fffffff)
          !dw = dataWords tag
          !pw = pointerWords tag
          !stride = dw + pw
      unless (tag .&. 3 == 0 && items * stride <= count) $
        lift (Left "CapnProto.Decode: invalid composite list layout")
      spendWords items
      C.List <$> V.generateM items (\i -> decodeStruct bytes depth (start + 8 * (1 + i * stride)) dw pw)
  where
    code = (pointer `shiftR` 32) .&. 7
    count = fromIntegral (pointer `shiftR` 35)
    dataList size element = do
      inSegment bytes start size
      spendWords count
      pure (C.List (V.generate count element))
