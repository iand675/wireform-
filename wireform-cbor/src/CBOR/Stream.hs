{-# LANGUAGE BangPatterns #-}

{- | Incremental CBOR decoding. A suspended decoder retains its parse state and
any pieces of the current string. Resuming it consumes only the new input.
-}
module CBOR.Stream (
  DecodeStep (..),
  streamDecode,
  feedMore,
) where

import CBOR.Internal (halfToFloat)
import CBOR.Value qualified as C
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Wireform.FFI (decodeTextFast)


-- | Result of an incremental decode step. Pass an empty feed to signal EOF.
data DecodeStep a
  = Done !a !ByteString
  | Partial (ByteString -> DecodeStep a)
  | Fail !String


instance Show a => Show (DecodeStep a) where
  show (Done a bs) = "Done " ++ show a ++ " (" ++ show (BS.length bs) ++ " leftover)"
  show (Partial _) = "Partial _"
  show (Fail e) = "Fail " ++ show e


-- | Begin streaming one CBOR value. On success the unused suffix of the final
-- feed is returned unchanged.
streamDecode :: ByteString -> DecodeStep C.Value
streamDecode bytes = readItem bytes 0 Root


-- | Feed more bytes into a 'Partial' continuation.
feedMore :: DecodeStep a -> ByteString -> DecodeStep a
feedMore (Partial k) bs = k bs
feedMore step _ = step


-- Frames are immutable, so feeding the same continuation twice can produce
-- two independent results. A remaining count of -1 means until a break.
data Frame
  = Root
  | ArrayItems {-# UNPACK #-} !Int {-# UNPACK #-} !Int ![C.Value] !Frame
  | MapKey {-# UNPACK #-} !Int {-# UNPACK #-} !Int ![(C.Value, C.Value)] !Frame
  | MapValue {-# UNPACK #-} !Int {-# UNPACK #-} !Int !C.Value ![(C.Value, C.Value)] !Frame
  | TagItem {-# UNPACK #-} !Word64 !Frame
  | ByteChunks {-# UNPACK #-} !Int ![ByteString] !Frame
  | TextChunks {-# UNPACK #-} !Int ![Text] !Frame


await :: (ByteString -> DecodeStep C.Value) -> DecodeStep C.Value
await resume = Partial $ \bytes ->
  if BS.null bytes
    then Fail "CBOR.Stream: incomplete CBOR value"
    else resume bytes
{-# INLINE await #-}


invalid :: String -> DecodeStep a
invalid message = Fail ("CBOR.Stream: " ++ message)


readItem :: ByteString -> Int -> Frame -> DecodeStep C.Value
readItem !buf !off !frame
  | off == BS.length buf = await (\more -> readItem more 0 frame)
  | header == 0xff = closeItem buf next frame
  | otherwise = case frame of
      ByteChunks _ _ _
        | major /= 2 || info == 31 -> invalid "expected a definite byte string chunk"
      TextChunks _ _ _
        | major /= 3 || info == 31 -> invalid "expected a definite text string chunk"
      _ -> decodeHead buf next header frame
  where
    header = BSU.unsafeIndex buf off
    next = off + 1
    major = header `shiftR` 5
    info = header .&. 31


decodeHead :: ByteString -> Int -> Word8 -> Frame -> DecodeStep C.Value
decodeHead buf off header frame
  | info < 24 = finishHead buf off header (fromIntegral info) frame
  | info < 28 = readArgument buf off header (1 `shiftL` fromIntegral (info - 24)) 0 frame
  | info /= 31 = invalid "reserved additional information"
  | otherwise = case header `shiftR` 5 of
      2 -> readItem buf off (ByteChunks 0 [] frame)
      3 -> readItem buf off (TextChunks 0 [] frame)
      4 -> readItem buf off (ArrayItems (-1) 0 [] frame)
      5 -> readItem buf off (MapKey (-1) 0 [] frame)
      _ -> invalid "indefinite length is invalid in this item"
  where
    info = header .&. 31
{-# INLINE decodeHead #-}


-- Only the bytes already read into the argument survive a short feed.
readArgument :: ByteString -> Int -> Word8 -> Int -> Word64 -> Frame -> DecodeStep C.Value
readArgument !buf !off !header !needed !arg !frame
  | needed == 0 = finishHead buf off header arg frame
  | off == BS.length buf = await (\more -> readArgument more 0 header needed arg frame)
  | otherwise =
      let !byte = BSU.unsafeIndex buf off
      in readArgument buf (off + 1) header (needed - 1) ((arg `shiftL` 8) .|. fromIntegral byte) frame


finishHead :: ByteString -> Int -> Word8 -> Word64 -> Frame -> DecodeStep C.Value
finishHead buf off header arg frame = case header `shiftR` 5 of
  0 -> acceptValue buf off (C.UInt arg) frame
  1 -> acceptValue buf off (C.NInt arg) frame
  2 -> string
  3 -> string
  4
    | arg > maxSize -> invalid "array size exceeds the supported range"
    | arg == 0 -> acceptValue buf off (C.Array V.empty) frame
    | otherwise -> readItem buf off (ArrayItems (fromIntegral arg) 0 [] frame)
  5
    | arg > maxSize `quot` 2 -> invalid "map size exceeds the supported range"
    | arg == 0 -> acceptValue buf off (C.Map V.empty) frame
    | otherwise -> readItem buf off (MapKey (fromIntegral arg) 0 [] frame)
  6 -> readItem buf off (TagItem arg frame)
  _ -> case header .&. 31 of
    20 -> acceptValue buf off (C.Bool False) frame
    21 -> acceptValue buf off (C.Bool True) frame
    22 -> acceptValue buf off C.Null frame
    23 -> acceptValue buf off C.Undefined frame
    24
      | arg < 32 -> invalid "invalid two-byte simple value"
    25 -> acceptValue buf off (C.Float16 (halfToFloat (fromIntegral arg))) frame
    26 -> acceptValue buf off (C.Float32 (castWord32ToFloat (fromIntegral arg))) frame
    27 -> acceptValue buf off (C.Float64 (castWord64ToDouble arg)) frame
    _ -> acceptValue buf off (C.Simple (fromIntegral arg)) frame
  where
    maxSize = fromIntegral (maxBound :: Int)
    string = case frame of
      ByteChunks size chunks parent ->
        addChunk size (\total -> payload chunks (ByteChunks total [] parent))
      TextChunks size chunks parent ->
        addChunk size (\total -> payload [] (TextChunks total chunks parent))
      _
        | arg > maxSize -> invalid "string size exceeds the supported range"
        | otherwise -> payload [] frame

    addChunk size resume
      | arg > maxSize - fromIntegral size =
          invalid "combined string size exceeds the supported range"
      | otherwise = resume (size + fromIntegral arg)

    payload = readPayload buf off (header `shiftR` 5 == 3) (fromIntegral arg)


-- A payload wholly inside a feed is a slice. Spanning payloads collect slices
-- across feeds and allocate one contiguous result when their last piece arrives.
readPayload :: ByteString -> Int -> Bool -> Int -> [ByteString] -> Frame -> DecodeStep C.Value
readPayload !buf !off !isText !needed !pieces !frame
  | needed <= available =
      let !lastPiece = BSU.unsafeTake needed (BSU.unsafeDrop off buf)
          !end = off + needed
      in case (isText, frame) of
           (False, ByteChunks size _ parent) ->
             -- Keep the slices in the outer accumulator. The indefinite
             -- string will be joined once, even if its chunks span feeds.
             let !allChunks = if needed == 0 then pieces else lastPiece : pieces
             in readItem buf end (ByteChunks size allChunks parent)
           _ ->
             let !payload = case pieces of
                   [] -> lastPiece
                   _ -> BS.concat (reverse (lastPiece : pieces))
             in if isText
                  then case decodeTextFast payload of
                    Left _ -> invalid "invalid UTF-8 in text string"
                    Right value -> acceptValue buf end (C.TextString value) frame
                  else acceptValue buf end (C.ByteString payload) frame
  | available == 0 =
      await (\more -> readPayload more 0 isText needed pieces frame)
  | otherwise =
      let !piece = BSU.unsafeDrop off buf
          !pieces' = piece : pieces
          !needed' = needed - available
      in await (\more -> readPayload more 0 isText needed' pieces' frame)
  where
    available = BS.length buf - off


closeItem :: ByteString -> Int -> Frame -> DecodeStep C.Value
closeItem buf off = \case
  ArrayItems (-1) count items parent ->
    acceptValue buf off (C.Array (fromReversed count items)) parent
  MapKey (-1) count pairs parent ->
    acceptValue buf off (C.Map (fromReversed count pairs)) parent
  ByteChunks _ chunks parent ->
    acceptValue buf off (C.ByteString (BS.concat (reverse chunks))) parent
  TextChunks _ chunks parent ->
    acceptValue buf off (C.TextString (T.concat (reverse chunks))) parent
  _ -> invalid "break outside an indefinite-length item"


acceptValue :: ByteString -> Int -> C.Value -> Frame -> DecodeStep C.Value
acceptValue !buf !off !value = \case
  Root -> Done value (BSU.unsafeDrop off buf)
  ArrayItems remaining count items parent
    | remaining == 1 -> acceptValue buf off (C.Array (fromReversed (count + 1) (value : items))) parent
    | otherwise -> readItem buf off (ArrayItems (nextCount remaining) (count + 1) (value : items) parent)
  MapKey remaining count pairs parent ->
    readItem buf off (MapValue remaining count value pairs parent)
  MapValue remaining count key pairs parent
    | remaining == 1 -> acceptValue buf off (C.Map (fromReversed (count + 1) ((key, value) : pairs))) parent
    | otherwise -> readItem buf off (MapKey (nextCount remaining) (count + 1) ((key, value) : pairs) parent)
  TagItem tag parent -> acceptValue buf off (C.Tag tag value) parent
  TextChunks size chunks parent -> case value of
    C.TextString t
      | T.null t -> readItem buf off (TextChunks size chunks parent)
      | otherwise -> readItem buf off (TextChunks size (t : chunks) parent)
    _ -> invalid "expected a text string chunk"
  ByteChunks {} -> invalid "expected a byte string chunk"
  where
    nextCount remaining
      | remaining < 0 = remaining
      | otherwise = remaining - 1


-- Every element increments the count once. Fill from the end to avoid
-- allocating a reversed list just to construct the vector.
fromReversed :: Int -> [a] -> V.Vector a
fromReversed !count items = V.create $ do
  vec <- MV.new count
  let fill !_ [] = pure ()
      fill !i (x : rest) = do
        MV.unsafeWrite vec i x
        fill (i - 1) rest
  fill (count - 1) items
  pure vec
