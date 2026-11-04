{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Direct UTF-8 output using the canonical growing buffer. Service
characters are ASCII, so byte-level escaping preserves non-ASCII payloads.
The recursive writer uses unboxed builder state rather than a fragment
per field. Each field reserves a safe upper bound before writing; there
is no full-document sizing pass or final output compaction copy.
-}
module EDI.Internal.Encode (Services, services, encode) where

import Data.Array.Byte (ByteArray (..))
import Data.Bits (bit, complement, unsafeShiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Array qualified as A
import Data.Text.Foreign qualified as TF
import Data.Text.Internal qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word64, Word8)
import EDI.Value
import Foreign.Storable (pokeByteOff)
import GHC.Exts (Int (I#), Ptr (..), indexWord8ArrayAsWord32#, indexWord8ArrayAsWord64#, plusAddr#)
import GHC.IO (IO (..))
import GHC.Word (Word32 (W32#), Word64 (W64#))
import Wireform.Builder.FastBuilder qualified as B


-- Constructed only after the public entry point validates the advice.
data Services = Services
  { component :: {-# UNPACK #-} !Word8
  , element :: {-# UNPACK #-} !Word8
  , terminator :: {-# UNPACK #-} !Word8
  , release :: {-# UNPACK #-} !Word8
  , lowMask :: {-# UNPACK #-} !Word64
  , highMask :: {-# UNPACK #-} !Word64
  , defaultEscapes :: !Bool
  }


services :: Syntax -> Char -> Services
services syntax rel =
  Services
    (ascii (componentSeparator syntax))
    (ascii (elementSeparator syntax))
    (ascii (segmentTerminator syntax))
    (ascii rel)
    low
    high
    (high == 0 && low == defaultMask)
  where
    low = foldl' (\mask c -> if ord c < 64 then mask .|. bit (ord c) else mask) 0 chars
    high = foldl' (\mask c -> if ord c >= 64 then mask .|. bit (ord c - 64) else mask) 0 chars
    chars =
      [componentSeparator syntax, elementSeparator syntax, segmentTerminator syntax, rel]
        <> maybe [] pure (repetitionSeparator syntax)
    ascii = fromIntegral . ord


-- The guards prove both shift counts lie in [0,63]. Using the unchecked
-- operation avoids redundant overflow checks on every payload byte.
reserved :: Services -> Word8 -> Bool
reserved cfg w
  | w < 64 = (lowMask cfg `unsafeShiftR` fromIntegral w) .&. 1 /= 0
  | w < 128 = (highMask cfg `unsafeShiftR` (fromIntegral w - 64)) .&. 1 /= 0
  | otherwise = False
{-# INLINE reserved #-}


-- A small per-segment estimate is only the initial capacity. The
-- canonical growing builder handles arbitrarily long fields; encoding
-- does not first walk every field just to calculate an upper bound.
encode :: Services -> ByteString -> Vector Segment -> Either String ByteString
encode cfg header segments
  | V.any (Text.null . segmentTag) segments = Left "EDI.EDIFACT: empty segment tag"
  | V.null segments = Right header
  | otherwise = Right $ B.toStrictByteStringWith initial builder
  where
    !initial = max 128 (64 * min 1024 (V.length segments))
    builder
      | defaultEscapes cfg = buildSegments cfg (writeDefault (release cfg)) header segments
      | otherwise = buildSegments cfg (writeGeneral cfg) header segments


-- One builder closure for the entire document. Recursive workers thread
-- the builder's unboxed cursor/state directly, rather than allocating a
-- Builder fragment for each segment, field or payload character.
buildSegments :: Services -> (Ptr Word8 -> Int -> Text -> IO Int) -> ByteString -> Vector Segment -> B.Builder
buildSegments cfg writeText header segments = B.Builder $ \sink state ->
  case B.unBuilder (B.byteStringCopy header) sink state of
    next -> segmentLoop 0 sink next
  where
    segmentLoop :: Int -> B.DataSink -> B.BuilderState -> B.BuilderState
    segmentLoop !i sink state
      | i >= V.length segments = state
      | otherwise = case V.unsafeIndex segments i of
          Segment tag values ->
            case B.unBuilder (boundedText writeText 0 0 tag) sink state of
              next -> fieldLoop i values 0 sink next
    fieldLoop :: Int -> Vector Element -> Int -> B.DataSink -> B.BuilderState -> B.BuilderState
    fieldLoop !segmentIndex values !i sink state
      | i >= V.length values =
          case B.unBuilder (boundedByte (terminator cfg)) sink state of
            next -> segmentLoop (segmentIndex + 1) sink next
      | otherwise = case V.unsafeIndex values i of
          Simple text ->
            case B.unBuilder (boundedText writeText 1 (element cfg) text) sink state of
              next -> fieldLoop segmentIndex values (i + 1) sink next
          Composite parts ->
            case B.unBuilder (boundedByte (element cfg)) sink state of
              next -> componentLoop segmentIndex i parts 0 sink next
    -- Fetch the existing parent vector again at component exit. Carrying
    -- it across these recursive workers makes GHC rebuild its Vector
    -- header and boxed segment index for every composite (48 bytes).
    componentLoop :: Int -> Int -> Vector Text -> Int -> B.DataSink -> B.BuilderState -> B.BuilderState
    componentLoop !segmentIndex !fieldIndex parts !i sink state
      | i >= V.length parts = case V.unsafeIndex segments segmentIndex of
          Segment _ values -> fieldLoop segmentIndex values (fieldIndex + 1) sink state
      | otherwise =
          case B.unBuilder (boundedText writeText (if i == 0 then 0 else 1) (component cfg) (V.unsafeIndex parts i)) sink state of
            next -> componentLoop segmentIndex fieldIndex parts (i + 1) sink next
{-# INLINE buildSegments #-}


-- The public advice is ASCII, so each UTF-8 payload byte needs at most
-- one extra release byte. Negative requests denote Int overflow and are
-- rejected before entering the unsafe builder or writing a prefix.
boundedText :: (Ptr Word8 -> Int -> Text -> IO Int) -> Int -> Word8 -> Text -> B.Builder
boundedText writeText !prefix !separator text
  | bound < 0 = error "EDI.EDIFACT: encoded field exceeds the addressable buffer size"
  | otherwise = B.Builder $ \sink state ->
      case B.unBuilder (B.ensureBytes bound) sink state of
        (# cur, end, s #) ->
          case ( do
                   if prefix == 0 then pure () else pokeByteOff (Ptr cur) 0 separator
                   writeText (Ptr cur) prefix text
               ) of
            IO run -> case run s of
              (# s', I# written #) -> (# plusAddr# cur written, end, s' #)
  where
    !bound = prefix + 2 * TF.lengthWord8 text
{-# INLINE boundedText #-}


boundedByte :: Word8 -> B.Builder
boundedByte !byte = B.Builder $ \sink state ->
  case B.unBuilder (B.ensureBytes 1) sink state of
    (# cur, end, s #) -> case pokeByteOff (Ptr cur) 0 byte of
      IO run -> case run s of
        (# s', () #) -> (# plusAddr# cur 1#, end, s' #)
{-# INLINE boundedByte #-}


-- Custom service alphabets use the full scalar mask.
writeGeneral :: Services -> Ptr Word8 -> Int -> Text -> IO Int
writeGeneral cfg ptr start (T.Text array off size) = copy 0 start
  where
    copy !i !position
      | i >= size = pure position
      | otherwise = do
          let !w = A.unsafeIndex array (off + i)
          if reserved cfg w
            then do
              pokeByteOff ptr position (release cfg)
              pokeByteOff ptr (position + 1) w
              copy (i + 1) (position + 2)
            else pokeByteOff ptr position w >> copy (i + 1) (position + 1)
{-# INLINE writeGeneral #-}


defaultMask :: Word64
defaultMask = bit 39 .|. bit 43 .|. bit 58 .|. bit 63


-- Each word read and write stays entirely within the input slice and
-- output capacity. A matching word is handled bytewise before the next
-- word; this avoids rescanning overlapping words around release runs.
writeDefault :: Word8 -> Ptr Word8 -> Int -> Text -> IO Int
writeDefault rel ptr start (T.Text array off size) = chunks 0 start
  where
    chunks !i !position
      | i >= size = pure position
      | size - i < 4 = scalar size i position
      | size - i < 8 =
          let !word = load32 array (off + i)
          in if mayContainReserved (fromIntegral word)
               then scalar (i + 4) i position
               else do
                 pokeByteOff ptr position word
                 chunks (i + 4) (position + 4)
      | otherwise =
          let !word = load64 array (off + i)
          in if mayContainReserved word
               then scalar (i + 8) i position
               else do
                 pokeByteOff ptr position word
                 chunks (i + 8) (position + 8)
    scalar !limit !i !position
      | i >= limit = chunks i position
      | otherwise = do
          let !w = A.unsafeIndex array (off + i)
          if w < 64 && (defaultMask `unsafeShiftR` fromIntegral w) .&. 1 /= 0
            then do
              pokeByteOff ptr position rel
              pokeByteOff ptr (position + 1) w
              scalar limit (i + 1) (position + 2)
            else do
              pokeByteOff ptr position w
              scalar limit (i + 1) (position + 1)
{-# INLINE writeDefault #-}


load32 :: A.Array -> Int -> Word32
load32 (ByteArray array#) (I# off#) = W32# (indexWord8ArrayAsWord32# array# off#)
{-# INLINE load32 #-}


load64 :: A.Array -> Int -> Word64
load64 (ByteArray array#) (I# off#) = W64# (indexWord8ArrayAsWord64# array# off#)
{-# INLINE load64 #-}


-- All four reserved bytes are in 0x20..0x3f and have bit1 set. This
-- conservative filter is much cheaper than four word-wide equalities.
-- A hit only selects the exact scalar mask check; false positives such
-- as digits 2/3/6/7 cannot cause extra escaping. UTF-8 bytes and zero-
-- extended short words remain outside the matching class.
mayContainReserved :: Word64 -> Bool
mayContainReserved word =
  let !w = (word .&. 0xe2e2e2e2e2e2e2e2) `xor` 0x2222222222222222
  in (w - 0x0101010101010101) .&. complement w .&. 0x8080808080808080 /= 0
{-# INLINE mayContainReserved #-}
