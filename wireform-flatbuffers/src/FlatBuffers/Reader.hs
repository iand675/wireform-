{-# LANGUAGE BangPatterns #-}

{- | Zero-copy FlatBuffers /reader/ primitives.

Mirror image of "FlatBuffers.Builder": chases @soffset_t@ /
@uoffset_t@ chains through a flat byte buffer and exposes the
table-slot resolution + scalar peeks that higher layers (Apache
Arrow IPC, hand-targeted decoders, "FlatBuffers.View") need.

The functions here never materialise an intermediate
'FlatBuffers.Value.Value' — they're for callers who know the
exact schema of the table they're decoding (e.g. Arrow's
@Schema.fbs@) and want to traverse it without paying for a
generic representation. "FlatBuffers.Decode" stays the
value-shaped surface.

= Performance discipline

All scalar peeks bottom out in a single 'peekByteOff' against
the input 'ByteString'\'s 'ForeignPtr'. FlatBuffers guarantees
every scalar lands at its natural alignment within the buffer,
so this is a single aligned load — no byte-shift loop, no
intermediate 'Word'-sized copy, no list of bytes. On x86_64 /
ARM64 the LE encoding is the in-memory encoding so the peek is
effectively free.

All offsets are absolute byte positions within the input
'ByteString' (= forward layout). 'resolveTable' returns a
closure that maps a slot index to the absolute byte offset of
that slot's inline data, or 'Nothing' for an absent slot.

= Zero-copy strings and byte vectors

'readStringSlice' and 'readByteVectorSlice' return 'ByteString'
slices that share the input's 'ForeignPtr' — no copying, no
allocation beyond a 24-byte 'ByteString' header. 'readString'
decodes UTF-8 via 'Data.Text.Encoding.decodeUtf8'' which copies
once into a 'Text' (unavoidable: 'Text' is UTF-16 internal in
text < 2.0 and a separate UTF-8 array in text >= 2.0). For the
common case where you want raw UTF-8 bytes (Arrow column names,
log lines, ...) prefer the slice variant.
-}
module FlatBuffers.Reader (
  -- * Position type
  Pos,

  -- * Scalar peeks (zero-allocation)
  peekU8,
  peekU16,
  peekU32,
  peekU64,
  peekI8,
  peekI16,
  peekI32,
  peekI64,
  peekFloat,
  peekDouble,

  -- * Table / vector navigation
  resolveTable,
  followUOffset,
  readString,
  readStringSlice,
  readByteVectorSlice,
  readVectorOfOffsets,
  readVectorInt64,
  readVectorOfStructs,

  -- * Vector-as-cursor primitives
  vectorLength,
  vectorElementAt,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)


{- | A position in a flatbuffer 'ByteString'. The reader uses
absolute byte offsets throughout, never the back-to-front
@UOffset@s that the builder works with.
-}
type Pos = Int


-- ============================================================
-- Scalar peeks
-- ============================================================
--
-- Every primitive bottoms out here. We grab the buffer's raw
-- 'Ptr Word8' through 'unsafeDupablePerformIO' (the read is pure
-- in the value sense — nothing in the buffer changes — but the
-- 'withForeignPtr' bracket is in IO). 'peekByteOff' compiles to a
-- single load on x86_64 / ARM64 because FlatBuffers data is
-- little-endian and naturally aligned.

-- | Read an unsigned byte at @off@.
peekU8 :: ByteString -> Pos -> Either String Word8
peekU8 bs off
  | off < 0 || off >= BS.length bs = boundsErr "peekU8"
  | otherwise = Right $! BSU.unsafeIndex bs off
{-# INLINE peekU8 #-}


peekI8 :: ByteString -> Pos -> Either String Int8
peekI8 bs off = fromIntegral <$> peekU8 bs off
{-# INLINE peekI8 #-}


peekU16 :: ByteString -> Pos -> Either String Word16
peekU16 = peekFixed 2 "peekU16"
{-# INLINE peekU16 #-}


peekU32 :: ByteString -> Pos -> Either String Word32
peekU32 = peekFixed 4 "peekU32"
{-# INLINE peekU32 #-}


peekU64 :: ByteString -> Pos -> Either String Word64
peekU64 = peekFixed 8 "peekU64"
{-# INLINE peekU64 #-}


peekI16 :: ByteString -> Pos -> Either String Int16
peekI16 bs off = fromIntegral <$> peekU16 bs off
{-# INLINE peekI16 #-}


peekI32 :: ByteString -> Pos -> Either String Int32
peekI32 bs off = fromIntegral <$> peekU32 bs off
{-# INLINE peekI32 #-}


peekI64 :: ByteString -> Pos -> Either String Int64
peekI64 bs off = fromIntegral <$> peekU64 bs off
{-# INLINE peekI64 #-}


peekFloat :: ByteString -> Pos -> Either String Float
peekFloat bs off = castWord32ToFloat <$> peekU32 bs off
{-# INLINE peekFloat #-}


peekDouble :: ByteString -> Pos -> Either String Double
peekDouble bs off = castWord64ToDouble <$> peekU64 bs off
{-# INLINE peekDouble #-}


{- | Generic fixed-width LE peek. Specialised internally by
'peekU16' / 'peekU32' / 'peekU64'. Picks up a 'Ptr Word8'
through 'unsafeWithForeignPtr' and lets GHC inline the @Storable@
dictionary into a single aligned load.
-}
peekFixed
  :: Storable a
  => Int
  -- ^ width in bytes
  -> String
  -- ^ caller name for error reporting
  -> ByteString
  -> Pos
  -> Either String a
peekFixed !w name bs off
  | off < 0 || off > BS.length bs - w = boundsErr name
  | otherwise =
      let !val = unsafeDupablePerformIO (withBSPtr bs (\p -> peekByteOff p off))
      in Right val
{-# INLINE peekFixed #-}


{- | Run @f@ with a 'Ptr' to the start of the input 'ByteString'.
Reads are pure in the value sense; the 'IO' bracket is just a
consequence of the 'ForeignPtr' API. Every caller passes a single
load, which cannot diverge, so 'unsafeWithForeignPtr' is sound and
avoids the @keepAlive#@ frame 'withForeignPtr' pushes on every peek.
-}
withBSPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withBSPtr (BSI.BS fp _) f = unsafeWithForeignPtr fp f
{-# INLINE withBSPtr #-}


boundsErr :: String -> Either String a
boundsErr name = Left ("FlatBuffers.Reader." <> name <> ": offset out of range")


-- ============================================================
-- Table navigation
-- ============================================================

{- | Resolve a table at @tablePos@. Returns a closure that maps
slot indices to absolute byte offsets, or 'Nothing' for absent
(zero-offset / out-of-vtable) slots.

The closure intentionally does /not/ thread an error monad: an
out-of-range slot index is treated as absent, matching how
generated FlatBuffers readers behave when a schema gains new
fields after a buffer was written.
-}
resolveTable :: ByteString -> Pos -> Either String (Int -> Maybe Pos)
resolveTable bs tablePos = do
  soff <- peekI32 bs tablePos
  let !vtablePos = tablePos - fromIntegral soff
  if vtablePos < 0 || vtablePos >= BS.length bs
    then Left "FlatBuffers.Reader.resolveTable: vtable out of bounds"
    else do
      vtSize <- peekU16 bs vtablePos
      let !nSlots = (fromIntegral vtSize - 4) `div` 2 :: Int
      Right $ \i ->
        if i < 0 || i >= nSlots
          then Nothing
          else case peekU16 bs (vtablePos + 4 + 2 * i) of
            Left _ -> Nothing
            Right 0 -> Nothing
            Right off -> Just (tablePos + fromIntegral off)
{-# INLINE resolveTable #-}


{- | Follow a u32 uoffset at @fieldPos@ to the absolute byte
position of the referenced object.
-}
followUOffset :: ByteString -> Pos -> Either String Pos
followUOffset bs fieldPos = do
  rel <- peekU32 bs fieldPos
  Right (fieldPos + fromIntegral rel)
{-# INLINE followUOffset #-}


-- ============================================================
-- Strings and byte vectors (zero-copy)
-- ============================================================

{- | Decode a UTF-8 string at the given uoffset target position.
Allocates a 'Text' (unavoidable). For raw UTF-8 access without
a copy, use 'readStringSlice'.
-}
readString :: ByteString -> Pos -> Either String T.Text
readString bs strPos = do
  raw <- readStringSlice bs strPos
  case TE.decodeUtf8' raw of
    Left _ -> Left "FlatBuffers.Reader.readString: invalid UTF-8"
    Right t -> Right t


{- | Slice the underlying 'ByteString' for a flatbuffer string.
The returned slice shares the input's 'ForeignPtr' — the only
new allocation is the 24-byte 'ByteString' record itself (no
payload copy).
-}
readStringSlice :: ByteString -> Pos -> Either String ByteString
readStringSlice bs strPos = do
  len <- peekU32 bs strPos
  let !n = fromIntegral len :: Int
  if strPos + 4 + n > BS.length bs
    then Left "FlatBuffers.Reader.readStringSlice: out of range"
    else Right $! BSU.unsafeTake n (BSU.unsafeDrop (strPos + 4) bs)
{-# INLINE readStringSlice #-}


{- | Slice the underlying 'ByteString' for a @[ubyte]@ vector
(FlatBuffers' canonical raw-bytes encoding). Same zero-copy
guarantee as 'readStringSlice'.
-}
readByteVectorSlice :: ByteString -> Pos -> Either String ByteString
readByteVectorSlice = readStringSlice
{-# INLINE readByteVectorSlice #-}


-- ============================================================
-- Vectors
-- ============================================================

{- | Number of elements in a vector at @vecPos@. Reads a single
u32 — does not touch the payload.
-}
vectorLength :: ByteString -> Pos -> Either String Int
vectorLength bs vecPos = fromIntegral <$> peekU32 bs vecPos
{-# INLINE vectorLength #-}


{- | Absolute byte position of the @i@th element in a vector at
@vecPos@, given the per-element stride. For UOffset vectors,
the caller still needs a 'followUOffset' to chase the pointer;
for inline scalar / struct vectors this is the value's
position directly.
-}
vectorElementAt :: Pos -> Int -> Int -> Pos
vectorElementAt vecPos !stride !i = vecPos + 4 + i * stride
{-# INLINE vectorElementAt #-}


{- | Decode a vector of @uoffset_t@ table references. Returns each
referenced object's absolute byte position.

Allocates a boxed 'V.Vector' of 'Int'. For pure index-based
traversal that doesn't need the materialised vector, use
'vectorLength' + 'vectorElementAt' + 'followUOffset' directly.
-}
readVectorOfOffsets :: ByteString -> Pos -> Either String (V.Vector Pos)
readVectorOfOffsets bs vecPos = do
  n <- checkedVectorLength "readVectorOfOffsets" bs vecPos 4
  V.generateM n $ \i -> followUOffset bs (vecPos + 4 + 4 * i)


-- | Decode a vector of little-endian Int64 values.
readVectorInt64 :: ByteString -> Pos -> Either String [Int64]
readVectorInt64 bs vecPos = do
  n <- checkedVectorLength "readVectorInt64" bs vecPos 8
  goVec n (\i -> peekI64 bs (vecPos + 4 + 8 * i))


{- | Element count of the vector at @vecPos@, checked so that all
@count * stride@ payload bytes lie inside the buffer. Callers can
then size allocations by the count without trusting the wire.
-}
checkedVectorLength :: String -> ByteString -> Pos -> Int -> Either String Int
checkedVectorLength name bs vecPos stride = do
  n <- fromIntegral <$> peekU32 bs vecPos
  -- peekU32 succeeded, so 0 <= vecPos <= length - 4.
  if n > (BS.length bs - vecPos - 4) `quot` max 1 stride
    then Left ("FlatBuffers.Reader." <> name <> ": vector of " <> show n <> " elements overruns the buffer")
    else Right n


{- | Walk @0..n-1@ and accumulate results, short-circuiting on
@Left@. Equivalent to @mapM f [0..n-1]@ but spelled out so we
don't lean on a list comprehension or laziness in a hot decode
path.
-}
goVec :: Int -> (Int -> Either String a) -> Either String [a]
goVec n f = go 0
  where
    go !i
      | i >= n = Right []
      | otherwise = do
          x <- f i
          xs <- go (i + 1)
          Right (x : xs)


{- | Decode a vector of fixed-size inline structs. Returns
@(elemCount, byte position of each element start)@; every element
lies inside the buffer.
-}
readVectorOfStructs
  :: ByteString
  -> Pos
  -> Int
  -- ^ stride (per-struct size)
  -> Either String (Int, V.Vector Pos)
readVectorOfStructs bs vecPos stride = do
  n <- checkedVectorLength "readVectorOfStructs" bs vecPos stride
  let elems = V.generate n (\i -> vecPos + 4 + i * stride)
  Right (n, elems)
