-- SpecConstr would specialise the table/vector loops on the unpacked
-- 'Builder' fields and then rebox the 'Builder' for every field-writer
-- call; the boxed handle is what those calls need anyway.
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

{- | Spec-compliant FlatBuffers /builder/.

Buffers are constructed back-to-front, like every other FlatBuffers
implementation (flatcc, flatbuffers-cpp, the Rust crate): each call
/prepends/ bytes while the builder tracks the running @minAlign@. At
'finish' the buffer is padded to @max minAlign 4@ and a u32 root
offset is emitted, yielding the canonical
@[root_offset][...padded...][vtables, tables, ...]@ layout.

This module is schema-agnostic. Higher layers like
"Arrow.FlatBufferIPC" and "FlatBuffers.Encode" compose 'writeTable',
'writeString' and 'writeVectorOfOffsets' on top of it.

= Representation

All bytes live in one pinned 'MutableByteArray#' that is filled from
its end towards its start. The write head, capacity, @minAlign@ and
the vtable count sit in a small unboxed 'Int' array, so a scalar
write is a bounds check, one unaligned little-endian store and a
head update: no allocation. When the front is exhausted the buffer
is reallocated (at least doubled) and the written suffix is copied
to the end of the new buffer; UOffsets are distances from the end,
so growth never invalidates them. 'finish' returns the written
suffix as a 'ByteString' slice of that buffer, without a copy.
Later prepends only touch bytes in front of the slice, so the
returned 'ByteString' stays immutable even if the builder is used
again.

Written vtables are deduplicated: the builder keeps the UOffset of
every distinct vtable and compares a candidate against them
byte-for-byte (as flatbuffers-cpp does), reusing an existing one on
a match.

= Build order vs forward layout

Because the builder is back-to-front, callers emit objects in
/reverse/ of the order a hex-dump reads. 'writeTable' encodes this
once and for all: forward layout ends up
@[vtable][soffset_t][slots][pad]@.

= Alignment guarantee

'prepForObject' bumps @minAlign@ and prepends padding so the
about-to-be-emitted object's UOffset (distance from the end) is
congruent to 0 modulo its alignment. Combined with the final pad in
'finish' (which forces the total size to a multiple of @minAlign@),
every object's /forward/ position @final_size - uoff@ is also
aligned, which is what consumers inspect.
-}
module FlatBuffers.Builder (
  -- * Builder state
  Builder,
  newBuilder,
  newBuilderWithCapacity,
  finish,
  currentUOff,

  -- * Low-level prepend primitives
  prependBS,
  prependU8,
  prependU16,
  prependU32,
  prependU64,
  prependI16,
  prependI32,
  prependI64,
  prepForObject,
  noteMinAlign,

  -- * Tables, vtables, fields
  Field' (Field', fsAlign, fsWrite),
  scalar,
  scalarU8,
  scalarU16,
  scalarU32,
  scalarU64,
  scalarI16,
  scalarI32,
  scalarI64,
  struct,
  voff,
  writeTable,

  -- * Strings, vectors, structs
  writeString,
  writeVectorOfOffsets,
  writeVectorOfStructs,
  writeVectorInt32,
  writeVectorInt64,

  -- * Alignment helper
  alignUp,
) where

import Data.Bits (complement, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int16, Int32, Int64)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Word (byteSwap16, byteSwap32, byteSwap64)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr), unsafeWithForeignPtr)
import GHC.IO (IO (..))
import GHC.Word (Word16 (..), Word32 (..), Word64 (..), Word8 (..))


-- ============================================================
-- Builder state
-- ============================================================

{- | A builder accumulates bytes in reverse: the /tail/ of the output
is written first. The number of bytes written so far is the
/UOffset/ of whatever is written next (its distance from the end of
the finished buffer); see 'currentUOff'.
-}
data Builder
  = Builder
      (MutableByteArray# RealWorld)
      -- ^ 'Int' slots: write head, capacity, minAlign, vtable count.
      (SmallMutableArray# RealWorld (MutableByteArray# RealWorld))
      {- ^ the arrays that get replaced when they grow: the output
      buffer (pinned, filled from the end), the UOffsets of the
      distinct vtables, and 'writeTable''s per-slot scratch. Their
      elements are unlifted, so reading one needs no evaluation check.
      -}


-- Indices into the builder's 'Int' state array.
pattern SlotHead, SlotCap, SlotMinAlign, SlotNVT :: Int#
pattern SlotHead = 0#
pattern SlotCap = 1#
pattern SlotMinAlign = 2#
pattern SlotNVT = 3#


-- Indices into the builder's array of arrays.
pattern ArrBuf, ArrVTables, ArrScratch :: Int#
pattern ArrBuf = 0#
pattern ArrVTables = 1#
pattern ArrScratch = 2#


-- | A builder with a 256-byte initial buffer (it grows as needed).
newBuilder :: IO Builder
newBuilder = newBuilderWithCapacity 256
{-# INLINE newBuilder #-}


{- | A builder whose buffer starts at (at least) the given number of
bytes. Sizing it to the expected output avoids regrowth; the buffer
still grows past it when needed.
-}
newBuilderWithCapacity :: Int -> IO Builder
newBuilderWithCapacity (I# n0) = IO $ \s0 ->
  let !cap = roundUp16# (if isTrue# (n0 <# 16#) then 16# else n0)
  in case newByteArray# 32# s0 of
      (# s1, st #) -> case newAlignedPinnedByteArray# cap 16# s1 of
        (# s2, buf #) -> case newByteArray# 64# s2 of
          -- 8 vtables and 8 table slots before either regrows.
          (# s3, vt #) -> case newByteArray# 128# s3 of
            (# s4, scr #) -> case newSmallArray# 3# buf s4 of
              (# s5, arrs #) ->
                case writeSmallArray# arrs ArrVTables vt s5 of
                  s6 -> case writeSmallArray# arrs ArrScratch scr s6 of
                    s7 -> case writeIntArray# st SlotHead cap s7 of
                      s8 -> case writeIntArray# st SlotCap cap s8 of
                        s9 -> case writeIntArray# st SlotMinAlign 1# s9 of
                          s10 -> case writeIntArray# st SlotNVT 0# s10 of
                            s11 -> (# s11, Builder st arrs #)


roundUp16# :: Int# -> Int#
roundUp16# n = (n +# 15#) `andI#` notI# 15#
{-# INLINE roundUp16# #-}


{- | Reserve @n@ bytes in front of the write head. Returns the current
buffer and the index of the first reserved byte; the caller must
fill all @n@ bytes before the next 'claim' (which may move them).
-}
claim :: Builder -> Int# -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld, Int# #)
claim b@(Builder st arrs) n s0 = case readIntArray# st SlotHead s0 of
  (# s1, hd #)
    | isTrue# (n <=# hd) ->
        let !hd' = hd -# n
        in case writeIntArray# st SlotHead hd' s1 of
            s2 -> case readSmallArray# arrs ArrBuf s2 of
              (# s3, mba #) -> (# s3, mba, hd' #)
    | otherwise -> grow b n s1
{-# INLINE claim #-}


-- | Slow path of 'claim': reallocate so @n@ more bytes fit.
grow :: Builder -> Int# -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld, Int# #)
grow (Builder st arrs) n s0 =
  case readIntArray# st SlotHead s0 of
    (# s1, hd #) -> case readIntArray# st SlotCap s1 of
      (# s2, cap #) -> case readSmallArray# arrs ArrBuf s2 of
        (# s3, old #) ->
          let !used = cap -# hd
              !doubled = cap *# 2#
              !needed = used +# n +# cap
              !newCap = roundUp16# (if isTrue# (doubled >=# needed) then doubled else needed)
              !newHd = newCap -# used -# n
          in case newAlignedPinnedByteArray# newCap 16# s3 of
              (# s4, new #) -> case copyMutableByteArray# old hd new (newCap -# used) used s4 of
                s5 -> case writeSmallArray# arrs ArrBuf new s5 of
                  s6 -> case writeIntArray# st SlotHead newHd s6 of
                    s7 -> case writeIntArray# st SlotCap newCap s7 of
                      s8 -> (# s8, new, newHd #)
{-# NOINLINE grow #-}


-- | Bytes written so far, unboxed.
uoff# :: Builder -> State# RealWorld -> (# State# RealWorld, Int# #)
uoff# (Builder st _) s0 = case readIntArray# st SlotHead s0 of
  (# s1, hd #) -> case readIntArray# st SlotCap s1 of
    (# s2, cap #) -> (# s2, cap -# hd #)
{-# INLINE uoff# #-}


-- | Prepend @n@ zero bytes (no-op when @n <= 0@).
zeros# :: Builder -> Int# -> State# RealWorld -> State# RealWorld
zeros# b n s0
  | isTrue# (n <=# 0#) = s0
  | otherwise = case claim b n s0 of
      (# s1, mba, i #) -> zeroFill# mba i n s1
{-# INLINE zeros# #-}


{- | Zero @n@ bytes at @i@. Padding and table bodies are a few dozen
bytes at most, where a word loop beats an out-of-line @memset@ call.
-}
zeroFill# :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> State# RealWorld
zeroFill# mba i n s0
  | isTrue# (n ># 64#) = setByteArray# mba i n 0# s0
  | otherwise = go i s0
  where
    !e = i +# n
    go j s
      | isTrue# (j +# 8# <=# e) = go (j +# 8#) (writeWord8ArrayAsWord64# mba j (wordToWord64# 0##) s)
      | isTrue# (j <# e) = go (j +# 1#) (writeWord8Array# mba j (wordToWord8# 0##) s)
      | otherwise = s
{-# INLINE zeroFill# #-}


noteMinAlign# :: Builder -> Int# -> State# RealWorld -> State# RealWorld
noteMinAlign# (Builder st _) a s0 = case readIntArray# st SlotMinAlign s0 of
  (# s1, m #)
    | isTrue# (a ># m) -> writeIntArray# st SlotMinAlign a s1
    | otherwise -> s1
{-# INLINE noteMinAlign# #-}


prepForObject# :: Builder -> Int# -> Int# -> State# RealWorld -> State# RealWorld
prepForObject# b objSize objAlign s0 = case noteMinAlign# b objAlign s0 of
  s1 -> case uoff# b s1 of
    (# s2, cur #) -> zeros# b (negateInt# (cur +# objSize) `andI#` (objAlign -# 1#)) s2
{-# INLINE prepForObject# #-}


{- | Note an alignment requirement on the running @minAlign@. The final
buffer is padded to a multiple of the largest @minAlign@ seen, which
propagates the alignment guarantee to every sub-object.
-}
noteMinAlign :: Builder -> Int -> IO ()
noteMinAlign b (I# a) = IO $ \s -> (# noteMinAlign# b a s, () #)
{-# INLINE noteMinAlign #-}


{- | Pad /before/ writing an object of @objSize@ bytes with alignment
@objAlign@, so that after emission the object's UOffset satisfies
@uoff % objAlign == 0@ (which, combined with the finalisation
padding, makes its forward position aligned).
-}
prepForObject :: Builder -> Int -> Int -> IO ()
prepForObject b (I# objSize) (I# objAlign) = IO $ \s -> (# prepForObject# b objSize objAlign s, () #)
{-# INLINE prepForObject #-}


-- ============================================================
-- Little-endian stores
-- ============================================================

le16 :: Word16 -> Word16
le16 w = case targetByteOrder of
  LittleEndian -> w
  BigEndian -> byteSwap16 w
{-# INLINE le16 #-}


le32 :: Word32 -> Word32
le32 w = case targetByteOrder of
  LittleEndian -> w
  BigEndian -> byteSwap32 w
{-# INLINE le32 #-}


le64 :: Word64 -> Word64
le64 w = case targetByteOrder of
  LittleEndian -> w
  BigEndian -> byteSwap64 w
{-# INLINE le64 #-}


put16 :: MutableByteArray# RealWorld -> Int# -> Word16 -> State# RealWorld -> State# RealWorld
put16 mba i w = case le16 w of W16# x -> writeWord8ArrayAsWord16# mba i x
{-# INLINE put16 #-}


put32 :: MutableByteArray# RealWorld -> Int# -> Word32 -> State# RealWorld -> State# RealWorld
put32 mba i w = case le32 w of W32# x -> writeWord8ArrayAsWord32# mba i x
{-# INLINE put32 #-}


put64 :: MutableByteArray# RealWorld -> Int# -> Word64 -> State# RealWorld -> State# RealWorld
put64 mba i w = case le64 w of W64# x -> writeWord8ArrayAsWord64# mba i x
{-# INLINE put64 #-}


-- | 'put16' of an 'Int#' (low 16 bits).
putI16# :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> State# RealWorld
putI16# mba i x = put16 mba i (fromIntegral (I# x))
{-# INLINE putI16# #-}


-- | 'put32' of an 'Int#' (low 32 bits).
putI32# :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> State# RealWorld
putI32# mba i x = put32 mba i (fromIntegral (I# x))
{-# INLINE putI32# #-}


-- ============================================================
-- Prepend primitives
-- ============================================================

{- | Prepend raw bytes to the builder (they land before any
previously-written content).
-}
prependBS :: Builder -> ByteString -> IO ()
prependBS b (BSI.BS fp (I# n)) = unsafeWithForeignPtr fp $ \(Ptr src) -> IO $ \s0 ->
  case claim b n s0 of
    (# s1, mba, i #) -> (# copyAddrToByteArray# src mba i n s1, () #)
{-# INLINE prependBS #-}


-- | Prepend one little-endian primitive.
prependU8 :: Builder -> Word8 -> IO ()
prependU8 b (W8# w) = IO $ \s0 -> case claim b 1# s0 of
  (# s1, mba, i #) -> (# writeWord8Array# mba i w s1, () #)
{-# INLINE [1] prependU8 #-}


prependU16 :: Builder -> Word16 -> IO ()
prependU16 b w = IO $ \s0 -> case claim b 2# s0 of
  (# s1, mba, i #) -> (# put16 mba i w s1, () #)
{-# INLINE [1] prependU16 #-}


prependU32 :: Builder -> Word32 -> IO ()
prependU32 b w = IO $ \s0 -> case claim b 4# s0 of
  (# s1, mba, i #) -> (# put32 mba i w s1, () #)
{-# INLINE [1] prependU32 #-}


prependU64 :: Builder -> Word64 -> IO ()
prependU64 b w = IO $ \s0 -> case claim b 8# s0 of
  (# s1, mba, i #) -> (# put64 mba i w s1, () #)
{-# INLINE [1] prependU64 #-}


prependI16 :: Builder -> Int16 -> IO ()
prependI16 b i = prependU16 b (fromIntegral i)
{-# INLINE [1] prependI16 #-}


prependI32 :: Builder -> Int32 -> IO ()
prependI32 b i = prependU32 b (fromIntegral i)
{-# INLINE [1] prependI32 #-}


prependI64 :: Builder -> Int64 -> IO ()
prependI64 b i = prependU64 b (fromIntegral i)
{-# INLINE [1] prependI64 #-}


{- | Finalise the builder into a single ByteString, given the root
table's UOffset.

The buffer is padded so its final size is a multiple of
@max minAlign 4@. Every object's UOffset is already a multiple of
its own alignment and @forward_pos = final_size - uoff@, so every
forward position is aligned too. The result is a slice of the
builder's buffer (no copy).
-}
finish :: Builder -> Int -> IO ByteString
finish b@(Builder st _) (I# rootUOff) = IO $ \s0 ->
  case readIntArray# st SlotMinAlign s0 of
    (# s1, minA #) ->
      case prepForObject# b 4# (if isTrue# (minA ># 4#) then minA else 4#) s1 of
        s2 -> case claim b 4# s2 of
          (# s3, buf, i #) -> case readIntArray# st SlotCap s3 of
            (# s4, cap #) -> case putI32# buf i ((cap -# i) -# rootUOff) s4 of
              s5 -> (# s5, BSI.BS (ForeignPtr (mutableByteArrayContents# buf `plusAddr#` i) (PlainPtr buf)) (I# (cap -# i)) #)


{- | Current UOffset of the bytes about to be written (= bytes written
so far). After the caller emits an object, its UOffset is the value
at completion; its absolute position in the finished buffer is
@totalSize - UOffset@.
-}
currentUOff :: Builder -> IO Int
currentUOff b = IO $ \s0 -> case uoff# b s0 of (# s1, u #) -> (# s1, I# u #)
{-# INLINE currentUOff #-}


-- ============================================================
-- Tables / vtables
-- ============================================================

{- | A single field slot within a table, described by its on-disk size
(1, 2, 4, 8, or a struct's size, which is also its alignment; 4 for
VOffset fields) and how to write it.

Offsets ('voff') and fixed-width scalars ('scalarU8' ... 'scalarI64')
are plain data that 'writeTable' stores directly. Only 'scalar',
'struct' and the 'Field'' pattern carry a writer closure, which
'writeTable' has to call.
-}
data Field'
  = FieldFn {-# UNPACK #-} !Int !(Builder -> Int -> IO ())
  | FieldWord {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
  -- ^ size (1, 2, 4 or 8) and value; written little-endian
  | FieldOff {-# UNPACK #-} !Int
  -- ^ target UOffset


{- | View of any 'Field'' as its size and a writer. @fsWrite builder 0@
must prepend exactly @fsAlign@ bytes of the field's inline data; the
second argument is always 0 and is kept for compatibility.
Constructing with 'Field'' is equivalent to 'scalar' with a writer
that ignores the extra argument.
-}
pattern Field' :: Int -> (Builder -> Int -> IO ()) -> Field'
pattern Field' {fsAlign, fsWrite} <- (fieldView -> (fsAlign, fsWrite))
  where
    Field' a w = FieldFn a w

{-# COMPLETE Field' #-}


fieldView :: Field' -> (Int, Builder -> Int -> IO ())
fieldView = \case
  FieldFn a w -> (a, w)
  FieldWord a x -> (a, \b _ -> prependWord b a x)
  FieldOff t -> (4, \b _ -> prependVOff b t)


-- | On-disk size (and alignment) of a field.
fieldSize# :: Field' -> Int#
fieldSize# = \case
  FieldFn (I# a) _ -> a
  FieldWord (I# a) _ -> a
  FieldOff _ -> 4#
{-# INLINE fieldSize# #-}


{- | A scalar field written by an arbitrary writer of exactly @align@
bytes. The common shapes @scalar n (\\b -> prependX b v)@ are rewritten
to the closure-free 'scalarU8' ... 'scalarI64' (same bytes) by
rewrite rules, which is why 'scalar' and the prepends inline late.
-}
scalar :: Int -> (Builder -> IO ()) -> Field'
scalar align writer = FieldFn align (\b _ -> writer b)
{-# INLINE [1] scalar #-}

{-# RULES
"scalar/prependU8" forall v. scalar 1 (\b -> prependU8 b v) = scalarU8 v
"scalar/prependU16" forall v. scalar 2 (\b -> prependU16 b v) = scalarU16 v
"scalar/prependU32" forall v. scalar 4 (\b -> prependU32 b v) = scalarU32 v
"scalar/prependU64" forall v. scalar 8 (\b -> prependU64 b v) = scalarU64 v
"scalar/prependI16" forall v. scalar 2 (\b -> prependI16 b v) = scalarI16 v
"scalar/prependI32" forall v. scalar 4 (\b -> prependI32 b v) = scalarI32 v
"scalar/prependI64" forall v. scalar 8 (\b -> prependI64 b v) = scalarI64 v
  #-}


-- | Fixed-width scalar fields, stored without a writer closure.
scalarU8 :: Word8 -> Field'
scalarU8 w = FieldWord 1 (fromIntegral w)
{-# INLINE scalarU8 #-}


scalarU16 :: Word16 -> Field'
scalarU16 w = FieldWord 2 (fromIntegral w)
{-# INLINE scalarU16 #-}


scalarU32 :: Word32 -> Field'
scalarU32 w = FieldWord 4 (fromIntegral w)
{-# INLINE scalarU32 #-}


scalarU64 :: Word64 -> Field'
scalarU64 = FieldWord 8
{-# INLINE scalarU64 #-}


scalarI16 :: Int16 -> Field'
scalarI16 i = FieldWord 2 (fromIntegral (fromIntegral i :: Word16))
{-# INLINE scalarI16 #-}


scalarI32 :: Int32 -> Field'
scalarI32 i = FieldWord 4 (fromIntegral (fromIntegral i :: Word32))
{-# INLINE scalarI32 #-}


scalarI64 :: Int64 -> Field'
scalarI64 i = FieldWord 8 (fromIntegral i)
{-# INLINE scalarI64 #-}


{- | An inline struct field: writes @size@ bytes directly in the
table's inline data area. @writer@ must prepend exactly @size@ bytes
of struct data (in reverse field-declaration order, since the builder
is back-to-front).

'writeTable' treats the size as the alignment too, so structs are
over-aligned to @size@. That wastes some padding when @size > align@
(a 16-byte struct on 8-byte alignment is placed 16-aligned), which
readers accept.
-}
struct :: Int -> Int -> (Builder -> IO ()) -> Field'
struct size _align writer = FieldFn size (\b _ -> writer b)
{-# INLINE struct #-}


{- | A VOffset field: a u32 relative offset to an already-laid-out
sub-object at @targetUOff@.
-}
voff :: Int -> Field'
voff = FieldOff
{-# INLINE voff #-}


-- | Prepend a 1, 2, 4 or 8 byte little-endian value (the low bytes of @x@).
prependWord :: Builder -> Int -> Word64 -> IO ()
prependWord b (I# a) x = IO $ \s0 -> case claim b a s0 of
  (# s1, mba, i #) -> (# putWord# mba i a x s1, () #)


putWord# :: MutableByteArray# RealWorld -> Int# -> Int# -> Word64 -> State# RealWorld -> State# RealWorld
putWord# mba i a x = case a of
  1# -> case fromIntegral x of W8# w -> writeWord8Array# mba i w
  2# -> put16 mba i (fromIntegral x)
  4# -> put32 mba i (fromIntegral x)
  _ -> put64 mba i x
{-# INLINE putWord# #-}


{- | Prepend a u32 offset to @targetUOff@. The field's own UOffset
after the write is @cur + 4@, and a forward offset is
@field - target@ in UOffset terms.
-}
prependVOff :: Builder -> Int -> IO ()
prependVOff b (I# targetUOff) = IO $ \s0 -> case claim b 4# s0 of
  (# s1, mba, i #) -> case uoff# b s1 of
    (# s2, cur #) -> (# putI32# mba i (cur -# targetUOff) s2, () #)


{- | A table's forward-order on-disk layout is:

@
[vtable] [soffset_t i32] [slot 0] [slot 1] ... [slot N-1] [tail padding]
@

Present slots are laid out in declaration order starting right after
the soffset_t, each aligned to its own 'fsAlign'. The table size is
rounded up to the largest field alignment (at least 4). The vtable
records each present slot's offset from the soffset_t and 0 for
absent slots; trailing absent slots are dropped.

@soffset_t = table_pos - vtable_pos@. A fresh vtable is written
directly in front of the table (soffset = vtable size); a duplicate
of an earlier vtable is not written and the soffset points at the
earlier copy instead.

Returns the UOffset of the table start (the soffset_t).

Implementation: the slot list is walked once ('scanSlots'), computing
the layout and recording each slot's offset, size and data in an
unboxed scratch array. The vtable, soffset_t and table are then
reserved with a single 'claim' and filled from the scratch array
without touching the list again ('emitSlots'). Only writer closures
('scalar', 'struct', 'Field'') need a second walk ('runWriters'):
each runs with the write head parked just past its slot, so its
prepend lands at the slot's offset.
-}
writeTable :: Builder -> [Maybe Field'] -> IO Int
writeTable b@(Builder st arrs) slots = IO $ \s0 ->
  case readSmallArray# arrs ArrScratch s0 of
    (# s1, scr0 #) -> case scanSlots scr0 0# 4# 0# 4# 0# 0# slots s1 of
      (# s2, scr, rawEnd, maxAlign, nT, writers #) ->
        let !tableSize = alignUp# rawEnd maxAlign
            -- An all-absent table still has its soffset_t (its recorded
            -- size is 0, as it always was here).
            !region = if isTrue# (tableSize ># 4#) then tableSize else 4#
            !vtSize = 4# +# 2# *# nT
            -- Keep a regrown scratch array for the next table.
            !s3 = if isTrue# (sameMutableByteArray# scr scr0) then s2 else writeSmallArray# arrs ArrScratch scr s2
        in case prepForObject# b tableSize maxAlign s3 of
            s4 -> case claim b (vtSize +# region) s4 of
              (# s5, mba, i #) -> case readIntArray# st SlotCap s5 of
                (# s6, cap #) ->
                  -- vtable at @i@, soffset_t (table start) at @t@.
                  let !t = i +# vtSize
                      !tableUOff = cap -# t
                  in case zeroFill# mba (t +# 4#) (region -# 4#) s6 of
                      s7 -> case putI16# mba i vtSize s7 of
                        s8 -> case putI16# mba (i +# 2#) tableSize s8 of
                          s9 -> case emitSlots mba scr (i +# 4#) t tableUOff nT 0# s9 of
                            s10 ->
                              let !s11 = if isTrue# writers then runWriters b t nT 0# 4# slots s10 else s10
                              in dedupVTable b mba cap i t vtSize tableUOff s11


-- Scratch encoding, two 'Int's per slot: word 0 is 0 for an absent
-- slot, else @off .|. size << 32 .|. kind << 56@; word 1 is the data.
pattern KindWord, KindOff, KindFn :: Int#
pattern KindWord = 1#
pattern KindOff = 2#
pattern KindFn = 3#


{- | The single walk over the slot list: forward layout plus the
scratch record of every slot. Returns the (possibly regrown) scratch
array; @rawEnd@, the end of the last slot (0 if none is present);
the largest slot alignment (at least 4); @nT@, one past the index of
the last present slot; and 1 if any slot needs its writer closure.
-}
scanSlots
  :: MutableByteArray# RealWorld
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> [Maybe Field']
  -> State# RealWorld
  -> (# State# RealWorld, MutableByteArray# RealWorld, Int#, Int#, Int#, Int# #)
scanSlots scr idx pos rawEnd maxA nT writers xs s0 = case xs of
  [] -> (# s0, scr, rawEnd, maxA, nT, writers #)
  m : rest
    | isTrue# (16# *# idx +# 16# ># sizeofMutableByteArray# scr) -> case growScratch scr s0 of
        (# s1, scr' #) -> scanSlots scr' idx pos rawEnd maxA nT writers xs s1
    | otherwise -> case m of
        Nothing -> scanSlots scr (idx +# 1#) pos rawEnd maxA nT writers rest (writeIntArray# scr (2# *# idx) 0# s0)
        Just f -> case f of
          FieldWord (I# a) (W64# x) -> present rest a KindWord (word2Int# (word64ToWord# x)) writers
          FieldOff (I# target) -> present rest 4# KindOff target writers
          FieldFn (I# a) _ -> present rest a KindFn 0# 1#
  where
    present rest a kind payload writers' =
      let !off = alignUp# pos a
          !end = off +# a
      in case writeIntArray# scr (2# *# idx) (off `orI#` (a `uncheckedIShiftL#` 32#) `orI#` (kind `uncheckedIShiftL#` 56#)) s0 of
          s1 ->
            scanSlots
              scr
              (idx +# 1#)
              end
              (if isTrue# (end ># rawEnd) then end else rawEnd)
              (if isTrue# (a ># maxA) then a else maxA)
              (idx +# 1#)
              writers'
              rest
              (writeIntArray# scr (2# *# idx +# 1#) payload s1)
    {-# INLINE present #-}


growScratch :: MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
growScratch scr s0 =
  let !bytes = sizeofMutableByteArray# scr
  in case newByteArray# (bytes *# 2#) s0 of
      (# s1, scr' #) -> case copyMutableByteArray# scr 0# scr' 0# bytes s1 of
        s2 -> (# s2, scr' #)
{-# NOINLINE growScratch #-}


{- | Write the vtable entries (at @vt@) and the data slots of the table
at index @t@ (UOffset @tableUOff@) from the scratch records of slots
@[idx, nT)@. Writer-closure slots only get their vtable entry here.
-}
emitSlots
  :: MutableByteArray# RealWorld
  -> MutableByteArray# RealWorld
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> State# RealWorld
  -> State# RealWorld
emitSlots mba scr vt t tableUOff nT idx s0
  | isTrue# (idx >=# nT) = s0
  | otherwise = case readIntArray# scr (2# *# idx) s0 of
      (# s1, w #) ->
        let !off = w `andI#` 0xFFFFFFFF#
            !a = (w `uncheckedIShiftRL#` 32#) `andI#` 0xFFFFFF#
            !kind = w `uncheckedIShiftRL#` 56#
            !at = t +# off
            next s = emitSlots mba scr vt t tableUOff nT (idx +# 1#) s
            {-# INLINE next #-}
        in case putI16# mba (vt +# 2# *# idx) off s1 of
            s2 -> case kind of
              KindWord -> case readWord64Array# scr (2# *# idx +# 1#) s2 of
                (# s3, x #) -> next (putWord# mba at a (W64# x) s3)
              KindOff -> case readIntArray# scr (2# *# idx +# 1#) s2 of
                (# s3, target #) -> next (putI32# mba at ((tableUOff -# off) -# target) s3)
              _ -> next s2


{- | Run the writer closures of the table at index @t@: each with the
head parked at @t + off + size@, so its prepend lands at offset @off@.
-}
runWriters :: Builder -> Int# -> Int# -> Int# -> Int# -> [Maybe Field'] -> State# RealWorld -> State# RealWorld
runWriters b@(Builder st _) t nT idx pos xs s0
  | isTrue# (idx >=# nT) = s0
  | otherwise = case xs of
      [] -> s0
      Nothing : rest -> runWriters b t nT (idx +# 1#) pos rest s0
      Just f : rest ->
        let !a = fieldSize# f
            !off = alignUp# pos a
            next s = runWriters b t nT (idx +# 1#) (off +# a) rest s
            {-# INLINE next #-}
        in case f of
            FieldFn _ w -> case writeIntArray# st SlotHead (t +# off +# a) s0 of
              s1 -> case w b 0 of
                IO g -> case g s1 of (# s2, _ #) -> next s2
            _ -> next s0


{- | Finish a table whose candidate vtable sits at @i@ and soffset_t at
@t@: reuse a byte-equal earlier vtable (dropping the candidate) or
keep and record the candidate. Leaves the head at the table's first
byte and returns the table's UOffset.
-}
dedupVTable
  :: Builder
  -> MutableByteArray# RealWorld
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> Int#
  -> State# RealWorld
  -> (# State# RealWorld, Int #)
dedupVTable (Builder st arrs) mba cap i t vtSize tableUOff s0 =
  case readIntArray# st SlotNVT s0 of
    (# s1, nvt #) -> case readSmallArray# arrs ArrVTables s1 of
      (# s2, vts #) ->
        let
          search k s
            | isTrue# (k >=# nvt) = fresh s
            | otherwise = case readIntArray# vts k s of
                (# s', u #) ->
                  let !p = cap -# u
                  in case readWord8ArrayAsWord16# mba p s' of
                      (# s'', sz #)
                        | fromIntegral (le16 (W16# sz)) == I# vtSize ->
                            same k u (p +# 2#) (i +# 2#) (vtSize -# 2#) s''
                        | otherwise -> search (k +# 1#) s''

          -- Compare the remaining @n@ bytes of candidate @k@ (at @p@)
          -- against the new vtable (at @q@); @n@ is even.
          same k u p q n s
            | isTrue# (n >=# 8#) = case readWord8ArrayAsWord64# mba p s of
                (# s', x #) -> case readWord8ArrayAsWord64# mba q s' of
                  (# s'', y #)
                    | isTrue# (eqWord64# x y) -> same k u (p +# 8#) (q +# 8#) (n -# 8#) s''
                    | otherwise -> search (k +# 1#) s''
            | isTrue# (n >=# 2#) = case readWord8ArrayAsWord16# mba p s of
                (# s', x #) -> case readWord8ArrayAsWord16# mba q s' of
                  (# s'', y #)
                    | isTrue# (eqWord16# x y) -> same k u (p +# 2#) (q +# 2#) (n -# 2#) s''
                    | otherwise -> search (k +# 1#) s''
            | otherwise = reuse u s

          -- Drop the candidate and point the soffset_t at the old copy.
          reuse u s = case writeIntArray# st SlotHead t s of
            s' -> (# putI32# mba t (u -# tableUOff) s', I# tableUOff #)

          -- Keep the candidate, directly in front of the table.
          fresh s = case writeIntArray# st SlotHead i s of
            s' -> case putI32# mba t vtSize s' of
              s'' -> (# record s'', I# tableUOff #)

          record s
            | isTrue# (nvt <# sizeofMutableByteArray# vts `quotInt#` 8#) = case writeIntArray# vts nvt (cap -# i) s of
                s' -> writeIntArray# st SlotNVT (nvt +# 1#) s'
            | otherwise = case growVTables vts nvt (cap -# i) s of
                (# s', vts' #) -> case writeSmallArray# arrs ArrVTables vts' s' of
                  s'' -> writeIntArray# st SlotNVT (nvt +# 1#) s''
        in search 0# s2


-- | Copy the vtable registry into one twice the size and append @u@.
growVTables :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)
growVTables vts nvt u s0 =
  let !bytes = sizeofMutableByteArray# vts
  in case newByteArray# (bytes *# 2#) s0 of
      (# s1, vts' #) -> case copyMutableByteArray# vts 0# vts' 0# bytes s1 of
        s2 -> (# writeIntArray# vts' nvt u s2, vts' #)
{-# NOINLINE growVTables #-}


alignUp :: Int -> Int -> Int
alignUp n a = (n + a - 1) .&. complement (a - 1)
{-# INLINE alignUp #-}


alignUp# :: Int# -> Int# -> Int#
alignUp# n a = (n +# a -# 1#) `andI#` notI# (a -# 1#)
{-# INLINE alignUp# #-}


-- ============================================================
-- Strings, vectors, structs
-- ============================================================

{- | Emit a UTF-8 string: length (u32), bytes, NUL terminator. Returns
the UOffset where the /length/ field begins. The string object is
4-aligned.
-}
writeString :: Builder -> T.Text -> IO Int
writeString b (TI.Text (TA.ByteArray src) (I# off) (I# n)) = IO $ \s0 ->
  let !total = 4# +# n +# 1#
  in case prepForObject# b total 4# s0 of
      s1 -> case claim b total s1 of
        (# s2, mba, i #) -> case putI32# mba i n s2 of
          s3 -> case copyShort# src off mba (i +# 4#) n s3 of
            s4 -> case writeWord8Array# mba (i +# 4# +# n) (wordToWord8# 0##) s4 of
              s5 -> uoffBoxed b s5


{- | Copy @n@ bytes from an immutable array. Field names and metadata
keys are short, where a word loop beats an out-of-line @memcpy@ call.
-}
copyShort# :: ByteArray# -> Int# -> MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> State# RealWorld
copyShort# src off dst at n s0
  | isTrue# (n ># 64#) = copyByteArray# src off dst at n s0
  | otherwise = go 0# s0
  where
    go k s
      | isTrue# (k +# 8# <=# n) =
          go (k +# 8#) (writeWord8ArrayAsWord64# dst (at +# k) (indexWord8ArrayAsWord64# src (off +# k)) s)
      | isTrue# (k <# n) = go (k +# 1#) (writeWord8Array# dst (at +# k) (indexWord8Array# src (off +# k)) s)
      | otherwise = s
{-# INLINE copyShort# #-}


uoffBoxed :: Builder -> State# RealWorld -> (# State# RealWorld, Int #)
uoffBoxed b s0 = case uoff# b s0 of (# s1, u #) -> (# s1, I# u #)
{-# INLINE uoffBoxed #-}


-- | Emit a vector of UOffsets to previously-laid-out objects.
writeVectorOfOffsets :: Builder -> [Int] -> IO Int
writeVectorOfOffsets b targets = IO $ \s0 ->
  let !(I# n) = length targets
      !total = 4# +# 4# *# n
  in case prepForObject# b total 4# s0 of
      s1 -> case claim b total s1 of
        (# s2, mba, i #) -> case uoff# b s2 of
          (# s3, after #) -> case putI32# mba i n s3 of
            -- Element k sits at UOffset @after - 4 - 4k@.
            s4 -> case go mba (i +# 4#) (after -# 4#) targets s4 of
              s5 -> (# s5, I# after #)
  where
    go :: MutableByteArray# RealWorld -> Int# -> Int# -> [Int] -> State# RealWorld -> State# RealWorld
    go _ _ _ [] s = s
    go mba at u (I# t : ts) s = case putI32# mba at (u -# t) s of
      s' -> go mba (at +# 4#) (u -# 4#) ts s'


-- | Emit a vector of fixed-size inline structs.
writeVectorOfStructs
  :: Builder
  -> Int
  -- ^ per-struct size in bytes
  -> Int
  -- ^ struct alignment
  -> [Builder -> IO ()]
  -- ^ one writer per element, in forward order; each prepends exactly
  -- the struct size
  -> IO Int
writeVectorOfStructs b@(Builder st _) (I# elemSize) (I# elemAlign) writers = IO $ \s0 ->
  let !(I# n) = length writers
      !align = if isTrue# (elemAlign ># 4#) then elemAlign else 4#
      !total = 4# +# elemSize *# n
  in case prepForObject# b total align s0 of
      s1 -> case claim b total s1 of
        (# s2, mba, i #) -> case putI32# mba i n s2 of
          s3 -> case go (i +# 4# +# elemSize) writers s3 of
            -- The writers moved the head; park it back at the length.
            s4 -> case writeIntArray# st SlotHead i s4 of
              s5 -> uoffBoxed b s5
  where
    -- Element k occupies @[i+4+k*size, i+4+(k+1)*size)@: park the head
    -- at its end so the writer's prepends fill exactly that range.
    go _ [] s = s
    go end (w : ws) s = case writeIntArray# st SlotHead end s of
      s' -> case w b of
        IO f -> case f s' of (# s'', _ #) -> go (end +# elemSize) ws s''


-- | Emit a vector of signed int32 scalars.
writeVectorInt32 :: Builder -> [Int32] -> IO Int
writeVectorInt32 b xs = IO $ \s0 ->
  let !(I# n) = length xs
      !total = 4# +# 4# *# n
  in case prepForObject# b total 4# s0 of
      s1 -> case claim b total s1 of
        (# s2, mba, i #) -> case putI32# mba i n s2 of
          s3 -> case go mba (i +# 4#) xs s3 of
            s4 -> uoffBoxed b s4
  where
    go :: MutableByteArray# RealWorld -> Int# -> [Int32] -> State# RealWorld -> State# RealWorld
    go _ _ [] s = s
    go mba at (x : rest) s = case put32 mba at (fromIntegral x) s of
      s' -> go mba (at +# 4#) rest s'


{- | Emit a vector of signed int64 scalars.

Vector elements must start at a forward position that is a multiple
of the element size. The u32 length prefix sits 4 bytes before the
elements, so the builder pads until @currentUOff@ is a multiple of 8
before writing elements and length (see 'prepForVector').
-}
writeVectorInt64 :: Builder -> [Int64] -> IO Int
writeVectorInt64 b xs = IO $ \s0 ->
  let !(I# n) = length xs
  in case prepForVector# b 8# s0 of
      s1 -> case claim b (4# +# 8# *# n) s1 of
        (# s2, mba, i #) -> case putI32# mba i n s2 of
          s3 -> case go mba (i +# 4#) xs s3 of
            s4 -> uoffBoxed b s4
  where
    go :: MutableByteArray# RealWorld -> Int# -> [Int64] -> State# RealWorld -> State# RealWorld
    go _ _ [] s = s
    go mba at (x : rest) s = case put64 mba at (fromIntegral x) s of
      s' -> go mba (at +# 8#) rest s'


{- | Pad so that a vector with a 4-byte length prefix followed by
@align@-aligned elements (whose total size is a multiple of @align@)
lands with its elements at a forward position that is a multiple of
@align@: pad until @currentUOff ≡ 0 (mod align)@.
-}
prepForVector# :: Builder -> Int# -> State# RealWorld -> State# RealWorld
prepForVector# b align s0 = case noteMinAlign# b align s0 of
  s1 -> case uoff# b s1 of
    (# s2, cur #) -> zeros# b (negateInt# cur `andI#` (align -# 1#)) s2
{-# INLINE prepForVector# #-}
