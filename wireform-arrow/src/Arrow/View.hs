{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | View types that share the source buffer with their
-- producer rather than copying out of it.
--
-- All primitive numeric / temporal columns in
-- 'Arrow.Column.ColumnArray' already use 'VS.Vector' under
-- the hood, which is the natural primitive view (carries a
-- 'ForeignPtr', adopts source 'ByteString' bytes for free
-- via 'VS.unsafeFromForeignPtr').
--
-- This module defines:
--
--   * 'Utf8View' / 'BinaryView' / 'LargeUtf8View' /
--     'LargeBinaryView' — variable-length columns held as
--     (offsets buffer, data buffer) view pairs. Each pair
--     supports per-row access without re-materialising the
--     boxed 'V.Vector Text' / 'V.Vector ByteString' the
--     consumer might want.
--   * The 'View' typeclass, a small shared API
--     ('vLength' / 'vIndex' / 'vSlice' / 'vToList') so
--     polymorphic loops can treat a column-view the same
--     way regardless of element type.
--
-- The 'Arrow.Column.ColumnArray' constructors that hold
-- 'V.Vector Text' / 'V.Vector ByteString' (ColUtf8 /
-- ColBinary / ColLargeUtf8 / ColLargeBinary) keep their
-- current shape; conversion helpers below let callers reach
-- a 'Utf8View' from those when they need slice-view
-- semantics. A future revision (unit 3 of the views
-- migration) flips the constructors over.
module Arrow.View
  ( -- * View typeclass
    View (..)
    -- * Variable-length view types
  , Utf8View (..)
  , BinaryView (..)
  , LargeUtf8View (..)
  , LargeBinaryView (..)
    -- * Accessors
  , utf8At
  , binaryAt
  , largeUtf8At
  , largeBinaryAt
    -- * Materialisation
  , utf8ViewToVector
  , binaryViewToVector
  , largeUtf8ViewToVector
  , largeBinaryViewToVector
    -- * Construction
  , utf8ViewFromVector
  , binaryViewFromVector
  , utf8ViewFromVector_l
  , binaryViewFromVector_l
    -- * Nullable variable-length view types
  , NullableBoolView (..)
  , NullableUtf8View (..)
  , NullableBinaryView (..)
  , NullableLargeUtf8View (..)
  , NullableLargeBinaryView (..)
  , NullableFixedSizeBinaryView (..)
    -- ** Nullable construction
  , nullableBoolViewFromMaybeVector
  , nullableUtf8ViewFromMaybeVector
  , nullableBinaryViewFromMaybeVector
  , nullableLargeUtf8ViewFromMaybeVector
  , nullableLargeBinaryViewFromMaybeVector
  , nullableFixedSizeBinaryViewFromMaybeVector
    -- ** Nullable materialisation
  , nullableBoolViewToMaybeVector
  , nullableUtf8ViewToMaybeVector
  , nullableBinaryViewToMaybeVector
  , nullableLargeUtf8ViewToMaybeVector
  , nullableLargeBinaryViewToMaybeVector
  , nullableFixedSizeBinaryViewToMaybeVector
    -- ** Nullable accessors / counts
  , nullableBoolAt
  , nullableUtf8At
  , nullableBinaryAt
  , nbvLength
  , nbvNullCount
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)

import Columnar.Bit (Bit (..))

-- ============================================================
-- View typeclass
-- ============================================================

-- | Generic column-view interface: things you can ask the
-- length of, index into, slice, and walk.
--
-- The associated 'Element' type lets a polymorphic consumer
-- reach the right scalar shape without committing to a
-- concrete view type.
class View v where
  type Element v

  -- | Number of rows in the view.
  vLength :: v -> Int

  -- | Bounds-checked indexing.
  vIndex :: v -> Int -> Maybe (Element v)
  vIndex v i
    | i < 0 || i >= vLength v = Nothing
    | otherwise               = Just (vUnsafeIndex v i)
  {-# INLINE vIndex #-}

  -- | Caller guarantees 0 <= i < vLength v.
  vUnsafeIndex :: v -> Int -> Element v

  -- | Sub-view of @l@ rows starting at @s@. Caller guarantees
  -- 0 <= s and s + l <= vLength v.
  vSlice :: Int -> Int -> v -> v

  -- | Realise the view as a strict list. Mostly for tests
  -- and pretty-printing — production code should iterate
  -- directly via 'vUnsafeIndex' to avoid the spine
  -- allocation.
  vToList :: v -> [Element v]
  vToList v = [ vUnsafeIndex v i | i <- [0 .. vLength v - 1] ]
  {-# INLINE vToList #-}

instance VS.Storable a => View (VS.Vector a) where
  type Element (VS.Vector a) = a
  vLength      = VS.length
  vUnsafeIndex = VS.unsafeIndex
  vSlice       = VS.unsafeSlice
  vToList      = VS.toList
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}
  {-# INLINE vToList #-}

instance View (VU.Vector Bit) where
  type Element (VU.Vector Bit) = Bool
  vLength      = VU.length
  vUnsafeIndex v i = unBit (VU.unsafeIndex v i)
  vSlice       = VU.unsafeSlice
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}

instance View (V.Vector a) where
  type Element (V.Vector a) = a
  vLength      = V.length
  vUnsafeIndex = V.unsafeIndex
  vSlice       = V.unsafeSlice
  vToList      = V.toList
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}
  {-# INLINE vToList #-}

-- ============================================================
-- Utf8 / Binary views (Int32 offsets)
-- ============================================================

-- | Variable-length UTF-8 column held as the on-the-wire
-- representation: an @(N + 1)@-element offsets buffer and a
-- contiguous data buffer.
--
--   * @uvOffsets@: 'Int32' offsets into 'uvData', in bytes.
--     Always @N + 1@ entries — the last is the total size.
--   * @uvData@:   contiguous UTF-8 bytes. The slice for row
--     @i@ is @uvData[ uvOffsets[i] .. uvOffsets[i+1] )@.
--
-- Both fields are 'VS.Vector', so the entire view can adopt
-- a source 'ByteString''s 'ForeignPtr' without copying. Per-
-- row access produces a 'Text' that shares the underlying
-- bytes (no allocation beyond the small @Text@ constructor).
data Utf8View = Utf8View
  { uvOffsets :: !(VS.Vector Int32)
  , uvData    :: !(VS.Vector Word8)
  } deriving stock (Eq, Show)

instance View Utf8View where
  type Element Utf8View = Text
  vLength      v = max 0 (VS.length (uvOffsets v) - 1)
  vUnsafeIndex   = utf8At
  vSlice s l (Utf8View off dat) =
    Utf8View (VS.unsafeSlice s (l + 1) off) dat
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}

-- | Same shape as 'Utf8View' but the data buffer is raw
-- bytes (no UTF-8 validation).
data BinaryView = BinaryView
  { bvOffsets :: !(VS.Vector Int32)
  , bvData    :: !(VS.Vector Word8)
  } deriving stock (Eq, Show)

instance View BinaryView where
  type Element BinaryView = ByteString
  vLength      v = max 0 (VS.length (bvOffsets v) - 1)
  vUnsafeIndex   = binaryAt
  vSlice s l (BinaryView off dat) =
    BinaryView (VS.unsafeSlice s (l + 1) off) dat
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}

-- | 64-bit-offset variant of 'Utf8View' for columns whose
-- data buffer is > 2 GiB.
data LargeUtf8View = LargeUtf8View
  { luvOffsets :: !(VS.Vector Int64)
  , luvData    :: !(VS.Vector Word8)
  } deriving stock (Eq, Show)

instance View LargeUtf8View where
  type Element LargeUtf8View = Text
  vLength      v = max 0 (VS.length (luvOffsets v) - 1)
  vUnsafeIndex   = largeUtf8At
  vSlice s l (LargeUtf8View off dat) =
    LargeUtf8View (VS.unsafeSlice s (l + 1) off) dat
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}

-- | 64-bit-offset variant of 'BinaryView'.
data LargeBinaryView = LargeBinaryView
  { lbvOffsets :: !(VS.Vector Int64)
  , lbvData    :: !(VS.Vector Word8)
  } deriving stock (Eq, Show)

instance View LargeBinaryView where
  type Element LargeBinaryView = ByteString
  vLength      v = max 0 (VS.length (lbvOffsets v) - 1)
  vUnsafeIndex   = largeBinaryAt
  vSlice s l (LargeBinaryView off dat) =
    LargeBinaryView (VS.unsafeSlice s (l + 1) off) dat
  {-# INLINE vLength #-}
  {-# INLINE vUnsafeIndex #-}
  {-# INLINE vSlice #-}

-- ============================================================
-- Per-row access
-- ============================================================

{-# INLINE utf8At #-}
utf8At :: Utf8View -> Int -> Text
utf8At (Utf8View off dat) i =
  let !s   = fromIntegral (VS.unsafeIndex off i) :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
      !len = e - s
      slice = vsSliceToBytes dat s len
  in case TE.decodeUtf8' slice of
       Right t -> t
       Left _  -> TE.decodeUtf8With TEE.lenientDecode slice

{-# INLINE binaryAt #-}
binaryAt :: BinaryView -> Int -> ByteString
binaryAt (BinaryView off dat) i =
  let !s   = fromIntegral (VS.unsafeIndex off i) :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
  in vsSliceToBytes dat s (e - s)

{-# INLINE largeUtf8At #-}
largeUtf8At :: LargeUtf8View -> Int -> Text
largeUtf8At (LargeUtf8View off dat) i =
  let !s   = fromIntegral (VS.unsafeIndex off i) :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
      slice = vsSliceToBytes dat s (e - s)
  in case TE.decodeUtf8' slice of
       Right t -> t
       Left _  -> TE.decodeUtf8With TEE.lenientDecode slice

{-# INLINE largeBinaryAt #-}
largeBinaryAt :: LargeBinaryView -> Int -> ByteString
largeBinaryAt (LargeBinaryView off dat) i =
  let !s   = fromIntegral (VS.unsafeIndex off i) :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
  in vsSliceToBytes dat s (e - s)

-- | Materialise a slice of a @VS.Vector Word8@ as a strict
-- 'ByteString'. Copies the bytes into a fresh
-- @ByteString@'s 'ForeignPtr' (we don't have a way to
-- share the storable vector's ForeignPtr through the
-- 'BS.PS' constructor without re-introducing the same
-- storable vector internals — small follow-up).
{-# INLINE vsSliceToBytes #-}
vsSliceToBytes :: VS.Vector Word8 -> Int -> Int -> ByteString
vsSliceToBytes v start len
  | len <= 0  = BS.empty
  | otherwise = BS.pack [ VS.unsafeIndex v (start + i) | i <- [0 .. len - 1] ]

-- ============================================================
-- Materialisation back to boxed vectors (legacy callers)
-- ============================================================

utf8ViewToVector :: Utf8View -> V.Vector Text
utf8ViewToVector v = V.generate (vLength v) (vUnsafeIndex v)

binaryViewToVector :: BinaryView -> V.Vector ByteString
binaryViewToVector v = V.generate (vLength v) (vUnsafeIndex v)

largeUtf8ViewToVector :: LargeUtf8View -> V.Vector Text
largeUtf8ViewToVector v = V.generate (vLength v) (vUnsafeIndex v)

largeBinaryViewToVector :: LargeBinaryView -> V.Vector ByteString
largeBinaryViewToVector v = V.generate (vLength v) (vUnsafeIndex v)

-- ============================================================
-- Construction from boxed vectors
-- ============================================================

-- | Build a 'Utf8View' from a boxed 'V.Vector' of 'Text'.
-- Single pass: walk once to compute the offsets buffer and
-- the total data size, then allocate the data buffer once
-- and copy each value's bytes in.
utf8ViewFromVector :: V.Vector Text -> Utf8View
utf8ViewFromVector v =
  let !bss = V.map TE.encodeUtf8 v
  in binaryViewToUtf8View (binaryViewFromVector bss)

-- | Build a 'BinaryView' from a boxed 'V.Vector' of
-- 'ByteString'. Same single-pass shape.
binaryViewFromVector :: V.Vector ByteString -> BinaryView
binaryViewFromVector v =
  let !n = V.length v
      offs = VS.generate (n + 1) $ \i ->
        if i == 0 then 0
        else fromIntegral
               (V.foldl' (\acc bs -> acc + BS.length bs) 0
                         (V.take i v)) :: Int32
      payload = BS.concat (V.toList v)
  in BinaryView offs (bsToStorable payload)

binaryViewToUtf8View :: BinaryView -> Utf8View
binaryViewToUtf8View (BinaryView off dat) = Utf8View off dat

-- | Like 'utf8ViewFromVector' but builds a 'LargeUtf8View'
-- (Int64 offsets).
utf8ViewFromVector_l :: V.Vector Text -> LargeUtf8View
utf8ViewFromVector_l v =
  let !bss = V.map TE.encodeUtf8 v
  in binaryViewToLargeUtf8View (binaryViewFromVector_l bss)

-- | Like 'binaryViewFromVector' but builds a 'LargeBinaryView'
-- (Int64 offsets).
binaryViewFromVector_l :: V.Vector ByteString -> LargeBinaryView
binaryViewFromVector_l v =
  let !n = V.length v
      offs = VS.generate (n + 1) $ \i ->
        if i == 0 then 0
        else fromIntegral
               (V.foldl' (\acc bs -> acc + BS.length bs) 0
                         (V.take i v)) :: Int64
      payload = BS.concat (V.toList v)
  in LargeBinaryView offs (bsToStorable payload)

binaryViewToLargeUtf8View :: LargeBinaryView -> LargeUtf8View
binaryViewToLargeUtf8View (LargeBinaryView off dat) = LargeUtf8View off dat

-- ============================================================
-- Nullable view types
-- ============================================================

-- | Nullable Bool column: validity bitmap + values bitmap.
-- Each is @ceil(N / 8)@ bytes — the wire format of an Arrow
-- nullable BOOL column directly.
data NullableBoolView = NullableBoolView
  { nbvValidity :: !(VU.Vector Bit)
  , nbvValues   :: !(VU.Vector Bit)
  } deriving stock (Eq, Show)

-- | Nullable UTF-8 column: validity bitmap + dense Utf8View.
-- Null slots in 'nuvValues' carry whatever was on the wire
-- (typically the empty string); consumers must consult
-- 'nuvValidity' before reading.
data NullableUtf8View = NullableUtf8View
  { nuvValidity :: !(VU.Vector Bit)
  , nuvValues   :: !Utf8View
  } deriving stock (Eq, Show)

data NullableBinaryView = NullableBinaryView
  { nbvBinValidity :: !(VU.Vector Bit)
  , nbvBinValues   :: !BinaryView
  } deriving stock (Eq, Show)

data NullableLargeUtf8View = NullableLargeUtf8View
  { nluvValidity :: !(VU.Vector Bit)
  , nluvValues   :: !LargeUtf8View
  } deriving stock (Eq, Show)

data NullableLargeBinaryView = NullableLargeBinaryView
  { nlbvValidity :: !(VU.Vector Bit)
  , nlbvValues   :: !LargeBinaryView
  } deriving stock (Eq, Show)

-- | Nullable fixed-size binary column: width + validity
-- bitmap + dense values vector. Null slots carry @width@
-- bytes of whatever was on the wire (typically zero).
data NullableFixedSizeBinaryView = NullableFixedSizeBinaryView
  { nfsbvWidth    :: !Int
  , nfsbvValidity :: !(VU.Vector Bit)
  , nfsbvValues   :: !(V.Vector ByteString)
  } deriving stock (Eq, Show)

-- ----------------------------------------------------------------
-- Common length / nullcount helpers
-- ----------------------------------------------------------------

{-# INLINE nbvLength #-}
nbvLength :: NullableBoolView -> Int
nbvLength = VU.length . nbvValidity

{-# INLINE nbvNullCount #-}
nbvNullCount :: NullableBoolView -> Int
nbvNullCount nv =
  let !v = nbvValidity nv
  in VU.length v
       - VU.foldl' (\acc (Bit b) -> if b then acc + 1 else acc) 0 v

-- ----------------------------------------------------------------
-- Nullable accessors
-- ----------------------------------------------------------------

{-# INLINE nullableBoolAt #-}
nullableBoolAt :: NullableBoolView -> Int -> Maybe Bool
nullableBoolAt nv i
  | not (validBit (nbvValidity nv) i) = Nothing
  | otherwise = Just (unBit (VU.unsafeIndex (nbvValues nv) i))

{-# INLINE nullableUtf8At #-}
nullableUtf8At :: NullableUtf8View -> Int -> Maybe Text
nullableUtf8At nv i
  | not (validBit (nuvValidity nv) i) = Nothing
  | otherwise = Just (utf8At (nuvValues nv) i)

{-# INLINE nullableBinaryAt #-}
nullableBinaryAt :: NullableBinaryView -> Int -> Maybe ByteString
nullableBinaryAt nv i
  | not (validBit (nbvBinValidity nv) i) = Nothing
  | otherwise = Just (binaryAt (nbvBinValues nv) i)

{-# INLINE validBit #-}
validBit :: VU.Vector Bit -> Int -> Bool
validBit v i
  | VU.null v = True
  | otherwise = unBit (VU.unsafeIndex v i)

-- ----------------------------------------------------------------
-- Construction / materialisation
-- ----------------------------------------------------------------

nullableBoolViewFromMaybeVector :: V.Vector (Maybe Bool) -> NullableBoolView
nullableBoolViewFromMaybeVector v =
  let !n = V.length v
  in NullableBoolView
       { nbvValidity = VU.generate n $ \i ->
           case V.unsafeIndex v i of
             Just _  -> Bit True
             Nothing -> Bit False
       , nbvValues   = VU.generate n $ \i ->
           case V.unsafeIndex v i of
             Just b  -> Bit b
             Nothing -> Bit False
       }

nullableUtf8ViewFromMaybeVector :: V.Vector (Maybe Text) -> NullableUtf8View
nullableUtf8ViewFromMaybeVector v =
  let !validity = VU.generate (V.length v) $ \i ->
        case V.unsafeIndex v i of
          Just _  -> Bit True
          Nothing -> Bit False
      !densified = V.map (maybe T.empty id) v
  in NullableUtf8View
       { nuvValidity = validity
       , nuvValues   = utf8ViewFromVector densified
       }

nullableBinaryViewFromMaybeVector
  :: V.Vector (Maybe ByteString) -> NullableBinaryView
nullableBinaryViewFromMaybeVector v =
  let !validity = VU.generate (V.length v) $ \i ->
        case V.unsafeIndex v i of
          Just _  -> Bit True
          Nothing -> Bit False
      !densified = V.map (maybe BS.empty id) v
  in NullableBinaryView
       { nbvBinValidity = validity
       , nbvBinValues   = binaryViewFromVector densified
       }

nullableLargeUtf8ViewFromMaybeVector
  :: V.Vector (Maybe Text) -> NullableLargeUtf8View
nullableLargeUtf8ViewFromMaybeVector v =
  let !validity = VU.generate (V.length v) $ \i ->
        case V.unsafeIndex v i of
          Just _  -> Bit True
          Nothing -> Bit False
      !densified = V.map (maybe T.empty id) v
  in NullableLargeUtf8View
       { nluvValidity = validity
       , nluvValues   = utf8ViewFromVector_l densified
       }

nullableLargeBinaryViewFromMaybeVector
  :: V.Vector (Maybe ByteString) -> NullableLargeBinaryView
nullableLargeBinaryViewFromMaybeVector v =
  let !validity = VU.generate (V.length v) $ \i ->
        case V.unsafeIndex v i of
          Just _  -> Bit True
          Nothing -> Bit False
      !densified = V.map (maybe BS.empty id) v
  in NullableLargeBinaryView
       { nlbvValidity = validity
       , nlbvValues   = binaryViewFromVector_l densified
       }

nullableFixedSizeBinaryViewFromMaybeVector
  :: Int -> V.Vector (Maybe ByteString) -> NullableFixedSizeBinaryView
nullableFixedSizeBinaryViewFromMaybeVector w v =
  let !validity = VU.generate (V.length v) $ \i ->
        case V.unsafeIndex v i of
          Just _  -> Bit True
          Nothing -> Bit False
      !densified = V.map (maybe (BS.replicate w 0) id) v
  in NullableFixedSizeBinaryView
       { nfsbvWidth    = w
       , nfsbvValidity = validity
       , nfsbvValues   = densified
       }

nullableBoolViewToMaybeVector :: NullableBoolView -> V.Vector (Maybe Bool)
nullableBoolViewToMaybeVector nv =
  V.generate (nbvLength nv) (nullableBoolAt nv)

nullableUtf8ViewToMaybeVector :: NullableUtf8View -> V.Vector (Maybe Text)
nullableUtf8ViewToMaybeVector nv =
  V.generate (vLength (nuvValues nv)) (nullableUtf8At nv)

nullableBinaryViewToMaybeVector :: NullableBinaryView -> V.Vector (Maybe ByteString)
nullableBinaryViewToMaybeVector nv =
  V.generate (vLength (nbvBinValues nv)) (nullableBinaryAt nv)

nullableLargeUtf8ViewToMaybeVector
  :: NullableLargeUtf8View -> V.Vector (Maybe Text)
nullableLargeUtf8ViewToMaybeVector nv =
  let !len = vLength (nluvValues nv)
  in V.generate len $ \i ->
       if not (validBit (nluvValidity nv) i)
         then Nothing
         else Just (largeUtf8At (nluvValues nv) i)

nullableLargeBinaryViewToMaybeVector
  :: NullableLargeBinaryView -> V.Vector (Maybe ByteString)
nullableLargeBinaryViewToMaybeVector nv =
  let !len = vLength (nlbvValues nv)
  in V.generate len $ \i ->
       if not (validBit (nlbvValidity nv) i)
         then Nothing
         else Just (largeBinaryAt (nlbvValues nv) i)

nullableFixedSizeBinaryViewToMaybeVector
  :: NullableFixedSizeBinaryView -> V.Vector (Maybe ByteString)
nullableFixedSizeBinaryViewToMaybeVector nv =
  let !values = nfsbvValues nv
  in V.generate (V.length values) $ \i ->
       if not (validBit (nfsbvValidity nv) i)
         then Nothing
         else Just (V.unsafeIndex values i)

-- | Helper: copy a 'ByteString' into a 'VS.Vector Word8'.
-- Used by the 'fromVector' constructors above.
bsToStorable :: ByteString -> VS.Vector Word8
bsToStorable bs = VS.generate (BS.length bs) (BS.index bs)
