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
  , mkUtf8View
  , mkLargeUtf8View
  , binaryViewToUtf8View
  , nullableBinaryToUtf8View
    -- * Buffer reinterpretation
  , vsSliceToBytes
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
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Internal as TI
import qualified Data.Vector as V
import Control.Monad.ST (runST)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
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
-- contiguous data buffer, plus a /lazily-validated/ shared
-- 'TA.Array' that lets per-row access skip per-element UTF-8
-- validation.
--
--   * @uvOffsets@: 'Int32' offsets into 'uvData', in bytes.
--     Always @N + 1@ entries — the last is the total size.
--   * @uvData@:   contiguous UTF-8 bytes. The slice for row
--     @i@ is @uvData[ uvOffsets[i] .. uvOffsets[i+1] )@.
--   * @uvText@:   lazy. On first access, the entire @uvData@
--     buffer is decoded once with the lenient UTF-8 decoder
--     and the resulting 'TA.Array' is cached. Per-row reads
--     then do @TI.text uvText offset length@ — a small
--     constructor, no validation, no allocation. For all-
--     ASCII data the decoder shares the source bytes, so the
--     cache is a no-copy view.
--
-- Eq / Show ignore the lazy 'uvText' cache (it's a function
-- of @uvData@); two views compare equal iff their offsets and
-- data buffers compare equal.
data Utf8View = Utf8View
  { uvOffsets :: !(VS.Vector Int32)
  , uvData    :: !(VS.Vector Word8)
  , uvText    :: TA.Array
    -- ^ /Lazy/. Cached pre-validated text array; computed
    -- once on first per-row access via 'utf8At'.
  }

instance Eq Utf8View where
  Utf8View o1 d1 _ == Utf8View o2 d2 _ = o1 == o2 && d1 == d2

instance Show Utf8View where
  show (Utf8View o d _) = "Utf8View { uvOffsets = " ++ show o
    ++ ", uvData = " ++ show d ++ ", uvText = <lazy> }"

instance View Utf8View where
  type Element Utf8View = Text
  vLength      v = max 0 (VS.length (uvOffsets v) - 1)
  vUnsafeIndex   = utf8At
  -- Slicing carves the offsets buffer; the data buffer +
  -- text cache stay shared (slicing does not change which
  -- bytes are valid). Subsequent index calls go through the
  -- (now narrower) offsets and read into the same text array.
  vSlice s l (Utf8View off dat arr) =
    Utf8View (VS.unsafeSlice s (l + 1) off) dat arr
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
-- data buffer is > 2 GiB. Same lazy-text cache.
data LargeUtf8View = LargeUtf8View
  { luvOffsets :: !(VS.Vector Int64)
  , luvData    :: !(VS.Vector Word8)
  , luvText    :: TA.Array
  }

instance Eq LargeUtf8View where
  LargeUtf8View o1 d1 _ == LargeUtf8View o2 d2 _ = o1 == o2 && d1 == d2

instance Show LargeUtf8View where
  show (LargeUtf8View o d _) = "LargeUtf8View { luvOffsets = " ++ show o
    ++ ", luvData = " ++ show d ++ ", luvText = <lazy> }"

instance View LargeUtf8View where
  type Element LargeUtf8View = Text
  vLength      v = max 0 (VS.length (luvOffsets v) - 1)
  vUnsafeIndex   = largeUtf8At
  vSlice s l (LargeUtf8View off dat arr) =
    LargeUtf8View (VS.unsafeSlice s (l + 1) off) dat arr
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

-- | Per-row UTF-8 access. Pulls the @(offset, length)@ pair
-- from the offsets buffer and constructs a 'Text' that
-- shares the pre-validated 'TA.Array' cached on the view.
-- Zero allocation beyond the small 'Text' constructor (3
-- machine words). UTF-8 validation runs once per view, on
-- first call to any row.
{-# INLINE utf8At #-}
utf8At :: Utf8View -> Int -> Text
utf8At (Utf8View off _ arr) i =
  let !s   = fromIntegral (VS.unsafeIndex off i)       :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
      !len = e - s
  in TI.text arr s len

{-# INLINE binaryAt #-}
binaryAt :: BinaryView -> Int -> ByteString
binaryAt (BinaryView off dat) i =
  let !s   = fromIntegral (VS.unsafeIndex off i) :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
  in vsSliceToBytes dat s (e - s)

{-# INLINE largeUtf8At #-}
largeUtf8At :: LargeUtf8View -> Int -> Text
largeUtf8At (LargeUtf8View off _ arr) i =
  let !s   = fromIntegral (VS.unsafeIndex off i)       :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
      !len = e - s
  in TI.text arr s len

{-# INLINE largeBinaryAt #-}
largeBinaryAt :: LargeBinaryView -> Int -> ByteString
largeBinaryAt (LargeBinaryView off dat) i =
  let !s   = fromIntegral (VS.unsafeIndex off i) :: Int
      !e   = fromIntegral (VS.unsafeIndex off (i + 1)) :: Int
  in vsSliceToBytes dat s (e - s)

-- | O(1) reinterpretation of a slice of a @VS.Vector Word8@ as
-- a strict 'ByteString'. Both representations are a
-- 'ForeignPtr' + length pair underneath; we just hand the
-- vector's underlying foreign pointer to 'BSI.fromForeignPtr'
-- with the slice offset baked in. Lifetime is tied to the
-- vector's foreign pointer.
{-# INLINE vsSliceToBytes #-}
vsSliceToBytes :: VS.Vector Word8 -> Int -> Int -> ByteString
vsSliceToBytes v start len
  | len <= 0  = BS.empty
  | otherwise =
      let !(fp, off, _) = VS.unsafeToForeignPtr v
      in BSI.fromForeignPtr fp (off + start) len

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
-- and copy each value's bytes in. Pre-populates the lazy
-- text cache via 'mkUtf8View'.
utf8ViewFromVector :: V.Vector Text -> Utf8View
utf8ViewFromVector v =
  let !bss = V.map TE.encodeUtf8 v
      !bv  = binaryViewFromVector bss
  in mkUtf8View (bvOffsets bv) (bvData bv)

-- | Smart constructor: build a 'Utf8View' from prepared
-- offsets + data buffers, populating the lazy text-array
-- cache. Same buffers in / out — the only added work is the
-- thunk that decodes the data buffer the first time
-- per-row access is invoked.
{-# INLINE mkUtf8View #-}
mkUtf8View :: VS.Vector Int32 -> VS.Vector Word8 -> Utf8View
mkUtf8View offs dat =
  Utf8View
    { uvOffsets = offs
    , uvData    = dat
    , uvText    = vsToTextArray dat
    }

{-# INLINE mkLargeUtf8View #-}
mkLargeUtf8View :: VS.Vector Int64 -> VS.Vector Word8 -> LargeUtf8View
mkLargeUtf8View offs dat =
  LargeUtf8View
    { luvOffsets = offs
    , luvData    = dat
    , luvText    = vsToTextArray dat
    }

-- | Decode a 'VS.Vector Word8' as one big lenient-decoded
-- 'Text' and return its underlying array. For valid all-
-- ASCII UTF-8 the underlying array shares the source bytes;
-- for general UTF-8 with replacement characters it's a
-- single per-buffer copy. Either way, /one/ validation walk
-- replaces the per-row 'TE.decodeUtf8'' calls the previous
-- shape paid.
{-# INLINE vsToTextArray #-}
vsToTextArray :: VS.Vector Word8 -> TA.Array
vsToTextArray dat =
  let !bs = vsSliceToBytes dat 0 (VS.length dat)
      !(TI.Text arr _off _len) =
        TE.decodeUtf8With TEE.lenientDecode bs
  in arr

-- | Build a 'BinaryView' from a boxed 'V.Vector' of
-- 'ByteString'. Same single-pass shape.
-- | Build a 'BinaryView' from a boxed vector of 'ByteString's.
--
-- Offsets are computed via a single cumulative-sum pass into a
-- pre-sized mutable storable vector (O(N), one allocation).
-- The previous shape was 'VS.generate (n+1) $ \\i -> ... (V.take i v) ...'
-- which is O(N^2) -- each offset re-folded the prefix.
--
-- Data is concatenated with 'BS.concat', then reinterpreted
-- zero-copy as a 'VS.Vector Word8' via 'bsToStorable'.
binaryViewFromVector :: V.Vector ByteString -> BinaryView
binaryViewFromVector v =
  let !n = V.length v
      !offs = runST $ do
        mv <- VSM.unsafeNew (n + 1)
        VSM.unsafeWrite mv 0 0
        let go !i !cur
              | i >= n = pure ()
              | otherwise = do
                  let !cur' = cur + BS.length (V.unsafeIndex v i)
                  VSM.unsafeWrite mv (i + 1) (fromIntegral cur' :: Int32)
                  go (i + 1) cur'
        go 0 (0 :: Int)
        VS.unsafeFreeze mv
      !payload = BS.concat (V.toList v)
  in BinaryView offs (bsToStorable payload)

binaryViewToUtf8View :: BinaryView -> Utf8View
binaryViewToUtf8View (BinaryView off dat) = mkUtf8View off dat

-- | Zero-copy reinterpretation of a 'NullableBinaryView' as a
-- 'NullableUtf8View'. Both wrap the same offsets + data
-- buffers; only the per-row decode path differs ('utf8At'
-- vs 'binaryAt'). UTF-8 validation runs lazily on first use
-- via the 'Utf8View' text-array cache. Used by the Parquet
-- bridge so we don't need a separate UTF-8 NV reader.
{-# INLINE nullableBinaryToUtf8View #-}
nullableBinaryToUtf8View :: NullableBinaryView -> NullableUtf8View
nullableBinaryToUtf8View (NullableBinaryView v b) =
  NullableUtf8View v (binaryViewToUtf8View b)

-- | Like 'utf8ViewFromVector' but builds a 'LargeUtf8View'
-- (Int64 offsets).
utf8ViewFromVector_l :: V.Vector Text -> LargeUtf8View
utf8ViewFromVector_l v =
  let !bss = V.map TE.encodeUtf8 v
      !lbv = binaryViewFromVector_l bss
  in mkLargeUtf8View (lbvOffsets lbv) (lbvData lbv)

-- | Like 'binaryViewFromVector' but builds a 'LargeBinaryView'
-- (Int64 offsets).
-- | Int64-offset variant. Same single-pass cumulative-sum
-- shape as 'binaryViewFromVector'; see that comment for why.
binaryViewFromVector_l :: V.Vector ByteString -> LargeBinaryView
binaryViewFromVector_l v =
  let !n = V.length v
      !offs = runST $ do
        mv <- VSM.unsafeNew (n + 1)
        VSM.unsafeWrite mv 0 0
        let go !i !cur
              | i >= n = pure ()
              | otherwise = do
                  let !cur' = cur + BS.length (V.unsafeIndex v i)
                  VSM.unsafeWrite mv (i + 1) (fromIntegral cur' :: Int64)
                  go (i + 1) cur'
        go 0 (0 :: Int)
        VS.unsafeFreeze mv
      !payload = BS.concat (V.toList v)
  in LargeBinaryView offs (bsToStorable payload)

binaryViewToLargeUtf8View :: LargeBinaryView -> LargeUtf8View
binaryViewToLargeUtf8View (LargeBinaryView off dat) = mkLargeUtf8View off dat

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
-- | Zero-copy reinterpretation of a 'ByteString' as a
-- 'VS.Vector Word8'. Both wrap a 'ForeignPtr Word8' + offset +
-- length internally; the only difference is which API you
-- call. Lifetime is tied to the source bytestring's foreign
-- pointer (so callers should keep the bytestring alive while
-- they use the vector).
--
-- The previous version was a per-byte 'VS.generate (BS.length bs) (BS.index bs)'
-- loop -- O(N) memory allocation + per-byte BS.index overhead.
bsToStorable :: ByteString -> VS.Vector Word8
bsToStorable bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in VS.unsafeFromForeignPtr fp off len
