{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Representation of "Arrow.Vector": the data family instances with
their constructors, the classes, and the bit and var-length helpers the
column conversions in "Arrow.Column" build on.

The constructors carry invariants the instances rely on (bitmaps cover
their bit range, every var-length row range lies inside its store).
Code outside this package should use "Arrow.Vector" and the generic
vector API instead.

Layouts:

* fixed-width element types: a storable vector ('VS.Vector'), so a
  decoded column converts by aliasing its value buffer;
* 'Bool': an LSB-first bitmap with a bit offset;
* @'Maybe' a@: a validity bitmap with a bit offset plus a @'Vector' a@
  whose null slots hold unspecified values;
* 'Text', 'ByteString' and @'Vector' a@ elements: per-row start and end
  offsets into one store (a text array, a byte buffer or a child
  vector), so indexing is an O(1) slice.
-}
module Arrow.Vector.Internal (
  -- * Families
  Vector (..),
  MVector (..),
  Element,
  FixedWidth (..),
  maybeValues,

  -- * Var-length rows
  VarElem (..),
  VarVec (..),
  MVarVec (..),

  -- * Bits
  bitBytes,
  indexBit,
  allSetBits,
  bytesToBits,
  bitsToBytes,
) where

import Arrow.Column.Buffer (Decimal128, Decimal256, Float16, IntervalDayTime, IntervalMonthDayNano)
import Columnar.SIMD qualified as K
import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits (complement, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray (
  ByteArray (..),
  MutableByteArray,
  copyByteArray,
  copyMutableByteArray,
  getSizeofMutableByteArray,
  newByteArray,
  sizeofByteArray,
  unsafeFreezeByteArray,
  unsafeThawByteArray,
 )
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Vector.Fusion.Util (Box (..))
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, plusForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes, unsafeWithForeignPtr)


-- ============================================================
-- Families and classes
-- ============================================================

-- | Immutable vectors in Arrow layout, indexed by element type.
data family Vector a


-- | Mutable vectors in Arrow layout.
data family MVector s a


type instance G.Mutable Vector = MVector


{- | Element types with a "Arrow.Vector" layout (the analogue of
@Data.Vector.Unboxed.Unbox@).
-}
class (G.Vector Vector a, GM.MVector MVector a) => Element a


{- | Fixed-width element types: the vector is a storable vector, and
both conversions are O(1).
-}
class (Element a, Storable a) => FixedWidth a where
  -- | O(1): view a storable vector as a 'Vector'.
  fromStorable :: VS.Vector a -> Vector a

  -- | O(1): the storable vector behind a 'Vector'.
  storableVector :: Vector a -> VS.Vector a


instance (Show a, Element a) => Show (Vector a) where
  showsPrec = G.showsPrec


instance (Eq a, Element a) => Eq (Vector a) where
  (==) = G.eq
  {-# INLINE (==) #-}


instance (Ord a, Element a) => Ord (Vector a) where
  compare = G.cmp
  {-# INLINE compare #-}


-- | Every layout is strict all the way down, so WHNF is normal form.
instance NFData (Vector a) where
  rnf v = v `seq` ()


instance Element a => Semigroup (Vector a) where
  (<>) = (G.++)
  {-# INLINE (<>) #-}


instance Element a => Monoid (Vector a) where
  mempty = G.empty
  {-# INLINE mempty #-}


-- ============================================================
-- Fixed-width element types
-- ============================================================

#define FIXED(ty,con,mcon) \
newtype instance MVector s (ty) = mcon (VSM.MVector s (ty)); \
newtype instance Vector (ty) = con (VS.Vector (ty)); \
instance GM.MVector MVector (ty) where { \
  {-# INLINE basicLength #-} \
; {-# INLINE basicUnsafeSlice #-} \
; {-# INLINE basicOverlaps #-} \
; {-# INLINE basicUnsafeNew #-} \
; {-# INLINE basicInitialize #-} \
; {-# INLINE basicUnsafeReplicate #-} \
; {-# INLINE basicUnsafeRead #-} \
; {-# INLINE basicUnsafeWrite #-} \
; {-# INLINE basicClear #-} \
; {-# INLINE basicSet #-} \
; {-# INLINE basicUnsafeCopy #-} \
; {-# INLINE basicUnsafeMove #-} \
; {-# INLINE basicUnsafeGrow #-} \
; basicLength (mcon v) = GM.basicLength v \
; basicUnsafeSlice i n (mcon v) = mcon (GM.basicUnsafeSlice i n v) \
; basicOverlaps (mcon a) (mcon b) = GM.basicOverlaps a b \
; basicUnsafeNew n = mcon <$> GM.basicUnsafeNew n \
; basicInitialize (mcon v) = GM.basicInitialize v \
; basicUnsafeReplicate n x = mcon <$> GM.basicUnsafeReplicate n x \
; basicUnsafeRead (mcon v) i = GM.basicUnsafeRead v i \
; basicUnsafeWrite (mcon v) i x = GM.basicUnsafeWrite v i x \
; basicClear (mcon v) = GM.basicClear v \
; basicSet (mcon v) x = GM.basicSet v x \
; basicUnsafeCopy (mcon a) (mcon b) = GM.basicUnsafeCopy a b \
; basicUnsafeMove (mcon a) (mcon b) = GM.basicUnsafeMove a b \
; basicUnsafeGrow (mcon v) n = mcon <$> GM.basicUnsafeGrow v n }; \
instance G.Vector Vector (ty) where { \
  {-# INLINE basicUnsafeFreeze #-} \
; {-# INLINE basicUnsafeThaw #-} \
; {-# INLINE basicLength #-} \
; {-# INLINE basicUnsafeSlice #-} \
; {-# INLINE basicUnsafeIndexM #-} \
; {-# INLINE basicUnsafeCopy #-} \
; {-# INLINE elemseq #-} \
; basicUnsafeFreeze (mcon v) = con <$> G.basicUnsafeFreeze v \
; basicUnsafeThaw (con v) = mcon <$> G.basicUnsafeThaw v \
; basicLength (con v) = G.basicLength v \
; basicUnsafeSlice i n (con v) = con (G.basicUnsafeSlice i n v) \
; basicUnsafeIndexM (con v) i = G.basicUnsafeIndexM v i \
; basicUnsafeCopy (mcon m) (con v) = G.basicUnsafeCopy m v \
; elemseq _ = seq }; \
instance Element (ty); \
instance FixedWidth (ty) where { \
  {-# INLINE fromStorable #-} \
; {-# INLINE storableVector #-} \
; fromStorable = con \
; storableVector (con v) = v }

FIXED(Int8, V_Int8, MV_Int8)
FIXED(Int16, V_Int16, MV_Int16)
FIXED(Int32, V_Int32, MV_Int32)
FIXED(Int64, V_Int64, MV_Int64)
FIXED(Int, V_Int, MV_Int)
FIXED(Word8, V_Word8, MV_Word8)
FIXED(Word16, V_Word16, MV_Word16)
FIXED(Word32, V_Word32, MV_Word32)
FIXED(Word64, V_Word64, MV_Word64)
FIXED(Word, V_Word, MV_Word)
FIXED(Float, V_Float, MV_Float)
FIXED(Double, V_Double, MV_Double)
FIXED(Float16, V_Float16, MV_Float16)
FIXED(IntervalDayTime, V_IntervalDayTime, MV_IntervalDayTime)
FIXED(IntervalMonthDayNano, V_IntervalMonthDayNano, MV_IntervalMonthDayNano)
FIXED(Decimal128, V_Decimal128, MV_Decimal128)
FIXED(Decimal256, V_Decimal256, MV_Decimal256)


-- ============================================================
-- Bits
-- ============================================================

-- | Bytes holding @n@ bits that start at bit @o@ of the first byte.
bitBytes :: Int -> Int -> Int
bitBytes o n = if n <= 0 then 0 else (o + n + 7) `unsafeShiftR` 3
{-# INLINE bitBytes #-}


-- | Bit @k@ (LSB first).
indexBit :: VS.Vector Word8 -> Int -> Bool
indexBit bs k = (VS.unsafeIndex bs (k `unsafeShiftR` 3) `unsafeShiftR` (k .&. 7)) .&. 1 /= 0
{-# INLINE indexBit #-}


-- | A fresh bitmap of @n@ set bits.
allSetBits :: Int -> VS.Vector Word8
allSetBits n = VS.replicate (bitBytes 0 n) 0xFF


-- | O(1): bytes as a bitmap buffer.
bytesToBits :: ByteString -> VS.Vector Word8
bytesToBits (BSI.BS fp len) = VS.unsafeFromForeignPtr0 fp len
{-# INLINE bytesToBits #-}


-- | O(1): a bitmap buffer as bytes.
bitsToBytes :: VS.Vector Word8 -> ByteString
bitsToBytes v = let !(fp, len) = VS.unsafeToForeignPtr0 v in BSI.BS fp len
{-# INLINE bitsToBytes #-}


readBitM :: VSM.MVector s Word8 -> Int -> ST s Bool
readBitM bs k = do
  w <- VSM.unsafeRead bs (k `unsafeShiftR` 3)
  pure ((w `unsafeShiftR` (k .&. 7)) .&. 1 /= 0)
{-# INLINE readBitM #-}


writeBitM :: VSM.MVector s Word8 -> Int -> Bool -> ST s ()
writeBitM bs k b = do
  let !j = k `unsafeShiftR` 3
      !m = 1 `unsafeShiftL` (k .&. 7) :: Word8
  w <- VSM.unsafeRead bs j
  VSM.unsafeWrite bs j (if b then w .|. m else w .&. complement m)
{-# INLINE writeBitM #-}


-- | Set bits @[off, off + n)@ to @b@, leaving the neighbouring bits alone.
fillBitsM :: VSM.MVector s Word8 -> Int -> Int -> Bool -> ST s ()
fillBitsM bs off n b
  | n <= 0 = pure ()
  | j0 == j1 = patch j0 (headMask .&. tailMask)
  | otherwise = do
      patch j0 headMask
      VSM.set (VSM.unsafeSlice (j0 + 1) (j1 - j0 - 1) bs) (if b then 0xFF else 0)
      patch j1 tailMask
  where
    !end = off + n
    !j0 = off `unsafeShiftR` 3
    !j1 = (end - 1) `unsafeShiftR` 3
    headMask = 0xFF `unsafeShiftL` (off .&. 7) :: Word8
    tailMask = 0xFF `unsafeShiftR` (7 - ((end - 1) .&. 7)) :: Word8
    patch j m = do
      w <- VSM.unsafeRead bs j
      VSM.unsafeWrite bs j (if b then w .|. m else w .&. complement m)


-- | Copy @n@ bits between non-overlapping ranges (destination neighbours kept).
copyBitsM :: VSM.MVector s Word8 -> Int -> VSM.MVector s Word8 -> Int -> Int -> ST s ()
copyBitsM (VSM.MVector _ fd) doff (VSM.MVector _ fs) soff n
  | n <= 0 = pure ()
  | otherwise =
      unsafeIOToST $ unsafeWithForeignPtr fd $ \pd -> unsafeWithForeignPtr fs $ \ps -> K.copyBits pd doff ps soff n
{-# INLINE copyBitsM #-}


-- | 'copyBitsM' from an immutable bitmap.
copyBitsFrom :: VSM.MVector s Word8 -> Int -> VS.Vector Word8 -> Int -> Int -> ST s ()
copyBitsFrom (VSM.MVector _ fd) doff src soff n
  | n <= 0 = pure ()
  | otherwise =
      unsafeIOToST $ unsafeWithForeignPtr fd $ \pd -> VS.unsafeWith src $ \ps -> K.copyBits pd doff ps soff n
{-# INLINE copyBitsFrom #-}


-- | 'copyBitsM' for ranges that may overlap (through a scratch buffer when they do).
moveBitsM :: VSM.MVector s Word8 -> Int -> VSM.MVector s Word8 -> Int -> Int -> ST s ()
moveBitsM dst doff src soff n
  | bitsOverlap dst doff n src soff n = do
      tmp <- VSM.unsafeNew (bitBytes 0 n)
      copyBitsM tmp 0 src soff n
      copyBitsM dst doff tmp 0 n
  | otherwise = copyBitsM dst doff src soff n


-- | Whether two bit ranges share a bit (compared by absolute bit address).
bitsOverlap :: VSM.MVector s Word8 -> Int -> Int -> VSM.MVector s Word8 -> Int -> Int -> Bool
bitsOverlap (VSM.MVector _ fa) oa na (VSM.MVector _ fb) ob nb =
  na > 0 && nb > 0 && a < b + nb && b < a + na
  where
    !a = addrOf fa * 8 + oa
    !b = addrOf fb * 8 + ob
    addrOf fp = unsafeForeignPtrToPtr fp `minusPtr` nullPtr


-- ============================================================
-- Bool
-- ============================================================

-- | Bit offset (below 8 after slicing), length, bytes.
data instance MVector s Bool = MV_Bool {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !(VSM.MVector s Word8)


-- | Bit offset (below 8 after slicing), length, bytes.
data instance Vector Bool = V_Bool {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !(VS.Vector Word8)


instance GM.MVector MVector Bool where
  {-# INLINE basicLength #-}
  basicLength (MV_Bool _ n _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MV_Bool o _ bs) =
    let !k = o + i
        !o' = k .&. 7
    in MV_Bool o' n (VSM.unsafeSlice (k `unsafeShiftR` 3) (bitBytes o' n) bs)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Bool oa na a) (MV_Bool ob nb b) = bitsOverlap a oa na b ob nb
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MV_Bool 0 n <$> VSM.unsafeNew (bitBytes 0 n)
  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Bool o n bs) = fillBitsM bs o n False
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Bool o _ bs) i = readBitM bs (o + i)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Bool o _ bs) i b = writeBitM bs (o + i) b
  {-# INLINE basicSet #-}
  basicSet (MV_Bool o n bs) b = fillBitsM bs o n b
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Bool od n dst) (MV_Bool os _ src) = copyBitsM dst od src os n
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_Bool od n dst) (MV_Bool os _ src) = moveBitsM dst od src os n
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_Bool o n bs) by = do
    nb <- VSM.unsafeNew (bitBytes 0 (n + by))
    copyBitsM nb 0 bs o n
    pure (MV_Bool 0 (n + by) nb)


instance G.Vector Vector Bool where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Bool o n bs) = V_Bool o n <$> VS.unsafeFreeze bs
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Bool o n bs) = MV_Bool o n <$> VS.unsafeThaw bs
  {-# INLINE basicLength #-}
  basicLength (V_Bool _ n _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (V_Bool o _ bs) =
    let !k = o + i
        !o' = k .&. 7
    in V_Bool o' n (VS.unsafeSlice (k `unsafeShiftR` 3) (bitBytes o' n) bs)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Bool o _ bs) i = let !b = indexBit bs (o + i) in Box b
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Bool od n dst) (V_Bool os _ src) = copyBitsFrom dst od src os n


instance Element Bool


-- ============================================================
-- Maybe
-- ============================================================

-- | Validity bit offset (below 8 after slicing), validity bytes (bit set = 'Just'), values.
data instance MVector s (Maybe a) = MV_Maybe {-# UNPACK #-} !Int {-# UNPACK #-} !(VSM.MVector s Word8) !(MVector s a)


-- | Validity bit offset (below 8 after slicing), validity bytes (bit set = 'Just'), values.
data instance Vector (Maybe a) = V_Maybe {-# UNPACK #-} !Int {-# UNPACK #-} !(VS.Vector Word8) !(Vector a)


instance Element a => GM.MVector MVector (Maybe a) where
  {-# INLINE basicLength #-}
  basicLength (MV_Maybe _ _ xs) = GM.basicLength xs
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MV_Maybe o bs xs) =
    let !k = o + i
        !o' = k .&. 7
    in MV_Maybe o' (VSM.unsafeSlice (k `unsafeShiftR` 3) (bitBytes o' n) bs) (GM.basicUnsafeSlice i n xs)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Maybe _ _ a) (MV_Maybe _ _ b) = GM.basicOverlaps a b
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MV_Maybe 0 <$> VSM.unsafeNew (bitBytes 0 n) <*> GM.basicUnsafeNew n
  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Maybe o bs xs) = do
    fillBitsM bs o (GM.basicLength xs) False
    GM.basicInitialize xs
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Maybe o bs xs) i = do
    ok <- readBitM bs (o + i)
    if ok then Just <$> GM.basicUnsafeRead xs i else pure Nothing
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Maybe o bs xs) i m = case m of
    Nothing -> writeBitM bs (o + i) False
    Just x -> do
      writeBitM bs (o + i) True
      GM.basicUnsafeWrite xs i x
  {-# INLINE basicClear #-}
  basicClear (MV_Maybe _ _ xs) = GM.basicClear xs
  {-# INLINE basicSet #-}
  basicSet (MV_Maybe o bs xs) m = case m of
    Nothing -> fillBitsM bs o (GM.basicLength xs) False
    Just x -> do
      fillBitsM bs o (GM.basicLength xs) True
      GM.basicSet xs x
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Maybe od dbs dxs) (MV_Maybe os sbs sxs) = do
    copyBitsM dbs od sbs os (GM.basicLength sxs)
    GM.basicUnsafeCopy dxs sxs
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_Maybe od dbs dxs) (MV_Maybe os sbs sxs) = do
    moveBitsM dbs od sbs os (GM.basicLength sxs)
    GM.basicUnsafeMove dxs sxs
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_Maybe o bs xs) by = do
    let !n = GM.basicLength xs
    nb <- VSM.unsafeNew (bitBytes 0 (n + by))
    copyBitsM nb 0 bs o n
    MV_Maybe 0 nb <$> GM.basicUnsafeGrow xs by


instance Element a => G.Vector Vector (Maybe a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Maybe o bs xs) = V_Maybe o <$> VS.unsafeFreeze bs <*> G.basicUnsafeFreeze xs
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Maybe o bs xs) = MV_Maybe o <$> VS.unsafeThaw bs <*> G.basicUnsafeThaw xs
  {-# INLINE basicLength #-}
  basicLength (V_Maybe _ _ xs) = G.basicLength xs
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (V_Maybe o bs xs) =
    let !k = o + i
        !o' = k .&. 7
    in V_Maybe o' (VS.unsafeSlice (k `unsafeShiftR` 3) (bitBytes o' n) bs) (G.basicUnsafeSlice i n xs)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Maybe o bs xs) i
    | indexBit bs (o + i) = case G.basicUnsafeIndexM xs i of Box x -> Box (Just x)
    | otherwise = Box Nothing
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Maybe od dbs dxs) (V_Maybe os sbs sxs) = do
    copyBitsFrom dbs od sbs os (G.basicLength sxs)
    G.basicUnsafeCopy dxs sxs
  {-# INLINE elemseq #-}
  elemseq _ m y = case m of
    Nothing -> y
    Just x -> G.elemseq (undefined :: Vector a) x y


instance Element a => Element (Maybe a)


{- | O(1): the values behind a vector of 'Maybe's. Slots whose row is
'Nothing' hold unspecified values.
-}
maybeValues :: Vector (Maybe a) -> Vector a
maybeValues (V_Maybe _ _ xs) = xs
{-# INLINE maybeValues #-}


-- ============================================================
-- Var-length rows: Text, ByteString, Vector a
-- ============================================================

{- | Immutable var-length rows: row @i@ is the slice
@[starts ! i, ends ! i)@ of the store. Rows may share or skip bytes of
the store; every range lies inside it.
-}
data VarVec a = VarVec {-# UNPACK #-} !(VS.Vector Int) {-# UNPACK #-} !(VS.Vector Int) !(Store a)


{- | Mutable var-length rows: per-row start and end offsets into a
growable, append-only store shared by every slice of the vector.
Writing a row appends its bytes (or child elements) and points the row
at them; bytes already written are never overwritten, so a row read
from the vector stays valid after later writes and growth.
-}
data MVarVec s a = MVarVec {-# UNPACK #-} !(VSM.MVector s Int) {-# UNPACK #-} !(VSM.MVector s Int) {-# UNPACK #-} !(MutVar s (Grow s a))


-- | Element types stored as slices of one store.
class VarElem a where
  -- | The immutable store.
  type Store a

  -- | The growable store and how much of it is used.
  data Grow s a

  -- | Row @[s, e)@ of a store, O(1).
  storeRow :: Store a -> Int -> Int -> a

  -- | Store units (bytes or child elements) a row occupies.
  rowLength :: a -> Int

  -- | Append a row; it lands at 'growUsed' of the old store.
  appendRow :: Grow s a -> a -> ST s (Grow s a)

  -- | Append @[s, e)@ of an immutable store.
  appendStore :: Grow s a -> Store a -> Int -> Int -> ST s (Grow s a)

  -- | Append @[s, e)@ of another growable store.
  appendGrow :: Grow s a -> Grow s a -> Int -> Int -> ST s (Grow s a)

  growUsed :: Grow s a -> Int

  -- | Row @[s, e)@ of a growable store, O(1).
  growRow :: Grow s a -> Int -> Int -> ST s a

  growEmpty :: ST s (Grow s a)

  -- | O(1); keeps the whole buffer, unused capacity included.
  growFreeze :: Grow s a -> ST s (Store a)

  -- | O(1); the store counts as full, so the first append copies it.
  growThaw :: Store a -> ST s (Grow s a)

  toVar :: Vector a -> VarVec a
  fromVar :: VarVec a -> Vector a


varLength :: VarVec a -> Int
varLength (VarVec st _ _) = VS.length st
{-# INLINE varLength #-}


varSlice :: Int -> Int -> VarVec a -> VarVec a
varSlice i n (VarVec st en d) = VarVec (VS.unsafeSlice i n st) (VS.unsafeSlice i n en) d
{-# INLINE varSlice #-}


varIndex :: VarElem a => VarVec a -> Int -> Box a
varIndex (VarVec st en d) i = let !x = storeRow d (VS.unsafeIndex st i) (VS.unsafeIndex en i) in Box x
{-# INLINE varIndex #-}


varFreeze :: VarElem a => MVarVec s a -> ST s (VarVec a)
varFreeze (MVarVec st en ref) = do
  st' <- VS.unsafeFreeze st
  en' <- VS.unsafeFreeze en
  d <- readMutVar ref >>= growFreeze
  pure (VarVec st' en' d)
{-# INLINE varFreeze #-}


{- | A vector converted from Arrow offsets keeps starts and ends as two
overlapping slices of one offsets buffer; writing row @i@ would then
move the start of row @i + 1@, so such a vector gets separate copies.
-}
varThaw :: VarElem a => VarVec a -> ST s (MVarVec s a)
varThaw (VarVec st en d) = do
  st' <- VS.unsafeThaw st
  en' <- if storableOverlap st en then VS.thaw en else VS.unsafeThaw en
  ref <- growThaw d >>= newMutVar
  pure (MVarVec st' en' ref)
{-# INLINE varThaw #-}


storableOverlap :: VS.Vector Int -> VS.Vector Int -> Bool
storableOverlap a b =
  let !(fa, na) = VS.unsafeToForeignPtr0 a
      !(fb, nb) = VS.unsafeToForeignPtr0 b
      !pa = unsafeForeignPtrToPtr fa `minusPtr` nullPtr
      !pb = unsafeForeignPtrToPtr fb `minusPtr` nullPtr
  in na > 0 && nb > 0 && pa < pb + nb * 8 && pb < pa + na * 8


mvarLength :: MVarVec s a -> Int
mvarLength (MVarVec st _ _) = VSM.length st
{-# INLINE mvarLength #-}


mvarSlice :: Int -> Int -> MVarVec s a -> MVarVec s a
mvarSlice i n (MVarVec st en ref) = MVarVec (VSM.unsafeSlice i n st) (VSM.unsafeSlice i n en) ref
{-# INLINE mvarSlice #-}


mvarOverlaps :: MVarVec s a -> MVarVec s a -> Bool
mvarOverlaps (MVarVec a _ _) (MVarVec b _ _) = VSM.overlaps a b
{-# INLINE mvarOverlaps #-}


-- | Offsets start zeroed even for 'GM.unsafeNew': an unwritten row is the empty row, never a wild range.
mvarNew :: VarElem a => Int -> ST s (MVarVec s a)
mvarNew n = do
  st <- zeroInts n
  en <- zeroInts n
  ref <- growEmpty >>= newMutVar
  pure (MVarVec st en ref)
{-# INLINE mvarNew #-}


zeroInts :: Int -> ST s (VSM.MVector s Int)
zeroInts n = do
  v <- VSM.unsafeNew n
  VSM.set v 0
  pure v


mvarInitialize :: MVarVec s a -> ST s ()
mvarInitialize (MVarVec st en _) = VSM.set st 0 >> VSM.set en 0
{-# INLINE mvarInitialize #-}


mvarRead :: VarElem a => MVarVec s a -> Int -> ST s a
mvarRead (MVarVec st en ref) i = do
  s <- VSM.unsafeRead st i
  e <- VSM.unsafeRead en i
  g <- readMutVar ref
  growRow g s e
{-# INLINE mvarRead #-}


mvarWrite :: VarElem a => MVarVec s a -> Int -> a -> ST s ()
mvarWrite (MVarVec st en ref) i x = do
  g <- readMutVar ref
  let !s = growUsed g
  g' <- appendRow g x
  writeMutVar ref g'
  VSM.unsafeWrite st i s
  VSM.unsafeWrite en i (s + rowLength x)
{-# INLINE mvarWrite #-}


-- | One append, then every row points at it.
mvarSet :: VarElem a => MVarVec s a -> a -> ST s ()
mvarSet (MVarVec st en ref) x = do
  g <- readMutVar ref
  let !s = growUsed g
  g' <- appendRow g x
  writeMutVar ref g'
  VSM.set st s
  VSM.set en (s + rowLength x)
{-# INLINE mvarSet #-}


-- | Slices of one vector share their store: only offsets move. Otherwise the rows' bytes are appended.
mvarCopy :: VarElem a => MVarVec s a -> MVarVec s a -> ST s ()
mvarCopy dst@(MVarVec dst' den dref) (MVarVec sst sen sref)
  | dref == sref = VSM.unsafeCopy dst' sst >> VSM.unsafeCopy den sen
  | otherwise = do
      sg <- readMutVar sref
      copyRows dst (VSM.length sst) (VSM.unsafeRead sst) (VSM.unsafeRead sen) (\g s e -> appendGrow g sg s e)
{-# INLINE mvarCopy #-}


mvarMove :: VarElem a => MVarVec s a -> MVarVec s a -> ST s ()
mvarMove dst@(MVarVec dst' den dref) src@(MVarVec sst sen sref)
  | dref == sref = VSM.unsafeMove dst' sst >> VSM.unsafeMove den sen
  | otherwise = mvarCopy dst src
{-# INLINE mvarMove #-}


-- | Grows the offsets (new rows empty); the store is shared, not copied.
mvarGrow :: MVarVec s a -> Int -> ST s (MVarVec s a)
mvarGrow (MVarVec st en ref) by = do
  let !n = VSM.length st
  st' <- VSM.unsafeGrow st by
  en' <- VSM.unsafeGrow en by
  VSM.set (VSM.unsafeSlice n by st') 0
  VSM.set (VSM.unsafeSlice n by en') 0
  pure (MVarVec st' en' ref)
{-# INLINE mvarGrow #-}


-- | Copy rows from an immutable vector (appending the bytes they reference).
copyFromVar :: VarElem a => MVarVec s a -> VarVec a -> ST s ()
copyFromVar dst (VarVec st en d) =
  copyRows dst (VS.length st) (\i -> pure (VS.unsafeIndex st i)) (\i -> pure (VS.unsafeIndex en i)) (\g s e -> appendStore g d s e)
{-# INLINE copyFromVar #-}


-- | Span of the non-empty source rows and their total length.
data Span = Span {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int


{- | Copy @n@ rows into @dst@. When the non-empty rows reference a span
no longer than their total length (contiguous rows, or rows sharing
bytes such as dictionary values), the span is appended once and the
rows are rebased into it; otherwise each row is appended on its own, so
the copy never carries bytes no row references.
-}
copyRows ::
  VarElem a =>
  MVarVec s a ->
  Int ->
  (Int -> ST s Int) ->
  (Int -> ST s Int) ->
  (Grow s a -> Int -> Int -> ST s (Grow s a)) ->
  ST s ()
copyRows (MVarVec dst den ref) n srcStart srcEnd append = do
  Span lo hi total <- scan 0 maxBound minBound 0
  g0 <- readMutVar ref
  let !base = growUsed g0
  if total == 0
    then VSM.set dst base >> VSM.set den base
    else
      if hi - lo <= total
        then do
          g1 <- append g0 lo hi
          writeMutVar ref g1
          let rebase !i
                | i >= n = pure ()
                | otherwise = do
                    s <- srcStart i
                    e <- srcEnd i
                    if e > s
                      then VSM.unsafeWrite dst i (s - lo + base) >> VSM.unsafeWrite den i (e - lo + base)
                      else VSM.unsafeWrite dst i base >> VSM.unsafeWrite den i base
                    rebase (i + 1)
          rebase 0
        else do
          let each !i g
                | i >= n = writeMutVar ref g
                | otherwise = do
                    s <- srcStart i
                    e <- srcEnd i
                    let !u = growUsed g
                    VSM.unsafeWrite dst i u
                    VSM.unsafeWrite den i (u + max 0 (e - s))
                    if e > s then append g s e >>= each (i + 1) else each (i + 1) g
          each 0 g0
  where
    scan !i !lo !hi !total
      | i >= n = pure (Span lo hi total)
      | otherwise = do
          s <- srcStart i
          e <- srcEnd i
          if e > s then scan (i + 1) (min lo s) (max hi e) (total + e - s) else scan (i + 1) lo hi total
{-# INLINE copyRows #-}


#define VARLEN(ctx,ty,con,mcon) \
instance ctx => GM.MVector MVector (ty) where { \
  {-# INLINE basicLength #-} \
; {-# INLINE basicUnsafeSlice #-} \
; {-# INLINE basicOverlaps #-} \
; {-# INLINE basicUnsafeNew #-} \
; {-# INLINE basicInitialize #-} \
; {-# INLINE basicUnsafeRead #-} \
; {-# INLINE basicUnsafeWrite #-} \
; {-# INLINE basicSet #-} \
; {-# INLINE basicUnsafeCopy #-} \
; {-# INLINE basicUnsafeMove #-} \
; {-# INLINE basicUnsafeGrow #-} \
; basicLength (mcon v) = mvarLength v \
; basicUnsafeSlice i n (mcon v) = mcon (mvarSlice i n v) \
; basicOverlaps (mcon a) (mcon b) = mvarOverlaps a b \
; basicUnsafeNew n = mcon <$> mvarNew n \
; basicInitialize (mcon v) = mvarInitialize v \
; basicUnsafeRead (mcon v) i = mvarRead v i \
; basicUnsafeWrite (mcon v) i x = mvarWrite v i x \
; basicSet (mcon v) x = mvarSet v x \
; basicUnsafeCopy (mcon a) (mcon b) = mvarCopy a b \
; basicUnsafeMove (mcon a) (mcon b) = mvarMove a b \
; basicUnsafeGrow (mcon v) n = mcon <$> mvarGrow v n }; \
instance ctx => G.Vector Vector (ty) where { \
  {-# INLINE basicUnsafeFreeze #-} \
; {-# INLINE basicUnsafeThaw #-} \
; {-# INLINE basicLength #-} \
; {-# INLINE basicUnsafeSlice #-} \
; {-# INLINE basicUnsafeIndexM #-} \
; {-# INLINE basicUnsafeCopy #-} \
; {-# INLINE elemseq #-} \
; basicUnsafeFreeze (mcon v) = con <$> varFreeze v \
; basicUnsafeThaw (con v) = mcon <$> varThaw v \
; basicLength (con v) = varLength v \
; basicUnsafeSlice i n (con v) = con (varSlice i n v) \
; basicUnsafeIndexM (con v) i = varIndex v i \
; basicUnsafeCopy (mcon m) (con v) = copyFromVar m v \
; elemseq _ = seq }; \
instance ctx => Element (ty)


-- Text ------------------------------------------------------------

-- | Rows are 'Text' slices of one text array (no copy, no re-validation).
newtype instance Vector Text = V_Text (VarVec Text)


newtype instance MVector s Text = MV_Text (MVarVec s Text)


instance VarElem Text where
  type Store Text = TA.Array
  data Grow s Text = TextGrow {-# UNPACK #-} !(MutableByteArray s) {-# UNPACK #-} !Int
  storeRow arr s e
    | e == s = T.empty
    | otherwise = TI.Text arr s (e - s)
  {-# INLINE storeRow #-}
  rowLength (TI.Text _ _ len) = len
  {-# INLINE rowLength #-}
  appendRow g (TI.Text arr off len) = appendStore g arr off (off + len)
  {-# INLINE appendRow #-}
  appendStore g (TA.ByteArray ba) s e = do
    TextGrow mba u <- reserveText g (e - s)
    copyByteArray mba u (ByteArray ba) s (e - s)
    pure (TextGrow mba (u + e - s))
  appendGrow g (TextGrow src _) s e = do
    TextGrow mba u <- reserveText g (e - s)
    copyMutableByteArray mba u src s (e - s)
    pure (TextGrow mba (u + e - s))
  growUsed (TextGrow _ u) = u
  {-# INLINE growUsed #-}
  growRow (TextGrow mba _) s e
    | e == s = pure T.empty
    | otherwise = do
        ByteArray ba <- unsafeFreezeByteArray mba
        pure (TI.Text (TA.ByteArray ba) s (e - s))
  {-# INLINE growRow #-}
  growEmpty = (\mba -> TextGrow mba 0) <$> newByteArray 0
  growFreeze (TextGrow mba _) = (\(ByteArray ba) -> TA.ByteArray ba) <$> unsafeFreezeByteArray mba
  growThaw (TA.ByteArray ba) = (\mba -> TextGrow mba (sizeofByteArray (ByteArray ba))) <$> unsafeThawByteArray (ByteArray ba)
  toVar (V_Text v) = v
  {-# INLINE toVar #-}
  fromVar = V_Text
  {-# INLINE fromVar #-}


reserveText :: Grow s Text -> Int -> ST s (Grow s Text)
reserveText g@(TextGrow mba u) need = do
  cap <- getSizeofMutableByteArray mba
  if u + need <= cap
    then pure g
    else do
      let !cap' = max (u + need) (max 64 (2 * cap))
      mba' <- newByteArray cap'
      copyMutableByteArray mba' 0 mba 0 u
      pure (TextGrow mba' u)


VARLEN((), Text, V_Text, MV_Text)


-- ByteString ------------------------------------------------------

-- | Rows are zero-copy 'ByteString' slices of one buffer.
newtype instance Vector ByteString = V_Bytes (VarVec ByteString)


newtype instance MVector s ByteString = MV_Bytes (MVarVec s ByteString)


instance VarElem ByteString where
  type Store ByteString = ByteString
  -- buffer, capacity, used
  data Grow s ByteString = BytesGrow !(ForeignPtr Word8) {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  storeRow (BSI.BS fp _) s e = BSI.BS (fp `plusForeignPtr` s) (e - s)
  {-# INLINE storeRow #-}
  rowLength = BS.length
  {-# INLINE rowLength #-}
  appendRow g bs = appendStore g bs 0 (BS.length bs)
  {-# INLINE appendRow #-}
  appendStore g (BSI.BS fp _) s e = appendBytes g fp s (e - s)
  appendGrow g (BytesGrow fp _ _) s e = appendBytes g fp s (e - s)
  growUsed (BytesGrow _ _ u) = u
  {-# INLINE growUsed #-}
  growRow (BytesGrow fp _ _) s e = pure (BSI.BS (fp `plusForeignPtr` s) (e - s))
  {-# INLINE growRow #-}
  growEmpty = pure (BytesGrow emptyBuffer 0 0)
  growFreeze (BytesGrow fp _ u) = pure (BSI.BS fp u)
  growThaw (BSI.BS fp len) = pure (BytesGrow fp len len)
  toVar (V_Bytes v) = v
  {-# INLINE toVar #-}
  fromVar = V_Bytes
  {-# INLINE fromVar #-}


emptyBuffer :: ForeignPtr Word8
emptyBuffer = case BS.empty of BSI.BS fp _ -> fp
{-# NOINLINE emptyBuffer #-}


appendBytes :: Grow s ByteString -> ForeignPtr Word8 -> Int -> Int -> ST s (Grow s ByteString)
appendBytes g src s len
  | len <= 0 = pure g
  | otherwise = do
      BytesGrow fp cap u <- reserveBytes g len
      unsafeIOToST $ unsafeWithForeignPtr fp $ \d -> unsafeWithForeignPtr src $ \p ->
        copyBytes (d `plusPtr` u) (p `plusPtr` s) len
      pure (BytesGrow fp cap (u + len))


reserveBytes :: Grow s ByteString -> Int -> ST s (Grow s ByteString)
reserveBytes g@(BytesGrow fp cap u) need
  | u + need <= cap = pure g
  | otherwise = unsafeIOToST $ do
      let !cap' = max (u + need) (max 64 (2 * cap))
      fp' <- mallocPlainForeignPtrBytes cap'
      unsafeWithForeignPtr fp' $ \d -> unsafeWithForeignPtr fp $ \p -> copyBytes d p u
      pure (BytesGrow fp' cap' u)


VARLEN((), ByteString, V_Bytes, MV_Bytes)


-- Vector a (list rows) --------------------------------------------

-- | Rows are O(1) slices of one child vector.
newtype instance Vector (Vector a) = V_List (VarVec (Vector a))


newtype instance MVector s (Vector a) = MV_List (MVarVec s (Vector a))


instance Element a => VarElem (Vector a) where
  type Store (Vector a) = Vector a
  -- child elements (capacity = length), used
  data Grow s (Vector a) = ListGrow !(MVector s a) {-# UNPACK #-} !Int
  storeRow xs s e = G.basicUnsafeSlice s (e - s) xs
  {-# INLINE storeRow #-}
  rowLength = G.basicLength
  {-# INLINE rowLength #-}
  appendRow g xs = appendStore g xs 0 (G.basicLength xs)
  {-# INLINE appendRow #-}
  appendStore g xs s e = do
    ListGrow mv u <- reserveList g (e - s)
    G.basicUnsafeCopy (GM.basicUnsafeSlice u (e - s) mv) (G.basicUnsafeSlice s (e - s) xs)
    pure (ListGrow mv (u + e - s))
  appendGrow g (ListGrow src _) s e = do
    ListGrow mv u <- reserveList g (e - s)
    GM.basicUnsafeCopy (GM.basicUnsafeSlice u (e - s) mv) (GM.basicUnsafeSlice s (e - s) src)
    pure (ListGrow mv (u + e - s))
  growUsed (ListGrow _ u) = u
  {-# INLINE growUsed #-}
  growRow (ListGrow mv _) s e = G.basicUnsafeFreeze (GM.basicUnsafeSlice s (e - s) mv)
  {-# INLINE growRow #-}
  growEmpty = (\mv -> ListGrow mv 0) <$> GM.basicUnsafeNew 0
  growFreeze (ListGrow mv u) = G.basicUnsafeFreeze (GM.basicUnsafeSlice 0 u mv)
  growThaw xs = (\mv -> ListGrow mv (G.basicLength xs)) <$> G.basicUnsafeThaw xs
  toVar (V_List v) = v
  {-# INLINE toVar #-}
  fromVar = V_List
  {-# INLINE fromVar #-}


reserveList :: Element a => Grow s (Vector a) -> Int -> ST s (Grow s (Vector a))
reserveList g@(ListGrow mv u) need
  | u + need <= cap = pure g
  | otherwise = do
      let !cap' = max (u + need) (max 16 (2 * cap))
      mv' <- GM.basicUnsafeGrow mv (cap' - cap)
      pure (ListGrow mv' u)
  where
    !cap = GM.basicLength mv


VARLEN((Element a), Vector a, V_List, MV_List)
