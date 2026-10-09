{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Low-level Arrow buffers: LSB-first bitmaps, validity, the element
types that have no Haskell primitive, and pinned 64-byte aligned
allocation.

This module exports raw constructors. 'Bitmap' and 'Validity' carry
invariants (documented on each type) that the accessors in
"Arrow.Column" rely on; code outside the reader, the writer, the
builders and the smart constructors should build them through
'mkBitmap', 'bitmapGenerate', 'mkValidity' and friends, which
"Arrow.Column" re-exports with the types kept abstract.
-}
module Arrow.Column.Buffer (
  -- * Bitmaps
  Bitmap (..),
  bitmapBytes,
  bitmapOffset,
  bitmapLength,
  mkBitmap,
  emptyBitmap,
  bitAt,
  unsafeBitAt,
  sliceBitmap,
  bitmapSetCount,
  bitmapGenerate,
  bitmapFromBools,
  bitmapToBools,
  copyBitmap,
  concatBitmaps,
  takeBitmap,

  -- * Validity
  Validity (..),
  validityBits,
  validityNullCount,
  mkValidity,
  validityGenerate,
  validityFromBools,
  checkValidity,
  isValidAt,
  unsafeIsValidAt,
  sliceValidity,
  concatValidity,
  takeValidity,
  andValidity,
  copyValidity,

  -- * Element types
  Float16 (..),
  float16ToDouble,
  IntervalDayTime (..),
  IntervalMonthDayNano (..),
  Decimal128 (..),
  decimal128ToInteger,
  decimal128FromInteger,
  Decimal256 (..),
  decimal256ToInteger,
  decimal256FromInteger,

  -- * Pinned buffers
  mallocAligned,
  createAligned,
  storableToBytes,
  bytesToStorable,
  unsafeBytesToStorable,
  copyStorable,
  withBytesPtr,
  withStorablePtr,
) where

import Columnar.SIMD (andBits, copyBits, gatherBits, popCountBits)
import Control.DeepSeq (NFData (..))
import Data.Bits (complement, shiftL, shiftR, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, plusPtr, ptrToWordPtr)
import Foreign.Storable (Storable (..))
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes, unsafeWithForeignPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)


-- ============================================================
-- Pinned buffers
-- ============================================================

-- | Fresh pinned memory, 64-byte aligned (the arrow-rs allocation alignment).
mallocAligned :: Int -> IO (ForeignPtr Word8)
mallocAligned n = mallocPlainForeignPtrAlignedBytes (max 0 n) 64
{-# INLINE mallocAligned #-}


-- | A 64-byte aligned 'ByteString' of @n@ bytes filled by the action.
createAligned :: Int -> (Ptr Word8 -> IO ()) -> ByteString
createAligned !n fill = unsafeDupablePerformIO $ do
  fp <- mallocAligned n
  unsafeWithForeignPtr fp fill
  pure $! BSI.BS fp (max 0 n)
{-# INLINE createAligned #-}


-- | O(1): view a storable vector's memory as bytes (no copy).
storableToBytes :: forall a. Storable a => VS.Vector a -> ByteString
storableToBytes v =
  let !(fp, n) = VS.unsafeToForeignPtr0 v
  in BSI.BS (castForeignPtr fp) (n * sizeOf (undefined :: a))
{-# INLINE storableToBytes #-}


{- | View bytes as a storable vector of @length / sizeOf a@ elements.
O(1) when the bytes are aligned for @a@; otherwise one copy into a
fresh aligned buffer (arrow-rs @align_buffers@). Trailing bytes that
do not fill an element are ignored.
-}
bytesToStorable :: forall a. Storable a => ByteString -> VS.Vector a
bytesToStorable bs@(BSI.BS fp len)
  | aligned = unsafeBytesToStorable bs
  | otherwise = unsafeBytesToStorable (createAligned (n * sz) (\dst -> unsafeWithForeignPtr fp (\src -> copyBytes dst src (n * sz))))
  where
    !sz = sizeOf (undefined :: a)
    !n = len `quot` sz
    !aligned = fromIntegral (ptrToWordPtr (unsafeForeignPtrToPtr fp)) `rem` alignment (undefined :: a) == (0 :: Int)
{-# INLINE bytesToStorable #-}


{- | O(1) alias of bytes as a storable vector. The caller guarantees
the bytes are aligned for @a@.
-}
unsafeBytesToStorable :: forall a. Storable a => ByteString -> VS.Vector a
unsafeBytesToStorable (BSI.BS fp len) = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `quot` sizeOf (undefined :: a))
{-# INLINE unsafeBytesToStorable #-}


-- | A detached copy of a storable vector in fresh aligned memory.
copyStorable :: forall a. Storable a => VS.Vector a -> VS.Vector a
copyStorable v = unsafeBytesToStorable (copyBytesFresh (storableToBytes v))


copyBytesFresh :: ByteString -> ByteString
copyBytesFresh bs = createAligned (BS.length bs) $ \dst -> withBytesPtr bs $ \src -> copyBytes dst src (BS.length bs)


-- | Run an action on the address of a 'ByteString'\'s first byte.
withBytesPtr :: ByteString -> (Ptr Word8 -> IO r) -> IO r
withBytesPtr (BSI.BS fp _) = unsafeWithForeignPtr fp
{-# INLINE withBytesPtr #-}


-- | Run an action on the address of a storable vector's first element.
withStorablePtr :: Storable a => VS.Vector a -> (Ptr a -> IO r) -> IO r
withStorablePtr = VS.unsafeWith
{-# INLINE withStorablePtr #-}


-- ============================================================
-- Bitmaps
-- ============================================================

{- | LSB-first bitmap: logical bit @i@ is bit @(offset + i) mod 8@ of
byte @(offset + i) div 8@. Slicing changes only the offset and the
length (arrow-rs @BooleanBuffer@).

Invariant: @offset >= 0@, @length >= 0@ and the bytes cover
@offset + length@ bits.
-}
data Bitmap = Bitmap !ByteString {-# UNPACK #-} !Int {-# UNPACK #-} !Int


instance NFData Bitmap where
  rnf = (`seq` ())


-- | Logical equality: same length, same bits (offsets ignored).
instance Eq Bitmap where
  a == b =
    bitmapLength a == bitmapLength b
      && all (\i -> unsafeBitAt a i == unsafeBitAt b i) [0 .. bitmapLength a - 1]


instance Show Bitmap where
  showsPrec d b =
    showParen (d > 10) $
      showString "bitmap " . shows (map (\x -> if x then '1' else '0') (bitmapToBools b))


-- | The underlying bytes (they may extend past the logical range).
bitmapBytes :: Bitmap -> ByteString
bitmapBytes (Bitmap bs _ _) = bs
{-# INLINE bitmapBytes #-}


-- | Offset of logical bit 0, in bits.
bitmapOffset :: Bitmap -> Int
bitmapOffset (Bitmap _ o _) = o
{-# INLINE bitmapOffset #-}


-- | Number of logical bits.
bitmapLength :: Bitmap -> Int
bitmapLength (Bitmap _ _ n) = n
{-# INLINE bitmapLength #-}


-- | Checked constructor: bytes, bit offset, bit length.
mkBitmap :: ByteString -> Int -> Int -> Either String Bitmap
mkBitmap bs off len
  | off < 0 || len < 0 = Left "Arrow.Column.Buffer.mkBitmap: negative offset or length"
  | toInteger (BS.length bs) * 8 < toInteger off + toInteger len =
      Left "Arrow.Column.Buffer.mkBitmap: bytes do not cover offset + length bits"
  | otherwise = Right (Bitmap bs off len)


emptyBitmap :: Bitmap
emptyBitmap = Bitmap BS.empty 0 0


-- | Bit @i@; errors when @i@ is outside @[0, length)@ (like 'V.!').
bitAt :: Bitmap -> Int -> Bool
bitAt b i
  | i < 0 || i >= bitmapLength b = errorWithoutStackTrace ("Arrow.Column.Buffer.bitAt: index " ++ show i ++ " out of range")
  | otherwise = unsafeBitAt b i
{-# INLINE bitAt #-}


-- | Bit @i@ without a range check.
unsafeBitAt :: Bitmap -> Int -> Bool
unsafeBitAt (Bitmap bs off _) i =
  let !k = off + i
  in (BSU.unsafeIndex bs (k `unsafeShiftR` 3) `unsafeShiftR` (k .&. 7)) .&. 1 /= 0
{-# INLINE unsafeBitAt #-}


-- | Bits @[s, s + l)@; the caller keeps the window inside the bitmap.
sliceBitmap :: Int -> Int -> Bitmap -> Bitmap
sliceBitmap s l (Bitmap bs off _) = Bitmap bs (off + s) l
{-# INLINE sliceBitmap #-}


-- | Number of set bits (C popcount over the bit range).
bitmapSetCount :: Bitmap -> Int
bitmapSetCount (Bitmap bs off len)
  | len <= 0 = 0
  | otherwise = unsafeDupablePerformIO $ withBytesPtr bs $ \p -> popCountBits p off len


{- | A fresh bitmap of @n@ bits whose bit @i@ is @f i@. One pass, eight
calls per output byte; @f@ inlines into the loop.
-}
bitmapGenerate :: Int -> (Int -> Bool) -> Bitmap
bitmapGenerate !n0 f =
  let !n = max 0 n0
      !nbytes = (n + 7) `unsafeShiftR` 3
      bs = createAligned nbytes $ \p ->
        let byteLoop !j
              | j >= nbytes = pure ()
              | otherwise = do
                  let !base = j `unsafeShiftL` 3
                      !hi = min 8 (n - base)
                      bitLoop !k !acc
                        | k >= hi = acc
                        | f (base + k) = bitLoop (k + 1) (acc .|. (1 `unsafeShiftL` k))
                        | otherwise = bitLoop (k + 1) acc
                  pokeByteOff p j (bitLoop 0 (0 :: Word8))
                  byteLoop (j + 1)
        in byteLoop 0
  in Bitmap bs 0 n
{-# INLINE bitmapGenerate #-}


bitmapFromBools :: V.Vector Bool -> Bitmap
bitmapFromBools v = bitmapGenerate (V.length v) (V.unsafeIndex v)


bitmapToBools :: Bitmap -> [Bool]
bitmapToBools b = map (unsafeBitAt b) [0 .. bitmapLength b - 1]


-- | A detached copy starting at bit offset 0.
copyBitmap :: Bitmap -> Bitmap
copyBitmap (Bitmap bs off len) =
  let !nbytes = (len + 7) `unsafeShiftR` 3
  in Bitmap
       ( createAligned nbytes $ \dst -> do
           fillBytes dst 0 nbytes
           withBytesPtr bs $ \src -> copyBits dst 0 src off len
       )
       0
       len


-- | Concatenate bitmaps into one fresh buffer (one allocation, bit copies).
concatBitmaps :: [Bitmap] -> Bitmap
concatBitmaps bms =
  let !total = sum (map bitmapLength bms)
      !nbytes = (total + 7) `unsafeShiftR` 3
      bs = createAligned nbytes $ \dst -> do
        fillBytes dst 0 nbytes
        let go !_ [] = pure ()
            go !pos (Bitmap b o l : rest) = do
              withBytesPtr b $ \src -> copyBits dst pos src o l
              go (pos + l) rest
        go 0 bms
  in Bitmap bs 0 total


-- | Gather bits by index (indices already range-checked). Returns the set count too.
takeBitmap :: VS.Vector Int -> Bitmap -> (Bitmap, Int)
takeBitmap ix (Bitmap bs off _) =
  let !n = VS.length ix
      !nbytes = (n + 7) `unsafeShiftR` 3
  in unsafeDupablePerformIO $ do
       fp <- mallocAligned nbytes
       set <- unsafeWithForeignPtr fp $ \dst ->
         withBytesPtr bs $ \src -> VS.unsafeWith ix $ \pix -> gatherBits dst src off pix n
       pure (Bitmap (BSI.BS fp nbytes) 0 n, set)


-- | Set bits @[pos, pos + r)@ of a zero-initialised buffer.
setBitRange :: Ptr Word8 -> Int -> Int -> IO ()
setBitRange p !pos !r
  | r <= 0 = pure ()
  | otherwise = do
      let !end = pos + r
          !b0 = pos `unsafeShiftR` 3
          !b1 = (end - 1) `unsafeShiftR` 3
          orByte j m = do
            x <- peekByteOff p j :: IO Word8
            pokeByteOff p j (x .|. m)
          lowMask k = (1 `unsafeShiftL` k) - 1 :: Word8
      if b0 == b1
        then orByte b0 (lowMask ((end - 1) .&. 7 + 1) .&. complement (lowMask (pos .&. 7)))
        else do
          orByte b0 (complement (lowMask (pos .&. 7)))
          fillBytes (p `plusPtr` (b0 + 1)) 0xFF (b1 - b0 - 1)
          orByte b1 (lowMask ((end - 1) .&. 7 + 1))


-- ============================================================
-- Validity
-- ============================================================

{- | A validity bitmap with its null count (arrow-rs @NullBuffer@).
Bit set means the row is valid.

Invariant: @nullCount > 0@ and equals the number of clear bits.
A column without nulls carries @Nothing@ instead (normalised by
every constructor in this package).
-}
data Validity = Validity !Bitmap {-# UNPACK #-} !Int
  deriving stock (Eq)


instance NFData Validity where
  rnf = (`seq` ())


instance Show Validity where
  showsPrec d (Validity b n) =
    showParen (d > 10) $ showString "validity " . showsPrec 11 n . showChar ' ' . showsPrec 11 b


validityBits :: Validity -> Bitmap
validityBits (Validity b _) = b
{-# INLINE validityBits #-}


validityNullCount :: Validity -> Int
validityNullCount (Validity _ n) = n
{-# INLINE validityNullCount #-}


-- | Count nulls (C popcount) and normalise: no nulls is 'Nothing'.
mkValidity :: Bitmap -> Maybe Validity
mkValidity b =
  let !nulls = bitmapLength b - bitmapSetCount b
  in if nulls == 0 then Nothing else Just (Validity b nulls)


-- | @mkValidity . bitmapGenerate n@.
validityGenerate :: Int -> (Int -> Bool) -> Maybe Validity
validityGenerate n f = mkValidity (bitmapGenerate n f)
{-# INLINE validityGenerate #-}


validityFromBools :: V.Vector Bool -> Maybe Validity
validityFromBools = mkValidity . bitmapFromBools


{- | Validate a caller-supplied validity for an array of @rows@ rows:
the bitmap covers its range, its length is @rows@, and its null count
matches the bits. Normalises a validity with no nulls to 'Nothing'.
-}
checkValidity :: String -> Int -> Maybe Validity -> Either String (Maybe Validity)
checkValidity _ _ Nothing = Right Nothing
checkValidity what rows (Just (Validity b@(Bitmap bs off len) declared))
  | off < 0 || len < 0 || toInteger (BS.length bs) * 8 < toInteger off + toInteger len =
      Left (what ++ ": validity bytes do not cover the bitmap")
  | len /= rows = Left (what ++ ": validity has " ++ show len ++ " bits for " ++ show rows ++ " rows")
  | otherwise =
      let !nulls = len - bitmapSetCount b
      in if nulls /= declared
           then Left (what ++ ": validity null count " ++ show declared ++ " does not match the bitmap (" ++ show nulls ++ ")")
           else Right (if nulls == 0 then Nothing else Just (Validity b nulls))


{- | Whether row @i@ is valid. 'Nothing' is all valid. Errors when @i@
is outside the bitmap (like 'V.!').
-}
isValidAt :: Maybe Validity -> Int -> Bool
isValidAt Nothing _ = True
isValidAt (Just (Validity b _)) i = bitAt b i
{-# INLINE isValidAt #-}


-- | 'isValidAt' without the range check: one load, shift and mask.
unsafeIsValidAt :: Maybe Validity -> Int -> Bool
unsafeIsValidAt Nothing _ = True
unsafeIsValidAt (Just (Validity b _)) i = unsafeBitAt b i
{-# INLINE unsafeIsValidAt #-}


-- | Rows @[s, s + l)@ (caller clamps); recounts nulls with popcount.
sliceValidity :: Int -> Int -> Maybe Validity -> Maybe Validity
sliceValidity _ _ Nothing = Nothing
sliceValidity s l (Just (Validity b _)) = mkValidity (sliceBitmap s l b)
{-# INLINE sliceValidity #-}


{- | Concatenate the validity of consecutive pieces, each given with
its row count. All-'Nothing' stays 'Nothing'; otherwise one fresh
bitmap (bit copies, all-valid runs set in bulk).
-}
concatValidity :: [(Int, Maybe Validity)] -> Maybe Validity
concatValidity pieces
  | all (\(_, v) -> null v) pieces = Nothing
  | otherwise =
      let !total = sum (map fst pieces)
          !nulls = sum (map (\(_, v) -> maybe 0 validityNullCount v) pieces)
          !nbytes = (total + 7) `unsafeShiftR` 3
          bs = createAligned nbytes $ \dst -> do
            fillBytes dst 0 nbytes
            let go !_ [] = pure ()
                go !pos ((r, v) : rest) = do
                  case v of
                    Nothing -> setBitRange dst pos r
                    Just (Validity (Bitmap b o _) _) -> withBytesPtr b $ \src -> copyBits dst pos src o r
                  go (pos + r) rest
            go 0 pieces
      in if nulls == 0 then Nothing else Just (Validity (Bitmap bs 0 total) nulls)


-- | Gather validity by index (indices already range-checked).
takeValidity :: VS.Vector Int -> Maybe Validity -> Maybe Validity
takeValidity _ Nothing = Nothing
takeValidity ix (Just (Validity b _)) =
  let !(b', set) = takeBitmap ix b
      !nulls = VS.length ix - set
  in if nulls == 0 then Nothing else Just (Validity b' nulls)


-- | Row-wise AND of two validities of the same length.
andValidity :: Maybe Validity -> Maybe Validity -> Maybe Validity
andValidity Nothing v = v
andValidity v Nothing = v
andValidity (Just (Validity (Bitmap ba oa la) _)) (Just (Validity (Bitmap bb ob _) _)) =
  let !nbytes = (la + 7) `unsafeShiftR` 3
  in unsafeDupablePerformIO $ do
       fp <- mallocAligned nbytes
       set <- unsafeWithForeignPtr fp $ \dst ->
         withBytesPtr ba $ \pa -> withBytesPtr bb $ \pb -> andBits dst pa oa pb ob la
       let !nulls = la - set
       pure (if nulls == 0 then Nothing else Just (Validity (Bitmap (BSI.BS fp nbytes) 0 la) nulls))


-- | Detached copy at bit offset 0.
copyValidity :: Maybe Validity -> Maybe Validity
copyValidity = fmap (\(Validity b n) -> Validity (copyBitmap b) n)


-- ============================================================
-- Element types
-- ============================================================

-- | IEEE 754 binary16, kept as its bit pattern.
newtype Float16 = Float16 Word16
  deriving stock (Show)
  deriving newtype (Eq, Ord, Storable)


-- | binary16 to 'Double' (exact).
float16ToDouble :: Float16 -> Double
float16ToDouble (Float16 w) =
  let !sign = if w .&. 0x8000 /= 0 then -1 else 1
      !ex = fromIntegral ((w `shiftR` 10) .&. 0x1f) :: Int
      !mant = fromIntegral (w .&. 0x3ff) :: Double
  in case ex of
       0 -> sign * mant * 2 ** (-24)
       31 -> if mant == 0 then sign * (1 / 0) else 0 / 0
       _ -> sign * (1 + mant / 1024) * 2 ^^ (ex - 15)


-- | Arrow @INTERVAL(DAY_TIME)@: days, milliseconds (8 bytes).
data IntervalDayTime = IntervalDayTime {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32
  deriving stock (Show, Eq, Ord)


instance Storable IntervalDayTime where
  sizeOf _ = 8
  alignment _ = 4
  peek p = IntervalDayTime <$> peekByteOff p 0 <*> peekByteOff p 4
  poke p (IntervalDayTime d m) = pokeByteOff p 0 d >> pokeByteOff p 4 m
  {-# INLINE peek #-}
  {-# INLINE poke #-}


-- | Arrow @INTERVAL(MONTH_DAY_NANO)@: months, days, nanoseconds (16 bytes).
data IntervalMonthDayNano = IntervalMonthDayNano {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int64
  deriving stock (Show, Eq, Ord)


instance Storable IntervalMonthDayNano where
  sizeOf _ = 16
  alignment _ = 8
  peek p = IntervalMonthDayNano <$> peekByteOff p 0 <*> peekByteOff p 4 <*> peekByteOff p 8
  poke p (IntervalMonthDayNano m d ns) = pokeByteOff p 0 m >> pokeByteOff p 4 d >> pokeByteOff p 8 ns
  {-# INLINE peek #-}
  {-# INLINE poke #-}


-- | 128-bit little-endian two's complement decimal payload: low word, high word.
data Decimal128 = Decimal128 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving stock (Eq)


instance Show Decimal128 where
  showsPrec d = showsPrec d . decimal128ToInteger


instance Storable Decimal128 where
  sizeOf _ = 16
  alignment _ = 8
  peek p = Decimal128 <$> peekByteOff p 0 <*> peekByteOff p 8
  poke p (Decimal128 lo hi) = pokeByteOff p 0 lo >> pokeByteOff p 8 hi
  {-# INLINE peek #-}
  {-# INLINE poke #-}


-- | Two's complement order.
instance Ord Decimal128 where
  compare a b = compare (decimal128ToInteger a) (decimal128ToInteger b)


decimal128ToInteger :: Decimal128 -> Integer
decimal128ToInteger (Decimal128 lo hi) =
  toInteger (fromIntegral hi :: Int64) `shiftL` 64 + toInteger lo


-- | Wraps modulo 2^128.
decimal128FromInteger :: Integer -> Decimal128
decimal128FromInteger x = Decimal128 (fromInteger x) (fromInteger (x `shiftR` 64))


-- | 256-bit little-endian two's complement decimal payload, least significant word first.
data Decimal256 = Decimal256 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving stock (Eq)


instance Show Decimal256 where
  showsPrec d = showsPrec d . decimal256ToInteger


instance Storable Decimal256 where
  sizeOf _ = 32
  alignment _ = 8
  peek p = Decimal256 <$> peekByteOff p 0 <*> peekByteOff p 8 <*> peekByteOff p 16 <*> peekByteOff p 24
  poke p (Decimal256 a b c d) = pokeByteOff p 0 a >> pokeByteOff p 8 b >> pokeByteOff p 16 c >> pokeByteOff p 24 d
  {-# INLINE peek #-}
  {-# INLINE poke #-}


instance Ord Decimal256 where
  compare a b = compare (decimal256ToInteger a) (decimal256ToInteger b)


decimal256ToInteger :: Decimal256 -> Integer
decimal256ToInteger (Decimal256 w0 w1 w2 w3) =
  toInteger (fromIntegral w3 :: Int64) `shiftL` 192
    + toInteger w2 `shiftL` 128
    + toInteger w1 `shiftL` 64
    + toInteger w0


-- | Wraps modulo 2^256.
decimal256FromInteger :: Integer -> Decimal256
decimal256FromInteger x =
  Decimal256 (fromInteger x) (fromInteger (x `shiftR` 64)) (fromInteger (x `shiftR` 128)) (fromInteger (x `shiftR` 192))
