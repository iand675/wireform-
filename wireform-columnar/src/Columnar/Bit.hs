{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Bit-packed boolean storage that plugs into the @vector@
-- ecosystem.
--
-- 'Bit' is a 'Bool' newtype with an 'Unbox' instance that
-- packs eight values per byte (LSB-first, matching Arrow /
-- Parquet wire format). The full
-- 'Data.Vector.Unboxed.Vector' API ('VU.length', 'VU.map',
-- 'VU.foldl'', 'VU.zip', 'VU.toList', etc.) just works on
-- @VU.Vector Bit@.
--
-- == Why
--
-- Boolean columns are currently stored as @V.Vector Bool@ —
-- a /boxed/ vector where each element is a pointer to the
-- 'True' or 'False' singleton. For an N-row column that's
-- @N * sizeof(pointer) = 8 N@ bytes of cache footprint just
-- for the spine. With @VU.Vector Bit@ that drops to
-- @ceil(N/8)@ bytes — 64x less cache pressure for the same
-- data, and the Arrow / Parquet wire format is already in
-- this layout so reads can be a single memcpy.
--
-- == Slicing semantics
--
-- The 'Unbox' instance carries (@bitOffset@, @bitLength@,
-- @byteVector@) so 'VU.slice' / 'VU.unsafeSlice' work at
-- arbitrary bit boundaries — the bitOffset shifts the
-- per-element index by 'bitOffset' before extracting from
-- the byte array.
--
-- See 'docs/columnar-views-design.md' for the broader
-- "view-based column" plan that this module is the first
-- concrete piece of.
module Columnar.Bit
  ( Bit (..)

    -- * Bit-vector helpers
  , fromByteString
  , fromByteStringN
  , toByteString
  , countOnes

    -- * Mutable / unboxed re-exports for callers that want
    -- to be explicit about packing.
  , unBitVec
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
  ( clearBit
  , complement
  , popCount
  , setBit
  , testBit
  , unsafeShiftL
  , unsafeShiftR
  , (.&.)
  , (.|.)
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Storable (pokeByteOff)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word8)

-- | Newtype around 'Bool' with a packed 'Unbox' instance.
newtype Bit = Bit { unBit :: Bool }
  deriving stock (Eq, Ord, Show)

-- ------------------------------------------------------------------
-- Unbox instance
-- ------------------------------------------------------------------
-- The runtime representation is an unboxed primitive vector
-- of Word8 plus a (bitOffset, bitLength) view. Slicing
-- adjusts bitOffset / bitLength only; reads / writes do
-- @(bitOffset + i) / 8@ to find the byte and
-- @(bitOffset + i) `mod` 8@ for the bit within it.

data instance VUM.MVector s Bit
  = MV_Bit !Int !Int !(VPM.MVector s Word8)
    -- ^ @MV_Bit !bitOffset !bitLength !backingBytes@

data instance VU.Vector Bit
  = V_Bit !Int !Int !(VP.Vector Word8)

-- | Drop the (offset, length) framing and return the raw
-- backing primitive vector. Caller is responsible for
-- consulting the original 'VU.length' to know how many bits
-- are valid; the byte vector may include padding bits in the
-- final byte if @length `mod` 8 /= 0@.
unBitVec :: VU.Vector Bit -> (Int, Int, VP.Vector Word8)
unBitVec (V_Bit off len v) = (off, len, v)
{-# INLINE unBitVec #-}

instance VGM.MVector VU.MVector Bit where
  basicLength (MV_Bit _ n _) = n
  {-# INLINE basicLength #-}

  basicUnsafeSlice s l (MV_Bit off _ v) =
    -- Note: we keep the *whole* underlying byte vector — only
    -- the (offset, length) window changes. This avoids having
    -- to re-shift bytes when the slice doesn't fall on a byte
    -- boundary.
    MV_Bit (off + s) l v
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (MV_Bit _ _ a) (MV_Bit _ _ b) = VGM.basicOverlaps a b
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew n = do
    let !bytes = (n + 7) `unsafeShiftR` 3
    v <- VGM.basicUnsafeNew bytes
    -- Zero-initialise the backing bytes so unwritten bits
    -- read as False rather than indeterminate.
    VGM.basicInitialize v
    pure (MV_Bit 0 n v)
  {-# INLINE basicUnsafeNew #-}

  basicInitialize (MV_Bit _ _ v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}

  basicUnsafeRead (MV_Bit off _ v) i = do
    let !idx     = off + i
        !byteIdx = idx `unsafeShiftR` 3
        !bit     = idx .&. 7
    !w <- VGM.basicUnsafeRead v byteIdx
    pure (Bit (testBit w bit))
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MV_Bit off _ v) i (Bit b) = do
    let !idx     = off + i
        !byteIdx = idx `unsafeShiftR` 3
        !bit     = idx .&. 7
    !w <- VGM.basicUnsafeRead v byteIdx
    let !w' = if b then setBit w bit else clearBit w bit
    VGM.basicUnsafeWrite v byteIdx w'
  {-# INLINE basicUnsafeWrite #-}

  basicUnsafeCopy dst src = do
    -- Naive bit-by-bit copy when the windows don't share a
    -- bit-aligned shift. Optimised path (whole-byte copy)
    -- when both dst and src start on byte boundaries.
    let nDst = VGM.basicLength dst
    if nDst == 0
      then pure ()
      else case (dst, src) of
        (MV_Bit dOff _ dv, MV_Bit sOff _ sv)
          | dOff .&. 7 == 0 && sOff .&. 7 == 0 ->
              -- Both byte-aligned: bulk copy whole bytes,
              -- then OR the trailing partial byte (if any)
              -- in place.
              let !dByte = dOff `unsafeShiftR` 3
                  !sByte = sOff `unsafeShiftR` 3
                  !nWhole = nDst `unsafeShiftR` 3
                  !nTail  = nDst .&. 7
              in do
                VGM.basicUnsafeCopy
                  (VGM.basicUnsafeSlice dByte nWhole dv)
                  (VGM.basicUnsafeSlice sByte nWhole sv)
                if nTail == 0
                  then pure ()
                  else do
                    -- For the last (partial) byte, blend the
                    -- low nTail bits from src with the high
                    -- (8 - nTail) bits of the existing dst.
                    !srcByte <- VGM.basicUnsafeRead sv (sByte + nWhole)
                    !dstByte <- VGM.basicUnsafeRead dv (dByte + nWhole)
                    let !mask = (1 `unsafeShiftL` nTail) - 1
                        !out  = (srcByte .&. mask)
                              .|. (dstByte .&. complement mask)
                    VGM.basicUnsafeWrite dv (dByte + nWhole) out
        _ -> bitwiseCopy dst src 0 nDst
  {-# INLINE basicUnsafeCopy #-}

  basicUnsafeMove dst src
    -- Same-pointer / same-window case is a no-op.
    | not (VGM.basicOverlaps dst src) = VGM.basicUnsafeCopy dst src
    | otherwise = bitwiseCopy dst src 0 (VGM.basicLength dst)
  {-# INLINE basicUnsafeMove #-}

  basicUnsafeReplicate n (Bit b) = do
    let !bytes = (n + 7) `unsafeShiftR` 3
        !fill  = if b then 0xFF :: Word8 else 0x00
    v <- VGM.basicUnsafeReplicate bytes fill
    pure (MV_Bit 0 n v)
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeGrow (MV_Bit off n v) by = do
    -- We always grow by at least @by@ bits; if the byte
    -- vector doesn't have enough trailing capacity we copy
    -- into a new larger one.
    let !needed = (off + n + by + 7) `unsafeShiftR` 3
        !have   = VGM.basicLength v
    v' <- if needed <= have
            then pure v
            else do
              new <- VGM.basicUnsafeNew needed
              VGM.basicInitialize new
              VGM.basicUnsafeCopy
                (VGM.basicUnsafeSlice 0 have new) v
              pure new
    pure (MV_Bit off (n + by) v')
  {-# INLINE basicUnsafeGrow #-}

bitwiseCopy
  :: PrimMonad m
  => VUM.MVector (PrimState m) Bit
  -> VUM.MVector (PrimState m) Bit
  -> Int -> Int -> m ()
bitwiseCopy !dst !src !i !n
  | i >= n = pure ()
  | otherwise = do
      x <- VUM.unsafeRead src i
      VUM.unsafeWrite dst i x
      bitwiseCopy dst src (i + 1) n
{-# INLINE bitwiseCopy #-}

instance VG.Vector VU.Vector Bit where
  basicUnsafeFreeze (MV_Bit off n v) = do
    !v' <- VG.basicUnsafeFreeze v
    pure (V_Bit off n v')
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_Bit off n v) = do
    !v' <- VG.basicUnsafeThaw v
    pure (MV_Bit off n v')
  {-# INLINE basicUnsafeThaw #-}

  basicLength (V_Bit _ n _) = n
  {-# INLINE basicLength #-}

  basicUnsafeSlice s l (V_Bit off _ v) = V_Bit (off + s) l v
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_Bit off _ v) i =
    let !idx     = off + i
        !byteIdx = idx `unsafeShiftR` 3
        !bit     = idx .&. 7
    in do
      !w <- VG.basicUnsafeIndexM v byteIdx
      pure (Bit (testBit w bit))
  {-# INLINE basicUnsafeIndexM #-}

  basicUnsafeCopy mv (V_Bit off n v) = do
    -- Wrap the immutable source into an MV view + delegate
    -- to the mutable copy. Slightly wasteful if the source
    -- and destination are byte-aligned in different ways,
    -- but always correct.
    !sv <- VG.basicUnsafeThaw v
    VGM.basicUnsafeCopy mv (MV_Bit off n sv)
  {-# INLINE basicUnsafeCopy #-}

instance VU.Unbox Bit

-- ------------------------------------------------------------------
-- ByteString helpers
-- ------------------------------------------------------------------

-- | Wrap an Arrow / Parquet bit-packed bitmap (LSB-first,
-- one bit per value) as a 'VU.Vector Bit'.
--
-- The @n@ argument is the bit-count; the underlying
-- 'ByteString' must have at least @ceil(n / 8)@ bytes.
--
-- This currently /copies/ the bitmap bytes into a fresh
-- 'VP.Vector Word8'. Genuine zero-copy needs
-- 'Data.Vector.Storable' (which carries a 'ForeignPtr' and
-- so can adopt a 'ByteString''s storage directly); see
-- @docs/columnar-views-design.md@ for the migration.
fromByteStringN :: Int -> ByteString -> VU.Vector Bit
fromByteStringN n bs =
  let !needed = (n + 7) `unsafeShiftR` 3
      !len    = BS.length bs
      !use    = min needed len
      bytes   = bsToPrimVec (BS.take use bs)
  in V_Bit 0 n bytes
{-# INLINE fromByteStringN #-}

-- | Like 'fromByteStringN' but treat the entire 'ByteString'
-- as the bitmap (length = @8 * BS.length bs@).
fromByteString :: ByteString -> VU.Vector Bit
fromByteString bs = fromByteStringN (BS.length bs `unsafeShiftL` 3) bs
{-# INLINE fromByteString #-}

-- | Convert a bit-packed 'VU.Vector Bit' back to the
-- LSB-first 'ByteString' encoding Arrow / Parquet expect.
-- For byte-aligned vectors (offset divisible by 8) the
-- bytes are the literal storage; for misaligned vectors the
-- result is a fresh copy with the bits shifted into place.
toByteString :: VU.Vector Bit -> ByteString
toByteString (V_Bit off n v)
  | off .&. 7 == 0 =
      -- Byte-aligned: bytes are literal, just slice off
      -- the prefix and trim trailing bytes.
      let !startByte = off `unsafeShiftR` 3
          !numBytes  = (n + 7) `unsafeShiftR` 3
      in primVecToBs (VP.unsafeSlice startByte numBytes v)
  | otherwise =
      -- Misaligned: rebuild bytes by shifting.
      let !numBytes = (n + 7) `unsafeShiftR` 3
      in BSI.unsafeCreate numBytes $ \p -> writeShifted p v off n
  where
    writeShifted p v0 startBit nBits = goByte 0
      where
        goByte !b
          | b * 8 >= nBits = pure ()
          | otherwise =
              let acc = packByte b
                  !len = min 8 (nBits - b * 8)
              in do
                pokeByteOff p b (acc len 0 0 :: Word8)
                goByte (b + 1)
        packByte !b !taken !bit !w
          | bit >= taken = w
          | otherwise =
              let !srcBit = startBit + b * 8 + bit
                  !sByte  = srcBit `unsafeShiftR` 3
                  !sBit   = srcBit .&. 7
                  !flag   = (VP.unsafeIndex v0 sByte `unsafeShiftR` sBit) .&. 1
                  !w'     = w .|. (flag `unsafeShiftL` bit)
              in packByte b taken (bit + 1) w'

-- | popcount over a bit vector. Forwards to the SIMDe-backed
-- 'Columnar.SIMD.bitmapPopCount' for the byte-aligned full
-- buffer case; arbitrary slices fall back to a per-bit walk
-- (still O(n) but with no SIMD).
countOnes :: VU.Vector Bit -> Int
countOnes (V_Bit off n v)
  | off .&. 7 == 0 && n .&. 7 == 0 =
      -- Both endpoints byte-aligned: pop-count the whole
      -- byte run.
      let !startByte = off `unsafeShiftR` 3
          !numBytes  = n `unsafeShiftR` 3
          !slice     = VP.unsafeSlice startByte numBytes v
      in VP.foldl' (\acc w -> acc + popCount w) 0 slice
  | otherwise =
      -- Slow path: per-bit walk.
      let go !i !acc
            | i >= n = acc
            | otherwise =
                let !idx     = off + i
                    !byteIdx = idx `unsafeShiftR` 3
                    !bit     = idx .&. 7
                    !flag    = (VP.unsafeIndex v byteIdx
                                 `unsafeShiftR` bit) .&. 1
                in go (i + 1) (acc + fromIntegral flag)
      in go 0 0
{-# INLINE countOnes #-}

-- ------------------------------------------------------------------
-- ByteString <-> primitive Vector (no copy)
-- ------------------------------------------------------------------

-- | Copy a 'ByteString' into a fresh @VP.Vector Word8@.
--
-- 'VP.Vector' wraps a 'Data.Primitive.ByteArray' (GC-managed
-- pinned-or-unpinned heap), not a 'ForeignPtr', so we can't
-- adopt the bytestring's existing storage. The migration
-- plan in @docs/columnar-views-design.md@ uses
-- 'Data.Vector.Storable' instead, which /can/ adopt a
-- 'ForeignPtr' — that's the path that gets us actual
-- zero-copy reads.
bsToPrimVec :: ByteString -> VP.Vector Word8
bsToPrimVec bs = VP.generate (BS.length bs) (BSU.unsafeIndex bs)
{-# INLINE bsToPrimVec #-}

primVecToBs :: VP.Vector Word8 -> ByteString
primVecToBs v =
  let !len = VP.length v
  in BSI.unsafeCreate len $ \p ->
       let go !i
             | i >= len = pure ()
             | otherwise = do
                 pokeByteOff p i (VP.unsafeIndex v i)
                 go (i + 1)
       in go 0
{-# INLINE primVecToBs #-}
