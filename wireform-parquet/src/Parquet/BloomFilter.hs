{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Apache Parquet split-block bloom filter (SBBF), parquet-format 2.10+.
--
-- See <https://parquet.apache.org/docs/file-format/bloomfilter/> for the
-- spec. SBBF stores a bit array as a sequence of 256-bit blocks, each of
-- which contains 8 32-bit words. Hashing uses XXH64 with seed 0 over the
-- value's PLAIN encoding ("Wireform.Hash"'s seed-zero @xxh64@).
--
-- The on-disk layout is:
--
-- @
-- BloomFilterHeader (Thrift Compact)
--   1: required i32 numBytes
--   2: required BloomFilterAlgorithm  algorithm  (always BLOCK)
--   3: required BloomFilterHash       hash       (always XXHASH)
--   4: required BloomFilterCompression compression (always UNCOMPRESSED)
-- bitset bytes (numBytes wide)
-- @
--
-- This module gives:
--
-- * 'newSbbf' / 'sbbfInsertHash' / 'sbbfCheckHash' for the in-memory filter
-- * 'sbbfInsert' / 'sbbfCheck' for direct value insertion (XXH64 internal)
-- * 'encodeBloomFilter' / 'decodeBloomFilter' for the spec's wire format
-- * 'optimalNumBytes' for sizing helpers
--
-- Hot paths use 'Data.Vector.Unboxed.Mutable.Vector Word64' for the
-- bitset, four 64-bit reads/writes per block, and avoid all 'Maybe'
-- allocation in the inner kernel.
module Parquet.BloomFilter
  ( -- * Filter
    Sbbf
  , newSbbf
  , newSbbfBytes
  , newSbbfFromBytes
  , sbbfNumBytes
  , sbbfInsert
  , sbbfCheck
  , sbbfInsertHash
  , sbbfCheckHash
    -- * Bulk build API (avoids the O(n²) thaw-per-insert)
  , buildSbbfFromHashes
  , buildSbbfFromBytes
    -- * Wire format
  , BloomFilterHeader (..)
  , encodeBloomFilter
  , decodeBloomFilter
    -- * Sizing
  , optimalNumBytes
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, unsafeShiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int16, Int32)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word32, Word64)

import qualified Wireform.Hash as Hash

import Parquet.Thrift.Schema
import qualified Thrift.Value as TV
import Thrift.Decode (decodeCompact)
import Thrift.Encode (encodeCompact)

-- | XXH64 with the Parquet bloom-filter default seed (@0@).
xxh64 :: ByteString -> Word64
xxh64 = Hash.xxh64 0
{-# INLINE xxh64 #-}

-- | Eight 32-bit "salt" multipliers from the Parquet SBBF spec. Each
-- salt is an odd 32-bit integer chosen to spread set bits well across
-- the eight words of one block.
salts :: VU.Vector Word32
salts = VU.fromListN 8
  [ 0x47b6137b, 0x44974d91, 0x8824ad5b, 0xa2b7289d
  , 0x705495c7, 0x2df1424b, 0x9efc4947, 0x5c6bfb31
  ]

-- | Block size in bytes (256 bits = eight 32-bit words = four 64-bit words).
blockBytes :: Int
blockBytes = 32

-- | A split-block bloom filter, stored as @numBytes / 8@ packed
-- 64-bit words. Bit ordering inside each block follows the Parquet spec
-- (block is eight 32-bit words, word @i@ stored at byte offset @4*i@,
-- little-endian).
data Sbbf = Sbbf
  { sbbfBytes  :: {-# UNPACK #-} !Int     -- ^ 'numBytes' in the header
  , sbbfBlocks :: {-# UNPACK #-} !Int     -- ^ derived: 'sbbfBytes' / 32
  , sbbfData   :: {-# UNPACK #-} !(VU.Vector Word64)
  } deriving stock (Show, Eq)

-- | Create a new SBBF with @numBytes@ bytes (rounded up to a multiple of 32).
-- All bits start at zero. Use 'optimalNumBytes' to pick a sensible size.
newSbbf :: Int -> Sbbf
newSbbf numBytes =
  let !nb = roundUpBlock (max blockBytes numBytes)
  in newSbbfBytes nb VU.empty
  where
    -- 'newSbbfBytes' takes raw words for cases where the bitset already
    -- exists (decode path); here we want a freshly zeroed bitset.

-- | Internal: build an 'Sbbf' from already-decoded 64-bit words.
{-# INLINE newSbbfBytes #-}
newSbbfBytes :: Int -> VU.Vector Word64 -> Sbbf
newSbbfBytes nb ws
  | VU.null ws =
      let !blocks = nb `quot` blockBytes
          !words_ = blocks * 4
      in Sbbf
           { sbbfBytes = nb
           , sbbfBlocks = blocks
           , sbbfData = VU.replicate words_ 0
           }
  | otherwise =
      let !blocks = nb `quot` blockBytes
      in Sbbf
           { sbbfBytes = nb
           , sbbfBlocks = blocks
           , sbbfData = ws
           }

-- | Build a filter from an already-serialised bitset (without the
-- 'BloomFilterHeader' prefix). Useful for golden tests that exercise
-- the SBBF check path against bytes produced by parquet-mr / arrow-rs.
newSbbfFromBytes :: ByteString -> Sbbf
newSbbfFromBytes bs =
  let !nb = BS.length bs - (BS.length bs `rem` blockBytes)
      !ws = parseBitset (BS.take nb bs)
  in newSbbfBytes nb ws

roundUpBlock :: Int -> Int
roundUpBlock n =
  let r = n `rem` blockBytes
  in if r == 0 then n else n + (blockBytes - r)

-- | Number of bytes in the bitset (matches 'numBytes' in the header).
sbbfNumBytes :: Sbbf -> Int
sbbfNumBytes = sbbfBytes

-- | Insert a value (encoded as raw bytes — usually the column's PLAIN
-- physical layout) into the filter.
{-# INLINE sbbfInsert #-}
sbbfInsert :: ByteString -> Sbbf -> Sbbf
sbbfInsert bs = sbbfInsertHash (xxh64 bs)

-- | Test whether a value /might/ be in the filter. False positives
-- are possible; false negatives are not.
{-# INLINE sbbfCheck #-}
sbbfCheck :: ByteString -> Sbbf -> Bool
sbbfCheck bs = sbbfCheckHash (xxh64 bs)

-- | Insert by raw 64-bit hash (callers supply XXH64 themselves).
sbbfInsertHash :: Word64 -> Sbbf -> Sbbf
sbbfInsertHash !h sbbf =
  let !blocks = sbbfBlocks sbbf
      !blockIdx = blockIndex h blocks
      !x = fromIntegral h :: Word32
      !ws = sbbfData sbbf
      !ws' = updateBlock ws blockIdx x
  in sbbf { sbbfData = ws' }

-- | Check by raw 64-bit hash.
sbbfCheckHash :: Word64 -> Sbbf -> Bool
sbbfCheckHash !h sbbf =
  let !blocks = sbbfBlocks sbbf
      !blockIdx = blockIndex h blocks
      !x = fromIntegral h :: Word32
  in checkBlock (sbbfData sbbf) blockIdx x

{-# INLINE blockIndex #-}
blockIndex :: Word64 -> Int -> Int
blockIndex h blocks =
  let !top = h `shiftR` 32
  in fromIntegral ((top * fromIntegral blocks) `shiftR` 32) :: Int

-- | Apply the @mask(x)@ to a single 32-bit lane @i@. Each word ends up
-- with exactly one bit set: bit @((x * salt[i]) >> 27)@.
{-# INLINE maskBitForLane #-}
maskBitForLane :: Word32 -> Int -> Word32
maskBitForLane x i =
  let !s = VU.unsafeIndex salts i
      !y = x * s
      !idx = fromIntegral (y `shiftR` 27) :: Int
  in 1 `shiftL` idx

-- | The previous shape was
--
--   updateBlock ws blockIdx x = runST $ do
--     mv <- VU.thaw ws            -- O(|ws|) COPY OF THE WHOLE BITSET
--     ...write 4 words...
--     VU.unsafeFreeze mv          -- another O(|ws|) copy
--
-- which made each 'sbbfInsert' O(bitset bytes). For 100 k
-- inserts into a 16 KB filter that's 100 k × 16 KB = ~1.6 GB
-- of pure memcpy work — pathological.
--
-- The replacement keeps the existing pure-API
-- ('updateBlock'/'sbbfInsertHash') path for backwards
-- compatibility and as a small-N convenience, but provides
-- 'updateBlockM' for in-place mutation. Bulk callers should
-- use 'buildSbbfFromHashes' / 'buildSbbfFromBytes' which thaw
-- once, write all values, freeze once.
updateBlock :: VU.Vector Word64 -> Int -> Word32 -> VU.Vector Word64
updateBlock ws !blockIdx !x = runST $ do
  mv <- VU.thaw ws
  updateBlockM mv blockIdx x
  VU.unsafeFreeze mv

-- | In-place block update — used by the bulk-build path.
{-# INLINE updateBlockM #-}
updateBlockM :: MVU.MVector s Word64 -> Int -> Word32 -> ST s ()
updateBlockM mv !blockIdx !x = do
  let !base = blockIdx * 4
  -- Block is 8 lanes -> packed as 4 words: lane0|lane1, lane2|lane3, ...
  applyLanePair mv (base + 0) x 0 1
  applyLanePair mv (base + 1) x 2 3
  applyLanePair mv (base + 2) x 4 5
  applyLanePair mv (base + 3) x 6 7
  where
    applyLanePair :: MVU.MVector s Word64 -> Int -> Word32 -> Int -> Int -> ST s ()
    applyLanePair mv' idx !x' lo hi = do
      !cur <- MVU.unsafeRead mv' idx
      let !mLo = fromIntegral (maskBitForLane x' lo) :: Word64
          !mHi = fromIntegral (maskBitForLane x' hi) :: Word64
          !packed = mLo .|. (mHi `unsafeShiftL` 32)
          !new    = cur .|. packed
      MVU.unsafeWrite mv' idx new

-- | Build an 'Sbbf' by inserting an entire vector of 64-bit
-- hashes in one ST pass: thaw once, write all, freeze once.
-- O(values + bitset bytes) total work — vs O(values × bitset
-- bytes) for the naive @V.foldl' sbbfInsertHash empty@.
{-# INLINE buildSbbfFromHashes #-}
buildSbbfFromHashes
  :: Int                  -- ^ bitset size in bytes (rounded up to a block)
  -> VU.Vector Word64     -- ^ pre-computed XXH64 hashes
  -> Sbbf
buildSbbfFromHashes nBytesIn hashes =
  let !nBytes = roundUpBlock (max blockBytes nBytesIn)
      !blocks = nBytes `quot` blockBytes
      !words_ = blocks * 4
  in Sbbf
       { sbbfBytes  = nBytes
       , sbbfBlocks = blocks
       , sbbfData   = VU.create $ do
           mv <- MVU.replicate words_ 0
           let go !i
                 | i >= VU.length hashes = pure ()
                 | otherwise = do
                     let !h = VU.unsafeIndex hashes i
                         !blockIdx = blockIndex h blocks
                         !x = fromIntegral h :: Word32
                     updateBlockM mv blockIdx x
                     go (i + 1)
           go 0
           pure mv
       }

-- | Build an 'Sbbf' by hashing + inserting each value in a
-- vector of byte-payloads. Computes XXH64 inline so we don't
-- materialise an intermediate hash vector.
{-# INLINE buildSbbfFromBytes #-}
buildSbbfFromBytes
  :: Int                 -- ^ bitset size in bytes
  -> V.Vector ByteString
  -> Sbbf
buildSbbfFromBytes nBytesIn vs =
  let !nBytes = roundUpBlock (max blockBytes nBytesIn)
      !blocks = nBytes `quot` blockBytes
      !words_ = blocks * 4
  in Sbbf
       { sbbfBytes  = nBytes
       , sbbfBlocks = blocks
       , sbbfData   = VU.create $ do
           mv <- MVU.replicate words_ 0
           let !n = V.length vs
               go !i
                 | i >= n = pure ()
                 | otherwise = do
                     let !h = xxh64 (V.unsafeIndex vs i)
                         !blockIdx = blockIndex h blocks
                         !x = fromIntegral h :: Word32
                     updateBlockM mv blockIdx x
                     go (i + 1)
           go 0
           pure mv
       }

checkBlock :: VU.Vector Word64 -> Int -> Word32 -> Bool
checkBlock ws !blockIdx !x =
  let !base = blockIdx * 4
      laneOk lo hi =
        let !cur = VU.unsafeIndex ws (base + (lo `quot` 2))
            !mLo = fromIntegral (maskBitForLane x lo) :: Word64
            !mHi = fromIntegral (maskBitForLane x hi) :: Word64
            !packed = mLo .|. (mHi `unsafeShiftL` 32)
        in (cur .&. packed) == packed
  in    laneOk 0 1
     && laneOk 2 3
     && laneOk 4 5
     && laneOk 6 7

-- ============================================================
-- Wire format
-- ============================================================

-- | Decoded @BloomFilterHeader@ struct. Algorithm and hash are
-- enums-of-one in the current spec; we preserve them so that future
-- variants don't silently round-trip into BLOCK/XXHASH.
data BloomFilterHeader = BloomFilterHeader
  { bfhNumBytes :: {-# UNPACK #-} !Int32
  , bfhAlgorithm :: !BloomFilterAlgorithm
  , bfhHash      :: !BloomFilterHash
  , bfhCompression :: !BloomFilterCompression
  } deriving stock (Show, Eq)

data BloomFilterAlgorithm = AlgBlock
  deriving stock (Show, Eq)
data BloomFilterHash = HashXxh64
  deriving stock (Show, Eq)
data BloomFilterCompression = CompUncompressed
  deriving stock (Show, Eq)

-- | Build the on-disk byte string: Thrift-Compact 'BloomFilterHeader'
-- followed by the bitset.
encodeBloomFilter :: Sbbf -> ByteString
encodeBloomFilter sbbf =
  let !hdr = BloomFilterHeader
              { bfhNumBytes = fromIntegral (sbbfBytes sbbf)
              , bfhAlgorithm = AlgBlock
              , bfhHash = HashXxh64
              , bfhCompression = CompUncompressed
              }
      !hdrBs = encodeCompact (bloomFilterHeaderToThrift hdr)
      !bitsetBs = serializeBitset (sbbfData sbbf) (sbbfBytes sbbf)
  in hdrBs <> bitsetBs

-- | Read an on-disk bloom filter (header + bitset). Validates that
-- @numBytes@ matches what the bitset contains.
decodeBloomFilter :: ByteString -> Either String (BloomFilterHeader, Sbbf)
decodeBloomFilter bs = do
  -- Thrift Compact does not record total length; we serialise the header
  -- with @encodeCompact@, then continue consuming bytes for the bitset.
  -- Use a streaming approach: try increasing prefixes until decode succeeds.
  -- In practice the header is short (~12 bytes); a linear scan is fine.
  (hdr, rest) <- splitHeader bs
  let !nb = fromIntegral (bfhNumBytes hdr) :: Int
  if BS.length rest < nb
    then Left $
      "Parquet.BloomFilter: bitset truncated (need " ++ show nb
        ++ " bytes, have " ++ show (BS.length rest) ++ ")"
    else do
      let !bitsetBs = BS.take nb rest
          !words_ = parseBitset bitsetBs
          !sbbf = newSbbfBytes nb words_
      Right (hdr, sbbf)

-- | Try to parse the Thrift Compact 'BloomFilterHeader' off the front
-- of @bs@ and return @(header, bytesAfterHeader)@.
splitHeader :: ByteString -> Either String (BloomFilterHeader, ByteString)
splitHeader bs = go 8
  where
    !maxHdr = min (BS.length bs) 64
    go !n
      | n > maxHdr = Left "Parquet.BloomFilter: could not parse header within first 64 bytes"
      | otherwise = case decodeCompact (BS.take n bs) of
          Left _ -> go (n + 1)
          Right v -> case thriftToBloomFilterHeader v of
            Left _ -> go (n + 1)
            Right hdr -> Right (hdr, BS.drop n bs)

bloomFilterHeaderToThrift :: BloomFilterHeader -> TV.Value
bloomFilterHeaderToThrift hdr = TV.Struct $ V.fromList
  [ BloomFilterHeader_NumBytes    (bfhNumBytes hdr)
  , BloomFilterHeader_Algorithm   (unionVariant1)   -- BLOCK
  , BloomFilterHeader_Hash        (unionVariant1)   -- XXHASH
  , BloomFilterHeader_Compression (unionVariant1)   -- UNCOMPRESSED
  ]
  where
    -- Parquet's three bloom-filter enums are each encoded as a Thrift
    -- union; we only support the first variant of each (BLOCK /
    -- XXHASH / UNCOMPRESSED), so the nested struct always has field 1
    -- set to an empty struct.
    unionVariant1 :: V.Vector (Int16, TV.Value)
    unionVariant1 = V.singleton (1, TV.Struct V.empty)

thriftToBloomFilterHeader :: TV.Value -> Either String BloomFilterHeader
thriftToBloomFilterHeader (TV.Struct fields) = do
  let fm = V.toList fields
  nb <- case findField fm (\case
          BloomFilterHeader_NumBytes v -> Just v
          _                            -> Nothing) of
    Just v  -> Right v
    Nothing -> Left "Parquet.BloomFilter: missing numBytes"
  alg <- requireUnionVariant1 fm "algorithm"
    (\case BloomFilterHeader_Algorithm fs -> Just fs; _ -> Nothing)
    AlgBlock
    "BloomFilterAlgorithm"
  hsh <- requireUnionVariant1 fm "hash"
    (\case BloomFilterHeader_Hash fs -> Just fs; _ -> Nothing)
    HashXxh64
    "BloomFilterHash"
  cmp <- requireUnionVariant1 fm "compression"
    (\case BloomFilterHeader_Compression fs -> Just fs; _ -> Nothing)
    CompUncompressed
    "BloomFilterCompression"
  Right BloomFilterHeader
    { bfhNumBytes = nb
    , bfhAlgorithm = alg
    , bfhHash = hsh
    , bfhCompression = cmp
    }
thriftToBloomFilterHeader _ =
  Left "Parquet.BloomFilter: expected struct for BloomFilterHeader"

-- | Match a nested union struct that carries exactly one field with id
-- @1@, return the corresponding Haskell enum value, and produce a
-- uniform error for missing or unsupported variants.
requireUnionVariant1
  :: [(Int16, TV.Value)]
  -> String
  -> ((Int16, TV.Value) -> Maybe (V.Vector (Int16, TV.Value)))
  -> a
  -> String
  -> Either String a
requireUnionVariant1 fm missingLabel probe variant unionName =
  case findField fm probe of
    Just branches -> case V.toList branches of
      [(1, _)] -> Right variant
      _        -> Left $ "Parquet.BloomFilter: unsupported "
                      ++ unionName ++ " union variant"
    Nothing -> Left $ "Parquet.BloomFilter: missing " ++ missingLabel

serializeBitset :: VU.Vector Word64 -> Int -> ByteString
serializeBitset ws nb =
  BL.toStrict $ B.toLazyByteString $ goWords 0
  where
    !nWords = nb `quot` 8
    goWords !i
      | i >= nWords = mempty
      | otherwise =
          let !w = VU.unsafeIndex ws i
          in B.word64LE w <> goWords (i + 1)

parseBitset :: ByteString -> VU.Vector Word64
parseBitset bs =
  let !n = BS.length bs `quot` 8
  in VU.generate n $ \i ->
       readLE64 bs (i * 8)

{-# INLINE readLE64 #-}
readLE64 :: ByteString -> Int -> Word64
readLE64 bs !off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
  in rd 0
   .|. (rd 1 `unsafeShiftL` 8)
   .|. (rd 2 `unsafeShiftL` 16)
   .|. (rd 3 `unsafeShiftL` 24)
   .|. (rd 4 `unsafeShiftL` 32)
   .|. (rd 5 `unsafeShiftL` 40)
   .|. (rd 6 `unsafeShiftL` 48)
   .|. (rd 7 `unsafeShiftL` 56)

-- ============================================================
-- Sizing
-- ============================================================

-- | Recommended @numBytes@ given the expected number of distinct values
-- @n@ and a target false-positive rate @fpp@ (e.g. @0.01@). The result
-- is rounded up to the next multiple of the SBBF block size (32 bytes).
--
-- Uses the closed-form approximation
-- @bits = ceil(-n * ln(fpp) / (ln(2) ^ 2))@, then clamps to at least one
-- block. The Parquet reference implementations agree to within one block
-- for typical inputs.
optimalNumBytes :: Int -> Double -> Int
optimalNumBytes n fpp
  | n <= 0 = blockBytes
  | fpp <= 0 || fpp >= 1 = blockBytes
  | otherwise =
      let !bits = ceiling (negate (fromIntegral n) * log fpp / (ln2 * ln2)) :: Int
          !bytes = (bits + 7) `quot` 8
      in roundUpBlock (max blockBytes bytes)
  where
    ln2 = log 2 :: Double
