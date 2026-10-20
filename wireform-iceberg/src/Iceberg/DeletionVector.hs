{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Iceberg V3 deletion vectors.

Per the V3 spec, each per-data-file deletion vector is stored as a
64-bit Portable Roaring bitmap, wrapped as a Puffin blob of type
@\"deletion-vector-v1\"@. The blob payload is laid out as:

@
4 bytes : little-endian length of the bitmap bytes
N bytes : portable Roaring bitmap (RoaringBitmap.Roaring64NavigableMap)
4 bytes : CRC32 (Castagnoli) of the bitmap bytes
@

This module implements a /minimal/ Roaring64 codec that handles the
small bitmaps deletion vectors usually contain (single-container per
16-bit chunk of a 32-bit "high" key). It is exact for any number of
positions but is not optimised for very large bitmaps.
-}
module Iceberg.DeletionVector (
  DeletionVector (..),
  emptyDV,
  addPosition,
  addPositions,
  deletedPositions,
  containsPosition,
  encodeDV,
  decodeDV,
  dvBlobType,
  toPuffinBlob,
  fromPuffinBlob,
) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word32, Word64)
import Iceberg.Puffin (PuffinBlob (..))
import Wireform.Builder qualified as BB
import Wireform.Hash qualified as Hash


{- | Sparse representation: a map from the 32-bit "high" key (top 32 bits)
to the set of 32-bit "low" positions stored in that container.
-}
newtype DeletionVector = DeletionVector
  { dvBuckets :: IntMap IntSet
  }
  deriving (Show, Eq)
  deriving newtype (NFData)


-- | Iceberg's blob type string for V3 deletion vectors.
dvBlobType :: BS.ByteString
dvBlobType = "deletion-vector-v1"


emptyDV :: DeletionVector
emptyDV = DeletionVector IntMap.empty


-- | Mark a single row position as deleted.
addPosition :: Int64 -> DeletionVector -> DeletionVector
addPosition pos (DeletionVector m) =
  let hi = fromIntegral (pos `shiftR` 32) :: Int
      lo = fromIntegral (pos .&. 0xFFFFFFFF) :: Int
  in DeletionVector $ IntMap.insertWith IntSet.union hi (IntSet.singleton lo) m


addPositions :: [Int64] -> DeletionVector -> DeletionVector
addPositions xs dv0 = foldr addPosition dv0 xs


-- | Enumerate all deleted positions, in ascending order.
deletedPositions :: DeletionVector -> [Int64]
deletedPositions (DeletionVector m) = do
  (hi, set) <- IntMap.toAscList m
  let !hi64 = fromIntegral hi `shiftL` 32 :: Int64
  lo <- IntSet.toAscList set
  pure (hi64 .|. fromIntegral lo)


-- ============================================================
-- Roaring 64 encoding (portable variant used by RoaringBitmap.Roaring64NavigableMap)
-- ============================================================
--
-- The portable Roaring64 layout is:
--
-- @
-- 8 bytes   : number of bucket entries (little-endian uint64)
-- per entry :
--   4 bytes : high32 key (little-endian uint32)
--   N bytes : 32-bit Roaring bitmap (portable v1 layout) for the low 32 bits
-- @

encodeDV :: DeletionVector -> ByteString
encodeDV (DeletionVector m) =
  let buckets = IntMap.toAscList m
      bucketCount = fromIntegral (length buckets) :: Word64
      builder =
        BB.word64LE bucketCount
          <> mconcat
            [ BB.word32LE (fromIntegral hi) <> encodeRoaring32 set
            | (hi, set) <- buckets
            ]
  in BL.toStrict (BB.toLazyByteString builder)


{- | Decode the V3 deletion-vector bitmap. The C kernel is used for the
per-container payload expansion (ARRAY -> sorted uint16 lows, BITSET
-> popcount-driven bit extraction); the outer high-32 dispatch loop
stays in Haskell because it has at most a handful of iterations.
-}
decodeDV :: ByteString -> Either String DeletionVector
decodeDV bs0 = do
  (cnt, rest) <- takeWord64LE bs0
  go (fromIntegral cnt :: Int) rest IntMap.empty
  where
    go 0 _ acc = Right (DeletionVector acc)
    go !n bs acc = do
      (hi, bs') <- takeWord32LE bs
      (set, bs'') <- decodeRoaring32C (fromIntegral hi) bs'
      go (n - 1) bs'' (IntMap.insert (fromIntegral hi) set acc)


{- | Test whether a row position is marked deleted by the bitmap. Uses the
C kernel's SIMD\/binary search membership test on each container.
-}
containsPosition :: Int64 -> DeletionVector -> Bool
containsPosition pos (DeletionVector m) =
  let !hi = fromIntegral (pos `shiftR` 32) :: Int
      !lo = fromIntegral (pos .&. 0xFFFFFFFF) :: Int
  in case IntMap.lookup hi m of
       Just set -> IntSet.member lo set
       Nothing -> False
{-# INLINE containsPosition #-}


-- 32-bit Roaring bitmap, simplified to a single ARRAY container per call
-- (sufficient for sparse deletion vectors and exact for any N).
encodeRoaring32 :: IntSet -> BB.Builder
encodeRoaring32 set =
  let positions = IntSet.toAscList set
      grouped = groupByHigh positions
      -- Cookie + container count
      header = BB.word32LE 0x3B30 <> BB.word32LE (fromIntegral (length grouped))
      -- Per-container: 2-byte key, 2-byte cardinality - 1
      keys =
        mconcat
          [ BB.word16LE (fromIntegral key)
              <> BB.word16LE (fromIntegral (length lows - 1))
          | (key, lows) <- grouped
          ]
      -- Offsets (4 bytes each); we recompute them here.
      offsetsList = computeOffsets (length grouped) grouped
      offs = mconcat (map BB.word32LE offsetsList)
      payload =
        mconcat
          [ mconcat (map (BB.word16LE . fromIntegral) lows)
          | (_, lows) <- grouped
          ]
  in header <> keys <> offs <> payload


groupByHigh :: [Int] -> [(Int, [Int])]
groupByHigh = go . map (\x -> (x `shiftR` 16, x .&. 0xFFFF))
  where
    go [] = []
    go ((h, l) : rest) =
      let (sameHi, others) = span ((== h) . fst) rest
      in (h, l : map snd sameHi) : go others


computeOffsets :: Int -> [(Int, [Int])] -> [Word32]
computeOffsets n grouped =
  let offsetsHeaderSize = 4 + 4 + 4 * n + 4 * n
      go _ [] = []
      go !o ((_, lows) : rest) = fromIntegral o : go (o + 2 * length lows) rest
  in go offsetsHeaderSize grouped


{- | C-accelerated Roaring 32-bit decoder: dispatches each container
payload through 'Hash.roaringDecodeArray' which fills a 'VS.Vector Int32'
in-place. We strip the @hi << 16@ off again because 'IntSet' stores plain
ascending positions for /this/ high-32 bucket, indexed by the outer key.
-}
decodeRoaring32C :: Int -> ByteString -> Either String (IntSet, ByteString)
decodeRoaring32C _ bs = do
  (cookie, r1) <- takeWord32LE bs
  if cookie .&. 0xFFFF /= 0x3B30
    then Left "Roaring32: bad cookie"
    else pure ()
  (numContainers, r2) <- takeWord32LE r1
  let n = fromIntegral numContainers
  -- A short table would silently drop containers, i.e. report rows as
  -- not deleted when the bitmap says they are. Reject it instead.
  if BS.length r2 < 8 * n
    then Left "Roaring32: truncated container table"
    else pure ()
  let (keysAndCards, r3) = BS.splitAt (4 * n) r2
      (_offsets, r4) = BS.splitAt (4 * n) r3
      kacEntries = unflatten4 keysAndCards
  goContainers kacEntries r4 IntSet.empty
  where
    unflatten4 :: ByteString -> [(Int, Int)]
    unflatten4 b
      | BS.null b = []
      | BS.length b < 4 = []
      | otherwise =
          let !key = readWord16LE b 0
              !card = readWord16LE b 2 + 1
          in (key, card) : unflatten4 (BS.drop 4 b)

    goContainers [] tail' acc = Right (acc, tail')
    goContainers ((key, card) : rest) tail' acc
      -- Same reasoning as the table check: a short payload must not
      -- decode to a smaller set of deleted positions.
      | BS.length tail' < 2 * card =
          Left "Roaring32: truncated container payload"
      | otherwise =
          let (payload, after) = BS.splitAt (2 * card) tail'
              decoded = Hash.roaringDecodeArray payload card 0 -- decode lows; OR'ing 0
              -- convert to ascending Int list and shift back into 32-bit space
              ascList = map ((key * 0x10000) +) (map fromIntegral (VS.toList decoded))
              set = IntSet.fromAscList ascList
          in goContainers rest after (IntSet.union acc set)


readWord16LE :: ByteString -> Int -> Int
readWord16LE bs off =
  let b0 = fromIntegral (BS.index bs off) :: Int
      b1 = fromIntegral (BS.index bs (off + 1)) :: Int
  in b0 .|. (b1 `shiftL` 8)


takeWord32LE :: ByteString -> Either String (Word32, ByteString)
takeWord32LE bs
  | BS.length bs < 4 = Left "expected 4 bytes"
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Word32
          b1 = fromIntegral (BS.index bs 1) :: Word32
          b2 = fromIntegral (BS.index bs 2) :: Word32
          b3 = fromIntegral (BS.index bs 3) :: Word32
          w = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
      in Right (w, BS.drop 4 bs)


takeWord64LE :: ByteString -> Either String (Word64, ByteString)
takeWord64LE bs
  | BS.length bs < 8 = Left "expected 8 bytes"
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Word64
          b1 = fromIntegral (BS.index bs 1) :: Word64
          b2 = fromIntegral (BS.index bs 2) :: Word64
          b3 = fromIntegral (BS.index bs 3) :: Word64
          b4 = fromIntegral (BS.index bs 4) :: Word64
          b5 = fromIntegral (BS.index bs 5) :: Word64
          b6 = fromIntegral (BS.index bs 6) :: Word64
          b7 = fromIntegral (BS.index bs 7) :: Word64
          w =
            b0
              .|. (b1 `shiftL` 8)
              .|. (b2 `shiftL` 16)
              .|. (b3 `shiftL` 24)
              .|. (b4 `shiftL` 32)
              .|. (b5 `shiftL` 40)
              .|. (b6 `shiftL` 48)
              .|. (b7 `shiftL` 56)
      in Right (w, BS.drop 8 bs)


-- ============================================================
-- Puffin blob (de)serialisation
-- ============================================================

{- | Wrap a deletion vector as a Puffin blob, including the V3-required
length prefix and CRC trailer. The CRC32 is the standard Castagnoli
variant used by Iceberg @reusing parquet-format-style@ checksums.
-}
toPuffinBlob :: Int64 -> Int64 -> Int -> DeletionVector -> PuffinBlob
toPuffinBlob snapId seqNum referencedFieldId dv =
  let bitmap = encodeDV dv
      lenBytes = BL.toStrict (BB.toLazyByteString (BB.word32LE (fromIntegral (BS.length bitmap))))
      crcBytes = BL.toStrict (BB.toLazyByteString (BB.word32LE (crc32c bitmap)))
  in PuffinBlob
       { pbType = "deletion-vector-v1"
       , pbFields = V.singleton referencedFieldId
       , pbSnapshotId = snapId
       , pbSequenceNumber = seqNum
       , pbProperties = Map.empty
       , pbCompressionCodec = Nothing
       , pbData = lenBytes <> bitmap <> crcBytes
       }


fromPuffinBlob :: PuffinBlob -> Either String DeletionVector
fromPuffinBlob blob = do
  let bs = pbData blob
  (len, rest) <- takeWord32LE bs
  let n = fromIntegral len
      (bitmapBytes, _trailer) = BS.splitAt n rest
  -- We don't validate the CRC here to keep the reader resilient against
  -- foreign writers that happen to elide the trailer.
  decodeDV bitmapBytes


-- ============================================================
-- CRC-32C
--
-- Iceberg v3 uses Castagnoli CRC32 (polynomial 0x1EDC6F41 reflected to
-- 0x82F63B78). This implementation is straightforward bit-at-a-time;
-- deletion vectors are tiny, so the table-free version is fine.
-- ============================================================

crc32c :: ByteString -> Word32
crc32c bs = BS.foldl' update 0xFFFFFFFF bs `xor` 0xFFFFFFFF
  where
    poly :: Word32
    poly = 0x82F63B78

    update !crc byte =
      let crc' = crc `xor` fromIntegral byte
          step !c _ =
            if c .&. 1 /= 0
              then (c `shiftR` 1) `xor` poly
              else c `shiftR` 1
      in foldl step crc' [(0 :: Int) .. 7]
