{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Apache Parquet split-block bloom filter (SBBF), parquet-format 2.10+.

See <https://parquet.apache.org/docs/file-format/bloomfilter/> for the
spec. SBBF stores a bit array as a sequence of 256-bit blocks, each of
which contains 8 32-bit words. Hashing uses XXH64 with seed 0 over the
value's PLAIN encoding ("Wireform.Hash"'s seed-zero @xxh64@).

The on-disk layout is:

@
BloomFilterHeader (Thrift Compact)
  1: required i32 numBytes
  2: required BloomFilterAlgorithm  algorithm  (always BLOCK)
  3: required BloomFilterHash       hash       (always XXHASH)
  4: required BloomFilterCompression compression (always UNCOMPRESSED)
bitset bytes (numBytes wide)
@

This module gives:

* 'newSbbf' / 'sbbfInsertHash' / 'sbbfCheckHash' for the in-memory filter
* 'sbbfInsert' / 'sbbfCheck' for direct value insertion (XXH64 internal)
* 'encodeBloomFilter' / 'decodeBloomFilter' for the spec's wire format
* 'optimalNumBytes' for sizing helpers

Hot paths use 'Data.Vector.Unboxed.Mutable.Vector Word64' for the
bitset, four 64-bit reads/writes per block, and avoid all 'Maybe'
allocation in the inner kernel.
-}
module Parquet.BloomFilter (
  -- * Filter
  Sbbf,
  newSbbf,
  newSbbfChecked,
  newSbbfBytes,
  newSbbfBytesChecked,
  newSbbfFromBytes,
  newSbbfFromBytesChecked,
  SbbfBuilder,
  buildSbbf,
  buildSbbfChecked,
  sbbfBuilderInsert,
  sbbfBuilderInsertHash,
  sbbfNumBytes,
  sbbfInsert,
  sbbfCheck,
  sbbfInsertHash,
  sbbfCheckHash,

  -- * Wire format
  BloomFilterHeader (..),
  encodeBloomFilter,
  decodeBloomFilter,
  readBloomFilter,

  -- * Sizing
  optimalNumBytes,
) where

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits (shiftR, unsafeShiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32)
import Data.Primitive.ByteArray (copyByteArrayToAddr)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VUB
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Float (expm1Double, log1pDouble)
import Parquet.Encryption qualified as Encryption
import Parquet.Read qualified as Read
import Parquet.Thrift.Schema
import Parquet.Types qualified as P
import Thrift.Decode (decodeCompactFrom)
import Thrift.Encode (encodeCompact)
import Thrift.Value qualified as TV
import Wireform.Encode.Direct qualified as Direct
import Wireform.Hash qualified as Hash


-- | XXH64 with the Parquet bloom-filter default seed (@0@).
xxh64 :: ByteString -> Word64
xxh64 = Hash.xxh64 0
{-# INLINE xxh64 #-}


-- The spec's eight salt constants. Keeping them as cases lets each unrolled
-- lane use an immediate constant rather than loading a shared vector.
{-# INLINE saltForLane #-}
saltForLane :: Int -> Word32
saltForLane lane = case lane of
  0 -> 0x47b6137b
  1 -> 0x44974d91
  2 -> 0x8824ad5b
  3 -> 0xa2b7289d
  4 -> 0x705495c7
  5 -> 0x2df1424b
  6 -> 0x9efc4947
  _ -> 0x5c6bfb31


-- | Block size in bytes (256 bits = eight 32-bit words = four 64-bit words).
blockBytes :: Int
blockBytes = 32


{- | A split-block bloom filter, stored as @numBytes / 8@ packed
64-bit words. Bit ordering inside each block follows the Parquet spec
(block is eight 32-bit words, word @i@ stored at byte offset @4*i@,
little-endian).
-}
data Sbbf = Sbbf
  { sbbfBytes :: {-# UNPACK #-} !Int
  -- ^ 'numBytes' in the header
  , sbbfBlocks :: {-# UNPACK #-} !Int
  -- ^ derived: 'sbbfBytes' / 32
  , sbbfData :: {-# UNPACK #-} !(VU.Vector Word64)
  }
  deriving stock (Show, Eq)


{- | Create a new SBBF with @numBytes@ bytes (rounded up to a multiple of 32).
All bits start at zero. Use 'optimalNumBytes' to pick a sensible size.
-}
newSbbf :: Int -> Sbbf
newSbbf numBytes = either error id (newSbbfChecked numBytes)


{- | Checked, zeroed filter allocation. Sizes below one block select 32 bytes;
oversized requests are rejected before rounding or allocating.
-}
newSbbfChecked :: Int -> Either String Sbbf
newSbbfChecked numBytes = do
  nb <- normalisedBitsetSize numBytes
  newSbbfBytesChecked nb VU.empty


{- | Build an exact-size filter from decoded 64-bit words. Empty words
request a zeroed filter; other vectors must match the declared bitset.
-}
newSbbfBytes :: Int -> VU.Vector Word64 -> Sbbf
newSbbfBytes nb bitsetWords = either error id (newSbbfBytesChecked nb bitsetWords)


newSbbfBytesChecked :: Int -> VU.Vector Word64 -> Either String Sbbf
newSbbfBytesChecked nb bitsetWords
  | nb < blockBytes || nb `rem` blockBytes /= 0 || nb > largestBitset = Left "Parquet.BloomFilter: bitset size must be a positive multiple of 32 within the signed 32-bit wire limit"
  | not (VU.null bitsetWords) && VU.length bitsetWords /= nb `quot` 8 = Left "Parquet.BloomFilter: decoded word count does not match bitset size"
  | otherwise = Right (Sbbf nb (nb `quot` blockBytes) (if VU.null bitsetWords then VU.replicate (nb `quot` 8) 0 else bitsetWords))


{- | Build from a serialized bitset, without its header. Invalid sizes are
rejected rather than truncating bytes or creating a zero-block filter.
-}
newSbbfFromBytes :: ByteString -> Sbbf
newSbbfFromBytes bytes = either error id (newSbbfFromBytesChecked bytes)


newSbbfFromBytesChecked :: ByteString -> Either String Sbbf
newSbbfFromBytesChecked bytes
  | nb < blockBytes || nb `rem` blockBytes /= 0 || nb > largestBitset = Left "Parquet.BloomFilter: raw bitset size must be a positive multiple of 32 within the signed 32-bit wire limit"
  | otherwise = newSbbfBytesChecked nb (parseBitset bytes)
  where
    nb = BS.length bytes


largestBitset :: Int
largestBitset = fromIntegral (maxBound :: Int32) - (blockBytes - 1)


{-# INLINE normalisedBitsetSize #-}
normalisedBitsetSize :: Int -> Either String Int
normalisedBitsetSize bytes
  | bytes > largestBitset = Left "Parquet.BloomFilter: requested bitset exceeds the signed 32-bit wire limit"
  | otherwise = Right (roundUpBlock (max blockBytes bytes))


-- | Private mutable state confined by the rank-two 'buildSbbf' callback.
data SbbfBuilder s = SbbfBuilder
  { builderBlocks :: {-# UNPACK #-} !Int
  , builderData :: !(MVU.MVector s Word64)
  }


{- | Build a whole filter with one bitset allocation. The callback cannot
retain mutable state after the filter is frozen. Prefer this over repeated
'sbbfInsert' calls when constructing a column's complete filter.
-}
{-# INLINE buildSbbf #-}
buildSbbf :: Int -> (forall s. SbbfBuilder s -> ST s ()) -> Sbbf
buildSbbf bytes action = either error id (buildSbbfChecked bytes action)


{-# INLINE buildSbbfChecked #-}
buildSbbfChecked :: Int -> (forall s. SbbfBuilder s -> ST s ()) -> Either String Sbbf
buildSbbfChecked bytes action = do
  nb <- normalisedBitsetSize bytes
  Right $ runST $ do
    bitsetWords <- MVU.replicate (nb `quot` 8) 0
    let blocks = nb `quot` blockBytes
    action (SbbfBuilder blocks bitsetWords)
    frozen <- VU.unsafeFreeze bitsetWords
    pure (Sbbf nb blocks frozen)


{-# INLINE sbbfBuilderInsert #-}
sbbfBuilderInsert :: SbbfBuilder s -> ByteString -> ST s ()
sbbfBuilderInsert builder bytes = do
  -- XXH64 is read-only and keeps this immutable ByteString alive until the
  -- FFI finishes. Threading that IO result through ST avoids a separate
  -- unsafePerformIO thunk/noDuplicate boundary for every inserted value.
  hash <- unsafeIOToST (Hash.xxh64IO 0 bytes)
  sbbfBuilderInsertHash builder hash


{-# INLINE sbbfBuilderInsertHash #-}
sbbfBuilderInsertHash :: SbbfBuilder s -> Word64 -> ST s ()
sbbfBuilderInsertHash builder !hash =
  updateMutableBlock (builderData builder) (blockIndex hash (builderBlocks builder)) (fromIntegral hash)


roundUpBlock :: Int -> Int
roundUpBlock n =
  let r = n `rem` blockBytes
  in if r == 0 then n else n + (blockBytes - r)


-- | Number of bytes in the bitset (matches 'numBytes' in the header).
sbbfNumBytes :: Sbbf -> Int
sbbfNumBytes = sbbfBytes


{- | Insert a value (encoded as raw bytes — usually the column's PLAIN
physical layout) into the filter.
-}
{-# INLINE sbbfInsert #-}
sbbfInsert :: ByteString -> Sbbf -> Sbbf
sbbfInsert bs = sbbfInsertHash (xxh64 bs)


{- | Test whether a value /might/ be in the filter. False positives
are possible; false negatives are not.
-}
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
  in sbbf {sbbfData = ws'}


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


{- | Apply the @mask(x)@ to a single 32-bit lane @i@. Each word ends up
with exactly one bit set: bit @((x * salt[i]) >> 27)@.
-}
{-# INLINE maskBitForLane #-}
maskBitForLane :: Word32 -> Int -> Word32
maskBitForLane x i =
  let !s = saltForLane i
      !y = x * s
      !idx = fromIntegral (y `shiftR` 27) :: Int
  in 1 `unsafeShiftL` idx


-- One immutable insertion must preserve the original filter. Bulk callers
-- share the same lane kernel, but mutate one private bitset for all values.
updateBlock :: VU.Vector Word64 -> Int -> Word32 -> VU.Vector Word64
updateBlock bitsetWords block value = runST $ do
  mutable <- VU.thaw bitsetWords
  updateMutableBlock mutable block value
  VU.unsafeFreeze mutable


{-# INLINE updateMutableBlock #-}
updateMutableBlock :: forall s. MVU.MVector s Word64 -> Int -> Word32 -> ST s ()
updateMutableBlock bitsetWords !block !value = do
  let base = block * 4
  applyLanePair (base + 0) 0 1
  applyLanePair (base + 1) 2 3
  applyLanePair (base + 2) 4 5
  applyLanePair (base + 3) 6 7
  where
    {-# INLINE applyLanePair #-}
    applyLanePair :: Int -> Int -> Int -> ST s ()
    applyLanePair index low high = do
      current <- MVU.unsafeRead bitsetWords index
      let maskLow = fromIntegral (maskBitForLane value low) :: Word64
          maskHigh = fromIntegral (maskBitForLane value high) :: Word64
      MVU.unsafeWrite bitsetWords index (current .|. maskLow .|. (maskHigh `unsafeShiftL` 32))


checkBlock :: VU.Vector Word64 -> Int -> Word32 -> Bool
checkBlock ws !blockIdx !x =
  let !base = blockIdx * 4
      laneOk lo hi =
        let !cur = VU.unsafeIndex ws (base + (lo `quot` 2))
            !mLo = fromIntegral (maskBitForLane x lo) :: Word64
            !mHi = fromIntegral (maskBitForLane x hi) :: Word64
            !packed = mLo .|. (mHi `unsafeShiftL` 32)
        in (cur .&. packed) == packed
  in laneOk 0 1
       && laneOk 2 3
       && laneOk 4 5
       && laneOk 6 7


-- ============================================================
-- Wire format
-- ============================================================

{- | Decoded @BloomFilterHeader@ struct. Algorithm and hash are
enums-of-one in the current spec; we preserve them so that future
variants don't silently round-trip into BLOCK/XXHASH.
-}
data BloomFilterHeader = BloomFilterHeader
  { bfhNumBytes :: {-# UNPACK #-} !Int32
  , bfhAlgorithm :: !BloomFilterAlgorithm
  , bfhHash :: !BloomFilterHash
  , bfhCompression :: !BloomFilterCompression
  }
  deriving stock (Show, Eq)


data BloomFilterAlgorithm = AlgBlock
  deriving stock (Show, Eq)


data BloomFilterHash = HashXxh64
  deriving stock (Show, Eq)


data BloomFilterCompression = CompUncompressed
  deriving stock (Show, Eq)


{- | Build the on-disk byte string: Thrift-Compact 'BloomFilterHeader'
followed by the bitset.
-}
encodeBloomFilter :: Sbbf -> ByteString
encodeBloomFilter sbbf =
  let !hdr =
        BloomFilterHeader
          { bfhNumBytes = fromIntegral (sbbfBytes sbbf)
          , bfhAlgorithm = AlgBlock
          , bfhHash = HashXxh64
          , bfhCompression = CompUncompressed
          }
      !hdrBs = encodeCompact (bloomFilterHeaderToThrift hdr)
      !headerBytes = BS.length hdrBs
      !total = headerBytes + sbbfBytes sbbf
  in Direct.directEncode total $ \target _ -> do
       BSU.unsafeUseAsCString hdrBs $ \header -> copyBytes target (castPtr header) headerBytes
       copyBitset (target `plusPtr` headerBytes) (sbbfData sbbf) (sbbfBytes sbbf)
       pure total


{- | Read an on-disk bloom filter (header + bitset). Validates that
@numBytes@ matches what the bitset contains.
-}
decodeBloomFilter :: ByteString -> Either String (BloomFilterHeader, Sbbf)
decodeBloomFilter bs = do
  -- The Compact decoder reports the consumed header length, including
  -- unknown optional fields, without repeatedly parsing growing prefixes.
  (hdr, rest) <- splitHeader bs
  let !nb = fromIntegral (bfhNumBytes hdr) :: Int
  if nb < blockBytes || nb `rem` blockBytes /= 0
    then Left "Parquet.BloomFilter: bitset size must be a positive multiple of 32 bytes"
    else
      if BS.length rest < nb
        then
          Left $
            "Parquet.BloomFilter: bitset truncated (need "
              ++ show nb
              ++ " bytes, have "
              ++ show (BS.length rest)
              ++ ")"
        else do
          let !bitsetBs = BS.take nb rest
              !words_ = parseBitset bitsetBs
              !sbbf = newSbbfBytes nb words_
          Right (hdr, sbbf)


{- | Try to parse the Thrift Compact 'BloomFilterHeader' off the front
of @bs@ and return @(header, bytesAfterHeader)@.
-}
splitHeader :: ByteString -> Either String (BloomFilterHeader, ByteString)
splitHeader bs = do
  (value, end) <- decodeCompactFrom bs 0
  header <- thriftToBloomFilterHeader value
  Right (header, BS.drop end bs)


{- | Read a column's bloom filter, authenticating its separately encrypted
header and bitset where required. Old files may omit the length field.
-}
readBloomFilter :: Read.ParquetFile -> Int -> Int -> Either String (Maybe Sbbf)
readBloomFilter file groupIndex columnIndex = do
  group <- maybe (Left "Parquet.BloomFilter: row group out of range") Right (P.fmRowGroups (Read.pfFooter file) V.!? groupIndex)
  column <- maybe (Left "Parquet.BloomFilter: column out of range") Right (P.rgColumns group V.!? columnIndex)
  case P.ccFilePath column of
    Just path | path /= mempty -> Left "Parquet.BloomFilter: external column files are not supported"
    _ -> Right ()
  metadata <- maybe (Left "Parquet.BloomFilter: missing column metadata/key") Right (P.ccMetadata column)
  case P.cmBloomFilterOffset metadata of
    Nothing -> Right Nothing
    Just offset -> do
      let bytes = Read.pfBytes file
          size = maybe (fromIntegral (BS.length bytes) - offset) fromIntegral (P.cmBloomFilterLength metadata)
      if offset < 0 || offset > fromIntegral (BS.length bytes) || size < 0 || size > fromIntegral (BS.length bytes) - offset
        then Left "Parquet.BloomFilter: bloom filter slice out of bounds"
        else Right ()
      let raw = BS.take (fromIntegral size) (BS.drop (fromIntegral offset) bytes)
      plaintext <-
        if Read.isColumnEncrypted file groupIndex columnIndex
          then do
            (_, afterHeader) <- Encryption.readFramedModule raw 0
            headerBytes <- Read.decryptFileColumnModule file groupIndex columnIndex Encryption.ModuleBloomFilterHeader (BS.take afterHeader raw)
            (_, afterBitset) <- Encryption.readFramedModule raw afterHeader
            bitset <- Read.decryptFileColumnModule file groupIndex columnIndex Encryption.ModuleBloomFilterBitset (BS.take (afterBitset - afterHeader) (BS.drop afterHeader raw))
            case P.cmBloomFilterLength metadata of
              Just _ | afterBitset /= BS.length raw -> Left "Parquet.BloomFilter: encrypted bloom size differs from metadata"
              _ -> Right ()
            (value, headerEnd) <- decodeCompactFrom headerBytes 0
            header <- thriftToBloomFilterHeader value
            if not (BS.all (== 0) (BS.drop headerEnd headerBytes)) || BS.length bitset /= fromIntegral (bfhNumBytes header)
              then Left "Parquet.BloomFilter: encrypted header/bitset sizes disagree"
              else Right (BS.take headerEnd headerBytes <> bitset)
          else Right raw
      Just . snd <$> decodeBloomFilter plaintext


bloomFilterHeaderToThrift :: BloomFilterHeader -> TV.Value
bloomFilterHeaderToThrift hdr =
  TV.Struct $
    V.fromList
      [ BloomFilterHeader_NumBytes (bfhNumBytes hdr)
      , BloomFilterHeader_Algorithm (unionVariant1) -- BLOCK
      , BloomFilterHeader_Hash (unionVariant1) -- XXHASH
      , BloomFilterHeader_Compression (unionVariant1) -- UNCOMPRESSED
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
  nb <- case findField
    fm
    ( \case
        BloomFilterHeader_NumBytes v -> Just v
        _ -> Nothing
    ) of
    Just v -> Right v
    Nothing -> Left "Parquet.BloomFilter: missing numBytes"
  alg <-
    requireUnionVariant1
      fm
      "algorithm"
      (\case BloomFilterHeader_Algorithm fs -> Just fs; _ -> Nothing)
      AlgBlock
      "BloomFilterAlgorithm"
  hsh <-
    requireUnionVariant1
      fm
      "hash"
      (\case BloomFilterHeader_Hash fs -> Just fs; _ -> Nothing)
      HashXxh64
      "BloomFilterHash"
  cmp <-
    requireUnionVariant1
      fm
      "compression"
      (\case BloomFilterHeader_Compression fs -> Just fs; _ -> Nothing)
      CompUncompressed
      "BloomFilterCompression"
  Right
    BloomFilterHeader
      { bfhNumBytes = nb
      , bfhAlgorithm = alg
      , bfhHash = hsh
      , bfhCompression = cmp
      }
thriftToBloomFilterHeader _ =
  Left "Parquet.BloomFilter: expected struct for BloomFilterHeader"


{- | Match a nested union struct that carries exactly one field with id
@1@, return the corresponding Haskell enum value, and produce a
uniform error for missing or unsupported variants.
-}
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
      _ ->
        Left $
          "Parquet.BloomFilter: unsupported "
            ++ unionName
            ++ " union variant"
    Nothing -> Left $ "Parquet.BloomFilter: missing " ++ missingLabel


-- Sbbf constructors guarantee a complete, exact-size Word64 vector. Copy
-- only its logical slice; on big-endian hosts write individual bytes instead.
-- The primitive copy keeps the ByteArray alive across any GC and does not
-- require pinned source storage or an aligned destination.
copyBitset :: Ptr Word8 -> VU.Vector Word64 -> Int -> IO ()
copyBitset target words_@(VUB.V_Word64 (VP.Vector offset _ array)) bytes
  | targetByteOrder == LittleEndian = copyByteArrayToAddr target array (offset * 8) bytes
  | otherwise = go 0
  where
    go !index
      | index >= bytes `quot` 8 = pure ()
      | otherwise = do
          let word = VU.unsafeIndex words_ index
              putByte lane
                | lane == 8 = pure ()
                | otherwise = do
                    pokeByteOff target (index * 8 + lane) (fromIntegral (word `shiftR` (lane * 8)) :: Word8)
                    putByte (lane + 1)
          putByte 0
          go (index + 1)


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

{- | Estimate bitset bytes using the eight-hash SBBF model, rounded up to a
32-byte block. Invalid false-positive probabilities select one block. Sizes
saturate at the largest bitset representable by Parquet's signed 32-bit header;
that cap can prevent meeting a very demanding probability. Stable 'log1pDouble'
and 'expm1Double' arithmetic handles probabilities near both 0 and 1.
-}
optimalNumBytes :: Int -> Double -> Int
optimalNumBytes n fpp
  | n <= 0 || isNaN fpp || fpp <= 0 || fpp >= 1 = blockBytes
  | requested >= fromIntegral largestBitset = largestBitset
  | otherwise = roundUpBlock (max blockBytes (ceiling requested))
  where
    logRoot = log fpp / 8
    -- log(1-exp(x)) needs separate stable forms on either side of log(1/2).
    -- In particular, exp(x) can round to 1 for a valid probability below1.
    logGap
      | logRoot > negate (log 2) = log (negate (expm1Double logRoot))
      | otherwise = log1pDouble (negate (exp logRoot))
    requested = negate (fromIntegral n) / logGap
