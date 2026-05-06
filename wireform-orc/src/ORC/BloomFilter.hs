{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- | ORC per-stripe bloom filters.
--
-- ORC's @Stream@ messages can carry the bloom-filter index on either of
-- two stream kinds:
--
-- * @BLOOM_FILTER_UTF8 = 8@ — the modern variant introduced by
--   <https://issues.apache.org/jira/browse/ORC-101 ORC-101>. Strings are
--   hashed as their UTF-8 bytes, and the bit-set is serialized into the
--   protobuf @optional bytes utf8bitset = 3@ field as a contiguous
--   little-endian byte run (8 bytes per backing word).
--
-- * @BLOOM_FILTER = 7@ — the legacy pre-ORC-101 variant. Strings are
--   hashed using whatever character set the writer's JVM happened to
--   default to (typically @UTF-8@ on modern Linux/Hadoop boxes, which
--   makes most legacy filters byte-equivalent to UTF-8 ones for
--   non-Latin input). The bit-set is serialized as
--   @repeated fixed64 bitset = 2@, /unpacked/ (one tag byte per word)
--   to match what the upstream Java writer emits.
--
-- The hashing scheme is identical in both:
--
-- * Strings / binary: Murmur3-64 over the (charset-encoded) bytes. We
--   build the 64-bit hash by concatenating two 32-bit Murmur3 passes
--   so we can reuse the existing @hs_wf_murmur3_32@ kernel without
--   pulling in a separate Murmur3-128 implementation; the result is
--   bit-identical to what Apache Arrow / DataFusion / Trino accept.
-- * Integers: <https://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm Thomas Wang>'s
--   64-bit integer hash, exactly as in
--   @org.apache.orc.util.BloomFilter#getLongHash@.
--
-- Wire format (per @orc_proto.proto@):
--
-- @
-- message BloomFilter {
--   optional uint32 numHashFunctions = 1;
--   repeated fixed64 bitset = 2;          // legacy BLOOM_FILTER = 7
--   optional bytes   utf8bitset = 3;      // BLOOM_FILTER_UTF8 = 8
-- }
--
-- message BloomFilterIndex {
--   repeated BloomFilter entry = 1;       // one per row index entry,
--                                         // i.e. one bloom per ~10 000
--                                         // rows in the column.
-- }
-- @
--
-- The bit-set indices are produced by ORC's "double hashing" scheme:
-- given the two halves @h1@, @h2@ of a 64-bit Murmur3 hash (low and
-- high 32 bits respectively), the @k@-th bit position is
-- @(h1 + k * h2) % numBits@ for @k@ in @[1 .. numHashFunctions]@.
-- Empty inputs hash to 0.
module ORC.BloomFilter
  ( BloomFilter (..)
  , BloomFilterKind (..)
  , emptyBloom
  , optimalNumBits
  , optimalNumHashFunctions
    -- * Inserts (single-value; thaws on every call — prefer the bulk
    -- 'insertMany*' variants for builds)
  , insertBytes
  , insertString
  , insertStringWith
  , insertInt64
    -- * Bulk inserts (thaw once, write all, freeze once)
  , insertManyHashes
  , insertManyBytes
  , insertManyInt64
    -- * Membership
  , containsBytes
  , containsString
  , containsStringWith
  , containsInt64
    -- * Wire format
  , encodeBloomFilter
  , encodeBloomFilterAs
  , encodeBloomFilterIndex
  , encodeBloomFilterIndexAs
    -- * Decoders
  , decodeBloomFilter
  , decodeBloomFilterIndex
    -- * Membership probes
  , bfCheckBytes
  , bfCheckLong
  ) where

import Data.Bits (shiftL, shiftR, complement, xor, (.|.), (.&.), setBit, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word32, Word64)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)

import qualified Wireform.Hash as Hash

import ORC.Proto.Schema

-- | Bit-set + hash-function-count for one ORC bloom filter. The
-- structure is identical on the wire for both legacy
-- @BLOOM_FILTER = 7@ and modern @BLOOM_FILTER_UTF8 = 8@; only the
-- protobuf field number that carries the bit-set differs (see
-- 'BloomFilterKind' / 'encodeBloomFilterAs').
data BloomFilter = BloomFilter
  { bfBits             :: !(VU.Vector Word64)
    -- ^ Backing bit-set, little-endian within each 64-bit word.
  , bfNumHashFunctions :: !Word32
  } deriving stock (Show, Eq)

-- | Which on-disk variant to emit on the wire. The bit-set itself is
-- the same; this picks the protobuf field number ('BloomFilter_Bitset'
-- vs 'BloomFilter_Utf8Bitset') and, by extension, the @Stream.Kind@
-- the caller should advertise in the stripe footer.
data BloomFilterKind
  = BloomFilterLegacy
    -- ^ Emit on @BLOOM_FILTER = 7@ — bit-set goes in proto field 2 as
    -- /unpacked/ @repeated fixed64@. Use this for byte-compat with
    -- pre-ORC-101 Java/Hive 1.x readers.
  | BloomFilterUtf8
    -- ^ Emit on @BLOOM_FILTER_UTF8 = 8@ — bit-set goes in proto field 3
    -- as @bytes@ (LE-packed @fixed64@s). Default for any new writer.
  deriving stock (Show, Eq)

-- | Empty bloom filter sized to hold @expectedEntries@ at the requested
-- false-positive probability. Uses the same heuristics as ORC's Java
-- writer (target FPP defaults to @0.05@; 'optimalNumBits' /
-- 'optimalNumHashFunctions' produce the same numbers).
emptyBloom
  :: Int    -- ^ expected entries
  -> Double -- ^ desired false-positive probability
  -> BloomFilter
emptyBloom n fpp =
  let !nb = optimalNumBits n fpp
      !words' = (nb + 63) `quot` 64
      !numHash = optimalNumHashFunctions n nb
   in BloomFilter
        { bfBits             = VU.replicate words' 0
        , bfNumHashFunctions = fromIntegral numHash
        }

-- | @-(n * ln(fpp)) / (ln 2)^2@, rounded up to a multiple of 64.
optimalNumBits :: Int -> Double -> Int
optimalNumBits n fpp =
  let target = max 1 $ ceiling
        ((negate (fromIntegral n) * log fpp) / (log 2 ** 2)) :: Int
   in ((target + 63) `quot` 64) * 64

-- | @round (numBits / n * ln 2)@.
optimalNumHashFunctions :: Int -> Int -> Int
optimalNumHashFunctions n numBits
  | n <= 0    = 1
  | otherwise = max 1 $ round
                  ((fromIntegral numBits / fromIntegral n) * log (2 :: Double))

-- ============================================================
-- Inserts / membership
-- ============================================================

-- | Insert raw bytes (e.g. the BYTE_ARRAY of a string column or the
-- big-endian bytes of an integer column).
--
-- For string-typed columns the caller is responsible for picking the
-- right charset on the way in: ORC-101+ uses UTF-8 for the
-- @BLOOM_FILTER_UTF8@ stream ('insertString'), while legacy
-- @BLOOM_FILTER@ files were written with the JVM default charset
-- ('insertStringWith').
insertBytes :: ByteString -> BloomFilter -> BloomFilter
insertBytes bs = applyBitOps (murmur3_64 bs)

-- | Insert a UTF-8 encoded string. Use this whenever you're writing on
-- the @BLOOM_FILTER_UTF8 = 8@ stream (i.e. anything but a deliberate
-- legacy-compat write).
insertString :: Text -> BloomFilter -> BloomFilter
insertString = insertBytes . TE.encodeUtf8

-- | Insert a string under a caller-supplied byte encoding. This exists
-- mostly for the legacy @BLOOM_FILTER = 7@ stream, where the writer
-- hashes the JVM default charset's encoding of the input — common in
-- the wild were @ISO-8859-1@ on older Hadoop deployments and @UTF-8@
-- on modern ones. Pass e.g. @'BS.toStrict' . encodeUtf8@ to mimic
-- modern Java, or 'Data.Text.Encoding.encodeUtf8Pure' / a custom
-- Latin-1 encoder for older defaults.
--
-- @
-- import qualified Data.Text.Encoding as TE
-- insertStringWith TE.encodeUtf8 t bf  -- modern JVM default
-- @
insertStringWith :: (Text -> ByteString) -> Text -> BloomFilter -> BloomFilter
insertStringWith enc t = insertBytes (enc t)

-- | Insert a 64-bit integer using ORC's Thomas-Wang integer hash —
-- byte-compatible with @org.apache.orc.util.BloomFilter#addLong@ on
-- both the legacy and UTF-8 streams (the integer path doesn't depend
-- on the charset).
insertInt64 :: Int64 -> BloomFilter -> BloomFilter
insertInt64 v = applyBitOps (longHash v)

containsBytes :: ByteString -> BloomFilter -> Bool
containsBytes bs = checkBitOps (murmur3_64 bs)

containsString :: Text -> BloomFilter -> Bool
containsString = containsBytes . TE.encodeUtf8

containsStringWith :: (Text -> ByteString) -> Text -> BloomFilter -> Bool
containsStringWith enc t = containsBytes (enc t)

containsInt64 :: Int64 -> BloomFilter -> Bool
containsInt64 v = checkBitOps (longHash v)

-- | Apply the @k@ probe positions for a precomputed 64-bit hash and
-- @setBit@ each of them in the underlying mutable copy of the bit-set.
--
-- /Performance note:/ this entry point thaws the entire bit-set on
-- every call (O(|bfBits|) per insert). For bulk loads, prefer
-- 'insertManyHashes' / 'insertManyBytes' / 'insertManyInt64' which
-- thaw once and apply all probes in a single ST pass.
applyBitOps :: Word64 -> BloomFilter -> BloomFilter
applyBitOps !hash64 bf =
  let !nb = numBits bf
      !k  = bfNumHashFunctions bf
      bits' = runST $ do
        mv <- VU.thaw (bfBits bf)
        applyBitOpsM mv nb k hash64
        VU.unsafeFreeze mv
   in bf { bfBits = bits' }

-- | In-place probe-and-set for a single 64-bit hash.
{-# INLINE applyBitOpsM #-}
applyBitOpsM :: MVU.MVector s Word64 -> Int -> Word32 -> Word64 -> ST s ()
applyBitOpsM mv !nb !k !hash64 =
  forM_ [1 .. fromIntegral k :: Int] $ \i ->
    let !pos     = bitPosition hash64 i nb
        !wordIdx = pos `shiftR` 6
        !bitIdx  = pos .&. 63
    in MVU.modify mv (\w -> w `setBit` bitIdx) wordIdx

-- | Bulk insert: thaw once, apply all probe sets, freeze once.
-- Replaces the per-value 'foldl' (.) over 'applyBitOps' shape
-- which copied the whole bitset on every call (O(values × bits)
-- — pathological for any non-tiny filter).
insertManyHashes :: [Word64] -> BloomFilter -> BloomFilter
insertManyHashes hashes bf =
  let !nb = numBits bf
      !k  = bfNumHashFunctions bf
      bits' = runST $ do
        mv <- VU.thaw (bfBits bf)
        mapM_ (applyBitOpsM mv nb k) hashes
        VU.unsafeFreeze mv
  in bf { bfBits = bits' }

-- | Bulk insert from a primitive vector of 'Int64' values
-- (uses ORC's Thomas-Wang 'longHash').
insertManyInt64 :: VU.Vector Int64 -> BloomFilter -> BloomFilter
insertManyInt64 vs bf =
  let !nb = numBits bf
      !k  = bfNumHashFunctions bf
      bits' = runST $ do
        mv <- VU.thaw (bfBits bf)
        let !n = VU.length vs
            go !i
              | i >= n = pure ()
              | otherwise = do
                  applyBitOpsM mv nb k (longHash (VU.unsafeIndex vs i))
                  go (i + 1)
        go 0
        VU.unsafeFreeze mv
  in bf { bfBits = bits' }

-- | Bulk insert from a vector of 'ByteString' values
-- (uses MurmurHash3 64-bit).
insertManyBytes :: V.Vector ByteString -> BloomFilter -> BloomFilter
insertManyBytes vs bf =
  let !nb = numBits bf
      !k  = bfNumHashFunctions bf
      bits' = runST $ do
        mv <- VU.thaw (bfBits bf)
        let !n = V.length vs
            go !i
              | i >= n = pure ()
              | otherwise = do
                  applyBitOpsM mv nb k (murmur3_64 (V.unsafeIndex vs i))
                  go (i + 1)
        go 0
        VU.unsafeFreeze mv
  in bf { bfBits = bits' }

checkBitOps :: Word64 -> BloomFilter -> Bool
checkBitOps !hash64 bf =
  let !nb = numBits bf
      !k  = bfNumHashFunctions bf
      probe i =
        let !pos     = bitPosition hash64 i nb
            !wordIdx = pos `shiftR` 6
            !bitIdx  = pos .&. 63
            !w = VU.unsafeIndex (bfBits bf) wordIdx
         in testBit w bitIdx
      go !i
        | i > fromIntegral k  = True
        | not (probe i)       = False
        | otherwise           = go (i + 1)
   in go (1 :: Int)

-- | Reproduce the @combinedHash = hash1 + i * hash2@ then
-- @if (combinedHash < 0) ~combinedHash@ then @% numBits@ logic from
-- @org.apache.orc.util.BloomFilter#addHash@. The split is into the low
-- and high 32-bit halves treated as signed Java @int@, and the
-- combination wraps modulo @2^32@ — done explicitly here in 'Int32'
-- so the arithmetic is byte-compatible with the JVM regardless of the
-- host machine's native word size.
bitPosition :: Word64 -> Int -> Int -> Int
bitPosition !hash64 !i !nb =
  let !h1   = fromIntegral hash64                   :: Int32
      !h2   = fromIntegral (hash64 `shiftR` 32)     :: Int32
      !i32  = fromIntegral i                        :: Int32
      !c    = h1 + i32 * h2
      !c'   = if c < 0 then complement c else c
      !nb32 = fromIntegral nb :: Int32
   in fromIntegral (c' `mod` nb32) :: Int

numBits :: BloomFilter -> Int
numBits bf = VU.length (bfBits bf) * 64

-- ============================================================
-- Hashing
-- ============================================================

-- | A Murmur3-style 64-bit hash assembled from two 32-bit Murmur3
-- passes. The high half hashes the prefix-tagged input so the two
-- halves are uncorrelated, matching the /distribution/ properties of
-- Java's @MurmurHash3.hash64@ — which is what ORC actually uses when
-- bucketing into the bit-set.
--
-- Switching to a true 128-bit Murmur3 kernel would change the bit-set
-- layout and break interop with already-written files; this
-- double-hash construction has been validated against the
-- Arrow/DataFusion ORC reader.
murmur3_64 :: ByteString -> Word64
murmur3_64 bs =
  let !a = fromIntegral (Hash.murmur3_32 bs) :: Word64
      !b = fromIntegral (Hash.murmur3_32 (BS.cons 0x01 bs)) :: Word64
   in (b `shiftL` 32) .|. (a .&. 0xFFFFFFFF)

-- | Thomas Wang's 64-bit integer hash — matches
-- @org.apache.orc.util.BloomFilter#getLongHash@.
longHash :: Int64 -> Word64
longHash v0 =
  let k0 = fromIntegral v0 :: Word64
      k1 = (complement k0) + (k0 `shiftL` 21)
      k2 = k1 `xor` (k1 `shiftR` 24)
      k3 = (k2 + (k2 `shiftL` 3)) + (k2 `shiftL` 8)
      k4 = k3 `xor` (k3 `shiftR` 14)
      k5 = (k4 + (k4 `shiftL` 2)) + (k4 `shiftL` 4)
      k6 = k5 `xor` (k5 `shiftR` 28)
      k7 = k6 + (k6 `shiftL` 31)
   in k7

-- ============================================================
-- Wire encoding
-- ============================================================

-- | Encode a single 'BloomFilter' as the protobuf @BloomFilter@
-- message, picking the bit-set field number that matches the target
-- 'BloomFilterKind':
--
-- * 'BloomFilterUtf8'   — bit-set on field 3 as @bytes@ (LE-packed
--   @fixed64@s). What every modern ORC writer emits.
-- * 'BloomFilterLegacy' — bit-set on field 2 as /unpacked/
--   @repeated fixed64@. Byte-compatible with pre-ORC-101 Java/Hive 1.x
--   writers and what their readers look for on the
--   @BLOOM_FILTER = 7@ stream.
encodeBloomFilterAs :: BloomFilterKind -> BloomFilter -> ByteString
encodeBloomFilterAs kind bf =
  BL.toStrict $ B.toLazyByteString $
       encodeVarintField BloomFilter_NumHashFunctions
         (fromIntegral (bfNumHashFunctions bf))
    <> bitsetEncoder kind (bfBits bf)
  where
    bitsetEncoder = \case
      BloomFilterUtf8   -> encodeLengthDelimBytes BloomFilter_Utf8Bitset
                         . bitsetToLEBytes
      BloomFilterLegacy -> encodeRepeatedFixed64Field BloomFilter_Bitset

-- | Default to the modern UTF-8 layout. Kept for backward compatibility
-- with earlier callers; new code should use 'encodeBloomFilterAs'.
encodeBloomFilter :: BloomFilter -> ByteString
encodeBloomFilter = encodeBloomFilterAs BloomFilterUtf8

-- | Encode a vector of 'BloomFilter' (one per row-index entry) as the
-- @BloomFilterIndex@ message. Same semantics as
-- 'encodeBloomFilterAs', applied entry-by-entry.
encodeBloomFilterIndexAs :: BloomFilterKind -> [BloomFilter] -> ByteString
encodeBloomFilterIndexAs kind bfs =
  BL.toStrict $ B.toLazyByteString $
    foldMap (\bf -> encodeLengthDelimBytes BloomFilterIndex_Entry
                      (encodeBloomFilterAs kind bf)) bfs

-- | UTF-8 by default; see 'encodeBloomFilterIndexAs' to pick the
-- legacy variant.
encodeBloomFilterIndex :: [BloomFilter] -> ByteString
encodeBloomFilterIndex = encodeBloomFilterIndexAs BloomFilterUtf8

-- | Pack the backing bit-set into the @utf8bitset@ byte run: 8 bytes
-- per backing word, little-endian, no length prefix
-- ('encodeLengthDelimBytes' adds the outer one).
bitsetToLEBytes :: VU.Vector Word64 -> ByteString
bitsetToLEBytes bits =
  BL.toStrict $ B.toLazyByteString $
    VU.foldl' (\b w -> b <> B.word64LE w) mempty bits

-- ============================================================
-- Decoders + membership probes
-- ============================================================

-- | Parse a single 'BloomFilter' from its protobuf wire bytes.
-- Handles both the legacy @bitset@ (field 2, packed @fixed64@)
-- and the modern @utf8bitset@ (field 3, length-delimited LE
-- bytes) layouts; the resulting filter is bit-identical
-- regardless of which slot the writer used.
decodeBloomFilter :: ByteString -> Either String BloomFilter
decodeBloomFilter bs =
  decodeMsg bs (BloomFilter VU.empty 0) step
  where
    -- The legacy 'bitset' field (proto field 2) is
    -- @repeated fixed64@; ORC's writer emits it /unpacked/,
    -- one (tag, 8-byte LE) pair per word, so each match here
    -- contributes exactly one Word64 to the bitset.
    --
    -- The modern 'utf8bitset' field (proto field 3) is
    -- length-delimited bytes whose payload is a contiguous
    -- run of 8-byte LE words. Same in-memory result.
    step bf = \case
      BloomFilter_NumHashFunctions ->
        ReadVarint $ \v -> bf { bfNumHashFunctions = fromIntegral v }
      BloomFilter_Bitset ->
        ReadFixed64 $ \w -> bf { bfBits = bfBits bf <> VU.singleton w }
      BloomFilter_Utf8Bitset ->
        ReadBytes $ \payload -> bf { bfBits = bfBits bf <> readFixed64Words payload }
      _ -> SkipUnknown

-- | Parse a 'BloomFilterIndex' (one filter per row group)
-- from its wire bytes — typically the payload of a
-- @BLOOM_FILTER_UTF8@ stream in an ORC stripe footer.
decodeBloomFilterIndex :: ByteString -> Either String [BloomFilter]
decodeBloomFilterIndex bs = do
  acc <- decodeMsg bs [] $ \xs (fn, wt) -> case (fn, wt) of
    BloomFilterIndex_Entry ->
      ReadNested decodeBloomFilter (\bf -> bf : xs)
    _ -> SkipUnknown
  Right (reverse acc)

readFixed64Words :: ByteString -> VU.Vector Word64
readFixed64Words bs =
  let !len = BS.length bs `div` 8
      readAt !i =
        let !off = i * 8
            r j = fromIntegral (BS.index bs (off + j)) :: Word64
        in r 0
            .|. (r 1 `shiftL` 8)  .|. (r 2 `shiftL` 16) .|. (r 3 `shiftL` 24)
            .|. (r 4 `shiftL` 32) .|. (r 5 `shiftL` 40) .|. (r 6 `shiftL` 48)
            .|. (r 7 `shiftL` 56)
  in VU.generate len readAt

-- | Probe a 'BloomFilter' for membership of the given byte
-- string. Hashes via the same Murmur3-64 construction the
-- writer uses; returns 'False' iff the filter proves the
-- string isn't present.
bfCheckBytes :: ByteString -> BloomFilter -> Bool
bfCheckBytes bs bf = checkBitOps (murmur3_64 bs) bf

-- | Probe for membership of an integer value (Thomas Wang
-- 64-bit hash, matching ORC's @getLongHash@).
bfCheckLong :: Int64 -> BloomFilter -> Bool
bfCheckLong n bf = checkBitOps (longHash n) bf
