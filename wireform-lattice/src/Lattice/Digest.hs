{- | Cache digests (spec §10.4): the client store advertisement a request
carries so the origin can elide entity records the client already holds,
replacing them with @{"kind":"unchanged","id":…,"ver":…}@ markers.

Two header encodings:

* __Enumerated__ — @X-Have: Post:17\@e41,User:9\@b02@. Exact membership,
  OWS-tolerant parsing; meant for small stores (the Haskell client uses it
  at ≤ 32 entries).
* __Golomb-coded set__ — @X-Have-Digest: v1;fp=N;count=M;{b64url}@.
  Probabilistic membership with declared false-positive exponent @fp@
  (rate ≈ 2^-fp); a false positive means the origin wrongly elides an
  entity the client lacks, which the client repairs with a follow-up
  point fetch (the §10.4 tolerance trade).

== The GCS bit-level pin

Both ends of this implementation (and the spec, which pins the same
construction) agree on the following, byte for byte:

* A member is the UTF-8 bytes of @id \<> "\@" \<> ver@ (id is the rendered
  ref, e.g. @Post:17@).
* @hash64(member)@ = the first 8 bytes of @BLAKE3(member)@, interpreted
  as a big-endian 'Word64'.
* @count@ = the number of distinct @(id, ver)@ members; the hash range is
  @modulus = count * 2^fp@ and each member maps to @hash64 \`mod\` modulus@.
* The mapped values are sorted ascending and deduplicated (so the encoded
  value count may be less than @count@ under hash collisions), then
  delta-coded: the first delta is taken from 0 (i.e. it is the smallest
  value itself), each following delta is the difference from its
  predecessor (all > 0 after dedup).
* Each delta @d@ is Golomb-Rice coded with parameter @fp@: the quotient
  @d >> fp@ in unary (that many 1 bits, then one 0 bit), then the low
  @fp@ bits of @d@ MSB-first.
* The bitstream is packed MSB-first and zero-padded to a whole byte.
* The header value is @v1;fp=N;count=M;@ followed by the unpadded
  base64url (RFC 4648 §5) of those bytes. Carrying @count@ makes decoding
  self-delimiting: a decoder reads at most @count@ deltas and stops early
  when fewer than a delta's worth of bits remain (the zero padding can
  never be mistaken for a member, since a phantom zero delta re-yields
  the previous value, a set no-op).

@count = 0@ (an empty store) renders as @v1;fp=N;count=0;@ with an empty
payload and matches nothing; senders normally just omit the header.

== Origin policy

'requestDigest' implements the server-side header selection: the
enumerated form is preferred when both headers are present, and a GCS
digest is honored only when @8 <= fp <= 16@ ('acceptableFp') — otherwise
the header is ignored, per §10.4 (\"origins MUST honor only digests whose
declared rate they accept\"). Digests are consulted only on priv slices
and oneshot POSTs; pub\/ctx responses never vary on these headers (the
caller enforces that by not calling 'elideKnown' there).
-}
module Lattice.Digest (
  -- * Digests
  Digest (..),
  Gcs (..),
  digestContains,

  -- * Enumerated form (@X-Have@)
  renderHave,
  parseHave,

  -- * GCS form (@X-Have-Digest@)
  encodeGcs,
  renderHaveDigest,
  parseHaveDigest,
  acceptableFp,

  -- * Origin-side helpers
  requestDigest,
  elideKnown,
) where

import Control.Monad (guard)
import Data.Bits (shiftL, shiftR, testBit, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Text.Read qualified as TR
import Data.Word (Word64, Word8)
import Lattice.Hash (b64url, blake3)
import Lattice.Types (renderRef)
import Lattice.Wire (EntityRecord (..), Record (..), hXHave, hXHaveDigest)
import Network.HTTP.Types.Header (Headers, lookupHeader)


-- | A parsed client-store advertisement.
data Digest
  = -- | Exact @(id, ver)@ membership from @X-Have@.
    DigestEnumerated (Set (Text, Text))
  | -- | Probabilistic membership from @X-Have-Digest@.
    DigestGcs Gcs
  deriving stock (Eq, Show)


-- | A decoded Golomb-coded set (module header for the bit-level pin).
data Gcs = Gcs
  { gcsFp :: Int
  -- ^ False-positive exponent (Rice remainder width in bits).
  , gcsCount :: Int
  -- ^ Declared member count; the hash range is @count * 2^fp@.
  , gcsMembers :: Set Word64
  -- ^ The mapped hash values.
  }
  deriving stock (Eq, Show)


{- | Membership of one @(id, ver)@ pair: exact for the enumerated form,
probabilistic (rate ≈ @2^-fp@) for a GCS.
-}
digestContains :: Digest -> Text -> Text -> Bool
digestContains d i v = case d of
  DigestEnumerated s -> Set.member (i, v) s
  DigestGcs g
    | gcsCount g <= 0 -> False
    | otherwise -> Set.member (hash64 i v `mod` gcsModulus g) (gcsMembers g)


-- | The origin accepts GCS digests declaring @8 <= fp <= 16@ (§10.4).
acceptableFp :: Int -> Bool
acceptableFp fp = fp >= 8 && fp <= 16


-- ---------------------------------------------------------------------------
-- Enumerated form
-- ---------------------------------------------------------------------------

-- | Render an @X-Have@ value: @Post:17\@e41,User:9\@b02@.
renderHave :: [(Text, Text)] -> Text
renderHave = T.intercalate "," . map one
  where
    one (i, v) = i <> "@" <> v


{- | Parse an @X-Have@ value. OWS around commas and items is tolerated;
each item must be @id\@ver@ with a nonempty id and ver (the id may itself
contain @\@@; the last one separates). Empty list elements are ignored
(RFC 9110 §5.6.1: @\"a\@1,,b\@2\"@ parses as two members); an empty or
all-empty value is 'Nothing'. Any malformed nonempty item makes the whole
header unparseable ('Nothing'), which callers treat as the header being
absent.
-}
parseHave :: Text -> Maybe Digest
parseHave raw = case map T.strip (T.splitOn "," raw) of
  items
    | all T.null items -> Nothing
    | otherwise ->
        DigestEnumerated . Set.fromList <$> traverse item (filter (not . T.null) items)
  where
    item t = do
      let (pre, ver) = T.breakOnEnd "@" t
      guard (T.length pre > 1 && not (T.null ver))
      pure (T.dropEnd 1 pre, ver)


-- ---------------------------------------------------------------------------
-- GCS form
-- ---------------------------------------------------------------------------

{- | Build a GCS over @(id, ver)@ pairs at the given false-positive
exponent. Pairs are deduplicated before counting.
-}
encodeGcs :: Int -> [(Text, Text)] -> Gcs
encodeGcs fp pairs = Gcs {gcsFp = fp, gcsCount = count, gcsMembers = vals}
  where
    members = Set.toList (Set.fromList pairs)
    count = length members
    modulus = fromIntegral count `shiftL` fp :: Word64
    vals =
      if count == 0
        then Set.empty
        else Set.fromList (map (\(i, v) -> hash64 i v `mod` modulus) members)


-- | Render an @X-Have-Digest@ value: @v1;fp=10;count=123;{b64url}@.
renderHaveDigest :: Gcs -> Text
renderHaveDigest g =
  "v1;fp="
    <> tshow (gcsFp g)
    <> ";count="
    <> tshow (gcsCount g)
    <> ";"
    <> b64url (gcsBytes g)


{- | Parse an @X-Have-Digest@ value. Structural validation only (version
tag, decimal @fp@\/@count@, base64url payload; @0 <= fp <= 63@); the
policy bound 'acceptableFp' is the caller's ('requestDigest').
-}
parseHaveDigest :: Text -> Maybe Digest
parseHaveDigest raw = do
  rest1 <- T.stripPrefix "v1;fp=" (T.strip raw)
  (fp, rest2) <- decimalPrefix rest1
  rest3 <- T.stripPrefix ";count=" rest2
  (count, rest4) <- decimalPrefix rest3
  payload <- T.stripPrefix ";" rest4
  bytes <- either (const Nothing) Just (B64U.decodeUnpadded (encodeUtf8 (T.strip payload)))
  guard (fp >= 0 && fp <= 63 && count >= 0)
  pure (DigestGcs (Gcs fp count (decodeGcsBytes fp count bytes)))
  where
    decimalPrefix t = case TR.decimal t of
      Right (n, rest) -> Just (n :: Int, rest)
      Left _ -> Nothing


-- ---------------------------------------------------------------------------
-- Origin-side helpers
-- ---------------------------------------------------------------------------

{- | The effective digest of a request: @X-Have@ when parseable (preferred
when both headers are present), else @X-Have-Digest@ when parseable and
within 'acceptableFp'. 'Nothing' when absent, malformed, or out of
policy — the caller then elides nothing.
-}
requestDigest :: Headers -> Maybe Digest
requestDigest hdrs = case enumerated of
  Just d -> Just d
  Nothing -> do
    v <- lookupHeader hXHaveDigest hdrs
    d <- parseHaveDigest (decodeUtf8Lenient v)
    case d of
      DigestGcs g | not (acceptableFp (gcsFp g)) -> Nothing
      _ -> Just d
  where
    enumerated = lookupHeader hXHave hdrs >>= parseHave . decodeUtf8Lenient


{- | Replace entity records whose @(id, ver)@ the digest contains with
@unchanged@ markers (§10.4). Everything else — manifest, root ordering,
tombstones, errors — passes through untouched.
-}
elideKnown :: Maybe Digest -> [Record] -> [Record]
elideKnown Nothing rs = rs
elideKnown (Just d) rs = map step rs
  where
    step = \case
      REntity er
        | digestContains d (renderRef (erId er)) (erVer er) ->
            RUnchanged (erId er) (erVer er)
      r -> r


-- ---------------------------------------------------------------------------
-- Bit-level encode / decode
-- ---------------------------------------------------------------------------

-- | @hash64(id \<> \"\@\" \<> ver)@: first 8 bytes of BLAKE3, big-endian.
hash64 :: Text -> Text -> Word64
hash64 i v =
  BS.foldl'
    (\acc w -> (acc `shiftL` 8) .|. fromIntegral w)
    0
    (BS.take 8 (blake3 (encodeUtf8 (i <> "@" <> v))))


gcsModulus :: Gcs -> Word64
gcsModulus g = fromIntegral (gcsCount g) `shiftL` gcsFp g


-- | The packed Golomb-Rice bitstream of a GCS (module header pin).
gcsBytes :: Gcs -> ByteString
gcsBytes Gcs {..} =
  finishBits (fst (foldl' step (emptyBits, 0) (Set.toAscList gcsMembers)))
  where
    step (acc, prev) v = (pushDelta gcsFp acc (v - prev), v)


{- | Decode at most @count@ deltas; stop early when the remaining bits
cannot hold another delta. Duplicate values (phantom zero deltas read
from the padding) collapse in the set.
-}
decodeGcsBytes :: Int -> Int -> ByteString -> Set Word64
decodeGcsBytes fp count bs = go 0 0 Set.empty 0
  where
    totalBits = 8 * BS.length bs
    bitAt pos =
      let (byteIx, bitIx) = pos `divMod` 8
      in testBit (BS.index bs byteIx) (7 - bitIx)
    go !pos !prev !acc !k
      | k >= count = acc
      | otherwise = case readUnary pos 0 of
          Nothing -> acc
          Just (q, pos1) -> case readRemainder pos1 fp 0 of
            Nothing -> acc
            Just (r, pos2) ->
              let v = prev + ((q `shiftL` fp) .|. r)
              in go pos2 v (Set.insert v acc) (k + 1)
    readUnary !pos !q
      | pos >= totalBits = Nothing
      | bitAt pos = readUnary (pos + 1) (q + 1)
      | otherwise = Just (q :: Word64, pos + 1)
    readRemainder !pos !n !acc
      | n == 0 = Just (acc :: Word64, pos)
      | pos >= totalBits = Nothing
      | otherwise =
          readRemainder (pos + 1) (n - 1) ((acc `shiftL` 1) .|. (if bitAt pos then 1 else 0))


-- MSB-first bit writer: pending byte + fill count over a builder.
data BitAcc = BitAcc B.Builder !Word8 !Int


emptyBits :: BitAcc
emptyBits = BitAcc mempty 0 0


pushBit :: BitAcc -> Bool -> BitAcc
pushBit (BitAcc b cur n) one =
  let cur' = if one then cur .|. (0x80 `shiftR` n) else cur
  in if n == 7
      then BitAcc (b <> B.word8 cur') 0 0
      else BitAcc b cur' (n + 1)


finishBits :: BitAcc -> ByteString
finishBits (BitAcc b cur n)
  | n == 0 = BL.toStrict (B.toLazyByteString b)
  | otherwise = BL.toStrict (B.toLazyByteString (b <> B.word8 cur))


{- | One Golomb-Rice delta: quotient @d >> fp@ in unary (1-bits, 0
terminator), then the low @fp@ bits MSB-first.
-}
pushDelta :: Int -> BitAcc -> Word64 -> BitAcc
pushDelta fp acc0 d = foldl' remBit (pushBit (unary acc0 q) False) ixs
  where
    q = d `shiftR` fp
    unary acc 0 = acc
    unary acc k = unary (pushBit acc True) (k - (1 :: Word64))
    ixs = if fp == 0 then [] else reverse (take fp (iterate (+ 1) 0))
    remBit acc i = pushBit acc (testBit d i)


tshow :: (Show a) => a -> Text
tshow = T.pack . show
