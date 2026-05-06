{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- | ORC stripe footer (protobuf) — stream layout within a stripe.
--
-- Decodes the trailing protobuf @StripeFooter@ of a stripe (the last
-- @siFooterLength@ bytes). Individual stream payloads remain in the stripe
-- slice; use 'streamSlice' to extract bytes for a decoded 'Stream'.
module ORC.Stripe
  ( Stream (..)
  , StripeFooter (..)
  , ColumnEncoding (..)
  , ColumnEncodingKind (..)
  , defaultColumnEncodings
  , encodingsForTypes
  , decodeStripeFooter
  , stripeFooterBytes
  , streamSlice
  , stripeStreamSlices
    -- * Encoding
  , encodeStripeFooter
  , encodeStream
  , encodeColumnEncoding
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word32, Word64)
import qualified Data.Vector as V

import ORC.Proto.Schema
import ORC.Types (StripeInformation (..))

-- | One physical stream inside a stripe (see ORC @Stream@ protobuf).
data Stream = Stream
  { stKind   :: !Word64
  , stColumn :: !Word64
  , stLength :: !Word64
  } deriving stock (Show, Eq)

-- | Parsed @StripeFooter@.
--
-- Per the ORC spec the footer carries:
--
-- @
-- message StripeFooter {
--   repeated Stream streams = 1;
--   repeated ColumnEncoding columns = 2;
--   optional string writerTimezone = 3;
--   ...
-- }
-- @
--
-- Every official ORC reader (pyarrow.orc, the Java ORC reader,
-- arrow-rs, DuckDB) /requires/ field 2 — one 'ColumnEncoding'
-- entry per column in the file's type tree, including the
-- synthetic root — and rejects the footer when the count
-- doesn't match. Missing the field at all yields the famous
-- "bad number of ColumnEncodings in StripeFooter:
-- expected=N, actual=0" error.
data StripeFooter = StripeFooter
  { sfStreams   :: !(V.Vector Stream)
  , sfEncodings :: !(V.Vector ColumnEncoding)
    -- ^ One entry per column in the file's type tree (column 0
    -- is the synthetic root, columns 1.. are leaves in
    -- pre-order). Use 'defaultColumnEncodings' to populate
    -- with @DIRECT_V2@ for every column (the right default
    -- since wireform's writer emits RLEv2 streams).
  } deriving stock (Show, Eq)

-- | The @ColumnEncoding.Kind@ enum from @orc.proto@.
data ColumnEncodingKind
  = CEDirect          -- ^ 0  — DIRECT
  | CEDictionary      -- ^ 1  — DICTIONARY
  | CEDirectV2        -- ^ 2  — DIRECT_V2 (RLEv2 streams)
  | CEDictionaryV2    -- ^ 3  — DICTIONARY_V2
  deriving stock (Show, Eq, Enum, Bounded)

columnEncodingKindToInt :: ColumnEncodingKind -> Word64
columnEncodingKindToInt = \case
  CEDirect       -> 0
  CEDictionary   -> 1
  CEDirectV2     -> 2
  CEDictionaryV2 -> 3

intToColumnEncodingKind :: Word64 -> Maybe ColumnEncodingKind
intToColumnEncodingKind = \case
  0 -> Just CEDirect
  1 -> Just CEDictionary
  2 -> Just CEDirectV2
  3 -> Just CEDictionaryV2
  _ -> Nothing

-- | Per-column encoding entry. Only 'ceKind' is mandatory;
-- 'ceDictionarySize' must be present iff 'ceKind' is one of
-- the dictionary variants. 'ceBloomEncoding' is the
-- per-column bloom filter version — almost never set in
-- practice (the BLOOM_FILTER_UTF8 stream itself carries the
-- encoding info), so we keep it as a raw 'Maybe Word64' for
-- round-trip fidelity.
data ColumnEncoding = ColumnEncoding
  { ceKind           :: !ColumnEncodingKind
  , ceDictionarySize :: !(Maybe Word32)
  , ceBloomEncoding  :: !(Maybe Word64)
  } deriving stock (Show, Eq)

-- | The right default for a freshly-built stripe footer:
-- @DIRECT_V2@ for every column in the type tree. This matches
-- what wireform's writer actually produces (every leaf uses
-- RLEv2 streams; the synthetic root has no streams of its
-- own and DIRECT_V2 is the conventional value to put there).
--
-- Pass the file's column count, including the synthetic root.
defaultColumnEncodings :: Int -> V.Vector ColumnEncoding
defaultColumnEncodings !nCols =
  V.replicate nCols
    (ColumnEncoding CEDirectV2 Nothing Nothing)

-- | Per-column encodings derived from the file's type tree.
--
-- Most ORC readers (pyarrow, the Java reader) reject
-- @DIRECT_V2@ for compound columns: structs, lists, maps, and
-- unions only have the PRESENT stream and don't carry value
-- data of their own, so their valid encoding is @DIRECT@ /
-- @DIRECT_V2@ — which one specifically depends on whether the
-- column has any streams at all. The pyarrow reader's
-- StructColumnReader explicitly checks for @DIRECT@ (kind 0).
--
-- Apply this mapping by examining the column's 'TypeKind':
--
--   * Struct / List / Map / Union -> 'CEDirect' (kind 0)
--   * Anything else                -> 'CEDirectV2' (kind 2)
--
-- Pass the in-order @ORCType@ vector from the file footer
-- (column 0 = the root struct, columns 1.. = leaves in
-- pre-order). The result has the same length and is laid out
-- the same way pyarrow / Java ORC expect.
encodingsForTypes
  :: (Int -> Word64)  -- ^ kind id for column @i@; see 'TypeKind'
  -> Int              -- ^ total column count
  -> V.Vector ColumnEncoding
encodingsForTypes kindOf !nCols =
  V.generate nCols $ \i ->
    let !k = kindOf i
        !ek = if isCompoundKind k then CEDirect else CEDirectV2
    in  ColumnEncoding ek Nothing Nothing
  where
    -- These match the @ORC.Types.typeKindToInt@ values: 11=Struct,
    -- 12=Map, 13=List, 14=Union. Hard-coding the integers here
    -- avoids dragging the ORC.Types import into this lower-level
    -- module.
    isCompoundKind :: Word64 -> Bool
    isCompoundKind k = k == 11 || k == 12 || k == 13 || k == 14

-- | Take the stripe-footer protobuf bytes from a full stripe blob.
stripeFooterBytes :: ByteString -> StripeInformation -> Either String ByteString
stripeFooterBytes stripeBs si =
  let !flen = fromIntegral (siFooterLength si) :: Int
      !n = BS.length stripeBs
  in if flen <= 0 || flen > n
    then Left "ORC.Stripe: invalid stripe footer length"
    else Right $! BS.take flen (BS.drop (n - flen) stripeBs)

-- | Slice @stLength@ bytes for this stream starting at @offset@ within @stripeBs@.
streamSlice :: ByteString -> Word64 -> Word64 -> Either String ByteString
streamSlice stripeBs !offset !len =
  let !o = fromIntegral offset :: Int
      !l = fromIntegral len :: Int
      !n = BS.length stripeBs
  in if o < 0 || l < 0 || o + l > n
    then Left "ORC.Stripe: stream slice out of bounds"
    else Right $! BS.take l (BS.drop o stripeBs)

-- | Walk streams in @StripeFooter@ order and slice each payload from the start
-- of @stripeBs@ (index + data region; caller supplies the full stripe blob).
stripeStreamSlices :: ByteString -> StripeFooter -> Either String (V.Vector (Stream, ByteString))
stripeStreamSlices stripeBs (StripeFooter streams _encs) = do
  acc <- go 0 0 []
  Right (V.fromListN (V.length streams) (reverse acc))
  where
    go !i !pos !acc
      | i >= V.length streams = Right acc
      | otherwise =
          let st = V.unsafeIndex streams i
              !l = stLength st
          in case streamSlice stripeBs pos l of
            Left e -> Left e
            Right chunk ->
              go (i + 1) (pos + l) ((st, chunk) : acc)

-- | Parse protobuf @StripeFooter@. Reads the @streams@ list
-- (field 1) and the @columns@ list (field 2 — each entry is
-- one 'ColumnEncoding'); other fields (writerTimezone /
-- encryption variants / encryptedLocalKeys) are skipped for
-- now since this reader doesn't yet act on them.
decodeStripeFooter :: ByteString -> Either String StripeFooter
decodeStripeFooter bs = do
  (streamsRev, encsRev) <- go 0 [] []
  -- Reverse-cons + V.fromList instead of V.snoc loops.
  Right (StripeFooter (V.fromList (reverse streamsRev))
                      (V.fromList (reverse encsRev)))
  where
    !len = BS.length bs
    go !off !accStreams !accEncs
      | off >= len = Right (accStreams, accEncs)
      | otherwise = do
          (tag, off1) <- getVarint bs off len
          let !fn = fromIntegral (tag `shiftR` 3) :: Int
              !wt = tag .&. 7
          case (fn, wt) of
            StripeFooter_Streams -> do
              (chunk, off2) <- getLenDelim bs off1 len
              st <- decodeStream chunk
              go off2 (st : accStreams) accEncs
            StripeFooter_Columns -> do
              (chunk, off2) <- getLenDelim bs off1 len
              ce <- decodeColumnEncoding chunk
              go off2 accStreams (ce : accEncs)
            _ -> skipField wt bs off1 len >>= \off2 -> go off2 accStreams accEncs

-- | Decode one 'ColumnEncoding' payload (the inner protobuf
-- of one @repeated ColumnEncoding columns@ entry).
decodeColumnEncoding :: ByteString -> Either String ColumnEncoding
decodeColumnEncoding bs =
  go 0 (ColumnEncoding CEDirect Nothing Nothing)
  where
    !len = BS.length bs
    go !off !ce
      | off >= len = Right ce
      | otherwise = do
          (tag, off1) <- getVarint bs off len
          let !fn = fromIntegral (tag `shiftR` 3) :: Int
              !wt = tag .&. 7
          case (fn, wt) of
            ColumnEncoding_Kind -> do
              (v, off2) <- getVarint bs off1 len
              case intToColumnEncodingKind v of
                Just k  -> go off2 ce { ceKind = k }
                Nothing -> Left $
                  "ORC.Stripe: unknown ColumnEncoding kind " ++ show v
            ColumnEncoding_DictionarySize -> do
              (v, off2) <- getVarint bs off1 len
              go off2 ce { ceDictionarySize = Just (fromIntegral v) }
            ColumnEncoding_BloomEncoding -> do
              (v, off2) <- getVarint bs off1 len
              go off2 ce { ceBloomEncoding = Just v }
            _ -> skipField wt bs off1 len >>= \off2 -> go off2 ce

decodeStream :: ByteString -> Either String Stream
decodeStream bs = go 0 (Stream 0 0 0)
  where
    !len = BS.length bs
    go !off !st
      | off >= len = Right st
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fn = fromIntegral (tag `shiftR` 3) :: Int
              !wt = tag .&. 7
              readV f = do
                (v, off'') <- getVarint bs off' len
                go off'' (f v)
          case (fn, wt) of
            Stream_Kind   -> readV $ \v -> st { stKind   = v }
            Stream_Column -> readV $ \v -> st { stColumn = v }
            Stream_Length -> readV $ \v -> st { stLength = v }
            _             -> skipField wt bs off' len >>= \off'' -> go off'' st

-- Decoder primitives (getVarint, getLenDelim, skipField) come from
-- "ORC.Proto.Schema"; so do the encoder helpers. Both sides of this
-- module share the same named-field codec.

-- ============================================================
-- Protobuf encoding
-- ============================================================

-- | Encode a 'StripeFooter' as protobuf bytes. The streams
-- come first (field 1), then one ColumnEncoding entry per
-- column (field 2). ORC readers iterate the streams to find
-- payloads but use the ColumnEncodings list /length/ to
-- validate the file's schema; mismatch produces an immediate
-- decode error.
encodeStripeFooter :: StripeFooter -> ByteString
encodeStripeFooter (StripeFooter streams encs) =
  BL.toStrict $ B.toLazyByteString $
       V.foldl' (\acc s -> acc <> encodeLengthDelim StripeFooter_Streams
                                  (encodeStream s))
         mempty streams
    <> V.foldl' (\acc e -> acc <> encodeLengthDelim StripeFooter_Columns
                                  (encodeColumnEncoding e))
         mempty encs

-- | Encode a single 'Stream' as protobuf bytes.
encodeStream :: Stream -> B.Builder
encodeStream (Stream kind col len) = mconcat
  [ encodeVarintField Stream_Kind   kind
  , encodeVarintField Stream_Column col
  , encodeVarintField Stream_Length len
  ]

-- | Encode a single 'ColumnEncoding' as protobuf bytes.
-- Always emits the @kind@ field; @dictionarySize@ and
-- @bloomEncoding@ only when present.
encodeColumnEncoding :: ColumnEncoding -> B.Builder
encodeColumnEncoding (ColumnEncoding kind dictSz bloomEnc) = mconcat
  [ encodeVarintField ColumnEncoding_Kind (columnEncodingKindToInt kind)
  , maybe mempty (encodeVarintField ColumnEncoding_DictionarySize . fromIntegral)
      dictSz
  , maybe mempty (encodeVarintField ColumnEncoding_BloomEncoding) bloomEnc
  ]
