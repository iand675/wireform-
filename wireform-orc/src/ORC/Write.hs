{-# LANGUAGE BangPatterns #-}

-- | ORC column and file encoding.
--
-- Provides encoders for primitive column types (integer, boolean, float,
-- double, string) and a minimal ORC file builder that concatenates streams
-- into stripes with a proper footer + postscript.
module ORC.Write
  ( -- * Stream encoders
    encodeRLEv2Direct
  , encodeBooleanRLE
  , encodeIntColumn
  , encodeStringDirectColumn
  , encodeStringDictColumn
  , encodeFloatColumn
  , encodeDoubleColumn
    -- * Date / timestamp / decimal column encoders
  , encodeDateColumn
  , encodeTimestampColumn
  , encodeDecimalColumn
  , encodeDecimalRawColumn
  , encodeORCNano
    -- * File assembly
  , buildStripe
  , buildORCFile
  , buildORCFileWithRows
  , buildORCFileWith
  , buildEncryptedORCFile
    -- * Column encryption plumbing
  , StripeEncryption (..)
  , encryptStripeStreams
  , decryptStripeStream
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import Control.Monad.ST (runST)
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import qualified Data.ByteString.Internal as BSI
import Foreign.Storable (pokeByteOff)

import qualified ORC.Encryption as Enc
import ORC.Footer (orcMagic, writeORCFooter)
import ORC.RLE
  ( bitWidth
  , closestWidth
  , encodeByteRLE
  , encodeWidth
  , packBitsMSB
  , putVulong
  , zigzagEncode
  )
import ORC.Stripe
  ( encodeStripeFooter, encodeStream, Stream (..), StripeFooter (..)
  , defaultColumnEncodings, encodingsForTypes
  )
import ORC.Types

------------------------------------------------------------------------
-- RLE v2 Direct encoding
------------------------------------------------------------------------

-- | Encode values as RLE v2 Direct.
--
-- Handles up to 512 values per run. For longer inputs, emits multiple
-- Direct runs.
encodeRLEv2Direct :: VP.Vector Int64 -> Bool -> ByteString
encodeRLEv2Direct vals signed =
  BL.toStrict $ B.toLazyByteString $ goChunks 0
  where
    !n = VP.length vals
    goChunks :: Int -> B.Builder
    goChunks !off
      | off >= n = mempty
      | otherwise =
          let !remaining = n - off
              !chunkLen = min 512 remaining
              !chunk = VP.slice off chunkLen vals
          in encodeDirectChunk chunk signed <> goChunks (off + chunkLen)

encodeDirectChunk :: VP.Vector Int64 -> Bool -> B.Builder
encodeDirectChunk vals signed =
  let !n = VP.length vals
      !transformed = VP.generate n $ \i ->
        let !v = VP.unsafeIndex vals i
        in if signed then zigzagEncode v else fromIntegral v :: Word64
      !maxVal = VP.foldl' max 0 transformed
      !rawW = bitWidth maxVal
      !w = closestWidth rawW
      !encodedW = encodeWidth w
      -- Header: [01][encodedW 5 bits][lenHigh 1 bit]
      !len1 = n - 1
      !lenHigh = (len1 `shiftR` 8) .&. 1
      !lenLow  = len1 .&. 0xFF
      !byte0 = (1 `shiftL` 6) .|. (encodedW `shiftL` 1) .|. lenHigh
      !byte1 = lenLow
      !packed = packBitsMSB transformed w
  in B.word8 (fromIntegral byte0)
     <> B.word8 (fromIntegral byte1)
     <> B.byteString packed

------------------------------------------------------------------------
-- Boolean stream encoding
------------------------------------------------------------------------

-- | Encode a boolean vector as ORC boolean stream (byte-RLE of bit-packed bytes).
encodeBooleanRLE :: V.Vector Bool -> ByteString
encodeBooleanRLE vals =
  let !n = V.length vals
      !numBytes = (n + 7) `quot` 8
      !bytes = VP.generate numBytes $ \bi ->
        let buildByte !bit !acc
              | bit >= 8 = acc
              | otherwise =
                  let !idx = bi * 8 + bit
                      !bitVal = if idx < n && V.unsafeIndex vals idx then 1 else 0 :: Word8
                      !acc' = acc .|. (bitVal `shiftL` (7 - bit))
                  in buildByte (bit + 1) acc'
        in buildByte 0 0
  in encodeByteRLE bytes

------------------------------------------------------------------------
-- Column encoders
------------------------------------------------------------------------

-- | Encode an integer column's DATA stream using RLE v2 Direct.
encodeIntColumn :: VP.Vector Int64 -> Bool -> ByteString
encodeIntColumn = encodeRLEv2Direct

-- | Encode a string column with DIRECT_V2 encoding.
-- Returns (DATA stream, LENGTH stream).
encodeStringDirectColumn :: V.Vector T.Text -> (ByteString, ByteString)
encodeStringDirectColumn texts =
  let !encodedTexts = V.map TE.encodeUtf8 texts
      !dataBs = BS.concat (V.toList encodedTexts)
      !lengths = VP.generate (V.length texts) $ \i ->
        fromIntegral (BS.length (V.unsafeIndex encodedTexts i)) :: Int64
      !lengthBs = encodeRLEv2Direct lengths False
  in (dataBs, lengthBs)

-- | Encode a string column with DICTIONARY_V2 encoding. Deduplicates
-- the input, builds a lookup of unique strings, and emits the three
-- streams ORC stores per DICTIONARY_V2 column:
--
-- * DATA - per-row unsigned RLE-v2 dictionary indices
-- * LENGTH - dictionary entry lengths (one per unique string, RLE-v2 unsigned)
-- * DICTIONARY_DATA - concatenated UTF-8 bytes of the unique strings
--
-- The dictionary preserves first-occurrence order so that a reader
-- pairing these with a row-index stream can still seek correctly.
encodeStringDictColumn
  :: V.Vector T.Text
  -> (ByteString, ByteString, ByteString)
  -- ^ (DATA, LENGTH, DICTIONARY_DATA)
encodeStringDictColumn texts =
  let !n = V.length texts
      -- First-occurrence dictionary. We walk once and keep a
      -- 'Map' for lookup; the per-row index stream is written
      -- straight into a mutable primitive vector.
      --
      -- Implementation note (perf): the previous version used
      -- @V.snoc idxAcc@ to grow the indices vector — O(n) copy
      -- per row, so O(n²) total work. For a 100k-row column
      -- that's ~5 billion copies. The mutable-vector version
      -- below is O(n).
      (idxPrim, uniqueRev) = runST $ do
        mv <- MVP.unsafeNew n
        let go !i !dict !uniqAcc
              | i >= n    = pure (dict, uniqAcc)
              | otherwise = do
                  let !t = V.unsafeIndex texts i
                  case Map.lookup t dict of
                    Just k -> do
                      MVP.unsafeWrite mv i (fromIntegral k :: Int64)
                      go (i + 1) dict uniqAcc
                    Nothing -> do
                      let !k     = Map.size dict
                          !dict' = Map.insert t k dict
                      MVP.unsafeWrite mv i (fromIntegral k :: Int64)
                      go (i + 1) dict' (t : uniqAcc)
        (_finalDict, accRev) <- go 0 mempty []
        v <- VP.unsafeFreeze mv
        pure (v, accRev)
      !uniques = V.fromList (reverse uniqueRev)
      !(dictBytes, lengthBs) = encodeStringDirectColumn uniques
      !dataBs = encodeRLEv2Direct idxPrim False
  in (dataBs, lengthBs, dictBytes)

-- | Encode a float column (IEEE 754 single, little-endian).
--
-- Implementation note (perf): the previous version went
-- through @ByteString.Builder@ with a @VP.foldl'@ + 4 per-byte
-- @B.word8@ shift-extracts per element, then @BL.toStrict@.
-- For a 100k-row column that allocates a 100k-deep Builder
-- thunk plus the lazy chunk list. The new version
-- pre-allocates the strict ByteString to its exact final size
-- and writes each element with a single 4-byte poke.
encodeFloatColumn :: VP.Vector Float -> ByteString
encodeFloatColumn vals =
  let !n = VP.length vals
  in BSI.unsafeCreate (n * 4) $ \ptr -> do
       let go !i
             | i >= n    = pure ()
             | otherwise = do
                 let !w = castFloatToWord32 (VP.unsafeIndex vals i)
                 pokeByteOff ptr (i * 4) w
                 go (i + 1)
       go 0

-- | Encode a double column (IEEE 754 double, little-endian).
encodeDoubleColumn :: VP.Vector Double -> ByteString
encodeDoubleColumn vals =
  let !n = VP.length vals
  in BSI.unsafeCreate (n * 8) $ \ptr -> do
       let go !i
             | i >= n    = pure ()
             | otherwise = do
                 let !w = castDoubleToWord64 (VP.unsafeIndex vals i)
                 pokeByteOff ptr (i * 8) w
                 go (i + 1)
       go 0

------------------------------------------------------------------------
-- Date / timestamp / decimal column encoders
------------------------------------------------------------------------

-- | Encode an ORC @DATE@ column (signed days since 1970-01-01) as a single
-- DATA stream using RLE v2. The reader's 'decodeDateColumn' takes the
-- output of this function unchanged.
encodeDateColumn :: VP.Vector Int32 -> ByteString
encodeDateColumn vals =
  encodeRLEv2Direct (VP.map fromIntegral vals) True

-- | Encode an ORC @TIMESTAMP@ column. Returns two streams: the @DATA@
-- stream (signed seconds since 1970-01-01) and the @SECONDARY@ stream
-- (unsigned encoded nanoseconds where the bottom 3 bits hold the
-- trailing-zero scale). Mirrors @decodeTimestampColumn@.
--
-- Each input is @(seconds, nanoValue)@ - the nanos part is the literal
-- nanosecond value (e.g. @123_000_000@ for half a second + 123 ms);
-- 'encodeORCNano' compresses trailing-zero runs into the spec's 3-bit
-- scale field.
encodeTimestampColumn :: VP.Vector Int64 -> VP.Vector Int64 -> (ByteString, ByteString)
encodeTimestampColumn secs nanos =
  let !secStream  = encodeRLEv2Direct secs True
      !nanoVals   = VP.map encodeORCNano nanos
      !nanoStream = encodeRLEv2Direct nanoVals False
   in (secStream, nanoStream)

-- | Compress a nanosecond value into ORC's @nanos@ wire encoding: the
-- bottom 3 bits store the trailing-zero scale (0..7) and the upper bits
-- store the un-scaled nanosecond value. So @123_000_000@ becomes
-- @123 \`shiftL\` 3 .|. 6@ (six trailing zeros).
encodeORCNano :: Int64 -> Int64
encodeORCNano !n
  | n == 0    = 0
  | otherwise =
      let !zeros = countTrailingZeros10 n 0
          !base  = n `quot` (pow10w zeros)
       in (base `shiftL` 3) .|. fromIntegral zeros
  where
    countTrailingZeros10 !v !acc
      | acc >= 7        = acc
      | v `rem` 10 == 0 = countTrailingZeros10 (v `quot` 10) (acc + 1)
      | otherwise       = acc

    pow10w :: Int -> Int64
    pow10w k = case k of
      0 -> 1; 1 -> 10; 2 -> 100; 3 -> 1000; 4 -> 10000
      5 -> 100000; 6 -> 1000000; 7 -> 10000000
      _ -> 100000000

-- | Encode a DECIMAL64 column (precision &le; 18) as a DATA stream of
-- signed unscaled values via RLE v2. The scale is fixed per-column and
-- is recorded on the schema's 'ORCType', not in the data stream. Mirrors
-- 'decodeDecimalColumn'. For DECIMAL128 columns use
-- 'encodeDecimalRawColumn' and the @SECONDARY@ scale stream.
encodeDecimalColumn :: VP.Vector Int64 -> ByteString
encodeDecimalColumn vals = encodeRLEv2Direct vals True

-- | Encode a variable-byte (LEB128-style) DECIMAL128 stream and a
-- corresponding RLE-v2 @SECONDARY@ scale stream. Useful when the writer
-- needs the full DECIMAL spec; the simpler 'encodeDecimalColumn' covers
-- the DECIMAL64 fast path the reader uses today.
encodeDecimalRawColumn
  :: V.Vector Integer  -- ^ Unscaled values.
  -> Int               -- ^ Column scale (constant per column).
  -> (ByteString, ByteString)
encodeDecimalRawColumn vals scale =
  let !dataBs = BL.toStrict $ B.toLazyByteString $
                  V.foldl' (\acc v -> acc <> writeVarSigned v) mempty vals
      !scaleStream =
        encodeRLEv2Direct (VP.replicate (V.length vals) (fromIntegral scale)) False
   in (dataBs, scaleStream)

-- | LEB128-style signed varint used by ORC's @decimal128@ DATA stream.
-- Zig-zag encode then write 7 bits per byte with the MSB as a
-- continuation flag.
writeVarSigned :: Integer -> B.Builder
writeVarSigned !x =
  let !u = if x >= 0 then x `shiftL` 1 else (negate x `shiftL` 1) - 1
   in goVar u
  where
    goVar 0 = B.word8 0
    goVar n = goLoop n
    goLoop !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise =
          let !low = fromIntegral (n .&. 0x7F) .|. 0x80 :: Word8
           in B.word8 low <> goLoop (n `shiftR` 7)

------------------------------------------------------------------------
-- File assembly
------------------------------------------------------------------------

-- | Build a stripe from stream payloads and stream metadata.
--
-- Concatenates the DATA streams and appends a protobuf stripe
-- footer that includes one 'ColumnEncoding' entry per column
-- in the file's type tree (synthetic root + every leaf), each
-- defaulted to @DIRECT_V2@ since wireform's writer emits RLEv2
-- streams. Real ORC readers (pyarrow.orc, the Java reader,
-- arrow-rs, DuckDB) refuse stripes whose encoding count
-- doesn't match the schema.
buildStripe
  :: Int   -- ^ total column count (synthetic root + leaves)
  -> V.Vector (Word64, Word64, ByteString)
  -> ByteString
buildStripe !nCols streamInfos =
  let !streams = V.map (\(kind, col, bs) ->
        Stream { stKind = kind, stColumn = col, stLength = fromIntegral (BS.length bs) }) streamInfos
      !footer = StripeFooter streams (defaultColumnEncodings nCols)
      !footerBs = encodeStripeFooter footer
      !dataParts = V.toList (V.map (\(_, _, bs) -> bs) streamInfos)
  in BS.concat (dataParts ++ [footerBs])

-- | Build a complete ORC file from type info and stripe data.
--
-- @types@: column types for the schema
-- @stripeData@: for each stripe, a vector of (streamKind, columnId, payload)
--
-- Records @siNumberOfRows = 0@ in every stripe and
-- @orcNumberOfRows = 0@ in the footer — callers that know the row
-- counts should use 'buildORCFileWithRows' or 'buildORCFileWith'
-- to stamp them in. Predicate-pushdown-aware readers need correct
-- row counts for scan planning.
buildORCFile :: V.Vector ORCType -> V.Vector (V.Vector (Word64, Word64, ByteString)) -> ByteString
buildORCFile = buildORCFileWith id

-- | Like 'buildORCFile' but records per-stripe row counts.
--
-- @rowCounts@ must have the same length as @stripeData@;
-- mismatched lengths fall through to the length of the shorter
-- vector with the missing stripes stamped at 0 rows.
buildORCFileWithRows
  :: V.Vector ORCType
  -> V.Vector (V.Vector (Word64, Word64, ByteString))
  -> V.Vector Word64    -- ^ one row count per stripe
  -> ByteString
buildORCFileWithRows types stripeData rowCounts =
  let !lookupRows = \i ->
        case rowCounts V.!? i of
          Just r  -> r
          Nothing -> 0
  in  buildORCFileWithRowLookup lookupRows types stripeData

-- | Variant of 'buildORCFile' that lets the caller adjust the
-- computed 'ORCFooter' before serialisation. Used by
-- 'buildEncryptedORCFile' to stamp the @Encryption@ footer field in
-- without re-parsing the file. The transform is applied /after/ all
-- stripe offsets + content lengths are computed, so callers
-- shouldn't use it to change structural fields (stripes, types, row
-- counts) — the writer's offsets would no longer match.
buildORCFileWith
  :: (ORCFooter -> ORCFooter)
  -> V.Vector ORCType
  -> V.Vector (V.Vector (Word64, Word64, ByteString))
  -> ByteString
buildORCFileWith adjustFooter types stripeData =
  buildORCFileWithRowLookupFooter adjustFooter (const 0) types stripeData

-- | Shared implementation: row-count-aware stripe layout with
-- no extra footer adjustment. Used by 'buildORCFileWithRows'.
buildORCFileWithRowLookup
  :: (Int -> Word64)
  -> V.Vector ORCType
  -> V.Vector (V.Vector (Word64, Word64, ByteString))
  -> ByteString
buildORCFileWithRowLookup = buildORCFileWithRowLookupFooter id

-- | Shared implementation: row-count-aware stripe layout with
-- a caller-supplied footer adjustment.
buildORCFileWithRowLookupFooter
  :: (ORCFooter -> ORCFooter)
  -> (Int -> Word64)
  -> V.Vector ORCType
  -> V.Vector (V.Vector (Word64, Word64, ByteString))
  -> ByteString
buildORCFileWithRowLookupFooter adjustFooter rowsForStripe types stripeData =
  let !headerMagic = orcMagic
      !headerLen   = fromIntegral (BS.length headerMagic) :: Word64

      !nCols = V.length types
      !encs  = encodingsForTypes
                 (\i -> fromIntegral (typeKindToInt
                                        (otKind
                                           (V.unsafeIndex types i))))
                 nCols

      -- Reverse-cons accumulator for StripeInformation: avoids
      -- the O(N^2) V.snoc per stripe. The stripe payload list
      -- is already cons-then-reverse so just match that.
      buildStripes :: Int -> Word64 -> [StripeInformation] -> [ByteString]
                   -> (V.Vector StripeInformation, [ByteString])
      buildStripes !i !off !siRev !bsRev
        | i >= V.length stripeData =
            (V.fromListN (V.length stripeData) (reverse siRev), reverse bsRev)
        | otherwise =
            let !sdata = V.unsafeIndex stripeData i
                !streams = V.map (\(kind, col, bs) ->
                  Stream { stKind = kind, stColumn = col, stLength = fromIntegral (BS.length bs) }) sdata
                !footer = StripeFooter streams encs
                !footerBs = encodeStripeFooter footer
                !dataLen = V.foldl' (\a (_, _, bs) -> a + fromIntegral (BS.length bs)) 0 sdata :: Word64
                !ftrLen = fromIntegral (BS.length footerBs) :: Word64
                !nRows = rowsForStripe i
                !si = StripeInformation
                  { siOffset = off
                  , siIndexLength = 0
                  , siDataLength = dataLen
                  , siFooterLength = ftrLen
                  , siNumberOfRows = nRows
                  }
                !stripeBs = BS.concat (V.toList (V.map (\(_, _, bs) -> bs) sdata) ++ [footerBs])
                !stripeLen = fromIntegral (BS.length stripeBs) :: Word64
            in buildStripes (i + 1) (off + stripeLen) (si : siRev) (stripeBs : bsRev)

      (!stripeInfos, !stripeBss) = buildStripes 0 headerLen [] []
      !contentLen = V.foldl' (\a si -> a + siIndexLength si + siDataLength si + siFooterLength si) 0 stripeInfos

      !baseFooter = ORCFooter
        { orcHeaderLength  = headerLen
        , orcContentLength = contentLen
        , orcStripes       = stripeInfos
        , orcTypes         = types
        , orcMetadata      = V.empty
        , orcNumberOfRows  = V.foldl' (\a si -> a + siNumberOfRows si) 0 stripeInfos
        , orcStatistics    = V.empty
        , orcEncryption    = Nothing
        }
      !footer = adjustFooter baseFooter
      !footerBytes = writeORCFooter footer
  in BS.concat ([headerMagic] ++ stripeBss ++ [footerBytes])

-- ============================================================
-- Column encryption: whole-file integration
-- ============================================================

-- | Per-stripe encryption parameters. The ORC spec derives a stream
-- key from @AES-CTR(localKey, stripeId)@; this record captures the
-- information a writer needs to spin that up. The stripe id is
-- monotonically increasing within a file (one per stripe), and the
-- local key is the same across stripes — the per-stripe rotation
-- comes from the IV derived from @(stripeId, streamOffset)@.
data StripeEncryption = StripeEncryption
  { seLocalKey :: !ByteString
    -- ^ Column variant's local key (16 / 24 / 32 bytes, matching the
    -- 'Enc.EncryptionAlgorithm'). Produced by the caller, typically
    -- from a KMS; this module doesn't pick it.
  , seStripeId :: !Word64
    -- ^ Monotonic stripe identifier; fed into the stream-key +
    -- stream-IV derivation.
  } deriving (Show, Eq)

-- | Encrypt a stripe's streams using the ORC spec's AES-CTR scheme.
-- Takes the unencrypted @(kind, column, payload)@ vector that
-- 'buildStripe' consumes and returns the corresponding encrypted
-- vector. Stream offset within the stripe is computed by running
-- byte-length totals; the IV per stream is
-- @iv[0..7] = stripeId (BE); iv[8..15] = streamOffset (BE)@.
encryptStripeStreams
  :: StripeEncryption
  -> V.Vector (Word64, Word64, ByteString)
  -> Either String (V.Vector (Word64, Word64, ByteString))
encryptStripeStreams se streams = do
  -- Derive the per-stripe key once; reuse across all streams in the
  -- stripe.
  stripeKey <- Enc.encryptStripeKey (seLocalKey se) (seStripeId se)
  let go !i !streamOffset !acc
        | i >= V.length streams = Right (V.fromList (reverse acc))
        | otherwise = do
            let (kind, col, payload) = V.unsafeIndex streams i
                !iv = Enc.deriveStreamIv (seStripeId se) streamOffset
            ciphertext <- Enc.aesCtrXor stripeKey iv payload
            let !next = streamOffset + fromIntegral (BS.length payload)
            go (i + 1) next ((kind, col, ciphertext) : acc)
  go 0 0 []

-- | Inverse of 'encryptStripeStreams': given the same
-- 'StripeEncryption' and the encrypted byte payload, recover the
-- plaintext. Callers know the stream's offset within the stripe
-- (from 'stripeStreamSlices') and pass it as @streamOffset@.
decryptStripeStream
  :: StripeEncryption
  -> Word64
  -> ByteString
  -> Either String ByteString
decryptStripeStream se streamOffset ciphertext = do
  stripeKey <- Enc.encryptStripeKey (seLocalKey se) (seStripeId se)
  let !iv = Enc.deriveStreamIv (seStripeId se) streamOffset
  Enc.aesCtrXor stripeKey iv ciphertext

-- | Variant of 'buildORCFile' that encrypts every stripe's streams
-- with the caller-supplied 'StripeEncryption' and attaches the
-- protobuf @Encryption@ footer field. The encryption record is
-- serialised opaquely into 'orcEncryption'; readers round-trip it
-- verbatim through 'readORCFooter'.
--
-- The caller is responsible for deciding which stripes (all, per
-- column) get encrypted, and for producing an appropriate
-- 'Enc.Encryption' record (keys + variants + masks). A file-writer
-- that mixes plaintext and encrypted stripes can do so by building
-- per-stripe 'StripeEncryption' lazily and conditionally calling
-- 'encryptStripeStreams' on the stripes it wants protected.
buildEncryptedORCFile
  :: V.Vector ORCType
  -> V.Vector (V.Vector (Word64, Word64, ByteString))
  -> V.Vector (Maybe StripeEncryption)
    -- ^ One entry per stripe; 'Nothing' leaves that stripe in
    -- plaintext, 'Just' encrypts its streams with AES-CTR using the
    -- derived per-stripe key.
  -> Enc.Encryption
    -- ^ File-level encryption metadata emitted in the footer.
  -> Either String ByteString
buildEncryptedORCFile types stripeData stripeKeys enc
  | V.length stripeKeys /= V.length stripeData =
      Left $ "ORC.Write.buildEncryptedORCFile: stripeKeys length "
             ++ show (V.length stripeKeys)
             ++ " must match stripeData length "
             ++ show (V.length stripeData)
  | otherwise = do
      encryptedStripes <- V.imapM
        (\i sdata -> case V.unsafeIndex stripeKeys i of
           Nothing -> Right sdata
           Just se -> encryptStripeStreams se sdata)
        stripeData
      let !encFieldBytes = Enc.encodeEncryption enc
          !footerTransform =
            \fm -> fm { orcEncryption = Just (FooterEncryption encFieldBytes) }
      Right $! buildORCFileWith footerTransform types encryptedStripes
