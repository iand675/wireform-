{-# LANGUAGE BangPatterns #-}

{- | High-level Parquet API.

95% of callers should reach for this module. It hides the
@ColumnAux@ parallel-array machinery, the encrypted-footer
variant, and the per-row-group compression knobs of
"Parquet.Write" behind a single record-of-options:

@
-- Encode
let bytes = 'encodeParquet' 'defaultWriteOptions'
                           schema rowGroups

-- Read
case 'decodeParquet' bytes of
  Right (schema', rowGroups') -> ...
  Left  err                   -> ...
@

@rowGroups@ is a @[V.Vector ColumnData]@ — one entry per row
group, each entry one 'ColumnData' per /leaf/ schema column in
the same order @schema@ enumerates them.

The 'WriteOptions' record consolidates every Parquet writer
knob in one place: compression, page version, bloom filters,
page-index emission, per-column encryption, and footer
encryption (PARE mode). Defaults are the modern-Parquet
recommended settings (Snappy, V2 pages, no encryption).

For lower-level control (per-column compression overrides,
bespoke 'ColumnAux' values, custom row-group dictionaries) drop
down to "Parquet.Write".
-}
module Parquet.HighLevel (
  -- * Encoding
  encodeParquet,
  encodeParquetIO,
  encodeParquetMixed,
  encodeParquetMixedIO,
  encodeParquetNested,
  encodeParquetNestedWith,
  encodeParquetNestedIO,
  FooterMode (..),
  WriteOptions (..),
  defaultWriteOptions,
  ParquetColumn (..),
  OptionalColumn (..),

  -- * Decoding
  decodeParquet,
  ReadOptions (..),
  defaultReadOptions,
  FooterDecryption (..),
  DecryptionConfig (..),
  defaultDecryptionConfig,
  ColumnKeyMetadata (..),
  encryptedColumnKeyMetadata,
  ParquetFile (..),

  -- * Re-exports for convenience
  ColumnData (..),
  ColumnEncryption (..),
  FooterEncryption (..),
  Compression (..),
  PageVersion (..),
  SchemaElement (..),
  NestedSchema (..),
  NestedRow (..),
  LeafType (..),
  LeafValue (..),
) where

import Crypto.Random.Types qualified as RNG
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Parquet.Arrow qualified as Arrow
import Parquet.BloomFilter qualified as Bloom
import Parquet.Compress (validateCompression)
import Parquet.Footer qualified as Footer
import Parquet.Decryption (ColumnKeyMetadata (..), DecryptionConfig (..), defaultDecryptionConfig)
import Parquet.Nested (
  LeafType (..),
  LeafValue (..),
  NestedRow (..),
  NestedSchema (..),
  buildNestedFile,
  buildNestedFileWithAux,
  buildNestedFileWithBloomFilters,
  buildNestedFileWithBloomFiltersUsing,
  nestedSchemaToFlatSchema,
 )
import Parquet.Read (
  FooterDecryption (..),
  ParquetFile (..),
  encryptedColumnKeyMetadata,
  loadParquetFile,
  loadParquetFileEncrypted,
  loadParquetFileWithDecryption,
 )
import Parquet.Types (
  Compression (..),
  FileMetadata,
  SchemaElement (..),
 )
import Parquet.Write.Encrypted (FooterMode (..))
import Parquet.Write.Encrypted qualified as Encrypted
import Parquet.Write (
  ColumnAux (..),
  ColumnData (..),
  ColumnEncryption (..),
  FooterEncryption (..),
  OptionalColumn (..),
  PageVersion (..),
  ParquetColumn (..),
  buildParquetFileMixedWithAux,
  buildParquetFileMixedWithAuxUsing,
  buildParquetFileWithIndex,
  buildParquetFileWithIndexEncryptedFooter,
  buildParquetFileWithIndexIO,
  emptyColumnAux,
  optionalColumnPresentValues,
 )
import Wireform.Builder qualified as B


-- ============================================================
-- Options
-- ============================================================

{- | Parquet writer configuration. Construct one with
'defaultWriteOptions' and override the fields you care about:

@
let opts = 'defaultWriteOptions'
            { writeCompression = 'ZSTD'
            , writeBloomFilters = ["sku"]
            }
    bytes = 'encodeParquet' opts schema rowGroups
@
-}
data WriteOptions = WriteOptions
  { writeCompression :: !Compression
  {- ^ Compression codec applied to every column's data pages.
  Default: 'Snappy'. Use 'Uncompressed' to skip.
  -}
  , writePageVersion :: !PageVersion
  {- ^ Data page version. 'PageV2' is recommended for modern
  writers; 'PageV1' interoperates with older Parquet readers.
  Default: 'PageV2'.
  -}
  , writeBloomFilters :: ![Text]
  {- ^ Column paths (top-level field names) that should carry a
  split-block bloom filter. Empty list → no bloom filters.
  Filter parameters use parquet-cpp's defaults
  (~3% FPP at 1024 distinct values per row group).
  -}
  , writePageIndex :: !Bool
  {- ^ Emit per-column 'OffsetIndex' / 'ColumnIndex' regions in
  the trailing page-index area. Default: 'True' — page
  indexes substantially improve scan performance for filter
  pushdown and most ecosystem readers (pyarrow, parquet-mr,
  DuckDB, Trino) take advantage of them when present.
  -}
  , writeColumnEncryption :: !(V.Vector (Maybe ColumnEncryption))
  {- ^ Per-leaf-column encryption configuration. The vector
  length must match the number of leaf columns; 'Nothing'
  entries leave the column in plaintext. Default:
  'V.empty' (no per-column encryption — interpreted as
  "all columns plaintext").
  -}
  , writeFooterMode :: !FooterMode
  -- ^ Encrypted footer (default), or an authenticated plaintext footer.
  , writeFooterEncryption :: !(Maybe FooterEncryption)
  {- ^ Footer encryption/signing key and file AAD context. 'writeFooterMode'
  selects an encrypted footer (PARE) or a signed plaintext footer (PAR1).
  'Nothing' means an ordinary unencrypted file and requires plaintext columns.
  -}
  }
  deriving (Show, Eq)


{- | Sensible modern-Parquet defaults: Snappy compression, V2
pages, page indexes on, no encryption, no bloom filters.
-}
defaultWriteOptions :: WriteOptions
defaultWriteOptions =
  WriteOptions
    { writeCompression = Snappy
    , writePageVersion = PageV2
    , writeBloomFilters = []
    , writePageIndex = True
    , writeColumnEncryption = V.empty
    , writeFooterMode = EncryptedFooter
    , writeFooterEncryption = Nothing
    }


-- ============================================================
-- Encoding
-- ============================================================

{- | Serialise a Parquet file from a schema + row groups, applying
the supplied options.

The schema is a flat 'V.Vector' of 'SchemaElement' (one entry
per node in the parquet schema tree, including the synthetic
root). Row groups are a list of column-major collections — one
'V.Vector' 'ColumnData' per row group, each containing exactly
one entry per leaf column in declaration order.
-}
encodeParquet
  :: WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ColumnData]
  -> ByteString
encodeParquet opts _ _
  | encryptionRequested opts =
      error "Parquet.HighLevel: encryption requires encodeParquetIO"
encodeParquet opts schema rgs =
  let !rowGroups = V.fromList rgs
      !auxes = V.map (mkAuxes opts schema) rowGroups
  in case validateCompression (writeCompression opts) >> Right (writeFooterEncryption opts) of
       Left err -> error err
       Right Nothing -> buildParquetFileWithIndex schema rowGroups auxes
       Right (Just _) -> error "Parquet.HighLevel: encryption requires encodeParquetIO"


{- | Encoding with random encryption nonces. Invalid encryption keys return
'Left', with no plaintext file produced. Unencrypted callers can continue
to use 'encodeParquet'.
-}
encodeParquetIO
  :: RNG.MonadRandom m
  => WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ColumnData]
  -> m (Either String ByteString)
encodeParquetIO opts schema rgs =
  case validateCompression (writeCompression opts) >> validateWriteEncryption opts schema of
    Left err -> pure (Left err)
    Right () ->
      let groups = V.fromList rgs
      in case writeFooterEncryption opts of
        Nothing -> buildParquetFileWithIndexIO schema groups (V.map (mkAuxes opts schema) groups)
        Just footer
          | writeFooterMode opts == EncryptedFooter
          , V.all (maybe True ((== feKey footer) . ceKey)) (writeColumnEncryption opts) ->
              buildParquetFileWithIndexEncryptedFooter footer schema groups (V.map (mkAuxes opts schema) groups)
        Just _ -> do
          let plainOptions = withoutEncryption opts
              auxes = V.map (mkAuxes plainOptions schema) groups
          plaintext <- buildParquetFileWithIndexIO schema groups auxes
          finishEncrypted opts plaintext


withoutEncryption :: WriteOptions -> WriteOptions
withoutEncryption opts = opts {writeColumnEncryption = V.empty, writeFooterEncryption = Nothing, writeFooterMode = EncryptedFooter}


finishEncrypted :: RNG.MonadRandom m => WriteOptions -> Either String ByteString -> m (Either String ByteString)
finishEncrypted _ (Left err) = pure (Left err)
finishEncrypted opts (Right plaintext) = case writeFooterEncryption opts of
  Nothing -> pure (Right plaintext)
  Just footer -> Encrypted.encryptWrittenFile (writeFooterMode opts) footer (writeColumnEncryption opts) plaintext


finishWriterOutput :: RNG.MonadRandom m => FooterEncryption -> WriteOptions -> Either String Encrypted.WriterOutput -> m (Either String ByteString)
finishWriterOutput _ _ (Left err) = pure (Left err)
finishWriterOutput footer opts (Right output) =
  Encrypted.encryptWriterOutput (writeFooterMode opts) footer (writeColumnEncryption opts) output


{- | Build the leaf-name -> column-index lookup for a flat
schema. The synthetic root is at index 0; leaves start at 1
in the schema vector but are addressed 0-indexed in the
per-row-group columns. Used by the bloom-filter populator to
map the @writeBloomFilters@ name list to column positions.
-}
leafColumnIndex :: V.Vector SchemaElement -> Text -> Maybe Int
leafColumnIndex schema name =
  let leaves = V.filter (maybe False (const True) . seType) schema
  in V.findIndex ((== name) . seName) leaves


{- | Serialise a Parquet file from a schema + row groups where
each column may be either required ('PCRequired') or nullable
('PCOptional'). Honors compression, page version, bloom filters and page
indexes. Unsupported codecs and invalid layouts raise a descriptive error;
they never silently fall back to another codec. Use 'encodeParquetMixedIO'
for encryption with fresh random nonces.
-}
encodeParquetMixed
  :: WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ParquetColumn]
  -> ByteString
encodeParquetMixed opts _ _
  | encryptionRequested opts =
      error "Parquet.HighLevel: encryption is not supported by the mixed writer"
encodeParquetMixed opts schema rgs =
  either error id $
    validateCompression (writeCompression opts) >>
      buildParquetFileMixedWithAuxUsing plainMixedOutput schema (V.fromList rgs) (V.map (mkAuxes opts schema))


plainMixedOutput :: FileMetadata -> [ByteString] -> ByteString
plainMixedOutput metadata parts = BS.concat (Footer.parquetMagic : parts ++ [Footer.writeFooter metadata])


-- | Nullable/required columns with fresh random encryption nonces.
encodeParquetMixedIO
  :: RNG.MonadRandom m
  => WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ParquetColumn]
  -> m (Either String ByteString)
encodeParquetMixedIO opts schema rgs =
  case validateCompression (writeCompression opts) >> validateWriteEncryption opts schema of
    Left err -> pure (Left err)
    Right () ->
      let plainOptions = withoutEncryption opts
          groups = V.fromList rgs
          makeAuxes = V.map (mkAuxes plainOptions schema)
      in case writeFooterEncryption opts of
           Nothing -> pure (buildParquetFileMixedWithAuxUsing plainMixedOutput schema groups makeAuxes)
           Just footer ->
             finishWriterOutput footer opts $
               buildParquetFileMixedWithAuxUsing (\metadata parts -> Encrypted.WriterOutput metadata (BS.concat (Footer.parquetMagic : parts))) schema groups makeAuxes


encryptionRequested :: WriteOptions -> Bool
encryptionRequested opts =
  writeFooterMode opts == SignedPlaintextFooter
    || maybe False (const True) (writeFooterEncryption opts)
    || V.any (maybe False (const True)) (writeColumnEncryption opts)


{- | Compute a 'ColumnAux' vector per row group from the
write-time options.

Compression + page version are applied uniformly to every
column. Bloom filters are built per-column for the leaves
whose names appear in 'writeBloomFilters' — the writer hashes
each value's PLAIN payload into a fresh 'Sbbf' sized for the
column's row count so a downstream reader can probe membership
via 'Parquet.Predicate.evalBloomChunk' without false
negatives.
-}
mkAuxes
  :: WriteOptions
  -> V.Vector SchemaElement
  -> V.Vector ColumnData
  -> V.Vector ColumnAux
mkAuxes opts schema cols =
  let !bloomNames = writeBloomFilters opts
      bloomIdxs =
        foldr (\name indices -> maybe indices (: indices) (leafColumnIndex schema name)) [] bloomNames
      bloomSet = bloomIdxs
  in V.imap (\i col -> mkOne i col bloomSet) cols
  where
    mkOne :: Int -> ColumnData -> [Int] -> ColumnAux
    mkOne i col blooms =
      emptyColumnAux
        { caCodec = writeCompression opts
        , caEncryption = maybe Nothing id (writeColumnEncryption opts V.!? i)
        , caGeneratePageIndex = writePageIndex opts
        , caPageVersion = writePageVersion opts
        , caBloomFilter =
            if i `elem` blooms
              then Just $! buildBloomFilterFor col
              else Nothing
        }


-- Check the requested configuration even for an empty dataset. Building aux
-- metadata from zero row groups cannot detect a truncated key vector or a
-- missing encrypted footer on its own.
validateWriteEncryption :: WriteOptions -> V.Vector SchemaElement -> Either String ()
validateWriteEncryption opts schema
  | not (V.null columns) && V.length columns /= V.length leaves = Left "Parquet.HighLevel: column encryption vector must match the leaf-column count"
  | writeFooterMode opts == SignedPlaintextFooter && writeFooterEncryption opts == Nothing = Left "Parquet.HighLevel: signed plaintext footer requires a signing key"
  | otherwise = do
      case writeFooterEncryption opts of
        Nothing -> Right ()
        Just footer
          | BS.length (feKey footer) `notElem` [16,24,32] -> Left "Parquet.HighLevel: AES key must contain 16, 24, or 32 bytes"
          | BS.null (feFileId footer) -> Left "Parquet.HighLevel: footer requires a fresh random file identifier"
          | otherwise -> Right ()
      V.imapM_ check columns
  where
    columns = writeColumnEncryption opts
    leaves = V.filter (maybe False (const True) . seType) schema
    algorithm =
      V.foldl'
        ( \found entry -> case found of
            Just _ -> found
            Nothing -> ceAlgorithm <$> entry
        )
        Nothing
        columns
    check _ Nothing = Right ()
    check index (Just column)
      | index > 32767 = Left "Parquet.HighLevel: encrypted column ordinal exceeds signed 16-bit range"
      | BS.length (ceKey column) `notElem` [16, 24, 32] = Left "Parquet.HighLevel: AES key must contain 16, 24, or 32 bytes"
      | otherwise = case writeFooterEncryption opts of
          Nothing -> Left "Parquet.HighLevel: encrypted columns require an encrypted footer"
          Just footer
            | ceFileId column /= feFileId footer || ceAadPrefix column /= feAadPrefix footer -> Left "Parquet.HighLevel: column and footer AAD contexts must match"
            | Just (ceAlgorithm column) /= algorithm -> Left "Parquet.HighLevel: encrypted columns must share one file-level algorithm"
            | otherwise -> Right ()


{- | Build a split-block bloom filter populated from a column's
values. Sizes the filter via 'Bloom.optimalNumBytes' for the
column's row count at a 1% false-positive rate (parquet-cpp's
default), then inserts each value's PLAIN-encoded payload —
matching what 'Parquet.Predicate.encodePlain' probes with on
the read side.
-}
buildBloomFilterFor :: ColumnData -> Bloom.Sbbf
buildBloomFilterFor col =
  let !ndv = max 1 (columnDistinctEstimate col)
      !nBytes = Bloom.optimalNumBytes ndv 0.01
      !empty0 = Bloom.newSbbf nBytes
  in case col of
       ColInt32 v ->
         VP.foldl' (\acc x -> Bloom.sbbfInsert (i32LE x) acc) empty0 v
       ColInt64 v ->
         VP.foldl' (\acc x -> Bloom.sbbfInsert (i64LE x) acc) empty0 v
       ColFloat v ->
         VP.foldl' (\acc x -> Bloom.sbbfInsert (f32LE x) acc) empty0 v
       ColDouble v ->
         VP.foldl' (\acc x -> Bloom.sbbfInsert (f64LE x) acc) empty0 v
       ColBool v ->
         V.foldl' (\acc x -> Bloom.sbbfInsert (boolPayload x) acc) empty0 v
       ColByteArray v ->
         V.foldl' (\acc x -> Bloom.sbbfInsert x acc) empty0 v


{- | Cheap distinct-value upper bound: row count. Real
distinct-counting would need a second pass; sizing for the
worst case (all-distinct) keeps the filter slightly oversize
but never underfilled.
-}
columnDistinctEstimate :: ColumnData -> Int
columnDistinctEstimate = \case
  ColInt32 v -> VP.length v
  ColInt64 v -> VP.length v
  ColFloat v -> VP.length v
  ColDouble v -> VP.length v
  ColBool v -> V.length v
  ColByteArray v -> V.length v


{- | PLAIN encodings used for bloom-filter inserts. These must
match 'Parquet.Predicate.encodePlain' byte-for-byte so a
@PEq@ predicate hashes to the same key.
-}
i32LE :: Int32 -> ByteString
i32LE = BL.toStrict . B.toLazyByteString . B.int32LE


i64LE :: Int64 -> ByteString
i64LE = BL.toStrict . B.toLazyByteString . B.int64LE


f32LE :: Float -> ByteString
f32LE f = BL.toStrict (B.toLazyByteString (B.word32LE (castFloatToWord32 f)))


f64LE :: Double -> ByteString
f64LE d = BL.toStrict (B.toLazyByteString (B.word64LE (castDoubleToWord64 d)))


boolPayload :: Bool -> ByteString
boolPayload True = TE.encodeUtf8 "\x01"
boolPayload False = TE.encodeUtf8 "\x00"


-- | Nested records with random module nonces, per-leaf keys and either
-- encrypted or signed plaintext footer. Pure nested writing remains available
-- through 'encodeParquetNestedWith' when encryption is not requested.
encodeParquetNestedIO
  :: RNG.MonadRandom m
  => WriteOptions
  -> V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> m (Either String ByteString)
encodeParquetNestedIO opts columns rows =
  case validateCompression (writeCompression opts) >> validateWriteEncryption opts schema of
    Left err -> pure (Left err)
    Right () -> case writeFooterEncryption opts of
      Nothing -> pure (encodeParquetNestedWith (withoutEncryption opts) columns rows)
      Just footer ->
        let leaves = V.foldl' (\count field -> count + maybe 0 (const 1) (seType field)) 0 schema
            aux = emptyColumnAux {caCodec = writeCompression opts, caPageVersion = writePageVersion opts, caGeneratePageIndex = writePageIndex opts}
        in finishWriterOutput footer opts $
             buildNestedFileWithBloomFiltersUsing Encrypted.nestedWriterOutput (writeBloomFilters opts) (V.replicate leaves aux) columns rows
  where
    schema = nestedSchemaToFlatSchema columns


-- ============================================================
-- Decoding
-- ============================================================

{- | Serialise a Parquet file carrying /nested/ (struct / list /
map / variant) columns. Delegates to 'Parquet.Nested.buildNestedFile'
after the Dremel shredder in "Parquet.Nested".

@columns@ is a vector of @(column-name, schema)@ pairs;
@rowsPerColumn@ is a parallel vector where the i-th entry is
the row-major list of 'NestedRow' values for column @i@. Every
column must carry the same number of rows.

This entry point retains the original uncompressed @DATA_PAGE_V2@
layout. Use 'encodeParquetNestedWith' to select compression, page version
and indexes with 'WriteOptions', or 'encodeParquetNestedIO' for encryption.
-}
encodeParquetNested
  :: V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String ByteString
encodeParquetNested = buildNestedFile


{- | Nested writing with compression, page version and generated indexes.
The existing 'encodeParquetNested' retains its uncompressed V2 defaults.
Names in 'writeBloomFilters' select top-level fields and populate every
physical leaf below them. Unknown names return 'Left'. Encryption uses
'encodeParquetNestedIO'. Pre-built per-leaf filters and explicit indexes are available
through 'buildNestedFileWithAux'.
-}
encodeParquetNestedWith
  :: WriteOptions
  -> V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String ByteString
encodeParquetNestedWith opts columns rows
  | encryptionRequested opts = Left "Parquet.HighLevel: nested encryption requires an IO writer"
  | otherwise =
      let schema = nestedSchemaToFlatSchema columns
          leaves = V.length (V.filter (maybe False (const True) . seType) schema)
          aux = emptyColumnAux {caCodec = writeCompression opts, caPageVersion = writePageVersion opts, caGeneratePageIndex = writePageIndex opts}
      in buildNestedFileWithBloomFilters (writeBloomFilters opts) (V.replicate leaves aux) columns rows


-- ============================================================
-- Decoding
-- ============================================================

{- | Parquet reader configuration. Construct one with
'defaultReadOptions' and override the fields you care about.

@
let opts = 'defaultReadOptions'
            { readFooterDecryption = Just ('FooterDecryption' key fileId prefix) }
'decodeParquet' opts bytes
@
-}
data ReadOptions = ReadOptions
  { readFooterDecryption :: !(Maybe FooterDecryption)
  {- ^ When 'Just', authenticate an encrypted footer (PARE) or a signed
  plaintext footer (PAR1) with the supplied key, file identifier and AAD
  prefix. Ordinary unsigned files are also accepted. When 'Nothing'
  (the default), a plaintext footer is parsed without signature verification.
  -}
  , readDecryptionConfig :: !(Maybe DecryptionConfig)
  -- ^ Full file/column key routing. Do not also set 'readFooterDecryption'.
  }
  deriving (Show, Eq)


-- | Plaintext-footer defaults (matches the common case).
defaultReadOptions :: ReadOptions
defaultReadOptions =
  ReadOptions
    { readFooterDecryption = Nothing
    , readDecryptionConfig = Nothing
    }


{- | Parse a Parquet file's footer. By default expects a
plaintext PAR1 footer without signature verification; pass
'readFooterDecryption' in 'ReadOptions' to decrypt a PARE footer or
authenticate a signed plaintext footer and read footer-key columns. Use
'readDecryptionConfig' for column-specific keys and opaque key-metadata routing.

@
-- plaintext
'decodeParquet' 'defaultReadOptions' bytes

-- encrypted footer
'decodeParquet'
  'defaultReadOptions' { 'readFooterDecryption' = Just fd }
  bytes
@
-}
decodeParquet :: ReadOptions -> ByteString -> Either String ParquetFile
decodeParquet opts bytes = do
  file <- case (readDecryptionConfig opts, readFooterDecryption opts) of
    (Nothing, Nothing) -> loadParquetFile bytes
    (Nothing, Just fd) -> loadParquetFileEncrypted fd bytes
    (Just config, Nothing) -> loadParquetFileWithDecryption config bytes
    (Just _, Just _) -> Left "Parquet.HighLevel: set only one decryption configuration"
  _ <- Arrow.parquetFileArrowSchemaChecked file
  Right file
