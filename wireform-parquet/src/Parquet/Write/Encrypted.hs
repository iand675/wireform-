{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Finalize files from our single-data-page writers with modular encryption.
The private seam keeps nullable/nested encoding and compression independent
of encryption. It is not an API for rewriting arbitrary external files.
-}
module Parquet.Write.Encrypted (
  FooterMode (..),
  encryptWrittenFile,
  WriterOutput (..),
  nestedWriterOutput,
  encryptWriterOutput,
) where

import Control.Monad (unless)
import Crypto.Cipher.AES.GCM qualified as GCM
import Crypto.Error (CryptoFailable (..))
import Crypto.Random.Types qualified as RNG
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Word (Word32)
import Parquet.Encryption qualified as E
import Parquet.Footer qualified as F
import Parquet.Page qualified as P
import Parquet.PageIndex qualified as I
import Parquet.Thrift.Schema
import Parquet.Types
import Parquet.Write (ColumnEncryption (..), FooterEncryption (..), encodePageHeader)
import Thrift.Decode (decodeCompactFrom)
import Thrift.Encode (compactEncodedSizeBounded, encodeCompactBounded)
import Thrift.Value qualified as TV
import Wireform.Builder qualified as B


data FooterMode = EncryptedFooter | SignedPlaintextFooter
  deriving (Show, Eq)


-- A package-private writer result: magic and page/auxiliary bytes, without
-- a serialized footer. The metadata is already available from the writer.
data WriterOutput = WriterOutput !FileMetadata ~ByteString


nestedWriterOutput :: FileMetadata -> Integer -> Either String () -> B.Builder -> Either String WriterOutput
nestedWriterOutput metadata size validate body = do
  require (size >= 0 && size + 4 <= toInteger (maxBound :: Int)) "complete writer body exceeds addressable size"
  validate
  pure (WriterOutput metadata (B.toStrictByteStringWith (fromInteger size + 4) (B.byteString F.parquetMagic <> body)))


data PreparedFile = PreparedFile
  { preparedMetadata :: !FileMetadata
  , preparedGroups :: !(V.Vector (V.Vector PreparedColumn))
  , preparedAlgorithm :: !E.EncryptionAlgorithm
  , preparedFooterContext :: !GCM.Context
  }


data PreparedColumn = PreparedColumn
  { originalChunk :: !ColumnChunk
  , originalMetadata :: !ColumnMetadata
  , originalHeader :: !P.PageHeader
  , originalHeaderSize :: {-# UNPACK #-} !Int
  , originalBody :: !ByteString
  , originalBloom :: !(Maybe BloomParts)
  , originalOffsets :: !(Maybe OffsetIndex)
  , originalIndex :: !(Maybe ByteString)
  , columnEncryption :: !(Maybe ColumnEncryption)
  , columnContext :: !(Maybe GCM.Context)
  }


data BloomParts = BloomParts !ByteString !ByteString


data FileAssembly = FileAssembly
  { fileCursor :: {-# UNPACK #-} !Int
  , fileParts :: ![ByteString]
  , fileGroups :: ![RowGroup]
  }


data GroupAssembly = GroupAssembly
  { groupCursor :: {-# UNPACK #-} !Int
  , groupParts :: ![ByteString]
  , groupColumns :: ![ColumnChunk]
  , groupUncompressed :: {-# UNPACK #-} !Int64
  }


data EmittedColumn = EmittedColumn
  { emittedChunk :: !ColumnChunk
  , emittedParts :: ![ByteString]
  , emittedEnd :: {-# UNPACK #-} !Int
  }


{- | All layout and key checks precede random generation. A failed request
returns no bytes; encryption never falls back to a plaintext output.
-}
encryptWrittenFile
  :: RNG.MonadRandom m
  => FooterMode
  -> FooterEncryption
  -> V.Vector (Maybe ColumnEncryption)
  -> ByteString
  -> m (Either String ByteString)
encryptWrittenFile mode footer columns plaintext =
  case F.readFooter plaintext of
    Left err -> pure (Left err)
    Right metadata -> encryptWriterOutput mode footer columns (WriterOutput metadata plaintext)


encryptWriterOutput
  :: RNG.MonadRandom m
  => FooterMode
  -> FooterEncryption
  -> V.Vector (Maybe ColumnEncryption)
  -> WriterOutput
  -> m (Either String ByteString)
encryptWriterOutput mode footer columns (WriterOutput metadata plaintext) =
  case prepare footer columns metadata plaintext of
    Left err -> pure (Left err)
    Right plan -> do
      nonces <- V.mapM (V.mapM (\column -> case columnEncryption column of Nothing -> pure BS.empty; Just _ -> RNG.getRandomBytes 96)) (preparedGroups plan)
      footerNonce <- RNG.getRandomBytes 12
      pure (assemble mode footer plan nonces footerNonce)


prepare :: FooterEncryption -> V.Vector (Maybe ColumnEncryption) -> FileMetadata -> ByteString -> Either String PreparedFile
prepare footer columns metadata plaintext = do
  validKey (feKey footer)
  require (not (BS.null (feFileId footer))) "a fresh random file identifier is required"
  require (BS.take 4 plaintext == F.parquetMagic) "expected a plaintext writer output"
  let leaves = V.filter (maybe False (const True) . seType) (fmSchema metadata)
      algorithm = fromMaybe E.AesGcmV1 (V.foldl' firstAlgorithm Nothing columns)
  require (V.null columns || V.length columns == V.length leaves) "column encryption vector must match the leaf-column count"
  V.imapM_ (checkColumn algorithm) columns
  footerContext <- newGcmContext (feKey footer)
  contexts <- V.mapM (traverse (\ce -> if ceKey ce == feKey footer then Right footerContext else newGcmContext (ceKey ce))) columns
  groups <- V.imapM (prepareGroup contexts) (fmRowGroups metadata)
  -- Eight module slots per chunk and one footer signature. Count actual keys
  -- before encryption, conservatively including optional/unused slots. Bound
  -- local invocations without constructing per-page counters in the hot loop.
  let counts = V.foldl' (V.foldl' countColumn) (Map.singleton (feKey footer) (1 :: Integer)) groups
  require (all (<= 4294967296) (Map.elems counts)) "AES key invocation limit exceeded"
  pure (PreparedFile metadata groups algorithm footerContext)
  where
    firstAlgorithm found entry = case found of
      Just _ -> found
      Nothing -> ceAlgorithm <$> entry
    countColumn counts column = case columnEncryption column of
      Nothing -> counts
      Just ce -> Map.insertWith (+) (ceKey ce) 8 counts
    checkColumn algorithm col entry = case entry of
      Nothing -> Right ()
      Just ce -> do
        require (col <= 32767) "encrypted column ordinal exceeds signed 16-bit range"
        validKey (ceKey ce)
        require (ceFileId ce == feFileId footer && ceAadPrefix ce == feAadPrefix footer) "column and footer AAD contexts must match"
        require (ceAlgorithm ce == algorithm) "encrypted columns must share one file-level algorithm"
    prepareGroup contexts rg group = do
      require (rg <= 32767) "row-group ordinal exceeds signed 16-bit range"
      V.imapM (prepareColumn contexts rg) (rgColumns group)
    prepareColumn contexts _ col chunk = do
      require (ccFilePath chunk == Nothing) "external column files are not writer outputs"
      cm <- maybe (failure "missing column metadata") Right (ccMetadata chunk)
      require (cmDictionaryPageOffset cm == Nothing) "expected one non-dictionary data page"
      raw <- slice plaintext (cmDataPageOffset cm) (cmTotalCompressedSize cm)
      (header, end) <- P.readPageHeaderAt raw 0
      size <- maybe (failure "missing compressed page size") (Right . fromIntegral) (P.phCompressedPageSize header)
      require (size >= 0 && end <= BS.length raw && size == BS.length raw - end) "expected exactly one complete data page"
      require (P.pageTypeIsDataPageV1 (P.phType header) || P.pageTypeIsDataPageV2 (P.phType header)) "expected a data page"
      bloom <- traverse (prepareBloom cm) (cmBloomFilterOffset cm)
      offsets <- auxiliary (ccOffsetIndexOffset chunk) (ccOffsetIndexLength chunk) >>= traverse I.decodeOffsetIndex
      case offsets of
        Nothing -> Right ()
        Just oi -> require (V.length (oiPageLocations oi) == 1) "offset index must describe the single data page"
      index <- auxiliary (ccColumnIndexOffset chunk) (ccColumnIndexLength chunk)
      pure (PreparedColumn chunk cm header end (BS.drop end raw) bloom offsets index (fromMaybe Nothing (columns V.!? col)) (fromMaybe Nothing (contexts V.!? col)))
    auxiliary Nothing Nothing = Right Nothing
    auxiliary (Just offset) (Just len) = Just <$> slice plaintext offset (fromIntegral len)
    auxiliary _ _ = failure "incomplete auxiliary location"
    prepareBloom cm offset = do
      raw <- slice plaintext offset (maybe (fromIntegral (BS.length plaintext) - offset) fromIntegral (cmBloomFilterLength cm))
      (value, end) <- decodeCompactFrom raw 0
      size <- case value of
        TV.Struct fields -> case V.find ((== fst (BloomFilterHeader_NumBytes 0)) . fst) fields of
          Just (BloomFilterHeader_NumBytes n) -> Right (fromIntegral n)
          _ -> failure "missing bloom bitset length"
        _ -> failure "invalid bloom header"
      require (size > 0 && size `rem` 32 == 0 && end <= BS.length raw && size <= BS.length raw - end) "invalid bloom bitset bounds"
      case cmBloomFilterLength cm of
        Just _ -> require (end + size == BS.length raw) "bloom size differs from metadata"
        Nothing -> Right ()
      pure (BloomParts (BS.take end raw) (BS.take size (BS.drop end raw)))


assemble :: FooterMode -> FooterEncryption -> PreparedFile -> V.Vector (V.Vector ByteString) -> ByteString -> Either String ByteString
assemble mode footer plan nonces footerNonce = do
  output <- V.ifoldM emitGroup (FileAssembly 4 [] []) (preparedGroups plan)
  let metadata = (preparedMetadata plan) {fmRowGroups = V.fromList (reverse (fileGroups output))}
  raw <- cryptoMetadata mode footer (preparedGroups plan) nonces metadata
  let algorithm = algorithmFields (preparedAlgorithm plan) footer
      aad = E.buildAad (feAadPrefix footer) (E.buildAadSuffix (feFileId footer) E.ModuleFooter 0 0 0)
      magic = case mode of EncryptedFooter -> F.parquetEncryptedMagic; SignedPlaintextFooter -> F.parquetMagic
  let footerLimit = fromInteger (min (toInteger (maxBound :: Word32)) (toInteger (maxBound :: Int) - toInteger (fileCursor output) - 8))
  require (footerLimit >= 32) "output has no room for an authenticated footer"
  finalFooter <- case mode of
    EncryptedFooter -> do
      let cryptoValue = TV.Struct (V.fromList [FileCryptoMetaData_EncryptionAlgorithm algorithm, FileCryptoMetaData_KeyMetadata (feKeyMetadata footer)])
      cryptoSize <- compactEncodedSizeBounded (footerLimit - 32) cryptoValue
      -- Validate the remaining combined budget while encoding the plain
      -- metadata once. Delay even the crypto-header allocation until that
      -- succeeds, since a legal header alone can exhaust the footer budget.
      plain <- encodeCompactBounded (footerLimit - 32 - cryptoSize) raw
      crypto <- encodeCompactBounded cryptoSize cryptoValue
      cipher <- encryptGcmFramed (preparedFooterContext plan) footerNonce aad plain
      F.writeRawFooterEither magic (crypto <> cipher)
    SignedPlaintextFooter -> do
      fields <- struct raw
      plain <- encodeCompactBounded (footerLimit - 28) (TV.Struct (fields <> V.fromList [FileMetadata_EncryptionAlgorithm algorithm, FileMetadata_FooterSigningKeyMetadata (feKeyMetadata footer)]))
      cipher <- encryptGcmTagged (preparedFooterContext plan) footerNonce aad plain
      F.writeRawFooterEither magic (plain <> footerNonce <> BS.takeEnd 16 cipher)
  _ <- checkedAdd (fileCursor output) (BS.length finalFooter)
  pure (BS.concat (magic : reverse (fileParts output) ++ [finalFooter]))
  where
    emitGroup acc rg columns = do
      out <- V.ifoldM (emitColumn rg) (GroupAssembly (fileCursor acc) (fileParts acc) [] 0) columns
      let original = fmRowGroups (preparedMetadata plan) V.! rg
          group = original {rgColumns = V.fromList (reverse (groupColumns out)), rgTotalByteSize = groupUncompressed out}
      pure (FileAssembly (groupCursor out) (groupParts out) (group : fileGroups acc))
    emitColumn rg acc col column = do
      out <- emitOne rg col (nonces V.! rg V.! col) (groupCursor acc) column
      cm <- maybe (failure "lost emitted metadata") Right (ccMetadata (emittedChunk out))
      let total = toInteger (groupUncompressed acc) + toInteger (cmTotalUncompressedSize cm)
      require (total <= toInteger (maxBound :: Int64)) "row-group uncompressed size exceeds signed 64-bit limit"
      pure (GroupAssembly (emittedEnd out) (reverse (emittedParts out) ++ groupParts acc) (emittedChunk out : groupColumns acc) (fromInteger total))


emitOne :: Int -> Int -> ByteString -> Int -> PreparedColumn -> Either String EmittedColumn
emitOne rg col nonces start column = do
  let ce = columnEncryption column
      cm = originalMetadata column
      original = originalChunk column
      body = originalBody column
      header = originalHeader column
      gcm slot kind = encryptCached ce (columnContext column) nonces slot kind rg col
  let overhead = case ce of
        Nothing -> 0
        Just settings -> case ceAlgorithm settings of E.AesGcmV1 -> 32; E.AesGcmCtrV1 -> 16
  expectedBodySize <- checkedAdd (BS.length body) overhead >>= size32
  encryptedBody <- case ce of
    Nothing -> Right body
    Just settings -> case ceAlgorithm settings of
      E.AesGcmV1 -> gcm 1 E.ModuleDataPage body
      E.AesGcmCtrV1 -> E.encryptCtrModuleFramedWithNonce (ceKey settings) (nonceAt nonces 1) body
  require (BS.length encryptedBody == fromIntegral expectedBodySize) "encrypted body size differs from planned framing"
  let plainHeader = encodePageHeader header {P.phCompressedPageSize = Just expectedBodySize}
  encryptedHeader <- gcm 0 E.ModuleDataPageHeader plainHeader
  pageLength <- checkedAdd (BS.length encryptedHeader) (BS.length encryptedBody)
  pageSize <- case originalOffsets column of
    Nothing -> Right 0
    Just _ -> size32 pageLength
  afterPage <- checkedAdd start pageLength
  bloom <- traverse (emitBloom gcm) (originalBloom column)
  let bloomBytes = fromMaybe BS.empty bloom
  afterBloom <- checkedAdd afterPage (BS.length bloomBytes)
  offsets <- traverse (emitOffsets gcm pageSize) (originalOffsets column)
  let offsetBytes = fromMaybe BS.empty offsets
  offsetSize <- size32 (BS.length offsetBytes)
  afterOffsets <- checkedAdd afterBloom (BS.length offsetBytes)
  index <- traverse (gcm 5 E.ModuleColumnIndex) (originalIndex column)
  let indexBytes = fromMaybe BS.empty index
  indexSize <- size32 (BS.length indexBytes)
  end <- checkedAdd afterOffsets (BS.length indexBytes)
  let sizeDelta = BS.length encryptedHeader - originalHeaderSize column
      uncompressed = toInteger (cmTotalUncompressedSize cm) + toInteger sizeDelta
  require (uncompressed >= 0 && uncompressed <= toInteger (maxBound :: Int64)) "column uncompressed size exceeds signed 64-bit limit"
  let updated =
        cm
          { cmDataPageOffset = fromIntegral start
          , cmTotalCompressedSize = fromIntegral pageLength
          , cmTotalUncompressedSize = fromInteger uncompressed
          , cmBloomFilterOffset = fromIntegral afterPage <$ bloom
          , -- Optional i32 length may be omitted for a legal large bitset whose
            -- two encrypted modules no longer fit in the signed length field.
            cmBloomFilterLength = if BS.length bloomBytes <= fromIntegral (maxBound :: Int32) then fromIntegral (BS.length bloomBytes) <$ bloom else Nothing
          }
      chunk =
        original
          { ccFileOffset = fromIntegral start
          , ccMetadata = Just updated
          , ccOffsetIndexOffset = fromIntegral afterBloom <$ offsets
          , ccOffsetIndexLength = offsetSize <$ offsets
          , ccColumnIndexOffset = fromIntegral afterOffsets <$ index
          , ccColumnIndexLength = indexSize <$ index
          }
  pure (EmittedColumn chunk [encryptedHeader, encryptedBody, bloomBytes, offsetBytes, indexBytes] end)
  where
    emitBloom gcm (BloomParts header bits) = (<>) <$> gcm 2 E.ModuleBloomFilterHeader header <*> gcm 3 E.ModuleBloomFilterBitset bits
    emitOffsets gcm size oi =
      gcm 4 E.ModuleOffsetIndex (I.encodeOffsetIndex oi {oiPageLocations = V.map (\p -> p {plOffset = fromIntegral start, plCompressedPageSize = size}) (oiPageLocations oi)})


cryptoMetadata :: FooterMode -> FooterEncryption -> V.Vector (V.Vector PreparedColumn) -> V.Vector (V.Vector ByteString) -> FileMetadata -> Either String TV.Value
cryptoMetadata mode footer plans nonces metadata = do
  fields <- struct (F.fileMetadataToThrift metadata)
  TV.Struct <$> V.mapM rewriteFile fields
  where
    rewriteFile (FileMetadata_RowGroups groups) = FileMetadata_RowGroups <$> V.imapM rewriteGroup groups
    rewriteFile field = Right field
    rewriteGroup rg value = do
      fields <- struct value
      updated <- V.mapM (rewriteGroupField rg) fields
      pure (TV.Struct (V.snoc updated (RowGroup_Ordinal (fromIntegral rg))))
    rewriteGroupField rg (RowGroup_Columns columns) = RowGroup_Columns <$> V.imapM (rewriteColumn rg) columns
    rewriteGroupField _ field = Right field
    rewriteColumn rg col value = case columnEncryption (plans V.! rg V.! col) of
      Nothing -> Right value
      Just ce -> do
        fields <- struct value
        cm <- maybe (failure "missing emitted column metadata") Right (ccMetadata (rgColumns (fmRowGroups metadata V.! rg) V.! col))
        cmFields <- maybe (failure "missing serialized column metadata") (struct . snd) (V.find ((== fst (ColumnChunk_MetaData V.empty)) . fst) fields)
        let footerKey = ceKey ce == feKey footer
            crypto =
              if footerKey
                then V.singleton (ColumnCryptoMetaData_EncryptionWithFooterKey V.empty)
                else V.singleton (ColumnCryptoMetaData_EncryptionWithColumnKey (V.fromList [EncryptionWithColumnKey_PathInSchema (V.map TV.String (cmPathInSchema cm)), EncryptionWithColumnKey_KeyMetadata (ceKeyMetadata ce)]))
            base = V.snoc fields (ColumnChunk_CryptoMetadata crypto)
        if mode == EncryptedFooter && footerKey
          then Right (TV.Struct base)
          else do
            plain <- encode (TV.Struct cmFields)
            encrypted <- encryptCached (Just ce) (columnContext (plans V.! rg V.! col)) (nonces V.! rg V.! col) 6 E.ModuleColumnMetaData rg col plain
            let strip (ColumnChunk_MetaData _) = mode == EncryptedFooter
                strip _ = False
                sanitize (ColumnChunk_MetaData entries) = ColumnChunk_MetaData (V.filter ((/= fst (ColumnMetaData_Statistics V.empty)) . fst) entries)
                sanitize entry = entry
            pure (TV.Struct (V.snoc (V.map sanitize (V.filter (not . strip) base)) (ColumnChunk_EncryptedColumnMetadata encrypted)))


algorithmFields :: E.EncryptionAlgorithm -> FooterEncryption -> V.Vector (Int16, TV.Value)
algorithmFields algorithm footer = V.singleton (variant fields)
  where
    variant = case algorithm of E.AesGcmV1 -> EncryptionAlgorithm_AesGcmV1; E.AesGcmCtrV1 -> EncryptionAlgorithm_AesGcmCtrV1
    fields = V.fromList ([AesGcmV1_AadFileUnique (feFileId footer)] ++ if BS.null (feAadPrefix footer) then [] else [AesGcmV1_AadPrefix (feAadPrefix footer)])


encryptCached :: Maybe ColumnEncryption -> Maybe GCM.Context -> ByteString -> Int -> E.ModuleType -> Int -> Int -> ByteString -> Either String ByteString
encryptCached Nothing _ _ _ _ _ _ bytes = Right bytes
encryptCached (Just ce) (Just context) nonces slot kind rg col bytes =
  encryptGcmFramed
    context
    (nonceAt nonces slot)
    (E.buildAad (ceAadPrefix ce) (E.buildAadSuffix (ceFileId ce) kind (fromIntegral rg) (fromIntegral col) 0))
    bytes
encryptCached _ _ _ _ _ _ _ _ = failure "missing prepared GCM context"


newGcmContext :: ByteString -> Either String GCM.Context
newGcmContext key = case GCM.newContext key of
  CryptoPassed context -> Right context
  CryptoFailed err -> failure ("GCM key initialization: " ++ show err)


-- Keep all C-width/address checks ahead of the one-shot call. The framing
-- length includes nonce and tag; each module still uses its own fresh nonce.
encryptGcmTagged :: GCM.Context -> ByteString -> ByteString -> ByteString -> Either String ByteString
encryptGcmTagged context nonce aad plain = do
  require (BS.length nonce == 12) "nonce must contain 12 bytes"
  require (toInteger (BS.length aad) <= toInteger (maxBound :: Word32) && toInteger (BS.length plain) <= toInteger (maxBound :: Word32)) "GCM input exceeds unsigned 32-bit C limit"
  _ <- checkedAdd (BS.length plain) 16
  pure (GCM.encrypt context nonce aad plain 16)


encryptGcmFramed :: GCM.Context -> ByteString -> ByteString -> ByteString -> Either String ByteString
encryptGcmFramed context nonce aad plain = do
  framedSize <- checkedAdd (BS.length plain) 28
  require (toInteger framedSize <= toInteger (maxBound :: Word32)) "encrypted module exceeds unsigned 32-bit length"
  total <- checkedAdd framedSize 4
  cipher <- encryptGcmTagged context nonce aad plain
  require (BS.length cipher == framedSize - 12) "GCM output differs from planned tag length"
  pure (B.toStrictByteStringExact total (B.word32LE (fromIntegral framedSize) <> B.byteString nonce <> B.byteString cipher))


nonceAt :: ByteString -> Int -> ByteString
nonceAt bytes slot = BS.take 12 (BS.drop (slot * 12) bytes)


validKey :: ByteString -> Either String ()
validKey key = require (BS.length key `elem` [16, 24, 32]) "AES key must contain 16, 24, or 32 bytes"


slice :: ByteString -> Int64 -> Int64 -> Either String ByteString
slice bytes offset size = do
  require (offset >= 4 && size >= 0 && offset <= fromIntegral (BS.length bytes) && size <= fromIntegral (BS.length bytes) - offset) "writer module lies outside input"
  pure (BS.take (fromIntegral size) (BS.drop (fromIntegral offset) bytes))


checkedAdd :: Int -> Int -> Either String Int
checkedAdd a b = do
  require (a >= 0 && b >= 0 && b <= maxBound - a) "output exceeds addressable byte size"
  pure (a + b)


size32 :: Int -> Either String Int32
size32 size = do
  require (size >= 0 && toInteger size <= toInteger (maxBound :: Int32)) "page or index size exceeds signed 32-bit limit"
  pure (fromIntegral size)


encode :: TV.Value -> Either String ByteString
encode = encodeCompactBounded (fromInteger (min (toInteger (maxBound :: Word32)) (toInteger (maxBound :: Int) - 8)))


struct :: TV.Value -> Either String (V.Vector (Int16, TV.Value))
struct (TV.Struct fields) = Right fields
struct _ = failure "expected metadata struct"


require :: Bool -> String -> Either String ()
require valid message = unless valid (failure message)


failure :: String -> Either String a
failure message = Left ("Parquet.Write.Encrypted: " ++ message)
