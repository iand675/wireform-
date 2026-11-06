{-# LANGUAGE PatternSynonyms #-}

module EncryptedReadRegression (tests) where

import Arrow.Types qualified as AT
import Control.Monad (forM_, unless)
import Data.Bits (complement, shiftR, xor, (.&.))
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word32)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Parquet.Arrow qualified as A
import Parquet.BloomFilter qualified as Bloom
import Parquet.Decryption qualified as D
import Parquet.Encryption qualified as E
import Parquet.Footer qualified as F
import Parquet.HighLevel qualified as H
import Parquet.Page qualified as Page
import Parquet.PageIndex qualified as Index
import Parquet.Predicate qualified as Predicate
import Parquet.Read qualified as R
import Parquet.Thrift.Schema
import Parquet.Types
import Parquet.Write qualified as W
import Thrift.Decode (decodeCompactFrom)
import Thrift.Encode (encodeCompact)
import Thrift.Value qualified as TV


tests :: IO ()
tests = do
  bloomBoundaries
  checksumRegression E.AesGcmV1 W.PageV1 16
  checksumRegression E.AesGcmCtrV1 W.PageV2 24
  forM_ [16, 24, 32] $ \width ->
    forM_ [E.AesGcmV1, E.AesGcmCtrV1] $ \algorithm ->
      forM_ [W.PageV1, W.PageV2] $ \version -> do
        putStrLn ("Encrypted read regression: " ++ show width ++ " / " ++ show algorithm ++ " / " ++ show version)
        let key = BS.replicate width 0x42
            prefix = "tester"
        footer <- W.newFooterEncryption key prefix "kf"
        let encryption = W.ColumnEncryption algorithm key (W.feFileId footer) prefix "" 0
            options =
              H.defaultWriteOptions
                { H.writeCompression = Uncompressed
                , H.writeFooterEncryption = Just footer
                , H.writeColumnEncryption = V.fromList [Just encryption, Nothing]
                , H.writeBloomFilters = ["x"]
                , H.writePageVersion = version
                }
            rows base = V.fromList [W.ColInt32 (VP.fromList [base + 1, base + 2, base + 3, base + 4]), W.ColByteArray (V.fromList ["a", "b", "c", "d"])]
            groups = [rows 0, rows 10]
            config = D.defaultDecryptionConfig {D.dcKeyMetadataKeys = Map.singleton "kf" key}
        bytes <- H.encodeParquetIO options schema groups >>= right
        file <- right (H.decodeParquet H.defaultReadOptions {H.readDecryptionConfig = Just config} bytes)
        check "high-level writer encrypts exactly the requested column" (R.isColumnEncrypted file 0 0 && not (R.isColumnEncrypted file 0 1))
        forM_ [V.singleton (Just encryption), V.snoc (H.writeColumnEncryption options) Nothing] $ \wrongShape -> do
          rejected <- H.encodeParquetIO options {H.writeColumnEncryption = wrongShape} schema []
          check "empty dataset rejects an incomplete/oversized encryption vector" (isLeft rejected)
        noFooter <- H.encodeParquetIO options {H.writeFooterEncryption = Nothing} schema []
        check "empty dataset cannot silently ignore a requested encrypted column" (isLeft noFooter)
        badKey <- H.encodeParquetIO options {H.writeColumnEncryption = V.fromList [Just encryption {W.ceKey = "bad"}, Nothing]} schema []
        check "empty dataset validates requested column keys" (isLeft badKey)
        forM_ [0, 1] $ \group -> do
          chunk <- right (R.columnChunkSlice file group 0)
          values <- right (R.readInt32Levels Uncompressed 0 0 chunk)
          check "row-group AAD and page decoding" (R.clValues values == VP.generate 4 (\i -> fromIntegral (group * 10 + i + 1)))
          offset <- right (Index.readOffsetIndex file group 0)
          bounds <- right (Index.readColumnIndex file group 0)
          bloom <- right (Bloom.readBloomFilter file group 0)
          check "encrypted indexes and bloom filter are read" (maybe False (const True) offset && maybe False (const True) bounds && maybe False (const True) bloom)
        let metadata = R.pfFooter file
            column = rgColumns (fmRowGroups metadata V.! 0) V.! 0
        cm <- maybe (fail "missing metadata") pure (ccMetadata column)
        let damagedBytes = flipByte (fromIntegral (cmDataPageOffset cm)) bytes
        damaged <- right (R.loadParquetFileWithDecryption config damagedBytes)
        target <- right (A.parquetFileArrowSchemaChecked damaged)
        field <- maybe (fail "missing field") pure (V.find ((== "x") . AT.fieldName) (AT.arrowFields target))
        check "selected damaged page is rejected" (isLeft (A.readParquetColumnWithPagePruning damaged 0 0 field (Predicate.PGtEq (Predicate.PVInt32 0))))
        _ <- right (A.readParquetColumnWithPagePruning damaged 0 0 field (Predicate.PGtEq (Predicate.PVInt32 1000)))
        _ <- rightArrow (A.parquetRowGroupToArrow (publicSchema target) damaged 0)
        forM_ [(ccColumnIndexOffset column, ccColumnIndexLength column, \f -> Index.readColumnIndex f 0 0 >> Right ()), (ccOffsetIndexOffset column, ccOffsetIndexLength column, \f -> Index.readOffsetIndex f 0 0 >> Right ())] $ \(offset, size, readIndex) ->
          case (offset, size) of
            (Just start, Just count) -> do
              altered <- right (R.loadParquetFileWithDecryption config (flipByte (fromIntegral start + fromIntegral count - 1) bytes))
              check "altered encrypted index tag is rejected" (isLeft (readIndex altered))
            _ -> fail "missing encrypted index location"
        case (cmBloomFilterOffset cm, cmBloomFilterLength cm) of
          (Just start, Just size) -> do
            let raw = BS.drop (fromIntegral start) bytes
            (_, headerEnd) <- either (\message -> fail (message ++ "; bloom offset=" ++ show start ++ ", length=" ++ show size ++ ", prefix=" ++ show (BS.take 16 raw))) pure (E.readFramedModule raw 0)
            forM_ [fromIntegral start + headerEnd - 1, fromIntegral start + fromIntegral size - 1] $ \offset -> do
              altered <- right (R.loadParquetFileWithDecryption config (flipByte offset bytes))
              check "altered bloom header/bitset tag is rejected" (isLeft (Bloom.readBloomFilter altered 0 0))
          _ -> fail "missing encrypted bloom location"
        routed <- columnKeyMetadata footer bytes
        let explicit = D.defaultDecryptionConfig {D.dcFooterKey = Just key, D.dcColumnKeys = Map.singleton (V.singleton "x") key}
        resolved <- right (R.loadParquetFileWithDecryption explicit routed)
        restored <- right (Index.readColumnIndex resolved 0 0)
        check "column index is restored after decrypting column-specific metadata" (maybe False (const True) restored)
        _ <- right (R.columnChunkSlice resolved 1 0)
        missing <- right (R.loadParquetFileWithDecryption D.defaultDecryptionConfig {D.dcFooterKey = Just key} routed)
        let routes = R.encryptedColumnKeyMetadata missing
        check
          "unresolved column keys can be discovered from authenticated metadata"
          (V.length routes == 2 && V.all (\route -> D.ckmPath route == V.singleton "x" && D.ckmKeyMetadata route == Just "kf") routes)
        check "requested column requires its own resolved key" (isLeft (R.columnChunkSlice missing 0 0))
        missingSchema <- right (A.parquetFileArrowSchemaChecked missing)
        _ <- rightArrow (A.parquetRowGroupToArrow (publicSchema missingSchema) missing 0)
        let wrong = explicit {D.dcColumnKeys = Map.singleton (V.singleton "x") (BS.replicate width 0x43)}
        wrongFile <- right (R.loadParquetFileWithDecryption wrong routed)
        check "wrong column key reports an authentication failure" (isLeft (R.columnChunkSlice wrongFile 0 0))
        _ <- rightArrow (A.parquetRowGroupToArrow (publicSchema missingSchema) wrongFile 0)
        let ambiguous = H.defaultReadOptions {H.readDecryptionConfig = Just config, H.readFooterDecryption = Just (R.FooterDecryption key (W.feFileId footer) prefix)}
        check "ambiguous decryption options are rejected" (isLeft (H.decodeParquet ambiguous bytes))
  putStrLn "Encrypted column/key-routing/index/bloom/selective-read regressions passed (12 key/algorithm/page configurations, two row groups each)."


bloomBoundaries :: IO ()
bloomBoundaries = do
  let variant = V.singleton (1, TV.Struct V.empty)
      header size = TV.Struct (V.fromList [BloomFilterHeader_NumBytes size, BloomFilterHeader_Algorithm variant, BloomFilterHeader_Hash variant, BloomFilterHeader_Compression variant])
  forM_ [-1, 0, 1, 31, 33] $ \size ->
    check "invalid bloom sizes are rejected before constructing a filter" (isLeft (Bloom.decodeBloomFilter (encodeCompact (header size) <> BS.replicate 64 0)))
  let extended = case header 32 of
        TV.Struct fields -> TV.Struct (V.snoc fields (100, TV.Binary (BS.replicate 128 0xaa)))
        value -> value
  (_, filter_) <- right (Bloom.decodeBloomFilter (encodeCompact extended <> BS.replicate 32 0xff))
  check "unknown long bloom header fields preserve the bitset boundary" (Bloom.sbbfNumBytes filter_ == 32 && Bloom.sbbfCheck "value" filter_)


checksumRegression :: E.EncryptionAlgorithm -> W.PageVersion -> Int -> IO ()
checksumRegression algorithm version width = do
  _ <- right (Page.verifyPageChecksum 0xcbf43926 "123456789")
  let key = BS.replicate width 0x42
  footer <- W.newFooterEncryption key "tester" "kf"
  let inputSchema = V.fromList [(schema V.! 0) {seNumChildren = Just 1}, schema V.! 1]
      encryption = W.ColumnEncryption algorithm key (W.feFileId footer) "tester" "" 0
      options = H.defaultWriteOptions {H.writeCompression = Uncompressed, H.writeFooterEncryption = Just footer, H.writeColumnEncryption = V.singleton (Just encryption), H.writePageVersion = version, H.writePageIndex = False}
      config = D.defaultDecryptionConfig {D.dcFooterKey = Just key}
      aad kind = E.buildAad "tester" (E.buildAadSuffix (W.feFileId footer) kind 0 0 0)
  bytes <- H.encodeParquetIO options inputSchema [V.singleton (W.ColInt32 (VP.fromList [1, 2, 3, 4]))] >>= right
  trailer <- right (F.readFooterTrailer bytes)
  (_, cryptoEnd) <- right (decodeCompactFrom (F.ftBytes trailer) 0)
  (header, afterHeader) <- right (E.readFramedModule bytes 4)
  (_, afterBody) <- right (E.readFramedModule bytes afterHeader)
  let wireBody = BS.take (afterBody - afterHeader) (BS.drop afterHeader bytes)
  plainHeader <- right (E.decryptGcmModule key (aad E.ModuleDataPageHeader) header)
  (headerValue, _) <- right (decodeCompactFrom plainHeader 0)
  original <- right (E.authenticateFooter key Nothing Nothing bytes)
  forM_ [False, True] $ \incorrect -> do
    let checksum = crcReference wireBody `xor` if incorrect then 1 else 0
    updatedHeader <- case headerValue of
      TV.Struct fields -> pure (TV.Struct (V.snoc fields (PageHeader_Crc (fromIntegral checksum))))
      _ -> fail "expected encrypted page header"
    encryptedHeader <- E.encryptGcmModuleFramed key (aad E.ModuleDataPageHeader) (encodeCompact updatedHeader) >>= right
    let delta = fromIntegral (BS.length encryptedHeader - (afterHeader - 4))
        updatedFooter = resizeFooter delta (E.afValue original)
    encryptedFooter <- E.encryptGcmModuleFramed key (aad E.ModuleFooter) (encodeCompact updatedFooter) >>= right
    let changed = F.parquetEncryptedMagic <> encryptedHeader <> wireBody <> F.writeRawFooter F.parquetEncryptedMagic (BS.take cryptoEnd (F.ftBytes trailer) <> encryptedFooter)
    file <- right (R.loadParquetFileWithDecryption config changed)
    if incorrect
      then check "authenticated header with incorrect ciphertext CRC is rejected" (isLeft (R.columnChunkSlice file 0 0))
      else do
        chunk <- right (R.columnChunkSlice file 0 0)
        values <- right (R.readInt32Levels Uncompressed 0 0 chunk)
        check "CRC is checked on ciphertext before plaintext decoding" (R.clValues values == VP.fromList [1, 2, 3, 4])
  where
    resizeFooter delta = descend
      where
        descend (TV.Struct fields) = TV.Struct (V.map update fields)
        descend value = value
        update (FileMetadata_RowGroups values) = FileMetadata_RowGroups (V.map resizeGroup values)
        update field = field
        resizeGroup (TV.Struct fields) = TV.Struct (V.map resizeGroupField fields)
        resizeGroup value = value
        resizeGroupField (RowGroup_Columns values) = RowGroup_Columns (V.map resizeChunk values)
        resizeGroupField (RowGroup_TotalByteSize size) = RowGroup_TotalByteSize (size + delta)
        resizeGroupField field = field
        resizeChunk (TV.Struct fields) = TV.Struct (V.map resizeChunkField fields)
        resizeChunk value = value
        resizeChunkField (ColumnChunk_MetaData fields) = ColumnChunk_MetaData (V.map resizeColumn fields)
        resizeChunkField field = field
        resizeColumn (ColumnMetaData_TotalCompressedSize size) = ColumnMetaData_TotalCompressedSize (size + delta)
        resizeColumn (ColumnMetaData_TotalUncompressedSize size) = ColumnMetaData_TotalUncompressedSize (size + delta)
        resizeColumn field = field


-- Small, independent bitwise reference for regression fixtures, rather than
-- using the zlib implementation under test to produce expected checksums.
crcReference :: BS.ByteString -> Word32
crcReference = complement . BS.foldl' byte 0xffffffff
  where
    byte crc value = steps (8 :: Int) (crc `xor` fromIntegral value)
    steps 0 crc = crc
    steps n crc = steps (n - 1) ((crc `shiftR` 1) `xor` if crc .&. 1 /= 0 then 0xedb88320 else 0)


schema :: V.Vector SchemaElement
schema =
  V.fromList
    [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
    , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
    , SchemaElement "public" (Just Required) (Just PTByteArray) Nothing (Just CTUtf8) (Just LTString) Nothing Nothing
    ]


publicSchema :: AT.Schema -> AT.Schema
publicSchema schema = schema {AT.arrowFields = V.filter ((== "public") . AT.fieldName) (AT.arrowFields schema)}


columnKeyMetadata :: W.FooterEncryption -> BS.ByteString -> IO BS.ByteString
columnKeyMetadata footer bytes = do
  authenticated <- right (E.authenticateFooter key (Just prefix) Nothing bytes)
  value <- updateFile (E.afValue authenticated)
  trailer <- right (F.readFooterTrailer bytes)
  (_, cryptoEnd) <- right (decodeCompactFrom (F.ftBytes trailer) 0)
  encrypted <- E.encryptGcmModuleFramed key (aad E.ModuleFooter 0) (encodeCompact value) >>= right
  let before = BS.take (BS.length bytes - 8 - BS.length (F.ftBytes trailer)) bytes
  pure (before <> F.writeRawFooter F.parquetEncryptedMagic (BS.take cryptoEnd (F.ftBytes trailer) <> encrypted))
  where
    key = W.feKey footer
    prefix = W.feAadPrefix footer
    aad :: E.ModuleType -> Int -> BS.ByteString
    aad kind group = E.buildAad prefix (E.buildAadSuffix (W.feFileId footer) kind (fromIntegral group) 0 0)
    updateFile (TV.Struct fields) =
      TV.Struct
        <$> V.mapM
          ( \case
              FileMetadata_RowGroups groups -> FileMetadata_RowGroups <$> V.imapM updateGroup groups
              field -> pure field
          )
          fields
    updateFile _ = fail "expected file struct"
    updateGroup group (TV.Struct fields) =
      TV.Struct
        <$> V.mapM
          ( \case
              RowGroup_Columns columns -> RowGroup_Columns <$> V.imapM (updateColumn group) columns
              field -> pure field
          )
          fields
    updateGroup _ _ = fail "expected group struct"
    updateColumn group 0 (TV.Struct fields) = do
      plain <- maybe (fail "missing column metadata") pure (V.find (\case ColumnChunk_MetaData _ -> True; _ -> False) fields)
      value <- case plain of ColumnChunk_MetaData values -> pure (TV.Struct values); _ -> fail "missing column struct"
      ciphertext <- E.encryptGcmModuleFramed key (aad E.ModuleColumnMetaData group) (encodeCompact value) >>= right
      let removed = [fst (ColumnChunk_MetaData V.empty), fst (ColumnChunk_CryptoMetadata V.empty), fst (ColumnChunk_EncryptedColumnMetadata BS.empty)]
          retained = V.filter (\field -> fst field `notElem` removed) fields
          description = V.singleton (ColumnCryptoMetaData_EncryptionWithColumnKey (V.fromList [EncryptionWithColumnKey_PathInSchema (V.singleton (TV.String "x")), EncryptionWithColumnKey_KeyMetadata "kf"]))
      pure (TV.Struct (retained <> V.fromList [ColumnChunk_CryptoMetadata description, ColumnChunk_EncryptedColumnMetadata ciphertext]))
    updateColumn _ _ value = pure value


flipByte :: Int -> BS.ByteString -> BS.ByteString
flipByte offset bytes = BS.take offset bytes <> BS.singleton (BS.index bytes offset `xor` 1) <> BS.drop (offset + 1) bytes


check :: String -> Bool -> IO ()
check label success = unless success (fail ("EncryptedReadRegression: " ++ label))


right :: HasCallStack => Either String a -> IO a
right = either (\message -> fail (message ++ "\n" ++ prettyCallStack callStack)) pure


rightArrow :: Show e => Either e a -> IO a
rightArrow = either (fail . show) pure
