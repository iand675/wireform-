{-# LANGUAGE PatternSynonyms #-}

module EncryptedWriterRegression (tests) where

import Arrow.Types qualified as AT
import Control.Monad (forM_, unless)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Parquet.Arrow qualified as A
import Parquet.BloomFilter qualified as B
import Parquet.Compress qualified as C
import Parquet.Decryption qualified as D
import Parquet.Encryption qualified as E
import Parquet.HighLevel qualified as H
import Parquet.Nested qualified as N
import Parquet.PageIndex qualified as I
import Parquet.Read qualified as R
import Parquet.Types
import Parquet.Write qualified as W
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))


tests :: IO ()
tests = do
  output <- lookupEnv "WIREFORM_PARQUET_PRIMITIVE_OUTPUT"
  forM_ output $ \root -> createDirectoryIfMissing True (root </> "encryption-writer")
  forM_ [Uncompressed, GZip, Snappy, ZSTD, LZ4Raw, LZO, Brotli] $ \codec -> case C.validateCompression codec of
    Left _ -> pure ()
    Right () -> forM_ [W.PageV1, W.PageV2] $ \version -> do
      let base = H.defaultWriteOptions {H.writeCompression = codec, H.writePageVersion = version, H.writeBloomFilters = ["secret", "record", "mapping"]}
      nestedPlain <- right (H.encodeParquetNestedWith base nestedSchema nestedRows)
      let mixedBase = base {H.writeBloomFilters = ["secret"]}
          mixedPlain = H.encodeParquetMixed mixedBase mixedSchema mixedRows
      save output ("plain-nested-" ++ show codec ++ "-" ++ show version) nestedPlain
      save output ("plain-mixed-" ++ show codec ++ "-" ++ show version) mixedPlain
      forM_ [16, 24, 32] $ \width -> forM_ [E.AesGcmV1, E.AesGcmCtrV1] $ \algorithm ->
        forM_ [H.EncryptedFooter, H.SignedPlaintextFooter] $ \mode -> forM_ [False, True] $ \split -> do
          footer <- W.newFooterEncryption (BS.replicate width 0x42) "writer-regression" "kf"
          let encryption ordinal =
                W.ColumnEncryption
                  algorithm
                  (BS.replicate width (if split then if even ordinal then 0x43 else 0x44 else 0x42))
                  (W.feFileId footer)
                  (W.feAadPrefix footer)
                  (if even ordinal then "kc1" else "kc2")
                  ordinal
              configs count = V.generate count (\col -> if col == 0 then Nothing else Just (encryption col))
              nestedOptions = base {H.writeFooterMode = mode, H.writeFooterEncryption = Just footer, H.writeColumnEncryption = configs 6}
              mixedOptions = mixedBase {H.writeFooterMode = mode, H.writeFooterEncryption = Just footer, H.writeColumnEncryption = configs 3}
              suffix = "-" ++ show width ++ "-" ++ show algorithm ++ "-" ++ show mode ++ "-" ++ (if split then "split" else "footer") ++ "-" ++ show codec ++ "-" ++ show version
              config = D.defaultDecryptionConfig {D.dcKeyMetadataKeys = Map.fromList [("kf", BS.replicate width 0x42), ("kc1", BS.replicate width 0x43), ("kc2", BS.replicate width 0x44)], D.dcAadPrefix = Just "writer-regression"}
          nested <- H.encodeParquetNestedIO nestedOptions nestedSchema nestedRows >>= right
          mixed <- H.encodeParquetMixedIO mixedOptions mixedSchema mixedRows >>= right
          verify config nestedPlain nested
          verify config mixedPlain mixed
          save output ("feature-nested" ++ suffix) nested
          save output ("feature-mixed" ++ suffix) mixed
          forM_ [nested, mixed] $ \bytes -> do
            let damaged = BS.take (BS.length bytes - 9) bytes <> BS.singleton (BS.index bytes (BS.length bytes - 9) `xor` 1) <> BS.drop (BS.length bytes - 8) bytes
            check "altered footer/signature is rejected" (isLeft (R.loadParquetFileWithDecryption config damaged))
            file <- right (R.loadParquetFileWithDecryption config bytes)
            let withoutSecrets = config {D.dcKeyMetadataKeys = Map.singleton "kf" (BS.replicate width 0x42)}
            projection <- right (R.loadParquetFileWithDecryption withoutSecrets bytes)
            schema <- right (A.parquetFileArrowSchemaChecked projection)
            let public = schema {AT.arrowFields = V.filter ((== "public") . AT.fieldName) (AT.arrowFields schema)}
            _ <- either (fail . show) pure (A.parquetRowGroupToArrow public projection 0)
            whenSplit split $ do
              check "missing separate key fails selected secret" (isLeft (R.columnChunkSlice projection 0 1))
              let wrong = config {D.dcKeyMetadataKeys = Map.insert "kc2" (BS.replicate width 0) (D.dcKeyMetadataKeys config)}
              bad <- right (R.loadParquetFileWithDecryption wrong bytes)
              check "wrong separate key fails authentication" (isLeft (R.columnChunkSlice bad 0 1))
            check "requested encryption is present" (R.isColumnEncrypted file 0 1 && not (R.isColumnEncrypted file 0 0))
  invalidRequests
  putStrLn "Encrypted writer feature regressions passed: nested/nullable, separate keys and signed plaintext footers, all enabled codecs and key widths."


verify :: D.DecryptionConfig -> BS.ByteString -> BS.ByteString -> IO ()
verify config plaintext bytes = do
  expected <- right (R.loadParquetFile plaintext)
  file <- right (R.loadParquetFileWithDecryption config bytes)
  expectedSchema <- right (A.parquetFileArrowSchemaChecked expected)
  actualSchema <- right (A.parquetFileArrowSchemaChecked file)
  check "schema survives encryption" (actualSchema == expectedSchema)
  forM_ [0 .. V.length (fmRowGroups (R.pfFooter expected)) - 1] $ \rg -> do
    decoded <- either (fail . show) pure (A.parquetRowGroupToArrow actualSchema file rg)
    original <- either (fail . show) pure (A.parquetRowGroupToArrow expectedSchema expected rg)
    V.imapM_
      ( \col field -> do
          actual <- right (A.columnArrayToNestedRows field (decoded V.! col))
          wanted <- right (A.columnArrayToNestedRows field (original V.! col))
          check "nullable/nested values match plaintext control" (actual == wanted)
      )
      (AT.arrowFields actualSchema)
    V.imapM_
      ( \col _ -> do
          actualOffsets <- right (I.readOffsetIndex file rg col)
          wantedOffsets <- right (I.readOffsetIndex expected rg col)
          check "offset index remains present" (fmap (V.length . oiPageLocations) actualOffsets == fmap (V.length . oiPageLocations) wantedOffsets)
          actualIndex <- right (I.readColumnIndex file rg col)
          wantedIndex <- right (I.readColumnIndex expected rg col)
          check "column index retains every bound/count" (actualIndex == wantedIndex)
          actualBloom <- right (B.readBloomFilter file rg col)
          wantedBloom <- right (B.readBloomFilter expected rg col)
          check "bloom bitsets retain every inserted value" (fmap B.encodeBloomFilter actualBloom == fmap B.encodeBloomFilter wantedBloom)
      )
      (rgColumns (fmRowGroups (R.pfFooter expected) V.! rg))


nestedSchema :: V.Vector (Text, N.NestedSchema)
nestedSchema =
  V.fromList
    [ ("public", N.NSRequired (N.NSPrimitive N.LtInt32))
    , ("secret", N.NSOptional (N.NSList (N.NSOptional (N.NSPrimitive N.LtInt32))))
    , ("record", N.NSOptional (N.NSStruct (V.fromList [("a", N.NSOptional (N.NSPrimitive N.LtString)), ("b", N.NSRequired (N.NSPrimitive N.LtInt64))])))
    , ("mapping", N.NSOptional (N.NSMap (N.NSRequired (N.NSPrimitive N.LtString)) (N.NSOptional (N.NSPrimitive N.LtInt64))))
    ]


nestedRows :: V.Vector (V.Vector N.NestedRow)
nestedRows =
  V.fromList
    [ V.fromList (map (N.NRLeaf . N.LvInt32) [1, 2, 3])
    , V.fromList [N.NRNull, N.NRList V.empty, N.NRList (V.fromList [N.NRNull, N.NRLeaf (N.LvInt32 (-7)), N.NRLeaf (N.LvInt32 42)])]
    , V.fromList [N.NRNull, N.NRStruct (V.fromList [N.NRNull, N.NRLeaf (N.LvInt64 11)]), N.NRStruct (V.fromList [N.NRLeaf (N.LvString "hi"), N.NRLeaf (N.LvInt64 12)])]
    , V.fromList [N.NRNull, N.NRMapEntries V.empty, N.NRMapEntries (V.fromList [(N.NRLeaf (N.LvString "k"), N.NRNull), (N.NRLeaf (N.LvString "z"), N.NRLeaf (N.LvInt64 99))])]
    ]


mixedSchema :: V.Vector SchemaElement
mixedSchema =
  V.fromList
    [ SchemaElement "schema" Nothing Nothing (Just 3) Nothing Nothing Nothing Nothing
    , SchemaElement "public" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
    , SchemaElement "secret" (Just Optional) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
    , SchemaElement "text" (Just Optional) (Just PTByteArray) Nothing Nothing (Just LTString) Nothing Nothing
    ]


mixedRows :: [V.Vector W.ParquetColumn]
mixedRows = map group [0, 10]
  where
    group base = V.fromList [W.PCRequired (W.ColInt32 (VP.fromList [base + 1, base + 2, base + 3])), W.PCOptional (W.OptInt32 (V.fromList [Nothing, Just (-7), Just 42])), W.PCOptional (W.OptByteArray (V.fromList [Just "", Nothing, Just "private"]))]


invalidRequests :: IO ()
invalidRequests = do
  let signed = H.defaultWriteOptions {H.writeFooterMode = H.SignedPlaintextFooter, H.writeCompression = Uncompressed}
  absent <- H.encodeParquetNestedIO signed nestedSchema nestedRows
  check "signed plaintext mode requires a signing key" (isLeft absent)
  footer <- W.newFooterEncryption (BS.replicate 16 0x42) "" "kf"
  let wrong = signed {H.writeFooterEncryption = Just footer {W.feKey = "bad"}}
  bad <- H.encodeParquetMixedIO wrong mixedSchema []
  check "empty mixed writer validates footer key" (isLeft bad)
  invalid <- H.encodeParquetNestedIO signed {H.writeFooterEncryption = Just footer, H.writeColumnEncryption = V.singleton Nothing} nestedSchema nestedRows
  check "nested writer rejects truncated encryption vectors" (isLeft invalid)


save :: Maybe FilePath -> String -> BS.ByteString -> IO ()
save output name bytes = forM_ output $ \root -> BS.writeFile (root </> "encryption-writer" </> (name ++ ".parquet")) bytes


whenSplit :: Bool -> IO () -> IO ()
whenSplit True action = action
whenSplit False _ = pure ()


check :: String -> Bool -> IO ()
check message valid = unless valid (fail ("EncryptedWriterRegression: " ++ message))


right :: Either String a -> IO a
right = either fail pure
