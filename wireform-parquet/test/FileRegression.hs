{-# LANGUAGE OverloadedStrings #-}

module FileRegression (tests) where

import Arrow.Column qualified as A
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Arrow qualified as Arrow
import Parquet.File
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P


load :: Either String a -> IO a
load = either fail pure


fullSchema :: Either String FileSchema
fullSchema = mkFileSchema (V.fromList (map (\field -> FileFieldSpec field (Just (fromIntegral (fromEnum field + 10)))) [minBound .. maxBound]))


readReferences :: BS.ByteString -> Either String (V.Vector (Maybe FileReference))
readReferences bytes = do
  file <- Read.loadParquetFile bytes
  let elements = P.fmSchema (Read.pfFooter file)
  schema <- parseFileSchema (elements V.! 1) (V.drop 2 elements)
  arrow <- Arrow.parquetFileArrowSchemaChecked file
  columns <- either (Left . show) Right (Arrow.parquetRowGroupToArrow arrow file 0)
  reader <- compileFileReader schema (V.head columns)
  V.generateM (A.columnLength (V.head columns)) (fileReferenceAt reader)


tests :: IO ()
tests = do
  let check name condition = unless condition (fail ("FILE regression: " ++ name))
      inline = emptyFileReference {fileInline = Just ""}
      external = emptyFileReference {fileUri = Just "../images/a%20b.png?raw=1#part"}
      both = external {fileOffset = Just 0, fileSize = Just 0, fileInline = Just "whole inline payload"}
      maximumRange = external {fileOffset = Just maxBound, fileSize = Just maxBound}
  schema <- load fullSchema
  forM_ [inline, external, both, maximumRange, inline {fileUri = Just "", fileContentType = Just "", fileChecksum = Just ""}] $ \reference ->
    check "valid reference" (isRight (resolveFileReference reference))
  forM_ [emptyFileReference, external {fileUri = Just ""}, external {fileOffset = Just 0}, external {fileOffset = Just (-1), fileSize = Just 1}, external {fileSize = Just (-1)}, inline {fileOffset = Just 0, fileSize = Just 0}, inline {fileSize = Just 1}] $ \reference ->
    check "invalid locator is not hidden by inline bytes" (isLeft (resolveFileReference reference))
  resolution <- load (resolveFileReference both)
  check "inline is not sliced by offset/size" (filePreferredSource resolution == InlineBytes "whole inline payload")
  check "external alternative retained" (fmap fileLocationSize (fileExternalLocation resolution) == Just (Just 0))
  check "default content type" (fileMediaType resolution == "application/octet-stream")
  maximumResolution <- load (resolveFileReference maximumRange)
  check "exclusive end does not overflow signed 64 bits" ((fileExternalLocation maximumResolution >>= fileLocationEnd) == Just 18446744073709551614)
  whole <- load (resolveFileReference external)
  check "whole-file URI is preserved verbatim" (fmap fileLocationUri (fileExternalLocation whole) == fileUri external)
  check "whole-file size remains unknown" ((fileExternalLocation whole >>= fileLocationEnd) == Nothing)
  forM_ [("MD5", 32, FileMD5), ("CRC32", 8, FileCRC32), ("CRC32C", 8, FileCRC32C), ("SHA-256", 64, FileSHA256)] $ \(name, width, kind) -> do
    let digest = T.replicate width "a"
    check "recognized digest" (parseFileChecksum (Just (name <> ":" <> digest)) == Right (Just (FileDigest kind digest)))
    forM_ [T.replicate width "A", T.drop 1 digest, digest <> "0", T.replicate width "g"] $ \bad ->
      check "malformed known digest" (isLeft (parseFileChecksum (Just (name <> ":" <> bad))))
  check "opaque eTag includes colons verbatim" (parseFileChecksum (Just "ETAG:\"opaque:token-2\"") == Right (Just (FileETag "\"opaque:token-2\"")))
  check "unknown algorithm ignored" (parseFileChecksum (Just "future:any opaque data") == Right Nothing)
  check "missing checksum separator rejected" (isLeft (parseFileChecksum (Just "SHA-256")))
  inlineSchema <- load (mkFileSchema (V.singleton (FileFieldSpec FileInline Nothing)))
  check "schema cannot silently drop even empty strings" (isLeft (validateFileReferenceForSchema inlineSchema inline {fileUri = Just ""}))
  check "duplicate fields rejected" (isLeft (mkFileSchema (V.fromList [FileFieldSpec FileUri Nothing, FileFieldSpec FileUri (Just 1)])))
  let group = P.SchemaElement "f" (Just P.Optional) Nothing (Just 6) Nothing (Just P.LTFile) Nothing Nothing
      elements = fileSchemaElements schema
  check "schema IDs/order retained" (parseFileSchema group elements == Right schema)
  forM_ [P.Required, P.Repeated] $ \repetition ->
    check "FILE children must be optional" (isLeft (parseFileSchema group (V.imap (\i f -> if i == 0 then f {P.seRepetition = Just repetition} else f) elements)))
  forM_ ["URI", "unknown", "contentType"] $ \name ->
    check "case-sensitive exact names" (isLeft (parseFileSchema group (elements V.// [(0, (elements V.! 0) {P.seName = name})])))
  forM_ [P.LTString, P.LTTimestamp True P.LtNanos, P.LTInteger 64 False] $ \logical ->
    check "offset cannot borrow unrelated INT64 semantics" (isLeft (parseFileSchema group (elements V.// [(1, (elements V.! 1) {P.seLogicalType = Just logical})])))
  check "writer rejects locally provable inline/locator disagreement" (isLeft (validateFileReferenceForSchema schema both))
  let matching = both {fileSize = Just (fromIntegral (BS.length "whole inline payload"))}
      rows = V.fromList [Nothing, Just inline, Just external, Just matching, Just maximumRange, Just inline {fileUri = Just "", fileContentType = Just "", fileChecksum = Just ""}]
      nested = N.NSOptional (N.NSFile schema)
  bytes <- load (N.buildNestedFile (V.singleton ("f", nested)) (V.singleton (V.map (maybe N.NRNull N.NRFile) rows)))
  actual <- load (readReferences bytes)
  check "native file/Arrow/typed reader roundtrip" (actual == rows)
  forM_ [emptyFileReference, external {fileOffset = Just 0}] $ \invalid ->
    check "writer rejects invalid reference" (isLeft (N.buildNestedFile (V.singleton ("f", nested)) (V.singleton (V.singleton (N.NRFile invalid)))))
  check "required FILE cannot be null" (isLeft (N.shred (N.NSFile schema) (V.singleton N.NRNull)))
  -- All source-presence combinations, with zero specifically distinguished
  -- from absence, against an independent truth table.
  forM_ [Nothing, Just "", Just "relative"] $ \uri ->
    forM_ [Nothing, Just 0, Just 17, Just (-1)] $ \offset ->
      forM_ [Nothing, Just 0, Just 23, Just (-1)] $ \size ->
        forM_ [Nothing, Just "", Just "bytes"] $ \payload -> do
          let reference = FileReference uri offset size Nothing Nothing payload
              hasUri = maybe False (not . T.null) uri
              expected =
                (hasUri || isJust payload)
                  && maybe True (>= 0) offset
                  && maybe True (>= 0) size
                  && (not (isJust offset) || (hasUri && isJust size))
                  && (not (isJust size) || hasUri)
          check "144-case independent resolution table" (isRight (validateFileReference reference) == expected)
  passed <- H.check $ H.withTests 100 $ H.property $ do
    fields <- H.forAll (Gen.shuffle [minBound .. maxBound])
    count <- H.forAll (Gen.int (Range.linear 0 6))
    let selected = take count fields
    subset <- H.evalEither (mkFileSchema (V.fromList (map (`FileFieldSpec` Nothing) selected)))
    let children = fileSchemaElements subset
    reparsed <- H.evalEither (parseFileSchema group {P.seNumChildren = Just (fromIntegral count)} children)
    reparsed H.=== subset
  check "field permutations/subsets preserve schema" passed
  roundtrips <- H.check $ H.withTests 100 $ H.property $ do
    input <- H.forAll (Gen.list (Range.linear 1 30) (Gen.maybe referenceGen))
    encoded <- H.evalEither (N.buildNestedFile (V.singleton ("f", nested)) (V.singleton (V.fromList (map (maybe N.NRNull N.NRFile) input))))
    decoded <- H.evalEither (readReferences encoded)
    decoded H.=== V.fromList input
  check "arbitrary binary payloads, signed limits and null rows roundtrip" roundtrips
  opaque <- load (A.mkBinary Nothing (VS.fromList [0, 0, 1]) "x")
  -- A null parent wins over the deliberately invalid source stored under it.
  column <- load (A.mkStruct 2 (A.validityFromBools (V.fromList [False, True])) (V.singleton ("inline", opaque)))
  reader <- load (compileFileReader inlineSchema column)
  check "null parent masks child" (fileReferenceAt reader 0 == Right Nothing)
  check "reader retains non-null bytes" (fileReferenceAt reader 1 == Right (Just inline {fileInline = Just "x"}))
  check "reader index checked" (isLeft (fileReferenceAt reader (-1)) && isLeft (fileReferenceAt reader 2))
  largeText <- load (A.mkLargeUtf8 Nothing (VS.fromList [0, 1, 2]) "ab")
  largeBytes <- load (A.mkLargeBinary Nothing (VS.fromList [0, 0, 2]) (BS.pack [0, 255]))
  largeSchema <- load (mkFileSchema (V.fromList [FileFieldSpec FileUri Nothing, FileFieldSpec FileInline Nothing]))
  largeColumn <- load (A.mkStruct 2 Nothing (V.fromList [("inline", largeBytes), ("uri", largeText)]))
  largeReader <- load (compileFileReader largeSchema largeColumn)
  check "large columns use named cached views" (fileReferenceAt largeReader 1 == Right (Just emptyFileReference {fileUri = Just "b", fileInline = Just (BS.pack [0, 255])}))
  duplicateColumn <- load (A.mkStruct 2 Nothing (V.fromList [("inline", opaque), ("inline", opaque)]))
  check "duplicate column names cannot replace a missing field" (isLeft (compileFileReader largeSchema duplicateColumn))
  textStorage <- load (A.mkStruct 2 Nothing (V.singleton ("inline", largeText)))
  check "inline must be binary rather than STRING" (isLeft (compileFileReader inlineSchema textStorage))
  putStrLn "FILE schemas, resolution table, checksums and native roundtrip regression cases passed."


referenceGen :: H.Gen FileReference
referenceGen = Gen.choice [inlineGen, externalGen]
  where
    uriGen = Gen.element ["s3://bucket/a%20b", "../images/a%20b.png?raw=1#part", "file.parquet", "https://example.test/file"]
    position = Gen.int64 (Range.constant 0 maxBound)
    externalGen = do
      uri <- uriGen
      offset <- Gen.maybe position
      size <- case offset of
        Nothing -> Gen.maybe position
        Just _ -> Just <$> position
      pure emptyFileReference {fileUri = Just uri, fileOffset = offset, fileSize = size}
    inlineGen = do
      bytes <- Gen.bytes (Range.linear 0 100)
      uri <- Gen.choice [pure Nothing, pure (Just ""), Just <$> uriGen]
      offset <- if maybe False (not . T.null) uri then Gen.maybe position else pure Nothing
      size <- case offset of
        Just _ -> pure (Just (fromIntegral (BS.length bytes)))
        Nothing -> pure Nothing
      content <- Gen.element [Nothing, Just "", Just "application/octet-stream"]
      checksum <- Gen.element [Nothing, Just "", Just "future:opaque:token"]
      pure (FileReference uri offset size content checksum (Just bytes))
