{-# LANGUAGE OverloadedStrings #-}

module FileZeroRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Vector qualified as V
import Parquet.Arrow qualified as Arrow
import Parquet.File qualified as F
import Parquet.Footer qualified as Footer
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P


load :: Either String a -> IO a
load = either fail pure


encode :: N.NestedSchema -> V.Vector N.NestedRow -> Either String BS.ByteString
encode schema rows = N.buildNestedFile (V.singleton ("f", schema)) (V.singleton rows)


decode :: BS.ByteString -> Either String (V.Vector C.ColumnArray)
decode bytes = do
  file <- Read.loadParquetFile bytes
  schema <- Arrow.parquetFileArrowSchemaChecked file
  either (Left . show) Right (Arrow.parquetRowGroupToArrow schema file 0)


tests :: IO ()
tests = do
  let check name value = unless value (fail ("zero-leaf FILE: " ++ name))
      integer value = N.NRLeaf (N.LvInt32 value)
  empty <- load (F.mkFileSchema V.empty)
  let file = N.NSFile (F.withFileSchemaFieldId (Just 27) empty)
      optional = N.NSOptional file
  bytes <- load (encode optional (V.replicate 3 N.NRNull))
  parsed <- load (Read.loadParquetFile bytes)
  schema <- load (Arrow.parquetFileArrowSchemaChecked parsed)
  columns <- load (decode bytes)
  let column = V.head columns
      field = V.head (A.arrowFields schema)
  check "column identity/schema/IDs retained without inventing a leaf" (A.fieldName field == "f" && A.fieldType field == A.AStruct && V.null (A.fieldChildren field) && V.elem ("PARQUET:field_id", "27") (A.fieldMetadata field))
  check "metadata-only rows become null references" (C.columnLength column == 3 && C.nullCount column == 3)
  check "no dummy columns/pages" (V.null (P.rgColumns (V.head (P.fmRowGroups (Read.pfFooter parsed)))) && P.rgTotalByteSize (V.head (P.fmRowGroups (Read.pfFooter parsed))) == 0)
  typed <- load (F.compileFileReader empty column)
  actual <- load (V.generateM 3 (F.fileReferenceAt typed))
  check "typed reader retains all null rows" (actual == V.replicate 3 Nothing)
  restored <- load (Arrow.arrowFieldToNestedSchema field)
  rows <- load (Arrow.columnArrayToNestedRows field column)
  check "Arrow/native roundtrip keeps FILE ID and null rows" (restored == optional && rows == V.replicate 3 N.NRNull)
  forM_ [file, optional, N.NSList optional, N.NSOptional (N.NSList optional), N.NSOptional (N.NSStruct (V.singleton ("empty", optional))), N.NSMap file optional] $ \shape -> do
    zero <- load (encode shape V.empty)
    result <- load (decode zero)
    check "zero-row schemas retain a typed empty column" (V.length result == 1 && C.columnLength (V.head result) == 0)
  zero <- load (encode file V.empty)
  zeroFile <- load (Read.loadParquetFile zero)
  let footer = Read.pfFooter zeroFile
      legacySchema = V.imap (\index node -> if index == 1 then node {P.seRepetition = Just P.Repeated} else node) (P.fmSchema footer)
      legacy = zeroFile {Read.pfFooter = footer {P.fmSchema = legacySchema}}
  legacyArrow <- load (Arrow.parquetFileArrowSchemaChecked legacy)
  let legacyField = V.head (A.arrowFields legacyArrow)
  check "legacy repeated FILE annotates the element rather than list storage" (A.fieldType legacyField == A.AList && not (V.elem ("ARROW:extension:name", "arrow.parquet.file") (A.fieldMetadata legacyField)) && V.elem ("ARROW:extension:name", "arrow.parquet.file") (A.fieldMetadata (V.head (A.fieldChildren legacyField))))
  _ <- load (Arrow.arrowFieldToNestedSchema legacyField)
  check "non-null FILE cannot resolve without a source" (isLeft (encode file (V.singleton (N.NRFile F.emptyFileReference))))
  check "required FILE cannot be null" (isLeft (encode file (V.singleton N.NRNull)))
  forM_ [N.NSList optional, N.NSOptional (N.NSList optional)] $ \shape ->
    check "list boundaries without physical leaves are not silently lost" (isLeft (encode shape (V.singleton (N.NRList V.empty))))
  check "optional parent presence without physical leaves is not silently lost" (isLeft (encode (N.NSOptional (N.NSStruct (V.singleton ("empty", optional)))) (V.singleton N.NRNull)))
  -- A required parent has known presence; a sibling leaf also carries an
  -- optional parent's boundaries, including when only the FILE is projected.
  let requiredParent = N.NSStruct (V.singleton ("empty", optional))
      requiredRows = V.replicate 3 (N.NRStruct (V.singleton N.NRNull))
  requiredBytes <- load (encode requiredParent requiredRows)
  requiredColumn <- V.head <$> load (decode requiredBytes)
  case requiredColumn of
    C.ColStruct 3 Nothing children -> check "required zero-leaf parent has known presence" (C.nullCount (snd (V.head children)) == 3)
    _ -> fail "zero-leaf FILE: required parent was not retained"
  forM_ [False, True] $ \reverseFields -> do
    let fields = if reverseFields then V.fromList [("anchor", N.NSPrimitive N.LtInt32), ("empty", optional)] else V.fromList [("empty", optional), ("anchor", N.NSPrimitive N.LtInt32)]
        values n = if reverseFields then V.fromList [integer n, N.NRNull] else V.fromList [N.NRNull, integer n]
        shape = N.NSOptional (N.NSStruct fields)
    encoded <- load (encode shape (V.fromList [N.NRNull, N.NRStruct (values 10), N.NRStruct (values 20)]))
    decoded <- load (Read.loadParquetFile encoded)
    inferred <- load (Arrow.parquetFileArrowSchemaChecked decoded)
    let parent = V.head (A.arrowFields inferred)
        projected = parent {A.fieldChildren = V.filter ((== "empty") . A.fieldName) (A.fieldChildren parent)}
    output <- either (fail . show) pure (Arrow.parquetRowGroupToArrow inferred {A.arrowFields = V.singleton projected} decoded 0)
    case V.head output of
      C.ColStruct 3 validity children -> check "projection retains parent boundaries and empty FILE on either side of anchor" (not (C.isValidAt validity 0) && C.isValidAt validity 1 && C.nullCount (snd (V.head children)) == 3)
      _ -> fail "zero-leaf FILE: projected container shape changed"
  let element = N.NSStruct (V.fromList [("empty", optional), ("anchor", N.NSPrimitive N.LtInt32)])
      list = N.NSOptional (N.NSList element)
      item n = N.NRStruct (V.fromList [N.NRNull, integer n])
  listBytes <- load (encode list (V.fromList [N.NRNull, N.NRList V.empty, N.NRList (V.fromList [item 10, item 20, item 30, item 40, item 50])]))
  listColumn <- V.head <$> load (decode listBytes)
  case listColumn of
    C.ColList _ _ (C.ColStruct 5 _ children) -> check "nested FILE uses element count rather than top-level count" (C.nullCount (snd (V.head children)) == 5)
    _ -> fail "zero-leaf FILE: nested list counts changed"
  listFile <- load (Read.loadParquetFile listBytes)
  listSchema <- load (Arrow.parquetFileArrowSchemaChecked listFile)
  check "nested budget counts elements, not top-level rows" (isLeft (Arrow.parquetRowGroupToArrowWithLimits (Arrow.ReadLimits 4) listSchema listFile 0))
  let mapShape = N.NSOptional (N.NSMap (N.NSPrimitive N.LtInt32) optional)
  mapBytes <- load (encode mapShape (V.fromList [N.NRNull, N.NRMapEntries V.empty, N.NRMapEntries (V.fromList [(integer 1, N.NRNull), (integer 2, N.NRNull)])]))
  mapColumn <- V.head <$> load (decode mapBytes)
  case mapColumn of
    C.ColMap _ _ keys values -> check "map value uses entry count" (C.columnLength keys == 2 && C.columnLength values == 2 && C.nullCount values == 2)
    _ -> fail "zero-leaf FILE: map values changed"
  let emptyKeyMap = N.NSOptional (N.NSMap file (N.NSOptional (N.NSPrimitive N.LtInt32)))
  keyBytes <- load (encode emptyKeyMap (V.fromList [N.NRNull, N.NRMapEntries V.empty, N.NRMapEntries V.empty]))
  keyColumn <- V.head <$> load (decode keyBytes)
  case keyColumn of
    C.ColMap _ _ keys _ -> check "empty map with zero-leaf key has no null key values" (C.columnLength keys == 0 && C.nullCount keys == 0)
    _ -> fail "zero-leaf FILE: empty map key changed"
  let readLimit limit = Arrow.parquetRowGroupToArrowWithLimits (Arrow.ReadLimits limit) schema parsed 0
  check "caller can select unbacked-value budget" (isLeft (readLimit 2) && either (const False) (const True) (readLimit 3) && isLeft (readLimit (-1)))
  both <- load (N.buildNestedFile (V.fromList [("left", optional), ("right", optional)]) (V.replicate 2 (V.replicate 3 N.NRNull)))
  bothFile <- load (Read.loadParquetFile both)
  bothSchema <- load (Arrow.parquetFileArrowSchemaChecked bothFile)
  check "budget is aggregate across output FILE columns" (isLeft (Arrow.parquetRowGroupToArrowWithLimits (Arrow.ReadLimits 5) bothSchema bothFile 0))
  _ <- either (fail . show) pure (Arrow.parquetRowGroupToArrowWithLimits (Arrow.ReadLimits 6) bothSchema bothFile 0)
  let inflate count =
        let footer = Read.pfFooter parsed
            changed = footer {P.fmNumRows = count, P.fmRowGroups = V.map (\group -> group {P.rgNumRows = count}) (P.fmRowGroups footer)}
        in "PAR1" <> Footer.writeFooter changed
  forM_ [1048577, maxBound :: Int64] $ \claimed ->
    check "tiny footers cannot trigger unbounded allocation or Int overflow" (isLeft (decode (inflate claimed)))
  raisedFile <- load (Read.loadParquetFile (inflate 1048577))
  raised <- either (fail . show) pure (Arrow.parquetRowGroupToArrowWithLimits (Arrow.ReadLimits 1048577) schema raisedFile 0)
  check "explicit budget override supports larger valid null columns" (C.columnLength (V.head raised) == 1048577 && C.nullCount (V.head raised) == 1048577)
  forM_ ["root-0-optional", "root-0-required", "root-3-optional", "root-3-required", "with-anchor"] $ \name -> do
    fixtureBytes <- BS.readFile ("test/fixtures/file-zero-" <> name <> ".parquet")
    decoded <- load (Read.loadParquetFile fixtureBytes)
    inferred <- load (Arrow.parquetFileArrowSchemaChecked decoded)
    let expectedRows = if name == "root-0-optional" || name == "root-0-required" then 0 else 3
    check "independent fixture retains the FILE field even without row groups" (A.fieldName (V.head (A.arrowFields inferred)) == "f")
    if V.null (P.fmRowGroups (Read.pfFooter decoded))
      then check "absent row groups represent an empty file" (expectedRows == 0 && P.fmNumRows (Read.pfFooter decoded) == 0)
      else do
        values <- load (decode fixtureBytes)
        check "independent Thrift/PyArrow fixture retains zero-leaf FILE column" (C.columnLength (V.head values) == expectedRows && C.nullCount (V.head values) == expectedRows)
  putStrLn "Zero-leaf FILE identity, representable nulls/containers, projection and aggregate resource budgets passed."
