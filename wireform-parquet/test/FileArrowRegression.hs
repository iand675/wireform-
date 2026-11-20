{-# LANGUAGE OverloadedStrings #-}

module FileArrowRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Vector qualified as V
import Parquet.Arrow qualified as Arrow
import Parquet.File qualified as F
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P


load :: Either String a -> IO a
load = either fail pure


child :: Text -> A.ArrowType -> Text -> A.Field
child name ty ident = A.Field name True ty V.empty Nothing (V.singleton ("PARQUET:field_id", ident))


fileField :: V.Vector A.Field -> A.Field
fileField children =
  A.Field
    "f"
    True
    A.AStruct
    children
    Nothing
    (V.fromList [("ARROW:extension:name", "arrow.parquet.file"), ("ARROW:extension:metadata", ""), ("PARQUET:field_id", "-7")])


tests :: IO ()
tests = do
  let check name condition = unless condition (fail ("Arrow FILE regression: " ++ name))
      uri = child "uri" A.AUtf8 "-2147483648"
      inline = child "inline" A.ABinary "2147483647"
      field = fileField (V.fromList [uri, inline])
  check "known extension accepted" (isRight (Arrow.arrowFieldToNestedSchema field))
  check "absent parameters accepted" (isRight (Arrow.arrowFieldToNestedSchema field {A.fieldMetadata = V.filter ((/= "ARROW:extension:metadata") . fst) (A.fieldMetadata field)}))
  forM_ ["{}", "v2", " "] $ \parameters ->
    check "unsupported extension metadata cannot disappear silently" (isLeft (Arrow.arrowFieldToNestedSchema field {A.fieldMetadata = V.map (\(key, value) -> (key, if key == "ARROW:extension:metadata" then parameters else value)) (A.fieldMetadata field)}))
  forM_ ["ARROW:extension:name", "ARROW:extension:metadata", "PARQUET:field_id"] $ \key -> do
    let pair = V.find ((== key) . fst) (A.fieldMetadata field)
    check "duplicate reserved metadata rejected" (isLeft (Arrow.arrowFieldToNestedSchema field {A.fieldMetadata = A.fieldMetadata field V.++ maybe V.empty V.singleton pair}))
  forM_ [uri {A.fieldNullable = False}, uri {A.fieldName = "URI"}, uri {A.fieldType = A.ABinary}, inline {A.fieldType = A.AUtf8}, child "size" (A.AInt 64 False) "0", child "offset" (A.AInt 32 True) "0"] $ \bad ->
    check "extension validates names/nullability/types" (isLeft (Arrow.arrowFieldToNestedSchema (fileField (V.singleton bad))))
  forM_ ["2147483648", "-2147483649", "1junk", "", " 1"] $ \ident ->
    check "field IDs are bounded strict integers" (isLeft (Arrow.arrowFieldToNestedSchema (fileField (V.singleton (uri {A.fieldMetadata = V.singleton ("PARQUET:field_id", ident)})))))
  check "duplicate names rejected" (isLeft (Arrow.arrowFieldToNestedSchema (fileField (V.fromList [uri, uri]))))
  check "FILE extension cannot use primitive storage" (isLeft (Arrow.arrowFieldToNestedSchema field {A.fieldType = A.AUtf8, A.fieldChildren = V.empty}))
  -- Each representation contains both short/inline and long/out-of-line view
  -- strings. Parent nulls must mask arbitrary child storage in every case.
  let texts = V.fromList [Just "../images/a%20b-long-relative.png", Just "masked", Nothing, Just ""]
      bytes = V.fromList [Nothing, Just "masked", Just "", Just (BS.replicate 80 255)]
      expected =
        V.fromList
          [ Just F.emptyFileReference {F.fileUri = Just "../images/a%20b-long-relative.png"}
          , Nothing
          , Just F.emptyFileReference {F.fileInline = Just ""}
          , Just F.emptyFileReference {F.fileUri = Just "", F.fileInline = Just (BS.replicate 80 255)}
          ]
      representations =
        [ (A.AUtf8, A.ABinary, C.fromMaybeTexts texts, C.fromMaybeByteStrings bytes)
        , (A.ALargeUtf8, A.ALargeBinary, C.fromMaybeLargeTexts texts, C.fromMaybeLargeByteStrings bytes)
        , (A.AUtf8View, A.ABinaryView, C.fromMaybeUtf8View texts, C.fromMaybeBinaryView bytes)
        ]
  forM_ representations $ \(textType, binaryType, textColumn, binaryColumn) -> do
    let input = fileField (V.fromList [uri {A.fieldType = textType}, inline {A.fieldType = binaryType}])
    column <- load (C.mkStruct 4 (C.validityFromBools (V.fromList [True, False, True, True])) (V.fromList [("uri", textColumn), ("inline", binaryColumn)]))
    nested <- load (Arrow.arrowFieldToNestedSchema input)
    rows <- load (Arrow.columnArrayToNestedRows input column)
    check "FILE rows use typed references" (rows == V.map (maybe N.NRNull N.NRFile) expected)
    encoded <- load (N.buildNestedFile (V.singleton ("f", nested)) (V.singleton rows))
    file <- load (Read.loadParquetFile encoded)
    let schema = P.fmSchema (Read.pfFooter file)
    check "FILE and IDs written to native footer" (P.seLogicalType (schema V.! 1) == Just P.LTFile && V.map P.seFieldId (V.drop 1 schema) == V.fromList [Just (-7), Just minBound, Just maxBound])
    output <- load (Arrow.parquetFileArrowSchemaChecked file)
    let outputField = V.head (A.arrowFields output)
    check "parent metadata has experimental FILE name and empty parameters" (A.fieldMetadata outputField == A.fieldMetadata field)
    check "child IDs survive schema inference" (V.map A.fieldMetadata (A.fieldChildren outputField) == V.map A.fieldMetadata (A.fieldChildren field))
    columns <- either (fail . show) pure (Arrow.parquetRowGroupToArrow output file 0)
    nativeSchema <- load (F.parseFileSchema (schema V.! 1) (V.drop 2 schema))
    reader <- load (F.compileFileReader nativeSchema (V.head columns))
    actual <- load (V.generateM 4 (F.fileReferenceAt reader))
    check "all storage representations retain semantic values" (actual == expected)
    restored <- load (Arrow.arrowFieldToNestedSchema outputField)
    check "FILE identity and parent/child IDs survive Arrow roundtrip" (restored == nested)
  let plain = field {A.fieldMetadata = V.empty}
  case Arrow.arrowFieldToNestedSchema plain of
    Right (N.NSOptional N.NSStruct {}) -> pure ()
    _ -> fail "Arrow FILE regression: plain struct must remain unannotated"
  putStrLn "Arrow FILE extension, IDs, strict metadata and standard/large/view storage regressions passed."
