{-# LANGUAGE OverloadedStrings #-}

module VariantSchemaRegression (tests) where

import Arrow.Types qualified as A
import Control.Monad (forM_, unless)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Parquet.Nested qualified as N
import Parquet.Variant.Schema qualified as S


leaf :: A.ArrowType -> A.Field
leaf typ = A.defaultLeafField "typed_value" True typ


node :: Text -> A.Field
node name = A.defaultField name False A.AStruct (V.singleton (A.defaultLeafField "value" True A.ABinary))


root :: [A.Field] -> A.Field
root fields = (A.defaultField "v" True A.AStruct (V.fromList fields)) {A.fieldMetadata = S.variantExtensionMetadata}


metadata :: A.Field
metadata = A.defaultLeafField "metadata" False A.ABinary


value :: A.Field
value = A.defaultLeafField "value" True A.ABinary


load :: Either String a -> IO a
load = either fail pure


check :: String -> Bool -> IO ()
check name ok = unless ok (fail ("Variant schema regression: " ++ name))


tests :: IO ()
tests = do
  let primitives = [A.ABool, A.AInt 8 True, A.AInt 16 True, A.AInt 32 True, A.AInt 64 True, A.AFloatingPoint A.Single, A.AFloatingPoint A.DoublePrecision, A.ADate A.DateDay, A.ATime A.Microsecond 64, A.ATimestamp A.Microsecond Nothing, A.ATimestamp A.Nanosecond Nothing, A.ATimestamp A.Microsecond (Just "UTC"), A.ATimestamp A.Nanosecond (Just "UTC"), A.ABinary, A.ALargeBinary, A.ABinaryView, A.AUtf8, A.ALargeUtf8, A.AUtf8View]
      uuid = (leaf (A.AFixedSizeBinary 16)) {A.fieldMetadata = V.fromList [("ARROW:extension:name", "arrow.uuid"), ("ARROW:extension:metadata", "")]}
  forM_ (map leaf primitives ++ [uuid]) $ \typed -> do
    _ <- load (S.variantTypedLeaf typed)
    schema <- load (S.variantField "v" True (Just typed))
    check "primitive constructor emits canonical extension and fields" (S.isVariantField schema && isRight (S.validateVariantField schema))
    check "typed-only storage is valid" (isRight (S.validateVariantField (root [metadata, typed])))
  check "UUID maps to exact Parquet UUID physical type" (S.variantTypedLeaf uuid == Right N.LtUuid)
  forM_ [(A.ADecimal32, 1, 9), (A.ADecimal64, 10, 18), (A.ADecimal, 19, 38)] $ \(make, low, high) -> do
    forM_ [low .. high] $ \precision -> forM_ [0, precision] $ \scale ->
      check "every canonical decimal precision/scale boundary maps exactly" (S.variantTypedLeaf (leaf (make precision scale)) == Right (N.LtDecimal (fromIntegral precision) (fromIntegral scale)))
    forM_ [(low - 1, 0), (high + 1, 0), (low, -1), (low, low + 1), (maxBound, 0)] $ \(precision, scale) ->
      check "wrong carrier or scale rejected" (isLeft (S.variantTypedLeaf (leaf (make precision scale))))
  forM_ [A.ANull, A.AInt 8 False, A.AInt 64 False, A.AFloatingPoint A.Half, A.ADecimal256 76 0, A.ADecimal 9 0, A.ADate A.DateMillisecond, A.ATime A.Nanosecond 64, A.ATime A.Microsecond 32, A.ATimestamp A.Millisecond Nothing, A.ATimestamp A.Microsecond (Just "Europe/Amsterdam"), A.AFixedSizeBinary 16] $ \typ ->
    check "Arrow types outside canonical table rejected" (isLeft (S.variantField "v" True (Just (leaf typ))))
  let object = A.defaultField "typed_value" True A.AStruct (V.fromList [node "a", node "b"])
  _ <- load (S.variantField "v" True (Just object))
  forM_ [A.AList, A.ALargeList, A.AListView] $ \typ -> do
    let list = A.defaultField "typed_value" True typ (V.singleton (node "element"))
    _ <- load (S.variantField "v" False (Just list))
    let child = (node "element") {A.fieldNullable = True}
    check "array elements cannot be nullable wrappers" (isLeft (S.variantField "v" True (Just list {A.fieldChildren = V.singleton child})))
  let optionalObject = object {A.fieldChildren = V.singleton ((node "a") {A.fieldNullable = True})}
  check "object wrappers cannot be nullable" (isLeft (S.variantField "v" True (Just optionalObject)))
  check "duplicate object field rejected" (isLeft (S.variantField "v" True (Just object {A.fieldChildren = V.fromList [node "a", node "a"]})))
  let foreignExtension = V.singleton ("ARROW:extension:name", "arrow.parquet.file")
  check "typed object cannot masquerade as another extension" (isLeft (S.variantField "v" True (Just object {A.fieldMetadata = foreignExtension})))
  let huge = object {A.fieldChildren = V.generate 1000 (\i -> node (T.singleton (toEnum (65 + i))))}
  check "field limit precedes wide object name allocation" (isLeft (S.validateVariantFieldWithLimits (S.VariantSchemaLimits 64 4) (root [metadata, huge])))
  let ends = A.defaultLeafField "run_ends" False (A.AInt 16 True)
      entries = A.defaultLeafField "values" False A.ABinary
      ree = A.defaultField "metadata" False A.ARunEndEncoded (V.fromList [ends, entries])
  check "run-end encoded metadata accepted" (isRight (S.validateVariantField (root [value, ree])))
  check "invalid metadata run-end width rejected" (isLeft (S.validateVariantField (root [value, ree {A.fieldChildren = V.fromList [ends {A.fieldType = A.AInt 8 True}, entries]}])))
  forM_ [8, 16, 32, 64] $ \width -> forM_ [True, False] $ \signed -> do
    let dict = metadata {A.fieldDictionary = Just (A.DictionaryEncoding 42 (A.AInt width signed) False)}
    check "all legal dictionary index widths accepted for metadata" (isRight (S.validateVariantField (root [value, dict])))
  let wrongDict = metadata {A.fieldDictionary = Just (A.DictionaryEncoding 42 A.ABool False)}
  check "metadata dictionary indices must be integers" (isLeft (S.validateVariantField (root [wrongDict, value])))
  forM_ [root [value], root [metadata], root [metadata, value, value], root [metadata, value {A.fieldName = "Value"}], root [metadata {A.fieldNullable = True}, value], root [metadata {A.fieldType = A.AUtf8}, value], root [metadata, value {A.fieldDictionary = Just (A.DictionaryEncoding 42 (A.AInt 8 True) False)}]] $ \invalid ->
    check "malformed storage shape rejected" (isLeft (S.validateVariantField invalid))
  forM_ [V.empty, V.fromList [("ARROW:extension:name", "arrow.parquet.variant"), ("ARROW:extension:name", "arrow.parquet.variant")], V.fromList [("ARROW:extension:name", "arrow.parquet.variant"), ("ARROW:extension:metadata", "not empty")]] $ \extensions ->
    check "missing/duplicate/parameterized extension rejected" (isLeft (S.validateVariantField (root [metadata, value]) {A.fieldMetadata = extensions}))
  check "reordered and required unshredded fields accepted" (isRight (S.validateVariantField (root [value {A.fieldNullable = False}, metadata])))
  let nested = A.defaultField "typed_value" True A.AList (V.singleton (node "element"))
      schema = root [metadata, value, nested]
  check "schema depth limit enforced" (isLeft (S.validateVariantFieldWithLimits (S.VariantSchemaLimits 2 100) schema))
  check "schema field budget enforced" (isLeft (S.validateVariantFieldWithLimits (S.VariantSchemaLimits 64 2) schema))
  let dag 0 = node "x"
      dag n = let child = dag (n - 1); typed = A.defaultField "typed_value" True A.AStruct (V.fromList [child {A.fieldName = "left"}, child {A.fieldName = "right"}]) in A.defaultField "x" False A.AStruct (V.singleton typed)
      shared = root [metadata, (A.defaultField "typed_value" True A.AStruct (V.singleton (dag (20 :: Int))))]
  check "shared schema DAG cannot expand past its work budget" (isLeft (S.validateVariantFieldWithLimits (S.VariantSchemaLimits 64 100) shared))
  putStrLn "Canonical Variant schema primitive/nesting/metadata/resource regressions passed"
