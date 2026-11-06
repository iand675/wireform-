{-# LANGUAGE OverloadedStrings #-}

module VariantWriteRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import GHC.Conc (getAllocationCounter)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Arrow qualified as Arrow
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import Parquet.Variant qualified as B
import Parquet.Variant.Arrow qualified as VariantArrow
import Parquet.Variant.Parquet qualified as VP
import Parquet.Variant.Schema qualified as Schema
import Parquet.Variant.Shred qualified as Shred
import VariantRegression qualified as Cases


load :: Either String a -> IO a
load = either fail pure


file :: Schema.VariantStorageSchema -> V.Vector N.NestedRow -> Either String Read.ParquetFile
file storage rows = N.buildNestedFile (V.singleton ("v", N.NSOptional (N.NSVariantTyped storage))) (V.singleton rows) >>= Read.loadParquetFile


leaf :: BS.ByteString -> N.NestedRow
leaf = N.NRLeaf . N.LvBinary


tests :: IO ()
tests = do
  let check name good = unless good (fail ("typed Variant writer: " ++ name))
      metadata = leaf (BS.pack [1, 0, 0])
      int32 = A.defaultLeafField "typed_value" True (A.AInt 32 True)
  -- Canonical fields are accessed by name, including when the column's
  -- named children are in a different order than its declared Field.
  plain <- load (Schema.variantField "v" True Nothing)
  named <- load (C.mkStruct 1 Nothing (V.fromList [("value", C.fromByteStrings (V.singleton (BS.singleton 0))), ("metadata", C.fromByteStrings (V.singleton (BS.pack [1, 0, 0])))]))
  namedSchema <- load (Arrow.arrowFieldToNestedSchema plain)
  namedRows <- load (Arrow.columnArrayToNestedRows plain named)
  namedBytes <- load (N.buildNestedFile (V.singleton ("v", namedSchema)) (V.singleton namedRows))
  namedFile <- load (Read.loadParquetFile namedBytes)
  namedValues <- load (VP.readVariantColumn namedFile 0 "v")
  check "permuted metadata/value buffers bind by name before row lowering" (namedValues == V.singleton (Just B.VNull))
  duplicate <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", C.fromByteStrings (V.singleton (BS.pack [1, 0, 0]))), ("metadata", C.fromByteStrings (V.singleton (BS.singleton 0)))]))
  check "duplicate named Variant children rejected before binding" (isLeft (Arrow.columnArrayToNestedRows plain duplicate) && isLeft (VariantArrow.compileVariantArrowReader plain duplicate))
  let elementFields = V.fromList [A.defaultLeafField "value" True A.ABinary, int32]
      elementField = A.defaultField "element" False A.AStruct elementFields
      listField = A.defaultField "typed_value" True A.AList (V.singleton elementField)
      namedField = (A.defaultField "v" True A.AStruct (V.fromList [A.defaultLeafField "metadata" False A.ABinary, listField])) {A.fieldMetadata = Schema.variantExtensionMetadata}
  elementColumns <- load (C.mkStruct 2 Nothing (V.fromList [("typed_value", C.fromMaybes C.PInt32 (V.fromList [Just 3, Nothing])), ("value", C.fromMaybeByteStrings (V.fromList [Nothing, Just (BS.singleton 0)]))]))
  listColumn <- load (C.mkList Nothing (S.fromList [0, 2 :: Int32]) elementColumns)
  nestedNames <- load (C.mkStruct 1 Nothing (V.fromList [("typed_value", listColumn), ("metadata", C.fromByteStrings (V.singleton (BS.pack [1, 0, 0])))]))
  directReader <- load (VariantArrow.compileVariantArrowReader namedField nestedNames)
  directValue <- load (VariantArrow.variantArrowAt directReader 0)
  let expectedNames = Just (B.VArray (V.fromList [B.VInt32 3, B.VNull]))
  check "reader aligns different child types recursively before shape validation" (directValue == expectedNames)
  nestedSchema <- load (Arrow.arrowFieldToNestedSchema namedField)
  nestedRows <- load (Arrow.columnArrayToNestedRows namedField nestedNames)
  nestedBytes <- load (N.buildNestedFile (V.singleton ("v", nestedSchema)) (V.singleton nestedRows))
  nestedFile <- load (Read.loadParquetFile nestedBytes)
  nestedValues <- load (VP.readVariantColumn nestedFile 0 "v")
  check "writer aligns nested list element wrappers by name" (nestedValues == V.singleton expectedNames)
  field <- load (Schema.variantField "v" True (Just int32))
  storage <- load (Schema.compileVariantStorageSchema field)
  let row value = N.NRStruct (V.fromList [metadata, N.NRNull, value])
      rows = V.fromList [row (N.NRLeaf (N.LvInt32 7)), N.NRNull, row N.NRNull]
  native <- load (file storage rows)
  actual <- load (VP.readVariantColumn native 0 "v")
  check "primitive, null root and implicit Variant null stay distinct" (actual == V.fromList [Just (B.VInt32 7), Nothing, Just B.VNull])
  check "native annotation emitted" (P.seLogicalType (P.fmSchema (Read.pfFooter native) V.! 1) == Just (P.LTVariant 1))
  let conflict = N.NRStruct (V.fromList [metadata, leaf (BS.singleton 0), N.NRLeaf (N.LvInt32 7)])
  check "conflicting primitive storage rejected" (isLeft (file storage (V.singleton conflict)))
  let wrapper = A.defaultField "element" False A.AStruct (V.singleton int32)
      array = A.defaultField "typed_value" True A.AList (V.singleton wrapper)
      root = (A.defaultField "v" True A.AStruct (V.fromList [A.defaultLeafField "metadata" False A.ABinary, array])) {A.fieldMetadata = Schema.variantExtensionMetadata}
      elements = V.fromList [N.NRStruct (V.singleton (N.NRLeaf (N.LvInt32 3))), N.NRStruct (V.singleton N.NRNull), N.NRStruct (V.singleton N.NRNull)]
      arrayRow = N.NRStruct (V.fromList [metadata, N.NRList elements])
  arrays <- load (Schema.compileVariantStorageSchema root)
  arrayFile <- load (file arrays (V.singleton arrayRow))
  arrayValues <- load (VP.readVariantColumn arrayFile 0 "v")
  check "typed-only arrays gain explicit null fallbacks without losing elements" (arrayValues == V.singleton (Just (B.VArray (V.fromList [B.VInt32 3, B.VNull, B.VNull]))))
  forLimits <- load (Schema.compileVariantStorageSchemaWithLimits (B.VariantLimits 10 3 0) Schema.defaultVariantSchemaLimits root)
  check "one budget covers typed array and materialized null elements" (isLeft (file forLimits (V.singleton arrayRow)))
  noDepth <- load (Schema.compileVariantStorageSchemaWithLimits (B.VariantLimits 0 100 0) Schema.defaultVariantSchemaLimits root)
  check "missing array elements still obey nesting depth" (isLeft (file noDepth (V.singleton arrayRow)))
  let identified = field {A.fieldMetadata = A.fieldMetadata field `V.snoc` ("PARQUET:field_id", "-7"), A.fieldChildren = V.imap (\index child -> child {A.fieldMetadata = A.fieldMetadata child `V.snoc` ("PARQUET:field_id", if index == 0 then "11" else if index == 1 then "12" else "13")}) (A.fieldChildren field)}
  ids <- load (Schema.compileVariantStorageSchema identified)
  withIds <- load (file ids (V.singleton (row (N.NRLeaf (N.LvInt32 1)))))
  check "field IDs preserved through native schema" (V.map P.seFieldId (V.tail (P.fmSchema (Read.pfFooter withIds))) == V.fromList [Just (-7), Just 11, Just 12, Just 13])
  recovered <- load (Arrow.parquetFileArrowSchemaChecked withIds)
  let recoveredField = V.head (A.arrowFields recovered)
  check "native Arrow schema retains root and child field IDs" (V.elem ("PARQUET:field_id", "-7") (A.fieldMetadata recoveredField) && V.elem ("PARQUET:field_id", "13") (A.fieldMetadata (A.fieldChildren recoveredField V.! 2)))
  let invalidId = field {A.fieldMetadata = A.fieldMetadata field V.++ V.fromList [("PARQUET:field_id", "1"), ("PARQUET:field_id", "2")]}
  check "duplicate field IDs rejected" (isLeft (Schema.compileVariantStorageSchema invalidId))
  -- Semantic selection must preserve all primitive tags, not just values
  -- that happen to fit the target's numeric carrier.
  target <- load (Schema.variantField "v" True (Just (A.defaultLeafField "typed_value" True (A.AInt 64 True))))
  semantic <- load (Shred.compileVariantShredder target)
  semanticRows <- V.mapM (load . Shred.shredVariant semantic . Just) Cases.values
  semanticBytes <- load (N.buildNestedFile (V.singleton ("v", Shred.variantShredSchema semantic)) (V.singleton semanticRows))
  semanticFile <- load (Read.loadParquetFile semanticBytes)
  semanticValues <- load (VP.readVariantColumn semanticFile 0 "v")
  actualBytes <- V.mapM (load . traverse B.encodeVariant) semanticValues
  expectedBytes <- V.mapM (load . traverse B.encodeVariant) (Just <$> Cases.values)
  check "semantic shreds preserve every Variant primitive/container/IEEE tag" (actualBytes == expectedBytes)
  let decimalField = A.defaultLeafField "typed_value" True (A.ADecimal32 8 2)
  decimalTarget <- load (Schema.variantField "v" True (Just decimalField))
  decimalShred <- load (Shred.compileVariantShredder decimalTarget)
  let decimalValues = V.fromList [B.VDecimal4 2 1, B.VDecimal4 3 1, B.VDecimal4 2 100000000, B.VDecimal8 2 1]
  decimalRows <- V.mapM (load . Shred.shredVariant decimalShred . Just) decimalValues
  decimalBytes <- load (N.buildNestedFile (V.singleton ("v", Shred.variantShredSchema decimalShred)) (V.singleton decimalRows))
  decimalFile <- load (Read.loadParquetFile decimalBytes)
  decimalOutput <- load (VP.readVariantColumn decimalFile 0 "v")
  check "decimal scale, precision overflow and width mismatch use exact fallback" (decimalOutput == (Just <$> decimalValues))
  emptyDictionary <- load (B.buildVariantMetadata V.empty)
  objectTarget <- load (Schema.variantField "v" True (Just (A.defaultField "typed_value" True A.AStruct (V.singleton (A.defaultField "a" False A.AStruct (V.singleton int32))))))
  objectShred <- load (Shred.compileVariantShredder objectTarget)
  check "fully shredded names must exist in the supplied dictionary" (isLeft (Shred.shredVariantWithMetadata objectShred emptyDictionary (Just (B.VObject (M.singleton "a" (B.VInt32 1))))))
  passed <-
    H.check
      ( H.withTests
          500
          ( H.property $ do
              number <- H.forAll (Gen.int32 (Range.linear (-1000000) 1000000))
              present <- H.forAll Gen.bool
              remainder <- H.forAll Gen.bool
              dictionary <- H.evalEither (B.buildVariantMetadata (V.fromList ["a", "rest"]))
              let wrap = A.defaultField "a" False A.AStruct (V.singleton int32)
                  object = A.defaultField "typed_value" True A.AStruct (V.singleton wrap)
              objectField <- H.evalEither (Schema.variantField "v" True (Just object))
              objectStorage <- H.evalEither (Schema.compileVariantStorageSchema objectField)
              fragment <- H.evalEither (B.encodeVariantValue dictionary (B.VObject (if remainder then M.singleton "rest" (B.VInt8 9) else M.empty)))
              let child = N.NRStruct (V.singleton (if present then N.NRLeaf (N.LvInt32 number) else N.NRNull))
                  objectRow = N.NRStruct (V.fromList [leaf (B.metadataBytes dictionary), if remainder then leaf fragment else N.NRNull, N.NRStruct (V.singleton child)])
                  expected = (if present then M.insert "a" (B.VInt32 number) else id) (if remainder then M.singleton "rest" (B.VInt8 9) else M.empty)
              encoded <- H.evalEither (file objectStorage (V.singleton objectRow))
              decoded <- H.evalEither (VP.readVariantColumn encoded 0 "v")
              decoded H.=== V.singleton (Just (B.VObject expected))
          )
      )
  check "generated partial objects with shared dictionaries" passed
  dictionary <- load (B.buildVariantMetadata (V.singleton (T.replicate (1024 * 1024) "a")))
  let bytes = B.metadataBytes dictionary
      count = fromIntegral (maxBound :: Int32) `quot` (BS.length bytes + 4) + 1
      oversized = V.replicate count (N.NRStruct (V.fromList [leaf bytes, N.NRNull, N.NRLeaf (N.LvInt32 1)]))
  _ <- evaluate (BS.length bytes)
  _ <- evaluate (V.length oversized)
  before <- getAllocationCounter
  rejected <- evaluate (isLeft (N.shred (N.NSOptional (N.NSVariantTyped storage)) oversized))
  after <- getAllocationCounter
  check "all byte preflights precede repeated dictionary decoding" rejected
  check ("oversized typed page allocation: " ++ show (before - after)) (before - after < 8 * 1024 * 1024)
  putStrLn "Typed Variant native writing, dictionaries, nulls, field IDs, budgets and preflight regressions passed"
