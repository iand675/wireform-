{-# LANGUAGE OverloadedStrings #-}

module VariantArrowRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Map.Strict qualified as M
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import GHC.Conc (getAllocationCounter)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Variant qualified as B
import Parquet.Variant.Arrow qualified as R
import Parquet.Variant.Schema qualified as Schema


load :: Either String a -> IO a
load = either fail pure


check :: String -> Bool -> IO ()
check name ok = unless ok (fail ("Variant Arrow regression: " ++ name))


metadataField :: A.Field
metadataField = A.defaultLeafField "metadata" False A.ABinary


valueField :: A.Field
valueField = A.defaultLeafField "value" True A.ABinary


longField :: A.Field
longField = A.defaultLeafField "typed_value" True (A.AInt 64 True)


wrapper :: A.Field
wrapper = A.defaultField "a" False A.AStruct (V.fromList [valueField, longField])


root :: V.Vector A.Field -> A.Field
root children = (A.defaultField "v" True A.AStruct children) {A.fieldMetadata = Schema.variantExtensionMetadata}


-- A partial object whose fallback fragment uses a dictionary also containing
-- the completely shredded name a. Encoding that fragment with a new dictionary
-- would silently change its object IDs and lose/corrupt the rest field.
data PartialCase = PartialCase {partialField :: !A.Field, partialColumn :: !C.ColumnArray, partialExpected :: !B.Variant}


partial :: Int64 -> Either String PartialCase
partial number = do
  dictionary <- B.buildVariantMetadata (V.fromList ["a", "rest"])
  let remainder = B.VObject (M.singleton "rest" (B.VArray (V.fromList [B.VInt8 1, B.VInt8 2])))
  fallback <- B.encodeVariantValue dictionary remainder
  child <- C.mkStruct 1 Nothing (V.fromList [("value", C.fromMaybeByteStrings (V.singleton Nothing)), ("typed_value", C.fromMaybes C.PInt64 (V.singleton (Just number)))])
  typed <- C.mkStruct 1 Nothing (V.singleton ("a", child))
  let typedField = A.defaultField "typed_value" True A.AStruct (V.singleton wrapper)
      field = root (V.fromList [typedField, metadataField, valueField])
      expected = B.VObject (M.insert "a" (B.VInt64 number) (M.singleton "rest" (B.VArray (V.fromList [B.VInt8 1, B.VInt8 2]))))
  column <- C.mkStruct 1 Nothing (V.fromList [("typed_value", typed), ("metadata", C.fromMaybeByteStrings (V.singleton (Just (B.metadataBytes dictionary)))), ("value", C.fromMaybeByteStrings (V.singleton (Just fallback)))])
  pure (PartialCase field column expected)


tests :: IO ()
tests = do
  example <- load (partial minBound)
  let field = partialField example; column = partialColumn example; expected = partialExpected example
  reader <- load (R.compileVariantArrowReader field column)
  check "partial object reconstructs typed plus fallback with original IDs" (R.variantArrowAt reader 0 == Right (Just expected))
  check "row bounds reject" (isLeft (R.variantArrowAt reader (-1)) && isLeft (R.variantArrowAt reader 1))
  let compile limits = R.compileVariantArrowReaderWithLimits limits Schema.defaultVariantSchemaLimits field column
  limited <- load (compile (B.VariantLimits 500 4 100))
  check "typed and fallback siblings share one global count budget" (isLeft (R.variantArrowAt limited 0))
  exact <- load (compile (B.VariantLimits 2 5 2))
  check "exact tree count/depth/dictionary budgets succeed" (R.variantArrowAt exact 0 == Right (Just expected))
  shallow <- load (compile (B.VariantLimits 1 100 2))
  check "fallback fragments retain their position in typed tree depth" (isLeft (R.variantArrowAt shallow 0))
  let emptyMetadata = C.fromMaybeByteStrings (V.replicate 4 (Just (BS.pack [1, 0, 0])))
      binary = C.fromMaybeByteStrings (V.fromList [Nothing, Just (BS.singleton 0), Nothing, Just "poison"])
      typed = C.fromMaybes C.PInt64 (V.fromList [Nothing, Nothing, Just 34, Just 999])
      validity = C.validityFromBools (V.fromList [True, True, True, False])
      primitiveField = root (V.fromList [metadataField, valueField, longField])
  primitiveColumn <- load (C.mkStruct 4 validity (V.fromList [("metadata", emptyMetadata), ("value", binary), ("typed_value", typed)]))
  primitive <- load (R.compileVariantArrowReader primitiveField primitiveColumn)
  actual <- load (V.generateM 4 (R.variantArrowAt primitive))
  check "required missing root becomes Variant null; null root masks poison" (actual == V.fromList [Just B.VNull, Just B.VNull, Just (B.VInt64 34), Nothing])
  let requiredTyped = primitiveField {A.fieldChildren = V.fromList [metadataField, valueField, longField {A.fieldNullable = False}]}
      requiredValue = primitiveField {A.fieldChildren = V.fromList [metadataField, valueField {A.fieldNullable = False}, longField]}
  typedRequired <- load (R.compileVariantArrowReader requiredTyped primitiveColumn)
  check "visible required typed null is invalid instead of fallback" (isLeft (R.variantArrowAt typedRequired 0) && isLeft (R.variantArrowAt typedRequired 1))
  check "present required typed value remains valid" (R.variantArrowAt typedRequired 2 == Right (Just (B.VInt64 34)))
  valueRequired <- load (R.compileVariantArrowReader requiredValue primitiveColumn)
  check "visible required fallback null is invalid instead of typed value" (isLeft (R.variantArrowAt valueRequired 0) && isLeft (R.variantArrowAt valueRequired 2))
  check "present required fallback remains valid" (R.variantArrowAt valueRequired 1 == Right (Just B.VNull))
  maskedColumn <- load (C.mkStruct 1 (C.validityFromBools (V.singleton False)) (V.fromList [("metadata", C.fromMaybeByteStrings (V.singleton (Just "poison"))), ("value", C.fromMaybeByteStrings (V.singleton (Just "poison"))), ("typed_value", C.fromMaybes C.PInt64 (V.singleton Nothing))]))
  maskedReader <- load (R.compileVariantArrowReader requiredTyped maskedColumn)
  check "null parent masks null required typed child and poison payloads" (R.variantArrowAt maskedReader 0 == Right Nothing)
  check "non-nullable root cannot mask a null slot" (isLeft (R.compileVariantArrowReader primitiveField {A.fieldNullable = False} primitiveColumn))
  let missing = C.fromMaybeByteStrings (V.singleton Nothing)
      metadata = C.fromMaybeByteStrings (V.singleton (Just (BS.pack [1, 0, 0])))
  elements <- load (C.mkStruct 3 Nothing (V.singleton ("value", C.fromMaybeByteStrings (V.fromList [Just (BS.singleton 0), Nothing, Just (BS.pack [12, 127])]))))
  list <- load (C.mkList Nothing (S.fromList [0, 3]) elements)
  let elementField = A.defaultField "element" False A.AStruct (V.singleton valueField)
      listField = A.defaultField "typed_value" True A.AList (V.singleton elementField)
      arrayField = root (V.fromList [metadataField, listField])
  array <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", metadata), ("typed_value", list)]))
  arrays <- load (R.compileVariantArrowReader arrayField array)
  check "missing array element materializes null and preserves cardinality" (R.variantArrowAt arrays 0 == Right (Just (B.VArray (V.fromList [B.VNull, B.VNull, B.VInt8 127]))))
  check "bad root schema is rejected before column traversal" (isLeft (R.compileVariantArrowReader arrayField {A.fieldMetadata = V.empty} array))
  let ordinaryRoot = root (V.fromList [metadataField, valueField])
  metadataMissing <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", missing), ("value", missing)]))
  mm <- load (R.compileVariantArrowReader ordinaryRoot metadataMissing)
  check "present root requires metadata even when its value is missing" (isLeft (R.variantArrowAt mm 0))
  -- A fragment nested under a typed object starts at depth one. Resetting
  -- its binary decoder to depth zero would incorrectly admit this array.
  dict <- load (B.buildVariantMetadata (V.singleton "a"))
  payload <- load (B.encodeVariantValue dict (B.VArray (V.singleton B.VNull)))
  fragment <- load (C.mkStruct 1 Nothing (V.singleton ("value", C.fromMaybeByteStrings (V.singleton (Just payload)))))
  containing <- load (C.mkStruct 1 Nothing (V.singleton ("a", fragment)))
  let valueWrapper = A.defaultField "a" False A.AStruct (V.singleton valueField)
      containingField = A.defaultField "typed_value" True A.AStruct (V.singleton valueWrapper)
      nestedField = root (V.fromList [metadataField, containingField])
  nestedColumn <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", C.fromMaybeByteStrings (V.singleton (Just (B.metadataBytes dict)))), ("typed_value", containing)]))
  tooShallow <- load (R.compileVariantArrowReaderWithLimits (B.VariantLimits 1 100 100) Schema.defaultVariantSchemaLimits nestedField nestedColumn)
  check "nested fallback decoder never resets its actual depth" (isLeft (R.variantArrowAt tooShallow 0))
  deepEnough <- load (R.compileVariantArrowReaderWithLimits (B.VariantLimits 2 3 1) Schema.defaultVariantSchemaLimits nestedField nestedColumn)
  check "nested fallback decoder counts one semantic root" (R.variantArrowAt deepEnough 0 == Right (Just (B.VObject (M.singleton "a" (B.VArray (V.singleton B.VNull))))))
  -- Directly sliced REE metadata retains a nonzero logical offset. No
  -- expansion or assumption that logical row zero is physical run zero.
  metadataValues <- load (C.mkBinary Nothing (S.fromList [0, 6, 9]) (BS.pack [112, 111, 105, 115, 111, 110, 1, 0, 0]))
  runs <- load (C.mkRunEndEncoded (C.primColumn C.PInt16 (S.fromList [2, 5])) metadataValues)
  let sliced = C.sliceColumnArray 1 4 runs
      reeField = A.defaultField "metadata" False A.ARunEndEncoded (V.fromList [A.defaultLeafField "run_ends" False (A.AInt 16 True), A.defaultLeafField "values" True A.ABinary])
      reeSchema = root (V.fromList [reeField, longField])
      rootValidity = C.validityFromBools (V.fromList [False, True, True, True])
  reeColumn <- load (C.mkStruct 4 rootValidity (V.fromList [("metadata", sliced), ("typed_value", C.primColumn C.PInt64 (S.fromList [999, 7, 8, 9]))]))
  reeReader <- load (R.compileVariantArrowReader reeSchema reeColumn)
  reeValues <- load (V.generateM 4 (R.variantArrowAt reeReader))
  check "sliced run-end metadata preserves offset and masks unreachable poison" (reeValues == V.fromList [Nothing, Just (B.VInt64 7), Just (B.VInt64 8), Just (B.VInt64 9)])
  -- Wide objects can share their all-missing child buffers. The schema
  -- work budget bounds their compiled shape; names must not be rebound by
  -- rescanning the whole column vector once for every sibling.
  emptyFieldColumn <- load (C.mkStruct 1 Nothing (V.singleton ("value", missing)))
  let width = 50000
      nameAt i = TL.toStrict (TB.toLazyText ("k" <> TBI.decimal i))
      wideFields = V.generate width (\i -> A.defaultField (nameAt i) False A.AStruct (V.singleton valueField))
      wideColumns = V.map (\child -> (A.fieldName child, emptyFieldColumn)) wideFields
      wideTyped = A.defaultField "typed_value" True A.AStruct wideFields
      wideSchema = root (V.fromList [metadataField, wideTyped])
  wideObject <- load (C.mkStruct 1 Nothing wideColumns)
  wideColumn <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", metadata), ("typed_value", wideObject)]))
  wideReader <- load (R.compileVariantArrowReaderWithLimits (B.VariantLimits 1 1 0) Schema.defaultVariantSchemaLimits wideSchema wideColumn)
  check "wide all-missing object consumes only its semantic root" (R.variantArrowAt wideReader 0 == Right (Just (B.VObject M.empty)))
  -- Valid Arrow buffers, but a required shared dictionary name is absent.
  -- Reject by presence before allocating 100000 reconstructed values.
  let many = 100000
      elementType = A.defaultField "element" False A.AStruct (V.singleton longField)
      largeListType = A.defaultField "typed_value" True A.AList (V.singleton elementType)
      largeWrapperType = A.defaultField "a" False A.AStruct (V.singleton largeListType)
      largeObjectType = A.defaultField "typed_value" True A.AStruct (V.singleton largeWrapperType)
      hostileSchema = root (V.fromList [metadataField, largeObjectType])
  manyElements <- load (C.mkStruct many Nothing (V.singleton ("typed_value", C.primColumn C.PInt64 (S.replicate many 42))))
  manyList <- load (C.mkList Nothing (S.fromList [0, fromIntegral many]) manyElements)
  largeWrapper <- load (C.mkStruct 1 Nothing (V.singleton ("typed_value", manyList)))
  largeObject <- load (C.mkStruct 1 Nothing (V.singleton ("a", largeWrapper)))
  hostileColumn <- load (C.mkStruct 1 Nothing (V.fromList [("metadata", metadata), ("typed_value", largeObject)]))
  hostileReader <- load (R.compileVariantArrowReader hostileSchema hostileColumn)
  _ <- evaluate (R.variantArrowLength hostileReader)
  before <- getAllocationCounter
  rejected <- evaluate (isLeft (R.variantArrowAt hostileReader 0))
  after <- getAllocationCounter
  check "missing shared name rejects before typed subtree allocation" (rejected && before - after < 1024 * 1024)
  good <- H.check $ H.withTests 1000 $ H.property $ do
    number <- H.forAll (Gen.int64 Range.constantBounded)
    exampleCase <- H.evalEither (partial number)
    compiled <- H.evalEither (R.compileVariantArrowReader (partialField exampleCase) (partialColumn exampleCase))
    R.variantArrowAt compiled 0 H.=== Right (Just (partialExpected exampleCase))
  check "generated partial object semantic round trips" good
  putStrLn "Variant Arrow typed/fallback/global-budget/missing/null regressions passed"
