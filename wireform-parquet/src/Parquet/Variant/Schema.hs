{-# LANGUAGE OverloadedStrings #-}

{- | Canonical @arrow.parquet.variant@ storage schemas. The binary Variant
codec accepts a wider scale range than a typed decimal column: typed storage
must use the narrowest precision band and @0 <= scale <= precision@.

See https://arrow.apache.org/docs/format/CanonicalExtensions.html#parquet-variant.
This module checks storage shape and type mappings; value reconstruction is
owned by the Variant semantic reader, not by this schema validator.
-}
module Parquet.Variant.Schema (
  VariantSchemaLimits (..),
  VariantStorageSchema,
  compileVariantStorageSchema,
  compileVariantStorageSchemaWithLimits,
  defaultVariantSchemaLimits,
  variantExtensionMetadata,
  isVariantField,
  variantField,
  validateVariantField,
  validateVariantFieldWithLimits,
  variantTypedLeaf,
) where

import Arrow.Types qualified as A
import Control.Monad (unless)
import Data.Int (Int32)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Parquet.Nested.Types (VariantStorageSchema)
import Parquet.Nested.Types qualified as N
import Parquet.Types qualified as P
import Parquet.Variant qualified as B


data VariantSchemaLimits = VariantSchemaLimits
  { variantSchemaMaxDepth :: {-# UNPACK #-} !Int
  , variantSchemaMaxFields :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)


defaultVariantSchemaLimits :: VariantSchemaLimits
defaultVariantSchemaLimits = VariantSchemaLimits 64 1048576


variantExtensionMetadata :: V.Vector (Text, Text)
variantExtensionMetadata = V.fromList [("ARROW:extension:name", "arrow.parquet.variant"), ("ARROW:extension:metadata", "")]


isVariantField :: A.Field -> Bool
isVariantField = V.elem ("ARROW:extension:name", "arrow.parquet.variant") . A.fieldMetadata


{- | Standard metadata/value storage, optionally with a typed column. Its
supplied name/nullability are replaced with the canonical optional field.
-}
variantField :: Text -> Bool -> Maybe A.Field -> Either String A.Field
variantField name nullable typed = do
  let fields = V.fromList ([A.defaultLeafField "metadata" False A.ABinary, A.defaultLeafField "value" True A.ABinary] ++ maybe [] (\field -> [field {A.fieldName = "typed_value", A.fieldNullable = True}]) typed)
      result = (A.defaultField name nullable A.AStruct fields) {A.fieldMetadata = variantExtensionMetadata}
  validateVariantField result
  pure result


values :: Text -> A.Field -> V.Vector Text
values key = V.map snd . V.filter ((== key) . fst) . A.fieldMetadata


bad :: String -> Either String a
bad = Left . ("Parquet.Variant.Schema: " ++)


need :: Bool -> String -> Either String ()
need condition message = unless condition (bad message)


emptyExtension :: Text -> A.Field -> Either String ()
emptyExtension name field = do
  need (values "ARROW:extension:name" field == V.singleton name) "extension name must be unique and exact"
  let parameters = values "ARROW:extension:metadata" field
  need (V.length parameters <= 1 && V.all T.null parameters) "extension parameters must be absent or empty"


plain :: A.Field -> Bool
plain field = A.fieldDictionary field == Nothing && V.null (A.fieldChildren field)


binary :: A.ArrowType -> Bool
binary typ = typ `elem` [A.ABinary, A.ALargeBinary, A.ABinaryView]


validateVariantField :: A.Field -> Either String ()
validateVariantField = validateVariantFieldWithLimits defaultVariantSchemaLimits


validateVariantFieldWithLimits :: VariantSchemaLimits -> A.Field -> Either String ()
validateVariantFieldWithLimits limits field = do
  need (variantSchemaMaxDepth limits >= 0 && variantSchemaMaxFields limits > 0) "invalid schema resource limits"
  emptyExtension "arrow.parquet.variant" field
  _ <- node 0 True (variantSchemaMaxFields limits) field
  pure ()
  where
    charge depth remaining = do
      need (depth <= variantSchemaMaxDepth limits) "schema depth limit exceeded"
      need (remaining > 0) "schema field limit exceeded"
      pure (remaining - 1)
    node depth root remaining current = do
      budget <- charge depth remaining
      need (A.fieldType current == A.AStruct && A.fieldDictionary current == Nothing) "Variant node must use struct storage"
      let fields = A.fieldChildren current
          names = A.fieldName <$> fields
          allowed = if root then ["metadata", "value", "typed_value"] else ["value", "typed_value"]
      need (V.length fields <= length allowed && Set.size (Set.fromList (V.toList names)) == V.length fields && V.all (`elem` allowed) names) "node fields must be unique canonical names"
      need (V.elem "value" names || V.elem "typed_value" names) "node requires value or typed_value"
      need (not root || V.elem "metadata" names) "Variant root requires metadata"
      V.foldM'
        ( \left child -> case A.fieldName child of
            "metadata" -> metadata (depth + 1) left child
            "value" -> do
              next <- charge (depth + 1) left
              need (plain child && binary (A.fieldType child)) "value must use binary storage"
              pure next
            "typed_value" -> typed (depth + 1) left child
            _ -> bad "unexpected node field"
        )
        budget
        fields
    metadata depth remaining current = do
      budget <- charge depth remaining
      need (not (A.fieldNullable current)) "metadata must be non-nullable"
      case A.fieldType current of
        A.ARunEndEncoded -> do
          need (A.fieldDictionary current == Nothing) "metadata cannot combine dictionary and run-end encoding"
          case V.toList (A.fieldChildren current) of
            [ends, entries] -> do
              b1 <- charge (depth + 1) budget
              b2 <- charge (depth + 1) b1
              need (plain ends && not (A.fieldNullable ends) && A.fieldType ends `elem` [A.AInt 16 True, A.AInt 32 True, A.AInt 64 True]) "metadata run ends require non-null signed16/32/64 storage"
              need (plain entries && binary (A.fieldType entries)) "metadata run values must be binary"
              pure b2
            _ -> bad "metadata run-end encoding requires two children"
        typ -> do
          need (binary typ && V.null (A.fieldChildren current)) "metadata must use binary storage"
          case A.fieldDictionary current of
            Nothing -> pure budget
            Just dictionary -> do
              case A.deIndexType dictionary of
                A.AInt width _ -> need (width `elem` [8, 16, 32, 64]) "metadata dictionary index width must be8/16/32/64"
                _ -> bad "metadata dictionary indices must be integers"
              pure budget
    typed depth remaining current = case A.fieldType current of
      A.AStruct -> do
        need (V.null (values "ARROW:extension:name" current)) "typed object must not carry another extension"
        budget <- charge depth remaining
        need (A.fieldDictionary current == Nothing) "typed objects cannot be dictionary encoded"
        let fields = A.fieldChildren current
        need (V.length fields <= budget) "schema field limit exceeded"
        need (Set.size (Set.fromList (V.toList (A.fieldName <$> fields))) == V.length fields) "object field names must be unique"
        V.foldM'
          ( \left child -> do
              need (not (A.fieldNullable child)) "shredded object field wrappers must be non-nullable"
              node (depth + 1) False left child
          )
          budget
          fields
      typ | typ `elem` [A.AList, A.ALargeList, A.AListView] -> do
        need (V.null (values "ARROW:extension:name" current)) "typed list must not carry another extension"
        budget <- charge depth remaining
        need (A.fieldDictionary current == Nothing) "typed lists cannot be dictionary encoded"
        case V.toList (A.fieldChildren current) of
          [child] -> do
            need (not (A.fieldNullable child)) "shredded list elements must be non-nullable"
            node (depth + 1) False budget child
          _ -> bad "typed list requires one struct element field"
      _ -> do
        budget <- charge depth remaining
        _ <- variantTypedLeaf current
        pure budget


{- | Exact Arrow-to-Parquet primitive mapping for typed Variant storage.
Generic Arrow decimals with negative scales, wider carriers or different
units remain valid Arrow types, but cannot be used as typed Variants.
-}
variantTypedLeaf :: A.Field -> Either String N.LeafType
variantTypedLeaf field = do
  need (plain field) "typed primitives cannot have children or dictionary encoding"
  let extension = values "ARROW:extension:name" field
  if extension == V.singleton "arrow.uuid"
    then do
      emptyExtension "arrow.uuid" field
      need (A.fieldType field == A.AFixedSizeBinary 16) "UUID storage must be fixed binary16"
      pure N.LtUuid
    else do
      need (V.null extension) "unrecognized typed primitive extension"
      case A.fieldType field of
        A.ABool -> pure N.LtBool
        A.AInt 8 True -> pure (N.LtInteger 8 True)
        A.AInt 16 True -> pure (N.LtInteger 16 True)
        A.AInt 32 True -> pure N.LtInt32
        A.AInt 64 True -> pure N.LtInt64
        A.AFloatingPoint A.Single -> pure N.LtFloat
        A.AFloatingPoint A.DoublePrecision -> pure N.LtDouble
        A.ADecimal32 precision scale -> decimal 1 9 precision scale
        A.ADecimal64 precision scale -> decimal 10 18 precision scale
        A.ADecimal precision scale -> decimal 19 38 precision scale
        A.ADate A.DateDay -> pure N.LtDate
        A.ATime A.Microsecond 64 -> pure (N.LtTime False P.LtMicros)
        A.ATimestamp A.Microsecond Nothing -> pure (N.LtTimestamp False P.LtMicros)
        A.ATimestamp A.Nanosecond Nothing -> pure (N.LtTimestamp False P.LtNanos)
        A.ATimestamp A.Microsecond (Just "UTC") -> pure (N.LtTimestamp True P.LtMicros)
        A.ATimestamp A.Nanosecond (Just "UTC") -> pure (N.LtTimestamp True P.LtNanos)
        typ | typ `elem` [A.ABinary, A.ALargeBinary, A.ABinaryView] -> pure N.LtBinary
        typ | typ `elem` [A.AUtf8, A.ALargeUtf8, A.AUtf8View] -> pure N.LtString
        _ -> bad "Arrow type has no canonical typed Variant mapping"
  where
    decimal low high precision scale = do
      need (precision >= low && precision <= high && scale >= 0 && scale <= precision) "typed decimal requires its exact precision band and scale0..precision"
      pure (N.LtDecimal (fromIntegral precision) (fromIntegral scale))


{- | Compile canonical Arrow Variant storage to a native typed schema.
Primitive tags/scales are never inferred from row values. Metadata storage
encodings normalize to binary while the original payload dictionary stays
byte-for-byte intact. Use 'N.NSVariantTyped' with this opaque schema.
-}
compileVariantStorageSchema :: A.Field -> Either String VariantStorageSchema
compileVariantStorageSchema = compileVariantStorageSchemaWithLimits B.defaultVariantLimits defaultVariantSchemaLimits


compileVariantStorageSchemaWithLimits :: B.VariantLimits -> VariantSchemaLimits -> A.Field -> Either String VariantStorageSchema
compileVariantStorageSchemaWithLimits limits schemaLimits field = do
  validateVariantFieldWithLimits schemaLimits field
  need (B.variantMaxDepth limits >= 0 && B.variantMaxValues limits > 0 && B.variantMaxDictionaryEntries limits >= 0) "invalid Variant resource limits"
  layout <- nodeLayout True field
  plan <- nodePlan field
  identifiers <- nodeIds field
  let metadataIndex = maybe (-1) id (V.findIndex ((== "metadata") . A.fieldName) (A.fieldChildren field))
  pure (N.VariantStorageSchema limits field layout metadataIndex identifiers plan)
  where
    optional nullable value = if nullable then N.NSOptional value else N.NSRequired value
    nodeLayout root current = do
      let fields = A.fieldChildren current
          shredded = V.any ((== "typed_value") . A.fieldName) fields
      children <-
        V.mapM
          ( \child -> do
              layout <- case A.fieldName child of
                "metadata" -> pure (N.NSRequired (N.NSPrimitive N.LtBinary))
                "value" -> pure (optional (not root || shredded) (N.NSPrimitive N.LtBinary))
                "typed_value" -> N.NSOptional <$> typedLayout child
                _ -> bad "unexpected storage field"
              pure (A.fieldName child, layout)
          )
          fields
      let complete = if V.any ((== "value") . A.fieldName) fields then children else V.snoc children ("value", N.NSOptional (N.NSPrimitive N.LtBinary))
      pure (N.NSStruct complete)
    typedLayout current = case A.fieldType current of
      A.AStruct ->
        N.NSStruct
          <$> V.mapM
            ( \child -> do
                layout <- nodeLayout False child
                pure (A.fieldName child, N.NSRequired layout)
            )
            (A.fieldChildren current)
      typ | typ `elem` [A.AList, A.ALargeList, A.AListView] -> do
        layout <- nodeLayout False (V.head (A.fieldChildren current))
        pure (N.NSList (N.NSRequired layout))
      _ -> N.NSPrimitive <$> variantTypedLeaf current
    nodePlan current = do
      let fields = A.fieldChildren current
          index name = maybe (-1) id (V.findIndex ((== name) . A.fieldName) fields)
          required position = maybe False (not . A.fieldNullable) (fields V.!? position)
          value = index "value"
          typed = index "typed_value"
      plan <- case fields V.!? typed of
        Nothing -> pure Nothing
        Just child -> Just <$> typedPlan child
      pure (N.VariantStorageNode (V.length fields) value typed (required value) (required typed) plan (maybe False below plan))
    typedPlan current = case A.fieldType current of
      A.AStruct -> do
        children <- V.imapM (\index child -> N.VariantStorageChild (A.fieldName child) index <$> nodePlan child) (A.fieldChildren current)
        pure (N.StorageObject children (Set.fromList (V.toList (N.storageChildName <$> children))))
      typ | typ `elem` [A.AList, A.ALargeList, A.AListView] -> N.StorageArray <$> nodePlan (V.head (A.fieldChildren current))
      _ -> pure N.StoragePrimitive

    fieldId current = case V.toList (values "PARQUET:field_id" current) of
      [] -> pure Nothing
      [text] -> case TR.signed TR.decimal text :: Either String (Integer, Text) of
        Right (number, rest) | T.null rest && number >= toInteger (minBound :: Int32) && number <= toInteger (maxBound :: Int32) -> pure (Just (fromInteger number))
        _ -> bad "field ID must fit a signed INT32"
      _ -> bad "field ID must be unique"
    nodeIds current = do
      ident <- fieldId current
      children <- V.mapM (\child -> if A.fieldName child == "typed_value" then typedIds child else V.singleton <$> fieldId child) (A.fieldChildren current)
      let body = V.concat (V.toList children)
          complete = if V.any ((== "value") . A.fieldName) (A.fieldChildren current) then body else V.snoc body Nothing
      pure (V.cons ident complete)
    typedIds current = do
      ident <- fieldId current
      case A.fieldType current of
        A.AStruct -> V.cons ident . V.concat . V.toList <$> V.mapM nodeIds (A.fieldChildren current)
        typ | typ `elem` [A.AList, A.ALargeList, A.AListView] -> V.cons ident . V.cons Nothing <$> nodeIds (V.head (A.fieldChildren current))
        _ -> pure (V.singleton ident)

    needs required plan = N.storageValueIndex plan < 0 || N.storageNormalizationBelow plan || (required && not (N.storageValueRequired plan) && not (N.storageTypedRequired plan))
    below current = case current of
      N.StoragePrimitive -> False
      N.StorageObject children _ -> V.any (needs False . N.storageChildNode) children
      N.StorageArray child -> needs True child
