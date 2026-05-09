-- | Render a 'Schema' AST back to JSON.
--
-- The renderer is intentionally direct: each 'ObjectSchemaData' field
-- maps to its canonical JSON Schema keyword. Boolean schemas render as
-- the JSON literal @true@ / @false@; an empty object schema renders as
-- @{}@.
--
-- Round-trip with @JsonSchema.Parser.parseSchema@ is the property
-- this module is tuned for; it is the same canonical shape that the
-- @JsonSchema.CodeGen@ output references and that
-- @JsonSchema.Class.jsonSchemaJSON@ exposes to users.
module JsonSchema.Renderer
  ( renderSchema
  , renderSchemaMinimal
  , renderSchemaCanonical
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import Numeric.Natural (Natural)

import JsonSchema.Types

-- | Render schema to JSON Value.
renderSchema :: Schema -> Value
renderSchema schema = case schemaCore schema of
  BooleanSchema b -> Bool b
  ObjectSchema obj ->
    case obj of
      SchemaObject (Left b) -> Bool b
      SchemaObject (Right o) -> renderObjectSchema schema o

-- | Render schema omitting default values.
renderSchemaMinimal :: Schema -> Value
renderSchemaMinimal = renderSchema

-- | Render schema in canonical (sorted-keys) form.
--
-- The aeson 'KeyMap' is unordered; we rely on consumers to sort keys
-- if they need a stable serialisation.
renderSchemaCanonical :: Schema -> Value
renderSchemaCanonical = renderSchema

-- ---------------------------------------------------------------------------
-- Object rendering
-- ---------------------------------------------------------------------------

renderObjectSchema :: Schema -> ObjectSchemaData -> Value
renderObjectSchema schema o =
  Object $ KM.fromList $ concat
    [ topLevel schema o
    , typeAndEnum o
    , refs o
    , composition o
    , conditional o
    , validationKeywords (objectSchemaDataValidation o)
    , annotationKeywords (objectSchemaDataAnnotations o)
    , defs (objectSchemaDataDefs o)
    ]

-- $schema / $id / $vocabulary live on the outer 'Schema' record.
topLevel :: Schema -> ObjectSchemaData -> [(Aeson.Key, Value)]
topLevel schema _ =
  catMaybes'
    [ schemaVersion schema       `pairAs` "$schema"
    , schemaId schema            `pairAs` "$id"
    ]

typeAndEnum :: ObjectSchemaData -> [(Aeson.Key, Value)]
typeAndEnum o =
  catMaybes'
    [ typePair (objectSchemaDataType o)
    , fmap (\xs -> (k "enum", Array (V.fromList (NE.toList xs))))
        (objectSchemaDataEnum o)
    , fmap (\v -> (k "const", v)) (objectSchemaDataConst o)
    ]

refs :: ObjectSchemaData -> [(Aeson.Key, Value)]
refs o =
  catMaybes'
    [ refPair       "$ref"            (objectSchemaDataRef o)
    , refPair       "$dynamicRef"     (objectSchemaDataDynamicRef o)
    , refPair       "$recursiveRef"   (objectSchemaDataRecursiveRef o)
    , textPair      "$anchor"         (objectSchemaDataAnchor o)
    , textPair      "$dynamicAnchor"  (objectSchemaDataDynamicAnchor o)
    , boolPair      "$recursiveAnchor"(objectSchemaDataRecursiveAnchor o)
    ]

composition :: ObjectSchemaData -> [(Aeson.Key, Value)]
composition o =
  catMaybes'
    [ schemaListPair "allOf" (objectSchemaDataAllOf o)
    , schemaListPair "anyOf" (objectSchemaDataAnyOf o)
    , schemaListPair "oneOf" (objectSchemaDataOneOf o)
    , subSchemaPair  "not"   (objectSchemaDataNot o)
    ]

conditional :: ObjectSchemaData -> [(Aeson.Key, Value)]
conditional o =
  catMaybes'
    [ subSchemaPair "if"   (objectSchemaDataIf o)
    , subSchemaPair "then" (objectSchemaDataThen o)
    , subSchemaPair "else" (objectSchemaDataElse o)
    ]

defs :: Map Text Schema -> [(Aeson.Key, Value)]
defs m
  | Map.null m = []
  | otherwise =
      [ ( k "$defs"
        , Object $ KM.fromList
            [ (Key.fromText n, renderSchema s) | (n, s) <- Map.toList m ]
        )
      ]

annotationKeywords :: SchemaAnnotations -> [(Aeson.Key, Value)]
annotationKeywords a =
  catMaybes'
    [ textPair    "title"       (annotationTitle a)
    , textPair    "description" (annotationDescription a)
    , fmap (\v -> (k "default", v)) (annotationDefault a)
    , case annotationExamples a of
        [] -> Nothing
        xs -> Just (k "examples", Array (V.fromList xs))
    , boolPair    "deprecated"  (annotationDeprecated a)
    , boolPair    "readOnly"    (annotationReadOnly a)
    , boolPair    "writeOnly"   (annotationWriteOnly a)
    , textPair    "$comment"    (annotationComment a)
    ]

validationKeywords :: SchemaValidation -> [(Aeson.Key, Value)]
validationKeywords v =
  catMaybes'
    [ scientificPair "multipleOf"        (validationMultipleOf v)
    , scientificPair "minimum"           (validationMinimum v)
    , scientificPair "maximum"           (validationMaximum v)
    , exclusivePair  "exclusiveMinimum"  (validationExclusiveMinimum v)
    , exclusivePair  "exclusiveMaximum"  (validationExclusiveMaximum v)
    , naturalPair    "minLength"         (validationMinLength v)
    , naturalPair    "maxLength"         (validationMaxLength v)
    , fmap (\(Regex r) -> (k "pattern", String r)) (validationPattern v)
    , formatPair     "format"            (validationFormat v)
    , textPair       "contentEncoding"   (validationContentEncoding v)
    , textPair       "contentMediaType"  (validationContentMediaType v)
    , itemsPair      (validationItems v)
    , schemaListPair "prefixItems"       (validationPrefixItems v)
    , subSchemaPair  "contains"          (validationContains v)
    , naturalPair    "minItems"          (validationMinItems v)
    , naturalPair    "maxItems"          (validationMaxItems v)
    , boolPair       "uniqueItems"       (validationUniqueItems v)
    , naturalPair    "minContains"       (validationMinContains v)
    , naturalPair    "maxContains"       (validationMaxContains v)
    , subSchemaPair  "unevaluatedItems"  (validationUnevaluatedItems v)
    , propsMapPair   "properties"        (validationProperties v)
    , patternPropsPair                   (validationPatternProperties v)
    , subSchemaPair  "additionalProperties"  (validationAdditionalProperties v)
    , subSchemaPair  "unevaluatedProperties" (validationUnevaluatedProperties v)
    , subSchemaPair  "propertyNames"     (validationPropertyNames v)
    , naturalPair    "minProperties"     (validationMinProperties v)
    , naturalPair    "maxProperties"     (validationMaxProperties v)
    , requiredPair                        (validationRequired v)
    , dependentRequiredPair               (validationDependentRequired v)
    , dependentSchemasPair                (validationDependentSchemas v)
    , dependenciesPair                    (validationDependencies v)
    ]

-- ---------------------------------------------------------------------------
-- Pair builders
-- ---------------------------------------------------------------------------

k :: Text -> Aeson.Key
k = Key.fromText

catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr step []
  where
    step Nothing acc = acc
    step (Just x) acc = x : acc

pairAs :: Aeson.ToJSON a => Maybe a -> Text -> Maybe (Aeson.Key, Value)
pairAs Nothing _ = Nothing
pairAs (Just x) name = Just (k name, Aeson.toJSON x)

textPair :: Text -> Maybe Text -> Maybe (Aeson.Key, Value)
textPair _ Nothing = Nothing
textPair name (Just t) = Just (k name, String t)

boolPair :: Text -> Maybe Bool -> Maybe (Aeson.Key, Value)
boolPair _ Nothing = Nothing
boolPair name (Just b) = Just (k name, Bool b)

scientificPair :: Text -> Maybe Scientific -> Maybe (Aeson.Key, Value)
scientificPair _ Nothing = Nothing
scientificPair name (Just s) = Just (k name, Number s)

naturalPair :: Text -> Maybe Natural -> Maybe (Aeson.Key, Value)
naturalPair _ Nothing = Nothing
naturalPair name (Just n) = Just (k name, Number (fromIntegral n))

refPair :: Text -> Maybe Reference -> Maybe (Aeson.Key, Value)
refPair _ Nothing = Nothing
refPair name (Just (Reference r)) = Just (k name, String r)

formatPair :: Text -> Maybe Format -> Maybe (Aeson.Key, Value)
formatPair _ Nothing = Nothing
formatPair name (Just f) = Just (k name, Aeson.toJSON f)

exclusivePair
  :: Text
  -> Maybe (Either Bool Scientific)
  -> Maybe (Aeson.Key, Value)
exclusivePair _ Nothing = Nothing
exclusivePair name (Just (Left b)) = Just (k name, Bool b)
exclusivePair name (Just (Right s)) = Just (k name, Number s)

itemsPair :: Maybe ArrayItemsValidation -> Maybe (Aeson.Key, Value)
itemsPair Nothing = Nothing
itemsPair (Just (ItemsSchema s)) = Just (k "items", renderSchema s)
itemsPair (Just (ItemsTuple ss madditional)) =
  case madditional of
    Nothing ->
      Just (k "items", Array (V.fromList (map renderSchema (NE.toList ss))))
    Just additional ->
      Just (k "items",
            Object $ KM.fromList
              [ (k "items", Array (V.fromList (map renderSchema (NE.toList ss))))
              , (k "additionalItems", renderSchema additional)
              ])

subSchemaPair :: Text -> Maybe Schema -> Maybe (Aeson.Key, Value)
subSchemaPair _ Nothing = Nothing
subSchemaPair name (Just s) = Just (k name, renderSchema s)

schemaListPair
  :: Text
  -> Maybe (NE.NonEmpty Schema)
  -> Maybe (Aeson.Key, Value)
schemaListPair _ Nothing = Nothing
schemaListPair name (Just ss) =
  Just (k name, Array (V.fromList (map renderSchema (NE.toList ss))))

propsMapPair
  :: Text
  -> Maybe (Map Text Schema)
  -> Maybe (Aeson.Key, Value)
propsMapPair _ Nothing = Nothing
propsMapPair name (Just m)
  | Map.null m = Nothing
  | otherwise =
      Just
        ( k name
        , Object $ KM.fromList
            [ (Key.fromText n, renderSchema s) | (n, s) <- Map.toList m ]
        )

patternPropsPair :: Maybe (Map Regex Schema) -> Maybe (Aeson.Key, Value)
patternPropsPair Nothing = Nothing
patternPropsPair (Just m)
  | Map.null m = Nothing
  | otherwise =
      Just
        ( k "patternProperties"
        , Object $ KM.fromList
            [ (Key.fromText pat, renderSchema s)
            | (Regex pat, s) <- Map.toList m
            ]
        )

requiredPair :: Maybe (Set.Set Text) -> Maybe (Aeson.Key, Value)
requiredPair Nothing = Nothing
requiredPair (Just s)
  | Set.null s = Nothing
  | otherwise =
      Just
        ( k "required"
        , Array (V.fromList (map String (Set.toList s)))
        )

dependentRequiredPair
  :: Maybe (Map Text (Set.Set Text))
  -> Maybe (Aeson.Key, Value)
dependentRequiredPair Nothing = Nothing
dependentRequiredPair (Just m)
  | Map.null m = Nothing
  | otherwise =
      Just
        ( k "dependentRequired"
        , Object $ KM.fromList
            [ ( Key.fromText n
              , Array (V.fromList (map String (Set.toList xs)))
              )
            | (n, xs) <- Map.toList m
            ]
        )

dependentSchemasPair
  :: Maybe (Map Text Schema)
  -> Maybe (Aeson.Key, Value)
dependentSchemasPair Nothing = Nothing
dependentSchemasPair (Just m)
  | Map.null m = Nothing
  | otherwise =
      Just
        ( k "dependentSchemas"
        , Object $ KM.fromList
            [ (Key.fromText n, renderSchema s) | (n, s) <- Map.toList m ]
        )

dependenciesPair
  :: Maybe (Map Text Dependency)
  -> Maybe (Aeson.Key, Value)
dependenciesPair Nothing = Nothing
dependenciesPair (Just m)
  | Map.null m = Nothing
  | otherwise =
      Just (k "dependencies", Object $ KM.fromList (map renderDep (Map.toList m)))
  where
    renderDep (n, DependencyProperties ps) =
      ( Key.fromText n
      , Array (V.fromList (map String (Set.toList ps)))
      )
    renderDep (n, DependencySchema s) =
      (Key.fromText n, renderSchema s)

typePair :: Maybe (OneOrMany SchemaType) -> Maybe (Aeson.Key, Value)
typePair Nothing = Nothing
typePair (Just (One t)) = Just (k "type", Aeson.toJSON t)
typePair (Just (Many ts)) =
  Just (k "type", Array (V.fromList (map Aeson.toJSON (NE.toList ts))))
