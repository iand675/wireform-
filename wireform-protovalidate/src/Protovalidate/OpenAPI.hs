{-# LANGUAGE OverloadedStrings #-}

{- | Map protovalidate rules onto JSON Schema (OpenAPI 3.1) validation
keywords, so the constraints declared with @(buf.validate.field)@ /
@(buf.validate.message)@ carry through into a generated OpenAPI document.

The bridge is a 'Proto.JSONSchema.SchemaOptions' built from the rules
extracted by "Protovalidate.Schema" ('fileMessageRules' /
'parseProtoRules'). Pass it to
'Network.Connect.OpenAPI.connectOpenApiWith' (or
'Proto.JSONSchema.componentSchemasWith').

== What maps to a standard keyword

Rules whose semantics have a faithful JSON Schema equivalent become standard
keywords, so any OpenAPI/JSON-Schema tool understands them:

  * @string.min_len@ / @max_len@ / @len@       → @minLength@ / @maxLength@ (+ both for @len@)
  * @string.pattern@                            → @pattern@
  * @string.const@                              → @const@; @string.in@ → @enum@
  * @string.email@\/@hostname@\/@uri@\/@uri_ref@\/@ipv4@\/@ipv6@\/@uuid@ → @format@
  * numeric @gt@\/@gte@\/@lt@\/@lte@            → @exclusiveMinimum@\/@minimum@\/@exclusiveMaximum@\/@maximum@
  * numeric @const@                             → @const@; numeric @in@ → @enum@
  * @repeated.min_items@ / @max_items@          → @minItems@ / @maxItems@
  * @repeated.unique@                           → @uniqueItems@
  * @map.min_pairs@ / @max_pairs@               → @minProperties@ / @maxProperties@
  * @required@                                  → the field joins the object's @required@ list

== What is preserved as @x-@ extensions

Everything without a clean JSON Schema analogue is preserved losslessly under
an @x-protovalidate@ object (so nothing is silently dropped) and, for CEL, an
@x-cel@ array of @{id, message, expression}@:

  * custom field / message CEL (@(buf.validate.field).cel@ / @.message).cel@) → @x-cel@
  * @string.prefix@\/@suffix@\/@contains@\/@not_contains@, @not_in@, IP-prefix
    variants, @host_and_port@, @tuuid@, @address@
  * @bytes.*@ length/prefix/... rules (JSON @bytes@ is a base64 string, so
    byte-length semantics don't match @minLength@)
  * @enum.defined_only@, @string.well_known_regex@ (they arrive as custom
    constraints), timestamp/duration bounds, @map.keys@ / @map.values@ /
    per-@items@ sub-rules, and predefined constraints
-}
module Protovalidate.OpenAPI (
  protovalidateSchemaOptions,
  fieldConstraintsFor,
) where

import CEL.Value qualified as CV
import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), object, (.=))
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (Pair)
import Data.ByteString.Base64 qualified as B64
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Proto.JSONSchema (FieldConstraints (..), SchemaOptions (..), defaultSchemaOptions)
import Protovalidate.Constraint (Constraint, constraintId, constraintMessage, constraintSource)
import Protovalidate.Rules (FieldRules (..), MessageRules (..), RuleKind (..))


{- | Build a 'SchemaOptions' from extracted per-message rules (keyed by bare
message name, as 'Protovalidate.Schema.fileMessageRules' returns them). The
field/message annotators look rules up by the leaf of the fully-qualified
message name.
-}
protovalidateSchemaOptions :: [(Text, MessageRules)] -> SchemaOptions
protovalidateSchemaOptions rules =
  defaultSchemaOptions
    { soFieldAnnotator = \fqn fld -> do
        mr <- Map.lookup (leaf fqn) table
        fr <- lookup fld (mrFields mr)
        fieldConstraintsFor fr
    , soMessageAnnotator = \fqn -> case Map.lookup (leaf fqn) table of
        Just mr | not (null (mrCustom mr)) -> [celPair (mrCustom mr)]
        _ -> []
    }
  where
    table = Map.fromList rules
    leaf = last . T.splitOn "."


{- | Translate a single field's rules into JSON Schema keywords + a required
flag. 'Nothing' when the field contributes nothing (no rules, not required).
-}
fieldConstraintsFor :: FieldRules -> Maybe FieldConstraints
fieldConstraintsFor fr
  | null keywords && not (frRequired fr) = Nothing
  | otherwise = Just (FieldConstraints keywords (frRequired fr))
  where
    keywords = standardKws <> xProtovalidateKws <> celKws
    standardKws = concatMap (standardKeyword (frKind fr)) (frRules fr)
    -- Rules with no standard-keyword analogue, kept losslessly.
    xProtovalidateKws = case extensionObject fr of
      [] -> []
      x -> ["x-protovalidate" .= object x]
    -- Field-level custom CEL always rides along (independent of x-protovalidate).
    celKws = if null (frCustom fr) then [] else [celPair (frCustom fr)]


-- ---------------------------------------------------------------------------
-- Standard keyword mapping
-- ---------------------------------------------------------------------------

{- | Map one @(ruleName, value)@ to standard JSON Schema keyword(s), given the
field's rule kind. Rules with no clean analogue return @[]@ (they are picked
up by 'extensionObject' instead).
-}
standardKeyword :: Maybe RuleKind -> (Text, CV.Value) -> [Pair]
standardKeyword mk (name, v) = case name of
  "min_len" | isStringKind mk -> ["minLength" .= toJson v]
  "max_len" | isStringKind mk -> ["maxLength" .= toJson v]
  "len" | isStringKind mk -> ["minLength" .= toJson v, "maxLength" .= toJson v]
  "pattern" | isStringKind mk -> ["pattern" .= toJson v]
  "const" -> ["const" .= toJson v]
  "in" -> ["enum" .= enumArray v]
  "gt" | isNumericKind mk -> ["exclusiveMinimum" .= toJson v]
  "gte" | isNumericKind mk -> ["minimum" .= toJson v]
  "lt" | isNumericKind mk -> ["exclusiveMaximum" .= toJson v]
  "lte" | isNumericKind mk -> ["maximum" .= toJson v]
  "min_items" -> ["minItems" .= toJson v]
  "max_items" -> ["maxItems" .= toJson v]
  "unique" -> ["uniqueItems" .= toJson v]
  "min_pairs" -> ["minProperties" .= toJson v]
  "max_pairs" -> ["maxProperties" .= toJson v]
  _ | Just fmt <- stringFormat name, isStringKind mk -> ["format" .= fmt]
  _ -> []


-- | The @format@ keyword for a string format-flag rule that has a registered
-- OpenAPI/JSON-Schema format.
stringFormat :: Text -> Maybe Text
stringFormat = \case
  "email" -> Just "email"
  "hostname" -> Just "hostname"
  "uri" -> Just "uri"
  "uri_ref" -> Just "uri-reference"
  "ipv4" -> Just "ipv4"
  "ipv6" -> Just "ipv6"
  "uuid" -> Just "uuid"
  _ -> Nothing


isStringKind :: Maybe RuleKind -> Bool
isStringKind = (== Just KString)


isNumericKind :: Maybe RuleKind -> Bool
isNumericKind mk = case mk of
  Just k -> k `elem` numericKinds
  Nothing -> False
  where
    numericKinds =
      [ KFloat, KDouble, KInt32, KInt64, KUint32, KUint64
      , KSint32, KSint64, KFixed32, KFixed64, KSfixed32, KSfixed64
      , KEnum, KDuration, KTimestamp
      ]


-- ---------------------------------------------------------------------------
-- x-protovalidate extension (lossless capture of the rest)
-- ---------------------------------------------------------------------------

{- | The rules NOT rendered as a standard keyword, plus recursive sub-rules,
gathered into the @x-protovalidate@ object so nothing is dropped.
-}
extensionObject :: FieldRules -> [Pair]
extensionObject fr =
  kindPair
    <> rawRulePairs
    <> subRulePairs
    <> predefinedPairs
  where
    kindPair = case frKind fr of
      Just k | not (null leftover) -> ["kind" .= showKind k]
      _ -> []
    leftover = mapMaybe keep (frRules fr)
    keep (n, v)
      | isStandard (frKind fr) n = Nothing
      | otherwise = Just (AKey.fromText n, toJson v)
    rawRulePairs = if null leftover then [] else ["rules" .= Object (AKM.fromList leftover)]
    subRulePairs =
      concat
        [ maybe [] (subPair "items") (frItems fr)
        , maybe [] (subPair "mapKeys") (frMapKeys fr)
        , maybe [] (subPair "mapValues") (frMapValues fr)
        ]
    subPair key r = case subObjectPairs r of
      [] -> []
      ps -> [AKey.fromText key .= Object (AKM.fromList ps)]
    predefinedPairs
      | null (frPredefined fr) = []
      | otherwise = ["predefined" .= Array (V.fromList (fmap predefinedJson (frPredefined fr)))]


-- | The @x-protovalidate@ sub-object pairs for a nested (items / map key /
-- value) rule set: its standard keywords + its own recursive extension +
-- any custom CEL. Empty when the nested rules are vacuous.
subObjectPairs :: FieldRules -> [Pair]
subObjectPairs fr =
  concatMap (standardKeyword (frKind fr)) (frRules fr)
    <> extensionObject fr
    <> (if null (frCustom fr) then [] else [celPair (frCustom fr)])


predefinedJson :: (Constraint, CV.Value) -> Data.Aeson.Value
predefinedJson (con, v) =
  object ["constraint" .= constraintJson con, "rule" .= toJson v]


-- | True if @(kind, ruleName)@ becomes a standard keyword (so it should be
-- omitted from the extension's @rules@).
isStandard :: Maybe RuleKind -> Text -> Bool
isStandard mk name = not (null (standardKeyword mk (name, CV.VBool True)))


showKind :: RuleKind -> Text
showKind = T.toLower . T.pack . drop 1 . show


-- ---------------------------------------------------------------------------
-- CEL
-- ---------------------------------------------------------------------------

-- | An @x-cel@ pair from a list of custom constraints.
celPair :: [Constraint] -> Pair
celPair cs = "x-cel" .= Array (V.fromList (fmap constraintJson cs))


constraintJson :: Constraint -> Data.Aeson.Value
constraintJson con =
  object
    [ "id" .= constraintId con
    , "message" .= constraintMessage con
    , "expression" .= constraintSource con
    ]


-- ---------------------------------------------------------------------------
-- CEL Value → JSON
-- ---------------------------------------------------------------------------

-- | An @enum@ array from an @in@ rule's list value (or a singleton fallback).
enumArray :: CV.Value -> Data.Aeson.Value
enumArray (CV.VList xs) = Array (fmap toJson xs)
enumArray v = Array (V.singleton (toJson v))


-- | Convert a CEL rule 'Value' to a JSON 'Data.Aeson.Value'. Timestamp /
-- Duration render as their canonical string forms (they only appear in
-- @x-protovalidate@, never a standard numeric keyword).
toJson :: CV.Value -> Data.Aeson.Value
toJson = \case
  CV.VNull -> Null
  CV.VBool b -> Bool b
  CV.VInt i -> Number (fromIntegral i)
  CV.VUInt u -> Number (fromIntegral u)
  CV.VDouble d -> Number (fromFloatDigits d)
  CV.VString s -> String s
  CV.VBytes bs -> String (TE.decodeUtf8 (B64.encode bs))
  CV.VList xs -> Array (fmap toJson xs)
  CV.VMap m -> Object (AKM.fromList (fmap kv (CV.celMapEntries m)))
  CV.VType t -> String (T.pack (show t))
  CV.VTimestamp ts -> String (T.pack (show ts))
  CV.VDuration d -> String (T.pack (show d))
  where
    kv (k, v) = (AKey.fromText (keyText k), toJson v)
    keyText (CV.VString s) = s
    keyText (CV.VInt i) = T.pack (show i)
    keyText (CV.VUInt u) = T.pack (show u)
    keyText other = T.pack (show other)
