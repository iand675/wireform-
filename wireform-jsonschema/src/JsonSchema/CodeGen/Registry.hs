{-# LANGUAGE LambdaCase #-}

-- | A registry of mappings from JSON Schema fragments to Haskell types.
--
-- The lookup vocabulary is a composable 'Matcher' algebra rather than
-- a fixed set of axes, so the same registry can answer "this Haskell
-- type owns the @date-time@ format", "the @\$ref@
-- @https://example.com/schemas/User@ resolves to my @User@ type",
-- "any string whose @pattern@ keyword starts with @\\^[0-9a-f]@ should
-- map to a @ByteString@", and so on.
--
-- Two consumers share this registry:
--
-- * 'JsonSchema.CodeGen.generateJsonSchemaTypesWith' — when emitting
--   Haskell from a parsed schema, the generator consults the registry
--   before falling back to a fresh data declaration.
-- * 'JsonSchema.Class.Instances' — pre-built 'HasJsonSchema' instances
--   that drive a registered format / pattern back out when a
--   registered Haskell type is reflected.
--
-- == Quick reference
--
-- @
-- myReg :: 'MappingRegistry'
-- myReg =
--     'register' ('matchFormat' \"date-time\")
--                 ('typeMapping' \"UTCTime\" [\"Data.Time (UTCTime)\"])
--   $ 'register' ('matchRef' \"https://example.com/schemas/User\")
--                 ('typeMapping' \"User\" [\"Acme.User (User)\"])
--   $ 'register' ('matchAll'
--                   [ 'matchType' StringType
--                   , 'matchPatternRegex' \"^[0-9a-fA-F]{64}\$\"
--                   ])
--                 ('typeMapping' \"Sha256\" [\"Acme.Crypto (Sha256)\"])
--   $ 'emptyMappingRegistry'
-- @
module JsonSchema.CodeGen.Registry
  ( -- * Type mappings
    TypeMapping (..)
  , typeMapping

    -- * Matchers
  , Matcher
  , matchFormat
  , matchPattern
  , matchPatternRegex
  , matchRef
  , matchDynamicRef
  , matchId
  , matchType
  , matchEnum
  , matchHasProperty
  , matchAll
  , matchAny
  , matchNot
  , matchPredicate
  , matchAnything

    -- * Registry
  , MappingRegistry
  , Rule
  , emptyMappingRegistry
  , defaultMappingRegistry
  , register

    -- * Lookup
  , lookupMapping
  , runMatcher

    -- * Helpers used by the code generator
  , registryImports
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified JsonSchema.Regex as Re

import JsonSchema.Types
  ( Format (..)
  , ObjectSchemaData (..)
  , OneOrMany (..)
  , Reference (..)
  , Regex (..)
  , Schema (..)
  , SchemaCore (..)
  , SchemaObject (..)
  , SchemaType (..)
  , SchemaValidation (..)
  )

-- ---------------------------------------------------------------------------
-- TypeMapping
-- ---------------------------------------------------------------------------

-- | Description of the Haskell type to use in place of a matched
-- schema, plus the imports needed to make that type accessible.
--
-- 'tmHaskellImports' entries are bare module specifications without
-- the leading @import@ keyword (e.g. @\"Data.Time (UTCTime)\"@). The
-- generator emits @import \<spec\>@ lines from them, deduping across
-- the whole module.
data TypeMapping = TypeMapping
  { tmHaskellType    :: !Text
    -- ^ The Haskell type expression to splice in (e.g. @\"UTCTime\"@,
    -- @\"V.Vector ByteString\"@).
  , tmHaskellImports :: ![Text]
    -- ^ Import specifications, one per line, sans the @import@
    -- keyword. May be qualified
    -- (@\"qualified Data.Vector as V\"@).
  } deriving (Eq, Show)

-- | Smart constructor for 'TypeMapping'. Stylistic — same as the
-- record constructor.
typeMapping :: Text -> [Text] -> TypeMapping
typeMapping = TypeMapping

-- ---------------------------------------------------------------------------
-- Matchers
-- ---------------------------------------------------------------------------

-- | A first-class predicate over a 'Schema'. Evaluating one is pure
-- and total ('runMatcher'); the algebra closes over conjunction,
-- disjunction, and negation, so registered rules can express
-- structural shape ("string with this format AND this pattern")
-- without dropping to a lambda.
data Matcher
  = MFormat !Text
    -- ^ Schema's @format@ keyword equals this value (matched on the
    -- canonical spec spelling, e.g. @\"date-time\"@).
  | MPattern !Text
    -- ^ Schema's @pattern@ keyword equals this exact string.
  | MPatternRegex !Text
    -- ^ Schema's @pattern@ keyword regex-matches this (ECMA-262)
    -- pattern.
  | MRef !Text
    -- ^ Schema's @$ref@ equals this URI.
  | MDynamicRef !Text
    -- ^ Schema's @$dynamicRef@ equals this URI.
  | MId !Text
    -- ^ Schema's @$id@ equals this URI.
  | MType !SchemaType
    -- ^ Schema's @type@ keyword is exactly this primitive type, or
    -- a union containing it.
  | MEnum ![Aeson.Value]
    -- ^ Schema's @enum@ keyword equals this list (order-insensitive).
  | MHasProperty !Text !(Maybe Matcher)
    -- ^ Schema's @properties@ contains the named property; the
    -- optional inner matcher is then run against that property's
    -- subschema.
  | MAll ![Matcher]
  | MAny ![Matcher]
  | MNot !Matcher
  | MAnything
    -- ^ Always matches. Useful as the fallback in 'matchAny' or
    -- when registering a default type.
  | MPredicate !Text !(Schema -> Bool)
    -- ^ Escape hatch. The 'Text' label is purely cosmetic but lets
    -- the registry's 'Show' output stay informative.

instance Show Matcher where
  show = \case
    MFormat t          -> "MFormat "        <> show t
    MPattern t         -> "MPattern "       <> show t
    MPatternRegex t    -> "MPatternRegex "  <> show t
    MRef t             -> "MRef "           <> show t
    MDynamicRef t      -> "MDynamicRef "    <> show t
    MId t              -> "MId "            <> show t
    MType ty           -> "MType "          <> show ty
    MEnum vs           -> "MEnum "          <> show vs
    MHasProperty p m   -> "MHasProperty "   <> show p <> " " <> show m
    MAll ms            -> "MAll "           <> show ms
    MAny ms            -> "MAny "           <> show ms
    MNot m             -> "MNot ("          <> show m <> ")"
    MAnything          -> "MAnything"
    MPredicate lbl _   -> "MPredicate "     <> show lbl

-- | Match if the schema's @format@ keyword is exactly this.
matchFormat :: Text -> Matcher
matchFormat = MFormat

-- | Match if the schema's @pattern@ keyword equals this regex
-- verbatim. Use 'matchPatternRegex' to instead match the @pattern@
-- value /against/ a regex.
matchPattern :: Text -> Matcher
matchPattern = MPattern

-- | Match if the schema has a @pattern@ keyword and that pattern's
-- string representation is matched (anchored anywhere) by the given
-- ECMA-262 regex. Powered by the @ecma262-regex@ engine.
matchPatternRegex :: Text -> Matcher
matchPatternRegex = MPatternRegex

-- | Match a specific @$ref@ URI.
matchRef :: Text -> Matcher
matchRef = MRef

-- | Match a specific @$dynamicRef@ URI.
matchDynamicRef :: Text -> Matcher
matchDynamicRef = MDynamicRef

-- | Match a specific @$id@ URI. Useful for naming a top-level schema
-- from a registry by its canonical identifier.
matchId :: Text -> Matcher
matchId = MId

-- | Match if the schema's @type@ keyword either is this single type
-- or contains it as part of a union.
matchType :: SchemaType -> Matcher
matchType = MType

-- | Match if the schema's @enum@ keyword equals the given values
-- (order-insensitive).
matchEnum :: [Aeson.Value] -> Matcher
matchEnum = MEnum

-- | Match if the schema declares a property of the given name. With
-- @Just inner@, that property's subschema must additionally satisfy
-- @inner@.
matchHasProperty :: Text -> Maybe Matcher -> Matcher
matchHasProperty = MHasProperty

-- | Conjunction. An empty list is always true.
matchAll :: [Matcher] -> Matcher
matchAll = MAll

-- | Disjunction. An empty list is always false.
matchAny :: [Matcher] -> Matcher
matchAny = MAny

-- | Negation.
matchNot :: Matcher -> Matcher
matchNot = MNot

-- | Match every schema.
matchAnything :: Matcher
matchAnything = MAnything

-- | Lift a Haskell predicate. The 'Text' label is shown by 'Show'
-- so registry dumps stay informative.
matchPredicate :: Text -> (Schema -> Bool) -> Matcher
matchPredicate = MPredicate

-- | Run a 'Matcher' against a 'Schema'. The implementation is pure
-- and total; only the @MPredicate@ escape hatch can do anything
-- exotic.
runMatcher :: Matcher -> Schema -> Bool
runMatcher m schema = case m of
  MAll ms          -> all (`runMatcher` schema) ms
  MAny ms          -> any (`runMatcher` schema) ms
  MNot inner       -> not (runMatcher inner schema)
  MAnything        -> True
  MPredicate _ p   -> p schema
  _                -> withObjectData schema (atomicMatch m)
  where
    atomicMatch m' o = case m' of
      MFormat name ->
        case validationFormat (objectSchemaDataValidation o) of
          Just f  -> formatToText f == name
          Nothing -> False

      MPattern p ->
        case validationPattern (objectSchemaDataValidation o) of
          Just (Regex r) -> r == p
          Nothing -> False

      MPatternRegex re ->
        case validationPattern (objectSchemaDataValidation o) of
          Just (Regex r) -> regexMatches re r
          Nothing -> False

      MRef ref ->
        case objectSchemaDataRef o of
          Just (Reference r) -> r == ref
          Nothing -> False

      MDynamicRef ref ->
        case objectSchemaDataDynamicRef o of
          Just (Reference r) -> r == ref
          Nothing -> False

      MId i ->
        schemaId schema == Just i

      MType ty ->
        case objectSchemaDataType o of
          Just (One t)  -> t == ty
          Just (Many ts) -> ty `elem` NE.toList ts
          Nothing -> False

      MEnum vs ->
        case objectSchemaDataEnum o of
          Just ne -> sortedJSON (NE.toList ne) == sortedJSON vs
          Nothing -> False

      MHasProperty propName mInner ->
        case validationProperties (objectSchemaDataValidation o) of
          Just props ->
            case Map.lookup propName props of
              Nothing -> False
              Just sub -> case mInner of
                Nothing -> True
                Just inner -> runMatcher inner sub
          Nothing -> False

      _ -> False  -- handled above

    withObjectData s f = case schemaCore s of
      ObjectSchema (SchemaObject (Right o)) -> f o
      _ -> False

    sortedJSON :: [Aeson.Value] -> [Aeson.Value]
    sortedJSON = map id . Set.toAscList . Set.fromList . map valueWrap
      where
        valueWrap = id

    regexMatches :: Text -> Text -> Bool
    regexMatches pat input =
      case Re.compileText pat [] of
        Left _ -> False
        Right re -> Re.test re (encodeUtf8 input)

-- ---------------------------------------------------------------------------
-- Registry
-- ---------------------------------------------------------------------------

-- | A single registry entry: a 'Matcher' plus the 'TypeMapping' to
-- emit when the matcher fires.
data Rule = Rule !Matcher !TypeMapping
  deriving Show

-- | A registry is an ordered list of 'Rule's. 'lookupMapping' returns
-- the first match, so register more specific rules before more
-- general ones (e.g. a @\$ref@ rule before a fall-through @MType@
-- rule).
newtype MappingRegistry = MappingRegistry { regRules :: [Rule] }
  deriving Show

emptyMappingRegistry :: MappingRegistry
emptyMappingRegistry = MappingRegistry []

-- | Push a rule onto the front of the registry. Lookups try rules in
-- the order returned by @register@-chained calls (most-recently
-- registered first).
register :: Matcher -> TypeMapping -> MappingRegistry -> MappingRegistry
register m tm (MappingRegistry rs) = MappingRegistry (Rule m tm : rs)

-- | A registry pre-populated with mappings for the common JSON Schema
-- formats. Each mapping references types from the standard
-- @time@ / @uuid@ / @network-uri@ ecosystem; you only need to add
-- those packages to @build-depends@ if the registry is in play.
--
-- Pull in 'emptyMappingRegistry' instead if you want to start from
-- scratch (no surprise imports).
defaultMappingRegistry :: MappingRegistry
defaultMappingRegistry =
    register (matchFormat "date-time")
             (typeMapping "UTCTime"   ["Data.Time (UTCTime)"])
  $ register (matchFormat "date")
             (typeMapping "Day"       ["Data.Time.Calendar (Day)"])
  $ register (matchFormat "time")
             (typeMapping "TimeOfDay" ["Data.Time.LocalTime (TimeOfDay)"])
  $ register (matchFormat "duration")
             (typeMapping "NominalDiffTime"
                                       ["Data.Time.Clock (NominalDiffTime)"])
  $ register (matchFormat "uuid")
             (typeMapping "UUID"      ["Data.UUID (UUID)"])
  $ register (matchAny [ matchFormat "uri"
                       , matchFormat "uri-reference"
                       , matchFormat "iri"
                       , matchFormat "iri-reference"
                       ])
             (typeMapping "URI"       ["Network.URI (URI)"])
  $ register (matchFormat "regex")
             (typeMapping "Text"      ["Data.Text (Text)"])
  $ register (matchFormat "json-pointer")
             (typeMapping "JsonPointer" ["JsonPointer (JsonPointer)"])
  $ emptyMappingRegistry

-- ---------------------------------------------------------------------------
-- Lookup
-- ---------------------------------------------------------------------------

-- | Find the first registered 'TypeMapping' whose 'Matcher' fires
-- against the given schema.
lookupMapping :: MappingRegistry -> Schema -> Maybe TypeMapping
lookupMapping (MappingRegistry rs) schema =
  case [ tm | Rule m tm <- rs, runMatcher m schema ] of
    (tm:_) -> Just tm
    []     -> Nothing

-- | Every import declaration referenced by the registry (without the
-- leading @import@ keyword), deduped. The generator uses this when it
-- consults a registry to know which @import@ lines to add to the
-- header.
registryImports :: MappingRegistry -> Set Text
registryImports (MappingRegistry rs) =
  Set.fromList [ imp | Rule _ tm <- rs, imp <- tmHaskellImports tm ]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

formatToText :: Format -> Text
formatToText f = case Aeson.toJSON f of
  Aeson.String t -> t
  _              -> ""

-- silence unused-import warning when 'T' is the only unused alias
_unused :: Text
_unused = T.empty
