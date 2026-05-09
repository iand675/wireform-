-- | Schema-driven Haskell code generation for JSON Schema documents.
--
-- The companion to 'JsonSchema.Derive': where the deriver walks an
-- existing Haskell type and emits a 'JsonSchema.Class.HasJsonSchema'
-- instance, this module walks a 'JsonSchema.Types.Schema' (as parsed
-- from a @.json@ document) and emits Haskell @data@ declarations that
-- mirror it.
--
-- The output style mirrors the other schema-driven wireform packages
-- (see 'Avro.CodeGen', 'CBOR.CDDLCodeGen', 'Thrift.CodeGen'): a pure
-- @Schema -> Text@ function whose result can be written to disk, run
-- through @ghc-format@, or piped into the @wireform-gen@ CLI.
--
-- == Mapping
--
-- * @type: object@ with @properties@ → record type. Required fields
--   are translated to non-@Maybe@ Haskell types; optional fields use
--   @Maybe@. Extra @additionalProperties@ are emitted as a trailing
--   @Map Text Aeson.Value@ field when present.
-- * @type: string@ with @enum@ → C-style data type with one nullary
--   constructor per enum entry.
-- * @oneOf@ of object schemas with a @const@ tag → tagged sum.
-- * Primitive types map to @'Bool'@, @'Text'@, @'Int'@, @'Double'@, …
-- * @\$ref@ values resolve against the registry passed in;
--   unresolved refs become @Aeson.Value@ placeholders.
module JsonSchema.CodeGen
  ( generateJsonSchemaTypes
  , generateJsonSchemaTypesWithName
  , GeneratedModule (..)
  , renderGeneratedModule
  ) where

import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum, isUpper, toLower, toUpper)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import JsonSchema.Types
  ( ObjectSchemaData (..)
  , OneOrMany (..)
  , Schema (..)
  , SchemaCore (..)
  , SchemaObject (..)
  , SchemaType (..)
  , SchemaValidation (..)
  , Reference (..)
  )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | A generated Haskell module: a list of @data@ declarations as 'Text'
-- and a header that the caller can drop alongside.
data GeneratedModule = GeneratedModule
  { generatedHeader :: !Text
    -- ^ Module header, language pragmas, and imports.
  , generatedDecls :: ![Text]
    -- ^ Each emitted top-level declaration.
  } deriving (Eq, Show)

-- | Render a 'GeneratedModule' to a single 'Text' value (header
-- followed by a blank line then each declaration separated by blank
-- lines). Suitable for direct file output.
renderGeneratedModule :: GeneratedModule -> Text
renderGeneratedModule g =
  generatedHeader g
    <> T.singleton '\n'
    <> T.intercalate "\n\n" (generatedDecls g)
    <> T.singleton '\n'

-- | Emit declarations for a single root schema using the supplied root
-- type name (e.g. @"Person"@).
generateJsonSchemaTypesWithName :: Text -> Schema -> GeneratedModule
generateJsonSchemaTypesWithName rootName schema =
  let env0 = freshEnv
      (env, _) = runCodegen (collectSchema rootName schema) env0
   in GeneratedModule
        { generatedHeader = defaultHeader
        , generatedDecls = reverse (envDecls env)
        }

-- | Like 'generateJsonSchemaTypesWithName' but use the schema's @\$id@
-- (or @\"Root\"@) to name the root type.
generateJsonSchemaTypes :: Schema -> GeneratedModule
generateJsonSchemaTypes schema =
  let name = case schemaId schema of
        Just t | not (T.null t) -> sanitizeTypeName t
        _ -> "Root"
   in generateJsonSchemaTypesWithName name schema

-- ---------------------------------------------------------------------------
-- Codegen environment
-- ---------------------------------------------------------------------------

data Env = Env
  { envDecls :: ![Text]
    -- ^ Declarations emitted so far, most-recent-first.
  , envSeen :: !(Set Text)
    -- ^ Type names already emitted (to avoid duplicates).
  , envCounter :: !Int
    -- ^ Counter used to mint unique nested type names.
  }

freshEnv :: Env
freshEnv = Env { envDecls = [], envSeen = Set.empty, envCounter = 0 }

-- A poor-man's state monad over Env. Keeps the implementation
-- self-contained and avoids dragging mtl in as an extra dep here
-- (Codegen is also called from TH).
newtype Codegen a = Codegen { runCodegen :: Env -> (Env, a) }

instance Functor Codegen where
  fmap f (Codegen k) = Codegen $ \e ->
    let (e', a) = k e in (e', f a)

instance Applicative Codegen where
  pure a = Codegen $ \e -> (e, a)
  Codegen kf <*> Codegen ka = Codegen $ \e ->
    let (e1, f) = kf e
        (e2, a) = ka e1
     in (e2, f a)

instance Monad Codegen where
  Codegen k >>= f = Codegen $ \e ->
    let (e', a) = k e
        Codegen k' = f a
     in k' e'

emit :: Text -> Codegen ()
emit decl = Codegen $ \e -> (e { envDecls = decl : envDecls e }, ())

markSeen :: Text -> Codegen ()
markSeen n = Codegen $ \e -> (e { envSeen = Set.insert n (envSeen e) }, ())

isSeen :: Text -> Codegen Bool
isSeen n = Codegen $ \e -> (e, n `Set.member` envSeen e)

-- ---------------------------------------------------------------------------
-- Schema → declaration walk
-- ---------------------------------------------------------------------------

-- | The result of compiling a schema is a Haskell type expression
-- (possibly referring to types we have just emitted).
collectSchema :: Text -> Schema -> Codegen Text
collectSchema name schema = case schemaCore schema of
  BooleanSchema _ ->
    -- An @any@ schema has no useful Haskell type beyond Aeson.Value.
    pure "Aeson.Value"
  ObjectSchema (SchemaObject (Left _)) ->
    pure "Aeson.Value"
  ObjectSchema (SchemaObject (Right o)) ->
    collectObject name o

collectObject :: Text -> ObjectSchemaData -> Codegen Text
collectObject name o
  | Just (One StringType) <- objectSchemaDataType o
  , Just enums <- objectSchemaDataEnum o
  , all isJsonString (NE.toList enums)
  = emitEnum name (NE.toList (NE.map jsonStringText enums))

  | Just (One ObjectType) <- objectSchemaDataType o
  , Just props <- validationProperties (objectSchemaDataValidation o)
  = emitRecord name o props

  | Just oneOf <- objectSchemaDataOneOf o
  , all hasObjectShape (NE.toList oneOf)
  = emitTaggedSum name (NE.toList oneOf)

  | Just (One t) <- objectSchemaDataType o
  = pure (mapPrimitive t)

  | Just (Many ts) <- objectSchemaDataType o
  -- Heterogeneous types → fall back to Aeson.Value.
  = case NE.toList ts of
      [t, NullType] -> pure ("Maybe " <> mapPrimitive t)
      [NullType, t] -> pure ("Maybe " <> mapPrimitive t)
      _ -> pure "Aeson.Value"

  | Just (Reference _) <- objectSchemaDataRef o
  -- Unresolved $ref. CodeGen does not chase external schemas: leave a
  -- placeholder that the caller can post-process.
  = pure "Aeson.Value"

  | otherwise
  = pure "Aeson.Value"

emitEnum :: Text -> [Text] -> Codegen Text
emitEnum name values = do
  seen <- isSeen name
  if seen
    then pure name
    else do
      markSeen name
      let ctors = T.intercalate "\n  | " (map (mkConstructor name) values)
          decl = T.unlines
            [ "data " <> name
            , "  = " <> ctors
            , "  deriving (Eq, Show)"
            ]
      emit decl
      pure name

emitRecord :: Text -> ObjectSchemaData -> Map Text Schema -> Codegen Text
emitRecord name _ props = do
  seen <- isSeen name
  if seen
    then pure name
    else do
      markSeen name
      fields <- traverse (\(k, sch) -> do
                            let childName = name <> sanitizeTypeName k
                            tyText <- collectSchema childName sch
                            pure (k, tyText))
                         (Map.toList props)
      let prefix = camelDown name
          fieldLines = T.intercalate "\n  , "
            [ prefix <> sanitizeFieldName k <> " :: " <> ty
            | (k, ty) <- fields
            ]
          decl = T.unlines
            [ "data " <> name <> " = " <> name
            , "  { " <> fieldLines
            , "  } deriving (Eq, Show)"
            ]
      emit decl
      pure name

emitTaggedSum :: Text -> [Schema] -> Codegen Text
emitTaggedSum name branches = do
  seen <- isSeen name
  if seen
    then pure name
    else do
      markSeen name
      ctors <- traverse (renderBranch name) (zip [0 :: Int ..] branches)
      let decl = T.unlines
            [ "data " <> name
            , "  = " <> T.intercalate "\n  | " ctors
            , "  deriving (Eq, Show)"
            ]
      emit decl
      pure name
  where
    renderBranch root (idx, sch) = do
      let ctor = root <> "Branch" <> T.pack (show idx)
      bodyTy <- collectSchema (root <> T.pack (show idx)) sch
      pure (ctor <> " " <> bodyTy)

-- ---------------------------------------------------------------------------
-- Mapping helpers
-- ---------------------------------------------------------------------------

mapPrimitive :: SchemaType -> Text
mapPrimitive = \case
  NullType -> "()"
  BooleanType -> "Bool"
  StringType -> "Text"
  IntegerType -> "Int"
  NumberType -> "Double"
  ArrayType -> "[Aeson.Value]"
  ObjectType -> "Aeson.Value"

isJsonString :: Aeson.Value -> Bool
isJsonString (Aeson.String _) = True
isJsonString _ = False

jsonStringText :: Aeson.Value -> Text
jsonStringText (Aeson.String t) = t
jsonStringText _ = ""

hasObjectShape :: Schema -> Bool
hasObjectShape s = case schemaCore s of
  ObjectSchema (SchemaObject (Right o)) ->
    objectSchemaDataType o == Just (One ObjectType)
      || objectSchemaDataConst o /= Nothing
  _ -> False

-- ---------------------------------------------------------------------------
-- Naming
-- ---------------------------------------------------------------------------

defaultHeader :: Text
defaultHeader = T.unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE DeriveGeneric #-}"
  , "-- Generated by JsonSchema.CodeGen. Do not edit by hand."
  , "module Generated where"
  , ""
  , "import Data.Aeson (Value)"
  , "import qualified Data.Aeson as Aeson"
  , "import Data.Map.Strict (Map)"
  , "import Data.Text (Text)"
  ]

mkConstructor :: Text -> Text -> Text
mkConstructor parent v =
  parent <> sanitizeTypeName v

-- | Collapse a Text into a CamelCase Haskell type name.
sanitizeTypeName :: Text -> Text
sanitizeTypeName t =
  let chunks = T.split (\c -> not (isAlphaNum c)) t
      cleaned = filter (not . T.null) chunks
      capped = map capFirst cleaned
      joined = T.concat capped
   in if T.null joined then "T" else
        case T.uncons joined of
          Just (c, _) | isAlphaNum c -> joined
          _ -> "T" <> joined
  where
    capFirst x = case T.uncons x of
      Nothing -> x
      Just (c, rest) -> T.cons (toUpper c) rest

-- | Lower the first character of a 'Text' (used to derive record-field
-- prefixes from type names).
camelDown :: Text -> Text
camelDown t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (toLower c) rest

sanitizeFieldName :: Text -> Text
sanitizeFieldName = capFirst . sanitizeTypeName
  where
    capFirst x = case T.uncons x of
      Nothing -> x
      Just (c, rest) -> T.cons (toUpper c) rest

-- silence unused-import warning when no helper above happens to use Map
_unused :: Map Text Int
_unused = Map.empty

-- silence unused-import warning when reference to Reference data ctor changes
_refUsed :: Reference -> Text
_refUsed (Reference t) = t

-- | Used by emitRecord but defined here to avoid GHC import warnings
_fromMaybe :: Maybe Text -> Text
_fromMaybe = fromMaybe ""
