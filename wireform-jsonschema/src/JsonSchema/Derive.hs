{-# LANGUAGE TemplateHaskell #-}

-- | Annotation-driven Template Haskell deriver for
-- 'JsonSchema.Class.HasJsonSchema'.
--
-- The deriver is the JSON-Schema sibling of @Wireform.Derive.Aeson@:
-- it consumes the same 'Wireform.Derive.Modifier.Modifier' vocabulary
-- (so a single set of @ANN@ pragmas drives Aeson encoding,
-- JsonSchema reflection, and every other wireform format simultaneously)
-- and emits a 'JsonSchema.Class.HasJsonSchema' instance.
--
-- == Quick start
--
-- @
-- data Person = Person
--   { personName :: !Text
--   , personAge  :: !Int
--   } deriving (Show)
--
-- {-\# ANN type Person   ('Wireform.Derive.rename' \"person\")        \#-}
-- {-\# ANN personName    ('Wireform.Derive.rename' \"name\")          \#-}
-- {-\# ANN personAge     ('Wireform.Derive.renameStyle' SnakeCase)  \#-}
--
-- 'deriveHasJsonSchema' \'\'Person
-- @
--
-- == Type-shape support
--
-- * 'TypeShapeNewtype' — reuse the inner instance.
-- * 'TypeShapeRecord'  — emit an @object@ schema with each field's schema
--   under @properties@. Required fields default to those whose Haskell
--   type is not @Maybe@.
-- * 'TypeShapeEnum'    — emit a @string@ schema with @enum@ listing each
--   constructor name (rename modifiers applied).
-- * 'TypeShapeSum'     — emit a @oneOf@ of the constructors using the
--   tagged-object encoding @{ \"tag\": \"Ctor\", \"contents\": ... }@,
--   matching @Wireform.Derive.Aeson@'s default.
module JsonSchema.Derive
  ( deriveHasJsonSchema
  ) where

import Control.Monad (foldM, unless)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH

import Wireform.Derive.Backend (backendJsonSchema)
import Wireform.Derive.ModifierInfo
  ( ModifierInfo (..)
  , RenameSpec (..)
  , reifyModifierInfoFor
  )
import Wireform.Derive.NameStyle (applyStyle)
import Wireform.Derive.TypeInfo
  ( ConInfo (..)
  , FieldInfo (..)
  , TypeInfo (..)
  , TypeShape (..)
  , reifyTypeInfo
  )

import JsonSchema.Class (HasJsonSchema (..), primitiveSchema, withValidation)
import JsonSchema.Types
  ( ArrayItemsValidation (..)
  , ObjectSchemaData (..)
  , OneOrMany (..)
  , Schema (..)
  , SchemaCore (..)
  , SchemaObject (..)
  , SchemaType (..)
  , SchemaValidation (..)
  , booleanSchema
  , defaultObjectSchemaData
  , defaultSchemaValidation
  , objectSchema
  )

import qualified Data.Aeson as Aeson

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Derive a 'HasJsonSchema' instance for the given type.
deriveHasJsonSchema :: Name -> Q [Dec]
deriveHasJsonSchema nm = do
  ti <- reifyTypeInfo nm
  body <- toJsonSchemaBody ti
  let typeHead = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''HasJsonSchema) typeHead)
          [FunD 'toJsonSchema [Clause [WildP] (NormalB body) []]]
  pure [decl]

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT

-- ---------------------------------------------------------------------------
-- Body construction
-- ---------------------------------------------------------------------------

-- | Build the @Q Exp@ for @'toJsonSchema' \_ = ...@ depending on the
-- shape of the data declaration.
toJsonSchemaBody :: TypeInfo -> Q Exp
toJsonSchemaBody ti = case typeInfoShape ti of
  TypeShapeNewtype con ->
    case conInfoFields con of
      [FieldInfo _ innerTy] ->
        [| toJsonSchema (Proxy :: Proxy $(pure innerTy)) |]
      _ -> fail "deriveHasJsonSchema: newtype must have exactly one field"

  TypeShapeRecord con -> recordSchema con

  TypeShapeEnum cons -> enumSchema cons

  TypeShapeSum cons -> sumSchema cons

-- ---------------------------------------------------------------------------
-- Records
-- ---------------------------------------------------------------------------

recordSchema :: ConInfo -> Q Exp
recordSchema con = do
  let fields = conInfoFields con
  fieldExprs <- traverse fieldEntry fields
  let propsExp = ListE (map fst fieldExprs)
      requiredExp = ListE (mapMaybe snd fieldExprs)
  [|
      let props = Map.fromList $(pure propsExp)
          req = $(pure requiredExp)
          v = defaultSchemaValidation
                { validationProperties = Just props
                , validationRequired =
                    if null req then Nothing else Just req
                }
       in objectSchema $ defaultObjectSchemaData
            { objectSchemaDataType = Just (One ObjectType)
            , objectSchemaDataValidation = v
            }
   |]
  where
    fieldEntry :: FieldInfo -> Q (Exp, Maybe Exp)
    fieldEntry FieldInfo{fieldInfoName = mName, fieldInfoType = ty} = do
      key <- case mName of
        Just n -> resolveFieldKey n
        Nothing ->
          fail "deriveHasJsonSchema: positional fields are not supported \
                \(use a record constructor)"
      let keyT = T.pack key
      keyExp <- [| T.pack key |]
      schemaExp <- [| toJsonSchema (Proxy :: Proxy $(pure ty)) |]
      let entry = TupE [Just keyExp, Just schemaExp]
          required = if isMaybeType ty then Nothing else Just keyExp
      pure (entry, required)

-- | Resolve a field selector to its wire key under the JSON-Schema
-- backend. Falls back to the field's selector base name if no
-- 'rename' modifier applies.
resolveFieldKey :: Name -> Q String
resolveFieldKey n = do
  mi <- reifyModifierInfoFor backendJsonSchema n
  let base = nameBase n
  pure $ case miRename mi of
    Just (RenameSpecLiteral t) -> T.unpack t
    Just (RenameSpecStyle st) -> T.unpack (applyStyle st (T.pack base))
    Just (RenameSpecApply _) -> base
    Nothing -> base

-- | Whether a Haskell type is a @Maybe a@. Required-field detection
-- treats @Maybe@-typed fields as optional in the JSON Schema.
isMaybeType :: Type -> Bool
isMaybeType (AppT (ConT n) _) = n == ''Maybe
isMaybeType _ = False

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

enumSchema :: [ConInfo] -> Q Exp
enumSchema cons = do
  let names = map (T.pack . nameBase . conInfoName) cons
  namesExp <- [| names |]
  [|
      let valuesNE = case NE.nonEmpty (map Aeson.String $(pure namesExp)) of
            Nothing -> error "deriveHasJsonSchema: enum has no constructors"
            Just ne -> ne
       in objectSchema $ defaultObjectSchemaData
            { objectSchemaDataType = Just (One StringType)
            , objectSchemaDataEnum = Just valuesNE
            }
   |]

-- ---------------------------------------------------------------------------
-- Tagged sum types
-- ---------------------------------------------------------------------------

sumSchema :: [ConInfo] -> Q Exp
sumSchema cons = do
  branchExps <- traverse sumBranch cons
  let listExp = ListE branchExps
  [|
      let branches = $(pure listExp)
          oneOfNE = case NE.nonEmpty branches of
            Nothing -> error "deriveHasJsonSchema: sum type has no constructors"
            Just ne -> ne
       in objectSchema $ defaultObjectSchemaData
            { objectSchemaDataOneOf = Just oneOfNE }
   |]
  where
    sumBranch :: ConInfo -> Q Exp
    sumBranch ConInfo{conInfoName = nm, conInfoFields = fs} = do
      let tag = T.pack (nameBase nm)
          hasContents = not (null fs)
      tagSchemaExp <- [|
          objectSchema defaultObjectSchemaData
            { objectSchemaDataType = Just (One StringType)
            , objectSchemaDataConst = Just (Aeson.String tag)
            }
        |]
      contentsSchemaExp <- contentsSchema fs
      let mkPropsExp =
            if hasContents
              then [| Map.fromList
                        [ (T.pack "tag",      $(pure tagSchemaExp))
                        , (T.pack "contents", $(pure contentsSchemaExp))
                        ] |]
              else [| Map.fromList
                        [ (T.pack "tag", $(pure tagSchemaExp)) ] |]
          mkRequiredExp =
            if hasContents
              then [| [T.pack "tag", T.pack "contents"] |]
              else [| [T.pack "tag"] |]
      [|
          let propsMap = $mkPropsExp
              requiredKeys = $mkRequiredExp
              v = defaultSchemaValidation
                    { validationProperties = Just propsMap
                    , validationRequired = Just requiredKeys
                    }
           in objectSchema defaultObjectSchemaData
                { objectSchemaDataType = Just (One ObjectType)
                , objectSchemaDataValidation = v
                }
       |]

    contentsSchema :: [FieldInfo] -> Q Exp
    contentsSchema [] = [| booleanSchema True |]
    contentsSchema [FieldInfo _ t] =
      [| toJsonSchema (Proxy :: Proxy $(pure t)) |]
    contentsSchema fs = do
      schemas <- traverse (\(FieldInfo _ t) ->
                              [| toJsonSchema (Proxy :: Proxy $(pure t)) |]) fs
      let listSchemas = ListE schemas
      [|
          let arrSchemas = $(pure listSchemas)
              prefixNE = case NE.nonEmpty arrSchemas of
                Nothing -> error "deriveHasJsonSchema: empty constructor"
                Just ne -> ne
              v = defaultSchemaValidation
                    { validationPrefixItems = Just prefixNE
                    , validationMinItems = Just (fromIntegral (length arrSchemas))
                    , validationMaxItems = Just (fromIntegral (length arrSchemas))
                    }
           in objectSchema defaultObjectSchemaData
                { objectSchemaDataType = Just (One ArrayType)
                , objectSchemaDataValidation = v
                }
       |]
