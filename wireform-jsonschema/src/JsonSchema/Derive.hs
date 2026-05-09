{-# LANGUAGE TemplateHaskell #-}

-- | Annotation-driven Template Haskell deriver for
-- 'JsonSchema.Class.HasJsonSchema'.
--
-- The deriver is the JSON-Schema sibling of @Wireform.Derive.Aeson@:
-- it consumes the same 'Wireform.Derive.Modifier.Modifier' vocabulary
-- (so a single set of @ANN@ pragmas drives Aeson encoding,
-- JsonSchema reflection, and every other wireform format
-- simultaneously) and emits a 'JsonSchema.Class.HasJsonSchema'
-- instance.
--
-- Internally the deriver builds the schema as an 'Aeson.Value' at use
-- time and runs it through @JsonSchema.Parser.parseSchema@. This way
-- it inherits all of the parser's invariants — @schemaRawKeywords@ is
-- populated, the dialect / vocabulary is resolved correctly, and the
-- result drops straight into the runtime validator with no extra
-- bookkeeping.
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

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
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

import JsonSchema.Class (HasJsonSchema (..))
import JsonSchema.Parser (parseSchema)
import JsonSchema.Renderer (renderSchema)

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
--
-- The expression we splice is
--
-- @
-- case 'parseSchema' \<aeson value built from the type\> of
--   Right s -> s
--   Left e  -> error (\"deriveHasJsonSchema: parse failed for T: \" \<\> show e)
-- @
--
-- so the resulting 'Schema' has @schemaRawKeywords@ populated and
-- the runtime validator finds every keyword we emit.
toJsonSchemaBody :: TypeInfo -> Q Exp
toJsonSchemaBody ti = case typeInfoShape ti of
  TypeShapeNewtype con ->
    case conInfoFields con of
      [FieldInfo _ innerTy] ->
        [| toJsonSchema (Proxy :: Proxy $(pure innerTy)) |]
      _ -> fail "deriveHasJsonSchema: newtype must have exactly one field"

  TypeShapeRecord con -> wrap (recordSchemaJSON con)
  TypeShapeEnum cons  -> wrap (enumSchemaJSON cons)
  TypeShapeSum cons   -> wrap (sumSchemaJSON cons)
  where
    -- Wrap a @Q Exp@ producing an Aeson.Value in the parseSchema
    -- pipeline.
    wrap mkValue = do
      v <- mkValue
      [|
          case parseSchema $(pure v) of
            Right s  -> s
            Left err ->
              error
                ("JsonSchema.Derive: parseSchema failed for "
                   <> $(litE (StringL (nameBase (typeInfoName ti))))
                   <> ": " <> show err)
       |]

-- ---------------------------------------------------------------------------
-- Records
-- ---------------------------------------------------------------------------

recordSchemaJSON :: ConInfo -> Q Exp
recordSchemaJSON con = do
  let fields = conInfoFields con
  fieldEntries <- traverse fieldEntry fields
  let propPairsExp = ListE (map fst fieldEntries)
      requiredExp  = ListE (mapMaybe snd fieldEntries)
  [|
      let propsList = $(pure propPairsExp)
          requiredFields = $(pure requiredExp)
          props =
            Aeson.Object $
              KM.fromList
                [ (Key.fromText k, v) | (k, v) <- propsList ]
          base =
            [ (Key.fromText (T.pack "type"),       Aeson.String (T.pack "object"))
            , (Key.fromText (T.pack "properties"), props)
            ]
          required =
            if null requiredFields
              then []
              else
                [ ( Key.fromText (T.pack "required")
                  , Aeson.Array (V.fromList (map Aeson.String requiredFields))
                  )
                ]
       in Aeson.Object (KM.fromList (base ++ required))
   |]
  where
    fieldEntry FieldInfo{fieldInfoName = mName, fieldInfoType = ty} = do
      key <- case mName of
        Just n -> resolveFieldKey n
        Nothing -> fail "deriveHasJsonSchema: positional fields are not \
                        \supported (use a record constructor)"
      keyExp <- [| T.pack key |]
      -- The inner instance for @Maybe a@ widens the @type@ keyword to
      -- @[t, \"null\"]@; pass the field type through unchanged so we
      -- pick that up.
      schemaJsonExp <-
        [| renderSchema (toJsonSchema (Proxy :: Proxy $(pure ty))) |]
      let entry = TupE [Just keyExp, Just schemaJsonExp]
          required = if isMaybeType ty then Nothing else Just keyExp
      pure (entry, required)

resolveFieldKey :: Name -> Q String
resolveFieldKey n = do
  mi <- reifyModifierInfoFor backendJsonSchema n
  let base = nameBase n
  pure $ case miRename mi of
    Just (RenameSpecLiteral t) -> T.unpack t
    Just (RenameSpecStyle st)  -> T.unpack (applyStyle st (T.pack base))
    Just (RenameSpecApply _)   -> base
    Nothing                    -> base

isMaybeType :: Type -> Bool
isMaybeType (AppT (ConT n) _) = n == ''Maybe
isMaybeType _ = False

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

enumSchemaJSON :: [ConInfo] -> Q Exp
enumSchemaJSON cons = do
  let names = map (T.pack . nameBase . conInfoName) cons
  namesExp <- [| names |]
  [|
      let xs = $(pure namesExp)
       in Aeson.Object $ KM.fromList
            [ (Key.fromText (T.pack "type"), Aeson.String (T.pack "string"))
            , ( Key.fromText (T.pack "enum")
              , Aeson.Array (V.fromList (map Aeson.String xs))
              )
            ]
   |]

-- ---------------------------------------------------------------------------
-- Sum types (tagged-object encoding)
-- ---------------------------------------------------------------------------

sumSchemaJSON :: [ConInfo] -> Q Exp
sumSchemaJSON cons = do
  branches <- traverse sumBranch cons
  let listExp = ListE branches
  [|
      let bs = $(pure listExp)
       in Aeson.Object $ KM.fromList
            [ ( Key.fromText (T.pack "oneOf")
              , Aeson.Array (V.fromList bs)
              )
            ]
   |]
  where
    sumBranch :: ConInfo -> Q Exp
    sumBranch ConInfo{conInfoName = nm, conInfoFields = fs} = do
      let tag = T.pack (nameBase nm)
          hasContents = not (null fs)
      tagExp <- [| tag |]
      contentsExp <- contentsSchemaJSON fs
      [|
          let tagSchema =
                Aeson.Object $ KM.fromList
                  [ (Key.fromText (T.pack "type"),  Aeson.String (T.pack "string"))
                  , (Key.fromText (T.pack "const"), Aeson.String $(pure tagExp))
                  ]
              base =
                [ (Key.fromText (T.pack "type"), Aeson.String (T.pack "object"))
                ]
              propsAndRequired
                | hasContents =
                    [ ( Key.fromText (T.pack "properties")
                      , Aeson.Object $ KM.fromList
                          [ (Key.fromText (T.pack "tag"), tagSchema)
                          , (Key.fromText (T.pack "contents"), $(pure contentsExp))
                          ]
                      )
                    , ( Key.fromText (T.pack "required")
                      , Aeson.Array
                          (V.fromList
                             [ Aeson.String (T.pack "tag")
                             , Aeson.String (T.pack "contents")
                             ])
                      )
                    ]
                | otherwise =
                    [ ( Key.fromText (T.pack "properties")
                      , Aeson.Object $ KM.fromList
                          [(Key.fromText (T.pack "tag"), tagSchema)]
                      )
                    , ( Key.fromText (T.pack "required")
                      , Aeson.Array
                          (V.fromList [Aeson.String (T.pack "tag")])
                      )
                    ]
           in Aeson.Object (KM.fromList (base ++ propsAndRequired))
       |]

    contentsSchemaJSON :: [FieldInfo] -> Q Exp
    contentsSchemaJSON [] = [| Aeson.Bool True |]
    contentsSchemaJSON [FieldInfo _ ty] =
      [| renderSchema (toJsonSchema (Proxy :: Proxy $(pure ty))) |]
    contentsSchemaJSON tys = do
      schemas <- traverse (\(FieldInfo _ t) ->
                              [| renderSchema (toJsonSchema (Proxy :: Proxy $(pure t))) |]) tys
      let listExp = ListE schemas
      [|
          let xs = $(pure listExp)
              n  = length xs
           in Aeson.Object $ KM.fromList
                [ ( Key.fromText (T.pack "type")
                  , Aeson.String (T.pack "array")
                  )
                , ( Key.fromText (T.pack "prefixItems")
                  , Aeson.Array (V.fromList xs)
                  )
                , ( Key.fromText (T.pack "minItems"), Aeson.Number (fromIntegral n))
                , ( Key.fromText (T.pack "maxItems"), Aeson.Number (fromIntegral n))
                ]
       |]
