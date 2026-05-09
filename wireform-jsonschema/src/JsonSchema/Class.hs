{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reflect a Haskell type into a JSON Schema 'Schema'.
--
-- Mirrors the @<Format>.Class@ surface every other wireform format
-- ships ('Avro.Class', 'CBOR.Class', 'Thrift.Class', …) — pick the
-- typeclass that the deriver in 'JsonSchema.Derive' targets.
--
-- Usage: derive an instance with TH or write one by hand.
--
-- @
-- data Person = Person { name :: !Text, age :: !Int } deriving (Generic)
-- 'JsonSchema.Derive.deriveHasJsonSchema' \'\'Person
--
-- jsonSchemaJSON \@Person   -- :: 'Aeson.Value'
-- @
module JsonSchema.Class
  ( HasJsonSchema (..)
  , jsonSchemaJSON
  , validateAgainstSchema
  , Proxy (..)
    -- * Builder helpers
  , primitiveSchema
  , withValidation
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Numeric.Natural (Natural)

import JsonSchema.Renderer (renderSchema)
import JsonSchema.Types
  ( Schema (..)
  , SchemaCore (..)
  , SchemaObject (..)
  , ObjectSchemaData (..)
  , SchemaType (..)
  , OneOrMany (..)
  , SchemaValidation (..)
  , ArrayItemsValidation (..)
  , ValidationResult
  , defaultObjectSchemaData
  , objectSchema
  , booleanSchema
  )
import qualified JsonSchema.Validator as V

-- | Types whose JSON shape is described by a JSON Schema.
class HasJsonSchema a where
  -- | The schema describing the JSON encoding of @a@. The argument is a
  -- 'Proxy' so the class can be called purely off the type.
  toJsonSchema :: Proxy a -> Schema

-- | Render the schema for @a@ to JSON. This is the canonical way to
-- export a schema (e.g. publish to a registry / OpenAPI doc).
jsonSchemaJSON :: forall a. HasJsonSchema a => Value
jsonSchemaJSON = renderSchema (toJsonSchema (Proxy @a))

-- | Validate a JSON value against the schema for @a@. Useful for
-- runtime guards on already-parsed values.
validateAgainstSchema
  :: forall a
   . HasJsonSchema a
  => Proxy a
  -> Value
  -> ValidationResult
validateAgainstSchema p =
  V.validateValue V.defaultValidationConfig (toJsonSchema p)

-- ---------------------------------------------------------------------------
-- Builder helpers
-- ---------------------------------------------------------------------------

-- | Schema for a single JSON primitive type.
primitiveSchema :: SchemaType -> Schema
primitiveSchema ty =
  objectSchema $
    defaultObjectSchemaData
      { objectSchemaDataType = Just (One ty) }

-- | Apply a validation tweak (set @minimum@, @uniqueItems@, …) to an
-- already-built schema. Boolean schemas are returned unchanged.
withValidation :: Schema -> (SchemaValidation -> SchemaValidation) -> Schema
withValidation s f = case schemaCore s of
  ObjectSchema (SchemaObject (Right o)) ->
    s { schemaCore =
          ObjectSchema
            (SchemaObject
               (Right
                  o { objectSchemaDataValidation =
                        f (objectSchemaDataValidation o) }))
      }
  _ -> s

intRange :: Scientific -> Scientific -> Schema
intRange lo hi =
  withValidation (primitiveSchema IntegerType) $ \v ->
    v { validationMinimum = Just lo
      , validationMaximum = Just hi
      }

-- ---------------------------------------------------------------------------
-- Primitive instances
-- ---------------------------------------------------------------------------

instance HasJsonSchema () where
  toJsonSchema _ = primitiveSchema NullType

instance HasJsonSchema Bool where
  toJsonSchema _ = primitiveSchema BooleanType

instance HasJsonSchema Text where
  toJsonSchema _ = primitiveSchema StringType

instance HasJsonSchema Char where
  toJsonSchema _ =
    withValidation (primitiveSchema StringType) $ \v ->
      v { validationMinLength = Just 1, validationMaxLength = Just 1 }

instance HasJsonSchema Scientific where
  toJsonSchema _ = primitiveSchema NumberType

instance HasJsonSchema Double where
  toJsonSchema _ = primitiveSchema NumberType

instance HasJsonSchema Float where
  toJsonSchema _ = primitiveSchema NumberType

instance HasJsonSchema Int where
  toJsonSchema _ = primitiveSchema IntegerType

instance HasJsonSchema Integer where
  toJsonSchema _ = primitiveSchema IntegerType

instance HasJsonSchema Natural where
  toJsonSchema _ =
    withValidation (primitiveSchema IntegerType) $ \v ->
      v { validationMinimum = Just 0 }

instance HasJsonSchema Int8 where
  toJsonSchema _ = intRange (-128) 127
instance HasJsonSchema Int16 where
  toJsonSchema _ = intRange (-32768) 32767
instance HasJsonSchema Int32 where
  toJsonSchema _ = intRange (-2147483648) 2147483647
instance HasJsonSchema Int64 where
  toJsonSchema _ = intRange (-9223372036854775808) 9223372036854775807
instance HasJsonSchema Word8 where
  toJsonSchema _ = intRange 0 255
instance HasJsonSchema Word16 where
  toJsonSchema _ = intRange 0 65535
instance HasJsonSchema Word32 where
  toJsonSchema _ = intRange 0 4294967295
instance HasJsonSchema Word64 where
  toJsonSchema _ = intRange 0 18446744073709551615

-- | An optional value is encoded by adding @null@ to the underlying
-- type's @type@ keyword. Schemas that do not declare a @type@ are
-- left untouched.
instance HasJsonSchema a => HasJsonSchema (Maybe a) where
  toJsonSchema _ =
    let inner = toJsonSchema (Proxy @a)
        addNull o = case objectSchemaDataType o of
          Just (One t) | t /= NullType ->
            o { objectSchemaDataType =
                  Just (Many (NE.fromList [t, NullType])) }
          Just (Many ts) | NullType `notElem` NE.toList ts ->
            o { objectSchemaDataType =
                  Just (Many (NE.fromList (NE.toList ts ++ [NullType]))) }
          _ -> o
     in case schemaCore inner of
          ObjectSchema (SchemaObject (Right o)) ->
            inner
              { schemaCore =
                  ObjectSchema (SchemaObject (Right (addNull o)))
              }
          _ -> inner

instance HasJsonSchema a => HasJsonSchema [a] where
  toJsonSchema _ =
    let item = toJsonSchema (Proxy @a)
     in withValidation (primitiveSchema ArrayType) $ \v ->
          v { validationItems = Just (ItemsSchema item) }

instance HasJsonSchema a => HasJsonSchema (Vector a) where
  toJsonSchema _ = toJsonSchema (Proxy @[a])

instance HasJsonSchema a => HasJsonSchema (Set a) where
  toJsonSchema _ =
    let arr = toJsonSchema (Proxy @[a])
     in withValidation arr $ \v -> v { validationUniqueItems = Just True }

-- | A 'Map' from 'Text' keys is rendered as an open object with the
-- value schema attached to @additionalProperties@.
instance HasJsonSchema v => HasJsonSchema (Map Text v) where
  toJsonSchema _ =
    let val = toJsonSchema (Proxy @v)
     in objectSchema $
          defaultObjectSchemaData
            { objectSchemaDataType = Just (One ObjectType)
            , objectSchemaDataValidation =
                (objectSchemaDataValidation defaultObjectSchemaData)
                  { validationAdditionalProperties = Just val }
            }

-- | An 'Aeson.Value' admits any JSON; encoded as the boolean schema
-- @true@ which always validates.
instance HasJsonSchema Aeson.Value where
  toJsonSchema _ = booleanSchema True
