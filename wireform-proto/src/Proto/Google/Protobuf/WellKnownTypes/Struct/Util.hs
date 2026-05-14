-- | Utility functions for @google.protobuf.Struct@ and @Value@.
--
-- Provides a builder DSL for constructing 'Struct' and 'Value' from
-- native Haskell types, plus conversions to\/from 'Aeson.Value' — mirroring
-- Go's @structpb.NewStruct@, @structpb.NewValue@, etc.
module Proto.Google.Protobuf.WellKnownTypes.Struct.Util
  ( -- * Struct construction
    fromMap
  , fromPairs
  , toMap

    -- * Value construction
  , nullValue
  , numberValue
  , stringValue
  , boolValue
  , structValue
  , listValue

    -- * Value extraction
  , asNull
  , asNumber
  , asString
  , asBool
  , asStruct
  , asList

    -- * Aeson bridge
  , valueFromAeson
  , valueToAeson
  , structFromAeson
  , structToAeson
  ) where

import Data.Bifunctor (bimap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM

import Proto.Google.Protobuf.WellKnownTypes.Struct

-- | Construct a 'Struct' from a 'Map'.
fromMap :: Map Text Value -> Struct
fromMap m = defaultStruct { structFields = m }

-- | Construct a 'Struct' from key-value pairs.
fromPairs :: [(Text, Value)] -> Struct
fromPairs = fromMap . Map.fromList

-- | Extract the underlying 'Map' from a 'Struct'.
toMap :: Struct -> Map Text Value
toMap = structFields

-- | A null 'Value'.
nullValue :: Value
nullValue = defaultValue
  { valueKind = Just (Value'Kind'NullValue NullValue'NullValue) }

-- | A numeric 'Value'.
numberValue :: Double -> Value
numberValue d = defaultValue
  { valueKind = Just (Value'Kind'NumberValue d) }

-- | A string 'Value'.
stringValue :: Text -> Value
stringValue t = defaultValue
  { valueKind = Just (Value'Kind'StringValue t) }

-- | A boolean 'Value'.
boolValue :: Bool -> Value
boolValue b = defaultValue
  { valueKind = Just (Value'Kind'BoolValue b) }

-- | A 'Value' wrapping a 'Struct'.
structValue :: Struct -> Value
structValue s = defaultValue
  { valueKind = Just (Value'Kind'StructValue s) }

-- | A 'Value' wrapping a list of 'Value's.
listValue :: [Value] -> Value
listValue vs = defaultValue
  { valueKind = Just (Value'Kind'ListValue (defaultListValue { listValueValues = V.fromList vs })) }

-- | Extract 'Nothing' if the value is null, or 'Just ()' otherwise.
asNull :: Value -> Maybe ()
asNull v = case valueKind v of
  Just (Value'Kind'NullValue _) -> Just ()
  _ -> Nothing

-- | Extract a 'Double' if the value is numeric.
asNumber :: Value -> Maybe Double
asNumber v = case valueKind v of
  Just (Value'Kind'NumberValue d) -> Just d
  _ -> Nothing

-- | Extract a 'Text' if the value is a string.
asString :: Value -> Maybe Text
asString v = case valueKind v of
  Just (Value'Kind'StringValue t) -> Just t
  _ -> Nothing

-- | Extract a 'Bool' if the value is boolean.
asBool :: Value -> Maybe Bool
asBool v = case valueKind v of
  Just (Value'Kind'BoolValue b) -> Just b
  _ -> Nothing

-- | Extract a 'Struct' if the value wraps one.
asStruct :: Value -> Maybe Struct
asStruct v = case valueKind v of
  Just (Value'Kind'StructValue s) -> Just s
  _ -> Nothing

-- | Extract a list of 'Value's if the value is a list.
asList :: Value -> Maybe [Value]
asList v = case valueKind v of
  Just (Value'Kind'ListValue lv) -> Just (V.toList (listValueValues lv))
  _ -> Nothing

-- | Convert an 'Aeson.Value' to a protobuf 'Value'.
valueFromAeson :: Aeson.Value -> Value
valueFromAeson = \case
  Aeson.Null -> nullValue
  Aeson.Bool b -> boolValue b
  Aeson.Number n -> numberValue (toRealFloat n)
  Aeson.String s -> stringValue s
  Aeson.Array vs -> listValue (V.toList (fmap valueFromAeson vs))
  Aeson.Object o -> structValue (structFromAeson (Aeson.Object o))

-- | Convert a protobuf 'Value' to an 'Aeson.Value'.
valueToAeson :: Value -> Aeson.Value
valueToAeson v = case valueKind v of
  Nothing -> Aeson.Null
  Just vk -> case vk of
    Value'Kind'NullValue _   -> Aeson.Null
    Value'Kind'NumberValue d -> Aeson.Number (fromFloatDigits d)
    Value'Kind'StringValue s -> Aeson.String s
    Value'Kind'BoolValue b   -> Aeson.Bool b
    Value'Kind'StructValue s -> structToAeson s
    Value'Kind'ListValue l   -> Aeson.Array (fmap valueToAeson (listValueValues l))

-- | Convert an 'Aeson.Value' (must be an Object) to a 'Struct'.
-- Non-object inputs produce an empty 'Struct'.
structFromAeson :: Aeson.Value -> Struct
structFromAeson (Aeson.Object o) =
  fromMap (Map.fromList (fmap (bimap AesonKey.toText valueFromAeson) (AesonKM.toList o)))
structFromAeson _ = defaultStruct

-- | Convert a 'Struct' to an 'Aeson.Value' (Object).
structToAeson :: Struct -> Aeson.Value
structToAeson s =
  Aeson.Object (AesonKM.fromList
    (fmap (bimap AesonKey.fromText valueToAeson) (Map.toList (structFields s))))
