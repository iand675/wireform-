-- | Utility functions for @google.protobuf@ wrapper types.
--
-- Wrapper types (@DoubleValue@, @FloatValue@, @Int64Value@, etc.) are used
-- in protobuf to distinguish between "field is absent" and "field is present
-- with the default value". These utilities provide ergonomic conversions
-- between the wrapper message types and plain Haskell scalars, plus
-- 'Maybe'-based interop for optional wrapper fields — mirroring the
-- implicit boxing\/unboxing in Go, Java, and C++.
module Proto.Google.Protobuf.WellKnownTypes.Wrappers.Util
  ( -- * Double
    toDoubleValue
  , fromDoubleValue
  , maybeToDoubleValue
  , doubleValueToMaybe

    -- * Float
  , toFloatValue
  , fromFloatValue
  , maybeToFloatValue
  , floatValueToMaybe

    -- * Int64
  , toInt64Value
  , fromInt64Value
  , maybeToInt64Value
  , int64ValueToMaybe

    -- * UInt64
  , toUInt64Value
  , fromUInt64Value
  , maybeToUInt64Value
  , uInt64ValueToMaybe

    -- * Int32
  , toInt32Value
  , fromInt32Value
  , maybeToInt32Value
  , int32ValueToMaybe

    -- * UInt32
  , toUInt32Value
  , fromUInt32Value
  , maybeToUInt32Value
  , uInt32ValueToMaybe

    -- * Bool
  , toBoolValue
  , fromBoolValue
  , maybeToBoolValue
  , boolValueToMaybe

    -- * String
  , toStringValue
  , fromStringValue
  , maybeToStringValue
  , stringValueToMaybe

    -- * Bytes
  , toBytesValue
  , fromBytesValue
  , maybeToBytesValue
  , bytesValueToMaybe
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)

import Proto.Google.Protobuf.WellKnownTypes.Wrappers

-- Double

toDoubleValue :: Double -> DoubleValue
toDoubleValue v = defaultDoubleValue { doubleValueValue = v }

fromDoubleValue :: DoubleValue -> Double
fromDoubleValue = doubleValueValue

maybeToDoubleValue :: Maybe Double -> Maybe DoubleValue
maybeToDoubleValue = fmap toDoubleValue

doubleValueToMaybe :: Maybe DoubleValue -> Maybe Double
doubleValueToMaybe = fmap fromDoubleValue

-- Float

toFloatValue :: Float -> FloatValue
toFloatValue v = defaultFloatValue { floatValueValue = v }

fromFloatValue :: FloatValue -> Float
fromFloatValue = floatValueValue

maybeToFloatValue :: Maybe Float -> Maybe FloatValue
maybeToFloatValue = fmap toFloatValue

floatValueToMaybe :: Maybe FloatValue -> Maybe Float
floatValueToMaybe = fmap fromFloatValue

-- Int64

toInt64Value :: Int64 -> Int64Value
toInt64Value v = defaultInt64Value { int64ValueValue = v }

fromInt64Value :: Int64Value -> Int64
fromInt64Value = int64ValueValue

maybeToInt64Value :: Maybe Int64 -> Maybe Int64Value
maybeToInt64Value = fmap toInt64Value

int64ValueToMaybe :: Maybe Int64Value -> Maybe Int64
int64ValueToMaybe = fmap fromInt64Value

-- UInt64

toUInt64Value :: Word64 -> UInt64Value
toUInt64Value v = defaultUInt64Value { uInt64ValueValue = v }

fromUInt64Value :: UInt64Value -> Word64
fromUInt64Value = uInt64ValueValue

maybeToUInt64Value :: Maybe Word64 -> Maybe UInt64Value
maybeToUInt64Value = fmap toUInt64Value

uInt64ValueToMaybe :: Maybe UInt64Value -> Maybe Word64
uInt64ValueToMaybe = fmap fromUInt64Value

-- Int32

toInt32Value :: Int32 -> Int32Value
toInt32Value v = defaultInt32Value { int32ValueValue = v }

fromInt32Value :: Int32Value -> Int32
fromInt32Value = int32ValueValue

maybeToInt32Value :: Maybe Int32 -> Maybe Int32Value
maybeToInt32Value = fmap toInt32Value

int32ValueToMaybe :: Maybe Int32Value -> Maybe Int32
int32ValueToMaybe = fmap fromInt32Value

-- UInt32

toUInt32Value :: Word32 -> UInt32Value
toUInt32Value v = defaultUInt32Value { uInt32ValueValue = v }

fromUInt32Value :: UInt32Value -> Word32
fromUInt32Value = uInt32ValueValue

maybeToUInt32Value :: Maybe Word32 -> Maybe UInt32Value
maybeToUInt32Value = fmap toUInt32Value

uInt32ValueToMaybe :: Maybe UInt32Value -> Maybe Word32
uInt32ValueToMaybe = fmap fromUInt32Value

-- Bool

toBoolValue :: Bool -> BoolValue
toBoolValue v = defaultBoolValue { boolValueValue = v }

fromBoolValue :: BoolValue -> Bool
fromBoolValue = boolValueValue

maybeToBoolValue :: Maybe Bool -> Maybe BoolValue
maybeToBoolValue = fmap toBoolValue

boolValueToMaybe :: Maybe BoolValue -> Maybe Bool
boolValueToMaybe = fmap fromBoolValue

-- String

toStringValue :: Text -> StringValue
toStringValue v = defaultStringValue { stringValueValue = v }

fromStringValue :: StringValue -> Text
fromStringValue = stringValueValue

maybeToStringValue :: Maybe Text -> Maybe StringValue
maybeToStringValue = fmap toStringValue

stringValueToMaybe :: Maybe StringValue -> Maybe Text
stringValueToMaybe = fmap fromStringValue

-- Bytes

toBytesValue :: ByteString -> BytesValue
toBytesValue v = defaultBytesValue { bytesValueValue = v }

fromBytesValue :: BytesValue -> ByteString
fromBytesValue = bytesValueValue

maybeToBytesValue :: Maybe ByteString -> Maybe BytesValue
maybeToBytesValue = fmap toBytesValue

bytesValueToMaybe :: Maybe BytesValue -> Maybe ByteString
bytesValueToMaybe = fmap fromBytesValue
