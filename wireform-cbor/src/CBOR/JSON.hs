{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Lossy conversion between CBOR and Aeson values. Byte strings use
unpadded base64url. An object with exactly @tag@ and @value@ fields
and an unsigned integer tag is reserved for CBOR tags. Non-text map
keys use a flat array of alternating keys and values.
-}
module CBOR.JSON (
  toJSON,
  fromJSON,
) where

import CBOR.Value qualified as C
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Base64.URL qualified as B64
import Data.Scientific (coefficient, fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Prelude hiding (map)


-- | Convert a CBOR 'C.Value' to a JSON 'Aeson.Value'.
toJSON :: C.Value -> Aeson.Value
toJSON (C.UInt n) = Aeson.Number (fromIntegral n)
toJSON (C.NInt n) = Aeson.Number (fromIntegral (negate (fromIntegral n :: Integer) - 1))
toJSON (C.Bool b) = Aeson.Bool b
toJSON C.Null = Aeson.Null
toJSON C.Undefined = Aeson.Null
toJSON (C.Float16 f) = floatToJSON f
toJSON (C.Float32 f) = floatToJSON f
toJSON (C.Float64 d) = doubleToJSON d
toJSON (C.ByteString bs) = Aeson.String (TE.decodeUtf8 (B64.encodeUnpadded bs))
toJSON (C.TextString t) = Aeson.String t
toJSON (C.Array vec) = Aeson.Array (V.map toJSON vec)
toJSON (C.Map vec) = mapToJSON vec
toJSON (C.Tag tagNum content) =
  Aeson.Object $
    KM.fromList
      [ (Key.fromText "tag", Aeson.Number (fromIntegral tagNum))
      , (Key.fromText "value", toJSON content)
      ]
toJSON (C.Simple n) = Aeson.Number (fromIntegral n)


-- | Convert a JSON 'Aeson.Value' to a CBOR 'C.Value'.
fromJSON :: Aeson.Value -> C.Value
fromJSON Aeson.Null = C.Null
fromJSON (Aeson.Bool b) = C.Bool b
fromJSON (Aeson.String t) = C.TextString t
fromJSON (Aeson.Number n)
  | coefficient n >= 0
  , Just magnitude <- toBoundedInteger n = C.UInt magnitude
  | coefficient n < 0
  , Just magnitude <- toBoundedInteger (negate n) = C.NInt (magnitude - 1)
  -- The lowest basic CBOR integer has an absolute value one past Word64.
  | n == -18446744073709551616 = C.NInt maxBound
  | otherwise = C.Float64 (toRealFloat n)
fromJSON (Aeson.Array arr) = C.Array (V.map fromJSON arr)
fromJSON (Aeson.Object obj)
  | KM.size obj == 2
  , Just (Aeson.Number tn) <- KM.lookup "tag" obj
  , Just val <- KM.lookup "value" obj
  , Just tagN <- toBoundedInteger tn = C.Tag tagN (fromJSON val)
  | otherwise = objToMap obj


objToMap :: KM.KeyMap Aeson.Value -> C.Value
objToMap obj =
  C.Map $
    V.fromList
      [ (C.TextString (Key.toText k), fromJSON v)
      | (k, v) <- KM.toList obj
      ]


mapToJSON :: V.Vector (C.Value, C.Value) -> Aeson.Value
mapToJSON vec
  | V.all isTextKey vec =
      Aeson.Object $
        KM.fromList
          [ (Key.fromText k, toJSON v)
          | (C.TextString k, v) <- V.toList vec
          ]
  | otherwise =
      Aeson.Array $ V.concatMap (\(k, v) -> V.fromList [toJSON k, toJSON v]) vec
  where
    isTextKey (C.TextString _, _) = True
    isTextKey _ = False


floatToJSON :: Float -> Aeson.Value
floatToJSON !f
  | isNaN f = Aeson.String "NaN"
  | isInfinite f && f > 0 = Aeson.String "Infinity"
  | isInfinite f = Aeson.String "-Infinity"
  | otherwise = Aeson.Number (fromFloatDigits f)


doubleToJSON :: Double -> Aeson.Value
doubleToJSON !d
  | isNaN d = Aeson.String "NaN"
  | isInfinite d && d > 0 = Aeson.String "Infinity"
  | isInfinite d = Aeson.String "-Infinity"
  | otherwise = Aeson.Number (fromFloatDigits d)


instance Aeson.ToJSON C.Value where
  toJSON = toJSON


instance Aeson.FromJSON C.Value where
  parseJSON = pure . fromJSON
