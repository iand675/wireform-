-- | User-extensible CBOR tag handler registration.
--
-- CBOR uses numeric tags to annotate semantic meaning on values.
-- This module allows users to register custom 'TagHandler's that
-- provide human-readable names, optional Haskell type overrides
-- for codegen, and runtime validation functions.
module CBOR.TagRegistry
  ( -- * Registry
    CBORTagRegistry (..)
  , defaultCBORTagRegistry
    -- * Tag handlers
  , TagHandler (..)
  , registerTag
  , lookupTag
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Vector as V

import qualified CBOR.Value

-- | Handler for a specific CBOR tag.
data TagHandler = TagHandler
  { thName        :: !Text
  , thHaskellType :: !(Maybe Text)
  , thValidate    :: !(CBOR.Value.Value -> Either String CBOR.Value.Value)
  }

-- | Registry of custom CBOR tag handlers.
data CBORTagRegistry = CBORTagRegistry
  { ctrTags :: !(IntMap TagHandler)
  }

instance Semigroup CBORTagRegistry where
  a <> b = CBORTagRegistry
    { ctrTags = ctrTags a <> ctrTags b
    }

instance Monoid CBORTagRegistry where
  mempty = CBORTagRegistry IntMap.empty

-- | Default registry covering all of the standard CBOR tags defined
-- in RFC 8949 \xC2\xA73.4 plus the self-describe sentinel
-- (tag 55799, RFC 8949 \xC2\xA73.4.6). Each handler validates the shape of
-- the tagged value so callers can fail early on malformed input.
--
-- Standard tags covered:
--
-- * 0  — date/time string                         (text string)
-- * 1  — epoch-based date/time                    (number)
-- * 2  — positive bignum                          (byte string)
-- * 3  — negative bignum                          (byte string)
-- * 4  — decimal fraction                         (array [exp, mantissa])
-- * 5  — bigfloat                                 (array [exp, mantissa])
-- * 21 — expected base64url conversion            (any item)
-- * 22 — expected base64 conversion               (any item)
-- * 23 — expected base16 conversion               (any item)
-- * 24 — encoded CBOR data item                   (byte string)
-- * 32 — URI                                       (text string)
-- * 33 — base64url                                 (text string)
-- * 34 — base64                                    (text string)
-- * 36 — MIME message                              (text string)
-- * 55799 — self-described CBOR                   (any item)
defaultCBORTagRegistry :: CBORTagRegistry
defaultCBORTagRegistry = CBORTagRegistry
  { ctrTags = IntMap.fromList
      [ (0,  textString  "datetime"   (Just "UTCTime"))
      , (1,  number      "epoch"      (Just "POSIXTime"))
      , (2,  byteString  "posbignum"  (Just "Integer"))
      , (3,  byteString  "negbignum"  (Just "Integer"))
      , (4,  decimalFractionOrBigFloat "decimalfraction")
      , (5,  decimalFractionOrBigFloat "bigfloat")
      , (21, anyItem     "expected-b64url" Nothing)
      , (22, anyItem     "expected-b64"    Nothing)
      , (23, anyItem     "expected-b16"    Nothing)
      , (24, byteString  "encoded-cbor"    Nothing)
      , (32, textString  "uri"             (Just "Text"))
      , (33, textString  "b64url"          (Just "Text"))
      , (34, textString  "b64"             (Just "Text"))
      , (36, textString  "mime"            (Just "Text"))
      , (55799, anyItem  "self-describe"   Nothing)
      ]
  }
  where
    textString name ty = TagHandler
      { thName = name
      , thHaskellType = ty
      , thValidate = \v -> case v of
          CBOR.Value.TextString _ -> Right v
          _ -> Left $ "tag (" <> show name <> ") expects a text string"
      }

    byteString name ty = TagHandler
      { thName = name
      , thHaskellType = ty
      , thValidate = \v -> case v of
          CBOR.Value.ByteString _ -> Right v
          _ -> Left $ "tag (" <> show name <> ") expects a byte string"
      }

    number name ty = TagHandler
      { thName = name
      , thHaskellType = ty
      , thValidate = \v -> case v of
          CBOR.Value.UInt _    -> Right v
          CBOR.Value.NInt _    -> Right v
          CBOR.Value.Float32 _ -> Right v
          CBOR.Value.Float64 _ -> Right v
          _ -> Left $ "tag (" <> show name <> ") expects a number"
      }

    anyItem name ty = TagHandler
      { thName = name
      , thHaskellType = ty
      , thValidate = Right
      }

    -- Tag 4 (decimal fraction) and tag 5 (bigfloat) both wrap a 2-element
    -- array of [int exponent, int|bignum mantissa]. We accept any 2-element
    -- array here; downstream consumers can refine.
    decimalFractionOrBigFloat name = TagHandler
      { thName = name
      , thHaskellType = Just "(Integer, Integer)"
      , thValidate = \v -> case v of
          CBOR.Value.Array vs
            | V.length vs == 2 -> Right v
            | otherwise -> Left $ "tag (" <> show name
                                  <> ") expects a 2-element array"
          _ -> Left $ "tag (" <> show name <> ") expects an array"
      }

-- | Register a tag handler for a specific CBOR tag number.
registerTag :: Int -> TagHandler -> CBORTagRegistry -> CBORTagRegistry
registerTag tagNum handler reg =
  reg { ctrTags = IntMap.insert tagNum handler (ctrTags reg) }

-- | Look up a tag handler by tag number.
lookupTag :: Int -> CBORTagRegistry -> Maybe TagHandler
lookupTag tagNum reg = IntMap.lookup tagNum (ctrTags reg)
