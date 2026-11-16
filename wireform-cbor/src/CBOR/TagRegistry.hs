{- | User-extensible CBOR tag handler registration.

CBOR uses numeric tags to annotate semantic meaning on values.
This module allows users to register custom 'TagHandler's that
provide human-readable names, optional Haskell type overrides
for codegen, and runtime validation functions.
-}
module CBOR.TagRegistry (
  -- * Registry
  CBORTagRegistry (..),
  defaultCBORTagRegistry,

  -- * Tag handlers
  TagHandler (..),
  registerTag,
  lookupTag,
) where

import CBOR.Value qualified
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)


-- | Handler for a specific CBOR tag.
data TagHandler = TagHandler
  { thName :: !Text
  , thHaskellType :: !(Maybe Text)
  , thValidate :: !(CBOR.Value.Value -> Either String CBOR.Value.Value)
  }


-- | Registry of custom CBOR tag handlers.
data CBORTagRegistry = CBORTagRegistry
  { ctrTags :: !(IntMap TagHandler)
  }


instance Semigroup CBORTagRegistry where
  a <> b =
    CBORTagRegistry
      { ctrTags = ctrTags a <> ctrTags b
      }


instance Monoid CBORTagRegistry where
  mempty = CBORTagRegistry IntMap.empty


{- | Default registry with standard CBOR tags:
tag 0 = datetime string, tag 1 = epoch time,
tag 2 = positive bignum, tag 3 = negative bignum.
-}
defaultCBORTagRegistry :: CBORTagRegistry
defaultCBORTagRegistry =
  CBORTagRegistry
    { ctrTags =
        IntMap.fromList
          [
            ( 0
            , TagHandler
                { thName = "datetime"
                , thHaskellType = Just "UTCTime"
                , thValidate = \v -> case v of
                    CBOR.Value.TextString _ -> Right v
                    _ -> Left "tag 0 (datetime) expects a text string"
                }
            )
          ,
            ( 1
            , TagHandler
                { thName = "epoch"
                , thHaskellType = Just "POSIXTime"
                , thValidate = \v -> case v of
                    CBOR.Value.UInt _ -> Right v
                    CBOR.Value.NInt _ -> Right v
                    CBOR.Value.Float16 _ -> Right v
                    CBOR.Value.Float32 _ -> Right v
                    CBOR.Value.Float64 _ -> Right v
                    _ -> Left "tag 1 (epoch) expects a number"
                }
            )
          ,
            ( 2
            , TagHandler
                { thName = "posbignum"
                , thHaskellType = Just "Integer"
                , thValidate = \v -> case v of
                    CBOR.Value.ByteString _ -> Right v
                    _ -> Left "tag 2 (posbignum) expects a byte string"
                }
            )
          ,
            ( 3
            , TagHandler
                { thName = "negbignum"
                , thHaskellType = Just "Integer"
                , thValidate = \v -> case v of
                    CBOR.Value.ByteString _ -> Right v
                    _ -> Left "tag 3 (negbignum) expects a byte string"
                }
            )
          ]
    }


-- | Register a tag handler for a specific CBOR tag number.
registerTag :: Int -> TagHandler -> CBORTagRegistry -> CBORTagRegistry
registerTag tagNum handler reg =
  reg {ctrTags = IntMap.insert tagNum handler (ctrTags reg)}


-- | Look up a tag handler by tag number.
lookupTag :: Int -> CBORTagRegistry -> Maybe TagHandler
lookupTag tagNum reg = IntMap.lookup tagNum (ctrTags reg)
