{-# LANGUAGE ScopedTypeVariables #-}

{- | Utility functions for @google.protobuf.Any@ — type-safe packing,
unpacking, and runtime dispatch via 'TypeRegistry'.
-}
module Proto.Google.Protobuf.WellKnownTypes.Any.Util (
  -- * Type-safe packing / unpacking
  packAny,
  packAnyWithPrefix,
  unpackAny,
  isMessageType,

  -- * Utilities
  typeUrlPrefix,
  typeUrlOf,
  typeNameFromUrl,
) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Proto (DecodeError, decodeMessage)
import Proto (encodeMessage)
import Proto.Google.Protobuf.WellKnownTypes.Any (Any (..))
import Proto.Registry (IsMessage)
import Proto.Schema (ProtoMessage (..))


typeUrlPrefix :: Text
typeUrlPrefix = "type.googleapis.com/"


typeUrlOf :: forall a. IsMessage a => Proxy a -> Text
typeUrlOf p = typeUrlPrefix <> protoMessageName p


packAny :: forall a. IsMessage a => a -> Any
packAny msg =
  Any
    { anyTypeUrl = typeUrlOf (Proxy :: Proxy a)
    , anyValue = encodeMessage msg
    , anyUnknownFields = []
    }


packAnyWithPrefix :: forall a. IsMessage a => Text -> a -> Any
packAnyWithPrefix prefix msg =
  Any
    { anyTypeUrl = prefix <> protoMessageName (Proxy :: Proxy a)
    , anyValue = encodeMessage msg
    , anyUnknownFields = []
    }


unpackAny :: forall a. IsMessage a => Any -> Maybe (Either DecodeError a)
unpackAny any'
  | isTypeMatch (anyTypeUrl any') (Proxy :: Proxy a) = Just (decodeMessage (anyValue any'))
  | otherwise = Nothing


isMessageType :: forall a. IsMessage a => Proxy a -> Any -> Bool
isMessageType p any' = isTypeMatch (anyTypeUrl any') p


isTypeMatch :: forall a. IsMessage a => Text -> Proxy a -> Bool
isTypeMatch tu p =
  let expected = protoMessageName p
  in typeNameFromUrl tu == expected


typeNameFromUrl :: Text -> Text
typeNameFromUrl tu = case T.breakOnEnd "/" tu of
  ("", name) -> name
  (_, name) -> name
