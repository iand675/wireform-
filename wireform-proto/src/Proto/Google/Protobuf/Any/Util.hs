{-# LANGUAGE ScopedTypeVariables #-}
-- | Utility functions for @google.protobuf.Any@ — type-safe packing,
-- unpacking, and a runtime 'MessageRegistry' for dynamic dispatch.
module Proto.Google.Protobuf.Any.Util
  ( -- * Type-safe packing / unpacking
    packAny
  , packAnyWithPrefix
  , unpackAny
  , isMessageType

    -- * Type registry (re-exported from Proto.Registry)
  , MessageRegistry
  , emptyRegistry
  , registerType
  , lookupDecoder
  , unpackAnyDynamic
  , DynamicMessage (..)

    -- * Utilities
  , typeUrlPrefix
  , typeUrlOf
  , typeNameFromUrl
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T

import Proto.Encode (encodeMessage)
import Proto.Decode (DecodeError, decodeMessage)
import Proto.Message (IsMessage (..))
import Proto.Google.Protobuf.Any (Any(..))
import Proto.Registry (MessageRegistry, emptyRegistry, registerType, lookupDecoder, DynamicMessage(..))

typeUrlPrefix :: Text
typeUrlPrefix = "type.googleapis.com/"

typeUrlOf :: forall a. IsMessage a => Proxy a -> Text
typeUrlOf p = typeUrlPrefix <> messageTypeName p

packAny :: forall a. IsMessage a => a -> Any
packAny msg = Any
  { anyTypeUrl = typeUrlOf (Proxy :: Proxy a)
  , anyValue   = encodeMessage msg
  , anyUnknownFields = []
  }

packAnyWithPrefix :: forall a. IsMessage a => Text -> a -> Any
packAnyWithPrefix prefix msg = Any
  { anyTypeUrl = prefix <> messageTypeName (Proxy :: Proxy a)
  , anyValue   = encodeMessage msg
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
  let expected = messageTypeName p
  in typeNameFromUrl tu == expected

typeNameFromUrl :: Text -> Text
typeNameFromUrl tu = case T.breakOnEnd "/" tu of
  ("", name) -> name
  (_, name)  -> name

unpackAnyDynamic :: MessageRegistry -> Any -> Maybe (Either DecodeError DynamicMessage)
unpackAnyDynamic reg any' =
  let name = typeNameFromUrl (anyTypeUrl any')
  in case lookupDecoder name reg of
    Nothing      -> Nothing
    Just decoder -> Just (decoder (anyValue any'))
