{-# LANGUAGE OverloadedStrings #-}

{- | gRPC with Protobuf

The 'Protobuf' type is a type-level marker for protobuf-based RPCs.
The 'Proto' newtype wraps wireform-proto message types so that
gRPC infrastructure can provide blanket instances for serialization,
'NFData', 'Show', etc.

Unlike the upstream grapesy version which uses @proto-lens@, this
module is backed by @wireform-proto@.
-}
module Network.GRPC.Spec.RPC.Protobuf (
  Protobuf,
  Proto (..),
  getProto,
) where

import Control.DeepSeq (NFData (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Kind
import GHC.Generics (Generic)
import GHC.TypeLits
import Network.GRPC.Spec.CustomMetadata.Typed
import Network.GRPC.Spec.RPC
import Network.GRPC.Spec.RPC.StreamType
import Proto.Decode (MessageDecode (..), decodeMessage)
import Proto.Encode (MessageEncode (..), encodeMessage)
import Proto.Schema (ProtoMessage (..))
import Proto.Schema qualified as PS


{-------------------------------------------------------------------------------
  The spec defines the following in Appendix A, "GRPC for Protobuf":

  > Service-Name → ?( {proto package name} "." ) {service name}
  > Message-Type → {fully qualified proto message name}
  > Content-Type → "application/grpc+proto"
-------------------------------------------------------------------------------}

{- | Protobuf RPC

This exists only as a type-level marker. Users define per-service
@type instance Input@/@Output@ and @IsRPC@/@SupportsClientRpc@/
@SupportsServerRpc@ instances.
-}
data Protobuf (serv :: Type) (meth :: Symbol)


{-------------------------------------------------------------------------------
  Wrapper around Protobuf messages
-------------------------------------------------------------------------------}

{- | Wrapper around wireform-proto messages.

@Proto msg@ inherits encoding/decoding and field-access instances
from @msg@. You can create values with @mempty & #field .~ val@
and access fields with @msg ^. #field@ (via OverloadedLabels +
the 'PS.HasField' instance).
-}
newtype Proto msg = Proto msg
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Unwrap a 'Proto' value
getProto :: Proto msg -> msg
getProto (Proto msg) = msg


instance MessageEncode msg => MessageEncode (Proto msg) where
  buildMessage (Proto msg) = buildMessage msg


instance MessageDecode msg => MessageDecode (Proto msg) where
  {-# INLINE messageDecoder #-}
  messageDecoder = Proto <$> messageDecoder


-- | @Proto msg@ serializes to canonical proto3 JSON via the wrapped
-- message's @aeson@ instances (which @wireform-proto@'s @loadProto@ /
-- @Proto.Derive@ generate). This lets the Connect JSON codec treat a
-- method's @Input@ \/ @Output@ uniformly without unwrapping.
instance ToJSON msg => ToJSON (Proto msg) where
  toJSON (Proto msg) = toJSON msg
  toEncoding (Proto msg) = toEncoding msg


instance FromJSON msg => FromJSON (Proto msg) where
  parseJSON = fmap Proto . parseJSON


instance Semigroup msg => Semigroup (Proto msg) where
  Proto a <> Proto b = Proto (a <> b)


instance Monoid msg => Monoid (Proto msg) where
  mempty = Proto mempty


instance PS.HasField msg name a => PS.HasField (Proto msg) name a where
  getField (Proto msg) = PS.getField @msg @name msg
  setField a (Proto msg) = Proto (PS.setField @msg @name a msg)
