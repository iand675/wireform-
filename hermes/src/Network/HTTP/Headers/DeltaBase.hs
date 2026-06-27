{- |
@Delta-Base@ is a response header carrying the entity-tag of the base
instance against which a delta-encoded response was computed. It lets
the client locate its cached copy of that base instance and apply the
received delta to reconstruct the current instance. It accompanies the
@IM@ header on a @226 IM Used@ response (RFC 3229, "Delta encoding in
HTTP").

== Grammar

@
Delta-Base = entity-tag
@

The value is an HTTP entity-tag, so this reuses the entity-tag
machinery from "Network.HTTP.Headers.ETag".

Spec: <https://www.rfc-editor.org/rfc/rfc3229#section-10.5.1>

See also: "Network.HTTP.Headers.IM", "Network.HTTP.Headers.AIM", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfNoneMatch".
-}
module Network.HTTP.Headers.DeltaBase (
  DeltaBase (..),
  deltaBaseParser,
  renderDeltaBase,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.ETag (EntityTag, entityTagParser, renderEntityTag)
import Network.HTTP.Headers.HeaderFieldName (hDeltaBase)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The @Delta-Base@ header value: the entity-tag of the delta base instance.
newtype DeltaBase = DeltaBase
  { deltaBase :: EntityTag
  }
  deriving stock (Eq, Show)


instance KnownHeader DeltaBase where
  type ParseFailure DeltaBase = String
  type Cardinality DeltaBase = 'ZeroOrOne
  type Direction DeltaBase = 'Response


  parseFromHeaders _ headers = case runParser deltaBaseParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Delta-Base header: " <> show rest
    Fail -> Left "Failed to parse Delta-Base header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDeltaBase


  headerName _ = hDeltaBase


deltaBaseParser :: ParserT st String DeltaBase
deltaBaseParser = DeltaBase <$> entityTagParser


renderDeltaBase :: DeltaBase -> M.Builder
renderDeltaBase (DeltaBase t) = renderEntityTag t
