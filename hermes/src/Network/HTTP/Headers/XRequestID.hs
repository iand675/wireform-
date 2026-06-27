{- |
@X-Request-ID@ request and response header — a unique, opaque token identifying a
single HTTP request so that it can be correlated across logs and services. A
client or the first proxy in the chain generates the token; every reverse proxy
and service in the path preserves and forwards it, and the origin server
typically echoes the same value back in the response. It is supported by Nginx,
HAProxy, AWS ALB, Heroku, and most API gateways.

This is a __de-facto__ header with __no IANA registration__ and no formal
grammar — any opaque string is valid (a UUIDv4 is the typical value). We
therefore preserve the value verbatim as a 'ST.ShortText' rather than imposing a
structure that the wild does not honour.

Spec: de-facto, unregistered; see <https://http.dev/x-request-id>.

See also: "Network.HTTP.Headers.XCorrelationID", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XRequestID (
  XRequestID (..),
  xRequestIDParser,
  renderXRequestID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRequestID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | An @X-Request-ID@ value: an opaque request-correlation token, preserved
verbatim.
-}
newtype XRequestID = XRequestID {xRequestID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XRequestID where
  type ParseFailure XRequestID = String
  type Cardinality XRequestID = 'ZeroOrOne
  type Direction XRequestID = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser xRequestIDParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing X-Request-ID header: " <> show rest
      Fail -> Left "Failed to parse X-Request-ID header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXRequestID


  headerName _ = hXRequestID


xRequestIDParser :: ParserT st String XRequestID
xRequestIDParser = XRequestID <$> takeRestShortText


renderXRequestID :: XRequestID -> M.Builder
renderXRequestID (XRequestID tok) = shortText tok
