{- |
@X-Forwarded-Port@ — de-facto request header set by reverse proxies and load
balancers to convey the TCP port the client connected to on the proxy
(e.g. @443@), letting the origin reconstruct the public-facing port.

This header is __not IANA-registered__ and is related to the standardized
@Forwarded@ header (<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>), whose
@host@/@by@ parameters may carry an authority with a port. The value is a
single decimal port number.

Spec (de-facto, not IANA-registered; MDN has no dedicated page): AWS Elastic
Load Balancing documentation
<https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/x-forwarded-headers.html>.

See also: "Network.HTTP.Headers.Forwarded", "Network.HTTP.Headers.XForwardedHost",
"Network.HTTP.Headers.XForwardedProto", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XForwardedPort (
  XForwardedPort (..),
  xForwardedPortParser,
  renderXForwardedPort,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXForwardedPort)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The forwarded TCP port number the client used to reach the proxy.
newtype XForwardedPort = XForwardedPort {xForwardedPortNumber :: Word}
  deriving stock (Eq, Show)


instance KnownHeader XForwardedPort where
  type ParseFailure XForwardedPort = String
  type Cardinality XForwardedPort = 'ZeroOrOne
  type Direction XForwardedPort = 'Request


  parseFromHeaders _ headers = case runParser xForwardedPortParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Forwarded-Port header: " <> show rest
    Fail -> Left "Failed to parse X-Forwarded-Port header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXForwardedPort


  headerName _ = hXForwardedPort


xForwardedPortParser :: ParserT st String XForwardedPort
xForwardedPortParser = XForwardedPort <$> anyAsciiDecimalWord


renderXForwardedPort :: XForwardedPort -> M.Builder
renderXForwardedPort (XForwardedPort p) = M.wordDec p
