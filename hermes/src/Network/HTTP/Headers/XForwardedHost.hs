{- |
@X-Forwarded-Host@ — de-facto request header set by reverse proxies to convey
the original @Host@ requested by the client before the request was proxied.

This header is __not IANA-registered__ and is superseded by the @host@
parameter of the standardized @Forwarded@ header
(<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>). The value is nominally a
single host (optionally with a @:port@), but deployments occasionally emit
comma-separated chains, so the raw value is preserved verbatim.

Spec (de-facto, not IANA-registered): MDN
<https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-Host>.

See also: "Network.HTTP.Headers.Forwarded", "Network.HTTP.Headers.Host",
"Network.HTTP.Headers.XForwardedProto", "Network.HTTP.Headers.XForwardedPort",
"Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XForwardedHost (
  XForwardedHost (..),
  xForwardedHostParser,
  renderXForwardedHost,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXForwardedHost)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | The forwarded host value, preserved verbatim
(e.g. @\"example.com\"@ or @\"example.com:8443\"@).
-}
newtype XForwardedHost = XForwardedHost {xForwardedHostValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XForwardedHost where
  type ParseFailure XForwardedHost = String
  type Cardinality XForwardedHost = 'ZeroOrOne
  type Direction XForwardedHost = 'Request


  parseFromHeaders _ headers = case runParser xForwardedHostParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Forwarded-Host header: " <> show rest
    Fail -> Left "Failed to parse X-Forwarded-Host header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXForwardedHost


  headerName _ = hXForwardedHost


xForwardedHostParser :: ParserT st String XForwardedHost
xForwardedHostParser = XForwardedHost <$> takeRestShortText


renderXForwardedHost :: XForwardedHost -> M.Builder
renderXForwardedHost (XForwardedHost h) = shortText h
