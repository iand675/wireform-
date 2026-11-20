{- |
@X-Real-IP@ — de-facto request header (popularised by nginx's @realip@ module)
carrying the single originating client IP address as determined by the
immediate proxy.

This header is __not IANA-registered__ and overlaps the @for@ parameter of the
standardized @Forwarded@ header
(<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>). Unlike @X-Forwarded-For@,
it conveys exactly one address, so the value is parsed and re-rendered as a
typed IP literal through "Network.IPAddress".

Spec (de-facto, not IANA-registered): nginx @ngx_http_realip_module@
<https://nginx.org/en/docs/http/ngx_http_realip_module.html>.

See also: "Network.HTTP.Headers.XForwardedFor", "Network.HTTP.Headers.Forwarded",
"Network.HTTP.Headers.XForwardedProto", "Network.HTTP.Headers.XForwardedHost".
-}
module Network.HTTP.Headers.XRealIP (
  XRealIP (..),
  xRealIPParser,
  renderXRealIP,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRealIP)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.IPAddress (IPAddress (..), ipv4Parser, ipv6Parser, parseBareIPv4, parseBareIPv6, renderIPAddress)


-- | The single client IP address reported by the proxy.
newtype XRealIP = XRealIP {xRealIPAddress :: IPAddress}
  deriving stock (Eq, Show)


instance KnownHeader XRealIP where
  type ParseFailure XRealIP = String
  type Cardinality XRealIP = 'ZeroOrOne
  type Direction XRealIP = 'Request


  parseFromHeaders _ (wire :| [])
    | Just address <- parseBareIPv4 wire = Right (XRealIP (IPv4Address address))
    | Just address <- parseBareIPv6 wire = Right (XRealIP (IPv6Address address))
  parseFromHeaders _ fields = parseSingleFieldLine "X-Real-IP" xRealIPParser fields


  renderToHeaders _ = M.toStrictByteString . renderXRealIP


  headerName _ = hXRealIP


xRealIPParser :: ParserT st String XRealIP
xRealIPParser = XRealIP <$> ((IPv4Address <$> ipv4Parser) <|> (IPv6Address <$> ipv6Parser))


renderXRealIP :: XRealIP -> M.Builder
renderXRealIP (XRealIP ip) = renderIPAddress ip
