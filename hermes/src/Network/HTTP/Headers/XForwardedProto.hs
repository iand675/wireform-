{- |
@X-Forwarded-Proto@ — de-facto request header set by reverse proxies to convey
the protocol (URI scheme) the client used to reach the proxy.

This header is __not IANA-registered__ and is superseded by the @proto@
parameter of the standardized @Forwarded@ header
(<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>). The overwhelmingly common
values are @http@ and @https@, surfaced typed; any other scheme token is
preserved verbatim. Scheme tokens are matched case-sensitively (the de-facto
convention is lower case).

Spec (de-facto, not IANA-registered): MDN
<https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-Proto>.

See also: "Network.HTTP.Headers.Forwarded", "Network.HTTP.Headers.XForwardedHost",
"Network.HTTP.Headers.XForwardedPort", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XForwardedProto (
  XForwardedProto (..),
  xForwardedProtoParser,
  renderXForwardedProto,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXForwardedProto)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | The forwarded scheme. @http@ and @https@ are recognised; any other scheme
token is retained verbatim as 'XForwardedProtoOther'.
-}
data XForwardedProto
  = XForwardedProtoHttp
  | XForwardedProtoHttps
  | XForwardedProtoOther ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader XForwardedProto where
  type ParseFailure XForwardedProto = String
  type Cardinality XForwardedProto = 'ZeroOrOne
  type Direction XForwardedProto = 'Request


  parseFromHeaders _ headers = case runParser xForwardedProtoParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Forwarded-Proto header: " <> show rest
    Fail -> Left "Failed to parse X-Forwarded-Proto header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXForwardedProto


  headerName _ = hXForwardedProto


xForwardedProtoParser :: ParserT st String XForwardedProto
xForwardedProtoParser = classify <$> rfc9110Token
  where
    classify t
      | t == "http" = XForwardedProtoHttp
      | t == "https" = XForwardedProtoHttps
      | otherwise = XForwardedProtoOther t


renderXForwardedProto :: XForwardedProto -> M.Builder
renderXForwardedProto = \case
  XForwardedProtoHttp -> "http"
  XForwardedProtoHttps -> "https"
  XForwardedProtoOther t -> shortText t
