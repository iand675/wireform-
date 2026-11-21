{-# LANGUAGE TemplateHaskell #-}

{- |
@X-Forwarded-Host@ — de-facto request header set by reverse proxies to convey
the original @Host@ requested by the client before the request was proxied.

This header is __not IANA-registered__ and is superseded by the @host@
parameter of the standardized @Forwarded@ header
(<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>). The value is nominally a
single host (optionally with a @:port@), but deployments occasionally emit
comma-separated chains. Each authority is validated and the original
spelling and spacing within the chain are retained.

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

import Data.Either (isRight)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (hostAndPortListMemberBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXForwardedHost)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.Parser.Driver (parseByteString)


{- | The forwarded host value, preserved verbatim
(e.g. @\"example.com\"@ or @\"example.com:8443\"@).
-}
newtype XForwardedHost = XForwardedHost {xForwardedHostValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XForwardedHost where
  type ParseFailure XForwardedHost = String
  type Cardinality XForwardedHost = 'ZeroOrOne
  type Direction XForwardedHost = 'Request


  parseFromHeaders _ = parseSingleFieldLine "X-Forwarded-Host" xForwardedHostParser


  renderToHeaders _ = M.toStrictByteString . renderXForwardedHost


  renderToHeadersChecked _ (XForwardedHost host)
    | isRight (parseByteString (authorityList <* eof) wire) = Just wire
    | otherwise = Nothing
    where
      wire = ST.toByteString host


  headerName _ = hXForwardedHost


xForwardedHostParser :: ParserT st String XForwardedHost
xForwardedHostParser = XForwardedHost <$> shortASCIIFromParser_ authorityList


authorityList :: ParserT st String ()
authorityList = authority *> skipMany (ows *> $(char ',') *> ows *> authority)
  where
    authority = hostAndPortListMemberBytes


renderXForwardedHost :: XForwardedHost -> M.Builder
renderXForwardedHost (XForwardedHost h) = shortText h
