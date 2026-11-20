{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@X-Forwarded-Proto@ — de-facto request header set by reverse proxies to convey
the protocol (URI scheme) the client used to reach the proxy.

This header is __not IANA-registered__ and is superseded by the @proto@
parameter of the standardized @Forwarded@ header
(<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>). The overwhelmingly common
values are @http@ and @https@, recognized case-insensitively. Other values
follow the URI scheme grammar and retain their spelling.

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

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (uriSchemeBytes)
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


  parseFromHeaders _ ("http" :| []) = Right XForwardedProtoHttp
  parseFromHeaders _ ("https" :| []) = Right XForwardedProtoHttps
  parseFromHeaders _ fields = parseSingleFieldLine "X-Forwarded-Proto" xForwardedProtoParser fields


  renderToHeaders _ XForwardedProtoHttp = "http"
  renderToHeaders _ XForwardedProtoHttps = "https"
  renderToHeaders _ (XForwardedProtoOther scheme) = ST.toByteString scheme


  headerName _ = hXForwardedProto


xForwardedProtoParser :: ParserT st String XForwardedProto
xForwardedProtoParser =
  $( switchAsciiCI
       [|
         case _ of
           "http" -> XForwardedProtoHttp <$ endOfScheme
           "https" -> XForwardedProtoHttps <$ endOfScheme
         |]
   )
    <|> (XForwardedProtoOther <$> shortASCIIFromParser_ uriSchemeBytes)
  where
    endOfScheme = notFollowedBy tokenBytes


renderXForwardedProto :: XForwardedProto -> M.Builder
renderXForwardedProto = \case
  XForwardedProtoHttp -> "http"
  XForwardedProtoHttps -> "https"
  XForwardedProtoOther t -> shortText t
