{- |
@Proxy-Authentication-Info@ — the proxy-side analogue of
@Authentication-Info@: a response header in which an intermediary proxy
carries the final, scheme-specific data it communicates at the end of a
successful authentication exchange with the client.

== Grammar (RFC 7615 §4)

@
Proxy-Authentication-Info = #auth-param
auth-param                = token BWS "=" BWS ( token / quoted-string )
@

Identical shape to @Authentication-Info@: a comma-separated list of
@auth-param@s, surfaced generically as 'CredentialParam' (token or
quoted-string) in document order so the field round-trips.  Parameter
names are case-insensitive per RFC 9110 §11.2; the on-the-wire
spelling is preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc7615#section-4>

See also: "Network.HTTP.Headers.AuthenticationInfo",
"Network.HTTP.Headers.ProxyAuthenticate",
"Network.HTTP.Headers.ProxyAuthorization".
-}
module Network.HTTP.Headers.ProxyAuthenticationInfo (
  ProxyAuthenticationInfo (..),
  proxyAuthenticationInfoParser,
  renderProxyAuthenticationInfo,
  CredentialParam (..),
) where

import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (CredentialParam (..), authParamParser, encodeAuthParamsChecked, renderAuthParam)
import Network.HTTP.Headers.HeaderFieldName (hProxyAuthenticationInfo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | The @Proxy-Authentication-Info@ header value: an ordered
@auth-param@ list.
-}
newtype ProxyAuthenticationInfo = ProxyAuthenticationInfo
  { proxyAuthenticationInfoParams :: [(ShortText, CredentialParam)]
  }
  deriving stock (Eq, Show)


instance KnownHeader ProxyAuthenticationInfo where
  type ParseFailure ProxyAuthenticationInfo = String
  type Cardinality ProxyAuthenticationInfo = 'ZeroOrMore
  type Direction ProxyAuthenticationInfo = 'Response


  parseFromHeaders _ = fmap ProxyAuthenticationInfo . parseListFieldLines "Proxy-Authentication-Info" (httpList authParamParser)


  renderToHeaders _ pai = [M.toStrictByteString (renderProxyAuthenticationInfo pai)]


  renderToHeadersChecked _ = fmap pure . encodeAuthParamsChecked . proxyAuthenticationInfoParams


  headerName _ = hProxyAuthenticationInfo


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

{- | Parse a @Proxy-Authentication-Info@ value: a comma-separated
@auth-param@ list.
-}
proxyAuthenticationInfoParser :: ParserT st String ProxyAuthenticationInfo
proxyAuthenticationInfoParser = ProxyAuthenticationInfo <$> httpList authParamParser


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderProxyAuthenticationInfo :: ProxyAuthenticationInfo -> M.Builder
renderProxyAuthenticationInfo (ProxyAuthenticationInfo ps) =
  M.intersperse ", " (map renderAuthParam ps)
