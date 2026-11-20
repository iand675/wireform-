{- |
@Authentication-Info@ — a response header carrying the final,
scheme-specific data a server communicates at the end of a successful
authentication exchange (for example Digest's @nextnonce@, @rspauth@,
@qop@, @cnonce@, and @nc@), letting the server authenticate itself to
the client and supply state for the next request.

== Grammar (RFC 7615 §3)

@
Authentication-Info = #auth-param
auth-param          = token BWS "=" BWS ( token / quoted-string )
@

A comma-separated list of @auth-param@s.  The parameter vocabulary is
scheme-defined, so parameters are surfaced generically as the shared
'CredentialParam' (token or quoted-string) and kept in document order
so the field round-trips.  Parameter names are case-insensitive per
RFC 9110 §11.2; the on-the-wire spelling is preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc7615#section-3>

See also: "Network.HTTP.Headers.ProxyAuthenticationInfo",
"Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.Authorization",
"Network.HTTP.Headers.AuthenticationControl".
-}
module Network.HTTP.Headers.AuthenticationInfo (
  AuthenticationInfo (..),
  authenticationInfoParser,
  renderAuthenticationInfo,
  CredentialParam (..),
) where

import Data.Text.Short (ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (CredentialParam (..), authParamParser, renderAuthParam)
import Network.HTTP.Headers.HeaderFieldName (hAuthenticationInfo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | The @Authentication-Info@ header value: an ordered @auth-param@ list.
newtype AuthenticationInfo = AuthenticationInfo
  { authenticationInfoParams :: [(ShortText, CredentialParam)]
  }
  deriving stock (Eq, Show)


instance KnownHeader AuthenticationInfo where
  type ParseFailure AuthenticationInfo = String
  type Cardinality AuthenticationInfo = 'ZeroOrMore
  type Direction AuthenticationInfo = 'Response


  parseFromHeaders _ = fmap AuthenticationInfo . parseListFieldLines "Authentication-Info" (httpList authParamParser)


  renderToHeaders _ ai = [M.toStrictByteString (renderAuthenticationInfo ai)]


  headerName _ = hAuthenticationInfo


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

-- | Parse an @Authentication-Info@ value: a comma-separated @auth-param@ list.
authenticationInfoParser :: ParserT st String AuthenticationInfo
authenticationInfoParser = AuthenticationInfo <$> httpList authParamParser


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderAuthenticationInfo :: AuthenticationInfo -> M.Builder
renderAuthenticationInfo (AuthenticationInfo ps) =
  M.intersperse ", " (map renderAuthParam ps)
