{- |
@Proxy-Authenticate@ — a response header sent by an intermediary proxy
on a @407 Proxy Authentication Required@ response to challenge the client
for credentials to the proxy itself (rather than the origin server). It
is the proxy-tier counterpart of @WWW-Authenticate@ and carries the same
comma-separated @#challenge@ list.

== Grammar (RFC 9110 §11.7.1)

@
Proxy-Authenticate = #challenge
@

The grammar is identical to @WWW-Authenticate@ (RFC 9110 §11.6.1); only
the field name and HTTP-semantic role differ.  We reuse the parser and
renderer from "Network.HTTP.Headers.WWWAuthenticate" wholesale and wrap
the result in a distinct newtype so the 'KnownHeader' instance can pin
the right 'HeaderFieldName'.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-11.7.1>

See also: "Network.HTTP.Headers.WWWAuthenticate",
"Network.HTTP.Headers.ProxyAuthorization",
"Network.HTTP.Headers.ProxyAuthenticationInfo".
-}
module Network.HTTP.Headers.ProxyAuthenticate (
  ProxyAuthenticate (..),
  proxyAuthenticateParser,
  renderProxyAuthenticate,

  -- * Re-exports
  AuthChallenge (..),
  ChallengeContents (..),
  AuthScheme (..),
  CredentialParam (..),
) where

import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyAuthenticate)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.WWWAuthenticate (
  AuthChallenge (..),
  AuthScheme (..),
  ChallengeContents (..),
  CredentialParam (..),
  challengesParser,
  parseChallengeFieldLines,
  renderAuthChallenge,
 )


-- | Challenge list for the proxy auth tier.
newtype ProxyAuthenticate = ProxyAuthenticate
  { proxyAuthChallenges :: [AuthChallenge]
  }
  deriving stock (Eq, Show)


instance KnownHeader ProxyAuthenticate where
  type ParseFailure ProxyAuthenticate = String
  type Cardinality ProxyAuthenticate = 'ZeroOrMore
  type Direction ProxyAuthenticate = 'Response


  parseFromHeaders _ = fmap ProxyAuthenticate . parseChallengeFieldLines "Proxy-Authenticate"


  renderToHeaders _ (ProxyAuthenticate cs) =
    [M.toStrictByteString (renderProxyAuthenticate (ProxyAuthenticate cs))]


  headerName _ = hProxyAuthenticate


proxyAuthenticateParser :: ParserT st String ProxyAuthenticate
proxyAuthenticateParser = ProxyAuthenticate <$> challengesParser


renderProxyAuthenticate :: ProxyAuthenticate -> M.Builder
renderProxyAuthenticate (ProxyAuthenticate cs) =
  M.intersperse ", " (map renderAuthChallenge cs)
