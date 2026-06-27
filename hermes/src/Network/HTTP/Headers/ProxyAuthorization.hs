{- |
@Proxy-Authorization@ — the request header a client uses to present
credentials to an intermediary proxy in response to that proxy's
@Proxy-Authenticate@ challenge on a @407 Proxy Authentication Required@.
It is the proxy-tier counterpart of @Authorization@ and carries the same
single @credentials@ value (an auth-scheme plus a @token68@ blob or
@auth-param@ list); it is consumed hop-by-hop by the proxy rather than
forwarded to the origin server.

== Grammar (RFC 9110 §11.7.2)

@
Proxy-Authorization = credentials
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-11.7.2>

See also: "Network.HTTP.Headers.ProxyAuthenticate",
"Network.HTTP.Headers.Authorization",
"Network.HTTP.Headers.ProxyAuthenticationInfo".
-}
module Network.HTTP.Headers.ProxyAuthorization where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization
import Network.HTTP.Headers.HeaderFieldName (hProxyAuthorization)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ProxyAuthorization = ProxyAuthorization {proxyAuthorizationCredentials :: Credentials}
  deriving stock (Eq, Show)


instance KnownHeader ProxyAuthorization where
  type ParseFailure ProxyAuthorization = String
  type Cardinality ProxyAuthorization = 'ZeroOrOne
  type Direction ProxyAuthorization = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser credentialsParser header of
      OK creds "" -> Right $ ProxyAuthorization creds
      OK _ rest -> Left $ "Unconsumed input after parsing Proxy-Authorization header: " <> show rest
      Fail -> Left "Failed to parse Proxy-Authorization header"
      Err e -> Left e


  renderToHeaders _ (ProxyAuthorization creds) = M.toStrictByteString $ renderCredentials creds


  headerName _ = hProxyAuthorization
