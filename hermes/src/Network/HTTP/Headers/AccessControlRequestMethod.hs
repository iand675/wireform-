{- |
@Access-Control-Request-Method@ (WHATWG Fetch / W3C CORS) request header sent in
a preflight (@OPTIONS@) request to tell the server which HTTP method the actual
cross-origin request will use, so the server can approve it via
@Access-Control-Allow-Methods@.

== Grammar

@
Access-Control-Request-Method = method
method                        = token
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-request-method>

See also: "Network.HTTP.Headers.AccessControlRequestHeaders", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlMaxAge", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.AccessControlRequestMethod (
  AccessControlRequestMethod (..),
  requestMethodAsMethod,
  requestMethodFromMethod,
  accessControlRequestMethodParser,
  renderAccessControlRequestMethod,
) where

import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlRequestMethod)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Network.HTTP.Methods (Method, MethodError, methodToBytes, mkMethod)


-- | Value of the @Access-Control-Request-Method@ header: a single method token.
newtype AccessControlRequestMethod = AccessControlRequestMethod
  {requestMethod :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader AccessControlRequestMethod where
  type ParseFailure AccessControlRequestMethod = String
  type Cardinality AccessControlRequestMethod = 'ZeroOrOne
  type Direction AccessControlRequestMethod = 'Request


  parseFromHeaders _ = parseTokenFieldLine "Access-Control-Request-Method" AccessControlRequestMethod


  renderToHeaders _ = ST.toByteString . requestMethod


  headerName _ = hAccessControlRequestMethod


accessControlRequestMethodParser :: ParserT st String AccessControlRequestMethod
accessControlRequestMethodParser = AccessControlRequestMethod <$> rfc9110Token


-- | Interpret the case-sensitive token as the shared HTTP method type.
requestMethodAsMethod :: AccessControlRequestMethod -> Either MethodError Method
requestMethodAsMethod = mkMethod . ST.toByteString . requestMethod


-- | Validate the method's wire spelling before authoring the header.
requestMethodFromMethod :: Method -> Either MethodError AccessControlRequestMethod
requestMethodFromMethod method = AccessControlRequestMethod (STU.fromByteStringUnsafe wire) <$ mkMethod wire
  where
    wire = methodToBytes method


renderAccessControlRequestMethod :: AccessControlRequestMethod -> M.Builder
renderAccessControlRequestMethod (AccessControlRequestMethod method) = shortText method
