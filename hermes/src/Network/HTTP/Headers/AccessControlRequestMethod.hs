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
  accessControlRequestMethodParser,
  renderAccessControlRequestMethod,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlRequestMethod)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Value of the @Access-Control-Request-Method@ header: a single method token.
newtype AccessControlRequestMethod = AccessControlRequestMethod
  {requestMethod :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader AccessControlRequestMethod where
  type ParseFailure AccessControlRequestMethod = String
  type Cardinality AccessControlRequestMethod = 'ZeroOrOne
  type Direction AccessControlRequestMethod = 'Request


  parseFromHeaders _ headers = case runParser accessControlRequestMethodParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Request-Method header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Request-Method header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlRequestMethod


  headerName _ = hAccessControlRequestMethod


accessControlRequestMethodParser :: ParserT st String AccessControlRequestMethod
accessControlRequestMethodParser = AccessControlRequestMethod <$> rfc9110Token


renderAccessControlRequestMethod :: AccessControlRequestMethod -> M.Builder
renderAccessControlRequestMethod (AccessControlRequestMethod method) = shortText method
