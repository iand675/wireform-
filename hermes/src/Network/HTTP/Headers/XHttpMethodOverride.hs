{- |
@X-Http-Method-Override@ — de-facto request header letting a client tunnel a
real HTTP method (e.g. @PUT@, @DELETE@, @PATCH@) inside a @POST@ when an
intermediary, firewall, or client library cannot emit that method directly.
The receiving server replaces the request method with the header value before
dispatching.

This header is __not IANA-registered__. The value is a single HTTP method
token, surfaced as 'Network.HTTP.Methods.Method' (which normalises the token to
upper case, the universal method-name convention).

Spec (de-facto, not IANA-registered; no single specification — the closest
formal definition of the verb-tunneling convention is OData @X-HTTP-Method@,
[MS-ODATA]):
<https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-odata/bdbabfa6-8c4a-4741-85a9-8d93ffd66c41>.

See also: "Network.HTTP.Headers.Allow",
"Network.HTTP.Headers.AccessControlAllowMethods",
"Network.HTTP.Headers.AccessControlRequestMethod",
"Network.HTTP.Headers.Forwarded".
-}
module Network.HTTP.Headers.XHttpMethodOverride (
  XHttpMethodOverride (..),
  xHttpMethodOverrideParser,
  renderXHttpMethodOverride,
) where

import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXHttpMethodOverride)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Methods (Method, methodToBytes)


-- | The overridden HTTP request method.
newtype XHttpMethodOverride = XHttpMethodOverride {xHttpMethodOverrideMethod :: Method}
  deriving stock (Eq, Show)


instance KnownHeader XHttpMethodOverride where
  type ParseFailure XHttpMethodOverride = String
  type Cardinality XHttpMethodOverride = 'ZeroOrOne
  type Direction XHttpMethodOverride = 'Request


  parseFromHeaders _ headers = case runParser xHttpMethodOverrideParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Http-Method-Override header: " <> show rest
    Fail -> Left "Failed to parse X-Http-Method-Override header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXHttpMethodOverride


  headerName _ = hXHttpMethodOverride


xHttpMethodOverrideParser :: ParserT st String XHttpMethodOverride
xHttpMethodOverrideParser =
  XHttpMethodOverride . fromString . ST.toString <$> rfc9110Token


renderXHttpMethodOverride :: XHttpMethodOverride -> M.Builder
renderXHttpMethodOverride (XHttpMethodOverride m) =
  M.byteString (methodToBytes m)
