{- |
@X-Http-Method-Override@ — de-facto request header letting a client tunnel a
real HTTP method (e.g. @PUT@, @DELETE@, @PATCH@) inside a @POST@ when an
intermediary, firewall, or client library cannot emit that method directly.
The receiving server replaces the request method with the header value before
dispatching.

The value is a single, case-sensitive HTTP method token, represented by
'Network.HTTP.Methods.Method'.

The Google Cloud API system parameters document this convention:
<https://docs.cloud.google.com/apis/docs/system-parameters#definitions>.

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

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXHttpMethodOverride)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Methods (Method, methodBytesChecked, methodToBytes, methodTokenParser, mkMethodWithOWS)


-- | The overridden HTTP request method.
newtype XHttpMethodOverride = XHttpMethodOverride {xHttpMethodOverrideMethod :: Method}
  deriving stock (Eq, Show)


instance KnownHeader XHttpMethodOverride where
  type ParseFailure XHttpMethodOverride = String
  type Cardinality XHttpMethodOverride = 'ZeroOrOne
  type Direction XHttpMethodOverride = 'Request


  parseFromHeaders _ (wire :| []) =
    case mkMethodWithOWS wire of
      Right method -> Right (XHttpMethodOverride method)
      Left _ -> Left "Failed to parse X-Http-Method-Override header"
  parseFromHeaders _ _ = Left "Multiple X-Http-Method-Override field lines"
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ = methodToBytes . xHttpMethodOverrideMethod


  renderToHeadersChecked _ = methodBytesChecked . xHttpMethodOverrideMethod
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hXHttpMethodOverride


xHttpMethodOverrideParser :: ParserT st String XHttpMethodOverride
xHttpMethodOverrideParser =
  ows *> (XHttpMethodOverride <$> methodTokenParser) <* ows


renderXHttpMethodOverride :: XHttpMethodOverride -> M.Builder
renderXHttpMethodOverride (XHttpMethodOverride m) =
  M.byteString (methodToBytes m)
