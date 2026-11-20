{-# LANGUAGE PatternSynonyms #-}

{- |
@Access-Control-Request-Headers@ (WHATWG Fetch / W3C CORS) request header sent in
a preflight (@OPTIONS@) request to tell the server which header field-names the
actual cross-origin request will carry, so the server can approve them via
@Access-Control-Allow-Headers@.

== Grammar

@
Access-Control-Request-Headers = 1#field-name
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-request-headers>

See also: "Network.HTTP.Headers.AccessControlRequestMethod", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlMaxAge", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.AccessControlRequestHeaders (
  AccessControlRequestHeaders (RequestHeaders, RequestHeadersWildcard),
  accessControlRequestHeadersParser,
  renderAccessControlRequestHeaders,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlRequestHeaders)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The non-empty list of requested field names.
newtype AccessControlRequestHeaders = RequestHeaders (NE.NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


-- | Compatibility spelling for a request listing the literal field name @*@.
pattern RequestHeadersWildcard :: AccessControlRequestHeaders
pattern RequestHeadersWildcard = RequestHeaders ("*" NE.:| [])


instance KnownHeader AccessControlRequestHeaders where
  type ParseFailure AccessControlRequestHeaders = String
  type Cardinality AccessControlRequestHeaders = 'ZeroOrOne
  type Direction AccessControlRequestHeaders = 'Request


  parseFromHeaders _ headers = do
    entries <- parseListFieldLines "Access-Control-Request-Headers" (httpList fieldName) headers
    case NE.nonEmpty entries of
      Nothing -> Left "Access-Control-Request-Headers requires a field name"
      Just names -> Right (RequestHeaders names)


  renderToHeaders _ = M.toStrictByteString . renderAccessControlRequestHeaders


  headerName _ = hAccessControlRequestHeaders


accessControlRequestHeadersParser :: ParserT st String AccessControlRequestHeaders
accessControlRequestHeadersParser = do
  entries <- httpList fieldName
  case NE.nonEmpty entries of
    Nothing -> err "Access-Control-Request-Headers requires a field name"
    Just names -> pure (RequestHeaders names)


renderAccessControlRequestHeaders :: AccessControlRequestHeaders -> M.Builder
renderAccessControlRequestHeaders (RequestHeaders names) =
  M.intersperse ", " (map shortText (NE.toList names))
