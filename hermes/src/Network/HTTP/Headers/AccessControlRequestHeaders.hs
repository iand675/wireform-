{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Request-Headers@ (WHATWG Fetch / W3C CORS) request header sent in
a preflight (@OPTIONS@) request to tell the server which header field-names the
actual cross-origin request will carry, so the server can approve them via
@Access-Control-Allow-Headers@.

== Grammar

@
Access-Control-Request-Headers = "*" / 1#field-name
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-request-headers>

See also: "Network.HTTP.Headers.AccessControlRequestMethod", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlMaxAge", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.AccessControlRequestHeaders (
  AccessControlRequestHeaders (..),
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


-- | Value of the @Access-Control-Request-Headers@ header.
data AccessControlRequestHeaders
  = -- | @*@ — a wildcard request for all headers.
    RequestHeadersWildcard
  | -- | An explicit, comma-separated list of field-names.
    RequestHeaders !(NE.NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


instance KnownHeader AccessControlRequestHeaders where
  type ParseFailure AccessControlRequestHeaders = String
  type Cardinality AccessControlRequestHeaders = 'ZeroOrOne
  type Direction AccessControlRequestHeaders = 'Request


  parseFromHeaders _ headers = case runParser accessControlRequestHeadersParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Request-Headers header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Request-Headers header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlRequestHeaders


  headerName _ = hAccessControlRequestHeaders


accessControlRequestHeadersParser :: ParserT st String AccessControlRequestHeaders
accessControlRequestHeadersParser = wildcard <|> list
  where
    wildcard = RequestHeadersWildcard <$ $(char '*')
    list = do
      first <- fieldName
      rest <- many (ows *> $(char ',') *> ows *> fieldName)
      pure $ RequestHeaders (first NE.:| rest)


renderAccessControlRequestHeaders :: AccessControlRequestHeaders -> M.Builder
renderAccessControlRequestHeaders = \case
  RequestHeadersWildcard -> "*"
  RequestHeaders names -> M.intersperse ", " (map shortText (NE.toList names))
