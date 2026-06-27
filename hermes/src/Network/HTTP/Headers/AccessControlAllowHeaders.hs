{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Allow-Headers@ (WHATWG Fetch / W3C CORS) response header used in
a preflight (@OPTIONS@) response to indicate which request header field-names a
client may include in the actual cross-origin request. The value @*@ acts as a
wildcard for requests without credentials.

== Grammar

@
Access-Control-Allow-Headers = "*" / 1#field-name
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-allow-headers>

See also: "Network.HTTP.Headers.AccessControlRequestHeaders", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlMaxAge", "Network.HTTP.Headers.AccessControlExposeHeaders".
-}
module Network.HTTP.Headers.AccessControlAllowHeaders (
  AccessControlAllowHeaders (..),
  accessControlAllowHeadersParser,
  renderAccessControlAllowHeaders,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowHeaders)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Value of the @Access-Control-Allow-Headers@ header.
data AccessControlAllowHeaders
  = -- | @*@ — any header (for requests without credentials).
    AllowHeadersWildcard
  | -- | An explicit, comma-separated list of field-names.
    AllowHeaders !(NE.NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


instance KnownHeader AccessControlAllowHeaders where
  type ParseFailure AccessControlAllowHeaders = String
  type Cardinality AccessControlAllowHeaders = 'ZeroOrOne
  type Direction AccessControlAllowHeaders = 'Response


  parseFromHeaders _ headers = case runParser accessControlAllowHeadersParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Allow-Headers header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Allow-Headers header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowHeaders


  headerName _ = hAccessControlAllowHeaders


accessControlAllowHeadersParser :: ParserT st String AccessControlAllowHeaders
accessControlAllowHeadersParser = wildcard <|> list
  where
    wildcard = AllowHeadersWildcard <$ $(char '*')
    list = do
      first <- fieldName
      rest <- many (ows *> $(char ',') *> ows *> fieldName)
      pure $ AllowHeaders (first NE.:| rest)


renderAccessControlAllowHeaders :: AccessControlAllowHeaders -> M.Builder
renderAccessControlAllowHeaders = \case
  AllowHeadersWildcard -> "*"
  AllowHeaders names -> M.intersperse ", " (map shortText (NE.toList names))
