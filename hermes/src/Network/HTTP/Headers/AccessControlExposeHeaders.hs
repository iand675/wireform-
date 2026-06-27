{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Expose-Headers@ (WHATWG Fetch / W3C CORS) response header listing
the response header field-names that browsers are allowed to expose to client
script (beyond the CORS-safelisted set such as @Content-Type@ and @Cache-Control@).
The value @*@ exposes every header for requests without credentials.

== Grammar

@
Access-Control-Expose-Headers = "*" / 1#field-name
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-expose-headers>

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlAllowCredentials", "Network.HTTP.Headers.TimingAllowOrigin", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.AccessControlExposeHeaders (
  AccessControlExposeHeaders (..),
  accessControlExposeHeadersParser,
  renderAccessControlExposeHeaders,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlExposeHeaders)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Value of the @Access-Control-Expose-Headers@ header.
data AccessControlExposeHeaders
  = -- | @*@ — expose all response headers (for requests without credentials).
    ExposeHeadersWildcard
  | -- | An explicit, comma-separated list of field-names.
    ExposeHeaders !(NE.NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


instance KnownHeader AccessControlExposeHeaders where
  type ParseFailure AccessControlExposeHeaders = String
  type Cardinality AccessControlExposeHeaders = 'ZeroOrOne
  type Direction AccessControlExposeHeaders = 'Response


  parseFromHeaders _ headers = case runParser accessControlExposeHeadersParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Expose-Headers header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Expose-Headers header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlExposeHeaders


  headerName _ = hAccessControlExposeHeaders


accessControlExposeHeadersParser :: ParserT st String AccessControlExposeHeaders
accessControlExposeHeadersParser = wildcard <|> list
  where
    wildcard = ExposeHeadersWildcard <$ $(char '*')
    list = do
      first <- fieldName
      rest <- many (ows *> $(char ',') *> ows *> fieldName)
      pure $ ExposeHeaders (first NE.:| rest)


renderAccessControlExposeHeaders :: AccessControlExposeHeaders -> M.Builder
renderAccessControlExposeHeaders = \case
  ExposeHeadersWildcard -> "*"
  ExposeHeaders names -> M.intersperse ", " (map shortText (NE.toList names))
