{-# LANGUAGE TemplateHaskell #-}

{- |
@Access-Control-Allow-Methods@ (WHATWG Fetch / W3C CORS) response header
specifying the HTTP methods allowed when accessing the resource, returned in
reply to a preflight (@OPTIONS@) request. The value @*@ acts as a wildcard for
requests without credentials.

== Grammar

@
Access-Control-Allow-Methods = "*" / 1#method
method                       = token
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-allow-methods>

See also: "Network.HTTP.Headers.AccessControlRequestMethod", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlMaxAge", "Network.HTTP.Headers.Allow".
-}
module Network.HTTP.Headers.AccessControlAllowMethods (
  AccessControlAllowMethods (..),
  accessControlAllowMethodsParser,
  renderAccessControlAllowMethods,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowMethods)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Value of the @Access-Control-Allow-Methods@ header.
data AccessControlAllowMethods
  = -- | @*@ — any method (for requests without credentials).
    AllowMethodsWildcard
  | -- | An explicit, comma-separated list of method tokens.
    AllowMethods !(NE.NonEmpty ST.ShortText)
  deriving stock (Eq, Show)


instance KnownHeader AccessControlAllowMethods where
  type ParseFailure AccessControlAllowMethods = String
  type Cardinality AccessControlAllowMethods = 'ZeroOrOne
  type Direction AccessControlAllowMethods = 'Response


  parseFromHeaders _ headers = case runParser accessControlAllowMethodsParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Access-Control-Allow-Methods header: " <> show rest
    Fail -> Left "Failed to parse Access-Control-Allow-Methods header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowMethods


  headerName _ = hAccessControlAllowMethods


accessControlAllowMethodsParser :: ParserT st String AccessControlAllowMethods
accessControlAllowMethodsParser = wildcard <|> list
  where
    wildcard = AllowMethodsWildcard <$ $(char '*')
    list = do
      first <- rfc9110Token
      rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
      pure $ AllowMethods (first NE.:| rest)


renderAccessControlAllowMethods :: AccessControlAllowMethods -> M.Builder
renderAccessControlAllowMethods = \case
  AllowMethodsWildcard -> "*"
  AllowMethods methods -> M.intersperse ", " (map shortText (NE.toList methods))
