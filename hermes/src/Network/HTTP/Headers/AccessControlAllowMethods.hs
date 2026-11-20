{- |
@Access-Control-Allow-Methods@ (WHATWG Fetch / W3C CORS) response header
specifying the HTTP methods allowed when accessing the resource, returned in
reply to a preflight (@OPTIONS@) request. The value @*@ acts as a wildcard for
requests without credentials.

== Grammar

@
Access-Control-Allow-Methods = #method
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
  | AllowMethodsEmpty
  deriving stock (Eq, Show)


instance KnownHeader AccessControlAllowMethods where
  type ParseFailure AccessControlAllowMethods = String
  type Cardinality AccessControlAllowMethods = 'ZeroOrOne
  type Direction AccessControlAllowMethods = 'Response


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Access-Control-Allow-Methods" (httpList rfc9110Token)


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowMethods


  headerName _ = hAccessControlAllowMethods


accessControlAllowMethodsParser :: ParserT st String AccessControlAllowMethods
accessControlAllowMethodsParser = fromEntries <$> httpList rfc9110Token


fromEntries :: [ST.ShortText] -> AccessControlAllowMethods
fromEntries [] = AllowMethodsEmpty
fromEntries ["*"] = AllowMethodsWildcard
fromEntries (first : rest) = AllowMethods (first NE.:| rest)


renderAccessControlAllowMethods :: AccessControlAllowMethods -> M.Builder
renderAccessControlAllowMethods = \case
  AllowMethodsEmpty -> mempty
  AllowMethodsWildcard -> "*"
  AllowMethods methods -> M.intersperse ", " (map shortText (NE.toList methods))
