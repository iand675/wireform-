{- |
@Access-Control-Allow-Methods@ (WHATWG Fetch / W3C CORS) response header
specifying the HTTP methods allowed when accessing the resource, returned in
reply to a preflight (@OPTIONS@) request. The value @*@ acts as a wildcard
outside credentials mode @include@.

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
  allowMethodsMatches,
  accessControlAllowMethodsParser,
  renderAccessControlAllowMethods,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.AccessControlAllowCredentials (CorsCredentialsMode (..))
import Network.HTTP.Headers.AccessControlRequestMethod (AccessControlRequestMethod (..))
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowMethods)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedTokensChecked, shortText)


-- | Value of the @Access-Control-Allow-Methods@ header.
data AccessControlAllowMethods
  = -- | A singleton @*@ entry. See 'allowMethodsMatches'.
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


  renderToHeadersChecked _ AllowMethodsEmpty = Just ""
  renderToHeadersChecked _ AllowMethodsWildcard = Just "*"
  renderToHeadersChecked _ (AllowMethods entries) = encodeCommaSeparatedTokensChecked entries


  headerName _ = hAccessControlAllowMethods


accessControlAllowMethodsParser :: ParserT st String AccessControlAllowMethods
accessControlAllowMethodsParser = fromEntries <$> httpList rfc9110Token


fromEntries :: [ST.ShortText] -> AccessControlAllowMethods
fromEntries [] = AllowMethodsEmpty
fromEntries ["*"] = AllowMethodsWildcard
fromEntries (first : rest) = AllowMethods (first NE.:| rest)


-- | Match a case-sensitive request method against the response's allow list.
allowMethodsMatches :: CorsCredentialsMode -> AccessControlRequestMethod -> AccessControlAllowMethods -> Bool
allowMethodsMatches mode (AccessControlRequestMethod method) = \case
  AllowMethodsEmpty -> False
  AllowMethodsWildcard -> matches mode method "*"
  AllowMethods (first NE.:| rest) -> matches mode method first || go mode method rest
  where
    matches credentials requested entry = requested == entry || (entry == "*" && credentials /= CorsInclude)
    go _ _ [] = False
    go credentials requested (entry : rest) =
      matches credentials requested entry || go credentials requested rest


renderAccessControlAllowMethods :: AccessControlAllowMethods -> M.Builder
renderAccessControlAllowMethods = \case
  AllowMethodsEmpty -> mempty
  AllowMethodsWildcard -> "*"
  AllowMethods methods -> M.intersperse ", " (map shortText (NE.toList methods))
