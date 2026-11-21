{- |
@Access-Control-Expose-Headers@ (WHATWG Fetch / W3C CORS) response header listing
the response header field-names that browsers are allowed to expose to client
script (beyond the CORS-safelisted set such as @Content-Type@ and @Cache-Control@).
The value @*@ exposes every permitted header outside credentials mode @include@.

== Grammar

@
Access-Control-Expose-Headers = #field-name
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-expose-headers>

See also: "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlAllowCredentials", "Network.HTTP.Headers.TimingAllowOrigin", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.AccessControlExposeHeaders (
  AccessControlExposeHeaders (..),
  exposesResponseHeader,
  accessControlExposeHeadersParser,
  renderAccessControlExposeHeaders,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Headers
import Network.HTTP.Headers.AccessControlAllowCredentials (CorsCredentialsMode (..))
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, hAccessControlExposeHeaders, headerNameToShortByteString)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedTokensChecked, shortText)


-- | Value of the @Access-Control-Expose-Headers@ header.
data AccessControlExposeHeaders
  = -- | A singleton @*@ entry. See 'exposesResponseHeader'.
    ExposeHeadersWildcard
  | -- | An explicit, comma-separated list of field-names.
    ExposeHeaders !(NE.NonEmpty ST.ShortText)
  | ExposeHeadersEmpty
  deriving stock (Eq, Show)


instance KnownHeader AccessControlExposeHeaders where
  type ParseFailure AccessControlExposeHeaders = String
  type Cardinality AccessControlExposeHeaders = 'ZeroOrOne
  type Direction AccessControlExposeHeaders = 'Response


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Access-Control-Expose-Headers" (httpList fieldName)


  renderToHeaders _ = M.toStrictByteString . renderAccessControlExposeHeaders


  renderToHeadersChecked _ ExposeHeadersEmpty = Just ""
  renderToHeadersChecked _ ExposeHeadersWildcard = Just "*"
  renderToHeadersChecked _ (ExposeHeaders entries) = encodeCommaSeparatedTokensChecked entries


  headerName _ = hAccessControlExposeHeaders


accessControlExposeHeadersParser :: ParserT st String AccessControlExposeHeaders
accessControlExposeHeadersParser = fromEntries <$> httpList fieldName


fromEntries :: [ST.ShortText] -> AccessControlExposeHeaders
fromEntries [] = ExposeHeadersEmpty
fromEntries ["*"] = ExposeHeadersWildcard
fromEntries (first : rest) = ExposeHeaders (first NE.:| rest)


{- | Determine whether a field name is exposed on a CORS-filtered response.
Fetch's response safelist applies even when the field is absent. Cookie
response fields are excluded even when explicitly listed.
-}
exposesResponseHeader :: CorsCredentialsMode -> HeaderFieldName -> Maybe AccessControlExposeHeaders -> Bool
exposesResponseHeader mode header names
  | matches "set-cookie" || matches "set-cookie2" = False
  | any matches safelisted = True
  | otherwise = case names of
      Just ExposeHeadersWildcard -> matchesEntry mode name "*"
      Just (ExposeHeaders (first NE.:| rest)) -> matchesEntry mode name first || go mode name rest
      _ -> False
  where
    name = STU.fromShortByteStringUnsafe (headerNameToShortByteString header)
    matches = sameNegotiationToken name
    safelisted = ["cache-control", "content-language", "content-length", "content-type", "expires", "last-modified", "pragma"]
    matchesEntry credentials requested entry =
      sameNegotiationToken requested entry || (entry == "*" && credentials /= CorsInclude)
    go _ _ [] = False
    go credentials requested (entry : rest) =
      matchesEntry credentials requested entry || go credentials requested rest


renderAccessControlExposeHeaders :: AccessControlExposeHeaders -> M.Builder
renderAccessControlExposeHeaders = \case
  ExposeHeadersEmpty -> mempty
  ExposeHeadersWildcard -> "*"
  ExposeHeaders names -> M.intersperse ", " (map shortText (NE.toList names))
