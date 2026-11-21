{- |
@Access-Control-Allow-Headers@ (WHATWG Fetch / W3C CORS) response header used in
a preflight (@OPTIONS@) response to indicate which request header field-names a
client may include in the actual cross-origin request. The value @*@ acts as a
wildcard outside credentials mode @include@, with an exception for @Authorization@.

== Grammar

@
Access-Control-Allow-Headers = #field-name
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-allow-headers>

See also: "Network.HTTP.Headers.AccessControlRequestHeaders", "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlAllowOrigin", "Network.HTTP.Headers.AccessControlMaxAge", "Network.HTTP.Headers.AccessControlExposeHeaders".
-}
module Network.HTTP.Headers.AccessControlAllowHeaders (
  AccessControlAllowHeaders (..),
  allowHeadersMatches,
  accessControlAllowHeadersParser,
  renderAccessControlAllowHeaders,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (sameNegotiationToken)
import Network.HTTP.Headers
import Network.HTTP.Headers.AccessControlAllowCredentials (CorsCredentialsMode (..))
import Network.HTTP.Headers.HeaderFieldName (hAccessControlAllowHeaders)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedTokensChecked, shortText)


-- | Value of the @Access-Control-Allow-Headers@ header.
data AccessControlAllowHeaders
  = -- | A singleton @*@ entry. See 'allowHeadersMatches'.
    AllowHeadersWildcard
  | -- | An explicit, comma-separated list of field-names.
    AllowHeaders !(NE.NonEmpty ST.ShortText)
  | AllowHeadersEmpty
  deriving stock (Eq, Show)


instance KnownHeader AccessControlAllowHeaders where
  type ParseFailure AccessControlAllowHeaders = String
  type Cardinality AccessControlAllowHeaders = 'ZeroOrOne
  type Direction AccessControlAllowHeaders = 'Response


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Access-Control-Allow-Headers" (httpList fieldName)


  renderToHeaders _ = M.toStrictByteString . renderAccessControlAllowHeaders


  renderToHeadersChecked _ AllowHeadersEmpty = Just ""
  renderToHeadersChecked _ AllowHeadersWildcard = Just "*"
  renderToHeadersChecked _ (AllowHeaders entries) = encodeCommaSeparatedTokensChecked entries


  headerName _ = hAccessControlAllowHeaders


accessControlAllowHeadersParser :: ParserT st String AccessControlAllowHeaders
accessControlAllowHeadersParser = fromEntries <$> httpList fieldName


fromEntries :: [ST.ShortText] -> AccessControlAllowHeaders
fromEntries [] = AllowHeadersEmpty
fromEntries ["*"] = AllowHeadersWildcard
fromEntries (first : rest) = AllowHeaders (first NE.:| rest)


{- | Match a requested field name against the response's allow list.
Fetch requires an explicit entry for @Authorization@ in every mode.
-}
allowHeadersMatches :: CorsCredentialsMode -> ST.ShortText -> AccessControlAllowHeaders -> Bool
allowHeadersMatches mode name = \case
  AllowHeadersEmpty -> False
  AllowHeadersWildcard -> matches mode name "*"
  AllowHeaders (first NE.:| rest) -> matches mode name first || go mode name rest
  where
    matches credentials requested entry =
      sameNegotiationToken requested entry
        || (entry == "*" && credentials /= CorsInclude && not (sameNegotiationToken requested "authorization"))
    go _ _ [] = False
    go credentials requested (entry : rest) =
      matches credentials requested entry || go credentials requested rest


renderAccessControlAllowHeaders :: AccessControlAllowHeaders -> M.Builder
renderAccessControlAllowHeaders = \case
  AllowHeadersEmpty -> mempty
  AllowHeadersWildcard -> "*"
  AllowHeaders names -> M.intersperse ", " (map shortText (NE.toList names))
