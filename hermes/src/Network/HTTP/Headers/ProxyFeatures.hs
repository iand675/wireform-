{- |
@Proxy-Features@ — obsolete header from the W3C "Notification for Proxy Caches"
note, by which a proxy advertised the feature-notification facilities it
supports. The note never became a standard, so the value is preserved verbatim
as an opaque string.

Spec: W3C Working Draft <https://www.w3.org/TR/WD-proxy Notification for Proxy Caches> (never standardized).

See also: "Network.HTTP.Headers.ProxyInstruction", "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.ProxyFeatures (
  ProxyFeatures (..),
  proxyFeaturesParser,
  renderProxyFeatures,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyFeatures)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Proxy-Features@ value.
newtype ProxyFeatures = ProxyFeatures {proxyFeaturesValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ProxyFeatures where
  type ParseFailure ProxyFeatures = String
  type Cardinality ProxyFeatures = 'ZeroOrOne
  type Direction ProxyFeatures = 'Request


  parseFromHeaders _ headers = case runParser proxyFeaturesParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Proxy-Features header: " <> show rest
    Fail -> Left "Failed to parse Proxy-Features header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProxyFeatures


  headerName _ = hProxyFeatures


proxyFeaturesParser :: ParserT st String ProxyFeatures
proxyFeaturesParser = ProxyFeatures <$> takeRestShortText


renderProxyFeatures :: ProxyFeatures -> M.Builder
renderProxyFeatures = shortText . proxyFeaturesValue
