{- |
@Proxy-Instruction@ — obsolete header from the W3C "Notification for Proxy
Caches" note, by which an origin server passed instructions to feature-aware
proxies. The note never became a standard, so the value is preserved verbatim
as an opaque string.

Spec: W3C Working Draft <https://www.w3.org/TR/WD-proxy Notification for Proxy Caches> (never standardized).

See also: "Network.HTTP.Headers.ProxyFeatures", "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.ProxyInstruction (
  ProxyInstruction (..),
  proxyInstructionParser,
  renderProxyInstruction,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProxyInstruction)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Proxy-Instruction@ value.
newtype ProxyInstruction = ProxyInstruction {proxyInstructionValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ProxyInstruction where
  type ParseFailure ProxyInstruction = String
  type Cardinality ProxyInstruction = 'ZeroOrOne
  type Direction ProxyInstruction = 'Response


  parseFromHeaders _ headers = case runParser proxyInstructionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Proxy-Instruction header: " <> show rest
    Fail -> Left "Failed to parse Proxy-Instruction header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProxyInstruction


  headerName _ = hProxyInstruction


proxyInstructionParser :: ParserT st String ProxyInstruction
proxyInstructionParser = ProxyInstruction <$> takeRestShortText


renderProxyInstruction :: ProxyInstruction -> M.Builder
renderProxyInstruction = shortText . proxyInstructionValue
