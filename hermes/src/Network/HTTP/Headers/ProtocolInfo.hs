{- |
@Protocol-Info@ — deprecated protocol-negotiation header from the Joint
Electronic Payment Initiative (JEPI). Its value is host-specific with no live,
interoperable grammar, so it is preserved verbatim as an opaque string.

Spec: JEPI / PEP (August 1996) header family — <https://www.w3.org/TR/NOTE-jepi-970519 White Paper: Joint Electronic Payment Initiative>; deprecated per RFC 4229.

See also: "Network.HTTP.Headers.ProtocolQuery", "Network.HTTP.Headers.Protocol", "Network.HTTP.Headers.ProtocolRequest", "Network.HTTP.Headers.PEP".
-}
module Network.HTTP.Headers.ProtocolInfo (
  ProtocolInfo (..),
  protocolInfoParser,
  renderProtocolInfo,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocolInfo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Protocol-Info@ value.
newtype ProtocolInfo = ProtocolInfo {protocolInfoValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ProtocolInfo where
  type ParseFailure ProtocolInfo = String
  type Cardinality ProtocolInfo = 'ZeroOrOne
  type Direction ProtocolInfo = 'Response


  parseFromHeaders _ headers = case runParser protocolInfoParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Protocol-Info header: " <> show rest
    Fail -> Left "Failed to parse Protocol-Info header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProtocolInfo


  headerName _ = hProtocolInfo


protocolInfoParser :: ParserT st String ProtocolInfo
protocolInfoParser = ProtocolInfo <$> takeRestShortText


renderProtocolInfo :: ProtocolInfo -> M.Builder
renderProtocolInfo = shortText . protocolInfoValue
