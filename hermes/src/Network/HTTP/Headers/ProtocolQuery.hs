{- |
@Protocol-Query@ — deprecated protocol-negotiation header from the Joint
Electronic Payment Initiative (JEPI). Its value is host-specific with no live,
interoperable grammar, so it is preserved verbatim as an opaque string.

Spec: JEPI / PEP (August 1996) header family — <https://www.w3.org/TR/NOTE-jepi-970519 White Paper: Joint Electronic Payment Initiative>; deprecated per RFC 4229.

See also: "Network.HTTP.Headers.ProtocolInfo", "Network.HTTP.Headers.Protocol", "Network.HTTP.Headers.ProtocolRequest", "Network.HTTP.Headers.PEP".
-}
module Network.HTTP.Headers.ProtocolQuery (
  ProtocolQuery (..),
  protocolQueryParser,
  renderProtocolQuery,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocolQuery)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Protocol-Query@ value.
newtype ProtocolQuery = ProtocolQuery {protocolQueryValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ProtocolQuery where
  type ParseFailure ProtocolQuery = String
  type Cardinality ProtocolQuery = 'ZeroOrOne
  type Direction ProtocolQuery = 'Request


  parseFromHeaders _ headers = case runParser protocolQueryParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Protocol-Query header: " <> show rest
    Fail -> Left "Failed to parse Protocol-Query header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProtocolQuery


  headerName _ = hProtocolQuery


protocolQueryParser :: ParserT st String ProtocolQuery
protocolQueryParser = ProtocolQuery <$> takeRestShortText


renderProtocolQuery :: ProtocolQuery -> M.Builder
renderProtocolQuery = shortText . protocolQueryValue
