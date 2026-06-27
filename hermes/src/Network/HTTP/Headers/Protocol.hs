{- |
@Protocol@ — obsolete protocol-negotiation header from the PICS Label
Distribution protocol. It carries a host-specific value with no live,
interoperable grammar, so it is preserved verbatim as an opaque string.

Spec: <https://www.w3.org/TR/REC-PICS-labels-961031 PICS 1.1 Label Distribution -- Label Syntax and Communication Protocols> (HTTP registration RFC 4229).

See also: "Network.HTTP.Headers.ProtocolRequest", "Network.HTTP.Headers.PICSLabel", "Network.HTTP.Headers.ProtocolInfo", "Network.HTTP.Headers.ProtocolQuery".
-}
module Network.HTTP.Headers.Protocol (
  Protocol (..),
  protocolParser,
  renderProtocol,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocol)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Protocol@ value.
newtype Protocol = Protocol {protocolValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Protocol where
  type ParseFailure Protocol = String
  type Cardinality Protocol = 'ZeroOrOne
  type Direction Protocol = 'Request


  parseFromHeaders _ headers = case runParser protocolParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Protocol header: " <> show rest
    Fail -> Left "Failed to parse Protocol header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProtocol


  headerName _ = hProtocol


protocolParser :: ParserT st String Protocol
protocolParser = Protocol <$> takeRestShortText


renderProtocol :: Protocol -> M.Builder
renderProtocol = shortText . protocolValue
