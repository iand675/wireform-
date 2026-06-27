{- |
@Protocol-Request@ — obsolete protocol-negotiation request header from the PICS
Label Distribution protocol. Its value is host-specific with no live,
interoperable grammar, so it is preserved verbatim as an opaque string.

Spec: <https://www.w3.org/TR/REC-PICS-labels-961031 PICS 1.1 Label Distribution -- Label Syntax and Communication Protocols> (HTTP registration RFC 4229).

See also: "Network.HTTP.Headers.Protocol", "Network.HTTP.Headers.PICSLabel", "Network.HTTP.Headers.ProtocolQuery", "Network.HTTP.Headers.ProtocolInfo".
-}
module Network.HTTP.Headers.ProtocolRequest (
  ProtocolRequest (..),
  protocolRequestParser,
  renderProtocolRequest,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hProtocolRequest)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Opaque, raw-preserving @Protocol-Request@ value.
newtype ProtocolRequest = ProtocolRequest {protocolRequestValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ProtocolRequest where
  type ParseFailure ProtocolRequest = String
  type Cardinality ProtocolRequest = 'ZeroOrOne
  type Direction ProtocolRequest = 'Request


  parseFromHeaders _ headers = case runParser protocolRequestParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Protocol-Request header: " <> show rest
    Fail -> Left "Failed to parse Protocol-Request header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderProtocolRequest


  headerName _ = hProtocolRequest


protocolRequestParser :: ParserT st String ProtocolRequest
protocolRequestParser = ProtocolRequest <$> takeRestShortText


renderProtocolRequest :: ProtocolRequest -> M.Builder
renderProtocolRequest = shortText . protocolRequestValue
