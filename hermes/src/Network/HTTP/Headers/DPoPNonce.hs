{- |
RFC 9449 §8 @DPoP-Nonce@ — a server-chosen nonce returned in a response that
the client must incorporate into a subsequent DPoP proof JWT (via the @nonce@
claim).

The value is opaque to the client: RFC 9449 places no structural requirements
on it beyond being unpredictable, so we preserve it verbatim as a
'ST.ShortText'.

Spec: <https://www.rfc-editor.org/rfc/rfc9449.html#section-8 RFC 9449 §8>

See also: "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.ReplayNonce", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.DPoPNonce (
  DPoPNonce (..),
  dPoPNonceParser,
  renderDPoPNonce,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDPoPNonce)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @DPoP-Nonce@ value: an opaque, server-chosen anti-replay nonce.
newtype DPoPNonce = DPoPNonce {dPoPNonceValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader DPoPNonce where
  type ParseFailure DPoPNonce = String
  type Cardinality DPoPNonce = 'ZeroOrOne
  type Direction DPoPNonce = 'Response


  parseFromHeaders _ headers = case runParser dPoPNonceParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing DPoP-Nonce header: " <> show rest
    Fail -> Left "Failed to parse DPoP-Nonce header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDPoPNonce


  headerName _ = hDPoPNonce


dPoPNonceParser :: ParserT st String DPoPNonce
dPoPNonceParser = DPoPNonce <$> takeRestShortText


renderDPoPNonce :: DPoPNonce -> M.Builder
renderDPoPNonce (DPoPNonce v) = shortText v
