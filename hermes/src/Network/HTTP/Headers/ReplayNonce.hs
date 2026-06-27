{- |
RFC 8555 §6.5.1 @Replay-Nonce@ — an ACME server-supplied anti-replay nonce
that a client echoes in the protected header of its next signed request.

@
Replay-Nonce = 1*base64url
base64url    = ALPHA / DIGIT / \"-\" / \"_\"
@

The nonce is base64url-encoded without padding. We validate that character
shape on parse and keep the verbatim token as a 'ST.ShortText'.

Spec: <https://www.rfc-editor.org/rfc/rfc8555.html#section-6.5.1 RFC 8555 §6.5.1>

See also: "Network.HTTP.Headers.DPoPNonce", "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.ReplayNonce (
  ReplayNonce (..),
  replayNonceParser,
  renderReplayNonce,
) where

import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReplayNonce)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Replay-Nonce@ value: a base64url-encoded anti-replay token.
newtype ReplayNonce = ReplayNonce {replayNonce :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ReplayNonce where
  type ParseFailure ReplayNonce = String
  type Cardinality ReplayNonce = 'ZeroOrOne
  type Direction ReplayNonce = 'Response


  parseFromHeaders _ headers = case runParser replayNonceParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Replay-Nonce header: " <> show rest
    Fail -> Left "Failed to parse Replay-Nonce header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderReplayNonce


  headerName _ = hReplayNonce


replayNonceParser :: ParserT st String ReplayNonce
replayNonceParser =
  ReplayNonce <$> shortASCIIFromParser_ (some (satisfyAscii (`CharSet.member` base64urlCharSet)))
  where
    base64urlCharSet =
      CharSet.fromList (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']) <> "-_"


renderReplayNonce :: ReplayNonce -> M.Builder
renderReplayNonce (ReplayNonce n) = shortText n
