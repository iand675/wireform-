{- |
RFC 8555 §6.5.1 @Replay-Nonce@ — an ACME server-supplied anti-replay nonce
that a client echoes in the protected header of its next signed request.

@
Replay-Nonce = 1*base64url
base64url    = ALPHA / DIGIT / \"-\" / \"_\"
@

The nonce is canonical Base64url without padding. The codec validates the
alphabet, length, and final sextet, preserving the encoded spelling.

Spec: <https://www.rfc-editor.org/rfc/rfc8555.html#section-6.5.1 RFC 8555 §6.5.1>

See also: "Network.HTTP.Headers.DPoPNonce", "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.ReplayNonce (
  ReplayNonce (..),
  replayNonceParser,
  renderReplayNonce,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
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


  parseFromHeaders _ = parseSingleFieldLine "Replay-Nonce" replayNonceParser


  renderToHeaders _ = ST.toByteString . replayNonce


  renderToHeadersChecked _ (ReplayNonce nonce) =
    let wire = ST.toByteString nonce
    in either (const Nothing) (const (Just wire)) $
         parseSingleFieldLine "Replay-Nonce" replayNonceWire (wire :| [])


  headerName _ = hReplayNonce


replayNonceParser :: ParserT st String ReplayNonce
replayNonceParser = ReplayNonce . STU.fromByteStringUnsafe <$> replayNonceWire


replayNonceWire :: ParserT st String BS.ByteString
replayNonceWire =
  withByteString base64UrlBytes $ \() wire ->
    if canonicalTail wire
      then pure wire
      else err "Invalid unpadded Base64url Replay-Nonce"
  where
    -- The scanner guarantees a nonempty run. A partial group must have
    -- four or two zero bits in its last sextet, respectively.
    canonicalTail wire = case BS.length wire `rem` 4 of
      0 -> True
      2 -> BSU.unsafeLast wire `BS.elem` "AQgw"
      3 -> BSU.unsafeLast wire `BS.elem` "AEIMQUYcgkosw048"
      _ -> False


renderReplayNonce :: ReplayNonce -> M.Builder
renderReplayNonce (ReplayNonce n) = shortText n
