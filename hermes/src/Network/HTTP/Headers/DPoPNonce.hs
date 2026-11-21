{- |
RFC 9449 §8 @DPoP-Nonce@ — a server-chosen nonce returned in a response that
the client must incorporate into a subsequent DPoP proof JWT (via the @nonce@
claim).

The client treats the value as opaque. The codec checks the nonempty
@NQCHAR@ sequence from RFC 9449 §8.1 and preserves it verbatim.

Spec: <https://www.rfc-editor.org/rfc/rfc9449.html#section-8 RFC 9449 §8>

See also: "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.ReplayNonce", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.Authorization".
-}
module Network.HTTP.Headers.DPoPNonce (
  DPoPNonce (..),
  dPoPNonceParser,
  renderDPoPNonce,
) where

import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
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


  parseFromHeaders _ = parseSingleFieldLine "DPoP-Nonce" dPoPNonceParser


  renderToHeaders _ = ST.toByteString . dPoPNonceValue


  renderToHeadersChecked _ (DPoPNonce nonce) =
    let wire = ST.toByteString nonce
    in either (const Nothing) (const (Just wire)) $
         parseSingleFieldLine "DPoP-Nonce" (asciiSetBytes nonceChars) (wire :| [])


  headerName _ = hDPoPNonce


dPoPNonceParser :: ParserT st String DPoPNonce
dPoPNonceParser = DPoPNonce <$> shortASCIIFromParser_ (asciiSetBytes nonceChars)


nonceChars :: AsciiSet
nonceChars = asciiSet (CharSet.singleton '!' <> CharSet.range '#' '[' <> CharSet.range ']' '~')
{-# NOINLINE nonceChars #-}


renderDPoPNonce :: DPoPNonce -> M.Builder
renderDPoPNonce (DPoPNonce v) = shortText v
