{- |
RFC 9449 §4.1 @DPoP@ — the DPoP proof JWT a client presents to an
authorization server or resource server to demonstrate proof of possession
of a private key (Demonstrating Proof of Possession at the Application
Layer).

The field codec checks the HTTP @token68@ syntax and preserves the exact
proof spelling. The application verifies the compact JWT and its claims
using the checks in RFC 9449 §4.3.

Spec: <https://www.rfc-editor.org/rfc/rfc9449.html#section-4.1 RFC 9449 §4.1>

See also: "Network.HTTP.Headers.DPoPNonce", "Network.HTTP.Headers.Authorization", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.SecTokenBinding".
-}
module Network.HTTP.Headers.DPoP (
  DPoP (..),
  dPoPParser,
  renderDPoP,
) where

import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization (token68Parser)
import Network.HTTP.Headers.HeaderFieldName (hDPoP)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @DPoP@ proof JWT, preserved verbatim (compact JWS serialization).
newtype DPoP = DPoP {dPoPProof :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader DPoP where
  type ParseFailure DPoP = String
  type Cardinality DPoP = 'ZeroOrOne
  type Direction DPoP = 'Request


  parseFromHeaders _ = parseSingleFieldLine "DPoP" dPoPParser


  renderToHeaders _ = ST.toByteString . dPoPProof


  headerName _ = hDPoP


dPoPParser :: ParserT st String DPoP
dPoPParser = DPoP . STU.fromByteStringUnsafe <$> token68Parser


renderDPoP :: DPoP -> M.Builder
renderDPoP (DPoP proof) = shortText proof
