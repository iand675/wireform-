{- |
RFC 9449 §4.1 @DPoP@ — the DPoP proof JWT a client presents to an
authorization server or resource server to demonstrate proof of possession
of a private key (Demonstrating Proof of Possession at the Application
Layer).

The value is a JWT in JWS compact serialization
(@base64url \".\" base64url \".\" base64url@). Because the proof must be
verified against its exact on-the-wire bytes, we keep it verbatim as a
'ST.ShortText' rather than decoding the JWS structure.

Spec: <https://www.rfc-editor.org/rfc/rfc9449.html#section-4.1 RFC 9449 §4.1>

See also: "Network.HTTP.Headers.DPoPNonce", "Network.HTTP.Headers.Authorization", "Network.HTTP.Headers.WWWAuthenticate", "Network.HTTP.Headers.SecTokenBinding".
-}
module Network.HTTP.Headers.DPoP (
  DPoP (..),
  dPoPParser,
  renderDPoP,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
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


  parseFromHeaders _ headers = case runParser dPoPParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing DPoP header: " <> show rest
    Fail -> Left "Failed to parse DPoP header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDPoP


  headerName _ = hDPoP


dPoPParser :: ParserT st String DPoP
dPoPParser = DPoP <$> takeRestShortText


renderDPoP :: DPoP -> M.Builder
renderDPoP (DPoP proof) = shortText proof
