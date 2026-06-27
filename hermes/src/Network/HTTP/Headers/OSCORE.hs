{- | The @OSCORE@ header field.

Defined by RFC 8613 (Object Security for Constrained RESTful Environments),
§11.1. When OSCORE-protected exchanges traverse an HTTP hop (the HTTP-to-CoAP
mapping), the CoAP OSCORE option is carried in an @OSCORE@ HTTP header field
whose value is the base64-encoded contents of that option. The value is
opaque at the HTTP layer, so this module preserves the raw text faithfully in
a newtype rather than decoding the binary option structure.

Spec: <https://www.rfc-editor.org/rfc/rfc8613#section-11.1>

See also: "Network.HTTP.Headers.DPoP", "Network.HTTP.Headers.Authorization", "Network.HTTP.Headers.Signature", "Network.HTTP.Headers.SignatureInput".
-}
module Network.HTTP.Headers.OSCORE (
  OSCORE (..),
  oscoreParser,
  renderOSCORE,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOSCORE)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @OSCORE@ value: the base64-encoded CoAP OSCORE option, preserved verbatim.
newtype OSCORE = OSCORE {oscoreValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader OSCORE where
  type ParseFailure OSCORE = String
  type Cardinality OSCORE = 'ZeroOrOne
  type Direction OSCORE = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser oscoreParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing OSCORE header: " <> show rest
      Fail -> Left "Failed to parse OSCORE header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderOSCORE


  headerName _ = hOSCORE


oscoreParser :: ParserT st String OSCORE
oscoreParser = OSCORE <$> takeRestShortText


renderOSCORE :: OSCORE -> M.Builder
renderOSCORE (OSCORE v) = shortText v
