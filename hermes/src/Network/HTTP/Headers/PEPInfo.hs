{- |
@PEP-Info@ — the general header of the (abandoned) W3C \"PEP - an
Extension Mechanism for HTTP\" draft, the precursor to the RFC 2774 HTTP
Extension Framework.

@PEP-Info@ carries protocol /information/ rather than a protocol
declaration: it conveys the @map@ attribute (and related directives)
binding meta-information and transaction information to the HTTP message,
so that a recipient can interact correctly with the extension that the
companion 'Network.HTTP.Headers.PEP.PEP' header declared.

Like @PEP@, this mechanism never advanced beyond an Internet-Draft and its
nested, brace-delimited grammar is effectively dead; per the depth
guidance for obsolete/host-specific values we do not fabricate a
structured type but preserve the field value verbatim as a round-tripping
'ST.ShortText' (mirroring 'Network.HTTP.Headers.Referer.Referer').

Spec: W3C draft \"PEP - an Extension Mechanism for HTTP\" <https://www.w3.org/TR/WD-http-pep-971121> (design notes <https://www.w3.org/Protocols/PEP/Design.html>).

See also: "Network.HTTP.Headers.PEP", "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Opt", "Network.HTTP.Headers.Ext".
-}
module Network.HTTP.Headers.PEPInfo (
  PEPInfo (..),
  pepInfoParser,
  renderPEPInfo,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPEPInfo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of a @PEP-Info@ protocol-information header.
newtype PEPInfo = PEPInfo {rawPEPInfo :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader PEPInfo where
  type ParseFailure PEPInfo = String
  type Cardinality PEPInfo = 'ZeroOrOne
  type Direction PEPInfo = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser pepInfoParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing PEP-Info header: " <> show rest
    Fail -> Left "Failed to parse PEP-Info header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPEPInfo


  headerName _ = hPEPInfo


pepInfoParser :: ParserT st String PEPInfo
pepInfoParser = PEPInfo <$> takeRestShortText


renderPEPInfo :: PEPInfo -> M.Builder
renderPEPInfo (PEPInfo v) = shortText v
