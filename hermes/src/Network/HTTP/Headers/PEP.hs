{- |
@PEP@ — the general header of the (abandoned) W3C \"PEP - an Extension
Mechanism for HTTP\" draft, the precursor to the RFC 2774 HTTP Extension
Framework.

The @PEP@ header carries one or more /protocol declarations/, each
associating an extension with a URI and a brace-delimited @{ … }@ map of
@strength@ / @map@ / instance directives, e.g.

@
PEP: {{map \"/Pep/Inventory\"}{strength must}}
@

This mechanism never advanced beyond an Internet-Draft and the nested,
brace-delimited grammar is effectively dead; per the depth guidance for
obsolete/host-specific values we do not fabricate a structured type but
preserve the field value verbatim as a round-tripping 'ST.ShortText'
(mirroring 'Network.HTTP.Headers.Referer.Referer').

Spec: W3C draft \"PEP - an Extension Mechanism for HTTP\" <https://www.w3.org/TR/WD-http-pep-971121> (Internet-Draft <https://datatracker.ietf.org/doc/html/draft-nielsen-pep-http-00>).

See also: "Network.HTTP.Headers.PEPInfo", "Network.HTTP.Headers.Man", "Network.HTTP.Headers.Opt", "Network.HTTP.Headers.Ext".
-}
module Network.HTTP.Headers.PEP (
  PEP (..),
  pepParser,
  renderPEP,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPEP)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of a @PEP@ protocol-declaration header.
newtype PEP = PEP {rawPEP :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader PEP where
  type ParseFailure PEP = String
  type Cardinality PEP = 'ZeroOrOne
  type Direction PEP = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser pepParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing PEP header: " <> show rest
    Fail -> Left "Failed to parse PEP header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPEP


  headerName _ = hPEP


pepParser :: ParserT st String PEP
pepParser = PEP <$> takeRestShortText


renderPEP :: PEP -> M.Builder
renderPEP (PEP v) = shortText v
