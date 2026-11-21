{- |
RFC 9110 §8.7 @Content-Location@ — representation metadata giving a URI
reference for a specific representation of the resource. The value is a single fragmentless URI
reference. The codec validates its syntax and preserves its spelling.

== Grammar

@
Content-Location = absolute-URI / partial-URI
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.7>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.Location".
-}
module Network.HTTP.Headers.ContentLocation (
  ContentLocation (..),
  contentLocationParser,
  renderContentLocation,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (checkedURIField, fragmentlessReference, outboundFragmentlessReferenceBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentLocation)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The fragmentless URI reference carried by a @Content-Location@ header.
newtype ContentLocation = ContentLocation {contentLocationUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentLocation where
  type ParseFailure ContentLocation = String
  type Cardinality ContentLocation = 'ZeroOrOne
  type Direction ContentLocation = 'RequestAndResponse


  parseFromHeaders _ = parseSingleFieldLine "Content-Location" contentLocationParser


  renderToHeaders _ = M.toStrictByteString . renderContentLocation


  renderToHeadersChecked _ = checkedURIField outboundFragmentlessReferenceBytes . ST.toByteString . contentLocationUri


  headerName _ = hContentLocation


contentLocationParser :: ParserT st String ContentLocation
contentLocationParser = ContentLocation <$> fragmentlessReference


renderContentLocation :: ContentLocation -> M.Builder
renderContentLocation (ContentLocation uri) = shortText uri
