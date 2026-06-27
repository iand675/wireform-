{- |
RFC 9110 §8.7 @Content-Location@ — representation metadata giving a URI
reference for a specific representation of the resource. The value is a single
@URI-reference@; we keep it as an opaque 'ST.ShortText' rather than fabricating
a URI parser, faithfully preserving the bytes on the wire.

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

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentLocation)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The (opaque) URI reference carried by a @Content-Location@ header.
newtype ContentLocation = ContentLocation {contentLocationUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentLocation where
  type ParseFailure ContentLocation = String
  type Cardinality ContentLocation = 'ZeroOrOne
  type Direction ContentLocation = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser contentLocationParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Content-Location header: " <> show rest
    Fail -> Left "Failed to parse Content-Location header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentLocation


  headerName _ = hContentLocation


contentLocationParser :: ParserT st String ContentLocation
contentLocationParser = ContentLocation <$> takeRestShortText


renderContentLocation :: ContentLocation -> M.Builder
renderContentLocation (ContentLocation uri) = shortText uri
