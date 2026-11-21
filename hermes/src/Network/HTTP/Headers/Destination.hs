{- |
RFC 4918 §10.3 @Destination@ request header — names the destination
URI for a @COPY@ or @MOVE@ operation.

== Grammar

@
Destination = Simple-ref
Simple-ref  = absolute-URI | ( path-absolute [ "?" query ] )
@

The parser validates the reference syntax and preserves its spelling.

Spec: <https://www.rfc-editor.org/rfc/rfc4918#section-10.3>

See also: "Network.HTTP.Headers.Overwrite", "Network.HTTP.Headers.Depth", "Network.HTTP.Headers.Position", "Network.HTTP.Headers.Location".
-}
module Network.HTTP.Headers.Destination (
  Destination (..),
  destinationParser,
  renderDestination,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (absolutePathReference, absolutePathReferenceBytes, absoluteURI, checkedURIField, outboundAbsoluteURIBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDestination)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The destination URI reference, stored exactly as received.
newtype Destination = Destination {destinationUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Destination where
  type ParseFailure Destination = String
  type Cardinality Destination = 'ZeroOrOne
  type Direction Destination = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Destination" destinationParser


  renderToHeaders _ = M.toStrictByteString . renderDestination


  renderToHeadersChecked _ =
    checkedURIField (outboundAbsoluteURIBytes <|> absolutePathReferenceBytes) . ST.toByteString . destinationUri


  headerName _ = hDestination


destinationParser :: ParserT st String Destination
destinationParser = Destination <$> (absoluteURI <|> absolutePathReference)


renderDestination :: Destination -> M.Builder
renderDestination (Destination uri) = shortText uri
