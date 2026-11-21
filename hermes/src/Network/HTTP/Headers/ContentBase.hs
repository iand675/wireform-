{- |
@Content-Base@ — obsolete HTTP entity-header (RFC 2068 §14.11, dropped in
RFC 2616) that specified the base URI for resolving relative URIs inside the
entity (the role later filled by @Content-Location@ / the @<base>@ element).

== Grammar (RFC 2068 §14.11)

@
Content-Base = \"Content-Base\" \":\" absoluteURI
@

The codec checks the absolute-URI grammar of RFC 3986 and keeps the
original spelling of the URI, including its percent escapes.

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-14.11>

See also: "Network.HTTP.Headers.ContentLocation", "Network.HTTP.Headers.URI", "Network.HTTP.Headers.ContentVersion".
-}
module Network.HTTP.Headers.ContentBase (
  ContentBase (..),
  contentBaseParser,
  renderContentBase,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (absoluteURI, checkedURIField, outboundAbsoluteURIBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentBase)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Content-Base@ value: the (absolute) base URI, preserved verbatim.
newtype ContentBase = ContentBase {contentBaseUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentBase where
  type ParseFailure ContentBase = String
  type Cardinality ContentBase = 'ZeroOrOne
  type Direction ContentBase = 'RequestAndResponse


  parseFromHeaders _ = parseSingleFieldLine "Content-Base" contentBaseParser


  renderToHeaders _ = ST.toByteString . contentBaseUri


  renderToHeadersChecked _ = checkedURIField outboundAbsoluteURIBytes . ST.toByteString . contentBaseUri


  headerName _ = hContentBase


contentBaseParser :: ParserT st String ContentBase
contentBaseParser = ContentBase <$> absoluteURI


renderContentBase :: ContentBase -> M.Builder
renderContentBase (ContentBase uri) = shortText uri
