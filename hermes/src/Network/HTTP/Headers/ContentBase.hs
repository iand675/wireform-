{- |
@Content-Base@ — obsolete HTTP entity-header (RFC 2068 §14.11, dropped in
RFC 2616) that specified the base URI for resolving relative URIs inside the
entity (the role later filled by @Content-Location@ / the @<base>@ element).

== Grammar (RFC 2068 §14.11)

@
Content-Base = \"Content-Base\" \":\" absoluteURI
@

The value is a single opaque @absoluteURI@. Per the depth guidance this is a
faithful raw-preserving newtype: the URI is captured verbatim and re-emitted
unchanged, which round-trips exactly without fabricating a URI grammar.

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-14.11>

See also: "Network.HTTP.Headers.ContentLocation", "Network.HTTP.Headers.URI", "Network.HTTP.Headers.ContentVersion".
-}
module Network.HTTP.Headers.ContentBase (
  ContentBase (..),
  contentBaseParser,
  renderContentBase,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
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


  parseFromHeaders _ headers = case runParser contentBaseParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Content-Base header: " <> show rest
    Fail -> Left "Failed to parse Content-Base header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentBase


  headerName _ = hContentBase


contentBaseParser :: ParserT st String ContentBase
contentBaseParser = ContentBase <$> takeRestShortText


renderContentBase :: ContentBase -> M.Builder
renderContentBase (ContentBase uri) = shortText uri
