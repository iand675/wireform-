{-# LANGUAGE TemplateHaskell #-}

{- |
@URI@ — obsolete HTTP entity-header (RFC 2068 §19.6.2.5). In earlier HTTP
drafts it served as a combination of @Location@, @Content-Location@ and
@Vary@: a list of URIs by which the resource may also be identified.

== Grammar (RFC 2068 §19.6.2.5)

@
URI-header = \"URI\" \":\" 1#( \"\<\" URI \"\>\" )
@

A non-empty list of angle-bracketed URI references. Each reference is checked
against RFC 3986 and its spelling is preserved. Repeated lines form a single
ordered list, as specified by RFC 2068 §4.2.

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-19.6.2.5>

See also: "Network.HTTP.Headers.ContentBase", "Network.HTTP.Headers.ContentLocation", "Network.HTTP.Headers.Location", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.URI (
  URI (..),
  uriParser,
  renderURI,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (uriReference)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hURI)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @URI@ value: a non-empty list of angle-bracketed URIs (brackets stripped).
newtype URI = URI {uriList :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader URI where
  type ParseFailure URI = String
  type Cardinality URI = 'ZeroOrOne
  type Direction URI = 'RequestAndResponse


  parseFromHeaders _ fields = URI <$> (parseListFieldLines "URI" uriItems fields >>= requireNonEmptyList "URI")


  renderToHeaders _ = M.toStrictByteString . renderURI


  headerName _ = hURI


uriParser :: ParserT st String URI
uriParser = uriItems >>= maybe failed (pure . URI) . NE.nonEmpty


uriItems :: ParserT st String [ST.ShortText]
uriItems = httpList ($(char '<') *> uriReference <* $(char '>'))


renderURI :: URI -> M.Builder
renderURI (URI (first :| rest)) = renderOne first <> foldMap (\uri -> ", " <> renderOne uri) rest
  where
    renderOne u = M.char7 '<' <> shortText u <> M.char7 '>'
