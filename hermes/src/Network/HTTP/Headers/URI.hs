{-# LANGUAGE TemplateHaskell #-}

{- |
@URI@ — obsolete HTTP entity-header (RFC 2068 §19.6.2.5). In earlier HTTP
drafts it served as a combination of @Location@, @Content-Location@ and
@Vary@: a list of URIs by which the resource may also be identified.

== Grammar (RFC 2068 §19.6.2.5)

@
URI-header = \"URI\" \":\" 1#( \"\<\" URI \"\>\" )
@

A non-empty, comma-separated list of URIs, each enclosed in angle brackets. We
capture the list structure ('NE.NonEmpty') with each element holding the URI
text between the @\<@ and @\>@ delimiters verbatim (the URI itself is opaque, so
no inner URI grammar is fabricated).

Spec: <https://www.rfc-editor.org/rfc/rfc2068#section-19.6.2.5>

See also: "Network.HTTP.Headers.ContentBase", "Network.HTTP.Headers.ContentLocation", "Network.HTTP.Headers.Location", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.URI (
  URI (..),
  uriParser,
  renderURI,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
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
  type Direction URI = 'Response


  parseFromHeaders _ headers = case runParser uriParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing URI header: " <> show rest
    Fail -> Left "Failed to parse URI header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderURI


  headerName _ = hURI


uriParser :: ParserT st String URI
uriParser = do
  ows
  first <- uriItem
  rest <- many (ows *> $(char ',') *> ows *> uriItem)
  pure $ URI (first NE.:| rest)
  where
    uriItem = do
      $(char '<')
      uri <- shortASCIIFromParser_ (skipMany (skipSatisfyAscii (/= '>')))
      $(char '>')
      pure uri


renderURI :: URI -> M.Builder
renderURI (URI uris) = M.intersperse ", " $ map renderOne $ NE.toList uris
  where
    renderOne u = M.char7 '<' <> shortText u <> M.char7 '>'
