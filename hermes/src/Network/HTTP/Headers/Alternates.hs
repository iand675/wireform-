{- |
RFC 2295 §8.3 @Alternates@ — response header conveying the variant list
bound to a transparently negotiable resource, optionally with directives
for the negotiation process.

== Grammar

@
Alternates   = \"Alternates\" \":\" variant-list
variant-list = 1#( variant-description | fallback-variant | list-directive )
variant-description =
        \"{\" \<\"\> URI \<\"\> source-quality *variant-attribute \"}\"
variant-attribute =
        \"{\" \"type\" media-type \"}\" | \"{\" \"charset\" charset \"}\"
      | \"{\" \"language\" 1#language-tag \"}\" | \"{\" \"length\" 1*DIGIT \"}\"
      | \"{\" \"features\" feature-list \"}\"
      | \"{\" \"description\" quoted-string [ language-tag ] \"}\"
      | \"{\" extension-attribute \"}\"
fallback-variant = \"{\" \<\"\> URI \<\"\> \"}\"
list-directive   = ( \"proxy-rvsa\" \"=\" \<\"\> 0#rvsa-version \<\"\> )
                 | extension-list-directive
@

Each variant-description is a brace-delimited record nesting further
brace-delimited attributes (themselves embedding feature-lists with their
own @[ … ]@ predicate bags), quoted URIs and quoted descriptions. Faithful
structured parsing of this nested, experimental grammar is not warranted;
we preserve the field value verbatim as a round-tripping 'ST.ShortText'.

Spec: <https://www.rfc-editor.org/rfc/rfc2295#section-8.3>

See also: "Network.HTTP.Headers.AcceptFeatures", "Network.HTTP.Headers.Negotiate", "Network.HTTP.Headers.TCN", "Network.HTTP.Headers.VariantVary", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.Alternates (
  Alternates (..),
  alternatesParser,
  renderAlternates,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAlternates)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The raw, verbatim value of an @Alternates@ header.
newtype Alternates = Alternates {rawAlternates :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Alternates where
  type ParseFailure Alternates = String
  type Cardinality Alternates = 'ZeroOrOne
  type Direction Alternates = 'Response


  parseFromHeaders _ headers = case runParser alternatesParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left ("Unconsumed input after parsing Alternates: " <> show rest)
    Fail -> Left "Failed to parse Alternates header"
    Err err -> Left err


  renderToHeaders _ = M.toStrictByteString . renderAlternates


  headerName _ = hAlternates


alternatesParser :: ParserT st String Alternates
alternatesParser = Alternates <$> takeRestShortText


renderAlternates :: Alternates -> M.Builder
renderAlternates = shortText . rawAlternates
