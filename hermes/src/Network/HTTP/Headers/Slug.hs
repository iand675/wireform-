{- |
RFC 5023 §9.7 @SLUG@ — an Atom Publishing Protocol request header by which
a client suggests a human-readable title (and, by extension, a URI slug)
for a newly created Atom Collection member.

The value is opaque text: any sequence of characters, with octets outside
the permitted set percent-encoded by the client. There is no further
structure to interpret, so it is preserved verbatim as raw text.

Spec: <https://www.rfc-editor.org/rfc/rfc5023#section-9.7>

See also: "Network.HTTP.Headers.Location", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentLocation".
-}
module Network.HTTP.Headers.Slug (
  Slug (..),
  slugParser,
  renderSlug,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSlug)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The opaque, possibly percent-encoded title carried by a @SLUG@ header.
newtype Slug = Slug {slugText :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Slug where
  type ParseFailure Slug = String
  type Cardinality Slug = 'ZeroOrOne
  type Direction Slug = 'Request


  parseFromHeaders _ headers = case runParser slugParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing SLUG header: " <> show rest
    Fail -> Left "Failed to parse SLUG header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSlug


  headerName _ = hSlug


slugParser :: ParserT st String Slug
slugParser = Slug <$> takeRestShortText


renderSlug :: Slug -> M.Builder
renderSlug (Slug t) = shortText t
