{- | The @If-None-Match@ precondition request header makes a method conditional on
none of the listed entity-tags matching the selected representation (or, with
@*@, on no current representation existing). On @GET@ or @HEAD@ a match yields
@304 (Not Modified)@; on other methods it yields @412 (Precondition Failed)@.
It is the entity-tag counterpart of @If-Modified-Since@ and the usual way to
revalidate a cached response.

@
If-None-Match = "*" / #entity-tag
@

Either the wildcard @*@ (matching any current representation) or a
comma-separated list of entity-tags that can be empty. Entity-tags are reused from
"Network.HTTP.Headers.ETag".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.2>

See also: "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfModifiedSince", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfRange", "Network.HTTP.Headers.IfUnmodifiedSince".
-}
module Network.HTTP.Headers.IfNoneMatch (
  IfNoneMatch (..),
  ifNoneMatchParser,
  renderIfNoneMatch,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.ETag (EntityTag, conditionalEntityTagParser, encodeEntityTagListChecked, parseConditionalEntityTagFields, renderEntityTag)
import Network.HTTP.Headers.HeaderFieldName (hIfNoneMatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The parsed value of an @If-None-Match@ header.
data IfNoneMatch
  = -- | The wildcard @*@.
    IfNoneMatchAny
  | -- | A non-empty list of candidate entity-tags.
    IfNoneMatchTags (NE.NonEmpty EntityTag)
  | -- | A present field whose list contains no tags.
    IfNoneMatchNoTags
  deriving stock (Eq, Show)


instance KnownHeader IfNoneMatch where
  type ParseFailure IfNoneMatch = String
  type Cardinality IfNoneMatch = 'ZeroOrOne
  type Direction IfNoneMatch = 'Request


  parseFromHeaders _ = parseConditionalEntityTagFields "If-None-Match" IfNoneMatchAny fromTags


  renderToHeaders _ = M.toStrictByteString . renderIfNoneMatch


  renderToHeadersChecked _ IfNoneMatchAny = Just "*"
  renderToHeadersChecked _ IfNoneMatchNoTags = Just ""
  renderToHeadersChecked _ (IfNoneMatchTags tags) = encodeEntityTagListChecked tags


  headerName _ = hIfNoneMatch


ifNoneMatchParser :: ParserT st String IfNoneMatch
ifNoneMatchParser = conditionalEntityTagParser IfNoneMatchAny fromTags


fromTags :: [EntityTag] -> IfNoneMatch
fromTags = maybe IfNoneMatchNoTags IfNoneMatchTags . NE.nonEmpty


renderIfNoneMatch :: IfNoneMatch -> M.Builder
renderIfNoneMatch = \case
  IfNoneMatchAny -> M.char8 '*'
  IfNoneMatchTags tags -> M.intersperse ", " $ map renderEntityTag $ NE.toList tags
  IfNoneMatchNoTags -> mempty
