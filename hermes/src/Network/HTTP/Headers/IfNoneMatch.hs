{-# LANGUAGE TemplateHaskell #-}

{- | The @If-None-Match@ precondition request header makes a method conditional on
none of the listed entity-tags matching the selected representation (or, with
@*@, on no current representation existing). On @GET@ or @HEAD@ a match yields
@304 (Not Modified)@; on other methods it yields @412 (Precondition Failed)@.
It is the entity-tag counterpart of @If-Modified-Since@ and the usual way to
revalidate a cached response.

@
If-None-Match = "*" / #entity-tag
@

Either the wildcard @*@ (matching any current representation) or a non-empty,
comma-separated list of entity-tags. Entity-tags are reused from
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
import Network.HTTP.Headers.ETag (EntityTag, entityTagParser, renderEntityTag)
import Network.HTTP.Headers.HeaderFieldName (hIfNoneMatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The parsed value of an @If-None-Match@ header.
data IfNoneMatch
  = -- | The wildcard @*@.
    IfNoneMatchAny
  | -- | A non-empty list of candidate entity-tags.
    IfNoneMatchTags (NE.NonEmpty EntityTag)
  deriving stock (Eq, Show)


instance KnownHeader IfNoneMatch where
  type ParseFailure IfNoneMatch = String
  type Cardinality IfNoneMatch = 'ZeroOrOne
  type Direction IfNoneMatch = 'Request


  parseFromHeaders _ headers = case runParser ifNoneMatchParser $ NE.head headers of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing If-None-Match header: " <> show rest
    Fail -> Left "Failed to parse If-None-Match header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIfNoneMatch


  headerName _ = hIfNoneMatch


ifNoneMatchParser :: ParserT st String IfNoneMatch
ifNoneMatchParser =
  (IfNoneMatchAny <$ $(char '*'))
    <|> (IfNoneMatchTags <$> tagList)
  where
    tagList = do
      t <- entityTagParser
      ts <- many (ows *> $(char ',') *> ows *> entityTagParser)
      pure (t NE.:| ts)


renderIfNoneMatch :: IfNoneMatch -> M.Builder
renderIfNoneMatch = \case
  IfNoneMatchAny -> M.char8 '*'
  IfNoneMatchTags tags -> M.intersperse ", " $ map renderEntityTag $ NE.toList tags
