{-# LANGUAGE TemplateHaskell #-}

{- | The @If-Match@ precondition request header makes a method conditional on the
target resource's current entity-tag. The server proceeds only when @*@ is
given (any current representation exists) or one of the listed strong
entity-tags matches; otherwise it responds @412 (Precondition Failed)@. It is
most often used to prevent lost updates on @PUT@ and @DELETE@.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.1>

See also: "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.IfUnmodifiedSince", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfRange", "Network.HTTP.Headers.IfModifiedSince".
-}
module Network.HTTP.Headers.IfMatch (
  IfMatch (..),
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import qualified Data.ByteString as B
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.ETag
import Network.HTTP.Headers.HeaderFieldName (hIfMatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | The "If-Match" header field makes the request method conditional on
the recipient origin server either having at least one current representation of the target resource, when the field value is "*", or having a current representation of the target resource that has an entity tag matching a member of the
list of entity tags provided in the field value.
-}
data IfMatch
  = IfMatchAnyCurrentRepresentation
  | IfMatchEntityTags (NonEmpty EntityTag)


instance KnownHeader IfMatch where
  type ParseFailure IfMatch = String
  type Cardinality IfMatch = 'ZeroOrOne
  type Direction IfMatch = 'Request


  parseFromHeaders _ headers = case runParser ifMatchParser (B.intercalate ", " $ NE.toList headers) of
    OK im "" -> Right im
    OK _ rest -> Left $ "Unconsumed input after parsing If-Match header: " <> show rest
    Fail -> Left "Failed to parse If-Match header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIfMatch


  headerName _ = hIfMatch


ifMatchParser :: ParserT st e IfMatch
ifMatchParser =
  ($(string "*") $> IfMatchAnyCurrentRepresentation)
    <|> (IfMatchEntityTags <$> entityTagParser `sepBy1` (ows *> $(char ',') *> ows))


renderIfMatch :: IfMatch -> M.Builder
renderIfMatch = \case
  IfMatchAnyCurrentRepresentation -> "*"
  IfMatchEntityTags tags -> M.intersperse ", " $ renderEntityTag <$> tags
