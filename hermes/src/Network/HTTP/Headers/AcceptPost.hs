{-# LANGUAGE TemplateHaskell #-}

{- |
W3C Linked Data Platform §7.1 @Accept-Post@ — a response header by which
a server advertises the media types it accepts in a @POST@ request body.
A bare @*@ entry indicates that the server accepts any media type.

== Grammar

@
Accept-Post = \"Accept-Post\" \":\" #( media-type / \"*\" )
@

Each media-type entry is reduced to its @type/subtype@ core via 'MediaType'
(see "Network.HTTP.ContentNegotiation"); media-type parameters are not
surfaced. The literal @*@ (distinct from the @*\/*@ media type) is captured
as 'AcceptPostAny'. The value is comma-combinable across multiple header lines.

Spec: <https://www.w3.org/TR/ldp/#header-accept-post>

See also: "Network.HTTP.Headers.AcceptPatch", "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.AcceptPost (
  AcceptPost (..),
  AcceptPostEntry (..),
  acceptPostParser,
  renderAcceptPost,
) where

import Control.Monad.Combinators.NonEmpty
import qualified Data.ByteString as B
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentNegotiation (MediaType, mediaTypeParser, renderMediaType)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptPost)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1)


-- | One entry of an @Accept-Post@ value.
data AcceptPostEntry
  = -- | The literal @*@ — the server accepts any media type.
    AcceptPostAny
  | -- | A concrete media type (its @type/subtype@ core).
    AcceptPostMediaType !MediaType
  deriving stock (Eq, Show)


-- | A non-empty list of acceptable media types for @POST@ bodies.
newtype AcceptPost = AcceptPost {acceptPostEntries :: NE.NonEmpty AcceptPostEntry}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader AcceptPost where
  type ParseFailure AcceptPost = String
  type Cardinality AcceptPost = 'ZeroOrMore
  type Direction AcceptPost = 'Response


  parseFromHeaders _ headers = do
    res <- traverse runAcceptPostParser headers
    pure (fold1 res)


  renderToHeaders _ = pure . M.toStrictByteString . renderAcceptPost


  headerName _ = hAcceptPost


runAcceptPostParser :: B.ByteString -> Either String AcceptPost
runAcceptPostParser bs = case runParser acceptPostParser bs of
  OK v leftover
    | B.null (dropOws leftover) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Accept-Post header: " <> show leftover)
  Fail -> Left "Failed to parse Accept-Post header"
  Err e -> Left e
  where
    dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


acceptPostParser :: ParserT st String AcceptPost
acceptPostParser =
  AcceptPost <$> (ows *> entry `sepBy1` (ows *> $(char ',') *> ows))
  where
    -- Try the media-type grammar first so @*\/*@ is parsed as a media type;
    -- a bare @*@ only matches once 'mediaTypeParser' has backtracked.
    entry =
      (AcceptPostMediaType <$> mediaTypeParser)
        <|> (AcceptPostAny <$ $(char '*'))


renderAcceptPost :: AcceptPost -> M.Builder
renderAcceptPost = sepByCommas1 . fmap renderEntry . acceptPostEntries
  where
    renderEntry AcceptPostAny = "*"
    renderEntry (AcceptPostMediaType mt) = renderMediaType mt
