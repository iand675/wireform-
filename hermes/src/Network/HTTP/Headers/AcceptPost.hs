{-# LANGUAGE TemplateHaskell #-}

{- |
W3C Linked Data Platform §7.1 @Accept-Post@ — a response header by which
a server advertises the media types it accepts in a @POST@ request body.
The codec also accepts the legacy bare @*@ spelling for any media type.

== Grammar

@
Accept-Post = \"Accept-Post\" \":\" #media-range
@

Entries preserve media-range parameters. The value is comma-combinable
across multiple header lines.

Spec: <https://www.w3.org/TR/ldp/#header-accept-post>

See also: "Network.HTTP.Headers.AcceptPatch", "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.Allow", "Network.HTTP.Headers.ContentType".
-}
module Network.HTTP.Headers.AcceptPost (
  AcceptPost (..),
  AcceptPostEntry (..),
  acceptPostEntryList,
  acceptPostParser,
  renderAcceptPost,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType, encodeMediaRangesChecked, mediaRangeParser, renderMediaRange, renderMediaType)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptPost)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1)


-- | One entry of an @Accept-Post@ value.
data AcceptPostEntry
  = -- | The literal @*@ — the server accepts any media type.
    AcceptPostAny
  | -- | A media range with no parameters.
    AcceptPostMediaType !MediaType
  | -- | A media range with parameters.
    AcceptPostMediaRange !MediaRange
  deriving stock (Eq, Show)


-- | A list of acceptable media ranges for @POST@ bodies.
data AcceptPost
  = AcceptPost {acceptPostEntries :: NE.NonEmpty AcceptPostEntry}
  | AcceptPostEmpty
  deriving stock (Eq, Show)


instance Semigroup AcceptPost where
  AcceptPostEmpty <> entries = entries
  entries <> AcceptPostEmpty = entries
  AcceptPost left <> AcceptPost right = AcceptPost (left <> right)


instance KnownHeader AcceptPost where
  type ParseFailure AcceptPost = String
  type Cardinality AcceptPost = 'ZeroOrMore
  type Direction AcceptPost = 'Response


  parseFromHeaders _ = fmap fromEntries . parseListFieldLines "Accept-Post" entryList


  renderToHeaders _ = pure . M.toStrictByteString . renderAcceptPost


  renderToHeadersChecked _ value = pure <$> encodeMediaRangesChecked entryRange (acceptPostEntryList value)
    where
      entryRange AcceptPostAny = Nothing
      entryRange (AcceptPostMediaType media) = Just (MediaRange media [])
      entryRange (AcceptPostMediaRange range) = Just range


  headerName _ = hAcceptPost


acceptPostParser :: ParserT st String AcceptPost
acceptPostParser = fromEntries <$> entryList


fromEntries :: [AcceptPostEntry] -> AcceptPost
fromEntries = maybe AcceptPostEmpty AcceptPost . NE.nonEmpty


-- | Get the entries of either an empty or populated field.
acceptPostEntryList :: AcceptPost -> [AcceptPostEntry]
acceptPostEntryList AcceptPostEmpty = []
acceptPostEntryList (AcceptPost entries) = NE.toList entries


entryList :: ParserT st String [AcceptPostEntry]
entryList = httpList ((toEntry <$> mediaRangeParser) <|> (AcceptPostAny <$ $(char '*')))
  where
    toEntry (MediaRange media []) = AcceptPostMediaType media
    toEntry range = AcceptPostMediaRange range


renderAcceptPost :: AcceptPost -> M.Builder
renderAcceptPost AcceptPostEmpty = mempty
renderAcceptPost (AcceptPost entries) = sepByCommas1 (fmap renderEntry entries)
  where
    renderEntry AcceptPostAny = "*"
    renderEntry (AcceptPostMediaType mt) = renderMediaType mt
    renderEntry (AcceptPostMediaRange range) = renderMediaRange range
