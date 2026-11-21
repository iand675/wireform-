{-# LANGUAGE BangPatterns #-}

{- |
@Accept@ — a request header by which a client lists the media types
(MIME types) it is willing to receive in the response, each with an
optional @;q=@ quality weight expressing relative preference. Servers
use it for proactive content negotiation, selecting the best matching
representation to return (or replying @406 Not Acceptable@ when none is
acceptable). Servers can also send it to advertise media types preferred
for a subsequent request.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.1>

See also: "Network.HTTP.Headers.AcceptCharset", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.Accept (
  Accept (..),
  acceptedContentQuality,
  preferredContentType,
  acceptParser,
  renderAccept,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe)
import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation
import Network.HTTP.Headers
import Network.HTTP.Headers.ContentType (ContentType (..))
import Network.HTTP.Headers.HeaderFieldName (hAccept)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype Accept = Accept {accept :: [WeightedMediaRange]}
  deriving stock (Eq, Show)


{- | The effective quality of a concrete representation under RFC 9110
section 12.5.1. An exact type takes precedence over a subtype wildcard,
which takes precedence over a fully wildcarded type. Among ranges with
the same type specificity, more matching parameter names take precedence.
The first field entry wins when specificity is equal. Parameter names
and charset values compare case-insensitively. Other values compare by
their decoded spelling. The first occurrence supplies a representation
parameter's value. Repeated requirements for a parameter count once.

An absent field gives every representation quality one. An empty field
or an unmatched type gives it zero. Authored weights are clamped to
@[0,1]@, with NaN treated as zero.
-}
acceptedContentQuality :: Maybe Accept -> ContentType -> Double
acceptedContentQuality Nothing _ = 1
acceptedContentQuality (Just (Accept ranges)) candidate = contentQuality ranges candidate
{-# INLINE acceptedContentQuality #-}


-- | Select the available content type with the highest positive effective
-- quality. Order the available list by server preference to break ties.
preferredContentType :: Maybe Accept -> [ContentType] -> Maybe ContentType
preferredContentType Nothing = listToMaybe
preferredContentType (Just (Accept ranges)) = chooseContentType ranges
{-# INLINE preferredContentType #-}


chooseContentType :: [WeightedMediaRange] -> [ContentType] -> Maybe ContentType
chooseContentType ranges = preferredByQuality (contentQuality ranges)


contentQuality :: [WeightedMediaRange] -> ContentType -> Double
contentQuality [] _ = 0
contentQuality ranges (ContentType actualType) =
  selectQuality actualType [] 0 ranges
contentQuality ranges (ParameterizedContentType actualType (first :| rest)) =
  selectQuality actualType (first : rest) (1 + length rest) ranges


selectQuality :: MediaType -> [(ST.ShortText, ST.ShortText)] -> Int -> [WeightedMediaRange] -> Double
selectQuality actualType actualParams !maxParams = select (-1) 0 0
  where
    select :: Int -> Int -> Double -> [WeightedMediaRange] -> Double
    select !bestType !bestParams !bestWeight (WeightedMediaRange range weight : rest)
      | rank >= 0 && rank >= bestType
      , matched >= 0
      , rank > bestType || matched > bestParams =
          if rank == 2 && matched == maxParams
            then normalizedQuality weight
            else select rank matched weight rest
      | otherwise = select bestType bestParams bestWeight rest
      where
        rank = typeSpecificity (mediaType range) actualType
        matched = matchingParameters actualParams (mediaParams range)
    select _ _ bestWeight [] = normalizedQuality bestWeight


typeSpecificity :: MediaType -> MediaType -> Int
typeSpecificity (MediaType base sub) (MediaType actualBase actualSub)
  | ST.null base && ST.null sub = 0
  | not (sameNegotiationToken base actualBase) = -1
  | ST.null sub = 1
  | sameNegotiationToken sub actualSub = 2
  | otherwise = -1
{-# INLINE typeSpecificity #-}


matchingParameters :: [(ST.ShortText, ST.ShortText)] -> [(ST.ShortText, ST.ShortText)] -> Int
matchingParameters available = count 0
  where
    count !total [] = total
    count !total ((name, value) : rest)
      | matchValue name value available =
          if any (sameNegotiationToken name . fst) rest then count total rest else count (total + 1) rest
      | otherwise = -1

    matchValue name value ((key, actual) : rest)
      | sameNegotiationToken name key =
          if sameNegotiationToken name "charset" then sameNegotiationToken value actual else value == actual
      | otherwise = matchValue name value rest
    matchValue _ _ [] = False


instance KnownHeader Accept where
  type ParseFailure Accept = String
  type Cardinality Accept = 'ZeroOrOne
  type Direction Accept = 'RequestAndResponse


  parseFromHeaders _ = fmap Accept . parseNegotiationHeaders "Accept" listParser


  renderToHeaders _ = M.toStrictByteString . renderAccept


  renderToHeadersChecked _ = encodeWeightedMediaRangesChecked . accept


  headerName _ = hAccept


acceptParser :: ParserT st String Accept
acceptParser = Accept <$> listParser


listParser :: ParserT st String [WeightedMediaRange]
listParser = weightedMediaRangesParser


renderAccept :: Accept -> M.Builder
renderAccept (Accept mediaRanges) = renderWeightedMediaRanges mediaRanges
