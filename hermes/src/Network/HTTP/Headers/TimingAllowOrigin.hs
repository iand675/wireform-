{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
@Timing-Allow-Origin@ response header — W3C Resource Timing.

Lists the origins permitted to read otherwise opaque timing
information through the Resource Timing API:

@
Timing-Allow-Origin = 1#( \"*\" / origin-or-null )
origin-or-null      = "null" / serialized-origin
@

The wildcard @*@ grants access to every origin. Multiple header
lines are combined into a single list.

Spec: <https://www.w3.org/TR/resource-timing/#sec-timing-allow-origin>.

See also: "Network.HTTP.Headers.ServerTiming", "Network.HTTP.Headers.Origin",
"Network.HTTP.Headers.AccessControlAllowOrigin".
-}
module Network.HTTP.Headers.TimingAllowOrigin (
  TimingAllowOrigin (..),
  TimingAllowOriginItem (..),
  TimingAllowContext (..),
  timingAllowOriginMatches,
  timingAllowCheck,
  navigationTimingAllowCheck,
  timingAllowOriginParser,
  renderTimingAllowOrigin,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (isSerializedOriginListMember, serializedOriginListMember)
import Network.HTTP.Grammar.Rendering (encodeCommaSeparatedBytes)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTimingAllowOrigin)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Origin (CorsRequestOrigin, corsRequestOriginBytes)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single list entry: either the wildcard or one serialized origin.
data TimingAllowOriginItem
  = -- | The literal @*@ wildcard.
    TimingAllowOriginAny
  | -- | A serialized origin, including @null@ for an opaque origin.
    TimingAllowOriginOrigin !ST.ShortText
  deriving stock (Eq, Show)


-- | The non-empty list of permitted origins.
newtype TimingAllowOrigin = TimingAllowOrigin {timingAllowOriginItems :: NE.NonEmpty TimingAllowOriginItem}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


-- | Request and response state used by the Fetch timing allow check.
data TimingAllowContext = TimingAllowContext
  { timingRequestOrigin :: !CorsRequestOrigin
  -- ^ The serialized request origin with redirect taint already applied.
  , timingAllowFailed :: !Bool
  -- ^ The request's timing allow failed flag from earlier responses.
  , timingCrossOriginNavigation :: !Bool
  -- ^ The request is a navigation and its current URL has a different origin.
  , timingBasicResponse :: !Bool
  -- ^ The request's response tainting is @basic@.
  }
  deriving stock (Eq, Show)


{- | Match an origin or wildcard using Fetch's decoding and splitting rules.
Use the raw response to preserve the handling of quoted or malformed members.
-}
timingAllowOriginMatches :: CorsRequestOrigin -> HeaderMap -> Bool
timingAllowOriginMatches origin fields =
  case lookupRawHeader hTimingAllowOrigin fields of
    Nothing -> False
    Just values -> timingFieldMatches origin values


-- | Apply the Fetch timing allow check to the current response.
timingAllowCheck :: TimingAllowContext -> HeaderMap -> Bool
timingAllowCheck context fields
  | timingAllowFailed context = False
  | timingBasicResponse context && not (timingCrossOriginNavigation context) = True
  | otherwise = timingAllowOriginMatches (timingRequestOrigin context) fields


{- | Apply the navigation timing allow check to a chain of responses.
Each response that supplies a field must allow the final destination origin.
-}
navigationTimingAllowCheck :: CorsRequestOrigin -> [HeaderMap] -> Bool
navigationTimingAllowCheck origin = all allows
  where
    allows fields = case lookupRawHeader hTimingAllowOrigin fields of
      Nothing -> True
      Just values -> timingFieldMatches origin values


timingFieldMatches :: CorsRequestOrigin -> NE.NonEmpty ByteString -> Bool
timingFieldMatches origin fields = nextMember 0
  where
    expected = corsRequestOriginBytes origin
    bytes = combineFieldLines ", " fields
    size = BS.length bytes
    at = BSU.unsafeIndex bytes
    atOWS i = let byte = at i in byte == 9 || byte == 32
    nextMember !start
      | start == size = False
      | atOWS start = nextMember (start + 1)
      | otherwise = member start start
    member !start !i
      | i == size = matches start i
      | otherwise = case at i of
          44 -> matches start i || nextMember (i + 1)
          34 -> quoted start (i + 1)
          _ -> member start (i + 1)
    quoted !start !i
      | i == size = matches start i
      | otherwise = case at i of
          34 -> member start (i + 1)
          92 | i + 1 < size -> quoted start (i + 2)
          _ -> quoted start (i + 1)
    matches !start !end
      | start == end = False
      | atOWS (end - 1) = matches start (end - 1)
      | end - start == 1 = at start == 42
      | end - start == BS.length expected = BS.take (end - start) (BS.drop start bytes) == expected
      | otherwise = False


instance KnownHeader TimingAllowOrigin where
  type ParseFailure TimingAllowOrigin = String
  type Cardinality TimingAllowOrigin = 'ZeroOrMore
  type Direction TimingAllowOrigin = 'Response


  parseFromHeaders _ headers = do
    entries <- parseListFieldLines "Timing-Allow-Origin" (httpList originItem) headers
    case NE.nonEmpty entries of
      Nothing -> Left "Timing-Allow-Origin requires an origin or wildcard"
      Just origins -> Right (TimingAllowOrigin origins)


  renderToHeaders _ = pure . M.toStrictByteString . renderTimingAllowOrigin


  renderToHeadersChecked _ (TimingAllowOrigin items)
    | all valid items = Just [encodeCommaSeparatedBytes itemBytes items]
    | otherwise = Nothing
    where
      valid TimingAllowOriginAny = True
      valid (TimingAllowOriginOrigin origin) = origin == "null" || isSerializedOriginListMember origin
      itemBytes TimingAllowOriginAny = "*"
      itemBytes (TimingAllowOriginOrigin origin) = ST.toShortByteString origin


  headerName _ = hTimingAllowOrigin


timingAllowOriginParser :: ParserT st String TimingAllowOrigin
timingAllowOriginParser = do
  entries <- httpList originItem
  case NE.nonEmpty entries of
    Nothing -> err "Timing-Allow-Origin requires an origin or wildcard"
    Just origins -> pure (TimingAllowOrigin origins)


originItem :: ParserT st String TimingAllowOriginItem
originItem =
  (TimingAllowOriginAny <$ $(char '*'))
    <|> (TimingAllowOriginOrigin <$> serializedOriginListMember)
    <|> (TimingAllowOriginOrigin "null" <$ $(string "null"))


renderTimingAllowOrigin :: TimingAllowOrigin -> M.Builder
renderTimingAllowOrigin (TimingAllowOrigin items) =
  M.intersperse ", " (map renderItem (NE.toList items))
  where
    renderItem TimingAllowOriginAny = M.char7 '*'
    renderItem (TimingAllowOriginOrigin origin) = shortText origin
