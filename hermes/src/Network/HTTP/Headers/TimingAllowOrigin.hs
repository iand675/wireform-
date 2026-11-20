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
  timingAllowOriginParser,
  renderTimingAllowOrigin,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Authority (serializedOriginListMember)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTimingAllowOrigin)
import qualified Network.HTTP.Headers.Mason as M
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
