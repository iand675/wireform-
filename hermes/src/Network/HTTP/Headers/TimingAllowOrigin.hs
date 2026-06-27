{-# LANGUAGE TemplateHaskell #-}

{- |
@Timing-Allow-Origin@ response header — W3C Resource Timing.

Lists the origins permitted to read otherwise opaque timing
information through the Resource Timing API:

@
Timing-Allow-Origin = 1#( \"*\" / serialized-origin )
serialized-origin   = scheme \"://\" host [ \":\" port ]
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

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTimingAllowOrigin)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A single list entry: either the wildcard or one serialized origin.
data TimingAllowOriginItem
  = -- | The literal @*@ wildcard.
    TimingAllowOriginAny
  | -- | A serialized origin, e.g. @https://example.com@.
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


  parseFromHeaders _ headers = fold1 <$> traverse runTimingAllowOrigin headers


  renderToHeaders _ = pure . M.toStrictByteString . renderTimingAllowOrigin


  headerName _ = hTimingAllowOrigin


runTimingAllowOrigin :: ByteString -> Either String TimingAllowOrigin
runTimingAllowOrigin bs = case runParser timingAllowOriginParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Timing-Allow-Origin header: " <> show rest
  Fail -> Left "Failed to parse Timing-Allow-Origin header"
  Err e -> Left e


-- | Characters forming a @serialized-origin@ (no comma, whitespace or @*@).
originCharSet :: CharSet
originCharSet = alnum <> CharSet.fromList "+-.:/[]"


timingAllowOriginParser :: ParserT st String TimingAllowOrigin
timingAllowOriginParser = TimingAllowOrigin <$> (item `sepBy1` (ows *> $(char ',') *> ows))
  where
    item = wildcard <|> origin
    wildcard = TimingAllowOriginAny <$ $(char '*')
    origin =
      TimingAllowOriginOrigin
        <$> shortASCIIFromParser_ (skipSome (skipSatisfyAscii (`CharSet.member` originCharSet)))


renderTimingAllowOrigin :: TimingAllowOrigin -> M.Builder
renderTimingAllowOrigin (TimingAllowOrigin items) =
  M.intersperse ", " (map renderItem (NE.toList items))
  where
    renderItem TimingAllowOriginAny = M.char7 '*'
    renderItem (TimingAllowOriginOrigin origin) = shortText origin
