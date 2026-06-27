{-# LANGUAGE TemplateHaskell #-}

{- |
@X-Request-Start@ request header — added to a request by a reverse proxy or load
balancer to record the instant the request was first received at the edge.
Downstream application middleware (notably New Relic) subtracts this from the
time the app begins handling the request to report \"request queueing\" time.

This is a __de-facto__ header with __no IANA registration__. The value is a
Unix-epoch timestamp whose unit and precision are host-specific:

  * Heroku's router emits a bare epoch in milliseconds (e.g. @1700000000000@).
  * Nginx (@$msec@) emits @t=@ followed by seconds with millisecond resolution
    (e.g. @t=1700000000.123@).
  * New Relic's documented form is @t=MICROSECONDS_SINCE_EPOCH@; recent agents
    also accept seconds\/milliseconds, integer or floating point, with the
    leading @t=@ omitted.

Because the numeric unit and precision vary by deployment, we capture the one
piece of stable structure — the optional @t=@ marker — and preserve the
timestamp digits verbatim as a 'ST.ShortText'. This round-trips exactly and
fabricates no grammar the wild does not honour.

> X-Request-Start = [ "t=" ] timestamp

Spec: de-facto, unregistered; see
<https://docs.newrelic.com/docs/apm/applications-menu/features/configure-request-queue-reporting/>
and <https://devcenter.heroku.com/articles/http-routing>.

See also: "Network.HTTP.Headers.ServerTiming", "Network.HTTP.Headers.XRequestID",
"Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XRequestStart (
  XRequestStart (..),
  xRequestStartParser,
  renderXRequestStart,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRequestStart)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | An @X-Request-Start@ value: an edge-arrival Unix timestamp, optionally
carrying the @t=@ marker. The timestamp text is preserved verbatim because
its unit (seconds\/milliseconds\/microseconds) and precision are
host-specific.
-}
data XRequestStart = XRequestStart
  { xRequestStartTPrefixed :: !Bool
  -- ^ whether the value carried the leading @t=@ marker
  , xRequestStartTimestamp :: !ST.ShortText
  -- ^ the raw epoch timestamp text (digits, optionally with a fractional part)
  }
  deriving stock (Eq, Show)


instance KnownHeader XRequestStart where
  type ParseFailure XRequestStart = String
  type Cardinality XRequestStart = 'ZeroOrOne
  type Direction XRequestStart = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser xRequestStartParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing X-Request-Start header: " <> show rest
      Fail -> Left "Failed to parse X-Request-Start header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXRequestStart


  headerName _ = hXRequestStart


xRequestStartParser :: ParserT st String XRequestStart
xRequestStartParser = do
  prefixed <- (True <$ $(string "t=")) <|> pure False
  XRequestStart prefixed <$> takeRestShortText


renderXRequestStart :: XRequestStart -> M.Builder
renderXRequestStart (XRequestStart prefixed ts) =
  (if prefixed then "t=" else mempty) <> shortText ts
