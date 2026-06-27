{-# LANGUAGE TemplateHaskell #-}

{- |
@Server-Timing@ response header — W3C Server Timing.

Communicates one or more performance metrics about the
request-response cycle back to the user agent:

@
Server-Timing = #server-timing-metric
metric        = name *( OWS \";\" OWS param )
name          = token
param         = param-name OWS \"=\" OWS ( token / quoted-string )
@

The registered parameters are @dur@ (a duration in milliseconds)
and @desc@ (a human-readable description); both are surfaced as
ordinary, order-preserving parameters. Parameter values are stored
decoded; on render a value is emitted bare when it is a valid
@token@ and as a @quoted-string@ otherwise.

Spec: <https://www.w3.org/TR/server-timing/>.

See also: "Network.HTTP.Headers.TimingAllowOrigin",
"Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.Tracestate".
-}
module Network.HTTP.Headers.ServerTiming (
  ServerTiming (..),
  ServerTimingMetric (..),
  ServerTimingParam (..),
  serverTimingParser,
  renderServerTiming,
) where

import Control.Monad.Combinators (sepBy)
import Data.ByteString (ByteString)
import qualified Data.CharSet as CharSet
import Data.Foldable1 (fold1)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hServerTiming)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A single @param-name=value@ pair attached to a metric.
data ServerTimingParam = ServerTimingParam
  { serverTimingParamName :: !ST.ShortText
  , serverTimingParamValue :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | A single metric: a name plus its ordered parameters (e.g. @dur@, @desc@).
data ServerTimingMetric = ServerTimingMetric
  { serverTimingMetricName :: !ST.ShortText
  , serverTimingMetricParams :: ![ServerTimingParam]
  }
  deriving stock (Eq, Show)


-- | The list of metrics carried by a (possibly repeated) header.
newtype ServerTiming = ServerTiming {serverTimingMetrics :: [ServerTimingMetric]}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)


instance KnownHeader ServerTiming where
  type ParseFailure ServerTiming = String
  type Cardinality ServerTiming = 'ZeroOrMore
  type Direction ServerTiming = 'Response


  parseFromHeaders _ headers = fold1 <$> traverse runServerTiming headers


  renderToHeaders _ = pure . M.toStrictByteString . renderServerTiming


  headerName _ = hServerTiming


runServerTiming :: ByteString -> Either String ServerTiming
runServerTiming bs = case runParser serverTimingParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Server-Timing header: " <> show rest
  Fail -> Left "Failed to parse Server-Timing header"
  Err e -> Left e


serverTimingParser :: ParserT st String ServerTiming
serverTimingParser = ServerTiming <$> (metric `sepBy` (ows *> $(char ',') *> ows))
  where
    metric = do
      name <- rfc9110Token
      params <- many (ows *> $(char ';') *> ows *> param)
      pure (ServerTimingMetric name params)
    param = do
      name <- rfc9110Token
      ows
      $(char '=')
      ows
      value <- quotedString <|> rfc9110Token
      pure (ServerTimingParam name value)


renderServerTiming :: ServerTiming -> M.Builder
renderServerTiming (ServerTiming metrics) =
  M.intersperse ", " (map renderMetric metrics)
  where
    renderMetric (ServerTimingMetric name params) =
      R.shortText name <> foldMap renderParam params
    renderParam (ServerTimingParam name value) =
      M.char7 ';' <> R.shortText name <> M.char7 '=' <> renderValue value
    renderValue value
      | isToken value = R.shortText value
      | otherwise = R.rfc8941String (RFC8941String value)
    isToken t = not (ST.null t) && ST.all (`CharSet.member` tokenCharSet) t
