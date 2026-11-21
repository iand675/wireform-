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
and @desc@ (a human-readable description). Parameters retain wire order,
and 'serverTimingParameter' uses the first occurrence of a given name.
Decoded quoted values use Latin-1 code points for their wire octets.

Spec: <https://www.w3.org/TR/server-timing/>.

See also: "Network.HTTP.Headers.TimingAllowOrigin",
"Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.Tracestate".
-}
module Network.HTTP.Headers.ServerTiming (
  ServerTiming (..),
  ServerTimingMetric (..),
  ServerTimingParam (..),
  serverTimingParameter,
  serverTimingParser,
  renderServerTiming,
) where

import Data.List (find)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Rendering (encodeHttpParameterizedList, encodeHttpParameterizedListChecked, renderHttpParameterizedList)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hServerTiming)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


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


-- | Read the first parameter with this exact name, retaining its decoded
-- spelling. A later duplicate has no effect on this lookup.
serverTimingParameter :: ST.ShortText -> ServerTimingMetric -> Maybe ST.ShortText
serverTimingParameter name =
  fmap serverTimingParamValue . find ((== name) . serverTimingParamName) . serverTimingMetricParams


instance KnownHeader ServerTiming where
  type ParseFailure ServerTiming = String
  type Cardinality ServerTiming = 'ZeroOrMore
  type Direction ServerTiming = 'Response


  parseFromHeaders _ = parseFieldLine "Server-Timing" serverTimingParser . combineFieldLines ", "


  renderToHeaders _ (ServerTiming []) = []
  renderToHeaders _ (ServerTiming metrics) =
    [ encodeHttpParameterizedList
        serverTimingMetricName
        serverTimingMetricParams
        serverTimingParamName
        serverTimingParamValue
        metrics
    ]


  renderToHeadersChecked _ (ServerTiming []) = Just []
  renderToHeadersChecked _ (ServerTiming metrics) =
    pure
      <$> encodeHttpParameterizedListChecked
        isHttpTokenText
        serverTimingMetricName
        serverTimingMetricParams
        serverTimingParamName
        serverTimingParamValue
        metrics


  omitHeaderValue (ServerTiming metrics) = null metrics


  headerName _ = hServerTiming


serverTimingParser :: ParserT st String ServerTiming
serverTimingParser = ServerTiming <$> httpList metric
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
      value <- rfc9110Token <|> (shortTextFromLatin1 <$> httpQuotedBytes)
      pure (ServerTimingParam name value)


renderServerTiming :: ServerTiming -> M.Builder
renderServerTiming =
  renderHttpParameterizedList
    serverTimingMetricName
    serverTimingMetricParams
    serverTimingParamName
    serverTimingParamValue
    . serverTimingMetrics
