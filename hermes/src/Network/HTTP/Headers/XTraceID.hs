{- |
@X-Trace-ID@ request and response header — an opaque identifier for a
distributed trace, propagated across services so that logs, metrics, and spans
for one end-to-end operation can be stitched together. It plays the same role as
the standardised @traceparent@ (W3C Trace Context) or vendor headers (B3,
Datadog, AWS X-Ray) but uses the conventional @X-@ name many gateways and meshes
emit and forward.

This is a __de-facto__ header with __no IANA registration__ and no formal
grammar — the value is an opaque string. We preserve it verbatim as a
'ST.ShortText'.

Spec: de-facto, unregistered; see
<https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/observability/tracing>.

See also: "Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.Tracestate",
"Network.HTTP.Headers.XCorrelationID", "Network.HTTP.Headers.XRequestID".
-}
module Network.HTTP.Headers.XTraceID (
  XTraceID (..),
  xTraceIDParser,
  renderXTraceID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXTraceID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | An @X-Trace-ID@ value: an opaque distributed-trace identifier, preserved
verbatim.
-}
newtype XTraceID = XTraceID {xTraceID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XTraceID where
  type ParseFailure XTraceID = String
  type Cardinality XTraceID = 'ZeroOrOne
  type Direction XTraceID = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser xTraceIDParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing X-Trace-ID header: " <> show rest
      Fail -> Left "Failed to parse X-Trace-ID header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXTraceID


  headerName _ = hXTraceID


xTraceIDParser :: ParserT st String XTraceID
xTraceIDParser = XTraceID <$> takeRestShortText


renderXTraceID :: XTraceID -> M.Builder
renderXTraceID (XTraceID tok) = shortText tok
