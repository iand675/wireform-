{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@X-Trace-ID@ request and response header — an opaque identifier for a
distributed trace, propagated across services so that logs, metrics, and spans
for one end-to-end operation can be stitched together. It plays the same role as
the standardised @traceparent@ (W3C Trace Context) or vendor headers (B3,
Datadog, AWS X-Ray) but uses the conventional @X-@ name many gateways and meshes
emit and forward.

Deployments define the payload. The codec checks HTTP field octets and
preserves the original bytes.

Spec: de-facto, unregistered; see
<https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/observability/tracing>.

See also: "Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.Tracestate",
"Network.HTTP.Headers.XCorrelationID", "Network.HTTP.Headers.XRequestID".
-}
module Network.HTTP.Headers.XTraceID (
  XTraceID (XTraceID, xTraceID),
  xTraceIDBytes,
  xTraceIDFromBytes,
  xTraceIDParser,
  renderXTraceID,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXTraceID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


-- | An opaque distributed trace identifier.
newtype XTraceID = XTraceIDBytes ByteString
  deriving stock (Eq)


-- | Legacy text view. Use 'xTraceIDBytes' for the original bytes.
pattern XTraceID :: ST.ShortText -> XTraceID
pattern XTraceID {xTraceID} <- (opaqueFieldText . xTraceIDBytes -> xTraceID)
  where
    XTraceID text = XTraceIDBytes (ST.toByteString text)


{-# COMPLETE XTraceID #-}


instance Show XTraceID where
  showsPrec precedence (XTraceID text) =
    showParen (precedence > 10) $ showString "XTraceID {xTraceID = " . shows text . showChar '}'


-- | The original field bytes, including any received optional whitespace.
xTraceIDBytes :: XTraceID -> ByteString
xTraceIDBytes (XTraceIDBytes wire) = wire


-- | Check field-value octets and keep the payload unchanged.
xTraceIDFromBytes :: ByteString -> Either String XTraceID
xTraceIDFromBytes = checkOpaqueField "X-Trace-ID" XTraceIDBytes


instance KnownHeader XTraceID where
  type ParseFailure XTraceID = String
  type Cardinality XTraceID = 'ZeroOrOne
  type Direction XTraceID = 'RequestAndResponse


  parseFromHeaders _ = parseOpaqueFieldLine "X-Trace-ID" XTraceIDBytes


  renderToHeaders _ = xTraceIDBytes


  renderToHeadersChecked _ = renderOpaqueFieldChecked . xTraceIDBytes


  headerName _ = hXTraceID


xTraceIDParser :: ParserT st String XTraceID
xTraceIDParser = opaqueFieldParser "X-Trace-ID" XTraceIDBytes


renderXTraceID :: XTraceID -> M.Builder
renderXTraceID = M.byteString . xTraceIDBytes
