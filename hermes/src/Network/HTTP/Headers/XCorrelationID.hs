{- |
@X-Correlation-ID@ request and response header — a single, opaque identifier for
an entire logical transaction that may span multiple HTTP calls across many
services. Whereas @X-Request-ID@ identifies one hop, the correlation id ties all
calls in the transaction together: the gateway or first service receiving the
request generates it and every downstream call propagates it unchanged.

This is a __de-facto__ header with __no IANA registration__ and no formal
grammar — the value is an opaque string (commonly a UUID). We preserve it
verbatim as a 'ST.ShortText'.

Spec: de-facto, unregistered; see <https://http.dev/x-correlation-id>.

See also: "Network.HTTP.Headers.XRequestID", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XCorrelationID (
  XCorrelationID (..),
  xCorrelationIDParser,
  renderXCorrelationID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXCorrelationID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | An @X-Correlation-ID@ value: an opaque transaction-correlation token,
preserved verbatim.
-}
newtype XCorrelationID = XCorrelationID {xCorrelationID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XCorrelationID where
  type ParseFailure XCorrelationID = String
  type Cardinality XCorrelationID = 'ZeroOrOne
  type Direction XCorrelationID = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser xCorrelationIDParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing X-Correlation-ID header: " <> show rest
      Fail -> Left "Failed to parse X-Correlation-ID header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXCorrelationID


  headerName _ = hXCorrelationID


xCorrelationIDParser :: ParserT st String XCorrelationID
xCorrelationIDParser = XCorrelationID <$> takeRestShortText


renderXCorrelationID :: XCorrelationID -> M.Builder
renderXCorrelationID (XCorrelationID tok) = shortText tok
