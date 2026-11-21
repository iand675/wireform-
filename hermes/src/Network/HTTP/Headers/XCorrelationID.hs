{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@X-Correlation-ID@ request and response header — a single, opaque identifier for
an entire logical transaction that may span multiple HTTP calls across many
services. Whereas @X-Request-ID@ identifies one hop, the correlation id ties all
calls in the transaction together: the gateway or first service receiving the
request generates it and every downstream call propagates it unchanged.

Deployments define the payload. The codec checks HTTP field octets and
preserves the original bytes.

Spec: de-facto, unregistered; see <https://http.dev/x-correlation-id>.

See also: "Network.HTTP.Headers.XRequestID", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XCorrelationID (
  XCorrelationID (XCorrelationID, xCorrelationID),
  xCorrelationIDBytes,
  xCorrelationIDFromBytes,
  xCorrelationIDParser,
  renderXCorrelationID,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXCorrelationID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


-- | An opaque transaction correlation identifier.
newtype XCorrelationID = XCorrelationIDBytes ByteString
  deriving stock (Eq)


-- | Legacy text view. Use 'xCorrelationIDBytes' for the original bytes.
pattern XCorrelationID :: ST.ShortText -> XCorrelationID
pattern XCorrelationID {xCorrelationID} <- (opaqueFieldText . xCorrelationIDBytes -> xCorrelationID)
  where
    XCorrelationID text = XCorrelationIDBytes (ST.toByteString text)


{-# COMPLETE XCorrelationID #-}


instance Show XCorrelationID where
  showsPrec precedence (XCorrelationID text) =
    showParen (precedence > 10) $ showString "XCorrelationID {xCorrelationID = " . shows text . showChar '}'


-- | The original field bytes, including any received optional whitespace.
xCorrelationIDBytes :: XCorrelationID -> ByteString
xCorrelationIDBytes (XCorrelationIDBytes wire) = wire


-- | Check field-value octets and keep the payload unchanged.
xCorrelationIDFromBytes :: ByteString -> Either String XCorrelationID
xCorrelationIDFromBytes = checkOpaqueField "X-Correlation-ID" XCorrelationIDBytes


instance KnownHeader XCorrelationID where
  type ParseFailure XCorrelationID = String
  type Cardinality XCorrelationID = 'ZeroOrOne
  type Direction XCorrelationID = 'RequestAndResponse


  parseFromHeaders _ = parseOpaqueFieldLine "X-Correlation-ID" XCorrelationIDBytes


  renderToHeaders _ = xCorrelationIDBytes


  renderToHeadersChecked _ = renderOpaqueFieldChecked . xCorrelationIDBytes


  headerName _ = hXCorrelationID


xCorrelationIDParser :: ParserT st String XCorrelationID
xCorrelationIDParser = opaqueFieldParser "X-Correlation-ID" XCorrelationIDBytes


renderXCorrelationID :: XCorrelationID -> M.Builder
renderXCorrelationID = M.byteString . xCorrelationIDBytes
