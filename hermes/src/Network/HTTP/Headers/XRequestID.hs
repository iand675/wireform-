{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@X-Request-ID@ request and response header — a unique, opaque token identifying a
single HTTP request so that it can be correlated across logs and services. A
client or the first proxy in the chain generates the token; every reverse proxy
and service in the path preserves and forwards it, and the origin server
typically echoes the same value back in the response. It is supported by Nginx,
HAProxy, AWS ALB, Heroku, and most API gateways.

Deployments define the payload. The codec checks HTTP field octets and
preserves the original bytes.

Spec: de-facto, unregistered; see <https://http.dev/x-request-id>.

See also: "Network.HTTP.Headers.XCorrelationID", "Network.HTTP.Headers.XTraceID",
"Network.HTTP.Headers.Traceparent", "Network.HTTP.Headers.XForwardedFor".
-}
module Network.HTTP.Headers.XRequestID (
  XRequestID (XRequestID, xRequestID),
  xRequestIDBytes,
  xRequestIDFromBytes,
  xRequestIDParser,
  renderXRequestID,
) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRequestID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField


-- | An opaque request correlation identifier.
newtype XRequestID = XRequestIDBytes ByteString
  deriving stock (Eq)


-- | Legacy text view. Use 'xRequestIDBytes' for the original bytes.
pattern XRequestID :: ST.ShortText -> XRequestID
pattern XRequestID {xRequestID} <- (opaqueFieldText . xRequestIDBytes -> xRequestID)
  where
    XRequestID text = XRequestIDBytes (ST.toByteString text)


{-# COMPLETE XRequestID #-}


instance Show XRequestID where
  showsPrec precedence (XRequestID text) =
    showParen (precedence > 10) $ showString "XRequestID {xRequestID = " . shows text . showChar '}'


-- | The original field bytes, including any received optional whitespace.
xRequestIDBytes :: XRequestID -> ByteString
xRequestIDBytes (XRequestIDBytes wire) = wire


-- | Check field-value octets and keep the payload unchanged.
xRequestIDFromBytes :: ByteString -> Either String XRequestID
xRequestIDFromBytes = checkOpaqueField "X-Request-ID" XRequestIDBytes


instance KnownHeader XRequestID where
  type ParseFailure XRequestID = String
  type Cardinality XRequestID = 'ZeroOrOne
  type Direction XRequestID = 'RequestAndResponse


  parseFromHeaders _ = parseOpaqueFieldLine "X-Request-ID" XRequestIDBytes


  renderToHeaders _ = xRequestIDBytes


  renderToHeadersChecked _ = renderOpaqueFieldChecked . xRequestIDBytes


  headerName _ = hXRequestID


xRequestIDParser :: ParserT st String XRequestID
xRequestIDParser = opaqueFieldParser "X-Request-ID" XRequestIDBytes


renderXRequestID :: XRequestID -> M.Builder
renderXRequestID = M.byteString . xRequestIDBytes
