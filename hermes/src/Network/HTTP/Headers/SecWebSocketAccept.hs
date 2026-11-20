{- |
RFC 6455 §11.3.3 @Sec-WebSocket-Accept@ — response header sent by a
server to complete the WebSocket opening handshake. Its value is the
base64 encoding of the 20-byte SHA-1 digest of the client's
@Sec-WebSocket-Key@ concatenated with the WebSocket GUID.

== Grammar

@
Sec-WebSocket-Accept = base64-value-non-empty   ; base64 of a SHA-1 digest
@

The decoded digest octets are surfaced directly as a 'ByteString';
the renderer re-encodes them with canonical (padded) base64, so a
canonical value round-trips byte-for-byte.

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.3>

See also: "Network.HTTP.Headers.SecWebSocketKey", "Network.HTTP.Headers.SecWebSocketProtocol", "Network.HTTP.Headers.SecWebSocketExtensions", "Network.HTTP.Headers.SecWebSocketVersion", "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.SecWebSocketAccept (
  SecWebSocketAccept (..),
  secWebSocketAcceptParser,
  renderSecWebSocketAccept,
) where

import Data.ByteString (ByteString)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketAccept)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Base64 (encodeBase64)


-- | The decoded SHA-1 digest carried by @Sec-WebSocket-Accept@.
newtype SecWebSocketAccept = SecWebSocketAccept {secWebSocketAcceptBytes :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketAccept where
  type ParseFailure SecWebSocketAccept = String
  type Cardinality SecWebSocketAccept = 'ZeroOrOne
  type Direction SecWebSocketAccept = 'Response


  parseFromHeaders _ = parseSingleFieldLine "Sec-WebSocket-Accept" secWebSocketAcceptParser


  renderToHeaders _ = encodeBase64 . secWebSocketAcceptBytes


  headerName _ = hSecWebSocketAccept


secWebSocketAcceptParser :: ParserT st String SecWebSocketAccept
secWebSocketAcceptParser = ows *> (SecWebSocketAccept <$> fixedBase64Bytes 20)


renderSecWebSocketAccept :: SecWebSocketAccept -> M.Builder
renderSecWebSocketAccept (SecWebSocketAccept bs) = M.byteString (encodeBase64 bs)
