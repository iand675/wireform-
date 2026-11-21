{- |
RFC 6455 §11.3.1 @Sec-WebSocket-Key@ — request header sent by a
client during the WebSocket opening handshake. Its value is the
base64 encoding of a 16-byte random nonce.

== Grammar

@
Sec-WebSocket-Key = base64-value-non-empty   ; base64 of 16 octets
@

The decoded 16 octets are surfaced directly as a 'ByteString'; the
renderer re-encodes them with canonical (padded) base64, so a
canonical value round-trips byte-for-byte.

Spec: <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.1>

See also: "Network.HTTP.Headers.SecWebSocketAccept", "Network.HTTP.Headers.SecWebSocketVersion", "Network.HTTP.Headers.SecWebSocketProtocol", "Network.HTTP.Headers.SecWebSocketExtensions", "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.SecWebSocketKey (
  SecWebSocketKey (..),
  secWebSocketKeyParser,
  renderSecWebSocketKey,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketKey)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Base64 (encodeBase64)


-- | The decoded 16-octet nonce carried by @Sec-WebSocket-Key@.
newtype SecWebSocketKey = SecWebSocketKey {secWebSocketKeyBytes :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketKey where
  type ParseFailure SecWebSocketKey = String
  type Cardinality SecWebSocketKey = 'ZeroOrOne
  type Direction SecWebSocketKey = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Sec-WebSocket-Key" secWebSocketKeyParser


  renderToHeaders _ = encodeBase64 . secWebSocketKeyBytes


  renderToHeadersChecked _ (SecWebSocketKey bytes)
    | BS.length bytes == 16 = Just (encodeBase64 bytes)
    | otherwise = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hSecWebSocketKey


secWebSocketKeyParser :: ParserT st String SecWebSocketKey
secWebSocketKeyParser = ows *> (SecWebSocketKey <$> fixedBase64Bytes 16)


renderSecWebSocketKey :: SecWebSocketKey -> M.Builder
renderSecWebSocketKey (SecWebSocketKey bs) = M.byteString (encodeBase64 bs)
