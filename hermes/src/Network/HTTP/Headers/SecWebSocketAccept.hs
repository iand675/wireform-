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

import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alpha, digit)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketAccept)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The decoded SHA-1 digest carried by @Sec-WebSocket-Accept@.
newtype SecWebSocketAccept = SecWebSocketAccept {secWebSocketAcceptBytes :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketAccept where
  type ParseFailure SecWebSocketAccept = String
  type Cardinality SecWebSocketAccept = 'ZeroOrOne
  type Direction SecWebSocketAccept = 'Response


  parseFromHeaders _ headers = case runParser secWebSocketAcceptParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Sec-WebSocket-Accept: " <> show leftover)
    Fail -> Left "Failed to parse Sec-WebSocket-Accept header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketAccept


  headerName _ = hSecWebSocketAccept


base64CharSet :: CharSet
base64CharSet = alpha <> digit <> "+/="


secWebSocketAcceptParser :: ParserT st String SecWebSocketAccept
secWebSocketAcceptParser = do
  ows
  withByteString (skipSome (skipSatisfyAscii (`CharSet.member` base64CharSet))) $ \_ bs ->
    case convertFromBase Base64 bs of
      Left e -> err e
      Right ok -> pure (SecWebSocketAccept ok)


renderSecWebSocketAccept :: SecWebSocketAccept -> M.Builder
renderSecWebSocketAccept (SecWebSocketAccept bs) = M.byteString (convertToBase Base64 bs)
