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

import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alpha, digit)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSecWebSocketKey)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The decoded 16-octet nonce carried by @Sec-WebSocket-Key@.
newtype SecWebSocketKey = SecWebSocketKey {secWebSocketKeyBytes :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader SecWebSocketKey where
  type ParseFailure SecWebSocketKey = String
  type Cardinality SecWebSocketKey = 'ZeroOrOne
  type Direction SecWebSocketKey = 'Request


  parseFromHeaders _ headers = case runParser secWebSocketKeyParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Sec-WebSocket-Key: " <> show leftover)
    Fail -> Left "Failed to parse Sec-WebSocket-Key header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderSecWebSocketKey


  headerName _ = hSecWebSocketKey


base64CharSet :: CharSet
base64CharSet = alpha <> digit <> "+/="


secWebSocketKeyParser :: ParserT st String SecWebSocketKey
secWebSocketKeyParser = do
  ows
  withByteString (skipSome (skipSatisfyAscii (`CharSet.member` base64CharSet))) $ \_ bs ->
    case convertFromBase Base64 bs of
      Left e -> err e
      Right ok -> pure (SecWebSocketKey ok)


renderSecWebSocketKey :: SecWebSocketKey -> M.Builder
renderSecWebSocketKey (SecWebSocketKey bs) = M.byteString (convertToBase Base64 bs)
