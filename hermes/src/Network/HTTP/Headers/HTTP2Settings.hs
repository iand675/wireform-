{- |
The @HTTP2-Settings@ request header field (RFC 9113 §3.2.1, originally
RFC 7540), sent by a client in an HTTP/1.1 request that requests an upgrade to
HTTP/2 over cleartext (h2c). Its value carries the payload of an HTTP/2
@SETTINGS@ frame, encoded with URL- and filename-safe base64 (RFC 4648 §5)
without padding:

@
HTTP2-Settings = token68   ; base64url(SETTINGS payload), no padding
@

The @SETTINGS@ payload is a sequence of 6-octet identifier\/value pairs; that
binary structure is opaque at the HTTP layer, so we surface the decoded octets
directly as a 'ByteString' and re-encode them with unpadded base64url on
render. A canonical value therefore round-trips byte-for-byte.

Spec: <https://www.rfc-editor.org/rfc/rfc9113#name-starting-http-2-for-http-ur>

See also: "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.HTTP2Settings (
  HTTP2Settings (..),
  http2SettingsParser,
  renderHTTP2Settings,
) where

import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHTTP2Settings)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @HTTP2-Settings@ value: the decoded HTTP/2 @SETTINGS@ frame payload octets.
newtype HTTP2Settings = HTTP2Settings {http2SettingsPayload :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader HTTP2Settings where
  type ParseFailure HTTP2Settings = String
  type Cardinality HTTP2Settings = 'ZeroOrOne
  type Direction HTTP2Settings = 'Request


  parseFromHeaders _ headers = case runParser http2SettingsParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing HTTP2-Settings header: " <> show leftover)
    Fail -> Left "Failed to parse HTTP2-Settings header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderHTTP2Settings


  headerName _ = hHTTP2Settings


-- | The unpadded URL- and filename-safe base64 alphabet (RFC 4648 §5).
base64UrlCharSet :: CharSet
base64UrlCharSet = alnum <> CharSet.fromList "-_"


http2SettingsParser :: ParserT st String HTTP2Settings
http2SettingsParser = do
  ows
  withByteString (skipMany (skipSatisfyAscii (`CharSet.member` base64UrlCharSet))) $ \_ bs ->
    case convertFromBase Base64URLUnpadded bs of
      Left e -> err e
      Right ok -> pure (HTTP2Settings ok)


renderHTTP2Settings :: HTTP2Settings -> M.Builder
renderHTTP2Settings (HTTP2Settings bs) = M.byteString (convertToBase Base64URLUnpadded bs)
