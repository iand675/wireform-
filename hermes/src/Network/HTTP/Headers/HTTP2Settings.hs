{- |
The @HTTP2-Settings@ request field was defined in RFC 7540 for the
cleartext HTTP/2 upgrade mechanism. RFC 9113 §11.1 marks the field
obsolete. Its value carries the payload of an HTTP/2
@SETTINGS@ frame, encoded with URL- and filename-safe base64 (RFC 4648 §5)
without padding:

@
HTTP2-Settings = token68   ; base64url(SETTINGS payload), no padding
@

The decoded payload contains complete six-octet settings entries.
'http2SettingsEntries' provides the ordered typed view, including
unknown identifiers and repeated settings. Applying a setting to the
connection and validating its value are responsibilities of HTTP/2.

Spec: <https://www.rfc-editor.org/rfc/rfc7540.html#section-3.2.1>

See also: "Network.HTTP.Headers.Upgrade", "Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.HTTP2Settings (
  HTTP2Settings (..),
  HTTP2Setting (..),
  http2SettingsEntries,
  http2SettingsFromEntries,
  http2SettingsParser,
  renderHTTP2Settings,
) where

import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Unsafe as BU
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word16, Word32)
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHTTP2Settings)
import qualified Network.HTTP.Headers.Mason as M
import qualified Wireform.Builder as WB
import Wireform.Parser (err, takeRest)


-- | @HTTP2-Settings@ value: the decoded HTTP/2 @SETTINGS@ frame payload octets.
newtype HTTP2Settings = HTTP2Settings {http2SettingsPayload :: ByteString}
  deriving stock (Eq, Show)


-- | One SETTINGS parameter, in wire order. Identifiers are extensible.
data HTTP2Setting = HTTP2Setting
  { http2SettingIdentifier :: {-# UNPACK #-} !Word16
  , http2SettingValue :: {-# UNPACK #-} !Word32
  }
  deriving stock (Eq, Show)


instance KnownHeader HTTP2Settings where
  type ParseFailure HTTP2Settings = String
  type Cardinality HTTP2Settings = 'ZeroOrOne
  type Direction HTTP2Settings = 'Request


  parseFromHeaders _ (wire :| []) = decodeField wire
  parseFromHeaders _ _ = Left "Multiple HTTP2-Settings field lines"


  renderToHeaders _ = encodeField


  renderToHeadersChecked _ value
    | validSettingsPayload (http2SettingsPayload value) = Just (encodeField value)
    | otherwise = Nothing


  headerName _ = hHTTP2Settings


http2SettingsParser :: ParserT st String HTTP2Settings
http2SettingsParser = takeRest >>= either err pure . decodeField


decodeField :: ByteString -> Either String HTTP2Settings
decodeField wire
  | B.null encoded = Left "HTTP2-Settings requires a nonempty token68"
  | B.length encoded `rem` 8 /= 0 = Left "Incomplete HTTP2-Settings parameter"
  | otherwise = HTTP2Settings <$> Base64URL.decodeUnpadded encoded
  where
    encoded = B.dropWhileEnd isOWS (B.dropWhile isOWS wire)
    isOWS byte = byte == 32 || byte == 9
{-# INLINE decodeField #-}


{- | Read the parameters in order. An invalid payload supplied via the raw
constructor yields 'Nothing'.
-}
http2SettingsEntries :: HTTP2Settings -> Maybe (NonEmpty HTTP2Setting)
http2SettingsEntries (HTTP2Settings payload)
  | not (validSettingsPayload payload) = Nothing
  | otherwise = Just (entry 0 :| remaining 6)
  where
    size = B.length payload
    remaining offset
      | offset == size = []
      | otherwise = entry offset : remaining (offset + 6)
    entry offset =
      HTTP2Setting
        ((octet offset `unsafeShiftL` 8) .|. octet (offset + 1))
        ( (octet (offset + 2) `unsafeShiftL` 24)
            .|. (octet (offset + 3) `unsafeShiftL` 16)
            .|. (octet (offset + 4) `unsafeShiftL` 8)
            .|. octet (offset + 5)
        )
    octet :: Num a => Int -> a
    octet i = fromIntegral (BU.unsafeIndex payload i)
    {-# INLINE octet #-}


validSettingsPayload :: ByteString -> Bool
validSettingsPayload payload = size /= 0 && size `rem` 6 == 0
  where
    size = B.length payload
{-# INLINE validSettingsPayload #-}


-- | Construct a payload from one or more ordered parameters.
http2SettingsFromEntries :: NonEmpty HTTP2Setting -> HTTP2Settings
http2SettingsFromEntries =
  HTTP2Settings . WB.toStrictByteString . foldMap (\(HTTP2Setting ident value) -> WB.word16BE ident <> WB.word32BE value)


renderHTTP2Settings :: HTTP2Settings -> M.Builder
renderHTTP2Settings = M.byteString . encodeField


encodeField :: HTTP2Settings -> ByteString
encodeField = Base64URL.encodeUnpadded . http2SettingsPayload
{-# INLINE encodeField #-}
