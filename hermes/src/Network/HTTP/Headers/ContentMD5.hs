{- |
RFC 1864 @Content-MD5@ — an end-to-end integrity check carrying the
base64 encoding of the 128-bit (16-byte) MD5 digest of the message
body, as defined in RFC 1864 (and originally RFC 1521 §7.4.6). It may
appear on requests and responses.

This field is /obsolete/ and was removed from HTTP\/1.1 by RFC 7231;
it is retained here for interoperability with legacy peers. The value
is stored as the decoded 16-byte digest 'ByteString' and re-encoded to
base64 on rendering.

== Grammar

@
Content-MD5 = <base64 of 128-bit MD5 digest, per RFC 1864>
@

Spec: <https://www.rfc-editor.org/rfc/rfc1864>

See also: "Network.HTTP.Headers.ContentDigest", "Network.HTTP.Headers.Digest", "Network.HTTP.Headers.ReprDigest".
-}
module Network.HTTP.Headers.ContentMD5 (
  ContentMD5 (..),
  mkContentMD5,
  contentMD5Parser,
  renderContentMD5,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Grammar.Parser
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentMD5)
import qualified Network.HTTP.Headers.Mason as M
import Wireform.Base64 (encodeBase64)


-- | The decoded 128-bit (16-byte) MD5 digest of the message body.
newtype ContentMD5 = ContentMD5 {contentMD5Digest :: ByteString}
  deriving stock (Eq, Show)


-- | Construct the field from exactly sixteen digest octets.
mkContentMD5 :: ByteString -> Maybe ContentMD5
mkContentMD5 bytes
  | BS.length bytes == 16 = Just (ContentMD5 bytes)
  | otherwise = Nothing


instance KnownHeader ContentMD5 where
  type ParseFailure ContentMD5 = String
  type Cardinality ContentMD5 = 'ZeroOrOne
  type Direction ContentMD5 = 'RequestAndResponse


  parseFromHeaders _ (wire :| []) =
    maybe (Left "Invalid Content-MD5 digest") (Right . ContentMD5) $
      decodeFixedBase64 16 (BS.dropWhileEnd isFieldSpace (BS.dropWhile isFieldSpace wire))
    where
      isFieldSpace byte = byte == 32 || byte == 9
  parseFromHeaders _ _ = Left "Multiple Content-MD5 field lines"


  renderToHeaders _ = encodeBase64 . contentMD5Digest


  renderToHeadersChecked _ (ContentMD5 bytes)
    | BS.length bytes == 16 = Just (encodeBase64 bytes)
    | otherwise = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hContentMD5


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

contentMD5Parser :: ParserT st String ContentMD5
contentMD5Parser = ows *> (ContentMD5 <$> fixedBase64Bytes 16)


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderContentMD5 :: ContentMD5 -> M.Builder
renderContentMD5 = M.byteString . encodeBase64 . contentMD5Digest
