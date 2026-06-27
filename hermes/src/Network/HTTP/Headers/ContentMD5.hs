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
  contentMD5Parser,
  renderContentMD5,
) where

import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentMD5)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The decoded 128-bit (16-byte) MD5 digest of the message body.
newtype ContentMD5 = ContentMD5 {contentMD5Digest :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader ContentMD5 where
  type ParseFailure ContentMD5 = String
  type Cardinality ContentMD5 = 'ZeroOrOne
  type Direction ContentMD5 = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser contentMD5Parser (NE.head headers) of
    OK v rest
      | B.null (dropOws rest) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Content-MD5: " <> show rest)
    Fail -> Left "Failed to parse Content-MD5 header"
    Err e -> Left e
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)
  renderToHeaders _ = M.toStrictByteString . renderContentMD5
  headerName _ = hContentMD5


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

contentMD5Parser :: ParserT st String ContentMD5
contentMD5Parser = do
  ows
  withByteString (skipSome (skipSatisfyAscii (`CharSet.member` base64CharSet))) $ \_ bs ->
    case convertFromBase Base64 bs of
      Left e -> err e
      Right ok -> pure (ContentMD5 ok)
  where
    base64CharSet =
      CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> CharSet.range '0' '9' <> "+/="


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderContentMD5 :: ContentMD5 -> M.Builder
renderContentMD5 (ContentMD5 bs) = M.byteString (convertToBase Base64 bs)
