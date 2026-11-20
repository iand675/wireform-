{- |
RFC 5023 §9.7 @SLUG@ — an Atom Publishing Protocol request header by which
a client suggests a human-readable title (and, by extension, a URI slug)
for a newly created Atom Collection member.

The field carries percent-encoded UTF-8. The codec retains the exact wire
spelling and provides a decoded title and a constructor from Unicode text.

Spec: <https://www.rfc-editor.org/rfc/rfc5023#section-9.7>

See also: "Network.HTTP.Headers.Location", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentLocation".
-}
module Network.HTTP.Headers.Slug (
  Slug (..),
  slugDecodedText,
  slugFromText,
  slugParser,
  renderSlug,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Network.HTTP.Grammar.Parser (ParserT)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hSlug)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Rendering.Util (shortText)
import Network.HTTP.URL.Decode (urlDecodeMaybe)
import Wireform.Encode.Direct (dWord8, directEncode)
import Wireform.Parser (err, takeRest)


-- | The original ASCII wire spelling of the title.
newtype Slug = Slug {slugText :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Slug where
  type ParseFailure Slug = String
  type Cardinality Slug = 'ZeroOrOne
  type Direction Slug = 'Request


  parseFromHeaders _ (wire :| []) = decodeSlug wire
  parseFromHeaders _ _ = Left "Multiple Slug field lines"


  renderToHeaders _ = ST.toByteString . slugText


  headerName _ = hSlug


slugParser :: ParserT st String Slug
slugParser = takeRest >>= either err pure . decodeSlug


decodeSlug :: ByteString -> Either String Slug
decodeSlug wire
  | BS.all (\byte -> byte == 9 || isLiteralByte byte) wire =
      Right $! Slug (STU.fromByteStringUnsafe wire)
  | maybe False BS.isValidUtf8 (decodeWire wire) =
      Right $! Slug (STU.fromByteStringUnsafe wire)
  | otherwise = Left "Invalid percent-encoded UTF-8 in Slug"
{-# INLINE decodeSlug #-}


isSlugByte :: Word8 -> Bool
isSlugByte byte = byte == 9 || (byte >= 32 && byte <= 126)
{-# INLINE isSlugByte #-}


isLiteralByte :: Word8 -> Bool
isLiteralByte byte = byte >= 32 && byte <= 126 && byte /= 37
{-# INLINE isLiteralByte #-}


decodeWire :: ByteString -> Maybe ByteString
decodeWire wire
  | BS.all isSlugByte wire = urlDecodeMaybe wire
  | otherwise = Nothing


-- | Decode the original title. A manually supplied spelling may be invalid.
slugDecodedText :: Slug -> Maybe Text
slugDecodedText slug = do
  decoded <- decodeWire (ST.toByteString (slugText slug))
  either (const Nothing) Just (TE.decodeUtf8' decoded)


{- | Encode a title with uppercase escapes. Encode surrounding spaces so
they survive an HTTP transport that strips field whitespace.
-}
slugFromText :: Text -> Slug
slugFromText = Slug . STU.fromByteStringUnsafe . encodeTitle . TE.encodeUtf8


encodeTitle :: ByteString -> ByteString
encodeTitle bytes
  | escapes == 0 = bytes
  | escapes > (maxBound - size) `quot` 2 = errorWithoutStackTrace "Slug wire value exceeds ByteString capacity"
  | otherwise = directEncode (size + 2 * escapes) $ \ptr ->
      let write source dest
            | source == size = pure dest
            | source >= first && source < end && isLiteralByte byte =
                dWord8 ptr dest byte >>= write (source + 1)
            | otherwise = do
                off <- dWord8 ptr dest 37
                off' <- dWord8 ptr off (hexDigit (byte `shiftR` 4))
                dWord8 ptr off' (hexDigit (byte .&. 15)) >>= write (source + 1)
            where
              byte = BSU.unsafeIndex bytes source
      in write 0
  where
    size = BS.length bytes
    afterLeading = BS.dropWhile (== 32) bytes
    middle = BS.dropWhileEnd (== 32) afterLeading
    first = size - BS.length afterLeading
    end = first + BS.length middle
    escapes = size - BS.length middle + BS.foldl' countEscape 0 middle
    countEscape count byte = if isLiteralByte byte then count else count + 1
    hexDigit n
      | n < 10 = 48 + n
      | otherwise = 55 + n


renderSlug :: Slug -> M.Builder
renderSlug (Slug t) = shortText t
