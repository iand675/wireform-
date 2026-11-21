{-# LANGUAGE BangPatterns #-}

{- |
RFC 9110 §6.6.2 @Trailer@ — lists the field names the sender anticipates
emitting in the trailer section after the message content.

== Grammar

@
Trailer = #field-name
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-6.6.2>

See also: "Network.HTTP.Headers.TransferEncoding", "Network.HTTP.Headers.TE",
"Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.Trailer (
  Trailer (..),
  trailerParser,
  renderTrailer,
  isForbiddenTrailerName,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTrailer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedText, encodeCommaSeparatedTokensChecked, sepByCommas1, shortText)


-- | The field names that may appear in trailers. 'TrailerEmpty' announces none.
data Trailer
  = Trailer {trailerFieldNames :: NonEmpty ST.ShortText}
  | TrailerEmpty
  deriving stock (Eq, Show)


instance Semigroup Trailer where
  TrailerEmpty <> other = other
  other <> TrailerEmpty = other
  Trailer xs <> Trailer ys = Trailer (xs <> ys)


instance KnownHeader Trailer where
  type ParseFailure Trailer = String
  type Cardinality Trailer = 'ZeroOrMore
  type Direction Trailer = 'RequestAndResponse


  parseFromHeaders _ (wire :| [])
    | wire == "Content-Digest" = Right (Trailer ("Content-Digest" :| []))
    | wire == "content-digest" = Right (Trailer ("content-digest" :| []))
  parseFromHeaders _ fields = parseTrailerFields fields
  {-# INLINE parseFromHeaders #-}


  renderToHeaders _ TrailerEmpty = [""]
  renderToHeaders _ (Trailer names) = [encodeCommaSeparatedText names]


  renderToHeadersChecked _ TrailerEmpty = Just [""]
  renderToHeadersChecked _ (Trailer names) = pure <$> encodeCommaSeparatedTokensChecked names


  headerName _ = hTrailer


parseTrailerFields :: NonEmpty ByteString -> Either String Trailer
parseTrailerFields fields = case parseListFieldLines "Trailer" (httpList fieldName) fields of
  Right names -> Right $! fromNames names
  Left problem -> Left problem
{-# NOINLINE parseTrailerFields #-}


trailerParser :: ParserT st String Trailer
trailerParser = fromNames <$> httpList fieldName


fromNames :: [ST.ShortText] -> Trailer
fromNames = maybe TrailerEmpty Trailer . NE.nonEmpty


renderTrailer :: Trailer -> M.Builder
renderTrailer TrailerEmpty = mempty
renderTrailer (Trailer names) = sepByCommas1 (fmap shortText names)


-- | Common fields that require processing before the trailer section
-- or are connection-specific. For extensions, the caller is responsible
-- for knowing whether the field definition permits trailers.
{-# INLINE isForbiddenTrailerName #-}
isForbiddenTrailerName :: ByteString -> Bool
isForbiddenTrailerName name
  | BS.null name = False
  | otherwise = case chr (fromIntegral (asciiLowerByte (BSU.unsafeHead name))) of
      'a' -> matches "authorization"
      'c' -> matches "connection" || matches "content-length" || matches "content-type"
          || matches "content-encoding" || matches "content-range"
      'e' -> matches "expect"
      'h' -> matches "host"
      'k' -> matches "keep-alive"
      'p' -> matches "proxy-connection" || matches "proxy-authenticate" || matches "proxy-authorization"
      'r' -> matches "range"
      't' -> matches "te" || matches "trailer" || matches "transfer-encoding"
      'u' -> matches "upgrade"
      _ -> False
  where
    len = BS.length name
    matches known = len == BS.length known && equalTrailerNameTail name known 1


-- Both names have the same length and the dispatch already matched
-- the first byte. Pass the operands explicitly through the loop.
equalTrailerNameTail :: ByteString -> ByteString -> Int -> Bool
equalTrailerNameTail name known !i
  | i == BS.length name = True
  | otherwise =
      asciiLowerByte (BSU.unsafeIndex name i) == BSU.unsafeIndex known i
        && equalTrailerNameTail name known (i + 1)


{-# INLINE asciiLowerByte #-}
asciiLowerByte :: Word8 -> Word8
asciiLowerByte w = if w >= 65 && w <= 90 then w + 32 else w
