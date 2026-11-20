{-# LANGUAGE TemplateHaskell #-}

-- | Extended HTTP parameter values (RFC 8187).
module Network.HTTP.Grammar.ExtendedValue (
  ExtendedValue (..),
  extendedValueBytes,
  parseExtendedValue,
  decodeExtendedValue,
  renderExtendedValue,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Network.HTTP.Grammar.Authority (pctEncoded)
import Network.HTTP.Grammar.LanguageTag (languageTagBytes)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.URL.Decode (urlDecodeMaybe)


-- | The charset, optional language tag and decoded octets of a parameter.
data ExtendedValue = ExtendedValue
  { extendedCharset :: !ST.ShortText
  , extendedLanguage :: !(Maybe ST.ShortText)
  , extendedOctets :: !ByteString
  }
  deriving stock (Eq, Show)


-- | Validate the wire syntax without decoding or normalizing its spelling.
extendedValueBytes :: ParserT st e ()
extendedValueBytes = charset *> $(char '\'') *> language *> valueChars
{-# INLINE extendedValueBytes #-}


parseExtendedValue :: ByteString -> Either String ExtendedValue
parseExtendedValue = parseFieldLine "extended parameter" $ do
  encoding <- shortASCIIFromParser_ charset <* $(char '\'')
  lang <- fmap STU.fromByteStringUnsafe <$> language
  octets <- withByteString valueChars $ \() bytes -> maybe failed pure (urlDecodeMaybe bytes)
  pure (ExtendedValue encoding lang octets)


charset :: ParserT st e ()
charset = skipSome (skipSatisfyAscii (\c -> isLatinLetter c || isDigit c || BS.elem (fromIntegral (fromEnum c)) "!#$%&+-^_`{}~"))


language :: ParserT st e (Maybe ByteString)
language = optional languageTagBytes <* $(char '\'')


valueChars :: ParserT st e ()
valueChars = skipMany (skipSatisfyAscii (attrChar . fromIntegral . fromEnum) <|> pctEncoded)
{-# INLINE valueChars #-}


attrChar :: Word8 -> Bool
attrChar byte =
  (byte >= 65 && byte <= 90)
    || (byte >= 97 && byte <= 122)
    || (byte >= 48 && byte <= 57)
    || BS.elem byte "!#$&+-.^_`|~"
{-# INLINE attrChar #-}


{- | Decode UTF-8 and the legacy ISO-8859-1 extension. Unknown charsets and
undecodable sequences leave the caller free to use a plain-text parameter.
-}
decodeExtendedValue :: ExtendedValue -> Maybe Text
decodeExtendedValue value = case BC.map toLower (ST.toByteString (extendedCharset value)) of
  "utf-8" -> either (const Nothing) Just (TE.decodeUtf8' (extendedOctets value))
  "iso-8859-1" -> Just (TE.decodeLatin1 (extendedOctets value))
  _ -> Nothing


-- | Render an extended parameter with uppercase percent escapes.
renderExtendedValue :: ExtendedValue -> M.Builder
renderExtendedValue (ExtendedValue encoding lang octets) =
  shortText encoding <> "'" <> foldMap shortText lang <> "'" <> BS.foldr renderByte mempty octets
  where
    renderByte byte rest
      | attrChar byte = M.word8 byte <> rest
      | otherwise = "%" <> hex (byte `shiftR` 4) <> hex (byte .&. 15) <> rest
    hex digit = M.word8 (if digit < 10 then 48 + digit else 55 + digit)
