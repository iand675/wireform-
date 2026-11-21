{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Extended HTTP parameter values (RFC 8187).
module Network.HTTP.Grammar.ExtendedValue (
  ExtendedValue (..),
  extendedValueBytes,
  parseExtendedValue,
  decodeExtendedValue,
  decodeExtendedValueText,
  renderExtendedValue,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.CharSet as CharSet
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (pokeByteOff)
import Network.HTTP.Grammar.Authority (pctEncoded)
import Network.HTTP.Grammar.LanguageTag (languageTagBytes)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.Rendering (shortText)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.URL.Decode (urlDecodeMaybe)
import qualified Wireform.Builder as B
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Switch (switchAsciiCI)


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
  octets <- decodedOctets
  pure (ExtendedValue encoding lang octets)


decodedOctets :: ParserT st e ByteString
decodedOctets = withByteString valueChars $ \() bytes -> maybe failed pure (urlDecodeMaybe bytes)
{-# INLINE decodedOctets #-}


charset :: ParserT st e ()
charset = asciiAlnumSetBytes charsetChars
{-# INLINE charset #-}


language :: ParserT st e (Maybe ByteString)
language = optional languageTagBytes <* $(char '\'')


valueChars :: ParserT st e ()
valueChars = skipMany (asciiAlnumSetBytes attrChars <|> pctEncoded)
{-# INLINE valueChars #-}


attrChar :: Word8 -> Bool
attrChar byte = case attrChars of AsciiSet low high -> asciiMember low high (BSI.w2c byte)
{-# INLINE attrChar #-}


charsetChars :: AsciiSet
charsetChars = asciiSet (asciiAlphanumeric <> "!#$%&+-^_`{}~")
{-# NOINLINE charsetChars #-}


attrChars :: AsciiSet
attrChars = asciiSet (asciiAlphanumeric <> "!#$&+-.^_`|~")
{-# NOINLINE attrChars #-}


asciiAlphanumeric :: CharSet.CharSet
asciiAlphanumeric = CharSet.range 'A' 'Z' <> CharSet.range 'a' 'z' <> CharSet.range '0' '9'


{- | Decode UTF-8 and the legacy ISO-8859-1 extension. Unknown charsets and
undecodable sequences leave the caller free to use a plain-text parameter.
-}
decodeExtendedValue :: ExtendedValue -> Maybe Text
decodeExtendedValue value =
  either (const Nothing) (\decode -> decode (extendedOctets value)) $
    parseByteString (textDecoder <* eof) (ST.toByteString (extendedCharset value))


-- | Read the text of an extended parameter directly from its wire spelling.
decodeExtendedValueText :: ByteString -> Maybe Text
decodeExtendedValueText = either (const Nothing) id . parseByteString parser
  where
    parser :: ParserT st () (Maybe Text)
    parser = do
      decode <- textDecoder <* $(char '\'')
      _ <- language
      octets <- decodedOctets <* eof
      pure (decode octets)


textDecoder :: ParserT st e (ByteString -> Maybe Text)
textDecoder =
  $( switchAsciiCI
       [|
         case _ of
           "utf-8" -> pure (either (const Nothing) Just . TE.decodeUtf8')
           "iso-8859-1" -> pure (Just . TE.decodeLatin1)
         |]
   )
{-# INLINE textDecoder #-}


-- | Render an extended parameter with uppercase percent escapes.
renderExtendedValue :: ExtendedValue -> M.Builder
renderExtendedValue (ExtendedValue encoding lang octets) =
  shortText encoding <> "'" <> foldMap shortText lang <> "'" <> renderOctets
  where
    renderOctets
      | BS.all attrChar octets = M.byteString octets
      | otherwise = B.primBounded (Prim.boundedPrim bound write) octets
    len = BS.length octets
    add = BSI.checkedAdd "RFC 8187 encoded value"
    bound = add len (add len len)
    write input ptr = go 0 0
      where
        go !source !dest
          | source == len = pure (ptr `plusPtr` dest)
          | otherwise =
              let byte = BSU.unsafeIndex input source
              in if attrChar byte
                   then pokeByteOff ptr dest byte *> go (source + 1) (dest + 1)
                   else do
                     pokeByteOff ptr dest (37 :: Word8)
                     pokeByteOff ptr (dest + 1) (hex (byte `shiftR` 4))
                     pokeByteOff ptr (dest + 2) (hex (byte .&. 15))
                     go (source + 1) (dest + 3)
    hex digit = if digit < 10 then 48 + digit else 55 + digit
