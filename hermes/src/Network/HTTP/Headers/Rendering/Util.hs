module Network.HTTP.Headers.Rendering.Util where

import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Fixed (Fixed (..), Milli)
import qualified Data.Foldable1 as F1
import qualified Data.Text as T
import Data.Text.Short (ShortText, toShortByteString)
import qualified Data.Text.Short as TS
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, toText)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), RFC8941String (..), RFC8941Token (..))
import Network.HTTP.Headers.Settings


sepByCommas1 :: F1.Foldable1 t => t M.Builder -> M.Builder
sepByCommas1 = F1.intercalate1 ", "
{-# INLINE sepByCommas1 #-}


shortText :: ShortText -> M.Builder
shortText = M.shortByteString . toShortByteString
{-# INLINE shortText #-}


rfc8941Integer :: Int -> M.Builder
rfc8941Integer = M.intDec
{-# INLINE rfc8941Integer #-}


rfc8941Boolean :: Bool -> M.Builder
rfc8941Boolean True = "?0"
rfc8941Boolean False = "?1"
{-# INLINE rfc8941Boolean #-}


rfc8941Binary :: ByteString -> M.Builder
rfc8941Binary bs = M.char7 ':' <> M.byteString (convertToBase Base64 bs)
{-# INLINE rfc8941Binary #-}


rfc8941String :: RFC8941String -> M.Builder
rfc8941String (RFC8941String t) =
  M.char7 '"' <> escapedChars <> M.char7 '"'
  where
    -- RFC 8941 §4.1.5 and RFC 9110 §5.6.4 both require that
    -- DQUOTE and backslash inside the body be escaped with a
    -- leading backslash. The unescaped variant that lived here
    -- previously round-tripped values like @realm=\"foo\\\"bar\"@
    -- as @realm=\"foo\"bar\"@, which is a parse error on the wire.
    escapedChars :: M.Builder
    escapedChars =
      TS.foldl'
        ( \b c -> case c of
            '"' -> b <> M.char7 '\\' <> M.char7 '"'
            '\\' -> b <> M.char7 '\\' <> M.char7 '\\'
            c' -> b <> M.char7 c'
        )
        mempty
        t
{-# INLINE rfc8941String #-}


rfc8941Token :: RFC8941Token -> M.Builder
rfc8941Token = shortText . unsafeToRFC8941Token
{-# INLINE rfc8941Token #-}


rfc8941Decimal :: Milli -> M.Builder
rfc8941Decimal (MkFixed i) = case i `divMod` 1000 of
  (d, m) -> M.integerDec d <> M.char7 '.' <> M.integerDec m
{-# INLINE rfc8941Decimal #-}


rfc8941ItemValue :: ItemValue -> M.Builder
rfc8941ItemValue = \case
  Integer i -> rfc8941Integer i
  Decimal d -> rfc8941Decimal d
  Boolean b -> rfc8941Boolean b
  Binary bs -> rfc8941Binary bs
  String t -> rfc8941String t
  Token t -> rfc8941Token t
{-# INLINE rfc8941ItemValue #-}


data EmptyParameterRenderMode = IncludeIfEmpty | ExcludeIfEmpty


rfc8941Parameter :: EmptyParameterRenderMode -> (a -> M.Builder) -> ShortText -> Maybe a -> M.Builder
rfc8941Parameter IncludeIfEmpty f k = \case
  Nothing -> M.char7 ';' <> shortText k
  Just v -> M.char7 ';' <> shortText k <> M.char7 '=' <> f v
rfc8941Parameter ExcludeIfEmpty f k = \case
  Nothing -> mempty
  Just v -> M.char7 ';' <> shortText k <> M.char7 '=' <> f v
{-# INLINE rfc8941Parameter #-}


{- | Given maximum header size, a field name, and a field value, split the field value into a list of field values
that when emitted will each fit within the maximum header size when combined with the field name, and separated by a colon.

This function is best effort: if a comma-separated value is too large to fit within the maximum header size, it will be emitted as a singleton list.

If the field value is already small enough to fit within the maximum header size, it will be returned as a singleton list.

Multiple message-header fields with the same field-name MAY be present in a message if and only if the entire field-value
for that header field is defined as a comma-separated list [i.e., #(values)]. It MUST be possible to combine the
multiple header fields into one "field-name: field-value" pair, without changing the semantics of the message, by appending each subsequent field-value to the first,
each separated by a comma. The order in which header fields with the same field-name are received is therefore significant
to the interpretation of the combined field value, and thus a proxy MUST NOT change the order of these field values when a message is forwarded

The one exception to this rule is the "Set-Cookie" header field, which does not follow the list construct and SHOULD be handled as a
special case.
-}
fitHeaderSplitsToCommas :: HeaderSettings -> HeaderFieldName -> ByteString -> [ByteString]
fitHeaderSplitsToCommas hsettings hfield bs = go bs
  where
    chunkSize = maxHeaderSize hsettings - T.length (Network.HTTP.Headers.HeaderFieldName.toText hfield) - 1
    go headerContents =
      if B.length headerContents <= chunkSize
        then [headerContents]
        else
          let (chunk, rest) = B.splitAt chunkSize headerContents
              lastFittingComma = B.findIndexEnd (== 0x2C {- comma -}) chunk
              -- If there is no comma in the chunk, we can't split it, so we find the first comma we can and split there.
              firstFittingComma = case lastFittingComma of
                Nothing -> case B.findIndex (== 0x2C {- comma -}) bs of
                  Nothing -> [bs] -- No comma in the whole header, so we can't split it.
                  Just i -> B.take (i - 1) bs : go (B.drop (i + 1) bs)
                Just i -> B.take (i - 1) bs : go (B.drop (i + 1) bs)
          in firstFittingComma
