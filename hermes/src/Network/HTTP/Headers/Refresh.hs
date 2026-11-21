{- |
The @Refresh@ response header gives a whole-second delay and an optional
navigation target. HTML uses the same syntax for the HTTP field and the
@\<meta http-equiv="refresh"\>@ pragma. This codec extracts the delay and
target from an HTTP field value. Resolving the target and applying
navigation policy require a document context.

Spec (WHATWG HTML shared declarative refresh steps):
<https://html.spec.whatwg.org/multipage/semantics.html#shared-declarative-refresh-steps>

See also: "Network.HTTP.Headers.Location", "Network.HTTP.Headers.RetryAfter", "Network.HTTP.Headers.Link".
-}
module Network.HTTP.Headers.Refresh (
  Refresh (..),
  refreshParser,
  renderRefresh,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Network.HTTP.Grammar.Parser (ParserT, isHttpLatin1Text, scanCompactBytes, shortTextFromLatin1)
import Network.HTTP.Grammar.Rendering (renderLatin1ShortText)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hRefresh)
import qualified Network.HTTP.Headers.Mason as M
import Wireform.Encode.Direct (dBytes, directEncode)
import Wireform.Parser (err, takeRest)


{- | A @Refresh@ directive. Delays above the range of 'Word' fail to parse.
The target uses the HTTP field's isomorphic byte-to-code-point mapping.
Use URI percent escapes for target characters beyond that byte range.
When a target needs outer quotes and contains both delimiters, rendering
percent-encodes its double quotes to avoid truncating the target.
An explicitly empty target is distinct from a delay without a target.
-}
data Refresh = Refresh
  { refreshSeconds :: !Word
  , refreshUrl :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


instance KnownHeader Refresh where
  type ParseFailure Refresh = String
  type Cardinality Refresh = 'ZeroOrOne
  type Direction Refresh = 'Response


  -- HTML leaves repeated field lines unspecified. Preserve first-line lookup.
  parseFromHeaders _ = decodeRefresh . NE.head


  renderToHeaders _ = encodeRefresh


  renderToHeadersChecked _ (Refresh 0 (Just target))
    | let bytes = ST.toShortByteString target
    , not (needsTargetQuotes bytes)
    , scanCompactBytes (\byte -> byte == 9 || (byte >= 32 && byte < 127)) bytes 0 =
        Just (encodeImmediateTarget bytes)
  renderToHeadersChecked _ value
    | maybe True isHttpLatin1Text (refreshUrl value) = Just (M.toStrictByteString (renderRefresh value))
    | otherwise = Nothing


  headerName _ = hRefresh


refreshParser :: ParserT st String Refresh
refreshParser = takeRest >>= either err pure . decodeRefresh


decodeRefresh :: ByteString -> Either String Refresh
decodeRefresh wire =
  let input = skipWhitespace wire
  in case BSC.readWord input of
       Just (seconds, rest) -> parseTail seconds rest
       Nothing
         | not (BS.null input) && BSC.head input == '.' -> parseTail 0 input
         | otherwise -> Left "Invalid or out-of-range Refresh delay"
  where
    parseTail seconds rest
      | BS.null rest = Right $! Refresh seconds Nothing
      | otherwise =
          let suffix = BSC.dropWhile (\c -> c == '.' || ('0' <= c && c <= '9')) rest
          in case BSC.uncons suffix of
               Nothing -> Right $! Refresh seconds Nothing
               Just (first, _)
                 | BS.any invalidFieldByte suffix -> Left "Control byte in Refresh header"
                 | isRefreshWhitespace first || isSeparator first ->
                     let remainder = skipWhitespace suffix
                         rawTarget = case BSC.uncons remainder of
                           Just (sep, target) | isSeparator sep -> skipWhitespace target
                           _ -> remainder
                     in Right $! Refresh seconds (if BS.null rawTarget then Nothing else Just $! refreshTarget rawTarget)
                 | otherwise -> Left "Invalid Refresh separator"

    isSeparator c = c == ';' || c == ','
    invalidFieldByte byte = (byte < 32 && byte /= 9) || byte == 127
{-# INLINE decodeRefresh #-}


skipWhitespace :: ByteString -> ByteString
skipWhitespace = BSC.dropWhile isRefreshWhitespace
{-# INLINE skipWhitespace #-}


isRefreshWhitespace :: Char -> Bool
isRefreshWhitespace c = c == ' ' || c == '\t'
{-# INLINE isRefreshWhitespace #-}


refreshTarget :: ByteString -> ST.ShortText
refreshTarget input =
  let wire = stripUrlPrefix input
      target = case BS.uncons wire of
        Just (q, body) | isQuote q -> maybe body (`BS.take` body) (BS.elemIndex q body)
        _ -> wire
  in shortTextFromLatin1 target


stripUrlPrefix :: ByteString -> ByteString
stripUrlPrefix wire
  | BS.length wire >= 4
  , let u = BSC.index wire 0
  , u == 'u' || u == 'U'
  , let r = BSC.index wire 1
  , r == 'r' || r == 'R'
  , let l = BSC.index wire 2
  , l == 'l' || l == 'L'
  , Just ('=', target) <- BSC.uncons (skipWhitespace (BS.drop 3 wire)) =
      skipWhitespace target
  | otherwise = wire


isQuote :: Word8 -> Bool
isQuote byte = byte == 34 || byte == 39
{-# INLINE isQuote #-}


renderRefresh :: Refresh -> M.Builder
renderRefresh (Refresh seconds url) =
  M.wordDec seconds <> maybe mempty (\u -> "; url=" <> renderTarget u) url


encodeRefresh :: Refresh -> ByteString
encodeRefresh (Refresh 0 (Just target))
  | let bytes = ST.toShortByteString target
  , not (needsTargetQuotes bytes)
  , not (SBS.any (>= 128) bytes) = encodeImmediateTarget bytes
encodeRefresh value = M.toStrictByteString (renderRefresh value)


encodeImmediateTarget :: SBS.ShortByteString -> ByteString
encodeImmediateTarget bytes =
  directEncode (BSI.checkedAdd "Refresh" (BS.length prefix) len) $ \ptr start -> do
    offset <- dBytes ptr start prefix
    SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) len
    pure (offset + len)
  where
    prefix = "0; url="
    len = SBS.length bytes
{-# INLINE encodeImmediateTarget #-}


renderTarget :: ST.ShortText -> M.Builder
renderTarget target
  | not (needsTargetQuotes bytes) = body
  | not (SBS.elem 34 bytes) = quoted 34 body
  | not (SBS.elem 39 bytes) = quoted 39 body
  | otherwise = quoted 34 (renderLatin1ShortText (ST.fromText (T.replace "\"" "%22" (ST.toText target))))
  where
    bytes = ST.toShortByteString target
    body = renderLatin1ShortText target
    quoted quote value = M.word8 quote <> value <> M.word8 quote


needsTargetQuotes :: SBS.ShortByteString -> Bool
needsTargetQuotes bytes =
  not (SBS.null bytes) && (isQuote first || whitespace first || whitespace lastByte)
  where
    first = SBSI.unsafeIndex bytes 0
    lastByte = SBSI.unsafeIndex bytes (SBS.length bytes - 1)
    whitespace byte = byte == 32 || byte == 9
{-# INLINE needsTargetQuotes #-}
