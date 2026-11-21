{-# LANGUAGE BangPatterns #-}

{- |
Renderers for the HTTP wire-grammar substrate — the inverse of
"Network.HTTP.Grammar.Parser".

This module holds the RFC 8941 (Structured Fields) and shared builder
primitives that header renderers and other HTTP grammars consume.

Header-/policy/-specific rendering (e.g. splitting a field value to fit
a maximum header size) stays in "Network.HTTP.Headers.Rendering.Util".
-}
module Network.HTTP.Grammar.Rendering (
  -- * Builder primitives
  shortText,
  copyText,
  encodeCookiePairChecked,
  writeCookiePair,
  opaqueShortText,
  renderLatin1ShortText,
  renderHttpWordValue,
  sepByCommas1,
  encodeCommaSeparatedText,
  encodeCommaSeparatedBytes,
  encodeCommaSeparatedChecked,
  encodeCommaSeparatedTextChecked,
  encodeCommaSeparatedTokensChecked,
  encodeCommaSeparatedPairs,
  encodeCommaSeparatedTokenPairs,
  encodeWordDecimal,
  encodeDecimalVersion,

  -- * RFC 9110 products and comments
  renderComment,
  renderHttpQuotedString,
  renderHttpQuotedShortText,
  encodeHttpQuotedBytes,
  encodeHttpQuotedBytesChecked,
  writeHttpQuotedBytes,
  foldHttpQuotedShortBytes,
  foldHttpQuotedText,
  renderProductSequence,
  encodeProductSequenceChecked,
  renderProductToken,
  encodeHttpParameterizedList,
  encodeHttpParameterizedListChecked,
  writeParameterValue,
  renderHttpParameterizedList,
  encodeHttpParameterList,
  renderHttpParameterList,

  -- * RFC 8941 item renderers
  rfc8941Integer,
  rfc8941Decimal,
  rfc8941Boolean,
  rfc8941Binary,
  rfc8941String,
  rfc8941Token,
  rfc8941ItemValue,
  renderStructuredFieldDictionary,
  renderStructuredFieldDictionaryChecked,
  renderStructuredFieldItemDictionaryChecked,
  renderStructuredFieldList,
  renderStructuredFieldListChecked,
  renderStructuredFieldParameter,
  renderSeparated,

  -- * RFC 8941 parameters
  EmptyParameterRenderMode (..),
  rfc8941Parameter,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Char (chr, ord)
import Data.Fixed (Fixed (..), Milli)
import qualified Data.Foldable1 as F1
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal as TextInternal
import Data.Text.Short (ShortText, toShortByteString)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Word (Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke)
import Network.HTTP.Grammar.Parser (isCookieValueText, isHttpQuotedText, isHttpTokenText, isOpaqueByte, isStructuredFieldBareItem, isStructuredFieldKey, isTokenChar)
import Network.HTTP.Grammar.Types
import qualified Network.HTTP.Headers.Mason as M
import qualified Wireform.Builder as B
import Wireform.Encode.Direct (dBytes, dWord8, directEncode)


sepByCommas1 :: F1.Foldable1 t => t M.Builder -> M.Builder
sepByCommas1 = F1.intercalate1 ", "
{-# INLINE sepByCommas1 #-}


-- | Encode one unsigned decimal without allocating a builder chunk.
encodeWordDecimal :: Word -> ByteString
encodeWordDecimal value = BSI.unsafeCreateUptoN 20 $ \start -> do
  end <- Prim.runB P.word64Dec (fromIntegral value) start
  pure (end `minusPtr` start)


-- | Encode two unsigned decimal components with optional fractional padding.
encodeDecimalVersion :: Int -> Word -> Word -> ByteString
encodeDecimalVersion padding major minor = BSI.unsafeCreateUptoN capacity $ \start -> do
  dot <- Prim.runB P.word64Dec (fromIntegral major) start
  poke dot (46 :: Word8)
  let fraction = dot `plusPtr` 1
  fillBytes fraction 48 zeros
  end <- Prim.runB P.word64Dec (fromIntegral minor) (fraction `plusPtr` zeros)
  pure (end `minusPtr` start)
  where
    zeros = max 0 padding
    capacity = BSI.checkedAdd "decimal version" 41 zeros


-- | Join compact values with comma-space in one buffer. Values are copied
-- verbatim, so callers supply them in their wire form.
encodeCommaSeparatedText :: NonEmpty ShortText -> ByteString
encodeCommaSeparatedText = encodeCommaSeparatedBytes toShortByteString


-- | Join complete HTTP tokens without changing their spelling or order.
encodeCommaSeparatedTokensChecked :: NonEmpty ShortText -> Maybe ByteString
encodeCommaSeparatedTokensChecked = encodeCommaSeparatedTextChecked isHttpTokenText
{-# INLINE encodeCommaSeparatedTokensChecked #-}


-- | Check and measure each compact value in the same pass.
encodeCommaSeparatedTextChecked :: (ShortText -> Bool) -> NonEmpty ShortText -> Maybe ByteString
encodeCommaSeparatedTextChecked valid = encodeCommaSeparatedChecked bound copyText . NE.toList
  where
    bound text
      | valid text = SBS.length (toShortByteString text)
      | otherwise = -1
{-# INLINE encodeCommaSeparatedTextChecked #-}


-- | Write a comma-separated list into one buffer after sizing and
-- validating every entry. A negative entry bound indicates failure.
encodeCommaSeparatedChecked
  :: (a -> Int)
  -> (Ptr Word8 -> Int -> a -> IO Int)
  -> [a]
  -> Maybe ByteString
encodeCommaSeparatedChecked _ _ [] = Just ""
encodeCommaSeparatedChecked bound write (first : rest) = sizeEntries 0 first rest
  where
    add = BSI.checkedAdd "HTTP comma-separated field"
    sizeEntries !size entry remaining
      | entrySize < 0 = Nothing
      | otherwise =
          let capacity = add size entrySize
          in case remaining of
               [] -> Just $ BSI.unsafeCreateUptoN capacity $ \ptr -> write ptr 0 first >>= writeRest ptr rest
               next : more -> sizeEntries (add capacity 2) next more
      where
        entrySize = bound entry
    writeRest _ [] !offset = pure offset
    writeRest ptr (entry : more) !offset = do
      comma <- dWord8 ptr offset 44
      space <- dWord8 ptr comma 32
      end <- write ptr space entry
      writeRest ptr more end
{-# INLINE encodeCommaSeparatedChecked #-}


encodeCommaSeparatedBytes :: (a -> SBS.ShortByteString) -> NonEmpty a -> ByteString
encodeCommaSeparatedBytes bytes (first :| rest) = directEncode size $ \ptr start ->
  copyShortBytes ptr start (bytes first) >>= writeRest ptr rest
  where
    size = foldl' addSize (SBS.length (bytes first)) rest
    addSize total value =
      BSI.checkedAdd "HTTP comma-separated field" (BSI.checkedAdd "HTTP comma-separated field" total 2)
        (SBS.length (bytes value))
    writeRest _ [] !offset = pure offset
    writeRest ptr (value : more) !offset = do
      comma <- dWord8 ptr offset 44
      space <- dWord8 ptr comma 32
      end <- copyShortBytes ptr space (bytes value)
      writeRest ptr more end
{-# INLINE encodeCommaSeparatedBytes #-}


-- | Write comma-separated names and optional values in their original
-- spelling, including the Latin-1 octets of quoted values.
encodeCommaSeparatedPairs
  :: SBS.ShortByteString
  -> (a -> ShortText)
  -> (a -> Maybe ShortText)
  -> [a]
  -> ByteString
encodeCommaSeparatedPairs = encodeCommaSeparatedPairsWith writeValue
  where
    writeValue ptr start text =
      let bytes = toShortByteString text
      in if not (SBS.null bytes) && SBSI.unsafeIndex bytes 0 == 34 && SBS.any (>= 128) bytes
           then dBytes ptr start (M.toStrictByteString (renderNonAscii text))
           else copyShortBytes ptr start bytes
{-# INLINE encodeCommaSeparatedPairs #-}


-- | Copy comma-separated tokens and their optional token suffixes verbatim.
encodeCommaSeparatedTokenPairs
  :: SBS.ShortByteString
  -> (a -> ShortText)
  -> (a -> Maybe ShortText)
  -> [a]
  -> ByteString
encodeCommaSeparatedTokenPairs = encodeCommaSeparatedPairsWith copyText
{-# INLINE encodeCommaSeparatedTokenPairs #-}


encodeCommaSeparatedPairsWith
  :: (Ptr Word8 -> Int -> ShortText -> IO Int)
  -> SBS.ShortByteString
  -> (a -> ShortText)
  -> (a -> Maybe ShortText)
  -> [a]
  -> ByteString
encodeCommaSeparatedPairsWith writeValue separator name value = \case
  [] -> BS.empty
  entries@(first : rest) ->
    let bound = foldl' (\n entry -> addEntry (add n 2) entry) (addEntry 0 first) rest
    in BSI.unsafeCreateUptoN bound $ \ptr -> writeCommaSeparated ptr (writeEntry ptr) entries
  where
    add = BSI.checkedAdd "HTTP comma-separated pairs"
    addText n = add n . SBS.length . toShortByteString
    addEntry n entry =
      let named = addText n (name entry)
      in case value entry of
           Nothing -> named
           Just text -> addText (add named (SBS.length separator)) text
    writeEntry ptr offset entry = do
      end <- copyText ptr offset (name entry)
      case value entry of
        Nothing -> pure end
        Just text -> do
          start <- copyShortBytes ptr end separator
          writeValue ptr start text
{-# INLINE encodeCommaSeparatedPairsWith #-}


-- | Copy compact UTF-8 bytes into the caller's buffer and return the end offset.
copyText :: Ptr Word8 -> Int -> ShortText -> IO Int
copyText ptr offset = copyShortBytes ptr offset . toShortByteString
{-# INLINE copyText #-}


-- | Check and encode a single RFC 6265 cookie pair.
encodeCookiePairChecked :: ShortText -> ShortText -> Maybe ByteString
encodeCookiePairChecked name value
  | isHttpTokenText name, isCookieValueText value = Just $ directEncode size $ \ptr start ->
      writeCookiePair ptr start name value
  | otherwise = Nothing
  where
    add = BSI.checkedAdd "cookie pair"
    width = SBS.length . toShortByteString
    size = add (add (width name) 1) (width value)


-- | Write a validated cookie pair into the caller's buffer.
writeCookiePair :: Ptr Word8 -> Int -> ShortText -> ShortText -> IO Int
writeCookiePair ptr offset name value = do
  nameEnd <- copyText ptr offset name
  valueStart <- dWord8 ptr nameEnd 61
  copyText ptr valueStart value
{-# INLINE writeCookiePair #-}


copyShortBytes :: Ptr Word8 -> Int -> SBS.ShortByteString -> IO Int
copyShortBytes ptr offset bytes = do
  let len = SBS.length bytes
  SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) len
  pure $! offset + len
{-# INLINE copyShortBytes #-}


shortText :: ShortText -> M.Builder
shortText = M.shortByteString . toShortByteString
{-# INLINE shortText #-}


-- | Display opaque octets as UTF-8 when valid, with a Latin-1 fallback.
opaqueShortText :: SBS.ShortByteString -> ShortText
opaqueShortText bytes =
  maybe (TS.fromText (TE.decodeLatin1 (SBS.fromShort bytes))) id (TS.fromShortByteString bytes)


-- | Render characters in the Latin-1 range as their corresponding octets.
-- Higher code points supplied by a caller use UTF-8.
renderLatin1ShortText :: ShortText -> M.Builder
renderLatin1ShortText text
  | SBS.any (>= 128) bytes = renderNonAscii text
  | otherwise = M.shortByteString bytes
  where
    bytes = toShortByteString text
{-# INLINE renderLatin1ShortText #-}


renderNonAscii :: ShortText -> M.Builder
renderNonAscii = Text.foldr (\c rest -> renderChar c <> rest) mempty . TS.toText
  where
    renderChar c
      | c <= '\xff' = M.char8 c
      | otherwise = B.charUtf8 c
{-# NOINLINE renderNonAscii #-}


-- | Render the original spelling of a token or quoted word.
renderHttpWordValue :: ShortText -> M.Builder
renderHttpWordValue value
  | not (SBS.null bytes) && SBS.index bytes 0 == 34 = renderLatin1ShortText value
  | otherwise = M.shortByteString bytes
  where
    bytes = toShortByteString value
{-# INLINE renderHttpWordValue #-}


{- | Canonicalize an opaque comment by quoting its literal parentheses and
backslashes. Latin-1 characters preserve the corresponding wire bytes.
-}
renderComment :: Comment -> M.Builder
renderComment (Comment content) =
  M.char7 '(' <> renderOpaqueText (\c -> c == '(' || c == ')') content <> M.char7 ')'


-- | Render a quoted string whose Latin-1 characters represent wire bytes.
renderHttpQuotedString :: Text.Text -> M.Builder
renderHttpQuotedString content = M.char7 '"' <> renderOpaqueText (== '"') content <> M.char7 '"'


-- | Render a compact HTTP quoted string into a single builder buffer.
renderHttpQuotedShortText :: ShortText -> M.Builder
renderHttpQuotedShortText value = B.primBounded (Prim.boundedPrim bound write) bytes
  where
    bytes = toShortByteString value
    len = SBS.length bytes
    add = BSI.checkedAdd "HTTP quoted-string"
    bound = add 2 (add len len)
    write input ptr =
      (ptr `plusPtr`) <$> foldHttpQuotedShortBytes (dWord8 ptr) 0 input


-- | Encode validated quoted-string octets, quoting only DQUOTE and backslash.
encodeHttpQuotedBytes :: SBS.ShortByteString -> ByteString
encodeHttpQuotedBytes bytes =
  encodeHttpQuotedBytesWithEscapes bytes (SBS.foldl' (\total byte -> total + fromEnum (needsQuotedEscape byte)) 0 bytes)


-- | Reject invalid HTTP quoted-string octets before allocating the field.
encodeHttpQuotedBytesChecked :: SBS.ShortByteString -> Maybe ByteString
encodeHttpQuotedBytesChecked bytes = countEscapes 0 0
  where
    size = SBS.length bytes
    countEscapes !index !escapes
      | index == size = Just (encodeHttpQuotedBytesWithEscapes bytes escapes)
      | isOpaqueByte (fromIntegral byte) = countEscapes (index + 1) (escapes + fromEnum (needsQuotedEscape byte))
      | otherwise = Nothing
      where
        byte = SBSI.unsafeIndex bytes index


encodeHttpQuotedBytesWithEscapes :: SBS.ShortByteString -> Int -> ByteString
encodeHttpQuotedBytesWithEscapes bytes escapes =
  directEncode size $ \ptr start -> writeQuotedBytes ptr start bytes (escapes /= 0)
  where
    size = BSI.checkedAdd "HTTP quoted-string" 2 (BSI.checkedAdd "HTTP quoted-string" (SBS.length bytes) escapes)


-- | Write validated HTTP quoted-string octets. Allow space for two
-- delimiters and up to twice the size of the body.
writeHttpQuotedBytes :: Ptr Word8 -> Int -> SBS.ShortByteString -> IO Int
writeHttpQuotedBytes ptr start bytes = writeQuotedBytes ptr start bytes (SBS.any needsQuotedEscape bytes)
{-# INLINE writeHttpQuotedBytes #-}


writeQuotedBytes :: Ptr Word8 -> Int -> SBS.ShortByteString -> Bool -> IO Int
writeQuotedBytes ptr start bytes hasEscapes = do
  body <- dWord8 ptr start 34
  end <-
    if hasEscapes
      then
        let copy !source !dest
              | source == lengthBytes = pure dest
              | otherwise = do
                  let byte = SBSI.unsafeIndex bytes source
                  offset <- if needsQuotedEscape byte then dWord8 ptr dest 92 else pure dest
                  next <- dWord8 ptr offset byte
                  copy (source + 1) next
        in copy 0 body
      else copyShortBytes ptr body bytes
  dWord8 ptr end 34
  where
    lengthBytes = SBS.length bytes
{-# INLINE writeQuotedBytes #-}


needsQuotedEscape :: Word8 -> Bool
needsQuotedEscape byte = byte == 34 || byte == 92
{-# INLINE needsQuotedEscape #-}


{- | Traverse a quoted string encoded as compact UTF-8. Latin-1 code points
map to the original wire octets. Higher authored code points keep UTF-8.
-}
foldHttpQuotedShortBytes :: Monad m => (Int -> Word8 -> m Int) -> Int -> SBS.ShortByteString -> m Int
foldHttpQuotedShortBytes put start bytes =
  foldHttpQuotedBytes put start (SBS.length bytes) (SBSI.unsafeIndex bytes)
{-# INLINE foldHttpQuotedShortBytes #-}


-- | Write a decoded HTTP quoted string from its UTF-8 text slice.
foldHttpQuotedText :: Monad m => (Int -> Word8 -> m Int) -> Int -> Text.Text -> m Int
foldHttpQuotedText put start (TextInternal.Text bytes offset len) =
  foldHttpQuotedBytes put start len (\i -> TextArray.unsafeIndex bytes (offset + i))
{-# INLINE foldHttpQuotedText #-}


foldHttpQuotedBytes :: Monad m => (Int -> Word8 -> m Int) -> Int -> Int -> (Int -> Word8) -> m Int
foldHttpQuotedBytes = foldOpaqueBytes 34 34 (== 34)
{-# INLINE foldHttpQuotedBytes #-}


foldOpaqueBytes :: Monad m => Word8 -> Word8 -> (Word8 -> Bool) -> (Int -> Word8 -> m Int) -> Int -> Int -> (Int -> Word8) -> m Int
foldOpaqueBytes open close delimiter put start len byteAt = do
  opened <- put start open
  closed <- go 0 opened
  put closed close
  where
    go !source !offset
      | source == len = pure offset
      | otherwise = case byteAt source of
          byte
            | delimiter byte || byte == 92 ->
                put offset 92 >>= \escaped -> put escaped byte >>= go next
            | (byte < 32 && byte /= 9) || byte == 127 ->
                put offset 63 >>= go next
            | (byte == 0xc2 || byte == 0xc3) && next < len
            , let continuation = byteAt next
            , continuation >= 0x80 && continuation <= 0xbf ->
                put offset (continuation + (byte - 0xc2) * 64) >>= go (next + 1)
            | otherwise -> put offset byte >>= go next
      where
        next = source + 1
{-# INLINE foldOpaqueBytes #-}


{- | Encode comma-separated entries, each with zero or more HTTP
@;name=value@ parameters. The projections supply the entry name, its
parameters, and each parameter's name and decoded value.
-}
encodeHttpParameterizedList
  :: (entry -> ShortText)
  -> (entry -> [param])
  -> (param -> ShortText)
  -> (param -> ShortText)
  -> [entry]
  -> ByteString
encodeHttpParameterizedList name params key value = \case
  [] -> BS.empty
  entries -> BSI.unsafeCreateUptoN (parameterizedListBound name params key value entries) $ \ptr ->
    writeParameterizedList name params key value entries ptr
{-# INLINE encodeHttpParameterizedList #-}


-- | Check each entry name and decoded parameter while measuring the output.
encodeHttpParameterizedListChecked
  :: (ShortText -> Bool)
  -> (entry -> ShortText)
  -> (entry -> [param])
  -> (param -> ShortText)
  -> (param -> ShortText)
  -> [entry]
  -> Maybe ByteString
encodeHttpParameterizedListChecked validName name params key value =
  encodeCommaSeparatedChecked bound (writeParameterizedEntry name params key value)
  where
    bound entry
      | validName (name entry) = sizeParams (width (name entry)) (params entry)
      | otherwise = -1
    sizeParams !size [] = size
    sizeParams !size (param : rest)
      | isHttpTokenText (key param), isHttpQuotedText (value param) =
          let len = width (value param)
          in sizeParams (add (add (add size 4) (width (key param))) (add len len)) rest
      | otherwise = -1
    width = SBS.length . toShortByteString
    add = BSI.checkedAdd "HTTP parameterized list"
{-# INLINE encodeHttpParameterizedListChecked #-}


-- | Builder variant of 'encodeHttpParameterizedList'.
renderHttpParameterizedList
  :: (entry -> ShortText)
  -> (entry -> [param])
  -> (param -> ShortText)
  -> (param -> ShortText)
  -> [entry]
  -> M.Builder
renderHttpParameterizedList name params key value = \case
  [] -> mempty
  entries ->
    let bound = parameterizedListBound name params key value entries
        write values ptr = (ptr `plusPtr`) <$> writeParameterizedList name params key value values ptr
    in B.primBounded (Prim.boundedPrim bound write) entries
{-# INLINE renderHttpParameterizedList #-}


-- | Encode a comma-separated list of HTTP tokens and optional values.
encodeHttpParameterList :: (a -> ShortText) -> (a -> Maybe ShortText) -> [a] -> ByteString
encodeHttpParameterList name value = \case
  [] -> BS.empty
  entries -> BSI.unsafeCreateUptoN (flatParameterListBound name value entries) $
    writeFlatParameterList name value entries
{-# INLINE encodeHttpParameterList #-}


-- | Builder variant of 'encodeHttpParameterList'.
renderHttpParameterList :: (a -> ShortText) -> (a -> Maybe ShortText) -> [a] -> M.Builder
renderHttpParameterList name value = \case
  [] -> mempty
  entries ->
    let bound = flatParameterListBound name value entries
        write values ptr = (ptr `plusPtr`) <$> writeFlatParameterList name value values ptr
    in B.primBounded (Prim.boundedPrim bound write) entries
{-# INLINE renderHttpParameterList #-}


flatParameterListBound :: (a -> ShortText) -> (a -> Maybe ShortText) -> [a] -> Int
flatParameterListBound name value = \case
  [] -> 0
  first : rest -> foldl' (\n -> addEntry (add n 2)) (addEntry 0 first) rest
  where
    add = BSI.checkedAdd "HTTP parameter list"
    addEntry n entry =
      let named = add n (SBS.length (toShortByteString (name entry)))
      in case value entry of
           Nothing -> named
           Just text ->
             let len = SBS.length (toShortByteString text)
             in if len <= (maxBound - 3) `quot` 2
                  then add named (3 + len + len)
                  else add (add named 3) (add len len)
{-# INLINE flatParameterListBound #-}


writeFlatParameterList :: (a -> ShortText) -> (a -> Maybe ShortText) -> [a] -> Ptr Word8 -> IO Int
writeFlatParameterList name value entries ptr = writeCommaSeparated ptr writeEntry entries
  where
    writeEntry offset entry = do
      end <- copyText ptr offset (name entry)
      case value entry of
        Nothing -> pure end
        Just text -> dWord8 ptr end 61 >>= \equals -> writeParameterValue ptr equals text
{-# INLINE writeFlatParameterList #-}


writeCommaSeparated :: Ptr Word8 -> (Int -> a -> IO Int) -> [a] -> IO Int
writeCommaSeparated ptr writeEntry = \case
  [] -> pure 0
  first : rest -> writeEntry 0 first >>= writeRest rest
  where
    writeRest [] !offset = pure offset
    writeRest (entry : more) !offset = do
      comma <- dWord8 ptr offset 44
      space <- dWord8 ptr comma 32
      end <- writeEntry space entry
      writeRest more end
{-# INLINE writeCommaSeparated #-}


-- Four punctuation bytes per parameter cover ;="" and each value byte
-- can produce at most two bytes when escaped.
parameterizedListBound
  :: (entry -> ShortText)
  -> (entry -> [param])
  -> (param -> ShortText)
  -> (param -> ShortText)
  -> [entry]
  -> Int
parameterizedListBound name params key value = \case
  [] -> 0
  first : rest -> foldl' addSeparatedEntry (addEntry 0 first) rest
  where
    add = BSI.checkedAdd "HTTP parameterized list"
    size = SBS.length . toShortByteString
    addSeparatedEntry n = addEntry (add n 2)
    addEntry n entry = foldl' addParam (add n (size (name entry))) (params entry)
    addParam n param =
      let len = size (value param)
      in add (add (add n 4) (size (key param))) (add len len)
{-# INLINE parameterizedListBound #-}


writeParameterizedList
  :: (entry -> ShortText)
  -> (entry -> [param])
  -> (param -> ShortText)
  -> (param -> ShortText)
  -> [entry]
  -> Ptr Word8
  -> IO Int
writeParameterizedList name params key value entries ptr =
  writeCommaSeparated ptr (writeParameterizedEntry name params key value ptr) entries
{-# INLINE writeParameterizedList #-}


writeParameterizedEntry
  :: (entry -> ShortText)
  -> (entry -> [param])
  -> (param -> ShortText)
  -> (param -> ShortText)
  -> Ptr Word8
  -> Int
  -> entry
  -> IO Int
writeParameterizedEntry name params key value ptr start entry =
  copyText ptr start (name entry) >>= writeParams (params entry)
  where
    writeParams [] !offset = pure offset
    writeParams (param : rest) !offset = do
      semicolon <- dWord8 ptr offset 59
      nameEnd <- copyText ptr semicolon (key param)
      equals <- dWord8 ptr nameEnd 61
      end <- writeParameterValue ptr equals (value param)
      writeParams rest end
{-# INLINE writeParameterizedEntry #-}


-- | Write a validated decoded parameter value as a token or quoted string.
-- The caller reserves two delimiters and up to twice the compact byte length.
writeParameterValue :: Ptr Word8 -> Int -> ShortText -> IO Int
writeParameterValue ptr offset value = case SBS.length bytes of
  0 -> quoted
  1
    | let first = SBSI.unsafeIndex bytes 0
    , tokenByte first ->
        dWord8 ptr offset first
    | otherwise -> quoted
  2
    | let first = SBSI.unsafeIndex bytes 0
    , let second = SBSI.unsafeIndex bytes 1
    , tokenByte first && tokenByte second -> do
        next <- dWord8 ptr offset first
        dWord8 ptr next second
    | otherwise -> quoted
  _
    | SBS.all tokenByte bytes -> copyShortBytes ptr offset bytes
    | otherwise -> quoted
  where
    bytes = toShortByteString value
    tokenByte = isTokenChar . BSI.w2c
    quoted = foldHttpQuotedShortBytes (dWord8 ptr) offset bytes
{-# INLINE writeParameterValue #-}


renderOpaqueText :: (Char -> Bool) -> Text.Text -> M.Builder
renderOpaqueText delim content = encoded
  where
    encoded
      | Text.any needsEncoding content = body content
      | otherwise = M.textUtf8 content
    body input =
      let (plain, rest) = Text.break needsEncoding input
      in M.textUtf8 plain <> case Text.uncons rest of
           Nothing -> mempty
           Just (c, more) -> special c <> body more
    needsEncoding c = isDelimiter c || isControl c || c > '\x7f'
    isDelimiter c = delim c || c == '\\'
    isControl c = (c < ' ' && c /= '\t') || c == '\x7f'
    special c
      | isDelimiter c = M.char7 '\\' <> M.char7 c
      | isControl c = M.char7 '?'
      | c <= '\xff' = M.char8 c
      | otherwise = B.charUtf8 c
{-# INLINE renderOpaqueText #-}


renderProductSequence :: (product -> M.Builder) -> product -> [Either Comment product] -> M.Builder
renderProductSequence renderProduct first rest =
  renderProduct first <> foldMap (\element -> M.char7 ' ' <> either renderComment renderProduct element) rest
{-# INLINE renderProductSequence #-}


-- | Check product tokens and comment octets before encoding one field.
encodeProductSequenceChecked
  :: (product -> ShortText)
  -> (product -> Maybe ShortText)
  -> product
  -> [Either Comment product]
  -> Maybe ByteString
encodeProductSequenceChecked name version first items
  | firstSize < 0 = Nothing
  | [] <- items, Nothing <- version first = Just (TS.toByteString (name first))
  | otherwise = measure firstSize items
  where
    add = BSI.checkedAdd "HTTP product sequence"
    width = SBS.length . toShortByteString
    firstSize = productBound first
    productBound item
      | not (isHttpTokenText (name item)) = -1
      | otherwise = case version item of
          Nothing -> width (name item)
          Just token
            | isHttpTokenText token -> add (add (width (name item)) 1) (width token)
            | otherwise -> -1
    commentBound (Comment (TextInternal.Text bytes offset len))
      | valid 0 = add 2 (add len len)
      | otherwise = -1
      where
        valid !i = i == len || (isOpaqueByte (fromIntegral (TextArray.unsafeIndex bytes (offset + i))) && valid (i + 1))
    measure !size [] = Just $ BSI.unsafeCreateUptoN size $ \ptr ->
      writeProduct ptr 0 first >>= writeRest ptr items
    measure !size (entry : rest)
      | entrySize >= 0 = measure (add (add size 1) entrySize) rest
      | otherwise = Nothing
      where
        entrySize = either commentBound productBound entry
    writeProduct ptr start item = do
      end <- copyText ptr start (name item)
      case version item of
        Nothing -> pure end
        Just token -> dWord8 ptr end 47 >>= \offset -> copyText ptr offset token
    writeComment ptr start (Comment (TextInternal.Text bytes offset len)) =
      foldOpaqueBytes 40 41 (\b -> b == 40 || b == 41)
        (dWord8 ptr) start len (\i -> TextArray.unsafeIndex bytes (offset + i))
    writeRest _ [] !offset = pure offset
    writeRest ptr (entry : rest) !offset = do
      start <- dWord8 ptr offset 32
      end <- either (writeComment ptr start) (writeProduct ptr start) entry
      writeRest ptr rest end
{-# INLINE encodeProductSequenceChecked #-}


renderProductToken :: ShortText -> Maybe ShortText -> M.Builder
renderProductToken name version = shortText name <> maybe mempty (\v -> M.char7 '/' <> shortText v) version
{-# INLINE renderProductToken #-}


rfc8941Integer :: Int -> M.Builder
rfc8941Integer = M.intDec
{-# INLINE rfc8941Integer #-}


rfc8941Boolean :: Bool -> M.Builder
rfc8941Boolean True = "?1"
rfc8941Boolean False = "?0"
{-# INLINE rfc8941Boolean #-}


rfc8941Binary :: ByteString -> M.Builder
rfc8941Binary bs = M.char7 ':' <> M.byteString (Base64.encode bs) <> M.char7 ':'
{-# INLINE rfc8941Binary #-}


rfc8941String :: RFC8941String -> M.Builder
rfc8941String (RFC8941String text)
  | SBS.all (not . special) bytes =
      B.primBounded (Prim.boundedPrim size writePlain) bytes
  | otherwise = M.char7 '"' <> escape (TS.toByteString text) <> M.char7 '"'
  where
    bytes = toShortByteString text
    size = BSI.checkedAdd "Structured Field String" (SBS.length bytes) 2
    special byte = byte == 34 || byte == 92 || byte >= 128
    writePlain input ptr = do
      start <- dWord8 ptr 0 34
      end <- copyShortBytes ptr start input
      (ptr `plusPtr`) <$> dWord8 ptr end 34

    escape input =
      let (plain, rest) = BS.break special input
      in M.byteString plain <> case BS.uncons rest of
           Nothing -> mempty
           Just (byte, more)
             | byte < 128 -> M.char7 '\\' <> M.word8 byte <> escape more
             -- Preserve the old rendering for an unchecked non-ASCII payload.
             | otherwise -> TS.foldl' escapeChar mempty (TSU.fromByteStringUnsafe rest)
    escapeChar builder c = case c of
      '"' -> builder <> M.char7 '\\' <> M.char7 '"'
      '\\' -> builder <> M.char7 '\\' <> M.char7 '\\'
      _ -> builder <> M.char7 c
{-# INLINE rfc8941String #-}


rfc8941Token :: RFC8941Token -> M.Builder
rfc8941Token = shortText . unsafeToRFC8941Token
{-# INLINE rfc8941Token #-}


rfc8941Decimal :: Milli -> M.Builder
rfc8941Decimal (MkFixed i) =
  let (whole, fraction) = abs i `quotRem` 1000
      (prefix, ones) = (fromInteger fraction :: Int) `quotRem` 10
      (hundreds, tens) = prefix `quotRem` 10
  in (if i < 0 then M.char7 '-' else mempty)
       <> M.integerDec whole
       <> M.char7 '.'
       <> digit hundreds
       <> (if tens /= 0 || ones /= 0 then digit tens else mempty)
       <> (if ones /= 0 then digit ones else mempty)
  where
    digit d = M.char7 (chr (ord '0' + d))
{-# INLINE rfc8941Decimal #-}


rfc8941ItemValue :: ItemValue -> M.Builder
rfc8941ItemValue = \case
  Integer i -> rfc8941Integer i
  Decimal d -> rfc8941Decimal d
  Boolean b -> rfc8941Boolean b
  Binary bs -> rfc8941Binary bs
  String t -> rfc8941String t
  Token t -> rfc8941Token t
  DateValue seconds -> M.char7 '@' <> rfc8941Integer seconds
  DisplayString value -> renderStructuredDisplayString value
{-# INLINE rfc8941ItemValue #-}


renderStructuredDisplayString :: Text.Text -> M.Builder
renderStructuredDisplayString value =
  "%\"" <> encode (TE.encodeUtf8 value) <> M.char7 '"'
  where
    encode input =
      let (plain, rest) = BS.break needsEscape input
      in M.byteString plain <> case BS.uncons rest of
           Nothing -> mempty
           Just (byte, more) ->
             M.char7 '%' <> hex (byte `shiftR` 4) <> hex (byte .&. 15) <> encode more
    needsEscape byte = byte < 32 || byte >= 127 || byte == 34 || byte == 37
    hex n = M.word8 (if n < 10 then n + 48 else n + 87)


-- | Render the normalized members of a Structured Fields dictionary.
renderStructuredFieldDictionary :: [StructuredFieldDictionaryEntry] -> M.Builder
renderStructuredFieldDictionary = renderStructuredFieldDictionaryWith renderStructuredFieldParameters


-- | Normalize keys and parameter maps and validate the surviving members.
renderStructuredFieldDictionaryChecked :: [StructuredFieldDictionaryEntry] -> Either String M.Builder
renderStructuredFieldDictionaryChecked entries
  | all validEntry normalized = Right (renderStructuredFieldDictionaryWith renderCanonicalParameters normalized)
  | otherwise = Left "Invalid Structured Fields dictionary key, item, or parameter"
  where
    normalized = orderedFieldEntries structuredDictionaryKey entries
    validEntry (StructuredFieldDictionaryEntry key member) = isStructuredFieldKey key && validStructuredFieldMember member


renderStructuredFieldDictionaryWith :: ([StructuredFieldParameter] -> M.Builder) -> [StructuredFieldDictionaryEntry] -> M.Builder
renderStructuredFieldDictionaryWith renderParameters = renderSeparated ", " renderEntry
  where
    renderEntry (StructuredFieldDictionaryEntry key member) =
      shortText key <> case member of
        StructuredItem (StructuredFieldItem (Boolean True) params) -> renderParameters params
        _ -> M.char7 '=' <> renderStructuredFieldMemberWith renderParameters member
{-# INLINE renderStructuredFieldDictionaryWith #-}


{- | Render a typed dictionary whose values require an explicit @=@. Validate
the keys and parameters after normalizing duplicate entries. The value
renderer checks the constraints of the particular field.
-}
renderStructuredFieldItemDictionaryChecked
  :: (entry -> ShortText)
  -> (entry -> [(ShortText, Maybe ItemValue)])
  -> (entry -> Either String M.Builder)
  -> [entry]
  -> Either String M.Builder
renderStructuredFieldItemDictionaryChecked entryKey entryParameters renderValue =
  renderEntries . orderedFieldEntries entryKey
  where
    renderEntries [] = Right mempty
    renderEntries (entry : rest) = do
      member <- renderEntry entry
      case rest of
        [] -> Right member
        _ -> (\suffix -> member <> ", " <> suffix) <$> renderEntries rest

    renderEntry entry
      | not (isStructuredFieldKey key) = Left "Invalid Structured Fields dictionary key"
      | not (all validParameter parameters) = Left "Invalid Structured Fields dictionary parameter"
      | otherwise = do
          value <- renderValue entry
          pure (shortText key <> M.char7 '=' <> value <> foldMap renderParameter parameters)
      where
        key = entryKey entry
        parameters = orderedFieldEntries fst (entryParameters entry)

    validParameter (key, value) =
      isStructuredFieldKey key && maybe True isStructuredFieldBareItem value

    renderParameter (key, value) =
      renderStructuredFieldParameter key (maybe (Boolean True) id value)
{-# INLINE renderStructuredFieldItemDictionaryChecked #-}


-- | Render the members of a Structured Fields List in order.
renderStructuredFieldList :: [StructuredFieldMember] -> M.Builder
renderStructuredFieldList = renderSeparated ", " renderStructuredFieldMember


-- | Validate authored list members and normalize each parameter dictionary.
renderStructuredFieldListChecked :: [StructuredFieldMember] -> Either String M.Builder
renderStructuredFieldListChecked members
  | all validStructuredFieldMember members = Right (renderSeparated ", " (renderStructuredFieldMemberWith renderCanonicalParameters) members)
  | otherwise = Left "Invalid Structured Fields list item or parameter"


renderCanonicalParameters :: [StructuredFieldParameter] -> M.Builder
renderCanonicalParameters = renderStructuredFieldParameters . orderedFieldEntries structuredParameterKey
{-# INLINE renderCanonicalParameters #-}


validStructuredFieldMember :: StructuredFieldMember -> Bool
validStructuredFieldMember = validMember
  where
    validMember (StructuredItem item) = validItem item
    validMember (StructuredInnerList items params) = all validItem items && validParameters params

    validItem (StructuredFieldItem value params) = isStructuredFieldBareItem value && validParameters params

    validParameters = all validParameter . orderedFieldEntries structuredParameterKey
    validParameter (StructuredFieldParameter key value) = isStructuredFieldKey key && isStructuredFieldBareItem value
{-# INLINE validStructuredFieldMember #-}


renderStructuredFieldMember :: StructuredFieldMember -> M.Builder
renderStructuredFieldMember = renderStructuredFieldMemberWith renderStructuredFieldParameters


renderStructuredFieldMemberWith :: ([StructuredFieldParameter] -> M.Builder) -> StructuredFieldMember -> M.Builder
renderStructuredFieldMemberWith renderParameters member = case member of
  StructuredItem item -> renderItem item
  StructuredInnerList items params ->
    M.char7 '(' <> renderSeparated " " renderItem items <> M.char7 ')' <> renderParameters params
  where
    renderItem (StructuredFieldItem value params) = rfc8941ItemValue value <> renderParameters params
{-# INLINE renderStructuredFieldMemberWith #-}


renderSeparated :: M.Builder -> (a -> M.Builder) -> [a] -> M.Builder
renderSeparated _ _ [] = mempty
renderSeparated separator render (first : rest) = render first <> foldMap (\value -> separator <> render value) rest
{-# INLINE renderSeparated #-}


renderStructuredFieldParameters :: [StructuredFieldParameter] -> M.Builder
renderStructuredFieldParameters = foldMap $ \(StructuredFieldParameter key value) -> renderStructuredFieldParameter key value


renderStructuredFieldParameter :: ShortText -> ItemValue -> M.Builder
renderStructuredFieldParameter key value =
  M.char7 ';' <> shortText key <> case value of
    Boolean True -> mempty
    _ -> M.char7 '=' <> rfc8941ItemValue value
{-# INLINE renderStructuredFieldParameter #-}


data EmptyParameterRenderMode = IncludeIfEmpty | ExcludeIfEmpty


rfc8941Parameter :: EmptyParameterRenderMode -> (a -> M.Builder) -> ShortText -> Maybe a -> M.Builder
rfc8941Parameter IncludeIfEmpty f k = \case
  Nothing -> M.char7 ';' <> shortText k
  Just v -> M.char7 ';' <> shortText k <> M.char7 '=' <> f v
rfc8941Parameter ExcludeIfEmpty f k = \case
  Nothing -> mempty
  Just v -> M.char7 ';' <> shortText k <> M.char7 '=' <> f v
{-# INLINE rfc8941Parameter #-}
