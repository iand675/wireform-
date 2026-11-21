{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

{- | The @ETag@ response header field provides the entity-tag of the selected
representation: an opaque validator, either strong or @W/@-prefixed weak, that
the origin server changes whenever the representation changes. Clients echo it
in conditional requests so caches can be revalidated and concurrent updates
detected without transferring the whole representation.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3>

See also: "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.IfRange", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.ETag (
  ETag (..),
  EntityTag (StrongETagBytes, WeakETagBytes, StrongETag, WeakETag),
  entityTagBytes,
  strongMatch,
  weakMatch,
  eTagParser,
  entityTagParser,
  conditionalEntityTagParser,
  parseConditionalEntityTagFields,
  renderEntityTag,
  encodeEntityTagChecked,
  encodeEntityTagListChecked,
  isValidOpaqueTag,
  validTagChar,
  renderETag,
  parseETag,
) where

import Data.Bits (xor, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Exts (Addr#, Int (I#), compareByteArrays#, eqAddr#, eqWord64#, indexWord8ArrayAsWord64#, indexWord8OffAddr#, int2Word#, isTrue#, leWord#, or64#, plusAddr#, sizeofByteArray#, wordToWord64#, xor64#, (-#), (==#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Word (Word64 (W64#), Word8 (W8#))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Encode.Direct (dWord8, directEncode)
import Wireform.Parser.Internal (Parser (..), pattern OK#)


newtype ETag = ETag {etag :: EntityTag}
  deriving stock (Eq, Show)


-- | A strong or weak validator carrying its original opaque octets.
data EntityTag
  = StrongETagBytes {-# UNPACK #-} !SBS.ShortByteString
  | WeakETagBytes {-# UNPACK #-} !SBS.ShortByteString
  deriving stock (Eq, Show)


-- | Construct a strong tag from UTF-8 text or match a strong tag with valid UTF-8.
pattern StrongETag :: ST.ShortText -> EntityTag
pattern StrongETag text <- StrongETagBytes (ST.fromShortByteString -> Just text)
  where
    StrongETag text = StrongETagBytes (ST.toShortByteString text)


-- | Construct a weak tag from UTF-8 text or match a weak tag with valid UTF-8.
pattern WeakETag :: ST.ShortText -> EntityTag
pattern WeakETag text <- WeakETagBytes (ST.fromShortByteString -> Just text)
  where
    WeakETag text = WeakETagBytes (ST.toShortByteString text)


-- | Return the opaque octets, independent of tag strength.
entityTagBytes :: EntityTag -> SBS.ShortByteString
entityTagBytes (StrongETagBytes bytes) = bytes
entityTagBytes (WeakETagBytes bytes) = bytes


-- | Strong comparison requires two strong tags with identical octets.
strongMatch :: EntityTag -> EntityTag -> Bool
strongMatch (StrongETagBytes a) (StrongETagBytes b) = equalTagBytes a b
strongMatch _ _ = False
{-# INLINE strongMatch #-}


-- | Weak comparison compares opaque octets regardless of strength.
weakMatch :: EntityTag -> EntityTag -> Bool
weakMatch a b = equalTagBytes (entityTagBytes a) (entityTagBytes b)


-- Two bounded loads cover equal-sized tags of eight through sixteen bytes.
-- For intermediate sizes the loads overlap, but neither extends past the end.
equalTagBytes :: SBS.ShortByteString -> SBS.ShortByteString -> Bool
equalTagBytes (SBSI.SBS a) (SBSI.SBS b) =
  isTrue# (size ==# sizeofByteArray# b) && equalBytes
  where
    size = sizeofByteArray# a
    lastWord = size -# 8#
    difference offset =
      xor64# (indexWord8ArrayAsWord64# a offset) (indexWord8ArrayAsWord64# b offset)
    equalWords offset =
      isTrue# (eqWord64# (or64# (difference 0#) (difference offset)) (wordToWord64# 0##))
    equalBytes
      | isTrue# (size ==# 16#) = equalWords 8#
      -- A negative offset wraps above seven in the unsigned comparison.
      | isTrue# (leWord# (int2Word# lastWord) 7##) = equalWords lastWord
      | isTrue# (size ==# 0#) = True
      | otherwise = isTrue# (compareByteArrays# a 0# b 0# size ==# 0#)
{-# INLINE equalTagBytes #-}


instance KnownHeader ETag where
  type ParseFailure ETag = String
  type Cardinality ETag = 'ZeroOrOne
  type Direction ETag = 'Response


  parseFromHeaders _ (value :| []) = parseETag value
  parseFromHeaders _ _ = Left "Multiple ETag field lines"


  renderToHeaders _ = M.toStrictByteString . renderETag


  renderToHeadersChecked _ = encodeEntityTagChecked . etag


  headerName _ = hETag


entityTagParser :: ParserT st e EntityTag
entityTagParser =
  ($(string "W/") *> parseTag WeakETagBytes) <|> parseTag StrongETagBytes
  where
    parseTag wrap =
      $(char '"') *> withByteString tagBody (\() body -> wrap (SBS.toShort body) <$ $(char '"'))


tagBody :: ParserT st e ()
tagBody = Parser $ \_ end start st -> (# st, OK# () (scanTagBytes# start end) #)
{-# INLINE tagBody #-}


-- | Stop at the first byte outside etagc or at the end of the input.
scanTagBytes# :: Addr# -> Addr# -> Addr#
scanTagBytes# current end
  | isTrue# (eqAddr# current end) = current
  | isTagByte (W8# (indexWord8OffAddr# current 0#)) = scanTagBytes# (plusAddr# current 1#) end
  | otherwise = current


eTagParser :: ParserT st e ETag
eTagParser = ETag <$> entityTagParser


-- | Parse the wildcard or entity-tag list used by both match preconditions.
conditionalEntityTagParser :: a -> ([EntityTag] -> a) -> ParserT st e a
conditionalEntityTagParser wildcard fromTags =
  (wildcard <$ wildcardParser) <|> (fromTags <$> httpList entityTagParser)


parseConditionalEntityTagFields :: String -> a -> ([EntityTag] -> a) -> NonEmpty B.ByteString -> Either String a
parseConditionalEntityTagFields name wildcard fromTags fields =
  case fields of
    value :| [] -> parseFieldLine name (conditionalEntityTagParser wildcard fromTags) value
    _ -> fromTags <$> parseListFieldLines name (httpList entityTagParser) fields


wildcardParser :: ParserT st e ()
wildcardParser = ows *> $(char '*') *> ows


isTagByte :: Word8 -> Bool
isTagByte byte = byte >= 0x21 && byte /= 0x22 && byte /= 0x7f
{-# INLINE isTagByte #-}


-- | Check the opaque octets of an authored entity tag before quoting it.
isValidOpaqueTag :: SBS.ShortByteString -> Bool
isValidOpaqueTag bytes@(SBSI.SBS array) = scan 0
  where
    !size = SBS.length bytes
    scan offset@(I# index)
      | size - offset >= 8 =
          validTagWord (W64# (indexWord8ArrayAsWord64# array index)) && scan (offset + 8)
      | otherwise = scanCompactBytes isTagByte bytes offset
{-# INLINE isValidOpaqueTag #-}


-- The seven-bit mask isolates each byte so the additions cannot carry
-- between lanes. A high bit marks obs-text or an allowed ASCII byte.
validTagWord :: Word64 -> Bool
validTagWord bytes = (bytes .|. (visible .&. nonQuote .&. nonDelete)) .&. high == high
  where
    high = 0x8080808080808080
    ascii = 0x7f7f7f7f7f7f7f7f
    low = bytes .&. ascii
    visible = low + 0x5f5f5f5f5f5f5f5f
    nonQuote = (low `xor` 0x2222222222222222) + ascii
    nonDelete = (low `xor` ascii) + ascii
{-# INLINE validTagWord #-}


-- | Consume a single entity-tag octet and expose its Latin-1 code point.
validTagChar :: ParserT st e Char
validTagChar = withAnyWord8 $ \byte ->
  if isTagByte byte then pure (chr (fromIntegral byte)) else failed


parseETag :: B.ByteString -> Either String ETag
parseETag input
  | size >= prefix + 2
  , BU.unsafeIndex wire prefix == 0x22
  , BU.unsafeIndex wire (size - 1) == 0x22
  , let body = BU.unsafeTake (size - prefix - 2) (BU.unsafeDrop (prefix + 1) wire)
  , validBody body =
      let bytes = SBS.toShort body
      in Right $! ETag (if weak then WeakETagBytes bytes else StrongETagBytes bytes)
  | otherwise = case runParser (ows *> eTagParser <* ows) input of
      OK tag "" -> Right tag
      OK _ rest -> Left $ "Unconsumed input after parsing ETag header: " <> show rest
      Fail -> Left "Failed to parse ETag header"
      Err e -> Left e
  where
    wire = B.dropWhileEnd isOwsByte (B.dropWhile isOwsByte input)
    size = B.length wire
    weak = size >= 2 && BU.unsafeIndex wire 0 == 0x57 && BU.unsafeIndex wire 1 == 0x2f
    prefix = if weak then 2 else 0
    isOwsByte b = b == 0x20 || b == 0x09


validBody :: B.ByteString -> Bool
validBody (BI.BS fp@(ForeignPtr start _) (I# size)) =
  BI.accursedUnutterablePerformIO $
    withForeignPtr fp $ \_ ->
      let end = plusAddr# start size
      in pure $! isTrue# (eqAddr# (scanTagBytes# start end) end)
{-# INLINE validBody #-}


renderEntityTag :: EntityTag -> M.Builder
renderEntityTag (StrongETagBytes tag) = M.char8 '"' <> M.shortByteString tag <> M.char8 '"'
renderEntityTag (WeakETagBytes tag) = "W/\"" <> M.shortByteString tag <> M.char8 '"'


-- | Validate and write a single entity tag without a builder buffer.
encodeEntityTagChecked :: EntityTag -> Maybe B.ByteString
encodeEntityTagChecked tag
  | isValidOpaqueTag (entityTagBytes tag) =
      Just (directEncode (wireTagSize tag) (\ptr offset -> writeWireTag ptr offset tag))
  | otherwise = Nothing


-- | Validate every member before allocating the ordered field list.
encodeEntityTagListChecked :: NonEmpty EntityTag -> Maybe B.ByteString
encodeEntityTagListChecked (first :| []) = encodeEntityTagChecked first
encodeEntityTagListChecked (first :| rest) = sizeTags 0 first rest
  where
    add = BI.checkedAdd "entity-tag list"
    sizeTags !size tag remaining
      | isValidOpaqueTag (entityTagBytes tag) =
          let nextSize = add size (wireTagSize tag)
          in case remaining of
               [] -> Just (directEncode nextSize (\ptr offset -> writeWireTag ptr offset first >>= writeRest ptr rest))
               next : more -> sizeTags (add nextSize 2) next more
      | otherwise = Nothing
    writeRest _ [] !offset = pure offset
    writeRest ptr (next : more) !offset = do
      comma <- dWord8 ptr offset 44
      space <- dWord8 ptr comma 32
      afterTag <- writeWireTag ptr space next
      writeRest ptr more afterTag


wireTagSize :: EntityTag -> Int
wireTagSize (StrongETagBytes bytes) = BI.checkedAdd "entity-tag" 2 (SBS.length bytes)
wireTagSize (WeakETagBytes bytes) = BI.checkedAdd "entity-tag" 4 (SBS.length bytes)
{-# INLINE wireTagSize #-}


writeWireTag :: Ptr Word8 -> Int -> EntityTag -> IO Int
writeWireTag ptr offset tag = do
  prefix <- case tag of
    StrongETagBytes _ -> pure offset
    WeakETagBytes _ -> do
      weak <- dWord8 ptr offset 87
      dWord8 ptr weak 47
  start <- dWord8 ptr prefix 34
  let bytes = entityTagBytes tag
      lengthBytes = SBS.length bytes
  SBSI.copyToPtr bytes 0 (ptr `plusPtr` start) lengthBytes
  dWord8 ptr (start + lengthBytes) 34
{-# INLINE writeWireTag #-}


renderETag :: ETag -> M.Builder
renderETag = renderEntityTag . etag
