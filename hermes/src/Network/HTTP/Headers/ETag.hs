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
  validTagChar,
  renderETag,
  parseETag,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts (Addr#, Int (I#), eqAddr#, indexWord8OffAddr#, isTrue#, plusAddr#)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Word (Word8 (W8#))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
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
strongMatch (StrongETagBytes a) (StrongETagBytes b) = a == b
strongMatch _ _ = False


-- | Weak comparison compares opaque octets regardless of strength.
weakMatch :: EntityTag -> EntityTag -> Bool
weakMatch a b = entityTagBytes a == entityTagBytes b


instance KnownHeader ETag where
  type ParseFailure ETag = String
  type Cardinality ETag = 'ZeroOrOne
  type Direction ETag = 'Response


  parseFromHeaders _ (value :| []) = parseETag value
  parseFromHeaders _ _ = Left "Multiple ETag field lines"


  renderToHeaders _ = M.toStrictByteString . renderETag


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


renderETag :: ETag -> M.Builder
renderETag = renderEntityTag . etag
