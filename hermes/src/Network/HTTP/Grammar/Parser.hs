{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Parsers for the HTTP wire-grammar substrate.

This is the RFC 9110 (HTTP Semantics) and RFC 8941 (Structured Fields)
ABNF that underlies every HTTP header — and, deliberately, any future
non-header grammar (URIs, mailboxes, …) that wants to share the same
flatparse-based 'ParserT' and the shared character classes
('tokenCharSet', 'quotedCharSet', …). It is /not/ header-specific:
nothing here knows about 'Network.HTTP.Headers.HeaderFieldName' or the
'Network.HTTP.Headers.KnownHeader' class.

The pure value types live in "Network.HTTP.Grammar.Types"; the inverse
renderers live in "Network.HTTP.Grammar.Rendering".

This module previously lived at
@"Network.HTTP.Headers.Parsing.Util"@; that module now re-exports this
one for backwards compatibility.
-}
module Network.HTTP.Grammar.Parser (
  module Network.HTTP.Grammar.Parser,
  module Network.HTTP.Grammar.Types,
  module Wireform.Parser,
  module Wireform.Parser.Position,
  module Wireform.Parser.Switch,
) where

import Control.Applicative (asum)
import Control.Monad.Combinators hiding (empty, many, optional, skipMany, skipSome, some, (<|>))
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Bits (complement, setBit, unsafeShiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (ord)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii
import Data.Fixed
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Exts (Char (C#), Int (I#), Word (W#), eqAddr#, eqWord64#, gtWord#, indexCharOffAddr#, indexWord8OffAddr#, indexWord8OffAddrAsWord64#, isTrue#, leWord64#, ltWord64#, minusAddr#, minusWord#, plusAddr#, plusWord64#, timesWord64#, word8ToWord#, wordToWord64#, (<=#))
import GHC.IO (IO (..))
import GHC.Ptr (Ptr (..))
import GHC.Word (Word64 (W64#))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..))
import Network.HTTP.Grammar.Types
import Network.HTTP.URL.Decode (urlDecodeMaybe)
import Wireform.Base64 (decodeBase64)
import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Internal (Parser (..), Pure, pattern Err#, pattern Fail#, pattern OK#)
import Wireform.Parser.Position
import Wireform.Parser.Switch (char, string, switch)


------------------------------------------------------------------------
-- Flatparse compatibility layer
------------------------------------------------------------------------

{- | Type alias matching flatparse's @ParserT st e a@.
The @st@ parameter is ignored — hermes only uses pure mode.
-}
type ParserT (st :: k) e a = Parser Pure e a


-- | flatparse-compatible result type.
data Result e a
  = OK a !ByteString
  | Fail
  | Err !e
  deriving stock (Show, Functor)


-- | Run a parser on a ByteString, returning flatparse-compatible Result.
runParser :: Parser Pure e a -> ByteString -> Result e a
runParser p bs = case parseByteString p' bs of
  Right (a, rest) -> OK a rest
  Left (ParseFail _) -> Fail
  Left (ParseErr _ e) -> Err e
  Left _ -> Fail
  where
    p' = do
      a <- p
      rest <- takeRest
      pure (a, rest)


-- | flatparse's @failed@ = wireform's 'empty'
failed :: Parser Pure e a
failed = empty
{-# INLINE failed #-}


-- | Take the rest of input as a String (flatparse compat).
takeRestString :: Parser Pure e String
takeRestString = BSC.unpack <$> takeRest
{-# INLINE takeRestString #-}


------------------------------------------------------------------------
-- embedError (wireform version)
------------------------------------------------------------------------

-- | Run the parser; if it throws Err#, handle it with the given function.
embedError :: Parser Pure e b -> (e -> Parser Pure e' b) -> Parser Pure e' b
embedError (Parser f) hdl = Parser $ \env eob s st -> case f env eob s st of
  (# st', (# (# x, a #) | | #) #) -> (# st', (# (# x, a #) | | #) #)
  (# st', (# | (# #) | #) #) -> (# st', (# | (# #) | #) #)
  (# st', (# | | (# e #) #) #) -> case hdl e of
    Parser g -> g env eob s st'
{-# INLINE embedError #-}


------------------------------------------------------------------------
-- HTTP parsing utilities
------------------------------------------------------------------------

optionalWhitespace :: ParserT st e ()
optionalWhitespace = ows
{-# INLINE optionalWhitespace #-}


isOWS :: Char -> Bool
isOWS c = c == ' ' || c == '\t'


ows :: ParserT st e ()
ows = skipMany (skipSatisfyAscii isOWS)
{-# INLINE ows #-}


rws :: ParserT st e ()
rws = skipSome (skipSatisfyAscii isOWS)
{-# INLINE rws #-}


bws :: ParserT st e ()
bws = ows
{-# INLINE bws #-}


{- | Parse a nonempty sequence of decimal digits. Apply the overflow parser
at the first digit that would exceed the bound.
-}
boundedDecimalWord :: Word -> ParserT st e Word -> ParserT st e Word
boundedDecimalWord limit overflow =
  case quotRem (fromIntegral limit) 10 of
    (prefix, digit) -> fromIntegral <$> boundedDecimalDigits prefix digit (fromIntegral <$> overflow)
{-# INLINE boundedDecimalWord #-}


-- | Decimal parser for fields that store offsets in a fixed-width word.
boundedDecimalWord64 :: ParserT st e Word64 -> ParserT st e Word64
-- Keep the quotient and remainder of Word64's maximum out of the parser.
boundedDecimalWord64 = boundedDecimalDigits 1_844_674_407_370_955_161 5
{-# INLINE boundedDecimalWord64 #-}


-- The prefix and final digit are the quotient and remainder of the bound by ten.
boundedDecimalDigits :: Word64 -> Word64 -> ParserT st e Word64 -> ParserT st e Word64
boundedDecimalDigits (W64# maxPrefix) (W64# lastDigit) (Parser overflow) = Parser $ \env end start st ->
  let finish cur acc
        | isTrue# (eqAddr# cur start) = (# st, Fail# #)
        | otherwise = (# st, OK# (W64# acc) cur #)
      step cur acc
        | isTrue# (eqAddr# cur end) = finish cur acc
        | otherwise = case indexWord8OffAddr# cur 0# of
            byte ->
              let digit# = minusWord# (word8ToWord# byte) 48##
              in if isTrue# (gtWord# digit# 9##)
                   then finish cur acc
                   else
                     let next = plusAddr# cur 1#
                         digit = wordToWord64# digit#
                     in if isTrue# (ltWord64# acc maxPrefix) || (isTrue# (eqWord64# acc maxPrefix) && isTrue# (leWord64# digit lastDigit))
                          then step next (plusWord64# (timesWord64# acc (wordToWord64# 10##)) digit)
                          else overflow env end next st
  in step start (wordToWord64# 0##)
{-# INLINE boundedDecimalDigits #-}


-- | Delta-seconds with the saturation rule of RFC 9111 section 1.2.2.
saturatingDeltaSeconds :: Word -> ParserT st e Word
saturatingDeltaSeconds limit =
  boundedDecimalWord limit (limit <$ skipMany (skipSatisfyAscii isDigit))
{-# INLINE saturatingDeltaSeconds #-}


{- | Fast whole-buffer decoding for one through nine bare decimal digits.
This range fits in Int on both 32-bit and 64-bit hosts. The fallback
handles other lengths and any whitespace allowed by the field grammar.
-}
withShortDecimal :: ByteString -> (Int -> a) -> a -> a
withShortDecimal wire success fallback
  | let size = BS.length wire
  , size > 0 && size <= 9
  , let result = BS.foldl' addDigit 0 wire
  , result >= 0 =
      success result
  | otherwise = fallback
  where
    addDigit n byte
      | let digit = byte - 48, digit <= 9, n >= 0 = n * 10 + fromIntegral digit
      | otherwise = -1
{-# INLINE withShortDecimal #-}


shortASCIIFromParser_ :: ParserT st e a -> ParserT st e ST.ShortText
shortASCIIFromParser_ (Parser p) = Parser $ \env end start state ->
  case p env end start state of
    (# nextState, OK# _ next #) ->
      -- The pure driver holds the input alive while the parser runs.
      -- Finish the owned copy here so no input slice escapes in a thunk.
      case SBS.createFromPtr (Ptr start) (I# (minusAddr# next start)) of
        IO copy -> case copy nextState of
          (# copied, bytes #) -> (# copied, OK# (STU.fromShortByteStringUnsafe bytes) next #)
    (# nextState, Fail# #) -> (# nextState, Fail# #)
    (# nextState, Err# failure #) -> (# nextState, Err# failure #)
{-# INLINE shortASCIIFromParser_ #-}


-- | RFC 6265 cookie-value, including any surrounding quotes.
cookieValueBytes :: ParserT st e ()
cookieValueBytes = branch $(char '"') (octets *> $(char '"')) octets
  where
    octets = skipMany (skipSatisfyAscii cookieOctet)
    cookieOctet c =
      '!' <= c && c <= '~' && c /= '"' && c /= ',' && c /= ';' && c /= '\\'
{-# INLINE cookieValueBytes #-}


tokenCharSet :: CharSet
tokenCharSet = alnum <> "-+.^_`|~!#$%&'*"


rfc9110Token :: ParserT st e ST.ShortText
rfc9110Token = shortASCIIFromParser_ tokenBytes
{-# INLINE rfc9110Token #-}


rfc9110TokenBS :: ParserT st e ByteString
rfc9110TokenBS = byteStringOf tokenBytes
{-# INLINE rfc9110TokenBS #-}


tokenBytes :: ParserT st e ()
tokenBytes = asciiSetBytes tokenAsciiSet
{-# INLINE tokenBytes #-}


-- | Consume a nonempty run of octets in an ASCII character set.
asciiSetBytes :: AsciiSet -> ParserT st e ()
asciiSetBytes = scanAsciiSet (const False)
{-# INLINE asciiSetBytes #-}


-- | Like 'asciiSetBytes', for sets that include all ASCII letters and digits.
asciiAlnumSetBytes :: AsciiSet -> ParserT st e ()
asciiAlnumSetBytes = scanAsciiSet asciiAlnumWord
{-# INLINE asciiAlnumSetBytes #-}


-- | Consume a nonempty run of ASCII decimal digits.
decimalDigitBytes :: ParserT st e ()
decimalDigitBytes = scanAsciiSet asciiDigitWord asciiDigits
{-# INLINE decimalDigitBytes #-}


asciiDigits :: AsciiSet
asciiDigits = asciiSet (CharSet.range '0' '9')
{-# NOINLINE asciiDigits #-}


asciiDigitWord :: Word64 -> Bool
asciiDigitWord bytes = coversAsciiLanes bytes (asciiDigitMask (bytes .&. 0x7f7f7f7f7f7f7f7f))
{-# INLINE asciiDigitWord #-}


-- | Consume the Base64url alphabet without interpreting the bytes as encoded data.
base64UrlBytes :: ParserT st e ()
base64UrlBytes = scanAsciiSet asciiBase64UrlWord base64UrlChars
{-# INLINE base64UrlBytes #-}


base64UrlChars :: AsciiSet
base64UrlChars = asciiSet (alnum <> "-_")
{-# NOINLINE base64UrlChars #-}


scanAsciiSet :: (Word64 -> Bool) -> AsciiSet -> ParserT st e ()
scanAsciiSet wholeWord chars = Parser $ \_ end start st -> case chars of
  AsciiSet low high ->
    let finish cur
          | isTrue# (eqAddr# cur start) = (# st, Fail# #)
          | otherwise = (# st, OK# () cur #)
        scan cur
          | isTrue# (8# <=# minusAddr# end cur)
          , wholeWord (W64# (indexWord8OffAddrAsWord64# cur 0#)) =
              scan (plusAddr# cur 8#)
          | isTrue# (eqAddr# cur end) = finish cur
          | otherwise = case indexCharOffAddr# cur 0# of
              byte
                | asciiMember low high (C# byte) -> scan (plusAddr# cur 1#)
                | otherwise -> finish cur
    in scan start
{-# INLINE scanAsciiSet #-}


-- The high bit biases each byte for the inclusive range checks. Each
-- subtraction stays within its own lane, independent of machine byte order.
asciiAlnumWord :: Word64 -> Bool
asciiAlnumWord bytes = coversAsciiLanes bytes (asciiAlnumMask (bytes .&. 0x7f7f7f7f7f7f7f7f))
{-# INLINE asciiAlnumWord #-}


asciiBase64UrlWord :: Word64 -> Bool
asciiBase64UrlWord bytes = coversAsciiLanes bytes (asciiAlnumMask low .|. equalMask dash .|. equalMask underscore)
  where
    low = bytes .&. 0x7f7f7f7f7f7f7f7f
    dash = 0x2d2d2d2d2d2d2d2d
    underscore = 0x5f5f5f5f5f5f5f5f
    -- Zero difference leaves the lane's high bit clear after adding 127.
    equalMask byte = complement ((low `xor` byte) + 0x7f7f7f7f7f7f7f7f)
{-# INLINE asciiBase64UrlWord #-}


coversAsciiLanes :: Word64 -> Word64 -> Bool
coversAsciiLanes bytes mask = mask .&. complement bytes .&. asciiHighBits == asciiHighBits
{-# INLINE coversAsciiLanes #-}


asciiHighBits :: Word64
asciiHighBits = 0x8080808080808080


asciiAlnumMask :: Word64 -> Word64
asciiAlnumMask low = asciiDigitMask low .|. letters
  where
    lowercase = low .|. 0x2020202020202020
    letters = ((lowercase .|. asciiHighBits) - 0x6161616161616161) .&. (0xfafafafafafafafa - lowercase)
{-# INLINE asciiAlnumMask #-}


asciiDigitMask :: Word64 -> Word64
asciiDigitMask low =
  ((low .|. asciiHighBits) - 0x3030303030303030) .&. (0xb9b9b9b9b9b9b9b9 - low)
{-# INLINE asciiDigitMask #-}


-- Two precomputed machine words keep the ASCII scan independent of the
-- Unicode character-class representation.
data AsciiSet = AsciiSet {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64


asciiSet :: CharSet -> AsciiSet
asciiSet chars = AsciiSet (mask 0) (mask 64)
  where
    mask offset = foldl' add 0 [0 .. 63]
      where
        add bits i
          | toEnum (offset + i) `CharSet.member` chars = setBit bits i
          | otherwise = bits


-- | Test membership in an ASCII character set.
asciiMember :: Word64 -> Word64 -> Char -> Bool
asciiMember low high c
  | i < 64 = low `unsafeShiftR` i .&. 1 /= 0
  | i < 128 = high `unsafeShiftR` (i - 64) .&. 1 /= 0
  | otherwise = False
  where
    i = ord c
{-# INLINE asciiMember #-}


tokenAsciiSet :: AsciiSet
tokenAsciiSet = asciiSet tokenCharSet
{-# NOINLINE tokenAsciiSet #-}


isTokenChar :: Char -> Bool
isTokenChar c = case tokenAsciiSet of
  AsciiSet low high -> asciiMember low high c
{-# INLINE isTokenChar #-}


fieldName :: ParserT st e ST.ShortText
fieldName = rfc9110Token
{-# INLINE fieldName #-}


obsTextCharSet :: CharSet
obsTextCharSet = CharSet.fromList ['\x80' .. '\xFF']


{- | RFC 9110 §5.6.4 @quoted-pair@ body — the set of bytes
permitted after a literal backslash inside a quoted-string.
@quoted-pair = \"\\\\\" ( HTAB / SP / VCHAR / obs-text )@ where
@VCHAR = %x21-7E@.

(Previously written as the string literal @\"\\t !-~\"@; the
'IsString' instance for 'CharSet' interprets that as the
five-character set @{TAB, SP, !, -, ~}@, not a range from
@!@ to @~@. The literal was a long-standing bug that made
'mkRFC8941String' reject any payload containing DQUOTE or
backslash — both of which are explicitly meant to be
escapable.)
-}
quotedPairCharSet :: CharSet
quotedPairCharSet =
  CharSet.singleton '\t'
    <> CharSet.singleton ' '
    <> CharSet.range '\x21' '\x7E'
    <> obsTextCharSet


{- | RFC 9110 §5.6.4 @qdtext@ — bytes allowed /unescaped/ inside a
quoted-string.
@qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text@.

(Previously the @%x23-5B@ leg used the range @\\x2A..\\x5B@
instead of @\\x23..\\x5B@, silently dropping @#$%&\\'()@ from
the unescaped set.)
-}
quotedCharSet :: CharSet
quotedCharSet =
  CharSet.singleton '\t'
    <> CharSet.singleton ' '
    <> CharSet.singleton '\x21'
    <> CharSet.range '\x23' '\x5B'
    <> CharSet.range '\x5D' '\x7E'
    <> obsTextCharSet


quotedString :: ParserT st e ST.ShortText
quotedString =
  between
    $(char '"')
    $(char '"')
    (ST.pack <$> many (unescapedChar <|> escapedChar))
  where
    unescapedChar = satisfyAscii (`CharSet.member` quotedCharSet)
    escapedChar = do
      $(char '\\')
      satisfyAscii (`CharSet.member` quotedPairCharSet)
{-# INLINE quotedString #-}


commentCharSet :: CharSet
commentCharSet =
  "\t "
    <> CharSet.fromList ['\x21' .. '\x27']
    <> CharSet.fromList ['\x2A' .. '\x5B']
    <> CharSet.fromList ['\x5D' .. '\x7E']
    <> obsTextCharSet


{- | Read the opaque content of an RFC 9110 comment. Nested parentheses
are retained in the content, quoted pairs are unescaped, and obs-text
bytes are mapped one-for-one to Latin-1 code points.
-}
comment :: ParserT st e Text
comment = TextEncoding.decodeLatin1 <$> ($(char '(') *> withByteString (scanOpaqueBody 41 True) decodeOpaqueBody)
{-# INLINE comment #-}


-- | Consume a validated comment without materializing its body.
skipComment :: ParserT st e ()
skipComment = () <$ ($(char '(') *> scanOpaqueBody 41 True)
{-# INLINE skipComment #-}


-- | An RFC 9110 quoted string with a one-to-one Latin-1 mapping for obs-text.
httpQuotedString :: ParserT st e Text
httpQuotedString = TextEncoding.decodeLatin1 <$> httpQuotedBytes
{-# INLINE httpQuotedString #-}


-- | A quoted string's original octets after removing quoting backslashes.
httpQuotedBytes :: ParserT st e ByteString
httpQuotedBytes = $(char '"') *> withByteString (scanOpaqueBody 34 False) decodeOpaqueBody
{-# INLINE httpQuotedBytes #-}


-- | The validated body of an HTTP quoted string, retaining its escapes.
httpQuotedWireBytes :: ParserT st e ByteString
httpQuotedWireBytes =
  $(char '"') *> withByteString (scanOpaqueBody 34 False) (\_ bytes -> pure (BS.init bytes))
{-# INLINE httpQuotedWireBytes #-}


-- | An optional equals sign and HTTP token or quoted value, with BWS.
-- Quoted spellings include their delimiters and quoting backslashes.
optionalHttpWordValue :: ParserT st e (Maybe ST.ShortText)
optionalHttpWordValue =
  branch (bws *> $(char '=') *> bws) (Just <$> httpWordValue) (pure Nothing)
{-# INLINE optionalHttpWordValue #-}


-- | An HTTP token or a quoted string in its original wire spelling.
httpWordValue :: ParserT st e ST.ShortText
httpWordValue =
  rfc9110Token <|> (shortTextFromLatin1 <$> byteStringOf ($(char '"') *> scanOpaqueBody 34 False))
{-# INLINE httpWordValue #-}


-- | Map each wire octet to its corresponding Latin-1 code point.
shortTextFromLatin1 :: ByteString -> ST.ShortText
shortTextFromLatin1 bytes
  | BS.any (>= 128) bytes = ST.fromText (TextEncoding.decodeLatin1 bytes)
  | otherwise = STU.fromByteStringUnsafe bytes
{-# INLINE shortTextFromLatin1 #-}


decodeOpaqueBody :: Bool -> ByteString -> ParserT st e ByteString
decodeOpaqueBody hasEscapes bytes =
  let body = BS.init bytes
  in pure (if hasEscapes then unquotePairs body else body)


-- The scanner includes the closing delimiter. Comments can nest, so
-- their depth is tracked in the same loop.
scanOpaqueBody :: Word -> Bool -> ParserT st e Bool
scanOpaqueBody close nested = Parser $ \_ end start st ->
  let step cur !depth !hasEscapes
        | isTrue# (eqAddr# cur end) = (# st, Fail# #)
        | otherwise =
            let next = plusAddr# cur 1#
            in case W# (word8ToWord# (indexWord8OffAddr# cur 0#)) of
                 byte
                   | byte == close ->
                       if depth == 0 then (# st, OK# hasEscapes next #) else step next (depth - 1) hasEscapes
                 40 | nested -> step next (depth + 1) hasEscapes
                 92
                   | not (isTrue# (eqAddr# next end))
                   , isOpaqueByte (W# (word8ToWord# (indexWord8OffAddr# next 0#))) ->
                       step (plusAddr# next 1#) depth True
                   | otherwise -> (# st, Fail# #)
                 byte
                   | isOpaqueByte byte -> step next depth hasEscapes
                   | otherwise -> (# st, Fail# #)
  in step start (0 :: Int) False
{-# INLINE scanOpaqueBody #-}


isOpaqueByte :: Word -> Bool
isOpaqueByte byte = byte == 9 || (byte >= 32 && byte /= 127)
{-# INLINE isOpaqueByte #-}


unquotePairs :: ByteString -> ByteString
unquotePairs bytes = BSI.unsafeCreateUptoN (BS.length bytes) $ \dest ->
  BSU.unsafeUseAsCStringLen bytes $ \(source, size) -> do
    let copy !readAt !writeAt
          | readAt == size = pure writeAt
          | otherwise = do
              byte <- peekByteOff source readAt
              if byte == (92 :: Word8)
                then do
                  let next = readAt + 1
                  if next == size
                    then fail "Incomplete HTTP quoted pair"
                    else do
                      escaped <- peekByteOff source next
                      pokeByteOff dest writeAt (escaped :: Word8)
                      copy (next + 1) (writeAt + 1)
                else do
                  pokeByteOff dest writeAt byte
                  copy (readAt + 1) (writeAt + 1)
    copy 0 0


-- | The product and comment sequence shared by User-Agent and Server.
productSequence
  :: (ST.ShortText -> Maybe ST.ShortText -> product)
  -> (product -> [Either Comment product] -> field)
  -> ParserT st e field
productSequence mkProduct mkField =
  mkField <$> productToken <*> many (rws *> element)
  where
    element = (Right <$> productToken) <|> (Left . Comment <$> comment)
    productToken = mkProduct <$> rfc9110Token <*> branch $(char '/') (Just <$> rfc9110Token) (pure Nothing)
{-# INLINE productSequence #-}


-- | Parse an RFC 9110 list, skipping empty elements without allocating entries.
httpList :: ParserT st e a -> ParserT st e [a]
httpList = listWithEmptySlots $(char ',')
{-# INLINE httpList #-}


-- | Parse a required member directly into the head of an RFC 9110 list.
httpList1 :: String -> ParserT st String a -> ParserT st String (NonEmpty a)
httpList1 name element = do
  ows
  skipMany (comma *> ows)
  first <- (element <|> err (name <> " requires at least one member")) <* ows
  rest <- withOption comma (const (httpList element)) (pure [])
  pure (first :| rest)
  where
    comma = $(char ',')
{-# INLINE httpList1 #-}


-- | An ordered list that allows empty positions separated by a delimiter.
listWithEmptySlots :: ParserT st e () -> ParserT st e a -> ParserT st e [a]
listWithEmptySlots delimiter element = ows *> elements
  where
    separator = delimiter *> ows
    elements = skipMany separator *> (nonEmpty <|> pure [])
    nonEmpty = do
      value <- element <* ows
      rest <- withOption separator (const elements) (pure [])
      pure (value : rest)
{-# INLINE listWithEmptySlots #-}


-- | Interpret repeated field lines as one ordered list without copying their bytes.
parseListFieldLines :: Semigroup list => String -> ParserT st String list -> NonEmpty ByteString -> Either String list
parseListFieldLines name parser (first :| rest) = parseLines first rest
  where
    parseLine = parseFieldLine name parser
    parseLines bytes [] = parseLine bytes
    parseLines bytes (next : more) = (<>) <$> parseLine bytes <*> parseLines next more
{-# INLINE parseListFieldLines #-}


requireNonEmptyList :: String -> [a] -> Either String (NonEmpty a)
requireNonEmptyList name = maybe (Left (name <> " requires at least one member")) Right . NonEmpty.nonEmpty
{-# INLINE requireNonEmptyList #-}


-- | Read the padded Base64 encoding of an exact number of octets.
fixedBase64Bytes :: Int -> ParserT st String ByteString
fixedBase64Bytes size = do
  input <- takeBs ((size + 2) `quot` 3 * 4)
  maybe (err "Invalid fixed-width Base64 value") pure (decodeFixedBase64 size input)
{-# INLINE fixedBase64Bytes #-}


-- | Decode a complete canonical Base64 value with an exact decoded width.
decodeFixedBase64 :: Int -> ByteString -> Maybe ByteString
decodeFixedBase64 size input
  | BS.length input == (size + 2) `quot` 3 * 4
  , canonicalPadding
  , Just bytes <- decodeBase64 input
  , BS.length bytes == size =
      Just bytes
  | otherwise = Nothing
  where
    -- The last non-padding sextet has four or two unused bits.
    canonicalPadding = case size `rem` 3 of
      1 -> BS.index input (BS.length input - 3) `BS.elem` "AQgw"
      2 -> BS.index input (BS.length input - 2) `BS.elem` "AEIMQUYcgkosw048"
      _ -> True
{-# INLINE decodeFixedBase64 #-}


-- | Parse a singleton field, applying field-line OWS and cardinality.
parseSingleFieldLine :: String -> ParserT st String a -> NonEmpty ByteString -> Either String a
parseSingleFieldLine name parser (value :| []) =
  case parseByteString (ows *> parser <* ows <* eof) value of
    Right result -> Right result
    Left (ParseErr _ e) -> Left e
    Left _ -> Left ("Failed to parse " <> name <> " header")
parseSingleFieldLine name _ _ = Left ("Multiple " <> name <> " field lines")
{-# INLINE parseSingleFieldLine #-}


{- | Decode a singleton token. A bare token needs only validation and a copy
into its compact storage. The parser handles field-line OWS.
-}
parseTokenFieldLine :: String -> (ST.ShortText -> a) -> NonEmpty ByteString -> Either String a
parseTokenFieldLine name wrap fields@(wire :| [])
  | not (BS.null wire) && isTokenChar (BSC.head wire) && isTokenChar (BSC.last wire) =
      if BSC.all isTokenChar wire
        then Right $! wrap (STU.fromByteStringUnsafe wire)
        else Left ("Failed to parse " <> name <> " header")
  | otherwise = parseSingleFieldLine name (wrap <$> rfc9110Token) fields
parseTokenFieldLine name _ _ = Left ("Multiple " <> name <> " field lines")
{-# INLINE parseTokenFieldLine #-}


-- | Decode a singleton quoted-string field, interpreting quoted pairs.
parseQuotedFieldLine :: String -> (ByteString -> a) -> NonEmpty ByteString -> Either String a
parseQuotedFieldLine name wrap fields =
  parseQuotedFieldLineWith wrap (parseSingleFieldLine name (wrap <$> httpQuotedBytes) fields) fields
{-# INLINE parseQuotedFieldLine #-}


-- | Decode a singleton quoted-string field, retaining its wire escapes.
parseQuotedWireFieldLine :: String -> (ByteString -> a) -> NonEmpty ByteString -> Either String a
parseQuotedWireFieldLine name wrap fields =
  parseQuotedFieldLineWith wrap (parseSingleFieldLine name (wrap <$> httpQuotedWireBytes) fields) fields
{-# INLINE parseQuotedWireFieldLine #-}


-- A plain quoted body has no escapes to interpret and can be checked on the input slice.
parseQuotedFieldLineWith :: (ByteString -> a) -> Either String a -> NonEmpty ByteString -> Either String a
parseQuotedFieldLineWith wrap _ (wire :| [])
  | let n = BS.length wire
  , n >= 2
  , BSU.unsafeHead wire == 34
  , BSU.unsafeIndex wire (n - 1) == 34
  , let body = BSU.unsafeTake (n - 2) (BSU.unsafeDrop 1 wire)
  , BS.all (\byte -> byte /= 34 && byte /= 92 && isOpaqueByte (fromIntegral byte)) body =
      Right $! wrap body
parseQuotedFieldLineWith _ fallback _ = fallback
{-# INLINE parseQuotedFieldLineWith #-}


-- | The case-insensitive @T@ / @F@ literals used by WebDAV flag fields.
tfFlag :: ParserT st e Bool
tfFlag =
  (True <$ ($(char 'T') <|> $(char 't'))) <|> (False <$ ($(char 'F') <|> $(char 'f')))
{-# INLINE tfFlag #-}


-- | Decode a singleton flag. Canonical one-byte values bypass the parser.
parseTFFieldLine :: String -> (Bool -> a) -> NonEmpty ByteString -> Either String a
parseTFFieldLine _ wrap ("T" :| []) = Right (wrap True)
parseTFFieldLine _ wrap ("F" :| []) = Right (wrap False)
parseTFFieldLine name wrap fields = parseSingleFieldLine name (wrap <$> tfFlag) fields
{-# INLINE parseTFFieldLine #-}


parseFieldLine :: String -> ParserT st String a -> ByteString -> Either String a
parseFieldLine name parser bytes =
  case parseByteString (parser <* eof) bytes of
    Right value -> Right value
    Left (ParseErr _ e) -> Left e
    Left _ ->
      Left $ case runParser parser bytes of
        OK _ leftover -> "Unconsumed input after parsing " <> name <> " header: " <> show leftover
        _ -> "Failed to parse " <> name <> " header"
{-# INLINE parseFieldLine #-}


rfc8941List :: ParserT st e a -> ParserT st e [a]
rfc8941List p = p `sepBy` (ows *> $(char ',') *> ows)
{-# INLINE rfc8941List #-}


rfc8941List1 :: ParserT st e a -> ParserT st e (NonEmpty a)
rfc8941List1 p = p `NE.sepBy1` (ows *> $(char ',') *> ows)
{-# INLINE rfc8941List1 #-}


rfc8941InnerList :: ParserT st e a -> ParserT st e (NonEmpty a)
rfc8941InnerList p = do
  $(char '(')
  ows
  res <- p `NE.sepBy1` rws
  ows
  $(char ')')
  pure res
{-# INLINE rfc8941InnerList #-}


data ItemValueType t where
  ItemInteger :: ItemValueType Int
  ItemDecimal :: ItemValueType Milli
  ItemString :: ItemValueType RFC8941String
  ItemToken :: ItemValueType RFC8941Token
  ItemBinary :: ItemValueType ByteString
  ItemBoolean :: ItemValueType Bool
  ItemAny :: ItemValueType ItemValue


instance Lift (ItemValueType t) where
  lift ItemInteger = [|ItemInteger|]
  lift ItemDecimal = [|ItemDecimal|]
  lift ItemString = [|ItemString|]
  lift ItemToken = [|ItemToken|]
  lift ItemBinary = [|ItemBinary|]
  lift ItemBoolean = [|ItemBoolean|]
  lift ItemAny = [|ItemAny|]
  liftTyped = unsafeCodeCoerce . lift


rfc8941Integer :: ParserT st e Int
rfc8941Integer = do
  negative <- branch $(char '-') (pure True) (pure False)
  value <- sfDigits $ \number count ->
    if count <= 15 then pure (signedInteger negative number) else failed
  notFollowedBy $(char '.')
  pure value
{-# INLINE rfc8941Integer #-}


rfc8941Decimal :: ParserT st String Milli
rfc8941Decimal = do
  negative <- branch $(char '-') (pure True) (pure False)
  sfDigits $ \whole count -> $(char '.') *> sfDecimalTail negative whole count
{-# INLINE rfc8941Decimal #-}


-- Only use the accumulator after checking the digit count. Its Word
-- representation is wide enough for every supported Structured Field number.
sfDigits :: (Word -> Word64 -> ParserT st e a) -> ParserT st e a
sfDigits k = withSpan anyAsciiDecimalWord $ \n (Span (Pos start) (Pos end)) -> k n (end - start)
{-# INLINE sfDigits #-}


signedInteger :: Bool -> Word -> Int
signedInteger negative n = if negative then negate (fromIntegral n) else fromIntegral n
{-# INLINE signedInteger #-}


sfDecimalTail :: Bool -> Word -> Word64 -> ParserT st String Milli
sfDecimalTail negative whole count
  | count > 12 = err "Structured Field decimal has more than 12 integer digits"
  | otherwise = sfDigits $ \fraction digits -> case digits of
      1 -> decimal (fraction * 100)
      2 -> decimal (fraction * 10)
      3 -> decimal fraction
      _ -> err "Structured Field decimal has more than three fractional digits"
  where
    decimal fraction =
      let scaled = toInteger (whole * 1000 + fraction)
      in pure (MkFixed (if negative then negate scaled else scaled))


sfNumber :: ParserT st String ItemValue
sfNumber = do
  negative <- branch $(char '-') (pure True) (pure False)
  sfDigits $ \whole count ->
    branch
      $(char '.')
      (Decimal <$> sfDecimalTail negative whole count)
      (if count <= 15 then pure (Integer (signedInteger negative whole)) else err "Structured Field integer has more than 15 digits")


mkRFC8941Token :: ST.ShortText -> Maybe RFC8941Token
mkRFC8941Token t = case runParser rfc8941Token (ST.toByteString t) of
  OK x "" -> Just x
  _ -> Nothing


rfc8941Token :: ParserT st e RFC8941Token
rfc8941Token = RFC8941Token <$> shortASCIIFromParser_ scanToken
  where
    scanToken = do
      skipSatisfyAscii (\c -> c == '*' || isLatinLetter c)
      optional_ (asciiAlnumSetBytes structuredTokenTail)
{-# INLINE rfc8941Token #-}


structuredTokenTail :: AsciiSet
structuredTokenTail = asciiSet rfc8941TokenCharSet
{-# NOINLINE structuredTokenTail #-}


rfc8941TokenCharSet :: CharSet
rfc8941TokenCharSet = "/:" <> tokenCharSet
{-# NOINLINE rfc8941TokenCharSet #-}


rfc8941Boolean :: ParserT st e Bool
rfc8941Boolean =
  $( switch
       [|
         case _ of
           "?0" -> pure False
           "?1" -> pure True
         |]
   )
{-# INLINE rfc8941Boolean #-}


rfc8941Binary :: ParserT st String ByteString
rfc8941Binary = do
  $(char ':')
  rest <- lookahead takeRest
  case BS.elemIndex 58 rest of
    Nothing -> empty
    Just size ->
      case decodeBase64 (padBase64 (BS.take size rest)) of
        Nothing -> err "Invalid Structured Field byte sequence"
        Just bytes -> bytes <$ skip (size + 1)
  where
    padBase64 bs = case BS.length bs `rem` 4 of
      2 | not (BS.elem 61 bs) -> bs <> "=="
      3 | not (BS.elem 61 bs) -> BS.snoc bs 61
      _ -> bs
{-# INLINE rfc8941Binary #-}


mkRFC8941String :: ST.ShortText -> Maybe RFC8941String
mkRFC8941String t =
  if ST.all (`CharSet.member` (quotedCharSet <> quotedPairCharSet)) t
    then Just $ RFC8941String t
    else Nothing


rfc8941String :: ParserT st e RFC8941String
rfc8941String = RFC8941String <$> quotedString
{-# INLINE rfc8941String #-}


rfc8941ItemValue :: ParserT st String ItemValue
rfc8941ItemValue =
  asum
    [ sfNumber
    , String <$> rfc8941String
    , Token <$> rfc8941Token
    , Binary <$> rfc8941Binary
    , Boolean <$> rfc8941Boolean
    ]


itemValueTypeParser :: ItemValueType t -> ParserT st String t
itemValueTypeParser ItemInteger = rfc8941Integer
itemValueTypeParser ItemDecimal = rfc8941Decimal
itemValueTypeParser ItemString = rfc8941String
itemValueTypeParser ItemToken = rfc8941Token
itemValueTypeParser ItemBinary = rfc8941Binary
itemValueTypeParser ItemBoolean = rfc8941Boolean
itemValueTypeParser ItemAny = rfc8941ItemValue
{-# INLINE itemValueTypeParser #-}


anyRfc8941Parameter :: ItemValueType t -> ParserT st String (ST.ShortText, Maybe t)
anyRfc8941Parameter t = do
  $(char ';')
  ows
  key <- structuredFieldKey
  paramValue <- optional $ do
    $(char '=')
    itemValueTypeParser t
  pure (key, paramValue)
{-# INLINE anyRfc8941Parameter #-}


structuredFieldKey :: ParserT st e ST.ShortText
structuredFieldKey = shortASCIIFromParser_ scanStructuredFieldKey
{-# INLINE structuredFieldKey #-}


scanStructuredFieldKey :: ParserT st e ()
scanStructuredFieldKey = skipSatisfyAscii firstKeyChar *> skipMany (skipSatisfyAscii keyChar)
  where
    firstKeyChar c = ('a' <= c && c <= 'z') || c == '*'
    keyChar c = firstKeyChar c || isDigit c || c == '_' || c == '-' || c == '.'
{-# INLINE scanStructuredFieldKey #-}


knownRfc8941Parameter :: Q Exp -> ItemValueType t -> Q Exp
knownRfc8941Parameter k t = do
  [|
    do
      $(char ';')
      ows
      _ <- $(k)
      optional $ do
        $(char '=')
        itemValueTypeParser t
    |]
{-# INLINE knownRfc8941Parameter #-}


knownRfc8941WithRequiredKeyParser :: Q Exp -> Q Exp -> Q Exp
knownRfc8941WithRequiredKeyParser k t = do
  [|
    do
      $(char ';')
      ows
      _ <- $(k)
      $(char '=')
      $(t)
    |]
{-# INLINE knownRfc8941WithRequiredKeyParser #-}


knownRfc8941WithNoValueParser :: Q Exp -> Q Exp
knownRfc8941WithNoValueParser k = do
  [|
    do
      $(char ';')
      ows
      $(k)
    |]
{-# INLINE knownRfc8941WithNoValueParser #-}


rfc8941Parameters :: ParserT st String [(ST.ShortText, Maybe ItemValue)]
rfc8941Parameters = many (anyRfc8941Parameter ItemAny)
{-# INLINE rfc8941Parameters #-}


-- | Parse the ASCII string form from RFC 9651 section 4.2.5.
structuredFieldString :: ParserT st e RFC8941String
structuredFieldString =
  RFC8941String . STU.fromByteStringUnsafe
    <$> ($(char '"') *> withByteString scanStructuredString decodeOpaqueBody)


scanStructuredString :: ParserT st e Bool
scanStructuredString = Parser $ \_ end start st ->
  let step cur !escaped
        | isTrue# (eqAddr# cur end) = (# st, Fail# #)
        | otherwise =
            let next = plusAddr# cur 1#
            in case W# (word8ToWord# (indexWord8OffAddr# cur 0#)) of
                 34 -> (# st, OK# escaped next #)
                 92
                   | not (isTrue# (eqAddr# next end))
                   , let byte = W# (word8ToWord# (indexWord8OffAddr# next 0#))
                   , byte == 34 || byte == 92 ->
                       step (plusAddr# next 1#) True
                   | otherwise -> (# st, Fail# #)
                 byte
                   | byte >= 32 && byte <= 126 -> step next escaped
                   | otherwise -> (# st, Fail# #)
  in step start False
{-# INLINE scanStructuredString #-}


structuredFieldDisplayString :: ParserT st String Text
structuredFieldDisplayString = do
  $(string "%\"")
  withByteString (skipMany (literal <|> escape)) $ \() bytes -> do
    $(char '"')
    case urlDecodeMaybe bytes >>= either (const Nothing) Just . TextEncoding.decodeUtf8' of
      Just value -> pure value
      Nothing -> err "Invalid UTF-8 Structured Field display string"
  where
    literal = skipSatisfyAscii (\c -> c >= ' ' && c <= '~' && c /= '%' && c /= '"')
    escape = $(char '%') *> hex *> hex
    hex = skipSatisfyAscii (\c -> isDigit c || (c >= 'a' && c <= 'f'))


structuredFieldBareItem :: ParserT st String ItemValue
structuredFieldBareItem =
  asum
    [ sfNumber
    , String <$> structuredFieldString
    , Token <$> rfc8941Token
    , Binary <$> rfc8941Binary
    , Boolean <$> rfc8941Boolean
    , DateValue <$> ($(char '@') *> rfc8941Integer)
    , DisplayString <$> structuredFieldDisplayString
    ]


structuredFieldSpaces :: ParserT st e ()
structuredFieldSpaces = skipMany $(char ' ')
{-# INLINE structuredFieldSpaces #-}


-- | Parse a List using a field-specific member parser.
structuredFieldList :: ParserT st e a -> ParserT st e [a]
structuredFieldList member =
  structuredFieldSpaces *> ((member `sepBy1` (ows *> $(char ',') *> ows) <* ows) <|> ([] <$ eof))
{-# INLINE structuredFieldList #-}


-- | A Structured Fields identifier that accepts either a Token or a String.
structuredFieldTokenOrString :: ParserT st e (Either RFC8941Token RFC8941String)
structuredFieldTokenOrString = (Left <$> rfc8941Token) <|> (Right <$> structuredFieldString)
{-# INLINE structuredFieldTokenOrString #-}


structuredFieldParameters :: ParserT st String [StructuredFieldParameter]
structuredFieldParameters = orderedFieldEntries structuredParameterKey <$> many structuredFieldParameter


skipStructuredFieldParameters :: ParserT st String ()
skipStructuredFieldParameters = skipMany $ do
  $(char ';')
  structuredFieldSpaces
  scanStructuredFieldKey
  branch $(char '=') (() <$ structuredFieldBareItem) (pure ())
{-# INLINE skipStructuredFieldParameters #-}


{- | Read one named string from an Item's parameters. Each occurrence of
the name supersedes the previous one, including bare flags and values of
other types. Every parameter must conform to the Structured Fields grammar.
-}
structuredFieldStringParameter :: ByteString -> ParserT st String (Maybe RFC8941String)
structuredFieldStringParameter key = go Nothing
  where
    go !current = branch $(char ';') (structuredFieldSpaces *> parameter current) (pure current)
    parameter current = withByteString scanStructuredFieldKey $ \() name ->
      if name == key
        then branch $(char '=') (readString >>= go) (go Nothing)
        else branch $(char '=') (structuredFieldBareItem *> go current) (go current)
    readString = (Just <$> structuredFieldString) <|> (Nothing <$ structuredFieldBareItem)
{-# INLINE structuredFieldStringParameter #-}


structuredFieldParameter :: ParserT st String StructuredFieldParameter
structuredFieldParameter =
  $(char ';')
    *> structuredFieldSpaces
    *> (StructuredFieldParameter <$> structuredFieldKey <*> branch $(char '=') structuredFieldBareItem (pure (Boolean True)))
{-# INLINE structuredFieldParameter #-}


structuredFieldItem :: ParserT st String StructuredFieldItem
structuredFieldItem = StructuredFieldItem <$> structuredFieldBareItem <*> structuredFieldParameters


-- | Read a Boolean Item, validating parameters whose values are unused.
structuredFieldBooleanItem :: ParserT st String Bool
structuredFieldBooleanItem = rfc8941Boolean <* skipStructuredFieldParameters
{-# INLINE structuredFieldBooleanItem #-}


structuredFieldMemberParser :: ParserT st String StructuredFieldMember
structuredFieldMemberParser =
  branch
    (lookahead $(char '('))
    (StructuredInnerList <$> structuredFieldInnerList structuredFieldItem <*> structuredFieldParameters)
    (StructuredItem <$> structuredFieldItem)


-- | Parse an inner list, with ASCII spaces at its edges and between items.
structuredFieldInnerList :: ParserT st e a -> ParserT st e [a]
structuredFieldInnerList item = do
  $(char '(')
  structuredFieldSpaces
  items <- item `sepBy` skipSome $(char ' ')
  structuredFieldSpaces
  $(char ')')
  pure items
{-# INLINE structuredFieldInnerList #-}


structuredFieldDictionary :: ParserT st String [StructuredFieldDictionaryEntry]
structuredFieldDictionary = do
  structuredFieldSpaces
  branch eof (pure []) $
    orderedFieldEntries structuredDictionaryKey . NonEmpty.toList
      <$> (rfc8941List1 entry <* ows)
  where
    entry = StructuredFieldDictionaryEntry <$> structuredFieldKey <*> member
    member =
      branch
        $(char '=')
        structuredFieldMemberParser
        (StructuredItem . StructuredFieldItem (Boolean True) <$> structuredFieldParameters)


-- | Parse field lines as one dictionary, so repeated keys are resolved across lines.
parseStructuredFieldDictionary :: String -> NonEmpty ByteString -> Either String [StructuredFieldDictionaryEntry]
parseStructuredFieldDictionary name = parseStructuredFieldLines name structuredFieldDictionary


-- | Combine the physical lines of a Structured Field in wire order.
parseStructuredFieldLines :: String -> ParserT st String a -> NonEmpty ByteString -> Either String a
parseStructuredFieldLines name parser = parseFieldLine name parser . combineStructuredFieldLines
{-# INLINE parseStructuredFieldLines #-}


combineStructuredFieldLines :: NonEmpty ByteString -> ByteString
combineStructuredFieldLines = combineFieldLines ", "
{-# INLINE combineStructuredFieldLines #-}


combineFieldLines :: ByteString -> NonEmpty ByteString -> ByteString
combineFieldLines _ (first :| []) = first
combineFieldLines separator (first :| rest) = BS.intercalate separator (first : rest)
{-# INLINE combineFieldLines #-}


-- | Project parameters to the older typed-header representation.
structuredFieldParameterPairs :: [StructuredFieldParameter] -> [(ST.ShortText, Maybe ItemValue)]
structuredFieldParameterPairs = map parameter
  where
    parameter (StructuredFieldParameter key (Boolean True)) = (key, Nothing)
    parameter (StructuredFieldParameter key item) = (key, Just item)
{-# INLINE structuredFieldParameterPairs #-}


{- | Project a normalized dictionary whose members must be single items
of a field-specific type. The Boolean true parameter keeps the
established representation of a bare parameter.
-}
projectStructuredFieldItems
  :: String
  -> (ItemValue -> Maybe value)
  -> (ST.ShortText -> value -> [(ST.ShortText, Maybe ItemValue)] -> entry)
  -> [StructuredFieldDictionaryEntry]
  -> Either String [entry]
projectStructuredFieldItems name accept build = traverse project
  where
    project (StructuredFieldDictionaryEntry key (StructuredItem (StructuredFieldItem item params)))
      | Just value <- accept item = Right (build key value (structuredFieldParameterPairs params))
    project (StructuredFieldDictionaryEntry key _) = Left ("Invalid " <> name <> " member " <> show key)
{-# INLINE projectStructuredFieldItems #-}


takeRestShortText :: ParserT st e ST.ShortText
takeRestShortText = withByteString takeRest $ \_ bs -> pure $ STU.fromByteStringUnsafe bs
{-# INLINE takeRestShortText #-}
