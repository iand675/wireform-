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
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii
import Data.Fixed
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..))
import Network.HTTP.Grammar.Types
import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Internal (Parser (..), Pure)
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


shortASCIIFromParser_ :: ParserT st e a -> ParserT st e ST.ShortText
shortASCIIFromParser_ p = withByteString p $ \_res s -> pure $ STU.fromByteStringUnsafe s


tokenCharSet :: CharSet
tokenCharSet = alnum <> "-+.^_`|~!#$%&'*"


rfc9110Token :: ParserT st e ST.ShortText
rfc9110Token = shortASCIIFromParser_ $ some (satisfyAscii (`CharSet.member` tokenCharSet))
{-# INLINE rfc9110Token #-}


rfc9110TokenBS :: ParserT st e ByteString
rfc9110TokenBS = byteStringOf $ some (satisfyAscii (`CharSet.member` tokenCharSet))
{-# INLINE rfc9110TokenBS #-}


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


comment :: ParserT st e Text
comment =
  between
    $(char '(')
    $(char ')')
    (Text.pack <$> many (unescapedChar <|> escapedChar))
  where
    unescapedChar = satisfyAscii (`CharSet.member` quotedCharSet)
    escapedChar = do
      $(char '\\')
      satisfyAscii (`CharSet.member` quotedPairCharSet)
{-# INLINE comment #-}


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
  res <- baseVal
  notFollowedBy $(char '.')
  pure res
  where
    baseVal = do
      sign <- ($(char '-') $> negate) <|> pure id
      sign . fromIntegral <$> anyAsciiDecimalWord
{-# INLINE rfc8941Integer #-}


rfc8941Decimal :: ParserT st String Milli
rfc8941Decimal = do
  sign <- branch $(char '-') (pure negate) (pure id)
  decimalPart <- anyAsciiDecimalInteger
  fracPart <-
    branch
      $(char '.')
      ( do
          startingZeros <- length <$> many (satisfyAscii (== '0'))
          withOption
            anyAsciiDecimalInteger
            (\fractionalPart -> pure (fractionalPart * 10 ^ negate startingZeros))
            (pure 0)
      )
      (pure 0)
  pure $ MkFixed $ sign (decimalPart * 1000 + fracPart)
{-# INLINE rfc8941Decimal #-}


mkRFC8941Token :: ST.ShortText -> Maybe RFC8941Token
mkRFC8941Token t = case runParser rfc8941Token (ST.toByteString t) of
  OK x "" -> Just x
  _ -> Nothing


rfc8941Token :: ParserT st e RFC8941Token
rfc8941Token = withByteString tokenParser $ \_ bs -> do
  pure $ RFC8941Token $! STU.fromByteStringUnsafe bs
  where
    tokenParser = do
      skipSatisfyAscii (\c -> c == '*' || c `CharSet.member` alpha)
      skipMany (skipSatisfyAscii (`CharSet.member` rfc8941TokenCharSet))
    rfc8941TokenCharSet = "/:" <> tokenCharSet
{-# INLINE rfc8941Token #-}


rfc8941Boolean :: ParserT st e Bool
rfc8941Boolean =
  $( switch
      [|
        case _ of
          "?0" -> pure True
          "?1" -> pure False
        |]
   )
{-# INLINE rfc8941Boolean #-}


rfc8941Binary :: ParserT st String ByteString
rfc8941Binary = do
  $(char ':')
  withByteString (many (satisfyAscii (`CharSet.member` base64CharSet))) $ \_ bs -> do
    case convertFromBase Base64 bs of
      Left e -> err e
      Right ok -> pure ok
  where
    base64CharSet = alpha <> digit <> "+/="
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
    [ Integer <$> rfc8941Integer
    , Decimal <$> rfc8941Decimal
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
  key <- paramKey
  paramValue <- optional $ do
    $(char '=')
    itemValueTypeParser t
  pure (key, paramValue)
  where
    lcalpha = CharSet.fromList ['a' .. 'z']
    star = CharSet.singleton '*'
    firstKeyChar = lcalpha <> star
    keyChar = lcalpha <> digit <> "_-.*"
    paramKey = withByteString (skipSatisfyAscii (`CharSet.member` firstKeyChar) *> skipMany (skipSatisfyAscii (`CharSet.member` keyChar))) $ \_ bs -> do
      pure $ STU.fromByteStringUnsafe bs
{-# INLINE anyRfc8941Parameter #-}


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


takeRestShortText :: ParserT st e ST.ShortText
takeRestShortText = withByteString takeRest $ \_ bs -> pure $ STU.fromByteStringUnsafe bs
{-# INLINE takeRestShortText #-}
