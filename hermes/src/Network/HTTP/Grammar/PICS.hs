{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | PICS 1.1 label lists and lexical elements shared with label negotiation.
module Network.HTTP.Grammar.PICS (
  PicsLabelList (..),
  PicsServiceInfo (..),
  PicsServiceError (..),
  PicsLabelEntry (..),
  PicsLabelError (..),
  PicsSingleLabel (..),
  PicsOption (..),
  PicsExtensionRequirement (..),
  PicsData (..),
  PicsDate (..),
  PicsRating (..),
  PicsRatingValue (..),
  PicsMultiValue (..),
  PicsNumber,
  picsNumberValue,
  picsNumberBytes,
  parsePicsNumber,
  picsLabelListParser,
  renderPicsLabelList,
  picsQuotedName,
  picsQuotedURL,
) where

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet.Posix.Ascii as ASCII
import Data.Int (Int16)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific (scientific, toBoundedRealFloat)
import qualified Data.Set as Set
import Data.Time.Calendar (fromGregorianValid)
import Data.Word (Word16, Word8)
import GHC.Exts (Float (F#), Float#)
import Network.HTTP.Grammar (WireGrammar (..))
import Network.HTTP.Grammar.Authority (pctEncoded)
import Network.HTTP.Grammar.Parser
import Network.HTTP.Grammar.URIReference (absoluteURIBytes)
import qualified Network.HTTP.Headers.Mason as M
import Wireform.Base64 (decodeBase64)


newtype PicsLabelList = PicsLabelList {picsServices :: NonEmpty PicsServiceInfo}
  deriving stock (Eq, Show)


data PicsServiceInfo
  = PicsNoRatings ![ByteString]
  | PicsServiceFailure !ByteString !PicsServiceError
  | PicsServiceLabels !ByteString ![PicsOption] ![PicsLabelEntry]
  deriving stock (Eq, Show)


data PicsServiceError
  = PicsServiceDenied ![ByteString]
  | PicsServiceUnavailable
  deriving stock (Eq, Show)


data PicsLabelEntry
  = PicsLabel !PicsSingleLabel
  | PicsLabelTree ![PicsSingleLabel]
  | PicsLabelFailure !PicsLabelError
  deriving stock (Eq, Show)


data PicsLabelError
  = PicsLabelDenied !(Maybe ByteString) ![ByteString]
  | PicsNotLabeled ![ByteString]
  deriving stock (Eq, Show)


data PicsSingleLabel = PicsSingleLabel
  { picsLabelOptions :: ![PicsOption]
  , picsLabelRatings :: !(NonEmpty PicsRating)
  }
  deriving stock (Eq, Show)


{- | Quoted names retain percent escapes. Digest and signature options
contain decoded Base64 octets.
-}
data PicsOption
  = PicsBy !ByteString
  | PicsGeneric !Bool
  | PicsFor !ByteString
  | PicsOn !PicsDate
  | PicsUntil !PicsDate
  | PicsAt !PicsDate
  | PicsMIC !ByteString
  | PicsSignature !ByteString
  | PicsComment !ByteString
  | PicsCompleteLabel !ByteString
  | PicsExtension !PicsExtensionRequirement !ByteString ![PicsData]
  deriving stock (Eq, Show)


data PicsExtensionRequirement = PicsOptional | PicsMandatory
  deriving stock (Eq, Show)


data PicsData
  = PicsDataDate !PicsDate
  | PicsDataURL !ByteString
  | PicsDataNumber !PicsNumber
  | PicsDataName !ByteString
  | PicsDataList ![PicsData]
  deriving stock (Eq, Show)


-- | Local time with the offset east of GMT in minutes. PICS admits minute 60.
data PicsDate = PicsDate
  { picsYear :: {-# UNPACK #-} !Word16
  , picsMonth :: {-# UNPACK #-} !Word8
  , picsDay :: {-# UNPACK #-} !Word8
  , picsHour :: {-# UNPACK #-} !Word8
  , picsMinute :: {-# UNPACK #-} !Word8
  , picsOffsetMinutes :: {-# UNPACK #-} !Int16
  }
  deriving stock (Eq, Show)


data PicsRating = PicsRating
  { picsCategory :: !ByteString
  , picsRatingValue :: !PicsRatingValue
  }
  deriving stock (Eq, Show)


data PicsRatingValue = PicsScalar !PicsNumber | PicsMultiple ![PicsMultiValue]
  deriving stock (Eq, Show)


data PicsMultiValue = PicsExact !PicsNumber | PicsRange !PicsNumber !PicsNumber
  deriving stock (Eq, Show)


-- | A validated PICS decimal with its original spelling and binary32 value.
data PicsNumber = PicsNumber !ByteString {-# UNPACK #-} !Float
  deriving stock (Eq, Show)


picsNumberBytes :: PicsNumber -> ByteString
picsNumberBytes (PicsNumber wire _) = wire


picsNumberValue :: PicsNumber -> Float
picsNumberValue (PicsNumber _ value) = value


instance WireGrammar PicsLabelList where
  type GrammarErr PicsLabelList = String


  grammarParser = picsLabelListParser


  grammarRender = renderPicsLabelList


picsLabelListParser :: ParserT st String PicsLabelList
picsLabelListParser = ows *> parens body <* ows
  where
    body = do
      keyword "pics-1.1"
      first <- ows *> serviceInfo
      rest <- many (ows *> serviceInfo)
      pure (PicsLabelList (first :| rest))


serviceInfo :: ParserT st String PicsServiceInfo
serviceInfo =
  (PicsNoRatings <$> (keyword "error" *> ows *> parens (keyword "no-ratings" *> explanations)))
    <|> (picsQuotedURL >>= \uri -> ows *> serviceBody uri)
  where
    serviceBody uri = (PicsServiceFailure uri <$> serviceError) <|> serviceLabels uri
{-# NOINLINE serviceInfo #-}


serviceLabels :: ByteString -> ParserT st String PicsServiceInfo
serviceLabels uri = do
  options <- picsOptions
  ows *> (keyword "labels" <|> keyword "l")
  entries <- many (ows *> labelEntry (labelDefaults defaultOptions options))
  pure (PicsServiceLabels uri options entries)


serviceError :: ParserT st String PicsServiceError
serviceError =
  keyword "error"
    *> ows
    *> ( (PicsServiceUnavailable <$ keyword "service-unavailable")
           <|> parens (PicsServiceDenied <$> (keyword "request-denied" *> explanations))
       )


labelEntry :: LabelDefaults -> ParserT st String PicsLabelEntry
labelEntry defaults =
  (PicsLabelFailure <$> labelError)
    <|> (PicsLabelTree <$> parens (many (singleLabel defaults <* ows)))
    <|> (PicsLabel <$> singleLabel defaults)


labelError :: ParserT st String PicsLabelError
labelError =
  keyword "error"
    *> ows
    *> parens
      ( (keyword "request-denied" *> ows *> denied)
          <|> (keyword "not-labeled" *> ows *> (PicsNotLabeled <$> many (picsQuotedURL <* ows)))
      )
  where
    denied =
      withOption
        picsQuotedURL
        (\uri -> PicsLabelDenied (Just uri) <$> explanations)
        (pure (PicsLabelDenied Nothing []))


explanations :: ParserT st String [ByteString]
explanations = many (ows *> picsQuotedName)


data LabelDefaults = LabelDefaults !Bool !Bool


defaultOptions :: LabelDefaults
defaultOptions = LabelDefaults False False


labelDefaults :: LabelDefaults -> [PicsOption] -> LabelDefaults
labelDefaults = foldl' add
  where
    add (LabelDefaults _ target) (PicsGeneric generic) = LabelDefaults generic target
    add (LabelDefaults generic _) (PicsFor _) = LabelDefaults generic True
    add defaults _ = defaults


singleLabel :: LabelDefaults -> ParserT st String PicsSingleLabel
singleLabel defaults = do
  options <- picsOptions
  ows *> (keyword "ratings" <|> keyword "r")
  case labelDefaults defaults options of
    LabelDefaults True False -> err "Generic PICS label requires a for URL"
    _ -> PicsSingleLabel options <$> (ows *> parens ratings)
{-# NOINLINE singleLabel #-}


picsOptions :: ParserT st String [PicsOption]
picsOptions = go 0 Set.empty
  where
    go !seen !extensions = withOption (ows *> option) (accept seen extensions) (pure [])
    accept seen extensions value = case value of
      PicsExtension _ uri _
        | Set.member uri extensions -> err "Repeated PICS extension URL"
        | otherwise -> (value :) <$> go seen (Set.insert uri extensions)
      _
        | tag .&. seen /= 0 -> err "Repeated PICS label option"
        | otherwise -> (value :) <$> go (seen .|. tag) extensions
        where
          tag = optionTag value
{-# NOINLINE picsOptions #-}


optionTag :: PicsOption -> Word16
optionTag = \case
  PicsBy _ -> 1
  PicsGeneric _ -> 2
  PicsFor _ -> 4
  PicsOn _ -> 8
  PicsUntil _ -> 16
  PicsAt _ -> 32
  PicsMIC _ -> 64
  PicsSignature _ -> 128
  PicsCompleteLabel _ -> 256
  PicsComment _ -> 0
  PicsExtension {} -> 0


option :: ParserT st String PicsOption
option = do
  name <- lower <$> rfc9110TokenBS
  case name of
    "by" -> PicsBy <$> (ows *> picsQuotedName)
    "generic" -> PicsGeneric <$> (ows *> boolean)
    "gen" -> PicsGeneric <$> (ows *> boolean)
    "for" -> PicsFor <$> (ows *> picsQuotedURL)
    "on" -> PicsOn <$> (ows *> quotedDate)
    "until" -> PicsUntil <$> (ows *> quotedDate)
    "exp" -> PicsUntil <$> (ows *> quotedDate)
    "at" -> PicsAt <$> (ows *> quotedDate)
    "mic-md5" -> PicsMIC <$> (ows *> mic)
    "md5" -> PicsMIC <$> (ows *> mic)
    "signature-rsa-md5" -> PicsSignature <$> (ows *> signature)
    "comment" -> PicsComment <$> (ows *> picsQuotedName)
    "complete-label" -> PicsCompleteLabel <$> (ows *> picsQuotedURL)
    "full" -> PicsCompleteLabel <$> (ows *> picsQuotedURL)
    "extension" -> ows *> parens extension
    _ -> failed
  where
    mic = do
      bytes <- quotedBase64
      maybe (err "PICS MIC requires a 16-byte MD5 value") pure (decodeFixedBase64 16 bytes)
    signature = do
      bytes <- quotedBase64
      case decodeBase64 bytes of
        Just value | not (BS.null value) -> pure value
        _ -> err "Invalid PICS signature Base64"
    extension =
      PicsExtension
        <$> ((PicsOptional <$ keyword "optional") <|> (PicsMandatory <$ keyword "mandatory"))
        <*> (ows *> picsQuotedURL)
        <*> many (ows *> extensionData)
{-# NOINLINE option #-}


extensionData :: ParserT st String PicsData
extensionData =
  (PicsDataList <$> parens (many (extensionData <* ows)))
    <|> (PicsDataNumber <$> number)
    <|> (PicsDataDate <$> quotedDate)
    <|> (PicsDataURL <$> picsQuotedURL)
    <|> (PicsDataName <$> picsQuotedName)
{-# NOINLINE extensionData #-}


boolean :: ParserT st String Bool
boolean = do
  value <- lower <$> rfc9110TokenBS
  case value of
    "t" -> pure True
    "true" -> pure True
    "f" -> pure False
    "false" -> pure False
    _ -> err "Invalid PICS boolean"


quotedBase64 :: ParserT st String ByteString
quotedBase64 = do
  value <- $(char '"') *> byteStringOf (skipSome (skipSatisfyAscii allowed)) <* $(char '"')
  if BSC.any isOWS value then pure (BSC.filter (not . isOWS) value) else pure value
  where
    allowed c = isOWS c || c == '+' || c == '/' || c == '=' || isAsciiAlphaNumeric c


isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDigit c


quotedDate :: ParserT st String PicsDate
quotedDate = do
  $(char '"')
  century <- twoDigits
  yearInCentury <- twoDigits
  let year = 100 * century + yearInCentury
  month <- $(char '.') *> twoDigits
  day <- $(char '.') *> twoDigits
  hour <- $(char 'T') *> twoDigits
  minute <- $(char ':') *> twoDigits
  sign <- (1 <$ $(char '+')) <|> (-1 <$ $(char '-'))
  zoneHour <- twoDigits
  zoneMinute <- twoDigits <* $(char '"')
  case fromGregorianValid (fromIntegral year) month day of
    Just _
      | hour <= 23 && minute <= 60 && zoneHour <= 23 && zoneMinute <= 59 ->
          pure (PicsDate (fromIntegral year) (fromIntegral month) (fromIntegral day) (fromIntegral hour) (fromIntegral minute) (fromIntegral (sign * (60 * zoneHour + zoneMinute))))
    _ -> failed
{-# NOINLINE quotedDate #-}


twoDigits :: ParserT st String Int
twoDigits = do
  a <- digit
  b <- digit
  pure (a * 10 + b)
  where
    digit = do
      value <- anyWord8
      if value >= 48 && value <= 57 then pure (fromIntegral (value - 48)) else failed
{-# INLINE twoDigits #-}


ratings :: ParserT st String (NonEmpty PicsRating)
ratings = do
  first <- rating
  rest <- go (Set.singleton (picsCategory first))
  pure (first :| rest)
  where
    go !seen = withOption (ows *> rating) (accept seen) (pure [])
    accept seen value
      | Set.member name seen = err "Repeated PICS rating category"
      | otherwise = (value :) <$> go (Set.insert name seen)
      where
        name = picsCategory value
{-# NOINLINE ratings #-}


rating :: ParserT st String PicsRating
rating = do
  name <- transmitName
  ows
  value <- (PicsMultiple <$> parens (many (multiValue <* ows))) <|> (PicsScalar <$> number)
  pure (PicsRating name value)


multiValue :: ParserT st String PicsMultiValue
multiValue = do
  first <- number
  withOption (ows *> $(char ':') *> ows *> number) (pure . PicsRange first) (pure (PicsExact first))


parsePicsNumber :: ByteString -> Either String PicsNumber
parsePicsNumber = parseFieldLine "PICS number" number


number :: ParserT st String PicsNumber
number = withByteString grammar $ \() raw ->
  case readNumber raw of
    (# result | #) -> pure (PicsNumber raw (F# result))
    (# | (# #) #) -> err "PICS rating outside single-precision range"
  where
    grammar = optional_ ($(char '-') <|> $(char '+')) *> skipSome digit *> optional_ ($(char '.') *> skipMany digit)
    digit = skipSatisfyAscii isDigit
{-# NOINLINE number #-}


readNumber :: ByteString -> (# Float# | (# #) #)
readNumber wire = case BSC.readInteger digits of
  Just (whole, rest)
    | BS.length rest <= 1 -> finish (fromInteger whole)
    | otherwise ->
        let decimals = BSU.unsafeDrop 1 rest
            scale = BS.length decimals
        in case BSC.readInteger decimals of
             Just (fraction, _) ->
               let coefficient = if whole == 0 then fraction else whole * 10 ^ scale + fraction
               in finish (either id id (toBoundedRealFloat (scientific coefficient (negate scale))))
             Nothing -> (# | (# #) #)
  Nothing -> (# | (# #) #)
  where
    negative = BSU.unsafeHead wire == 45
    sign = negative || BSU.unsafeHead wire == 43
    digits = if sign then BSU.unsafeDrop 1 wire else wire
    finish value
      | isInfinite value = (# | (# #) #)
      | otherwise = case if negative then negate value else value of
          F# result -> (# result | #)
{-# NOINLINE readNumber #-}


transmitName :: ParserT st String ByteString
transmitName = byteStringOf (segment *> skipMany ($(char '/') *> segment))
  where
    segment = skipSome (asciiAlnumSetBytes transmitChars <|> pctEncoded)


transmitChars :: AsciiSet
transmitChars = asciiSet (ASCII.alnum <> "+-.$,;:&=?!*~@#_")
{-# NOINLINE transmitChars #-}


quotedNameChars :: AsciiSet
quotedNameChars = asciiSet (ASCII.alnum <> "+-.$,;:&=?!*~@#_() ")
{-# NOINLINE quotedNameChars #-}


picsQuotedName :: ParserT st String ByteString
picsQuotedName =
  $(char '"') *> byteStringOf (skipSome (asciiAlnumSetBytes quotedNameChars <|> pctEncoded)) <* $(char '"')
{-# NOINLINE picsQuotedName #-}


picsQuotedURL :: ParserT st String ByteString
picsQuotedURL = $(char '"') *> byteStringOf absoluteURIBytes <* $(char '"')
{-# NOINLINE picsQuotedURL #-}


keyword :: ByteString -> ParserT st String ()
keyword expected = do
  actual <- rfc9110TokenBS
  if lower actual == expected then pure () else failed
{-# INLINE keyword #-}


lower :: ByteString -> ByteString
lower bytes
  | BSC.any isAsciiUpper bytes = BSC.map toLower bytes
  | otherwise = bytes
{-# INLINE lower #-}


parens :: ParserT st e a -> ParserT st e a
parens parser = $(char '(') *> ows *> parser <* ows <* $(char ')')
{-# INLINE parens #-}


renderPicsLabelList :: PicsLabelList -> M.Builder
renderPicsLabelList (PicsLabelList services) = parensBuilder ("PICS-1.1" <> spaced renderService services)
  where
    renderService (PicsNoRatings notes) = "error (no-ratings" <> spaced quoted notes <> ")"
    renderService (PicsServiceFailure url failure) = quoted url <> " error " <> renderServiceError failure
    renderService (PicsServiceLabels url options entries) =
      quoted url <> spaced renderOption options <> " labels" <> spaced renderEntry entries
    renderServiceError (PicsServiceDenied notes) = parensBuilder ("request-denied" <> spaced quoted notes)
    renderServiceError PicsServiceUnavailable = "service-unavailable"
    renderEntry (PicsLabel label) = renderLabel label
    renderEntry (PicsLabelTree entries) = parensBuilder (renderItems renderLabel entries)
    renderEntry (PicsLabelFailure failure) = "error " <> renderFailure failure
    renderFailure (PicsLabelDenied url notes) = parensBuilder ("request-denied" <> spaced quoted url <> spaced quoted notes)
    renderFailure (PicsNotLabeled urls) = parensBuilder ("not-labeled" <> spaced quoted urls)


renderLabel :: PicsSingleLabel -> M.Builder
renderLabel (PicsSingleLabel options (first :| rest)) =
  foldMap (\value -> renderOption value <> " ") options
    <> "ratings "
    <> parensBuilder (renderRating first <> spaced renderRating rest)


renderRating :: PicsRating -> M.Builder
renderRating (PicsRating category value) = M.byteString category <> " " <> renderValue value
  where
    renderValue (PicsScalar scalar) = renderNumber scalar
    renderValue (PicsMultiple values) = parensBuilder (renderItems renderMulti values)
    renderMulti (PicsExact scalar) = renderNumber scalar
    renderMulti (PicsRange first lastValue) = renderNumber first <> ":" <> renderNumber lastValue


renderOption :: PicsOption -> M.Builder
renderOption = \case
  PicsBy name -> "by " <> quoted name
  PicsGeneric True -> "generic true"
  PicsGeneric False -> "generic false"
  PicsFor url -> "for " <> quoted url
  PicsOn stamp -> "on " <> renderDate stamp
  PicsUntil stamp -> "until " <> renderDate stamp
  PicsAt stamp -> "at " <> renderDate stamp
  PicsMIC bytes -> "MIC-md5 " <> quoted (Base64.encode bytes)
  PicsSignature bytes -> "signature-RSA-MD5 " <> quoted (Base64.encode bytes)
  PicsComment text -> "comment " <> quoted text
  PicsCompleteLabel url -> "complete-label " <> quoted url
  PicsExtension requirement uri values ->
    "extension " <> parensBuilder (renderRequirement requirement <> " " <> quoted uri <> spaced renderData values)
  where
    renderRequirement PicsOptional = "optional"
    renderRequirement PicsMandatory = "mandatory"


renderData :: PicsData -> M.Builder
renderData = \case
  PicsDataDate stamp -> renderDate stamp
  PicsDataURL url -> quoted url
  PicsDataNumber scalar -> renderNumber scalar
  PicsDataName name -> quoted name
  PicsDataList values -> parensBuilder (renderItems renderData values)


renderDate :: PicsDate -> M.Builder
renderDate (PicsDate year month day hour minute offset) =
  "\""
    <> M.intDecPadded 4 (fromIntegral year)
    <> "."
    <> two month
    <> "."
    <> two day
    <> "T"
    <> two hour
    <> ":"
    <> two minute
    <> M.char7 (if offset < 0 then '-' else '+')
    <> M.intDecPadded 2 offsetHours
    <> M.intDecPadded 2 offsetMinutes
    <> "\""
  where
    two = M.intDecPadded 2 . fromIntegral
    (offsetHours, offsetMinutes) = quotRem (abs (fromIntegral offset)) 60


renderNumber :: PicsNumber -> M.Builder
renderNumber = M.byteString . picsNumberBytes


parensBuilder :: M.Builder -> M.Builder
parensBuilder body = "(" <> body <> ")"


quoted :: ByteString -> M.Builder
quoted bytes = "\"" <> M.byteString bytes <> "\""


spaced :: Foldable t => (a -> M.Builder) -> t a -> M.Builder
spaced render = foldMap ((" " <>) . render)


renderItems :: (a -> M.Builder) -> [a] -> M.Builder
renderItems _ [] = mempty
renderItems render (first : rest) = render first <> spaced render rest
