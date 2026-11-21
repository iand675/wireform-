{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Network.HTTP.ContentNegotiation (
  MediaType (..),
  MediaRange (..),
  WeightedMediaRange (..),
  mediaTypeParser,
  restrictedMediaTypeParser,
  mediaTypeWithParametersParser,
  mediaTypeWithRawNames,
  mediaTypeSpelling,
  mediaRangeParser,
  weightParser,
  qualityParameter,
  qualityValueParser,
  isQualityValueText,
  weightedMediaRangeParser,
  weightedMediaRangesParser,
  negotiationList,
  parseNegotiationHeaders,
  preferredByQuality,
  normalizedQuality,
  sameNegotiationToken,
  renderMediaType,
  renderMediaRange,
  encodeMediaTypesChecked,
  encodeMediaRangesChecked,
  encodeWeightedMediaRangesChecked,
  encodeWeightedTokensChecked,
  renderMediaParameterValue,
  renderParameterValue,
  renderWeight,
  writeWeight,
  writeExplicitWeight,
  renderQualityValue,
  renderWeightedMediaRange,
  renderWeightedMediaRanges,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import Data.Char (chr, ord)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util
import qualified Wireform.Builder as WB
import Wireform.Encode.Direct (dBytes, dWord8)


data MediaRange = MediaRange
  { mediaType :: !MediaType
  , mediaParams :: ![(ST.ShortText, ST.ShortText)]
  }
  deriving stock (Eq, Show)


data WeightedMediaRange = WeightedMediaRange
  { weightedMediaRange :: !MediaRange
  , mediaWeight :: !Double
  }
  deriving stock (Eq, Show)


data MediaType = MediaType
  { mediaBaseType :: !ST.ShortText
  , mediaSubtype :: !ST.ShortText
  }
  deriving stock (Eq, Show)


-- | Parse the two names of a concrete media type.
mediaTypeParser :: ParserT st e MediaType
mediaTypeParser = mediaNames MediaType mediaName
{-# INLINE mediaTypeParser #-}


-- | Parse the RFC 6838 type and subtype names used by Link's @type@ hint.
restrictedMediaTypeParser :: ParserT st e MediaType
restrictedMediaTypeParser = mediaNames MediaType restrictedName
  where
    restrictedName = shortASCIIFromParser_ $ do
      lookahead (skipSatisfyAscii (\c -> isLatinLetter c || isDigit c))
      withSpan (asciiAlnumSetBytes restrictedMediaNameChars) $ \() (Span (Pos start) (Pos end)) ->
        if end - start <= 127 then pure () else failed
{-# INLINE restrictedMediaTypeParser #-}


restrictedMediaNameChars :: AsciiSet
restrictedMediaNameChars = asciiSet (alnum <> "!#$&-^_.+")
{-# NOINLINE restrictedMediaNameChars #-}


mediaNames :: (name -> name -> a) -> ParserT st e name -> ParserT st e a
mediaNames build name = build <$> (name <* $(char '/')) <*> name
{-# INLINE mediaNames #-}


mediaName :: ParserT st e ST.ShortText
mediaName = shortASCIIFromParser_ skipMediaName
{-# INLINE mediaName #-}


skipMediaName :: ParserT st e ()
skipMediaName = case mediaNameChars of
  AsciiSet low high -> skipSome (skipSatisfyAscii (asciiMember low high)) <* notFollowedBy $(char '*')
{-# INLINE skipMediaName #-}


mediaNameChars :: AsciiSet
mediaNameChars = asciiSet (CharSet.difference tokenCharSet "*")


mediaRangeTypeParser :: ParserT st e MediaType
mediaRangeTypeParser =
  branch $(string "*/") (MediaType mempty mempty <$ wildcard) $
    MediaType <$> (mediaName <* $(char '/')) <*> ((mempty <$ wildcard) <|> mediaName)
  where
    wildcard = $(char '*') *> notFollowedBy (skipSatisfyAscii (`CharSet.member` tokenCharSet))
{-# INLINE mediaRangeTypeParser #-}


-- | Parse a concrete media type including its optional parameters.
mediaTypeWithParametersParser :: ParserT st e MediaRange
mediaTypeWithParametersParser = mediaWithParameters mediaTypeParser


-- | Validate a concrete media type without allocating its parsed parameters.
mediaTypeSpelling :: ParserT st e ()
mediaTypeSpelling =
  skipMediaName *> $(char '/') *> skipMediaName
    *> skipMany (ows *> $(char ';') *> ows *> optional_ rawParameter)
  where
    rawParameter = tokenBytes *> $(char '=') *> (tokenBytes <|> (() <$ httpQuotedWireBytes))
{-# INLINE mediaTypeSpelling #-}


-- | Project a concrete media type from slices of the two wire names and
-- its decoded parameters. The name slices share the input buffer.
mediaTypeWithRawNames
  :: (ByteString -> ByteString -> [(ST.ShortText, ST.ShortText)] -> a)
  -> ParserT st e a
mediaTypeWithRawNames build = mediaNames build (byteStringOf skipMediaName) <*> mediaParameters
{-# INLINE mediaTypeWithRawNames #-}


mediaRangeParser :: ParserT st e MediaRange
mediaRangeParser = mediaWithParameters mediaRangeTypeParser


mediaWithParameters :: ParserT st e MediaType -> ParserT st e MediaRange
mediaWithParameters typeParser = MediaRange <$> typeParser <*> mediaParameters
{-# INLINE mediaWithParameters #-}


mediaParameters :: ParserT st e [(ST.ShortText, ST.ShortText)]
mediaParameters = catMaybes <$> many (ows *> $(char ';') *> ows *> optional mediaParam)
{-# INLINE mediaParameters #-}


mediaParam :: ParserT st e (ST.ShortText, ST.ShortText)
mediaParam = (,) <$> rfc9110Token <*> ($(char '=') *> (rfc9110Token <|> (ST.fromText <$> httpQuotedString)))
{-# INLINE mediaParam #-}


weightParser :: ParserT st String Double
weightParser = flip (<|>) (pure 1) $ do
  ows
  $(char ';')
  ows
  qualityParameter
  qualityValueParser
{-# INLINE weightParser #-}


qualityParameter :: ParserT st e ()
qualityParameter = $(string "q=") <|> $(string "Q=")
{-# INLINE qualityParameter #-}


-- | RFC 9110 §12.4.2 permits zero to three fractional digits.
qualityValueParser :: ParserT st String Double
qualityValueParser =
  $( switch
       [|
         case _ of
           "0." -> withSpan (anyAsciiDecimalWord <|> pure 0) $ \d (Span (Pos start) (Pos end)) ->
             case end - start of
               0 -> pure 0
               1 -> pure $! fromIntegral d / 10
               2 -> pure $! fromIntegral d / 100
               3 -> pure $! fromIntegral d / 1000
               _ -> err "Too many digits after the decimal point in q-value"
           "0" -> pure 0
           "1.000" -> pure 1
           "1.00" -> pure 1
           "1.0" -> pure 1
           "1." -> pure 1
           "1" -> pure 1
         |]
   )
{-# INLINE qualityValueParser #-}


-- | Check the complete compact wire spelling of a qvalue.
isQualityValueText :: ST.ShortText -> Bool
isQualityValueText text =
  size > 0 && size <= 5 && (whole == 48 || whole == 49)
    && (size == 1 || (at 1 == 46 && scanCompactBytes fractionDigit bytes 2))
  where
    bytes = ST.toShortByteString text
    size = SBS.length bytes
    at = SBSI.unsafeIndex bytes
    whole = at 0
    fractionDigit byte
      | whole == 48 = byte - 48 <= 9
      | otherwise = byte == 48
{-# INLINE isQualityValueText #-}


weightedMediaRangeParser :: ParserT st String WeightedMediaRange
weightedMediaRangeParser = do
  mt <- mediaRangeTypeParser
  parameters mt [] Nothing
  where
    parameters mt params weight =
      withOption
        (ows *> $(char ';') *> ows)
        (\() -> withOption qualityParameter parseWeight parseMediaParam)
        (pure (WeightedMediaRange (MediaRange mt (reverse params)) (maybe 1 id weight)))
      where
        parseWeight () = case weight of
          Just _ -> err "Duplicate Accept quality weight"
          Nothing -> do
            q <- qualityValueParser
            parameters mt params (Just q)
        parseMediaParam =
          withOption
            mediaParam
            (\param -> parameters mt (param : params) weight)
            (parameters mt params weight)


weightedMediaRangesParser :: ParserT st String [WeightedMediaRange]
weightedMediaRangesParser = negotiationList weightedMediaRangeParser


negotiationList :: ParserT st e a -> ParserT st e [a]
negotiationList = httpList
{-# INLINE negotiationList #-}


parseNegotiationHeaders :: String -> ParserT st String [a] -> NonEmpty ByteString -> Either String [a]
parseNegotiationHeaders = parseListFieldLines
{-# INLINE parseNegotiationHeaders #-}


-- | Select the highest positive quality, keeping offer order on ties.
-- Qualities at or above one are equally preferred.
preferredByQuality :: (a -> Double) -> [a] -> Maybe a
preferredByQuality rate = firstMatch
  where
    firstMatch [] = Nothing
    firstMatch (candidate : rest)
      | quality >= 1 = Just candidate
      | quality > 0 = bestMatch candidate quality rest
      | otherwise = firstMatch rest
      where
        quality = rate candidate

    bestMatch best !bestQuality (candidate : rest)
      | quality >= 1 = Just candidate
      | quality > bestQuality = bestMatch candidate quality rest
      | otherwise = bestMatch best bestQuality rest
      where
        quality = rate candidate
    bestMatch best _ [] = Just best
{-# INLINE preferredByQuality #-}


-- | Clamp an authored quality to @[0,1]@. NaN has quality zero.
normalizedQuality :: Double -> Double
normalizedQuality value
  | value >= 1 = 1
  | value > 0 = value
  | otherwise = 0
{-# INLINE normalizedQuality #-}


-- | Compare compact tokens using ASCII case folding.
sameNegotiationToken :: ST.ShortText -> ST.ShortText -> Bool
sameNegotiationToken a b = a == b || (SBS.length right == size && loop 0)
  where
    left = ST.toShortByteString a
    right = ST.toShortByteString b
    size = SBS.length left
    loop i
      | i == size = True
      | x == y || (lowerX == (y .|. 32) && 97 <= lowerX && lowerX <= 122) = loop (i + 1)
      | otherwise = False
      where
        x = SBSI.unsafeIndex left i
        y = SBSI.unsafeIndex right i
        lowerX = x .|. 32
{-# INLINE sameNegotiationToken #-}


renderMediaType :: MediaType -> M.Builder
renderMediaType (MediaType base sub)
  | ST.null base && ST.null sub = "*/*"
  | ST.null sub = shortText base <> "/*"
  | otherwise = shortText base <> "/" <> shortText sub


renderMediaRange :: MediaRange -> M.Builder
renderMediaRange (MediaRange mt mp) =
  renderMediaType mt
    <> foldMap (\(k, v) -> M.char7 ';' <> shortText k <> M.char7 '=' <> renderMediaParameterValue v) mp


data MediaFieldSyntax = ConcreteType | RangeType | WeightedRangeType


-- | Encode concrete media types with optional parameters.
encodeMediaTypesChecked :: [MediaRange] -> Maybe ByteString
encodeMediaTypesChecked = encodeCommaSeparatedChecked (mediaRangeBound ConcreteType) writeMediaRange


-- | Encode media ranges. A 'Nothing' projection emits a standalone wildcard.
encodeMediaRangesChecked :: (a -> Maybe MediaRange) -> [a] -> Maybe ByteString
encodeMediaRangesChecked project = encodeCommaSeparatedChecked bound write
  where
    bound entry = maybe 1 (mediaRangeBound RangeType) (project entry)
    write ptr offset entry =
      maybe (dWord8 ptr offset 42) (writeMediaRange ptr offset) (project entry)
{-# INLINE encodeMediaRangesChecked #-}


encodeWeightedMediaRangesChecked :: [WeightedMediaRange] -> Maybe ByteString
encodeWeightedMediaRangesChecked = encodeCommaSeparatedChecked bound write
  where
    bound (WeightedMediaRange range _) =
      let size = mediaRangeBound WeightedRangeType range
      in if size < 0 then size else addListSize size 8
    write ptr offset (WeightedMediaRange range weight) =
      writeMediaRange ptr offset range >>= \end -> writeWeight ptr end weight


-- | Encode a list of weighted tokens or ranges with one output allocation.
encodeWeightedTokensChecked
  :: (a -> Bool)
  -> (a -> ST.ShortText)
  -> (a -> Double)
  -> [a]
  -> Maybe ByteString
encodeWeightedTokensChecked valid name weight = encodeCommaSeparatedChecked bound write
  where
    bound entry
      | valid entry = addListSize 8 (shortWidth (name entry))
      | otherwise = -1
    write ptr offset entry =
      copyShort ptr offset (name entry) >>= \end -> writeWeight ptr end (weight entry)
{-# INLINE encodeWeightedTokensChecked #-}


mediaRangeBound :: MediaFieldSyntax -> MediaRange -> Int
mediaRangeBound syntax (MediaRange (MediaType base sub) params)
  | validNames = sizeParameters (addListSize 1 (addListSize (max 1 (shortWidth base)) (max 1 (shortWidth sub)))) params
  | otherwise = -1
  where
    validNames = case syntax of
      ConcreteType -> validName base && validName sub
      _ -> (ST.null base && ST.null sub) || (validName base && (ST.null sub || validName sub))
    validName text = case mediaNameChars of
      AsciiSet low high ->
        let bytes = ST.toShortByteString text
        in not (SBS.null bytes) && scanCompactBytes (asciiMember low high . BSI.w2c) bytes 0
    sizeParameters !size [] = size
    sizeParameters !size ((name, value) : rest)
      | isHttpTokenText name
      , not (reservedParameter name)
      , scanCompactBytes (isOpaqueByte . fromIntegral) (ST.toShortByteString value) 0 =
          let width = shortWidth value
              entrySize = addListSize (addListSize 4 (shortWidth name)) (addListSize width width)
          in sizeParameters (addListSize size entrySize) rest
      | otherwise = -1
    reservedParameter = case syntax of
      WeightedRangeType -> sameNegotiationToken "q"
      _ -> const False


writeMediaRange :: Ptr Word8 -> Int -> MediaRange -> IO Int
writeMediaRange ptr start (MediaRange (MediaType base sub) params) = do
  afterBase <- writeName start base
  slash <- dWord8 ptr afterBase 47
  afterSub <- writeName slash sub
  writeParameters afterSub params
  where
    writeName offset name
      | ST.null name = dWord8 ptr offset 42
      | otherwise = copyShort ptr offset name
    writeParameters !offset [] = pure offset
    writeParameters !offset ((name, value) : rest) = do
      afterSemicolon <- dWord8 ptr offset 59
      afterName <- copyShort ptr afterSemicolon name
      afterEquals <- dWord8 ptr afterName 61
      end <- if isHttpTokenText value
        then copyShort ptr afterEquals value
        else foldHttpQuotedShortBytes (dWord8 ptr) afterEquals (ST.toShortByteString value)
      writeParameters end rest


shortWidth :: ST.ShortText -> Int
shortWidth = SBS.length . ST.toShortByteString
{-# INLINE shortWidth #-}


copyShort :: Ptr Word8 -> Int -> ST.ShortText -> IO Int
copyShort ptr offset text = do
  let bytes = ST.toShortByteString text
      width = SBS.length bytes
  SBSI.copyToPtr bytes 0 (ptr `plusPtr` offset) width
  pure $! offset + width
{-# INLINE copyShort #-}


addListSize :: Int -> Int -> Int
addListSize = BSI.checkedAdd "HTTP negotiation field"
{-# INLINE addListSize #-}


-- | A media parameter with a one-to-one Latin-1 mapping for opaque bytes.
renderMediaParameterValue :: ST.ShortText -> M.Builder
renderMediaParameterValue = renderParameterWith renderHttpQuotedShortText


renderParameterValue :: ST.ShortText -> M.Builder
renderParameterValue = renderParameterWith quote
  where
    quote value = M.char7 '"' <> ST.foldl' escape mempty value <> M.char7 '"'
    escape builder c
      | c == '"' || c == '\\' = builder <> M.char7 '\\' <> M.char7 c
      | otherwise = builder <> WB.charUtf8 c


renderParameterWith :: (ST.ShortText -> M.Builder) -> ST.ShortText -> M.Builder
renderParameterWith quote value
  | not (ST.null value) && ST.all (`CharSet.member` tokenCharSet) value = shortText value
  | otherwise = quote value
{-# INLINE renderParameterWith #-}


renderWeightedMediaRange :: WeightedMediaRange -> M.Builder
renderWeightedMediaRange (WeightedMediaRange mr mw) = renderMediaRange mr <> renderWeight mw


{- | Render the nearest thousandth. Clamp caller-supplied weights to [0, 1]
and treat NaN as zero. Parsed weights are already within this range.
-}
renderWeight :: Double -> M.Builder
renderWeight weight = case qualityThousandths weight of
  1000 -> mempty
  value -> ";q=" <> renderQualityThousandths value
{-# INLINE renderWeight #-}


-- | Write a canonical weight suffix of at most eight octets into a buffer.
writeWeight :: Ptr Word8 -> Int -> Double -> IO Int
writeWeight ptr start weight = case qualityThousandths weight of
  1000 -> pure start
  value -> writeQualitySuffix ptr start value
{-# INLINE writeWeight #-}


-- | Write a quality suffix while retaining an explicit weight of one.
writeExplicitWeight :: Ptr Word8 -> Int -> Double -> IO Int
writeExplicitWeight ptr start weight = writeQualitySuffix ptr start (qualityThousandths weight)
{-# INLINE writeExplicitWeight #-}


writeQualitySuffix :: Ptr Word8 -> Int -> Int -> IO Int
writeQualitySuffix ptr start = \case
  1000 -> dBytes ptr start ";q=1"
  0 -> dBytes ptr start ";q=0"
  value -> do
    offset <- dBytes ptr start ";q=0."
    let (prefix, ones) = value `quotRem` 10
        (hundreds, tens) = prefix `quotRem` 10
        putDigit at digit = dWord8 ptr at (fromIntegral (48 + digit))
    afterHundreds <- putDigit offset hundreds
    if tens == 0 && ones == 0
      then pure afterHundreds
      else do
        afterTens <- putDigit afterHundreds tens
        if ones == 0 then pure afterTens else putDigit afterTens ones
{-# INLINE writeQualitySuffix #-}


-- | Render a qvalue including an explicitly supplied weight of one.
renderQualityValue :: Double -> M.Builder
renderQualityValue = renderQualityThousandths . qualityThousandths
{-# INLINE renderQualityValue #-}


qualityThousandths :: Double -> Int
qualityThousandths value
  | value >= 1 = 1000
  | not (value > 0) = 0
  | otherwise = round (value * 1000)
{-# INLINE qualityThousandths #-}


renderQualityThousandths :: Int -> M.Builder
renderQualityThousandths 0 = "0"
renderQualityThousandths 1000 = "1"
renderQualityThousandths n =
  let (prefix, ones) = n `quotRem` 10
      (hundreds, tens) = prefix `quotRem` 10
  in "0."
       <> digit hundreds
       <> (if tens /= 0 || ones /= 0 then digit tens else mempty)
       <> (if ones /= 0 then digit ones else mempty)
  where
    digit d = M.char7 (chr (ord '0' + d))


renderWeightedMediaRanges :: [WeightedMediaRange] -> M.Builder
renderWeightedMediaRanges = M.intersperse ", " . map renderWeightedMediaRange
