{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Network.HTTP.ContentNegotiation (
  MediaType (..),
  MediaRange (..),
  WeightedMediaRange (..),
  mediaTypeParser,
  mediaTypeWithParametersParser,
  mediaRangeParser,
  weightParser,
  qualityParameter,
  qualityValueParser,
  weightedMediaRangeParser,
  weightedMediaRangesParser,
  negotiationList,
  parseNegotiationHeaders,
  renderMediaType,
  renderMediaRange,
  renderMediaParameterValue,
  renderParameterValue,
  renderWeight,
  renderQualityValue,
  renderWeightedMediaRange,
  renderWeightedMediaRanges,
) where

import Data.ByteString (ByteString)
import Data.Char (chr, ord)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util
import qualified Wireform.Builder as WB


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
mediaTypeParser = MediaType <$> (mediaName <* $(char '/')) <*> mediaName
{-# INLINE mediaTypeParser #-}


mediaName :: ParserT st e ST.ShortText
mediaName = case mediaNameChars of
  AsciiSet low high ->
    shortASCIIFromParser_ (skipSome (skipSatisfyAscii (asciiMember low high)) <* notFollowedBy $(char '*'))
{-# INLINE mediaName #-}


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


mediaRangeParser :: ParserT st e MediaRange
mediaRangeParser = mediaWithParameters mediaRangeTypeParser


mediaWithParameters :: ParserT st e MediaType -> ParserT st e MediaRange
mediaWithParameters typeParser =
  MediaRange <$> typeParser <*> (catMaybes <$> many (ows *> $(char ';') *> ows *> optional mediaParam))
{-# INLINE mediaWithParameters #-}


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


renderMediaType :: MediaType -> M.Builder
renderMediaType (MediaType base sub)
  | ST.null base && ST.null sub = "*/*"
  | ST.null sub = shortText base <> "/*"
  | otherwise = shortText base <> "/" <> shortText sub


renderMediaRange :: MediaRange -> M.Builder
renderMediaRange (MediaRange mt mp) =
  renderMediaType mt
    <> foldMap (\(k, v) -> M.char7 ';' <> shortText k <> M.char7 '=' <> renderMediaParameterValue v) mp


-- | A media parameter with a one-to-one Latin-1 mapping for opaque bytes.
renderMediaParameterValue :: ST.ShortText -> M.Builder
renderMediaParameterValue = renderParameterWith (renderHttpQuotedString . ST.toText)


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
