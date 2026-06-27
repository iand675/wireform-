{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Network.HTTP.ContentNegotiation where

import Control.Monad.Combinators (sepBy)
import Data.Functor (($>))
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util


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


mediaTypeParser :: ParserT st e MediaType
mediaTypeParser = do
  let mediaTypeStar = $(string "*/*") $> MediaType mempty mempty
      mediaTypeWithoutSubtype = MediaType <$> rfc9110Token <*> ($(string "/*") $> mempty)
      mediaTypeWithSubtype = MediaType <$> rfc9110Token <*> ($(char '/') *> rfc9110Token)
  mediaTypeStar <|> mediaTypeWithoutSubtype <|> mediaTypeWithSubtype


mediaRangeParser :: ParserT st e MediaRange
mediaRangeParser = do
  mt <- mediaTypeParser
  mp <- many (ows *> $(char ';') *> ows *> mediaParam)
  pure $ MediaRange mt mp
  where
    mediaParam = (,) <$> rfc9110Token <*> ($(char '=') *> (rfc9110Token <|> quotedString))


weightParser :: ParserT st String Double
weightParser = flip (<|>) (pure 1) $ do
  ows
  $(char ';')
  ows
  $(string "q=")
  qValue
  where
    qValue =
      $( switch
          [|
            case _ of
              "0." -> withSpan anyAsciiDecimalWord $ \d (Span (Pos start) (Pos end)) -> do
                let d' = fromIntegral d
                case end - start of
                  1 -> pure $! d' / 10
                  2 -> pure $! d' / 100
                  3 -> pure $! d' / 1000
                  _ -> err "Too many digits after the decimal point in q-value"
              "0" -> pure 0
              "1.000" -> pure 1
              "1.00" -> pure 1
              "1.0" -> pure 1
              "1" -> pure 1
            |]
       )


weightedMediaRangeParser :: ParserT st String WeightedMediaRange
weightedMediaRangeParser = do
  mr <- mediaRangeParser
  WeightedMediaRange mr <$> weightParser


weightedMediaRangesParser :: ParserT st String [WeightedMediaRange]
weightedMediaRangesParser = weightedMediaRangeParser `sepBy` $(char ',')


renderMediaType :: MediaType -> M.Builder
renderMediaType (MediaType base sub)
  | ST.null base && ST.null sub = "*/*"
  | ST.null sub = shortText base <> "/*"
  | otherwise = shortText base <> "/" <> shortText sub


renderMediaRange :: MediaRange -> M.Builder
renderMediaRange (MediaRange mt mp) =
  renderMediaType mt
    <> foldMap (\(k, v) -> M.char7 ';' <> shortText k <> M.char7 '=' <> shortText v) mp


renderWeightedMediaRange :: WeightedMediaRange -> M.Builder
renderWeightedMediaRange (WeightedMediaRange mr mw) =
  renderMediaRange mr
    <> if mw == 1 then mempty else ";q=" <> M.doubleDec mw


renderWeightedMediaRanges :: [WeightedMediaRange] -> M.Builder
renderWeightedMediaRanges = M.intersperse ", " . map renderWeightedMediaRange
