{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
@Accept-Language@ — a request header by which a client states which
natural languages (language tags such as @en@, @en-US@, or @fr@) it
prefers for the response, each with an optional @;q=@ quality weight.
Servers use it for proactive content negotiation of localized
representations.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.4>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptCharset", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptLanguage (
  AcceptLanguage (..),
  WeightedLanguage (..),
  acceptLanguageParser,
  renderAcceptLanguage,
) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptLanguage)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A language tag with optional quality weight
data WeightedLanguage = WeightedLanguage
  { languageTag :: !ST.ShortText
  , languageWeight :: !Double
  }
  deriving stock (Eq, Show)


newtype AcceptLanguage = AcceptLanguage {languages :: [WeightedLanguage]}
  deriving stock (Eq, Show)


instance KnownHeader AcceptLanguage where
  type ParseFailure AcceptLanguage = String
  type Cardinality AcceptLanguage = 'ZeroOrOne
  type Direction AcceptLanguage = 'Request


  parseFromHeaders _ headers = case runParser acceptLanguageParser $ NE.head headers of
    OK accept "" -> Right accept
    OK _ rest -> Left $ "Unconsumed input after parsing Accept-Language header: " <> show rest
    Fail -> Left "Failed to parse Accept-Language header"
    Err err -> Left err


  renderToHeaders _ = M.toStrictByteString . renderAcceptLanguage


  headerName _ = hAcceptLanguage


acceptLanguageParser :: ParserT st String AcceptLanguage
acceptLanguageParser = AcceptLanguage <$> (weightedLanguageParser `sepBy` $(char ','))
  where
    weightedLanguageParser = do
      ows
      lang <- rfc9110Token
      q <- weightParser
      ows
      pure $ WeightedLanguage lang q


-- weightParser copied from ContentNegotiation.weightParser
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


renderWeightedLanguage :: WeightedLanguage -> M.Builder
renderWeightedLanguage (WeightedLanguage tag w) =
  shortText tag <> if w == 1 then mempty else ";q=" <> M.doubleDec w


renderAcceptLanguage :: AcceptLanguage -> M.Builder
renderAcceptLanguage (AcceptLanguage langs) = M.intersperse ", " $ map renderWeightedLanguage langs
