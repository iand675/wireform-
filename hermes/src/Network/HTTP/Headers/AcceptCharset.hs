{-# LANGUAGE TemplateHaskell #-}

{- |
@Accept-Charset@ — a request header by which a client indicated which
character encodings (charsets) it was willing to accept in the
response, each with an optional @;q=@ quality weight.

The header is **deprecated** by RFC 9110 (servers SHOULD ignore it;
clients SHOULD NOT send it, because UTF-8 is now the de-facto universal
encoding). It still appears on the wire for legacy services, so we ship
a parser \/ renderer for completeness.

Wire syntax mirrors "Network.HTTP.Headers.AcceptLanguage": a comma-separated list of charset
tokens, each optionally followed by @;q=value@. The token @*@ is allowed
and stands for \"any other charset not explicitly listed\".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.2>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptCharset (
  AcceptCharset (..),
  WeightedCharset (..),
  acceptCharsetParser,
  renderAcceptCharset,
) where

import Control.Monad.Combinators (sepBy)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptCharset)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A charset token (e.g. @utf-8@) plus its quality weight.  The
@*@ wildcard is represented by the literal short text @"*"@.
-}
data WeightedCharset = WeightedCharset
  { charsetTag :: !ST.ShortText
  , charsetWeight :: !Double
  }
  deriving stock (Eq, Show)


newtype AcceptCharset = AcceptCharset
  { acceptCharset :: [WeightedCharset]
  }
  deriving stock (Eq, Show)


instance KnownHeader AcceptCharset where
  type ParseFailure AcceptCharset = String
  type Cardinality AcceptCharset = 'ZeroOrOne
  type Direction AcceptCharset = 'Request


  parseFromHeaders _ headers =
    case runParser acceptCharsetParser (NE.head headers) of
      OK ac "" -> Right ac
      OK _ rest -> Left $ "Unconsumed input after parsing Accept-Charset header: " <> show rest
      Fail -> Left "Failed to parse Accept-Charset header"
      Err err -> Left err


  renderToHeaders _ = M.toStrictByteString . renderAcceptCharset


  headerName _ = hAcceptCharset


acceptCharsetParser :: ParserT st String AcceptCharset
acceptCharsetParser = AcceptCharset <$> (weightedCharsetParser `sepBy` $(char ','))
  where
    weightedCharsetParser = do
      ows
      tag <-
        ($(char '*') $> ST.fromString "*")
          <|> rfc9110Token
      -- \^ The parens around the @$>@ are mandatory.  Wireform's
      -- 'Wireform.Parser.(<|>)' is @infixr 6@ while 'Data.Functor.($>)'
      -- is @infixl 4@; without the parens this would group as
      -- @$(char '*') $> (ST.fromString \"*\" \<|\> rfc9110Token)@, which
      -- requires @*@ to match before the alternative is even
      -- considered.
      w <- weightParser
      ows
      pure $ WeightedCharset tag w


{- | Parser for the optional @;q=Q@ tail (same shape as
"Network.HTTP.Headers.AcceptLanguage"\u2019s 'weightParser').
-}
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


renderWeightedCharset :: WeightedCharset -> M.Builder
renderWeightedCharset (WeightedCharset tag w) =
  shortText tag
    <> if w == 1 then mempty else ";q=" <> M.doubleDec (realToFrac w)


renderAcceptCharset :: AcceptCharset -> M.Builder
renderAcceptCharset (AcceptCharset xs) =
  M.intersperse ", " (map renderWeightedCharset xs)
