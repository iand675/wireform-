{-# LANGUAGE TemplateHaskell #-}

{- |
@Accept-Encoding@ — a request header by which a client states which
content codings (compression schemes such as @gzip@, @br@, or
@deflate@) it can decode in the response, each with an optional @;q=@
quality weight. Servers use it to choose how to encode the returned
representation.

The wire syntax is a comma-separated list of @coding [;q=value]@
entries, where @coding@ is a content-coding name from
"Network.HTTP.ContentCoding" (or the @*@ wildcard) and @q@ is
optional. We surface that as @[WeightedEncoding]@ (along with the
two singleton constructors that carry the wildcard / single-coding
shape) so callers see the same vocabulary as "Network.HTTP.Headers.Accept"
and "Network.HTTP.Headers.AcceptLanguage".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.3>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptCharset", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentEncoding", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptEncoding (
  AcceptEncoding (..),
  WeightedEncoding (..),
  EncodingTag (..),
  acceptEncodingParser,
  renderAcceptEncoding,
) where

import Control.Monad.Combinators (sepBy)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentCoding
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A single entry in an @Accept-Encoding@ list.
data EncodingTag
  = -- | The @*@ wildcard.
    AnyEncoding
  | NamedEncoding !ContentCoding
  deriving stock (Eq, Show)


-- | An encoding tag with an optional quality weight (defaults to 1.0).
data WeightedEncoding = WeightedEncoding
  { encodingTag :: !EncodingTag
  , encodingWeight :: !Double
  }
  deriving stock (Eq, Show)


-- | The full @Accept-Encoding@ header value.
newtype AcceptEncoding = AcceptEncoding
  { acceptEncoding :: [WeightedEncoding]
  }
  deriving stock (Eq, Show)


instance KnownHeader AcceptEncoding where
  type ParseFailure AcceptEncoding = String
  type Cardinality AcceptEncoding = 'ZeroOrOne
  type Direction AcceptEncoding = 'Request


  parseFromHeaders _ headers =
    case runParser acceptEncodingParser (NE.head headers) of
      OK ae "" -> Right ae
      OK _ rest -> Left $ "Unconsumed input after parsing Accept-Encoding header: " <> show rest
      Fail -> Left "Failed to parse Accept-Encoding header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAcceptEncoding


  headerName _ = hAcceptEncoding


acceptEncodingParser :: ParserT st String AcceptEncoding
acceptEncodingParser = AcceptEncoding <$> (weightedEncodingParser `sepBy` $(char ','))
  where
    weightedEncodingParser = do
      ows
      tag <- encodingTagParser
      w <- weightParser
      ows
      pure $ WeightedEncoding tag w

    encodingTagParser =
      ($(char '*') $> AnyEncoding)
        <|> (NamedEncoding <$> contentCodingParser)


{- | Parser for the optional @;q=Q@ tail. Copied from
"Network.HTTP.ContentNegotiation"'s 'weightParser' so we can
avoid an import cycle with the other Accept-* parsers; same
semantics.
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


renderWeightedEncoding :: WeightedEncoding -> M.Builder
renderWeightedEncoding (WeightedEncoding tag w) =
  renderTag tag
    <> if w == 1 then mempty else ";q=" <> M.doubleDec (realToFrac w)
  where
    renderTag AnyEncoding = "*"
    renderTag (NamedEncoding c) = renderContentCoding c


renderAcceptEncoding :: AcceptEncoding -> M.Builder
renderAcceptEncoding (AcceptEncoding xs) =
  M.intersperse ", " (map renderWeightedEncoding xs)
