{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
RFC 9112 §7.4 @TE@ — request header announcing the transfer codings
(beyond the implicit @chunked@) that the client is willing to accept in
the response, plus the @trailers@ keyword indicating willingness to
accept trailer fields.

== Grammar

@
TE              = #t-codings
t-codings       = "trailers" / ( transfer-coding [ weight ] )
transfer-coding = token *( OWS ";" OWS transfer-parameter )
weight          = OWS ";" OWS "q=" qvalue
@

Each entry is surfaced as a coding name (the @trailers@ keyword is just a
name) plus an optional quality 'Double' weight. Transfer-parameters other
than the @q@ weight are not in current use and are not modelled.

Spec: <https://www.rfc-editor.org/rfc/rfc9112#section-7.4>

See also: "Network.HTTP.Headers.TransferEncoding", "Network.HTTP.Headers.Trailer",
"Network.HTTP.Headers.Connection", "Network.HTTP.Headers.AcceptEncoding".
-}
module Network.HTTP.Headers.TE (
  TE (..),
  TECoding (..),
  teParser,
  renderTE,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTE)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A single @t-codings@ entry: a transfer-coding name with an optional q weight.
data TECoding = TECoding
  { teCodingName :: !ST.ShortText
  , teCodingWeight :: !(Maybe Double)
  }
  deriving stock (Eq, Show)


-- | A @TE@ header value: a non-empty list of 'TECoding' entries.
newtype TE = TE {teCodings :: NonEmpty TECoding}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader TE where
  type ParseFailure TE = String
  type Cardinality TE = 'ZeroOrMore
  type Direction TE = 'Request


  parseFromHeaders _ neHeaders = sconcat <$> traverse parseLine neHeaders
    where
      parseLine bs = case runParser teParser bs of
        OK v "" -> Right v
        OK _ rest -> Left $ "Unconsumed input after parsing TE header: " <> show rest
        Fail -> Left "Failed to parse TE header"
        Err e -> Left e
  renderToHeaders _ = pure . M.toStrictByteString . renderTE
  headerName _ = hTE


teParser :: ParserT st String TE
teParser = TE <$> (teCodingParser `sepBy1` (ows *> $(char ',') *> ows))


teCodingParser :: ParserT st String TECoding
teCodingParser = do
  name <- rfc9110Token
  TECoding name <$> teWeightParser


{- | The optional @OWS ";" OWS "q=" qvalue@ tail (same shape as the
@Accept-*@ weight parsers).
-}
teWeightParser :: ParserT st String (Maybe Double)
teWeightParser = optional weight
  where
    weight = do
      ows
      $(char ';')
      ows
      $(string "q=")
      qValue
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


renderTE :: TE -> M.Builder
renderTE (TE codings) = sepByCommas1 (fmap renderTECoding codings)


renderTECoding :: TECoding -> M.Builder
renderTECoding (TECoding name mw) =
  shortText name <> maybe mempty (\w -> ";q=" <> M.doubleDec w) mw
