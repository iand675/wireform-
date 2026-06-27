{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9110 §6.6.2 @Trailer@ — lists the field names the sender anticipates
emitting in the trailer section that follows a chunked message body.

== Grammar

@
Trailer = #field-name
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-6.6.2>

See also: "Network.HTTP.Headers.TransferEncoding", "Network.HTTP.Headers.TE",
"Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.Trailer (
  Trailer (..),
  trailerParser,
  renderTrailer,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTrailer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | A @Trailer@ header value: a non-empty list of field names.
newtype Trailer = Trailer {trailerFieldNames :: NonEmpty ST.ShortText}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader Trailer where
  type ParseFailure Trailer = String
  type Cardinality Trailer = 'ZeroOrMore
  type Direction Trailer = 'RequestAndResponse


  parseFromHeaders _ neHeaders = sconcat <$> traverse parseLine neHeaders
    where
      parseLine bs = case runParser trailerParser bs of
        OK v "" -> Right v
        OK _ rest -> Left $ "Unconsumed input after parsing Trailer header: " <> show rest
        Fail -> Left "Failed to parse Trailer header"
        Err e -> Left e
  renderToHeaders _ = pure . M.toStrictByteString . renderTrailer
  headerName _ = hTrailer


trailerParser :: ParserT st String Trailer
trailerParser = Trailer <$> (fieldName `sepBy1` (ows *> $(char ',') *> ows))


renderTrailer :: Trailer -> M.Builder
renderTrailer (Trailer names) = sepByCommas1 (fmap shortText names)
