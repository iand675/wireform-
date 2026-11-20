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

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTrailer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (sepByCommas1, shortText)


-- | The field names that may appear in trailers. 'TrailerEmpty' announces none.
data Trailer
  = Trailer {trailerFieldNames :: NonEmpty ST.ShortText}
  | TrailerEmpty
  deriving stock (Eq, Show)


instance Semigroup Trailer where
  TrailerEmpty <> other = other
  other <> TrailerEmpty = other
  Trailer xs <> Trailer ys = Trailer (xs <> ys)


instance KnownHeader Trailer where
  type ParseFailure Trailer = String
  type Cardinality Trailer = 'ZeroOrMore
  type Direction Trailer = 'RequestAndResponse


  parseFromHeaders _ = fmap fromNames . parseListFieldLines "Trailer" (httpList fieldName)
  renderToHeaders _ = pure . M.toStrictByteString . renderTrailer
  headerName _ = hTrailer


trailerParser :: ParserT st String Trailer
trailerParser = fromNames <$> httpList fieldName


fromNames :: [ST.ShortText] -> Trailer
fromNames = maybe TrailerEmpty Trailer . NE.nonEmpty


renderTrailer :: Trailer -> M.Builder
renderTrailer TrailerEmpty = mempty
renderTrailer (Trailer names) = sepByCommas1 (fmap shortText names)
