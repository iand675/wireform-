{- |
@Accept@ — a request header by which a client lists the media types
(MIME types) it is willing to receive in the response, each with an
optional @;q=@ quality weight expressing relative preference. Servers
use it for proactive content negotiation, selecting the best matching
representation to return (or replying @406 Not Acceptable@ when none is
acceptable). Servers can also send it to advertise media types preferred
for a subsequent request.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.1>

See also: "Network.HTTP.Headers.AcceptCharset", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.Accept (
  Accept (..),
  acceptParser,
  renderAccept,
) where

import Network.HTTP.ContentNegotiation
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccept)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype Accept = Accept {accept :: [WeightedMediaRange]}
  deriving stock (Eq, Show)


instance KnownHeader Accept where
  type ParseFailure Accept = String
  type Cardinality Accept = 'ZeroOrOne
  type Direction Accept = 'RequestAndResponse


  parseFromHeaders _ = fmap Accept . parseNegotiationHeaders "Accept" listParser


  renderToHeaders _ = M.toStrictByteString . renderAccept


  headerName _ = hAccept


acceptParser :: ParserT st String Accept
acceptParser = Accept <$> listParser


listParser :: ParserT st String [WeightedMediaRange]
listParser = weightedMediaRangesParser


renderAccept :: Accept -> M.Builder
renderAccept (Accept mediaRanges) = renderWeightedMediaRanges mediaRanges
