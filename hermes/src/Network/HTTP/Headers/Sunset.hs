{- |
The @Sunset@ response header signals that a resource is expected to become
unresponsive at a specific point in time — typically used to announce that an
API endpoint will be deprecated and removed. Its value is a single HTTP-date,
and an accompanying @Link@ with @rel="sunset"@ may point to a human-readable
explanation.

@
Sunset = HTTP-date
@

Spec: <https://www.rfc-editor.org/rfc/rfc8594>

See also: "Network.HTTP.Headers.Link", "Network.HTTP.Headers.RetryAfter", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.Sunset (
  Sunset (..),
  sunsetParser,
  renderSunset,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hSunset)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype Sunset = Sunset {sunsetDate :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader Sunset where
  type ParseFailure Sunset = String
  type Cardinality Sunset = 'ZeroOrOne
  type Direction Sunset = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser sunsetParser header of
      OK s "" -> Right s
      OK _ rest -> Left $ "Unconsumed input after parsing Sunset header: " <> show rest
      Fail -> Left "Failed to parse Sunset header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderSunset


  headerName _ = hSunset


sunsetParser :: ParserT st String Sunset
sunsetParser = Sunset <$> dateParser


renderSunset :: Sunset -> M.Builder
renderSunset (Sunset time) = renderDate time
