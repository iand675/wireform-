{- |
@Expires@ is a response header giving the absolute date\/time after which the
response is considered stale. It is the original HTTP\/1.0 freshness control:
caches use it to decide whether a stored response may be reused without
revalidation. When the response also carries a @Cache-Control: max-age@ (or
@s-maxage@) directive, that directive takes precedence over @Expires@. A date
in the past — conventionally the single character @0@ — marks the response as
already stale.

Spec: <https://www.rfc-editor.org/rfc/rfc9111#section-5.3>

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Age", "Network.HTTP.Headers.Date", "Network.HTTP.Headers.LastModified".
-}
module Network.HTTP.Headers.Expires (
  Expires (..),
  expiresParser,
  renderExpires,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hExpires)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype Expires = Expires {expires :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader Expires where
  type ParseFailure Expires = String
  type Cardinality Expires = 'ZeroOrOne
  type Direction Expires = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser expiresParser header of
      OK lastModified "" -> Right lastModified
      OK _ rest -> Left $ "Unconsumed input after parsing Expires header: " <> show rest
      Fail -> Left "Failed to parse Expires header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderExpires


  headerName _ = hExpires


renderExpires :: Expires -> M.Builder
renderExpires (Expires time) = renderDate time


expiresParser :: ParserT st String Expires
expiresParser = Expires <$> dateParser
