{- | The @If-Modified-Since@ precondition request header makes a @GET@ or @HEAD@
conditional on the selected representation having changed since the given
HTTP-date, normally a previously received @Last-Modified@ value. If it has not
changed, the origin server responds @304 (Not Modified)@ with no body, saving
bandwidth on cache revalidation.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.3>

See also: "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.IfUnmodifiedSince", "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.IfModifiedSince (
  IfModifiedSince (..),
  renderIfModifiedSince,
  ifModifiedSinceParser,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hIfModifiedSince)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype IfModifiedSince = IfModifiedSince {ifModifiedSince :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader IfModifiedSince where
  type ParseFailure IfModifiedSince = String
  type Cardinality IfModifiedSince = 'ZeroOrOne
  type Direction IfModifiedSince = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser ifModifiedSinceParser header of
      OK ims "" -> Right ims
      OK _ rest -> Left $ "Unconsumed input after parsing If-Modified-Since header: " <> show rest
      Fail -> Left "Failed to parse If-Modified-Since header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIfModifiedSince


  headerName _ = hIfModifiedSince


renderIfModifiedSince :: IfModifiedSince -> M.Builder
renderIfModifiedSince (IfModifiedSince time) = renderDate time


ifModifiedSinceParser :: ParserT st String IfModifiedSince
ifModifiedSinceParser = IfModifiedSince <$> dateParser
