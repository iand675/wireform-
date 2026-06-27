{- | The @Last-Modified@ response header field gives the date and time at which the
origin server believes the selected representation was last changed, expressed
as an HTTP-date. It serves as a (usually weak) validator: clients store it and
replay it in @If-Modified-Since@ and @If-Unmodified-Since@ requests so caches
can be revalidated without refetching unchanged content.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.8.2>

See also: "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.IfModifiedSince", "Network.HTTP.Headers.IfUnmodifiedSince", "Network.HTTP.Headers.IfRange", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.LastModified (
  LastModified (..),
  lastModifiedParser,
  renderLastModified,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hLastModified)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype LastModified = LastModified {lastModified :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader LastModified where
  type ParseFailure LastModified = String
  type Cardinality LastModified = 'ZeroOrOne
  type Direction LastModified = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser lastModifiedParser header of
      OK lm "" -> Right lm
      OK _ rest -> Left $ "Unconsumed input after parsing Last-Modified header: " <> show rest
      Fail -> Left "Failed to parse Last-Modified header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderLastModified


  headerName _ = hLastModified


renderLastModified :: LastModified -> M.Builder
renderLastModified (LastModified time) = renderDate time


lastModifiedParser :: ParserT st String LastModified
lastModifiedParser = LastModified <$> dateParser
