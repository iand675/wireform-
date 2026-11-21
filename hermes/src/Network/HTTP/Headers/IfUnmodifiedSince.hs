{- | The @If-Unmodified-Since@ precondition request header makes a method
conditional on the selected representation /not/ having changed since the given
HTTP-date. If it has been modified, the origin server responds
@412 (Precondition Failed)@ and does not apply the request. It is the
date-based analogue of @If-Match@, used to guard unsafe methods such as @PUT@
against lost updates when only a @Last-Modified@ timestamp is available.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.4>

See also: "Network.HTTP.Headers.IfModifiedSince", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.IfUnmodifiedSince where

import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate, renderDateBytesChecked)
import Network.HTTP.Headers.HeaderFieldName (hIfUnmodifiedSince)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype IfUnmodifiedSince = IfUnmodifiedSince {ifUnmodifiedSince :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader IfUnmodifiedSince where
  type ParseFailure IfUnmodifiedSince = String
  type Cardinality IfUnmodifiedSince = 'ZeroOrOne
  type Direction IfUnmodifiedSince = 'Request


  parseFromHeaders settings = parseSingleFieldLine "If-Unmodified-Since" (IfUnmodifiedSince <$> dateParserWith settings)


  renderToHeaders _ = M.toStrictByteString . renderIfUnmodifiedSince


  renderToHeadersChecked _ = renderDateBytesChecked . ifUnmodifiedSince


  headerName _ = hIfUnmodifiedSince


renderIfUnmodifiedSince :: IfUnmodifiedSince -> M.Builder
renderIfUnmodifiedSince (IfUnmodifiedSince time) = renderDate time


ifUnmodifiedSinceParser :: ParserT st String IfUnmodifiedSince
ifUnmodifiedSinceParser = IfUnmodifiedSince <$> dateParser
