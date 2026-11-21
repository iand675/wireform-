{- |
RFC 7089 §2.1.1 @Accept-Datetime@ — request header used in datetime
negotiation (Memento time travel for the Web): the client expresses the
datetime of a desired prior state of a resource.

== Grammar

@
Accept-Datetime = HTTP-date
@

The value is an HTTP-date (RFC 9110 §5.6.7), so we reuse the shared
'Network.HTTP.Headers.Date.dateParser' / 'Network.HTTP.Headers.Date.renderDate'
and surface the parsed instant as a 'UTCTime'.

Spec: <https://www.rfc-editor.org/rfc/rfc7089#section-2.1.1>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.MementoDatetime", "Network.HTTP.Headers.Date", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptDatetime (
  AcceptDatetime (..),
  acceptDatetimeParser,
  renderAcceptDatetime,
) where

import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate, renderDateBytesChecked)
import Network.HTTP.Headers.HeaderFieldName (hAcceptDatetime)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Accept-Datetime@ value: the HTTP-date of the desired prior resource state.
newtype AcceptDatetime = AcceptDatetime {acceptDatetimeTime :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader AcceptDatetime where
  type ParseFailure AcceptDatetime = String
  type Cardinality AcceptDatetime = 'ZeroOrOne
  type Direction AcceptDatetime = 'Request


  parseFromHeaders settings = parseSingleFieldLine "Accept-Datetime" (AcceptDatetime <$> dateParserWith settings)


  renderToHeaders _ = M.toStrictByteString . renderAcceptDatetime


  renderToHeadersChecked _ = renderDateBytesChecked . acceptDatetimeTime


  headerName _ = hAcceptDatetime


acceptDatetimeParser :: ParserT st String AcceptDatetime
acceptDatetimeParser = AcceptDatetime <$> dateParser


renderAcceptDatetime :: AcceptDatetime -> M.Builder
renderAcceptDatetime (AcceptDatetime time) = renderDate time
