{- |
Module      : Network.HTTP.Headers.RepeatabilityFirstSent
Description : Representation of the @Repeatability-First-Sent@ request header field

Part of the OData repeatable-requests protocol. @Repeatability-First-Sent@
is a request header recording the time at which a client first attempted to
send a repeatable request. A server combines it with the request's
'Network.HTTP.Headers.RepeatabilityRequestID.RepeatabilityRequestID' to bound how
long it must remember the outcome of a request for de-duplication purposes.

@
  Repeatability-First-Sent = HTTP-date   ; IMF-fixdate, e.g. "Tue, 28 Feb 2012 12:34:56 GMT"
@

Senders emit IMF-fixdate. The shared "Network.HTTP.Headers.Date" parser also
accepts the two obsolete HTTP-date forms on receipt, per RFC 9110 §5.6.7.

Spec: <https://docs.oasis-open.org/odata/repeatable-requests/v1.0/repeatable-requests-v1.0.html>
(OASIS OData Repeatable Requests Version 1.0, §3.1.2).

See also: "Network.HTTP.Headers.RepeatabilityRequestID", "Network.HTTP.Headers.RepeatabilityClientID",
"Network.HTTP.Headers.RepeatabilityResult", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.RepeatabilityFirstSent (
  RepeatabilityFirstSent (..),
  repeatabilityFirstSentParser,
  renderRepeatabilityFirstSent,
) where

import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (Date (..), dateParser, dateParserWith, renderDate, renderDateBytesChecked)
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityFirstSent)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Repeatability-First-Sent@ value: the HTTP-date of the first send attempt.
newtype RepeatabilityFirstSent = RepeatabilityFirstSent {repeatabilityFirstSent :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader RepeatabilityFirstSent where
  type ParseFailure RepeatabilityFirstSent = String
  type Cardinality RepeatabilityFirstSent = 'ZeroOrOne
  type Direction RepeatabilityFirstSent = 'Request


  parseFromHeaders settings = parseSingleFieldLine "Repeatability-First-Sent" (RepeatabilityFirstSent <$> dateParserWith settings)


  renderToHeaders settings = renderToHeaders settings . Date . repeatabilityFirstSent


  renderToHeadersChecked _ = renderDateBytesChecked . repeatabilityFirstSent


  headerName _ = hRepeatabilityFirstSent


repeatabilityFirstSentParser :: ParserT st String RepeatabilityFirstSent
repeatabilityFirstSentParser = RepeatabilityFirstSent <$> dateParser


renderRepeatabilityFirstSent :: RepeatabilityFirstSent -> M.Builder
renderRepeatabilityFirstSent (RepeatabilityFirstSent time) = renderDate time
