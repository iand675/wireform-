{- |
@Memento-Datetime@ is a response header sent by a /memento/ — a
resource that captures a prior fixed state of an original resource —
to convey the datetime at which the original resource held the content
now being returned. It is the core dating header of the Memento
framework (RFC 7089, "time travel for the Web") and is the
response-side counterpart to a client's @Accept-Datetime@ request.

== Grammar

@
Memento-Datetime = HTTP-date
@

The value is an HTTP-date (RFC 9110 §5.6.7), so we reuse the shared
'Network.HTTP.Headers.Date.dateParser' / 'Network.HTTP.Headers.Date.renderDate'
and surface the parsed instant as a 'UTCTime'.

Spec: <https://www.rfc-editor.org/rfc/rfc7089#section-2.1.1>

See also: "Network.HTTP.Headers.AcceptDatetime", "Network.HTTP.Headers.Date", "Network.HTTP.Headers.Link", "Network.HTTP.Headers.LastModified".
-}
module Network.HTTP.Headers.MementoDatetime (
  MementoDatetime (..),
  mementoDatetimeParser,
  renderMementoDatetime,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hMementoDatetime)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | @Memento-Datetime@ value: the HTTP-date the memento captures.
newtype MementoDatetime = MementoDatetime {mementoDatetimeTime :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader MementoDatetime where
  type ParseFailure MementoDatetime = String
  type Cardinality MementoDatetime = 'ZeroOrOne
  type Direction MementoDatetime = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser mementoDatetimeParser header of
      OK v "" -> Right v
      OK _ rest -> Left $ "Unconsumed input after parsing Memento-Datetime header: " <> show rest
      Fail -> Left "Failed to parse Memento-Datetime header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderMementoDatetime


  headerName _ = hMementoDatetime


mementoDatetimeParser :: ParserT st String MementoDatetime
mementoDatetimeParser = MementoDatetime <$> dateParser


renderMementoDatetime :: MementoDatetime -> M.Builder
renderMementoDatetime (MementoDatetime time) = renderDate time
