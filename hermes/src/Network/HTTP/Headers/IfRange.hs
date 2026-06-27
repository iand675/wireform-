{- | The @If-Range@ precondition request header lets a client pair a @Range@
request with a single validator so the range is honored only while the
representation is unchanged. If the supplied entity-tag or HTTP-date still
matches, the server returns @206 (Partial Content)@ with just the requested
bytes; otherwise it ignores the @Range@ and returns the full @200 (OK)@
representation. This keeps an interrupted download consistent in one round trip.

@
If-Range = entity-tag / HTTP-date
@

Entity-tags are reused from "Network.HTTP.Headers.ETag" and HTTP-dates from
"Network.HTTP.Headers.Date".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-13.1.5>

See also: "Network.HTTP.Headers.Range", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfUnmodifiedSince".
-}
module Network.HTTP.Headers.IfRange (
  IfRange (..),
  ifRangeParser,
  renderIfRange,
) where

import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.ETag (EntityTag, entityTagParser, renderEntityTag)
import Network.HTTP.Headers.HeaderFieldName (hIfRange)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | The parsed value of an @If-Range@ header: either an entity-tag validator
or an HTTP-date validator.
-}
data IfRange
  = IfRangeETag EntityTag
  | IfRangeDate UTCTime
  deriving stock (Eq, Show)


instance KnownHeader IfRange where
  type ParseFailure IfRange = String
  type Cardinality IfRange = 'ZeroOrOne
  type Direction IfRange = 'Request


  parseFromHeaders _ headers = case runParser ifRangeParser $ NE.head headers of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing If-Range header: " <> show rest
    Fail -> Left "Failed to parse If-Range header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIfRange


  headerName _ = hIfRange


{- | Entity-tags begin with @"@ or the @W/@ marker, neither of which can start
an HTTP-date, so the entity-tag branch is tried first and falls through to
the date parser without ambiguity.
-}
ifRangeParser :: ParserT st String IfRange
ifRangeParser =
  (IfRangeETag <$> entityTagParser)
    <|> (IfRangeDate <$> dateParser)


renderIfRange :: IfRange -> M.Builder
renderIfRange = \case
  IfRangeETag tag -> renderEntityTag tag
  IfRangeDate t -> renderDate t
