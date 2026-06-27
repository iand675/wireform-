{- |
RFC 6638 §8.4 @If-Schedule-Tag-Match@ — a conditional request header whose
value is a 'ScheduleTag'. A scheduling-aware write proceeds only if the
resource's current @Schedule-Tag@ matches the supplied value, allowing a
client to apply scheduling changes without clobbering unrelated edits.

== Grammar

@
If-Schedule-Tag-Match = Schedule-Tag
@

<https://www.rfc-editor.org/rfc/rfc6638#section-8.4 RFC 6638 §8.4>

See also: "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.ScheduleReply", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.ETag".
-}
module Network.HTTP.Headers.IfScheduleTagMatch (
  IfScheduleTagMatch (..),
  ifScheduleTagMatchParser,
  renderIfScheduleTagMatch,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIfScheduleTagMatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.ScheduleTag (ScheduleTag, renderScheduleTag, scheduleTagParser)


-- | An @If-Schedule-Tag-Match@ precondition carrying a single 'ScheduleTag'.
newtype IfScheduleTagMatch = IfScheduleTagMatch {ifScheduleTagMatch :: ScheduleTag}
  deriving stock (Eq, Show)


instance KnownHeader IfScheduleTagMatch where
  type ParseFailure IfScheduleTagMatch = String
  type Cardinality IfScheduleTagMatch = 'ZeroOrOne
  type Direction IfScheduleTagMatch = 'Request


  parseFromHeaders _ headers = case runParser ifScheduleTagMatchParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing If-Schedule-Tag-Match header: " <> show rest
    Fail -> Left "Failed to parse If-Schedule-Tag-Match header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIfScheduleTagMatch


  headerName _ = hIfScheduleTagMatch


ifScheduleTagMatchParser :: ParserT st String IfScheduleTagMatch
ifScheduleTagMatchParser = IfScheduleTagMatch <$> scheduleTagParser


renderIfScheduleTagMatch :: IfScheduleTagMatch -> M.Builder
renderIfScheduleTagMatch = renderScheduleTag . ifScheduleTagMatch
