{- |
RFC 6638 §8.3 @If-Schedule-Tag-Match@ — a conditional request header whose
value is a 'ScheduleTag'. A scheduling-aware write proceeds only if the
resource's current @Schedule-Tag@ matches the supplied value, allowing a
client to apply scheduling changes without clobbering unrelated edits.

== Grammar

@
If-Schedule-Tag-Match = Schedule-Tag
@

<https://www.rfc-editor.org/rfc/rfc6638#section-8.3 RFC 6638 §8.3>

See also: "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.ScheduleReply", "Network.HTTP.Headers.IfMatch", "Network.HTTP.Headers.IfNoneMatch", "Network.HTTP.Headers.ETag".
-}
module Network.HTTP.Headers.IfScheduleTagMatch (
  IfScheduleTagMatch (..),
  ifScheduleTagMatchParser,
  renderIfScheduleTagMatch,
) where

import qualified Data.ByteString.Short as SBS
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIfScheduleTagMatch)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.ScheduleTag (ScheduleTag (ScheduleTagBytes), renderScheduleTag, scheduleTagParser)


-- | An @If-Schedule-Tag-Match@ precondition carrying a single 'ScheduleTag'.
newtype IfScheduleTagMatch = IfScheduleTagMatch {ifScheduleTagMatch :: ScheduleTag}
  deriving stock (Eq, Show)


instance KnownHeader IfScheduleTagMatch where
  type ParseFailure IfScheduleTagMatch = String
  type Cardinality IfScheduleTagMatch = 'ZeroOrOne
  type Direction IfScheduleTagMatch = 'Request


  parseFromHeaders _ = parseQuotedWireFieldLine "If-Schedule-Tag-Match" (IfScheduleTagMatch . ScheduleTagBytes . SBS.toShort)


  renderToHeaders _ = M.toStrictByteString . renderIfScheduleTagMatch


  headerName _ = hIfScheduleTagMatch


ifScheduleTagMatchParser :: ParserT st String IfScheduleTagMatch
ifScheduleTagMatchParser = IfScheduleTagMatch <$> scheduleTagParser


renderIfScheduleTagMatch :: IfScheduleTagMatch -> M.Builder
renderIfScheduleTagMatch = renderScheduleTag . ifScheduleTagMatch
