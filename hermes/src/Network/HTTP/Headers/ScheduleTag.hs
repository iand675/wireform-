{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
RFC 6638 §8.2 @Schedule-Tag@ — the opaque scheduling tag of a calendar
object resource. The syntax is the quoted-string production referenced by
RFC 6638, and the wire body is retained so it can be sent back in
@If-Schedule-Tag-Match@.

<https://www.rfc-editor.org/rfc/rfc6638#section-8.2 RFC 6638 §8.2>

See also: "Network.HTTP.Headers.IfScheduleTagMatch", "Network.HTTP.Headers.ScheduleReply".
-}
module Network.HTTP.Headers.ScheduleTag (
  ScheduleTag (ScheduleTag, ScheduleTagBytes, scheduleTag, scheduleTagBytes),
  scheduleTagParser,
  renderScheduleTag,
) where

import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hScheduleTag)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The bytes between the quotes, including any quoted pairs.
newtype ScheduleTag = ScheduleTagBytes {scheduleTagBytes :: SBS.ShortByteString}
  deriving stock (Eq, Show)


{- | The original text interface, constructing a tag from a UTF-8 wire
body. The accessor uses Latin-1 for bodies that are invalid UTF-8. Use
'scheduleTagBytes' for byte equality or forwarding.
-}
pattern ScheduleTag :: ST.ShortText -> ScheduleTag
pattern ScheduleTag {scheduleTag} <- ScheduleTagBytes (tagText -> scheduleTag)
  where
    ScheduleTag text = ScheduleTagBytes (ST.toShortByteString text)


{-# COMPLETE ScheduleTag #-}


tagText :: SBS.ShortByteString -> ST.ShortText
tagText bytes =
  maybe (ST.fromText (TE.decodeLatin1 (SBS.fromShort bytes))) id (ST.fromShortByteString bytes)


instance KnownHeader ScheduleTag where
  type ParseFailure ScheduleTag = String
  type Cardinality ScheduleTag = 'ZeroOrOne
  type Direction ScheduleTag = 'Response


  parseFromHeaders _ = parseQuotedWireFieldLine "Schedule-Tag" (ScheduleTagBytes . SBS.toShort)
  renderToHeaders _ = M.toStrictByteString . renderScheduleTag
  headerName _ = hScheduleTag


scheduleTagParser :: ParserT st String ScheduleTag
scheduleTagParser = ScheduleTagBytes . SBS.toShort <$> httpQuotedWireBytes


renderScheduleTag :: ScheduleTag -> M.Builder
renderScheduleTag (ScheduleTagBytes bytes) = M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'
