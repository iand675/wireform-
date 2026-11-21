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
  encodeScheduleTagChecked,
) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import Foreign.Ptr (plusPtr)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hScheduleTag)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Encode.Direct (dWord8, directEncode)


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
  renderToHeadersChecked _ = encodeScheduleTagChecked
  headerName _ = hScheduleTag


scheduleTagParser :: ParserT st String ScheduleTag
scheduleTagParser = ScheduleTagBytes . SBS.toShort <$> httpQuotedWireBytes


renderScheduleTag :: ScheduleTag -> M.Builder
renderScheduleTag (ScheduleTagBytes bytes) = M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'


-- | Quote a validated wire body. Existing quoted pairs retain their spelling.
encodeScheduleTagChecked :: ScheduleTag -> Maybe BSI.ByteString
encodeScheduleTagChecked (ScheduleTagBytes bytes)
  | valid 0 = Just $ directEncode (BSI.checkedAdd "Schedule-Tag" size 2) $ \ptr start -> do
      body <- dWord8 ptr start 34
      SBSI.copyToPtr bytes 0 (ptr `plusPtr` body) size
      dWord8 ptr (body + size) 34
  | otherwise = Nothing
  where
    size = SBS.length bytes
    valid offset
      | offset == size = True
      | otherwise = case SBSI.unsafeIndex bytes offset of
          34 -> False
          92 -> offset + 1 < size && isOpaqueByte (fromIntegral (SBSI.unsafeIndex bytes (offset + 1))) && valid (offset + 2)
          byte -> isOpaqueByte (fromIntegral byte) && valid (offset + 1)
