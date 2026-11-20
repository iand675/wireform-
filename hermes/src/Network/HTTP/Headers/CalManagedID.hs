{- |
RFC 8607 §5.1 @Cal-Managed-ID@ — the server-assigned identifier of a managed
attachment added or updated by a CalDAV POST request. Its value is an
iCalendar @paramtext@, preserved as UTF-8.

<https://www.rfc-editor.org/rfc/rfc8607.html#section-5.1 RFC 8607 §5.1>

See also: "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.ScheduleReply", "Network.HTTP.Headers.CalDAVTimezones", "Network.HTTP.Headers.ContentDisposition", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.CalManagedID (
  CalManagedID (..),
  calManagedIDParser,
  renderCalManagedID,
) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCalManagedID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Wireform.FFI (validateUtf8SWAR)


-- | An opaque managed-attachment identifier, preserved verbatim.
newtype CalManagedID = CalManagedID {calManagedID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader CalManagedID where
  type ParseFailure CalManagedID = String
  type Cardinality CalManagedID = 'ZeroOrOne
  type Direction CalManagedID = 'Response


  parseFromHeaders _ (wire :| [])
    | let body = BS.dropWhileEnd isFieldSpace (BS.dropWhile isFieldSpace wire)
    , BS.all isParamChar body =
        decodeID body
    | otherwise = Left "Invalid paramtext in Cal-Managed-ID"
  parseFromHeaders _ _ = Left "Multiple Cal-Managed-ID field lines"


  renderToHeaders _ = M.toStrictByteString . renderCalManagedID


  headerName _ = hCalManagedID


calManagedIDParser :: ParserT st String CalManagedID
calManagedIDParser =
  withByteString (skipMany paramChar) $ \() wire ->
    either err pure (decodeID (BS.dropWhileEnd isFieldSpace wire))
  where
    paramChar = withAnyWord8 $ \byte -> if isParamChar byte then pure () else failed


decodeID :: BS.ByteString -> Either String CalManagedID
decodeID body
  | validateUtf8SWAR body = Right $! CalManagedID (STU.fromByteStringUnsafe body)
  | otherwise = Left "Invalid UTF-8 in Cal-Managed-ID"


isFieldSpace :: Word8 -> Bool
isFieldSpace byte = byte == 9 || byte == 32
{-# INLINE isFieldSpace #-}


isParamChar :: Word8 -> Bool
isParamChar byte = byte == 9 || (byte >= 32 && byte /= 34 && byte /= 44 && byte /= 58 && byte /= 59 && byte /= 127)
{-# INLINE isParamChar #-}


renderCalManagedID :: CalManagedID -> M.Builder
renderCalManagedID = shortText . calManagedID
