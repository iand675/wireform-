{- |
RFC 8607 @Cal-Managed-ID@ — identifies a managed attachment in the CalDAV
Managed Attachments extension. The server assigns an opaque managed-id when
an attachment is added; clients echo it to reference, update, or remove the
attachment.

The value is server-defined and opaque, so it is preserved verbatim as a raw
'ST.ShortText' rather than having any structure imposed on it.

<https://www.rfc-editor.org/rfc/rfc8607 RFC 8607>

See also: "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.ScheduleReply", "Network.HTTP.Headers.CalDAVTimezones", "Network.HTTP.Headers.ContentDisposition", "Network.HTTP.Headers.DAV".
-}
module Network.HTTP.Headers.CalManagedID (
  CalManagedID (..),
  calManagedIDParser,
  renderCalManagedID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCalManagedID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | An opaque managed-attachment identifier, preserved verbatim.
newtype CalManagedID = CalManagedID {calManagedID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader CalManagedID where
  type ParseFailure CalManagedID = String
  type Cardinality CalManagedID = 'ZeroOrOne
  type Direction CalManagedID = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser calManagedIDParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cal-Managed-ID header: " <> show rest
    Fail -> Left "Failed to parse Cal-Managed-ID header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCalManagedID


  headerName _ = hCalManagedID


calManagedIDParser :: ParserT st String CalManagedID
calManagedIDParser = CalManagedID <$> takeRestShortText


renderCalManagedID :: CalManagedID -> M.Builder
renderCalManagedID = shortText . calManagedID
