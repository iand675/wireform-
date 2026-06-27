{- |
@OSLC-Core-Version@ — advertises the OSLC (Open Services for Lifecycle
Collaboration) Core protocol version in effect for a request or response,
e.g. @2.0@. Sent by both clients and servers.

The value is a short version token; it is preserved verbatim as a raw
'ST.ShortText' rather than decomposed into numeric components, since the OSLC
specifications do not constrain it to a fixed numeric shape.

<https://docs.oasis-open-projects.org/oslc-op/core/v3.0/oslc-core.html OSLC Core 3.0>

See also: "Network.HTTP.Headers.CalManagedID", "Network.HTTP.Headers.ScheduleTag", "Network.HTTP.Headers.ODataVersion", "Network.HTTP.Headers.ODataMaxVersion".
-}
module Network.HTTP.Headers.OSLCCoreVersion (
  OSLCCoreVersion (..),
  oslcCoreVersionParser,
  renderOSLCCoreVersion,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOSLCCoreVersion)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | An OSLC Core protocol version string (e.g. @2.0@), preserved verbatim.
newtype OSLCCoreVersion = OSLCCoreVersion {oslcCoreVersion :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader OSLCCoreVersion where
  type ParseFailure OSLCCoreVersion = String
  type Cardinality OSLCCoreVersion = 'ZeroOrOne
  type Direction OSLCCoreVersion = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser oslcCoreVersionParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing OSLC-Core-Version header: " <> show rest
    Fail -> Left "Failed to parse OSLC-Core-Version header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderOSLCCoreVersion


  headerName _ = hOSLCCoreVersion


oslcCoreVersionParser :: ParserT st String OSLCCoreVersion
oslcCoreVersionParser = OSLCCoreVersion <$> takeRestShortText


renderOSLCCoreVersion :: OSLCCoreVersion -> M.Builder
renderOSLCCoreVersion = shortText . oslcCoreVersion
