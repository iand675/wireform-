{- |
@Content-ID@ — historic entity-header. As registered for HTTP (RFC 4229
§2.1.24) it originates in the W3C HTTP Distribution and Replication Protocol
(DRP), where it carries a content identifier used to fetch the correct version
of a file (typically a checksum URN such as @urn:md5:PEFjWBDv\/sd9alS9BYuX0w==@).
It is syntactically modelled on the MIME / RFC 2045 @Content-ID@ (a @msg-id@
in angle brackets), which is also seen on the wire.

== Value

A single opaque content identifier. Because the on-the-wire form is
host/profile specific (DRP checksum URN vs. MIME @\<addr-spec\>@), per the depth
guidance this is a faithful raw-preserving newtype: the identifier is captured
verbatim and re-emitted unchanged.

Spec: <https://www.w3.org/TR/NOTE-drp-19970825> (W3C DRP Note; HTTP registration RFC 4229 §2.1.24).

See also: "Network.HTTP.Headers.DifferentialID", "Network.HTTP.Headers.ContentMD5", "Network.HTTP.Headers.ContentLocation".
-}
module Network.HTTP.Headers.ContentID (
  ContentID (..),
  contentIDParser,
  renderContentID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Content-ID@ value: the opaque content identifier, preserved verbatim.
newtype ContentID = ContentID {contentID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ContentID where
  type ParseFailure ContentID = String
  type Cardinality ContentID = 'ZeroOrOne
  type Direction ContentID = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser contentIDParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Content-ID header: " <> show rest
    Fail -> Left "Failed to parse Content-ID header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentID


  headerName _ = hContentID


contentIDParser :: ParserT st String ContentID
contentIDParser = ContentID <$> takeRestShortText


renderContentID :: ContentID -> M.Builder
renderContentID (ContentID ident) = shortText ident
