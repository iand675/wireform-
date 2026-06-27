{- |
@Differential-ID@ — historic entity-header from the W3C HTTP Distribution and
Replication Protocol (DRP, registered for HTTP in RFC 4229 §2.1.43). It carries
the content identifier of the file a client already holds, so the server can
return a differential (delta) update; the value is a checksum URN such as
@urn:md5:3FS2oCnWPZptpN05oKBemA==@.

== Value

A single opaque content identifier. Per the depth guidance for
obsolete/host-specific values this is a faithful raw-preserving newtype: the
identifier is captured verbatim and re-emitted unchanged.

Spec: <https://www.w3.org/TR/NOTE-drp-19970825> (W3C DRP Note; HTTP registration RFC 4229 §2.1.43).

See also: "Network.HTTP.Headers.ContentID", "Network.HTTP.Headers.DeltaBase", "Network.HTTP.Headers.IM", "Network.HTTP.Headers.AIM".
-}
module Network.HTTP.Headers.DifferentialID (
  DifferentialID (..),
  differentialIDParser,
  renderDifferentialID,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hDifferentialID)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | @Differential-ID@ value: the opaque content identifier, preserved verbatim.
newtype DifferentialID = DifferentialID {differentialID :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader DifferentialID where
  type ParseFailure DifferentialID = String
  type Cardinality DifferentialID = 'ZeroOrOne
  type Direction DifferentialID = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser differentialIDParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Differential-ID header: " <> show rest
    Fail -> Left "Failed to parse Differential-ID header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderDifferentialID


  headerName _ = hDifferentialID


differentialIDParser :: ParserT st String DifferentialID
differentialIDParser = DifferentialID <$> takeRestShortText


renderDifferentialID :: DifferentialID -> M.Builder
renderDifferentialID (DifferentialID ident) = shortText ident
