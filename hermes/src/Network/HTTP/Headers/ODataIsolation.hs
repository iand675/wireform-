{- |
@OData-Isolation@ — a request header by which a client specifies the
isolation of the current request from external changes. The only value
defined by the specification is @snapshot@ (snapshot isolation).

== Grammar

@
OData-Isolation = "snapshot"
@

Surfaced as an enumeration with a forward-compatible fallback constructor
for any future isolation level token. The value is case-sensitive.

Spec: OData Version 4.01 Part 1: Protocol §8.2.5
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataIsolation>

See also: "Network.HTTP.Headers.Isolation", "Network.HTTP.Headers.ODataVersion",
"Network.HTTP.Headers.ODataMaxVersion", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.ODataIsolation (
  ODataIsolation (..),
  oDataIsolationParser,
  renderODataIsolation,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataIsolation)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The isolation level requested via @OData-Isolation@.
data ODataIsolation
  = -- | @snapshot@ isolation.
    Snapshot
  | -- | A forward-compatible, not-yet-standardised isolation token.
    ODataIsolationOther !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader ODataIsolation where
  type ParseFailure ODataIsolation = String
  type Cardinality ODataIsolation = 'ZeroOrOne
  type Direction ODataIsolation = 'Request


  parseFromHeaders _ headers = case runParser oDataIsolationParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing OData-Isolation header: " <> show rest
    Fail -> Left "Failed to parse OData-Isolation header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderODataIsolation


  headerName _ = hODataIsolation


oDataIsolationParser :: ParserT st String ODataIsolation
oDataIsolationParser = classify <$> rfc9110Token
  where
    classify t
      | t == ST.fromString "snapshot" = Snapshot
      | otherwise = ODataIsolationOther t


renderODataIsolation :: ODataIsolation -> M.Builder
renderODataIsolation = \case
  Snapshot -> "snapshot"
  ODataIsolationOther t -> shortText t
