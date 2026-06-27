{- |
@Isolation@ — the de-facto (un-prefixed) spelling of OData's isolation
header, semantically identical to @OData-Isolation@. A request header by
which a client asks that the request be isolated from external changes; the
only value defined by the specification is @snapshot@ (snapshot isolation).

== Grammar

@
Isolation = "snapshot"
@

Surfaced as an enumeration with a forward-compatible fallback constructor
for any future isolation level token. The value is case-sensitive.

Spec: OData Version 4.01 Part 1: Protocol §8.2.5
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataIsolation>

See also: "Network.HTTP.Headers.ODataIsolation", "Network.HTTP.Headers.ODataVersion",
"Network.HTTP.Headers.ODataMaxVersion", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.Isolation (
  Isolation (..),
  isolationParser,
  renderIsolation,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIsolation)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The isolation level requested via the @Isolation@ header.
data Isolation
  = -- | @snapshot@ isolation.
    Snapshot
  | -- | A forward-compatible, not-yet-standardised isolation token.
    IsolationOther !ST.ShortText
  deriving stock (Eq, Show)


instance KnownHeader Isolation where
  type ParseFailure Isolation = String
  type Cardinality Isolation = 'ZeroOrOne
  type Direction Isolation = 'Request


  parseFromHeaders _ headers = case runParser isolationParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Isolation header: " <> show rest
    Fail -> Left "Failed to parse Isolation header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderIsolation


  headerName _ = hIsolation


isolationParser :: ParserT st String Isolation
isolationParser = classify <$> rfc9110Token
  where
    classify t
      | t == ST.fromString "snapshot" = Snapshot
      | otherwise = IsolationOther t


renderIsolation :: Isolation -> M.Builder
renderIsolation = \case
  Snapshot -> "snapshot"
  IsolationOther t -> shortText t
