{- |
@OData-EntityId@ — a response header returned for a create or update request
that completes with @204 No Content@, carrying the entity-id (an IRI) of the
affected entity so the client can reference it without a representation body.

== Grammar

@
OData-EntityId = IRI
@

The value is an IRI (per RFC 3987). It has no further fixed internal
structure that is useful to surface here, so the raw value is preserved
verbatim as text.

Spec: OData Version 4.01 Part 1: Protocol §8.3.3
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderODataEntityId>

See also: "Network.HTTP.Headers.ODataVersion", "Network.HTTP.Headers.ODataMaxVersion",
"Network.HTTP.Headers.PreferenceApplied", "Network.HTTP.Headers.Location".
-}
module Network.HTTP.Headers.ODataEntityId (
  ODataEntityId (..),
  oDataEntityIdParser,
  renderODataEntityId,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataEntityId)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | The entity-id (an IRI) carried by an @OData-EntityId@ response header.
newtype ODataEntityId = ODataEntityId {oDataEntityId :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader ODataEntityId where
  type ParseFailure ODataEntityId = String
  type Cardinality ODataEntityId = 'ZeroOrOne
  type Direction ODataEntityId = 'Response


  parseFromHeaders _ headers = case runParser oDataEntityIdParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing OData-EntityId header: " <> show rest
    Fail -> Left "Failed to parse OData-EntityId header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderODataEntityId


  headerName _ = hODataEntityId


oDataEntityIdParser :: ParserT st String ODataEntityId
oDataEntityIdParser = ODataEntityId <$> takeRestShortText


renderODataEntityId :: ODataEntityId -> M.Builder
renderODataEntityId (ODataEntityId iri) = shortText iri
