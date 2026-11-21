{- |
@OData-Isolation@ — a request header by which a client specifies the
isolation of the current request from external changes. The only value
defined by the specification is @snapshot@ (snapshot isolation).

== Grammar

@
OData-Isolation = "snapshot"
@

The @snapshot@ literal is case-insensitive. Other isolation tokens are
retained so the recipient can handle an unsupported isolation level.

Spec: OData Version 4.01 Part 1: Protocol §8.2.6
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderIsolationODataIsolation>

See also: "Network.HTTP.Headers.Isolation", "Network.HTTP.Headers.ODataVersion",
"Network.HTTP.Headers.ODataMaxVersion", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.ODataIsolation (
  ODataIsolation (..),
  oDataIsolationParser,
  renderODataIsolation,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hODataIsolation)
import qualified Network.HTTP.Headers.Isolation as Isolation
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


  parseFromHeaders _ ("snapshot" :| []) = Right Snapshot
  parseFromHeaders _ fields = parseSingleFieldLine "OData-Isolation" oDataIsolationParser fields


  renderToHeaders _ Snapshot = "snapshot"
  renderToHeaders _ value = M.toStrictByteString (renderODataIsolation value)


  renderToHeadersChecked _ Snapshot = Just "snapshot"
  renderToHeadersChecked _ (ODataIsolationOther token)
    | isHttpTokenText token = Just (ST.toByteString token)
    | otherwise = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hODataIsolation


oDataIsolationParser :: ParserT st String ODataIsolation
oDataIsolationParser = fromIsolation <$> Isolation.isolationParser
  where
    fromIsolation Isolation.Snapshot = Snapshot
    fromIsolation (Isolation.IsolationOther token) = ODataIsolationOther token


renderODataIsolation :: ODataIsolation -> M.Builder
renderODataIsolation = \case
  Snapshot -> "snapshot"
  ODataIsolationOther t -> shortText t
