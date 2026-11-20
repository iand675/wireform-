{-# LANGUAGE TemplateHaskell #-}

{- |
@Isolation@ — the OData 4.01 spelling of the isolation
header, semantically identical to @OData-Isolation@. A request header by
which a client asks that the request be isolated from external changes; the
only value defined by the specification is @snapshot@ (snapshot isolation).

== Grammar

@
Isolation = "snapshot"
@

The @snapshot@ literal is case-insensitive. Other isolation tokens are
retained so the recipient can handle an unsupported isolation level.

Spec: OData Version 4.01 Part 1: Protocol §8.2.6
<https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_HeaderIsolationODataIsolation>

See also: "Network.HTTP.Headers.ODataIsolation", "Network.HTTP.Headers.ODataVersion",
"Network.HTTP.Headers.ODataMaxVersion", "Network.HTTP.Headers.Prefer".
-}
module Network.HTTP.Headers.Isolation (
  Isolation (..),
  isolationParser,
  renderIsolation,
) where

import Data.List.NonEmpty (NonEmpty (..))
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


  parseFromHeaders _ ("snapshot" :| []) = Right Snapshot
  parseFromHeaders _ fields = parseSingleFieldLine "Isolation" isolationParser fields


  renderToHeaders _ Snapshot = "snapshot"
  renderToHeaders _ value = M.toStrictByteString (renderIsolation value)


  headerName _ = hIsolation


isolationParser :: ParserT st String Isolation
isolationParser =
  ($(switchAsciiCI [|case _ of "snapshot" -> pure Snapshot|]) <* notFollowedBy (skipSatisfyAscii isTokenChar))
    <|> (IsolationOther <$> rfc9110Token)


renderIsolation :: Isolation -> M.Builder
renderIsolation = \case
  Snapshot -> "snapshot"
  IsolationOther t -> shortText t
