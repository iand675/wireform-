{- |
@Origin-Agent-Cluster@ response header — requests that the document be
placed in an origin-keyed (rather than site-keyed) agent cluster, opting
out of synchronous cross-origin scripting via @document.domain@ and
giving the user agent latitude to isolate the origin in its own process.

The value is a Structured Fields Item with a Boolean bare value.
@?1@ requests origin-keyed clustering. @?0@ expresses a false value.
Any Item parameters are validated and ignored.

@
Origin-Agent-Cluster = sf-boolean   ; \"?1\" / \"?0\"
@

Spec: WHATWG HTML, <https://html.spec.whatwg.org/multipage/browsers.html#origin-keyed-agent-clusters>.

See also: "Network.HTTP.Headers.CrossOriginOpenerPolicy",
"Network.HTTP.Headers.CrossOriginEmbedderPolicy",
"Network.HTTP.Headers.CrossOriginResourcePolicy",
"Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.OriginAgentCluster (
  OriginAgentCluster (..),
  originAgentClusterParser,
  renderOriginAgentCluster,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOriginAgentCluster)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @Origin-Agent-Cluster@ value.
data OriginAgentCluster
  = -- | @?1@ — request an origin-keyed agent cluster.
    OriginAgentClusterEnabled
  | -- | @?0@ — the field carries a false value.
    OriginAgentClusterDisabled
  deriving stock (Eq, Show)


instance KnownHeader OriginAgentCluster where
  type ParseFailure OriginAgentCluster = String
  type Cardinality OriginAgentCluster = 'ZeroOrOne
  type Direction OriginAgentCluster = 'Response


  parseFromHeaders _ (wire :| [])
    | BS.length wire == 2
    , BSU.unsafeIndex wire 0 == 0x3f
    , let value = BSU.unsafeIndex wire 1
    , value == 0x30 || value == 0x31 =
        Right (if value == 0x31 then OriginAgentClusterEnabled else OriginAgentClusterDisabled)
  parseFromHeaders _ fields = parseSingleFieldLine "Origin-Agent-Cluster" originAgentClusterParser fields


  renderToHeaders _ = agentClusterBytes


  headerName _ = hOriginAgentCluster


originAgentClusterParser :: ParserT st String OriginAgentCluster
originAgentClusterParser = do
  enabled <- structuredFieldBooleanItem
  pure (if enabled then OriginAgentClusterEnabled else OriginAgentClusterDisabled)


renderOriginAgentCluster :: OriginAgentCluster -> M.Builder
renderOriginAgentCluster = M.byteString . agentClusterBytes


agentClusterBytes :: OriginAgentCluster -> ByteString
agentClusterBytes = \case
  OriginAgentClusterEnabled -> "?1"
  OriginAgentClusterDisabled -> "?0"
