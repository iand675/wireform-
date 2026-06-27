{-# LANGUAGE TemplateHaskell #-}

{- |
@Origin-Agent-Cluster@ response header — requests that the document be
placed in an origin-keyed (rather than site-keyed) agent cluster, opting
out of synchronous cross-origin scripting via @document.domain@ and
giving the user agent latitude to isolate the origin in its own process.

The value is an RFC 8941 structured-field boolean: @?1@ (origin-keyed)
or @?0@ (site-keyed).

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

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOriginAgentCluster)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A parsed @Origin-Agent-Cluster@ value.
data OriginAgentCluster
  = -- | @?1@ — request an origin-keyed agent cluster.
    OriginAgentClusterEnabled
  | -- | @?0@ — request a site-keyed agent cluster.
    OriginAgentClusterDisabled
  deriving stock (Eq, Show)


instance KnownHeader OriginAgentCluster where
  type ParseFailure OriginAgentCluster = String
  type Cardinality OriginAgentCluster = 'ZeroOrOne
  type Direction OriginAgentCluster = 'Response


  parseFromHeaders _ headers = case runParser originAgentClusterParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Origin-Agent-Cluster header: " <> show rest
    Fail -> Left "Failed to parse Origin-Agent-Cluster header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderOriginAgentCluster


  headerName _ = hOriginAgentCluster


originAgentClusterParser :: ParserT st String OriginAgentCluster
originAgentClusterParser =
  $( switch
      [|
        case _ of
          "?1" -> pure OriginAgentClusterEnabled
          "?0" -> pure OriginAgentClusterDisabled
        |]
   )


renderOriginAgentCluster :: OriginAgentCluster -> M.Builder
renderOriginAgentCluster = \case
  OriginAgentClusterEnabled -> "?1"
  OriginAgentClusterDisabled -> "?0"
