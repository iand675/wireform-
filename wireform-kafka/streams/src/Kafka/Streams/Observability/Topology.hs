{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Observability.Topology
Description : Topology DAG as structured JSON

Riffle Phase 1 §6: emit the validated 'Topology' graph as a
machine-readable JSON document suitable for:

  * a Flink-style web UI overlay (topology view with live
    overlays — see 'liveTopologyDescription' for the metrics-
    enriched variant);
  * CI golden-file comparison ("this topology shape didn't
    change");
  * downstream tooling that wants to walk the graph without
    touching the in-memory 'Topology' value.

The schema is versioned via the @"version"@ key so callers can
gate parsing on a known shape. The current value is @1@; any
backwards-incompatible change MUST bump it.
-}
module Kafka.Streams.Observability.Topology (
  -- * Structural rendering
  topologyDescription,
  topologyDescriptionWith,
  RenderConfig (..),
  defaultRenderConfig,

  -- * Live overlay

  --
  -- A topology paired with per-node counters from the engine's
  -- 'Kafka.Streams.Metrics.MetricsRegistry'. Lets a UI show
  -- "live throughput / lag / error count" overlays on the DAG.
  liveTopologyDescription,
) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Kafka.Streams.Consumed qualified as Consumed
import Kafka.Streams.Metrics (DurationStats (..), MetricValue (..))
import Kafka.Streams.Metrics qualified as Met
import Kafka.Streams.State.Store (StoreName)
import Kafka.Streams.State.Store qualified as Store
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (NodeName, TopicName, unNodeName, unTopicName)


----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

{- | Rendering knobs for 'topologyDescriptionWith'. Mostly there
so the live-metrics overlay can pass through an
@applicationId@ and any UI-specific tags without a positional-
arg explosion.
-}
data RenderConfig = RenderConfig
  { renderApplicationId :: !(Maybe Text)
  {- ^ Optional @applicationId@ stamped onto the root object.
  The JVM TopologyDescription includes this; the in-process
  'Topology' value doesn't carry it, so callers supply it
  here.
  -}
  , renderIncludeOrder :: !Bool
  {- ^ Include the @"insertionOrder"@ field listing nodes in
  their definition order. Useful for golden-file tests.
  Default: 'True'.
  -}
  }
  deriving stock (Eq, Show)


defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { renderApplicationId = Nothing
    , renderIncludeOrder = True
    }


----------------------------------------------------------------------
-- Public surface
----------------------------------------------------------------------

-- | Render a 'Topo.Topology' as a structured JSON value.
topologyDescription :: Topo.Topology -> A.Value
topologyDescription = topologyDescriptionWith defaultRenderConfig


-- | 'topologyDescription' with explicit rendering knobs.
topologyDescriptionWith :: RenderConfig -> Topo.Topology -> A.Value
topologyDescriptionWith cfg topo =
  A.Object
    $ maybe id (KM.insert "applicationId" . A.String) (renderApplicationId cfg)
    $ ( if renderIncludeOrder cfg
          then KM.insert "insertionOrder" insertionOrderArr
          else id
      )
    $ KM.fromList
      [ ("version", A.Number 1)
      , ("sources", renderSources topo)
      , ("processors", renderProcessors topo)
      , ("sinks", renderSinks topo)
      , ("stores", renderStores topo)
      , ("edges", renderEdges topo)
      ]
  where
    insertionOrderArr =
      A.Array $
        V.fromList
          ( map
              (A.String . unNodeName)
              (foldr (:) [] (Topo.topoOrder topo))
          )


{- | Render the topology with a snapshot of @metrics@
('Kafka.Streams.Runtime.streamsMetrics', or 'Met.registrySource')
under a top-level @"metrics"@ object.
-}
liveTopologyDescription
  :: Topo.Topology
  -> Met.MetricsSource
  -> RenderConfig
  -> IO A.Value
liveTopologyDescription topo metrics cfg = do
  dump <- metrics
  let base = topologyDescriptionWith cfg topo
      metricsObj =
        A.Object
          ( KM.fromList
              ( map
                  (\(k, v) -> (AK.fromText k, renderMetricValue v))
                  (Map.toList dump)
              )
          )
  pure $ case base of
    A.Object km -> A.Object (KM.insert "metrics" metricsObj km)
    other -> other


renderMetricValue :: MetricValue -> A.Value
renderMetricValue = \case
  MVCounter n -> A.Number (fromIntegral n)
  MVGauge d -> A.Number (realToFrac d)
  MVDuration s ->
    A.object
      -- Hand-rolled object so we don't need a ToJSON instance on
      -- DurationStats over in Kafka.Streams.Metrics (which would
      -- be an orphan-instance smell in this module).
      [ "count" A..= dsCount s
      , "sum" A..= dsSum s
      , "min" A..= dsMin s
      , "max" A..= dsMax s
      ]


----------------------------------------------------------------------
-- Section renderers
----------------------------------------------------------------------

renderSources :: Topo.Topology -> A.Value
renderSources topo =
  A.Array $
    V.fromList
      [ A.Object
          ( KM.fromList
              [ ("id", A.String (unNodeName (Topo.sourceName s)))
              ,
                ( "topics"
                , A.Array
                    ( V.fromList
                        ( map
                            (A.String . unTopicName)
                            (Topo.sourceTopics s)
                        )
                    )
                )
              , ("pattern", maybe A.Null A.String (Topo.sourcePattern s))
              , ("offsetReset", A.String (offsetResetText (Topo.sourceOffsetReset s)))
              , ("outputs", childList topo (Topo.sourceName s))
              ]
          )
      | (_, s) <- Map.toList (Topo.topoSources topo)
      ]


renderProcessors :: Topo.Topology -> A.Value
renderProcessors topo =
  A.Array $
    V.fromList
      [ A.Object
          ( KM.fromList
              [ ("id", A.String (unNodeName (Topo.processorSpecName p)))
              ,
                ( "inputs"
                , A.Array
                    ( V.fromList
                        ( map
                            (A.String . unNodeName)
                            (Topo.processorSpecParents p)
                        )
                    )
                )
              , ("outputs", childList topo (Topo.processorSpecName p))
              ,
                ( "stores"
                , A.Array
                    ( V.fromList
                        ( map
                            (A.String . Store.unStoreName)
                            (Topo.processorSpecStores p)
                        )
                    )
                )
              ]
          )
      | (_, p) <- Map.toList (Topo.topoProcessors topo)
      ]


renderSinks :: Topo.Topology -> A.Value
renderSinks topo =
  A.Array $
    V.fromList
      [ A.Object
          ( KM.fromList
              [ ("id", A.String (unNodeName (Topo.sinkName s)))
              ,
                ( "inputs"
                , A.Array
                    ( V.fromList
                        ( map
                            (A.String . unNodeName)
                            (Topo.sinkParents s)
                        )
                    )
                )
              , ("topic", A.String (unTopicName (Topo.sinkTopic s)))
              ]
          )
      | (_, s) <- Map.toList (Topo.topoSinks topo)
      ]


renderStores :: Topo.Topology -> A.Value
renderStores topo =
  A.Array $
    V.fromList
      [ A.Object
          ( KM.fromList
              [ ("name", A.String (Store.unStoreName name))
              , ("kind", A.String (storeKind builder))
              , ("loggingEnabled", A.Bool (builderLoggingEnabled builder))
              ,
                ( "changelogTopic"
                , maybe
                    A.Null
                    (A.String . unTopicName)
                    (effectiveChangelog topo name builder)
                )
              ,
                ( "owners"
                , A.Array
                    ( V.fromList
                        ( map
                            (A.String . unNodeName)
                            (Map.findWithDefault [] name (Topo.topoStoreOwners topo))
                        )
                    )
                )
              ,
                ( "global"
                , A.Bool (Set.member name (Topo.topoGlobalStores topo))
                )
              ]
          )
      | (name, builder) <- Map.toList (Topo.topoStores topo)
      ]


renderEdges :: Topo.Topology -> A.Value
renderEdges topo =
  A.Array $
    V.fromList $
      -- Walk every node in insertion order and emit one edge per
      -- (node, child) pair. Stable ordering keeps golden-file
      -- diffs deterministic.
      let nodes = foldr (:) [] (Topo.topoOrder topo)
      in concatMap
           ( \from ->
               map
                 ( \to ->
                     A.Object
                       ( KM.fromList
                           [ ("from", A.String (unNodeName from))
                           , ("to", A.String (unNodeName to))
                           ]
                       )
                 )
                 (Topo.childrenOf topo from)
           )
           nodes


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

childList :: Topo.Topology -> NodeName -> A.Value
childList topo nm =
  A.Array $
    V.fromList
      (map (A.String . unNodeName) (Topo.childrenOf topo nm))


storeKind :: Topo.AnyStoreBuilder -> Text
storeKind = \case
  Topo.AsKeyValueBuilder _ -> "keyValue"
  Topo.AsWindowBuilder _ -> "window"
  Topo.AsSessionBuilder _ -> "session"
  Topo.AsRawBuilder _ -> "raw"


builderLoggingEnabled :: Topo.AnyStoreBuilder -> Bool
builderLoggingEnabled = \case
  Topo.AsKeyValueBuilder b -> Store.loggingEnabled (Store.sbKvLogging b)
  Topo.AsWindowBuilder b -> Store.loggingEnabled (Store.sbWLogging b)
  Topo.AsSessionBuilder b -> Store.loggingEnabled (Store.sbSLogging b)
  Topo.AsRawBuilder b -> Store.loggingEnabled (Store.sbLogging b)


effectiveChangelog
  :: Topo.Topology -> StoreName -> Topo.AnyStoreBuilder -> Maybe TopicName
effectiveChangelog topo nm builder
  | not (builderLoggingEnabled builder) = Nothing
  | otherwise = Topo.effectiveChangelogReuse topo nm


offsetResetText :: Consumed.AutoOffsetReset -> Text
offsetResetText Consumed.OffsetEarliest = "earliest"
offsetResetText Consumed.OffsetLatest = "latest"
offsetResetText Consumed.OffsetNone = "none"
