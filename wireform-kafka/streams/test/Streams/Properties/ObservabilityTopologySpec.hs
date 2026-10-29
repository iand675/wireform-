{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Streams.Properties.ObservabilityTopologySpec
Description : Tests for the topology DAG JSON renderer

Covers:

  * Source / processor / sink / store sections are present
    and structurally well-formed.
  * Edges agree with 'Topo.childrenOf' for every node.
  * 'insertionOrder' tracks topology mutation order.
  * 'applicationId' is stamped when supplied via 'RenderConfig'.
  * Live metrics overlay surfaces non-zero counters.
-}
module Streams.Properties.ObservabilityTopologySpec (tests) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Streams.Imperative
import Kafka.Streams.Metrics qualified as Met
import Kafka.Streams.Observability.Topology qualified as Obs
import Kafka.Streams.Topology qualified as Topo
import Test.Syd


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

passthroughTopology :: IO Topo.Topology
passthroughTopology = do
  b <- newStreamsBuilder
  s <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde textSerde)
  s' <- mapValues T.toUpper s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  buildTopology b


{- | Read an object's field, asserting the parent is indeed an
object.
-}
field :: T.Text -> A.Value -> A.Value
field nm = \case
  A.Object km ->
    KM.lookup (AK.fromText nm) km
      `or'` error ("missing field: " <> T.unpack nm)
  _ -> error ("not an object when looking up " <> T.unpack nm)
  where
    or' Nothing x = x
    or' (Just v) _ = v


arrayLen :: A.Value -> Int
arrayLen (A.Array v) = V.length v
arrayLen _ = -1


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Observability.Topology" $
    sequence_
      [ structural_render_has_all_sections
      , insertion_order_tracks_addition
      , application_id_is_stamped
      , edges_agree_with_childrenOf
      , live_metrics_overlay_surfaces_counters
      ]


----------------------------------------------------------------------

structural_render_has_all_sections :: Spec
structural_render_has_all_sections =
  it "topologyDescription emits version + sources/processors/sinks/stores/edges/insertionOrder" $ do
    topo <- passthroughTopology
    let v = Obs.topologyDescription topo
    field "version" v `shouldBe` A.Number 1
    arrayLen (field "sources" v) `shouldBe` 1 -- streamFromTopic
    -- processors: one for mapValues, one for `to` (sink-aware internal)
    (arrayLen (field "processors" v) >= 1) `shouldBe` True
    arrayLen (field "sinks" v) `shouldBe` 1
    -- stores: zero in this passthrough
    arrayLen (field "stores" v) `shouldBe` 0
    (arrayLen (field "edges" v) >= 1) `shouldBe` True
    -- insertionOrder records every node
    (arrayLen (field "insertionOrder" v) >= 2) `shouldBe` True


insertion_order_tracks_addition :: Spec
insertion_order_tracks_addition =
  it "insertionOrder reflects the order nodes were added" $ do
    topo <- passthroughTopology
    let v = Obs.topologyDescription topo
        order = field "insertionOrder" v
    case order of
      A.Array ns -> do
        -- First node is the source (whichever name was given);
        -- last node is the sink.
        let firstName =
              case V.head ns of
                A.String s -> s
                _ -> error "non-string in insertionOrder"
            lastName =
              case V.last ns of
                A.String s -> s
                _ -> error "non-string in insertionOrder"
        ( if (T.isPrefixOf "KSTREAM-SOURCE" firstName)
            then pure ()
            else
              expectationFailure
                ( "expected first node to be a KSTREAM-SOURCE; got "
                    <> T.unpack firstName
                )
          )
        ( if (T.isPrefixOf "KSTREAM-SINK" lastName)
            then pure ()
            else
              expectationFailure
                ( "expected last node to be a KSTREAM-SINK; got "
                    <> T.unpack lastName
                )
          )
      _ -> error "insertionOrder not an array"


application_id_is_stamped :: Spec
application_id_is_stamped =
  it "applicationId from RenderConfig appears on the root" $ do
    topo <- passthroughTopology
    let v =
          Obs.topologyDescriptionWith
            ( Obs.defaultRenderConfig
                { Obs.renderApplicationId = Just "my-app"
                }
            )
            topo
    field "applicationId" v `shouldBe` A.String "my-app"


edges_agree_with_childrenOf :: Spec
edges_agree_with_childrenOf =
  it "every JSON edge matches Topo.childrenOf" $ do
    topo <- passthroughTopology
    let v = Obs.topologyDescription topo
    case field "edges" v of
      A.Array es -> mapM_ check (V.toList es)
        where
          check edge =
            case edge of
              A.Object km -> do
                let fromV = KM.lookup "from" km
                    toV = KM.lookup "to" km
                case (fromV, toV) of
                  (Just (A.String f), Just (A.String t)) -> do
                    let fromNm = Topo.nodeName f
                        children = Topo.childrenOf topo fromNm
                        expected = Topo.nodeName t `elem` children
                    ( if (expected)
                        then pure ()
                        else
                          expectationFailure
                            ( "edge "
                                <> T.unpack f
                                <> " -> "
                                <> T.unpack t
                                <> " not present in childrenOf"
                            )
                      )
                  _ -> error "edge missing from/to"
              _ -> error "edge not an object"
      _ -> error "edges not an array"


live_metrics_overlay_surfaces_counters :: Spec
live_metrics_overlay_surfaces_counters =
  it "liveTopologyDescription includes recorded counters" $ do
    topo <- passthroughTopology
    metrics <- Met.newMetricsRegistry
    Met.incCounter metrics "test-counter"
    Met.incCounter metrics "test-counter"
    Met.addCounter metrics "another" 7
    v <- Obs.liveTopologyDescription topo (Met.registrySource metrics) Obs.defaultRenderConfig
    case field "metrics" v of
      A.Object km -> do
        KM.lookup "test-counter" km `shouldBe` Just (A.Number 2)
        KM.lookup "another" km `shouldBe` Just (A.Number 7)
      _ -> error "metrics not an object"
