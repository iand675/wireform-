{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.IdiomaticPipeline
Description : Reusable composable fragments via 'F.Topology'

The other examples in this package mirror the JVM fluent-builder
shape one combinator at a time. This one shows the
"Haskell-native" complement: building reusable transformation
fragments as first-class 'F.Topology' values and composing them
with 'Control.Category.(>>>)'.

A 'F.Topology' is a 'Control.Category.Category' so two fragments
compose into one fragment without ever bottoming out into 'IO'.
Equivalent JVM idiom: extracting a topology-building helper
method that takes a @KStream@ and returns a transformed
@KStream@.
-}
module Kafka.Streams.Examples.IdiomaticPipeline (
  runDemo,
  idiomaticPipelineTopology,
  buildPipelineTopology,

  -- * Reusable fragments
  normalise,
  dropEmpties,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


{- | Strip whitespace + uppercase every value. A reusable
fragment — could be defined in a shared module and used by
many topologies.
-}
normalise :: F.Topology (KStream Text Text) (KStream Text Text)
normalise = F.mapValues T.strip >>> F.mapValues T.toUpper


-- | Drop records whose value is empty after normalisation.
dropEmpties :: F.Topology (KStream Text Text) (KStream Text Text)
dropEmpties = F.filter (\r -> recordValue r /= "")


{- | Wire the two fragments together with 'Control.Category.(>>>)'
and bracket them by a source / sink. The optimiser sees the
whole topology — fusion is across fragment boundaries, not
inside each one separately, so reusing fragments costs
nothing at run time.
-}
idiomaticPipelineTopology :: F.Topology Void ()
idiomaticPipelineTopology =
  F.source "lines-in"
    >>> normalise
    >>> dropEmpties
    >>> F.sink "lines-out"


buildPipelineTopology :: IO Topo.Topology
buildPipelineTopology = F.buildTopologyFrom idiomaticPipelineTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== IdiomaticPipelineDemo ==="
  topo <- buildPipelineTopology
  driver <- newDriver topo "idiomatic-pipeline"
  mapM_
    (send driver)
    [ ("k1", "  hello world  ")
    , ("k2", "   ")
    , ("k3", "kafka streams")
    ]
  out <- readOutput driver (topicName "lines-out")
  putStrLn ("Records emitted to lines-out (" <> show (length out) <> "):")
  mapM_ (\cr -> putStrLn ("  " <> BSC.unpack (crValueBytes cr))) out
  closeDriver driver
  where
    send d (k, v) =
      pipeInput
        d
        (topicName "lines-in")
        (Just (BSC.pack k))
        (BSC.pack v)
        (Timestamp 0)
        0
