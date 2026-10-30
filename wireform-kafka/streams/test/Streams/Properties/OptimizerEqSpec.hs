{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.OptimizerEqSpec
Description : Property: optimised and un-optimised compilations
              agree observably

The 'Kafka.Streams.Topology.Free' AST optimiser applies a long
list of semantics-preserving rewrites: 'MapValues' \/
'MapKeyValue' \/ 'MapRecord' fusion, 'Filter' \/ 'FilterNot'
fusion, 'ConcatMapValues' fusion, 'Peek' fusion, 'Tap' collapse,
repartition collapse, repartition hoisting, auto-insert
repartition, 'optFuseSyncIntoAsync' from the Riffle work, etc.

This property generates a random chain of @'KStream' 'Text'
'Text'@-preserving operations, compiles the topology twice (once
with @'defaultOptimizeConfig'@, once with @'noOptimization'@),
runs identical inputs through both via 'TopologyTestDriver',
and asserts the output sequences agree.

Hedgehog shrinks on failure, so a divergence in any rewrite
rule will reduce to a minimum reproducer — the smallest
combinator pair whose semantics differ before vs after
optimisation.
-}
module Streams.Properties.OptimizerEqSpec (tests) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Imperative
import Kafka.Streams.Topology.Free qualified as F
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Op DSL
----------------------------------------------------------------------

{- | A single 'KStream' 'Text' 'Text'-preserving op. The shape is
restricted to operations that keep the wire type stable, so we
can chain them freely without tracking types through Hedgehog
generators.
-}
data Op
  = OpMapToUpper
  | OpMapToLower
  | OpMapAppend !Text
  | OpFilterNonEmpty
  | -- | keep values with length < N
    OpFilterShort !Int
  | -- | keep values with length >= N
    OpFilterNotShort !Int
  | OpFlatMapWords
  | OpFlatMapDuplicate
  | -- | identity from the wire's POV
    OpPeek
  | -- | fusion barrier
    OpNoFuse
  | OpRepartition !Text
  deriving stock (Eq, Show)


genOp :: H.Gen Op
genOp =
  Gen.choice
    [ pure OpMapToUpper
    , pure OpMapToLower
    , OpMapAppend <$> Gen.text (Range.linear 0 3) Gen.alpha
    , pure OpFilterNonEmpty
    , OpFilterShort <$> Gen.int (Range.linear 1 8)
    , OpFilterNotShort <$> Gen.int (Range.linear 1 8)
    , pure OpFlatMapWords
    , pure OpFlatMapDuplicate
    , pure OpPeek
    , pure OpNoFuse
    , OpRepartition <$> Gen.text (Range.linear 1 4) Gen.alpha
    ]


-- | Compile an 'Op' to a 'KStream' 'Text' 'Text' topology fragment.
opToFragment :: Op -> F.Topology (KStream Text Text) (KStream Text Text)
opToFragment = \case
  OpMapToUpper -> F.mapValues T.toUpper
  OpMapToLower -> F.mapValues T.toLower
  OpMapAppend suffix -> F.mapValues (<> suffix)
  OpFilterNonEmpty -> F.filter (\r -> recordValue r /= "")
  OpFilterShort n ->
    F.filter (\r -> T.length (recordValue r) < n)
  OpFilterNotShort n ->
    F.filterNot (\r -> T.length (recordValue r) < n)
  OpFlatMapWords -> F.concatMapValues T.words
  OpFlatMapDuplicate -> F.concatMapValues (\v -> [v, v])
  OpPeek -> F.peek (\_r -> pure ())
  OpNoFuse -> F.noFuse
  OpRepartition pfx -> F.repartition pfx


-- | Compose a sequence of fragments into a single chain.
chain :: [Op] -> F.Topology (KStream Text Text) (KStream Text Text)
chain [] = F.askInput
chain (o : os) = opToFragment o >>> chain os


-- | A closed topology: source >>> chain >>> sink.
mkTopology :: [Op] -> F.Topology Void ()
mkTopology ops =
  F.source @Text @Text "in"
    >>> chain ops
    >>> F.sink "out"


----------------------------------------------------------------------
-- Test harness
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


{- | Compile via the supplied optimisation config, pipe the
inputs through the driver, return the output sequence.
-}
runWith
  :: F.OptimizeConfig
  -> [Op]
  -> [Text]
  -> IO [(Maybe Text, Text)]
runWith optCfg ops inputs = do
  (_, topo) <- F.compileWithOptimization optCfg (mkTopology ops)
  driver <- newDriver topo "opt-eq"
  mapM_
    ( \v ->
        pipeInput
          driver
          (topicName "in")
          Nothing
          (bytes v)
          (Timestamp 0)
          0
    )
    inputs
  out <- readOutput driver (topicName "out")
  closeDriver driver
  pure (map (\r -> (fmap unbytes (crKey r), unbytes (crValueBytes r))) out)


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Optimizer equivalence" $
    sequence_
      [ it "optimised compile observably equals un-optimised" $
          H.withTests 80 propEquivalence
      ]


propEquivalence :: H.Property
propEquivalence = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 0 10) genOp)
  inputs <-
    H.forAll
      (Gen.list (Range.linear 0 16) (Gen.text (Range.linear 0 8) Gen.alpha))
  (optOut, rawOut) <- H.evalIO $ do
    o <- runWith F.defaultOptimizeConfig ops inputs
    r <- runWith F.noOptimization ops inputs
    pure (o, r)
  H.annotate ("ops:     " <> show ops)
  H.annotate ("inputs:  " <> show inputs)
  optOut H.=== rawOut
