{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Hedgehog properties for the unified 'Wireform.Columnar'
facade.

The tests are layered by capability rather than by format:

* 'crossFormatRoundTrip': the shapes every format supports
  identically. Run against all four 'Col.Format' values.
* 'arrowFullCoverage': the shapes the Arrow IPC stream / file
  format round-trip (nullable + Int16 + binary included).
* 'parquetBridgeRoundTrip': the shapes the Arrow <-> Parquet
  bridge currently supports (non-nullable flat primitives +
  temporals).
* 'orcBridgeRoundTrip': the shapes the Arrow <-> ORC bridge
  currently supports (adds nullable via PRESENT stream).

A failure here indicates either a regression in the round-trip
path or a bridge-capability boundary that should be recorded
in the matching generator's doc.
-}
module Test.Columnar.Properties (columnarPropertyTests) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Data.Vector qualified as V
import Hedgehog
import Test.Columnar.Gen qualified as G
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Columnar qualified as Col


columnarPropertyTests :: Spec
columnarPropertyTests =
  describe "Wireform.Columnar properties" $
    sequence_
      [ describe "cross-format round-trips (every Format round-trips the same inputs)" $
          sequence_
            [ it "Arrow stream" (propCrossFormat Col.Arrow Col.defaultWriteOptions)
            , it "Arrow file" (propCrossFormat Col.ArrowFile Col.defaultWriteOptions)
            , it "ORC" (propCrossFormat Col.ORC Col.defaultWriteOptions)
            , it "Parquet" (propCrossFormat Col.Parquet parquetOpts)
            ]
      , describe "per-format bridge coverage" $
          sequence_
            [ it
                "Arrow (stream): full ColumnArray coverage"
                (propBridge Col.Arrow Col.defaultWriteOptions G.genArrowOnly)
            , it
                "Arrow (file): full ColumnArray coverage"
                (propBridge Col.ArrowFile Col.defaultWriteOptions G.genArrowOnly)
            , it
                "Parquet: required + nullable flat + temporal"
                (propBridge Col.Parquet parquetOpts G.genParquetBridge)
            , it
                "ORC: nullable flat + temporal"
                (propBridge Col.ORC Col.defaultWriteOptions G.genORCBridge)
            ]
      ]
  where
    -- The Parquet Arrow-bridge reader expects PageV1 +
    -- Uncompressed pages (the simple per-chunk readers used by
    -- Parquet.Arrow don't support V2 / compressed pages yet).
    -- That's a bridge limitation, not a format one, the
    -- lower-level Parquet.Write path handles every combination.
    parquetOpts =
      Col.defaultWriteOptions
        { Col.parquetWrite =
            (Col.parquetWrite Col.defaultWriteOptions)
              { Col.writePageVersion = Col.PageV1
              , Col.writeCompression = Col.Uncompressed
              }
        }


-- ============================================================
-- Property bodies
-- ============================================================

{- | Assert @decode fmt opts (encode fmt opts input) == Right input@
for any generator producing a valid @(schema, batches)@ pair
for the given format. The bridge-capability generators (see
'Test.Columnar.Gen') are already scoped to the shapes each
format round-trips, so failures here indicate real bugs rather
than known unsupported shapes.
-}
propBridge
  :: Col.Format
  -> Col.WriteOptions
  -> Gen (AT.Schema, [V.Vector AC.ColumnArray])
  -> Property
propBridge fmt opts gen = withTests 75 $ property $ do
  (sch, batches) <- forAll gen
  runRoundTrip fmt opts sch batches


propCrossFormat
  :: Col.Format
  -> Col.WriteOptions
  -> Property
propCrossFormat fmt opts = withTests 50 $ property $ do
  (sch, batches) <- forAll G.genCrossFormat
  runRoundTrip fmt opts sch batches


{- | Shared round-trip body used by every property. Runs the
encode / decode pair and asserts the batches come out
unchanged. The returned schema is /not/ compared: only Arrow
preserves the original schema verbatim, and only the
columnar-data content matters for a round-trip property.
-}
runRoundTrip
  :: Col.Format
  -> Col.WriteOptions
  -> AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> PropertyT IO ()
runRoundTrip fmt opts sch batches =
  case Col.encode fmt opts sch batches of
    Left e -> do
      annotate ("encode: " <> e)
      failure
    Right bytes ->
      case Col.decode fmt Col.defaultReadOptions bytes of
        Left e -> do
          annotate ("decode: " <> e)
          failure
        Right (_sch', batches') ->
          batches' === batches
