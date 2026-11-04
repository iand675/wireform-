{-# LANGUAGE OverloadedStrings #-}

module Test.Bench (tests) where

import Data.Aeson qualified as A
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Test.Syd
import Wireform.Stats.Bench qualified as Bench
import Wireform.Stats.SVG qualified as SVG
import Wireform.Stats.Table qualified as Tbl


tests :: Spec
tests =
  describe "Bench" $
    sequence_
      [ it "summary JSON round-trips" jsonRoundTrip
      , it "summaryToTable: header columns + row count" tableShape
      , it "summaryToBarChart: series + groups preserved" chartShape
      , it "compares the other series when the baseline is second" $ do
          let table = Bench.summaryToTable sampleSummary {Bench.bsBaseline = Just "cborg"}
          map last (Tbl.tableRows table) `shouldBe` ["0.78x", "0.80x"]
      , it "uses the first series as the default baseline" $ do
          let explicit = Bench.summaryToTable sampleSummary
              implicit = Bench.summaryToTable sampleSummary {Bench.bsBaseline = Nothing}
          map last (Tbl.tableRows implicit) `shouldBe` map last (Tbl.tableRows explicit)
      , it "does not invent a ratio for a missing baseline or comparison" $ do
          let missing = Bench.summaryToTable sampleSummary {Bench.bsBaseline = Just "unknown"}
              single = Bench.summaryToTable sampleSummary {Bench.bsSeries = take 1 (Bench.bsSeries sampleSummary)}
          map last (Tbl.tableRows missing) `shouldBe` ["-", "-"]
          map last (Tbl.tableRows single) `shouldBe` ["-", "-"]
      , it "carries rounded duration fractions into the integer part" $ do
          let table = Bench.summaryToTable sampleSummary {Bench.bsSeries = [Bench.BenchSeries "wireform-cbor" [9.999, 19.999]]}
          map (take 2) (Tbl.tableRows table) `shouldBe` [["encode", "10.00 ns"], ["decode", "20.0 ns"]]
      , it "rounds near-parity ratios to 1.00x" $ do
          let table = Bench.summaryToTable sampleSummary {Bench.bsSeries = [Bench.BenchSeries "wireform-cbor" [1000, 1000], Bench.BenchSeries "cborg" [999.9, 1999.9]]}
          map last (Tbl.tableRows table) `shouldBe` ["1.00x", "2.00x"]
      ]


sampleSummary :: Bench.BenchSummary
sampleSummary =
  Bench.BenchSummary
    { Bench.bsId = "cbor-vs-cborg-encode"
    , Bench.bsTitle = "wireform-cbor vs cborg"
    , Bench.bsUnit = Bench.Nanos
    , Bench.bsHigherIsBetter = False
    , Bench.bsGroups = ["encode", "decode"]
    , Bench.bsSeries =
        [ Bench.BenchSeries "wireform-cbor" [3200, 4700]
        , Bench.BenchSeries "cborg" [4100, 5900]
        ]
    , Bench.bsBaseline = Just "wireform-cbor"
    , Bench.bsCapturedAt = UTCTime (fromGregorian 2026 5 13) 0
    , Bench.bsToolchain = "ghc-9.8.4 on darwin-aarch64"
    }


jsonRoundTrip :: IO ()
jsonRoundTrip = do
  let bytes = A.encode sampleSummary
  case A.eitherDecode bytes of
    Right (back :: Bench.BenchSummary) -> back `shouldBe` sampleSummary
    Left err -> expectationFailure err


tableShape :: IO ()
tableShape = do
  let t = Bench.summaryToTable sampleSummary
  -- 4 header columns: Operation + 2 series + ratio.
  length (Tbl.tableHeader t) `shouldBe` 4
  -- 2 rows: one per group.
  length (Tbl.tableRows t) `shouldBe` 2
  -- Each row has the same arity as the header.
  mapM_ (\r -> length r `shouldBe` 4) (Tbl.tableRows t)


chartShape :: IO ()
chartShape = do
  let c = Bench.summaryToBarChart sampleSummary
  length (SVG.chartGroups c) `shouldBe` 2
  length (SVG.chartSeries c) `shouldBe` 2
  SVG.chartUnit c `shouldBe` "ns"
  SVG.chartHigherIsBetter c `shouldBe` False
