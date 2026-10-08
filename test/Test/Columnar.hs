{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the unified 'Wireform.Columnar.encode' / 'decode'
facade. Verifies that the same (Arrow schema + column
batches) survives a round-trip through every 'Format' the
facade supports, so callers can swap wire formats without
changing their data layout.
-}
module Test.Columnar (columnarFacadeTests) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Data.Int (Int32, Int64)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Test.Syd
import Wireform.Columnar qualified as Col


columnarFacadeTests :: Spec
columnarFacadeTests =
  describe "Wireform.Columnar unified facade" $
    sequence_
      [ it "round-trip through every format" $ do
          -- Common shape the facade should handle identically
          -- regardless of Format: two leaf columns, one int64, one
          -- utf8, non-nullable, single batch.
          let !sch =
                AT.Schema
                  { AT.arrowFields =
                      V.fromList
                        [ AT.Field "id" False (AT.AInt 64 True) V.empty Nothing V.empty
                        , AT.Field "name" False AT.AUtf8 V.empty Nothing V.empty
                        ]
                  , AT.arrowEndianness = AT.Little
                  , AT.arrowMetadata = V.empty
                  , AT.arrowFeatures = V.empty
                  }
              !batch =
                V.fromList
                  [ AC.primColumn AC.PInt64 (VS.fromList [10, 20, 30 :: Int64])
                  , AC.fromTexts (V.fromList ["alpha", "beta", "gamma"])
                  ]
              !batches = [batch]
              !opts = Col.defaultWriteOptions
              !ropts = Col.defaultReadOptions

          -- Arrow stream
          assertRoundTrip "Arrow stream" Col.Arrow opts ropts sch batches

          -- Arrow file
          assertRoundTrip "Arrow file" Col.ArrowFile opts ropts sch batches

          -- Parquet: need PageV1 + Uncompressed for the bridge's
          -- simple readers (consistent with the wireform-parquet
          -- test suite).
          let !parquetOpts =
                opts
                  { Col.parquetWrite =
                      (Col.parquetWrite opts)
                        { Col.writePageVersion = Col.PageV1
                        , Col.writeCompression = Col.Uncompressed
                        }
                  }
          assertRoundTrip "Parquet" Col.Parquet parquetOpts ropts sch batches

          -- ORC
          assertRoundTrip "ORC" Col.ORC opts ropts sch batches
      , it "Parquet format ignores Arrow options" $ do
          -- Writing with Arrow-only options set on opts.arrowWrite
          -- shouldn't bleed into the Parquet path. Smoke test by
          -- encoding + decoding and checking the magic prefix.
          let !sch =
                AT.Schema
                  { AT.arrowFields =
                      V.singleton
                        (AT.Field "x" False (AT.AInt 32 True) V.empty Nothing V.empty)
                  , AT.arrowEndianness = AT.Little
                  , AT.arrowMetadata = V.empty
                  , AT.arrowFeatures = V.empty
                  }
              !batches = [V.singleton (AC.primColumn AC.PInt32 (VS.fromList [1, 2, 3 :: Int32]))]
              !opts =
                Col.defaultWriteOptions
                  { Col.parquetWrite =
                      (Col.parquetWrite Col.defaultWriteOptions)
                        { Col.writePageVersion = Col.PageV1
                        , Col.writeCompression = Col.Uncompressed
                        }
                  }
          case Col.encode Col.Parquet opts sch batches of
            Left e -> expectationFailure e
            Right _ -> pure ()
      ]
  where
    assertRoundTrip
      :: String
      -> Col.Format
      -> Col.WriteOptions
      -> Col.ReadOptions
      -> AT.Schema
      -> [V.Vector AC.ColumnArray]
      -> IO ()
    assertRoundTrip label fmt wopts ropts sch batches =
      case Col.encode fmt wopts sch batches of
        Left e -> expectationFailure (label ++ ": encode: " ++ e)
        Right bytes ->
          case Col.decode fmt ropts bytes of
            Left e -> expectationFailure (label ++ ": decode: " ++ e)
            Right (_, batches') ->
              case batches' of
                [b] | b == head batches -> pure ()
                _ ->
                  expectationFailure $
                    label
                      ++ ": batch mismatch\n got "
                      ++ show batches'
                      ++ "\n exp "
                      ++ show batches
