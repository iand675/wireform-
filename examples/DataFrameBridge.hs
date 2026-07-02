{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Cross-library example: write a Parquet file with @wireform-parquet@,
then load it back through the @dataframe@ library
(<https://hackage.haskell.org/package/dataframe>) and run a few
aggregations on it via the typed expression DSL.

This exists to demonstrate that wireform's Parquet writer produces
bytes that other Haskell tooling can consume — there's no API
coupling between the two libraries, just the on-disk Parquet
format. The example doubles as an interop smoke test: every value
the dataframe pipeline reports is also computed in pure Haskell
from the source vectors, and the two are compared.

The dataframe dependency is heavy (cassava, attoparsec, regex-tdfa,
snappy-hs, zstd, zlib, granite, vector-algorithms, …) so the
example is hidden behind the @+dataframe-bridge@ Cabal flag and
excluded from a default @cabal build all@. To build and run it:

> cabal run example-dataframe-bridge -fdataframe-bridge
-}
module Main (main) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
-- The dataframe ecosystem is split into focused packages; this bridge
-- depends only on the three it uses and imports their submodules
-- directly instead of the umbrella @dataframe@ package's curated
-- @DataFrame@/@DataFrame.Typed@ re-exports:
--   * dataframe-parquet    — the Parquet reader (DataFrame.IO.Parquet)
--   * dataframe-operations — untyped describe + the typed DSL
--                            (groupBy/aggregate/expressions/access)
--   * dataframe-core       — the (|>) operator and the typed-schema
--                            'Column' type + 'freezeWithError'
-- The typed surface tracks column names and types in a phantom type;
-- everything past 'TDF.freezeWithError' is schema-checked at compile
-- time.
import DataFrame.IO.Parquet qualified as D
import DataFrame.Operations.Core qualified as D
import DataFrame.Operators ((|>))
import DataFrame.Typed.Expr ((.*.), (.==.), (.>.))
import DataFrame.Typed.Access qualified as TDF
import DataFrame.Typed.Aggregate qualified as TDF
import DataFrame.Typed.Expr qualified as TDF
import DataFrame.Typed.Freeze qualified as TDF
import DataFrame.Typed.Operations qualified as TDF
import DataFrame.Typed.Types qualified as TDF
import Parquet.Types qualified as P
import Parquet.Write qualified as PW
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  removeFile,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)


{- | The synthetic dataset: a tiny e-commerce order log. Three
columns line up with the three Parquet primitive shapes the
writer emits below ('PTInt64', 'PTByteArray', 'PTDouble').
-}
data Order = Order
  { orderId :: !Int64
  , region :: !Text
  , amount :: !Double
  }
  deriving (Show, Eq)


type OrderSchema =
  '[ TDF.Column "order_id" Int64
   , TDF.Column "region" Text
   , TDF.Column "amount" Double
   ]


orders :: [Order]
orders =
  [ Order 1 "us-west" 12.50
  , Order 2 "us-west" 17.00
  , Order 3 "us-east" 5.25
  , Order 4 "eu" 31.80
  , Order 5 "us-east" 9.75
  , Order 6 "eu" 22.40
  , Order 7 "us-west" 14.10
  , Order 8 "apac" 44.99
  , Order 9 "us-east" 7.30
  , Order 10 "apac" 19.20
  , Order 11 "eu" 28.65
  , Order 12 "us-west" 3.45
  ]


main :: IO ()
main = withSystemTempDirectory "wireform-dataframe-bridge" $ \dir -> do
  createDirectoryIfMissing True dir
  let parquetPath = dir </> "orders.parquet"

  -- 1. Write the data with wireform-parquet.
  putStrLn "=== wireform-parquet writer ==="
  let bytes = encodeOrders orders
  BS.writeFile parquetPath bytes
  putStrLn $ "wrote " ++ show (BS.length bytes) ++ " bytes to " ++ parquetPath

  -- 2. Hand the file off to the dataframe library and explore it.
  --    The untyped describe step runs first so we can see what the
  --    reader actually found, then 'TDF.freezeWithError' checks the
  --    columns against 'OrderSchema' once. Everything past this
  --    point operates on a 'TypedDataFrame' and is checked at
  --    compile time.
  putStrLn "\n=== dataframe reader ==="
  raw <- D.readParquet parquetPath
  putStrLn $ "dimensions: " ++ show (D.dimensions raw)
  print (D.describeColumns raw)
  df <- case TDF.freezeWithError @OrderSchema raw of
    Right typed -> pure typed
    Left err -> error $ "schema mismatch: " <> T.unpack err

  -- 3. Run a typed aggregation: total + order count + mean per
  --    region, sorted by total descending. Column references go
  --    through @TDF.col \@\"name\"@ — name and type are both
  --    resolved against @OrderSchema@; aggregation outputs are
  --    named with @TDF.agg \@\"name\"@ and reflected in the
  --    result's phantom schema.
  putStrLn "=== regional totals (dataframe pipeline) ==="
  let perRegion =
        df
          |> TDF.groupBy @'["region"]
          |> TDF.aggregate
            ( TDF.as @"total" (TDF.sum (TDF.col @"amount"))
                . TDF.as @"orders" (TDF.count (TDF.col @"order_id"))
                . TDF.as @"avg" (TDF.mean (TDF.col @"amount"))
            )
          |> TDF.sortBy [TDF.desc (TDF.col @"total")]
  print perRegion

  -- 4. Cross-check against the source-of-truth Haskell computation.
  --    The filter predicate and the column extractions are checked
  --    against the post-aggregate schema at compile time, so the
  --    only runtime failure mode is "no row for this region".
  putStrLn "=== ground-truth check ==="
  let groundTruth = expectedTotals orders
  for_ groundTruth $ \(reg, expectedTot, expectedCount) -> do
    let matched = perRegion |> TDF.filterWhere (TDF.col @"region" .==. TDF.lit reg)
    case ( TDF.columnAsList @"total" matched
         , TDF.columnAsList @"orders" matched
         ) of
      (actualTot : _, actualCount : _) -> do
        putStrLn $
          T.unpack reg
            ++ ": dataframe total="
            ++ showD actualTot
            ++ "  expected="
            ++ showD expectedTot
            ++ "  dataframe count="
            ++ show actualCount
            ++ "  expected="
            ++ show expectedCount
        assertClose ("total for " ++ T.unpack reg) expectedTot actualTot
        assertEq ("count for " ++ T.unpack reg) expectedCount actualCount
      _ -> error $ "ground-truth check: no row for region " <> T.unpack reg

  -- 5. A richer pipeline: filter to large orders, derive a
  --    synthetic "discounted" column, then take the top three rows.
  --    'TDF.derive' extends the schema with @"discounted_amount"@;
  --    the subsequent sort reference is checked against that new
  --    schema.
  putStrLn "\n=== filter + derive + take (dataframe pipeline) ==="
  let topDiscounted =
        df
          |> TDF.filterWhere (TDF.col @"amount" .>. TDF.lit 15.0)
          |> TDF.derive @"discounted_amount" (TDF.col @"amount" .*. TDF.lit 0.9)
          |> TDF.sortBy [TDF.desc (TDF.col @"discounted_amount")]
          |> TDF.take 3
  print topDiscounted

  -- Tidy up the temp file (withSystemTempDirectory removes the dir,
  -- but doing this explicitly keeps the example readable).
  exists <- doesFileExist parquetPath
  if exists then removeFile parquetPath else pure ()

  putStrLn "\nOK — wireform → dataframe round-trip passed."


-- ---------------------------------------------------------------------------
-- wireform-parquet writer side
-- ---------------------------------------------------------------------------

{- | Build a single-row-group Parquet file from the order list.

Three columns:

  order_id : INT64                   / required
  region   : BYTE_ARRAY (UTF-8)      / required
  amount   : DOUBLE                  / required
-}
encodeOrders :: [Order] -> BS.ByteString
encodeOrders rows =
  let schema =
        V.fromList
          [ P.SchemaElement
              { P.seName = "schema"
              , P.seRepetition = Nothing
              , P.seType = Nothing
              , P.seNumChildren = Just 3
              , P.seConvertedType = Nothing
              , P.seLogicalType = Nothing
              , P.seFieldId = Nothing
              }
          , P.SchemaElement
              "order_id"
              (Just P.Required)
              (Just P.PTInt64)
              Nothing
              Nothing
              Nothing
              Nothing
          , P.SchemaElement
              "region"
              (Just P.Required)
              (Just P.PTByteArray)
              Nothing
              (Just P.CTUtf8)
              Nothing
              Nothing
          , P.SchemaElement
              "amount"
              (Just P.Required)
              (Just P.PTDouble)
              Nothing
              Nothing
              Nothing
              Nothing
          ]

      idCol = VP.fromList (map orderId rows)
      regionCol = V.fromList (map (TE.encodeUtf8 . region) rows)
      amountCol = VP.fromList (map amount rows)

      cols =
        V.fromList
          [ PW.ColInt64 idCol
          , PW.ColByteArray regionCol
          , PW.ColDouble amountCol
          ]
  in PW.buildParquetFile schema (V.singleton cols)


-- ---------------------------------------------------------------------------
-- Ground-truth (pure-Haskell) reference aggregates
-- ---------------------------------------------------------------------------

expectedTotals :: [Order] -> [(Text, Double, Int)]
expectedTotals rows = map summarise (uniq (map region rows))
  where
    summarise reg =
      let matching = filter ((== reg) . region) rows
      in (reg, sum (map amount matching), length matching)


uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (filter (/= x) xs)


-- ---------------------------------------------------------------------------
-- Tiny assertion helpers
-- ---------------------------------------------------------------------------

assertClose :: String -> Double -> Double -> IO ()
assertClose msg expected actual
  | abs (expected - actual) < 1e-6 = pure ()
  | otherwise =
      error $
        "assertion failed: "
          <> msg
          <> " expected "
          <> showD expected
          <> " got "
          <> showD actual


assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg expected actual
  | expected == actual = pure ()
  | otherwise =
      error $
        "assertion failed: "
          <> msg
          <> " expected "
          <> show expected
          <> " got "
          <> show actual


showD :: Double -> String
showD d =
  let !rounded = fromIntegral (round (d * 100) :: Int) / 100 :: Double
  in show rounded
