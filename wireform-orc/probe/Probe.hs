{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-orc -> pyarrow / duckdb interop probe.
--
-- Writes a fixed catalogue of one-file-per-shape ORC files
-- under @argv[1]@. The companion 'scripts/orc_interop.py'
-- driver reads each one with pyarrow.orc and duckdb and
-- asserts the contents match what we wrote.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Columnar.Bit (Bit (..))
import qualified Arrow.View as AV
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Arrow.Column as AC
import qualified Arrow.Types as AT
import qualified ORC
import qualified ORC.Arrow as OArrow

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> do
      writeAllProbes outDir
      putStrLn $ "wrote orc probe outputs to " ++ outDir
    _ -> do
      putStrLn "usage: wireform-orc-interop-probe <output-dir>"
      exitFailure

writeAllProbes :: FilePath -> IO ()
writeAllProbes outDir = do
  writeOne outDir "int64_required.orc" int64Sch int64Batches
  writeOne outDir "double_required.orc" doubleSch doubleBatches
  writeOne outDir "string_required.orc" stringSch stringBatches
  writeOne outDir "mixed_required.orc" mixedSch mixedBatches
  writeOne outDir "bool_required.orc" boolSch boolBatches
  -- harder types: nested struct + list (the ORC bridge
  -- supports both via arrowToORC's TKStruct / TKList paths)
  writeOne outDir "nested_struct.orc"  structSch structBatches
  writeOne outDir "list_int64.orc"     listSch   listBatches
  -- additional integer widths exposed through Arrow
  writeOne outDir "int32_required.orc" int32Sch  int32Batches
  writeOne outDir "float_required.orc" floatSch  floatBatches
  -- timestamp
  writeOne outDir "timestamp_required.orc" tsSch tsBatches

writeOne :: FilePath -> FilePath -> AT.Schema -> [V.Vector AC.ColumnArray] -> IO ()
writeOne outDir fname sch batches =
  case OArrow.arrowToORC sch batches of
    Left err -> do
      putStrLn $ "FAIL " ++ fname ++ " (arrowToORC): " ++ err
      exitFailure
    Right (types, stripesWithRows) ->
      case ORC.encodeORC ORC.defaultWriteOptions types stripesWithRows of
        Right bs -> BS.writeFile (outDir </> fname) bs
        Left err -> do
          putStrLn $ "FAIL " ++ fname ++ " (encodeORC): " ++ err
          exitFailure

-- ============================================================
-- Schemas + payloads
-- ============================================================

int64Sch :: AT.Schema
int64Sch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "x" False (AT.AInt 64 True)))

int64Batches :: [V.Vector AC.ColumnArray]
int64Batches =
  [ V.singleton (AC.ColInt64 (VS.fromList [10, 20, 30, 40, 50 :: Int64])) ]

doubleSch :: AT.Schema
doubleSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "x" False (AT.AFloatingPoint AT.DoublePrecision)))

doubleBatches :: [V.Vector AC.ColumnArray]
doubleBatches =
  [ V.singleton (AC.ColDouble (VS.fromList [1.5, -2.5, 3.14159 :: Double])) ]

stringSch :: AT.Schema
stringSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "name" False AT.AUtf8))

stringBatches :: [V.Vector AC.ColumnArray]
stringBatches =
  [ V.singleton (AC.ColUtf8 (AV.utf8ViewFromVector (V.fromList ["alpha", "beta", "gamma"]))) ]

boolSch :: AT.Schema
boolSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "b" False AT.ABool))

boolBatches :: [V.Vector AC.ColumnArray]
boolBatches =
  [ V.singleton (AC.ColBool (VU.fromList (map Bit [True, False, True, True, False]))) ]

mixedSch :: AT.Schema
mixedSch = AT.defaultSchema (V.fromList
  [ AT.defaultLeafField "id"   False (AT.AInt 64 True)
  , AT.defaultLeafField "name" False AT.AUtf8
  , AT.defaultLeafField "score" False (AT.AFloatingPoint AT.DoublePrecision)
  ])

mixedBatches :: [V.Vector AC.ColumnArray]
mixedBatches =
  [ V.fromList
      [ AC.ColInt64 (VS.fromList [10, 20, 30 :: Int64])
      , AC.ColUtf8 (AV.utf8ViewFromVector (V.fromList ["alice", "bob", "carol"]))
      , AC.ColDouble (VS.fromList [1.5, 2.5, 3.5 :: Double])
      ]
  ]

-- ============================================================
-- Harder shapes
-- ============================================================

structSch :: AT.Schema
structSch = AT.defaultSchema $ V.singleton $ AT.Field
  { AT.fieldName       = "rec"
  , AT.fieldNullable   = False
  , AT.fieldType       = AT.AStruct
  , AT.fieldChildren   = V.fromList
      [ AT.defaultLeafField "i" False (AT.AInt 64 True)
      , AT.defaultLeafField "n" False AT.AUtf8
      ]
  , AT.fieldDictionary = Nothing
  , AT.fieldMetadata   = V.empty
  }

structBatches :: [V.Vector AC.ColumnArray]
structBatches =
  [ V.singleton (AC.ColStruct (V.fromList
      [ ("i", AC.ColInt64 (VS.fromList [1, 2, 3 :: Int64]))
      , ("n", AC.ColUtf8  (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"])))
      ]))
  ]

listSch :: AT.Schema
listSch = AT.defaultSchema $ V.singleton $ AT.Field
  { AT.fieldName       = "lst"
  , AT.fieldNullable   = False
  , AT.fieldType       = AT.AList
  , AT.fieldChildren   = V.singleton
      (AT.defaultLeafField "item" False (AT.AInt 64 True))
  , AT.fieldDictionary = Nothing
  , AT.fieldMetadata   = V.empty
  }

listBatches :: [V.Vector AC.ColumnArray]
listBatches =
  [ V.singleton (AC.ColList
      (VS.fromList ([0, 2, 5, 7] :: [Int32]))
      (AC.ColInt64 (VS.fromList ([10,20,30,40,50,60,70] :: [Int64]))))
  ]

int32Sch :: AT.Schema
int32Sch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "x" False (AT.AInt 32 True)))

int32Batches :: [V.Vector AC.ColumnArray]
int32Batches =
  [ V.singleton (AC.ColInt32 (VS.fromList [1, 2, 3, 4, 5 :: Int32])) ]

floatSch :: AT.Schema
floatSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "x" False (AT.AFloatingPoint AT.Single)))

floatBatches :: [V.Vector AC.ColumnArray]
floatBatches =
  [ V.singleton (AC.ColFloat (VS.fromList [1.5, 2.5, 3.5 :: Float])) ]

tsSch :: AT.Schema
tsSch = AT.defaultSchema (V.singleton
  (AT.defaultLeafField "ts" False
    (AT.ATimestamp AT.Nanosecond Nothing)))

tsBatches :: [V.Vector AC.ColumnArray]
tsBatches =
  [ V.singleton (AC.ColTimestamp (VS.fromList
      [0, 1700000000_000_000_000 :: Int64])) ]
