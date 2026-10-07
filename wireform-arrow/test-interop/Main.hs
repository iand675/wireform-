{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Bidirectional pyarrow interop suite for wireform-arrow.

Both sides define the same case matrix: this module as wireform-arrow
'Schema' plus 'ColumnArray' batches, @pyarrow_interop.py@ (next to this
file) as pyarrow record batches built with pyarrow's own constructors.
Case names tie the two together.

* Direction A: wireform-arrow writes every case as an IPC stream and an
  IPC file (plus zstd / lz4 body-compressed variants when the package is
  built with @-f+zstd@ / @-f+lz4@). pyarrow opens each one, runs
  @validate(full=True)@ on every batch, and compares schema (with custom
  metadata) and values against its own expected batches.
* Direction B: pyarrow writes every case (plain, sliced at a non-zero
  offset, zstd, lz4; stream and file). wireform-arrow decodes each with
  'decodeArrowStream' / 'decodeArrowFile' and compares the schema and the
  logical row values against the batches defined here.

Gating: the Python interpreter comes from @WIREFORM_ARROW_PYTHON@
(default @python3@). When @import pyarrow@ fails the suite prints SKIP
and exits 0, unless @WIREFORM_ARROW_REQUIRE_PYARROW=1@.

@--write DIR@ only writes the direction A files into @DIR@ (no Python
needed), for other readers such as @interop/arrow-rs@.
-}
module Main (main) where

import Arrow.Column (ColumnArray (..))
import Arrow.Stream (
  DictHandling (..),
  WriteOptions (..),
  decodeArrowFile,
  decodeArrowStream,
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowStream,
 )
import Arrow.Types (
  ArrowType (..),
  BodyCompressionCodec (..),
  DateUnit (..),
  DictionaryEncoding (..),
  Endianness (..),
  Field (..),
  IntervalUnit (..),
  Precision (..),
  Schema (..),
  TimeUnit (..),
  UnionMode (..),
 )
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Bits (shiftL, shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sortOn, stripPrefix)
import Data.Either (partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (float2Double)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stdout, utf8)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)


-- ============================================================
-- Case matrix. Keep in sync with pyarrow_interop.py.
-- ============================================================

data Case = Case
  { caseName :: String
  , caseSchema :: Schema
  , caseBatches :: [V.Vector ColumnArray]
  , caseStreamOnly :: Bool
  -- ^ Replacement / delta dictionaries are only legal in the stream format.
  , caseReplaceDicts :: Bool
  -- ^ Write with 'DictReplaceOnChange' instead of the default 'DictEmitOnce'.
  }


field :: Text -> Bool -> ArrowType -> [Field] -> Field
field n nullable ty kids = Field n nullable ty (V.fromList kids) Nothing V.empty


dictField :: Text -> Bool -> ArrowType -> Int64 -> ArrowType -> Field
dictField n nullable valueTy did indexTy =
  Field n nullable valueTy V.empty (Just (DictionaryEncoding did indexTy False)) V.empty


schemaOf :: [Field] -> Schema
schemaOf fs = Schema (V.fromList fs) Little V.empty V.empty


simple :: String -> Field -> [ColumnArray] -> Case
simple n f cols = Case n (schemaOf [f]) (map V.singleton cols) False False


{- | A non-nullable case @name@ (nulls dropped from the first batch) and a
nullable case @name_nullable@ (first batch with nulls, second batch with
none, so pyarrow omits the validity bitmap there).
-}
pair
  :: String
  -> ArrowType
  -> [Field]
  -> ([a] -> ColumnArray)
  -> ([Maybe a] -> ColumnArray)
  -> [Maybe a]
  -> [a]
  -> [Case]
pair n ty kids plain nullable b1 b2 =
  [ simple n (field "x" False ty kids) [plain (catMaybes b1), plain b2]
  , simple (n ++ "_nullable") (field "x" True ty kids) [nullable b1, nullable (map Just b2)]
  ]


primPair
  :: VP.Prim a
  => String
  -> ArrowType
  -> (VP.Vector a -> ColumnArray)
  -> (V.Vector (Maybe a) -> ColumnArray)
  -> [Maybe a]
  -> [a]
  -> [Case]
primPair n ty plain nullable = pair n ty [] (plain . VP.fromList) (nullable . V.fromList)


boxedPair
  :: String
  -> ArrowType
  -> (V.Vector a -> ColumnArray)
  -> (V.Vector (Maybe a) -> ColumnArray)
  -> [Maybe a]
  -> [a]
  -> [Case]
boxedPair n ty plain nullable = pair n ty [] (plain . V.fromList) (nullable . V.fromList)


-- | @w@-byte little-endian two's-complement encoding of a decimal's unscaled value.
decimalBytes :: Int -> Integer -> ByteString
decimalBytes w x =
  let u = x `mod` (1 `shiftL` (8 * w))
  in BS.pack (map (\i -> fromIntegral ((u `shiftR` (8 * i)) .&. 0xff)) [0 .. w - 1])


i32, i16, i64 :: [Int32] -> ColumnArray
i32 = ColInt32 . VP.fromList
i16 = ColInt16 . VP.fromList . map fromIntegral
i64 = ColInt64 . VP.fromList . map fromIntegral


i32m :: [Maybe Int32] -> ColumnArray
i32m = ColInt32Maybe . V.fromList


utf8m :: [Maybe Text] -> ColumnArray
utf8m = ColUtf8Maybe . V.fromList


cases :: [Case]
cases =
  concat
    [ primPair "int8" (AInt 8 True) ColInt8 ColInt8Maybe [Just 0, Just 1, Nothing, Just (-1), Just 127, Just (-128)] [42, -42 :: Int8]
    , primPair "int16" (AInt 16 True) ColInt16 ColInt16Maybe [Just 0, Just 1, Nothing, Just (-1), Just 32767, Just (-32768)] [1000, -1000 :: Int16]
    , primPair "int32" (AInt 32 True) ColInt32 ColInt32Maybe [Just 0, Just 1, Nothing, Just (-1), Just maxBound, Just minBound] [7, -7 :: Int32]
    , primPair "int64" (AInt 64 True) ColInt64 ColInt64Maybe [Just 0, Just 1, Nothing, Just (-1), Just maxBound, Just minBound] [123456789012, -5 :: Int64]
    , primPair "uint8" (AInt 8 False) ColUInt8 ColUInt8Maybe [Just 0, Just 1, Nothing, Just maxBound] [128 :: Word8]
    , primPair "uint16" (AInt 16 False) ColUInt16 ColUInt16Maybe [Just 0, Just 1, Nothing, Just maxBound] [256 :: Word16]
    , primPair "uint32" (AInt 32 False) ColUInt32 ColUInt32Maybe [Just 0, Just 1, Nothing, Just maxBound] [65536 :: Word32]
    , primPair "uint64" (AInt 64 False) ColUInt64 ColUInt64Maybe [Just 0, Just 1, Nothing, Just maxBound] [4294967296 :: Word64]
    , primPair "float16" (AFloatingPoint Half) ColFloat16 ColFloat16Maybe [Just 0x0000, Just 0x3C00, Nothing, Just 0xC000, Just 0x7BFF, Just 0x7C00] [0x3800 :: Word16]
    , primPair "float32" (AFloatingPoint Single) ColFloat ColFloatMaybe [Just 0, Just 1.5, Nothing, Just (-2.5), Just (1 / 0), Just 3.4028234663852886e38] [0.25 :: Float]
    , primPair "float64" (AFloatingPoint DoublePrecision) ColDouble ColDoubleMaybe [Just 0, Just 1.5, Nothing, Just (-2.5), Just (-1 / 0), Just 1e300, Just 5e-324] [3.141592653589793 :: Double]
    , boxedPair "bool" ABool ColBool ColBoolMaybe (map Just [True, False] ++ [Nothing] ++ map Just [True, True, False, False, True, False]) [False, True]
    , boxedPair "utf8" AUtf8 ColUtf8 ColUtf8Maybe strs ["second batch"]
    , boxedPair "large_utf8" ALargeUtf8 ColLargeUtf8 ColLargeUtf8Maybe strs ["second batch"]
    , boxedPair "binary" ABinary ColBinary ColBinaryMaybe bins ["z"]
    , boxedPair "large_binary" ALargeBinary ColLargeBinary ColLargeBinaryMaybe bins ["z"]
    , boxedPair "fixed_size_binary4" (AFixedSizeBinary 4) (ColFixedSizeBinary 4) (ColFixedSizeBinaryMaybe 4) [Just "abcd", Just "\0\0\0\0", Nothing, Just "\xff\xfe\xfd\xfc"] ["wxyz"]
    , boxedPair "utf8_view" AUtf8View ColUtf8View ColUtf8ViewMaybe views ["inline", "an out of line string value"]
    , boxedPair "binary_view" ABinaryView ColBinaryView ColBinaryViewMaybe viewBytes ["inline", BS.replicate 13 0]
    , primPair "date32" (ADate DateDay) ColDate32 ColDate32Maybe [Just 0, Just 18000, Nothing, Just (-1), Just 19000] [20000]
    , primPair "date64" (ADate DateMillisecond) ColDate64 ColDate64Maybe [Just 0, Just 1555200000000, Nothing, Just (-86400000)] [86400000]
    , primPair "time32_s" (ATime Second 32) ColTime32 ColTime32Maybe [Just 0, Just 3600, Nothing, Just 86399] [60]
    , primPair "time32_ms" (ATime Millisecond 32) ColTime32 ColTime32Maybe [Just 0, Just 1000, Nothing, Just 86399999] [1]
    , primPair "time64_us" (ATime Microsecond 64) ColTime64 ColTime64Maybe [Just 0, Just 12345000000, Nothing, Just 86399999999] [1]
    , primPair "time64_ns" (ATime Nanosecond 64) ColTime64 ColTime64Maybe [Just 0, Just 1, Nothing, Just 86399999999999] [2]
    , concatMap timeUnitCases [(Second, "s"), (Millisecond, "ms"), (Microsecond, "us"), (Nanosecond, "ns")]
    , primPair "timestamp_us_new_york" (ATimestamp Microsecond (Just "America/New_York")) ColTimestamp ColTimestampMaybe [Just 0, Just 1700000000000000, Nothing] [5]
    , decimalPair "decimal128_18_2" (ADecimal 18 2) (ColDecimal128 18 2) (ColDecimal128Maybe 18 2) 16 [Just 0, Just 123, Nothing, Just (-123), Just 999999999999999999] [1]
    , decimalPair "decimal128_38_10" (ADecimal 38 10) (ColDecimal128 38 10) (ColDecimal128Maybe 38 10) 16 [Just 123456789012345678901234567890, Nothing, Just (-1)] [10000000000]
    , decimalPair "decimal256_76_5" (ADecimal256 76 5) (ColDecimal256 76 5) (ColDecimal256Maybe 76 5) 32 [Just 100000, Nothing, Just (-1234567890123456789012345678901234567890123456789012345)] [1]
    , pair
        "interval_month_day_nano"
        (AInterval MonthDayNano)
        []
        ( \xs ->
            ColIntervalMonthDayNano
              (VP.fromList (map (\(m, _, _) -> m) xs))
              (VP.fromList (map (\(_, d, _) -> d) xs))
              (VP.fromList (map (\(_, _, ns) -> ns) xs))
        )
        (ColIntervalMonthDayNanoMaybe . V.fromList)
        [Just (1, 2, 3), Just (0, 0, 0), Nothing, Just (-1, -2, -3)]
        [(12, 30, 1000000000)]
    , primPair "interval_year_month" (AInterval YearMonth) ColIntervalYearMonth ColIntervalYearMonthMaybe [Just 0, Just 14, Nothing, Just (-3)] [5]
    , pair
        "interval_day_time"
        (AInterval DayTime)
        []
        (\xs -> ColIntervalDayTime (VP.fromList (map fst xs)) (VP.fromList (map snd xs)))
        (ColIntervalDayTimeMaybe . V.fromList)
        [Just (0, 0), Just (1, 1000), Nothing, Just (-1, -1)]
        [(2, 2)]
    , [simple "null" (field "x" True ANull []) [ColNull 4, ColNull 1]]
    , structCases
    , listCases
    , listViewCases
    , unionCases
    , dictCases
    , reeCases
    , [metadataCase]
    , mixedCases
    ]
  where
    strs = [Just "", Just "a", Nothing, Just "h\233llo w\246rld", Just "\128512 emoji"]
    bins = [Just "", Just "\0\1\2", Nothing, Just (BS.replicate 20 0xff)]
    views =
      [ Just ""
      , Just "short"
      , Nothing
      , Just "exactly12byt"
      , Just "thirteen byte"
      , Just "this string is definitely longer than twelve bytes"
      ]
    viewBytes = [Just "", Just "short", Nothing, Just "exactly12byt", Just "thirteen byte", Just "this string is definitely longer than twelve bytes"]
    timeUnitCases (u, s) =
      concat
        [ primPair ("timestamp_" ++ s) (ATimestamp u Nothing) ColTimestamp ColTimestampMaybe ts [42]
        , primPair ("timestamp_" ++ s ++ "_utc") (ATimestamp u (Just "UTC")) ColTimestamp ColTimestampMaybe ts [42]
        , primPair ("duration_" ++ s) (ADuration u) ColDuration ColDurationMaybe [Just 0, Just 60, Nothing, Just (-1)] [1]
        ]
    ts = [Just 0, Just 1700000000, Nothing, Just (-1)]
    decimalPair n ty plain nullable w b1 b2 =
      pair
        n
        ty
        []
        (plain . V.fromList . map (decimalBytes w))
        (nullable . V.fromList . map (fmap (decimalBytes w)))
        b1
        b2


structCases :: [Case]
structCases =
  [ simple
      "struct"
      (field "x" False AStruct kids)
      [ ColStruct (V.fromList [("i", i32 [1, 2, 3]), ("s", utf8m [Just "a", Nothing, Just "c"])])
      , ColStruct (V.fromList [("i", i32 [4]), ("s", utf8m [Just "d"])])
      ]
  , simple
      "struct_nullable"
      (field "x" True AStruct kids)
      [ ColStructMaybe
          (V.fromList [True, False, True, True])
          (V.fromList [("i", i32 [1, 0, 2, 3]), ("s", utf8m [Just "a", Nothing, Nothing, Just "c"])])
      , ColStructMaybe (V.fromList [True]) (V.fromList [("i", i32 [4]), ("s", utf8m [Just "d"])])
      ]
  ]
  where
    kids = [field "i" False (AInt 32 True) [], field "s" True AUtf8 []]


listCases :: [Case]
listCases =
  [ simple
      "list_int32"
      (field "x" False AList [item (AInt 32 True)])
      [ ColList (VP.fromList [0, 2, 2, 5]) (i32m [Just 1, Just 2, Just 3, Nothing, Just 5])
      , ColList (VP.fromList [0, 1, 3]) (i32m [Just 6, Just 7, Just 8])
      ]
  , simple
      "list_int32_nullable"
      (field "x" True AList [item (AInt 32 True)])
      [ ColListMaybe (bools [True, False, True, True]) (VP.fromList [0, 2, 2, 2, 5]) (i32m [Just 1, Just 2, Just 3, Nothing, Just 5])
      , ColListMaybe (bools [True, True]) (VP.fromList [0, 1, 3]) (i32m [Just 6, Just 7, Just 8])
      ]
  , simple
      "large_list_utf8"
      (field "x" False ALargeList [item AUtf8])
      [ ColLargeList (VP.fromList [0, 2, 2, 3]) (utf8m [Just "a", Just "b", Just "c"])
      , ColLargeList (VP.fromList [0, 1]) (utf8m [Just "d"])
      ]
  , simple
      "large_list_utf8_nullable"
      (field "x" True ALargeList [item AUtf8])
      [ ColLargeListMaybe (bools [True, False, True, True]) (VP.fromList [0, 2, 2, 2, 3]) (utf8m [Just "a", Just "b", Just "c"])
      , ColLargeListMaybe (bools [True]) (VP.fromList [0, 1]) (utf8m [Just "d"])
      ]
  , simple
      "fixed_size_list_int16_3"
      (field "x" False (AFixedSizeList 3) [item (AInt 16 True)])
      [ ColFixedSizeList 3 (i16m [Just 1, Just 2, Just 3, Just 4, Nothing, Just 6])
      , ColFixedSizeList 3 (i16m [Just 7, Just 8, Just 9])
      ]
  , simple
      "fixed_size_list_int16_3_nullable"
      (field "x" True (AFixedSizeList 3) [item (AInt 16 True)])
      [ ColFixedSizeListMaybe 3 (bools [True, False, True]) (i16m [Just 1, Just 2, Just 3, Nothing, Nothing, Nothing, Just 4, Nothing, Just 6])
      , ColFixedSizeListMaybe 3 (bools [True]) (i16m [Just 7, Just 8, Just 9])
      ]
  , simple
      "map_utf8_int32"
      (field "x" False (AMap False) [entries])
      [ ColMap (VP.fromList [0, 2, 2, 3]) (ColUtf8 (V.fromList ["a", "b", "c"])) (i32m [Just 1, Nothing, Just 3])
      , ColMap (VP.fromList [0, 1]) (ColUtf8 (V.fromList ["d"])) (i32m [Just 4])
      ]
  , simple
      "map_utf8_int32_nullable"
      (field "x" True (AMap False) [entries])
      [ ColMapMaybe (bools [True, False, True, True]) (VP.fromList [0, 2, 2, 2, 3]) (ColUtf8 (V.fromList ["a", "b", "c"])) (i32m [Just 1, Nothing, Just 3])
      , ColMapMaybe (bools [True]) (VP.fromList [0, 1]) (ColUtf8 (V.fromList ["d"])) (i32m [Just 4])
      ]
  ]
  where
    i16m = ColInt16Maybe . V.fromList
    entries =
      field "entries" False AStruct [field "key" False AUtf8 [], field "value" True (AInt 32 True) []]


item :: ArrowType -> Field
item ty = field "item" True ty []


bools :: [Bool] -> V.Vector Bool
bools = V.fromList


listViewCases :: [Case]
listViewCases =
  [ simple
      "list_view_int32"
      (field "x" False AListView [item (AInt 32 True)])
      [ ColListView (VP.fromList [4, 0, 1, 0]) (VP.fromList [2, 3, 0, 1]) child
      , ColListView (VP.fromList [0]) (VP.fromList [1]) child2
      ]
  , simple
      "list_view_int32_nullable"
      (field "x" True AListView [item (AInt 32 True)])
      [ ColListViewMaybe (bools [True, False, True, True, True]) (VP.fromList [4, 0, 1, 0, 2]) (VP.fromList [2, 0, 0, 1, 2]) child
      , ColListViewMaybe (bools [True]) (VP.fromList [0]) (VP.fromList [1]) child2
      ]
  , simple
      "large_list_view_int32"
      (field "x" False ALargeListView [item (AInt 32 True)])
      [ ColLargeListView (VP.fromList [4, 0, 1, 0]) (VP.fromList [2, 3, 0, 1]) child
      , ColLargeListView (VP.fromList [0]) (VP.fromList [1]) child2
      ]
  , simple
      "large_list_view_int32_nullable"
      (field "x" True ALargeListView [item (AInt 32 True)])
      [ ColLargeListViewMaybe (bools [True, False, True, True, True]) (VP.fromList [4, 0, 1, 0, 2]) (VP.fromList [2, 0, 0, 1, 2]) child
      , ColLargeListViewMaybe (bools [True]) (VP.fromList [0]) (VP.fromList [1]) child2
      ]
  ]
  where
    child = i32m [Just 10, Just 20, Just 30, Nothing, Just 50, Just 60]
    child2 = i32m [Just 99]


{- | Union columns store per-row /child indices/; the field's type codes
map them to wire type ids.
-}
unionCases :: [Case]
unionCases =
  [ dense "dense_union" [0, 1] False
  , dense "dense_union_nullable" [0, 1] True
  , sparse "sparse_union" [0, 1] False
  , sparse "sparse_union_nullable" [0, 1] True
  , dense "dense_union_type_codes" [3, 7] True
  , sparse "sparse_union_type_codes" [5, 2] True
  ]
  where
    kids = [field "i" True (AInt 32 True) [], field "s" True AUtf8 []]
    ufield mode codes nullable = field "x" nullable (AUnion mode (V.fromList codes)) kids
    tids = VP.fromList :: [Int8] -> VP.Vector Int8
    dense n codes nullable =
      simple
        n
        (ufield Dense codes nullable)
        [ ColDenseUnion
            (tids [0, 1, 0, 0, 1])
            (VP.fromList [0, 0, 1, 2, 1])
            (V.fromList [i32m [Just 1, Just 2, Nothing], utf8m [Just "a", Just "b"]])
        , ColDenseUnion (tids [1]) (VP.fromList [0]) (V.fromList [i32m [], utf8m [Just "z"]])
        ]
    sparse n codes nullable =
      simple
        n
        (ufield Sparse codes nullable)
        [ ColSparseUnion
            (tids [0, 1, 0, 1])
            (V.fromList [i32m [Just 1, Just 0, Nothing, Just 0], utf8m [Nothing, Just "a", Just "x", Just "b"]])
        , ColSparseUnion (tids [1]) (V.fromList [i32m [Just 0], utf8m [Just "z"]])
        ]


abc :: ColumnArray
abc = ColUtf8 (V.fromList ["a", "b", "c"])


dictCases :: [Case]
dictCases =
  [ simple
      "dict_utf8"
      (dictField "x" False AUtf8 0 i32t)
      [ColDictionary 0 (ix [0, 1, 0, 2, 1]) abc, ColDictionary 0 (ix [2, 2, 0]) abc]
  , simple
      "dict_utf8_nullable"
      (dictField "x" True AUtf8 0 i32t)
      [ ColDictionaryMaybe 0 (V.fromList [Just 0, Nothing, Just 1, Just 2]) abc
      , ColDictionaryMaybe 0 (V.fromList [Just 1, Just 0]) abc
      ]
  , simple
      "dict_int8_index"
      (dictField "x" False AUtf8 0 (AInt 8 True))
      [ColDictionary 0 (ix [1, 0, 1]) xy, ColDictionary 0 (ix [0]) xy]
  , simple
      "dict_int64_values"
      (dictField "x" False (AInt 64 True) 0 i32t)
      [ColDictionary 0 (ix [1, 0, 1]) (i64 [100, 200]), ColDictionary 0 (ix [0]) (i64 [100, 200])]
  , (simple "dict_replacement" (dictField "x" False AUtf8 0 i32t) [ColDictionary 0 (ix [0, 1, 1]) ab, ColDictionary 0 (ix [2, 0, 1]) xyz])
      { caseStreamOnly = True
      , caseReplaceDicts = True
      }
  , (simple "dict_delta" (dictField "x" False AUtf8 0 i32t) [ColDictionary 0 (ix [0, 1]) ab, ColDictionary 0 (ix [2, 0]) abc])
      { caseStreamOnly = True
      , caseReplaceDicts = True
      }
  , simple
      "dict_in_struct"
      (field "x" False AStruct [dictField "d" True AUtf8 0 i32t])
      [ ColStruct (V.singleton ("d", ColDictionaryMaybe 0 (V.fromList [Just 2, Just 0, Just 1]) abc))
      , ColStruct (V.singleton ("d", ColDictionaryMaybe 0 (V.fromList [Just 1]) abc))
      ]
  ]
  where
    i32t = AInt 32 True
    ix = VP.fromList :: [Int32] -> VP.Vector Int32
    xy = ColUtf8 (V.fromList ["x", "y"])
    ab = ColUtf8 (V.fromList ["a", "b"])
    xyz = ColUtf8 (V.fromList ["x", "y", "z"])


reeCases :: [Case]
reeCases =
  [ ree "ree_int32_int64" False (AInt 32 True) (AInt 64 True) [(i32 [3, 5, 8], int64m [Just 100, Just 200, Just 300]), (i32 [2], int64m [Just 7])]
  , ree "ree_int32_int64_nullable" True (AInt 32 True) (AInt 64 True) [(i32 [3, 5, 8], int64m [Just 100, Nothing, Just 300]), (i32 [2], int64m [Just 7])]
  , ree "ree_int16_utf8" True (AInt 16 True) AUtf8 [(i16 [1, 4], utf8m [Just "a", Just "bb"]), (i16 [3], utf8m [Just "c"])]
  , ree "ree_int64_float64" True (AInt 64 True) (AFloatingPoint DoublePrecision) [(i64 [2, 3], dblm [Just 1.5, Nothing]), (i64 [1], dblm [Just 2.5])]
  ]
  where
    int64m = ColInt64Maybe . V.fromList
    dblm = ColDoubleMaybe . V.fromList
    ree n nullable reTy valTy batches =
      simple
        n
        (field "x" nullable ARunEndEncoded [field "run_ends" False reTy [], field "values" True valTy []])
        (map (uncurry ColRunEndEncoded) batches)


metadataCase :: Case
metadataCase =
  Case
    { caseName = "custom_metadata"
    , caseSchema =
        Schema
          { arrowFields =
              V.fromList
                [ (field "a" False (AInt 32 True) []) {fieldMetadata = V.fromList [("unit", "ms"), ("note", "field level")]}
                , (field "b" True AUtf8 []) {fieldMetadata = V.fromList [("k", "v")]}
                ]
          , arrowEndianness = Little
          , arrowMetadata = V.fromList [("origin", "wireform-arrow interop"), ("k2", "v2")]
          , arrowFeatures = V.empty
          }
    , caseBatches =
        [ V.fromList [i32 [1, 2], utf8m [Just "x", Nothing]]
        , V.fromList [i32 [3], utf8m [Just "y"]]
        ]
    , caseStreamOnly = False
    , caseReplaceDicts = False
    }


mixedCases :: [Case]
mixedCases =
  [ multi "mixed" [m1, m2]
  , multi "zero_row_batches" [m1, m0, m2]
  , multi "only_zero_row_batch" [m0]
  , multi "no_batches" []
  ]
  where
    multi n bs = Case n mixedSchema bs False False
    mixedSchema =
      schemaOf
        [ field "i" False (AInt 64 True) []
        , field "s" True AUtf8 []
        , field "b" True ABool []
        , field "l" True AList [item (AInt 32 True)]
        , field "st" True AStruct [field "f" True (AFloatingPoint DoublePrecision) []]
        , dictField "d" True AUtf8 0 (AInt 32 True)
        , field "v" True AUtf8View []
        ]
    mk i s b (lv, lo, lc) (sv, sf) d v =
      V.fromList
        [ i64 i
        , utf8m s
        , ColBoolMaybe (V.fromList b)
        , ColListMaybe (bools lv) (VP.fromList lo) (i32m lc)
        , ColStructMaybe (bools sv) (V.singleton ("f", ColDoubleMaybe (V.fromList sf)))
        , ColDictionaryMaybe 0 (V.fromList d) abc
        , ColUtf8ViewMaybe (V.fromList v)
        ]
    m1 =
      mk
        [10, 20, 30]
        [Just "hello", Nothing, Just "!"]
        [Just True, Nothing, Just False]
        ([True, False, True], [0, 1, 1, 3], [Just 1, Just 2, Just 3])
        ([True, False, True], [Just 1.5, Nothing, Just 2.5])
        [Just 0, Just 2, Nothing]
        [Just "v", Nothing, Just "a view longer than twelve"]
    m0 = mk [] [] [] ([], [0], []) ([], []) [] []
    m2 =
      mk
        [40]
        [Just "w"]
        [Just True]
        ([True], [0, 0], [])
        ([True], [Just 0.5])
        [Just 1]
        [Just "z"]


-- ============================================================
-- Logical values: layout-independent row contents, used to compare
-- what wireform-arrow decoded with what the case expects.
-- ============================================================

data LV
  = LNull
  | LInt !Integer
  | LDouble !Double
  | LBool !Bool
  | LText !Text
  | LBytes !ByteString
  | LList [LV]
  | LStruct [(Text, LV)]
  | LMap [(LV, LV)]
  | LUnion !Int32 LV
  deriving stock (Eq, Show)


type Rows = V.Vector LV


ints :: (VP.Prim a, Integral a) => VP.Vector a -> Rows
ints = V.fromList . map (LInt . toInteger) . VP.toList


mints :: Integral a => V.Vector (Maybe a) -> Rows
mints = V.map (maybe LNull (LInt . toInteger))


opt :: (a -> LV) -> V.Vector (Maybe a) -> Rows
opt f = V.map (maybe LNull f)


-- | Signed value of a little-endian two's-complement byte string.
signedLE :: ByteString -> Integer
signedLE bs =
  let u = BS.foldr (\b acc -> acc * 256 + toInteger b) 0 bs
      n = BS.length bs
  in if n > 0 && testBit (BS.last bs) 7 then u - (1 `shiftL` (8 * n)) else u


childField :: Field -> Int -> Either String Field
childField f i =
  maybe (Left ("field " ++ show (fieldName f) ++ " has no child " ++ show i)) Right (fieldChildren f V.!? i)


sliceRows :: Rows -> Int -> Int -> Either String [LV]
sliceRows rs o n
  | o < 0 || n < 0 || o + n > V.length rs =
      Left ("slice [" ++ show o ++ ", +" ++ show n ++ ") outside child of length " ++ show (V.length rs))
  | otherwise = Right (V.toList (V.slice o n rs))


at :: Rows -> Int -> Either String LV
at rs i = maybe (Left ("index " ++ show i ++ " outside child of length " ++ show (V.length rs))) Right (rs V.!? i)


validAt :: Maybe (V.Vector Bool) -> Int -> Bool
validAt mv i = maybe True (\v -> fromMaybe False (v V.!? i)) mv


-- | Rows of an offsets-based list column.
offsetLists :: Maybe (V.Vector Bool) -> [Int] -> Rows -> Either String Rows
offsetLists valid offs cr = do
  let spans = zip offs (drop 1 offs)
  case valid of
    Just v | V.length v /= length spans -> Left "validity length differs from offsets length - 1"
    _ -> Right ()
  V.fromList
    <$> traverse
      (\(i, (o, e)) -> if validAt valid i then LList <$> sliceRows cr o (e - o) else Right LNull)
      (zip [0 ..] spans)


viewLists :: Maybe (V.Vector Bool) -> [Int] -> [Int] -> Rows -> Either String Rows
viewLists valid offs sizes cr =
  V.fromList
    <$> traverse
      (\(i, (o, s)) -> if validAt valid i then LList <$> sliceRows cr o s else Right LNull)
      (zip [0 ..] (zip offs sizes))


rowsOf :: Field -> ColumnArray -> Either String Rows
rowsOf f col = case col of
  ColInt8 v -> Right (ints v)
  ColInt16 v -> Right (ints v)
  ColInt32 v -> Right (ints v)
  ColInt64 v -> Right (ints v)
  ColUInt8 v -> Right (ints v)
  ColUInt16 v -> Right (ints v)
  ColUInt32 v -> Right (ints v)
  ColUInt64 v -> Right (ints v)
  ColFloat16 v -> Right (ints v)
  ColFloat v -> Right (V.fromList (map (LDouble . float2Double) (VP.toList v)))
  ColDouble v -> Right (V.fromList (map LDouble (VP.toList v)))
  ColBool v -> Right (V.map LBool v)
  ColUtf8 v -> Right (V.map LText v)
  ColBinary v -> Right (V.map LBytes v)
  ColLargeUtf8 v -> Right (V.map LText v)
  ColLargeBinary v -> Right (V.map LBytes v)
  ColFixedSizeBinary _ v -> Right (V.map LBytes v)
  ColDate32 v -> Right (ints v)
  ColDate64 v -> Right (ints v)
  ColTime32 v -> Right (ints v)
  ColTime64 v -> Right (ints v)
  ColTimestamp v -> Right (ints v)
  ColDuration v -> Right (ints v)
  ColDecimal128 _ _ v -> Right (V.map (LInt . signedLE) v)
  ColDecimal256 _ _ v -> Right (V.map (LInt . signedLE) v)
  ColIntervalYearMonth v -> Right (ints v)
  ColIntervalDayTime ds ms -> Right (V.fromList (zipWith dayTime (VP.toList ds) (VP.toList ms)))
  ColIntervalMonthDayNano ms ds ns ->
    Right (V.fromList (zipWith3 (\m d n -> monthDayNano (m, d, n)) (VP.toList ms) (VP.toList ds) (VP.toList ns)))
  ColInt8Maybe v -> Right (mints v)
  ColInt16Maybe v -> Right (mints v)
  ColInt32Maybe v -> Right (mints v)
  ColInt64Maybe v -> Right (mints v)
  ColUInt8Maybe v -> Right (mints v)
  ColUInt16Maybe v -> Right (mints v)
  ColUInt32Maybe v -> Right (mints v)
  ColUInt64Maybe v -> Right (mints v)
  ColFloat16Maybe v -> Right (mints v)
  ColFloatMaybe v -> Right (opt (LDouble . float2Double) v)
  ColDoubleMaybe v -> Right (opt LDouble v)
  ColBoolMaybe v -> Right (opt LBool v)
  ColUtf8Maybe v -> Right (opt LText v)
  ColBinaryMaybe v -> Right (opt LBytes v)
  ColLargeUtf8Maybe v -> Right (opt LText v)
  ColLargeBinaryMaybe v -> Right (opt LBytes v)
  ColFixedSizeBinaryMaybe _ v -> Right (opt LBytes v)
  ColDate32Maybe v -> Right (mints v)
  ColDate64Maybe v -> Right (mints v)
  ColTime32Maybe v -> Right (mints v)
  ColTime64Maybe v -> Right (mints v)
  ColTimestampMaybe v -> Right (mints v)
  ColDurationMaybe v -> Right (mints v)
  ColDecimal128Maybe _ _ v -> Right (opt (LInt . signedLE) v)
  ColDecimal256Maybe _ _ v -> Right (opt (LInt . signedLE) v)
  ColIntervalYearMonthMaybe v -> Right (mints v)
  ColIntervalDayTimeMaybe v -> Right (opt (uncurry dayTime) v)
  ColIntervalMonthDayNanoMaybe v -> Right (opt monthDayNano v)
  ColStruct cs -> structRows Nothing cs
  ColStructMaybe valid cs -> structRows (Just valid) cs
  ColList offs c -> do
    cr <- childRows 0 c
    offsetLists Nothing (map fromIntegral (VP.toList offs)) cr
  ColListMaybe valid offs c -> do
    cr <- childRows 0 c
    offsetLists (Just valid) (map fromIntegral (VP.toList offs)) cr
  ColLargeList offs c -> do
    cr <- childRows 0 c
    offsetLists Nothing (map fromIntegral (VP.toList offs)) cr
  ColLargeListMaybe valid offs c -> do
    cr <- childRows 0 c
    offsetLists (Just valid) (map fromIntegral (VP.toList offs)) cr
  ColFixedSizeList w c -> do
    cr <- childRows 0 c
    fixedLists Nothing w (if w > 0 then V.length cr `div` w else 0) cr
  ColFixedSizeListMaybe w valid c -> do
    cr <- childRows 0 c
    fixedLists (Just valid) w (V.length valid) cr
  ColMap offs ks vs -> mapRows Nothing offs ks vs
  ColMapMaybe valid offs ks vs -> mapRows (Just valid) offs ks vs
  ColDenseUnion tids offs cs -> do
    crs <- V.imapM childRows cs
    V.fromList
      <$> traverse
        (\(t, o) -> do
            rs <- maybe (Left ("union child index " ++ show t ++ " out of range")) Right (crs V.!? fromIntegral t)
            LUnion (typeCode t) <$> at rs (fromIntegral o)
        )
        (zip (VP.toList tids) (VP.toList offs))
  ColSparseUnion tids cs -> do
    crs <- V.imapM childRows cs
    V.fromList
      <$> traverse
        (\(i, t) -> do
            rs <- maybe (Left ("union child index " ++ show t ++ " out of range")) Right (crs V.!? fromIntegral t)
            LUnion (typeCode t) <$> at rs i
        )
        (zip [0 ..] (VP.toList tids))
  ColDictionary _ idx vals -> do
    vr <- rowsOf (f {fieldDictionary = Nothing}) vals
    V.fromList <$> traverse (at vr . fromIntegral) (VP.toList idx)
  ColDictionaryMaybe _ idx vals -> do
    vr <- rowsOf (f {fieldDictionary = Nothing}) vals
    V.fromList <$> traverse (maybe (Right LNull) (at vr . fromIntegral)) (V.toList idx)
  ColRunEndEncoded ends vals -> do
    er <- childRows 0 ends
    vr <- childRows 1 vals
    endsI <- traverse (\case LInt e -> Right (fromInteger e); other -> Left ("non-integer run end " ++ show other)) (V.toList er)
    let expand _ [] _ = Right []
        expand prev (e : es) k
          | e < prev = Left "run ends are not ascending"
          | otherwise = do
              v <- at vr k
              rest <- expand e es (k + 1)
              Right (replicate (e - prev) v ++ rest)
    V.fromList <$> expand (0 :: Int) endsI 0
  ColListView offs sizes c -> do
    cr <- childRows 0 c
    viewLists Nothing (map fromIntegral (VP.toList offs)) (map fromIntegral (VP.toList sizes)) cr
  ColListViewMaybe valid offs sizes c -> do
    cr <- childRows 0 c
    viewLists (Just valid) (map fromIntegral (VP.toList offs)) (map fromIntegral (VP.toList sizes)) cr
  ColLargeListView offs sizes c -> do
    cr <- childRows 0 c
    viewLists Nothing (map fromIntegral (VP.toList offs)) (map fromIntegral (VP.toList sizes)) cr
  ColLargeListViewMaybe valid offs sizes c -> do
    cr <- childRows 0 c
    viewLists (Just valid) (map fromIntegral (VP.toList offs)) (map fromIntegral (VP.toList sizes)) cr
  ColUtf8View v -> Right (V.map LText v)
  ColUtf8ViewMaybe v -> Right (opt LText v)
  ColBinaryView v -> Right (V.map LBytes v)
  ColBinaryViewMaybe v -> Right (opt LBytes v)
  ColNull n -> Right (V.replicate n LNull)
  where
    childRows i c = do
      cf <- childField f i
      rowsOf cf c
    dayTime d m = LList [LInt (toInteger d), LInt (toInteger m)]
    monthDayNano (m, d, n) = LList [LInt (toInteger m), LInt (toInteger d), LInt (toInteger n)]
    typeCode :: Int8 -> Int32
    typeCode t = case fieldType f of
      AUnion _ codes | not (V.null codes) -> fromMaybe (-1) (codes V.!? fromIntegral t)
      _ -> fromIntegral t
    structRows valid cs = do
      crs <- V.imapM (\i (nm, c) -> (,) nm <$> childRows i c) cs
      let n = maybe (if V.null crs then 0 else V.length (snd (V.head crs))) V.length valid
      V.generateM n $ \j ->
        if validAt valid j
          then LStruct . V.toList <$> traverse (\(nm, rs) -> (,) nm <$> at rs j) crs
          else Right LNull
    fixedLists valid w n cr =
      V.generateM n $ \i ->
        if validAt valid i then LList <$> sliceRows cr (i * w) w else Right LNull
    mapRows valid offs ks vs = do
      entries <- childField f 0
      kf <- childField entries 0
      vf <- childField entries 1
      kr <- rowsOf kf ks
      vr <- rowsOf vf vs
      unless (V.length kr == V.length vr) (Left "map keys and values differ in length")
      lists <- offsetLists valid (map fromIntegral (VP.toList offs)) (V.zipWith (\k v -> LList [k, v]) kr vr)
      Right (V.map (\case LList kvs -> LMap (map (\case LList [k, v] -> (k, v); other -> (other, LNull)) kvs); other -> other) lists)


-- | Schemas compare equal up to the order of custom-metadata pairs.
normaliseSchema :: Schema -> Schema
normaliseSchema s =
  s
    { arrowFields = V.map normaliseField (arrowFields s)
    , arrowMetadata = sortMeta (arrowMetadata s)
    }
  where
    normaliseField fl =
      fl
        { fieldChildren = V.map normaliseField (fieldChildren fl)
        , fieldMetadata = sortMeta (fieldMetadata fl)
        }
    sortMeta = V.fromList . sortOn fst . V.toList


-- | Compare a decoded file with the case's expectation.
compareDecoded :: Case -> Schema -> [V.Vector ColumnArray] -> Either String ()
compareDecoded c sch batches = do
  unless (normaliseSchema sch == normaliseSchema (caseSchema c)) $
    Left ("schema mismatch: got " ++ show sch ++ " want " ++ show (caseSchema c))
  unless (length batches == length (caseBatches c)) $
    Left ("got " ++ show (length batches) ++ " batches, want " ++ show (length (caseBatches c)))
  forM_ (zip3 [0 :: Int ..] batches (caseBatches c)) $ \(bi, got, want) -> do
    let fields = arrowFields (caseSchema c)
    unless (V.length got == V.length fields) $
      Left ("batch " ++ show bi ++ ": got " ++ show (V.length got) ++ " columns")
    forM_ (zip3 (V.toList fields) (V.toList got) (V.toList want)) $ \(fl, g, w) -> do
      let ctx = "batch " ++ show bi ++ " column " ++ show (fieldName fl) ++ ": "
      gr <- either (\e -> Left (ctx ++ "decoded column is malformed: " ++ e ++ " (" ++ show g ++ ")")) Right (rowsOf fl g)
      wr <- either (\e -> Left (ctx ++ "expected column is malformed: " ++ e)) Right (rowsOf fl w)
      unless (gr == wr) $
        Left (ctx ++ "got " ++ show (V.toList gr) ++ " want " ++ show (V.toList wr) ++ " (decoded " ++ show g ++ ")")


-- ============================================================
-- Driver
-- ============================================================

data Tally = Tally {tPass :: !Int, tFail :: !Int, tSkip :: !Int, tFailures :: [String]}


report :: IORef Tally -> String -> String -> Either String () -> IO ()
report ref dir name = \case
  Right () -> do
    putStrLn ("[" ++ dir ++ "] PASS " ++ name)
    modifyIORef' ref (\t -> t {tPass = tPass t + 1})
  Left e -> do
    let line = "[" ++ dir ++ "] FAIL " ++ name ++ ": " ++ truncateMsg e
    putStrLn line
    modifyIORef' ref (\t -> t {tFail = tFail t + 1, tFailures = line : tFailures t})


skip :: IORef Tally -> String -> String -> String -> IO ()
skip ref dir name why = do
  putStrLn ("[" ++ dir ++ "] SKIP " ++ name ++ ": " ++ why)
  modifyIORef' ref (\t -> t {tSkip = tSkip t + 1})


truncateMsg :: String -> String
truncateMsg s = if length s > 2000 then take 2000 s ++ " ..." else s


-- | Body-compression variants this build of wireform-arrow supports.
compressionVariants :: [(String, Maybe BodyCompressionCodec)]
compressionVariants =
  [("plain", Nothing)]
#ifdef HAVE_ZSTD
    ++ [("zstd", Just BodyZstd)]
#endif
#ifdef HAVE_LZ4
    ++ [("lz4", Just LZ4Frame)]
#endif


-- | Whether this build can decode files pyarrow wrote with @variant@.
variantSupported :: String -> Bool
variantSupported v = case lookup v compressionVariants of
  Just _ -> True
  Nothing -> v == "sliced"


{- | Write every case into @dir@ as @<case>.<variant>.arrows@ (and
@.arrow@ unless stream-only). Returns the file names written plus the
cases whose encoder threw.
-}
writeHaskellFiles :: FilePath -> IO ([String], [(String, String)])
writeHaskellFiles dir = do
  createDirectoryIfMissing True dir
  results <- forM cases $ \c -> forM compressionVariants $ \(variant, codec) -> do
    let opts =
          defaultWriteOptions
            { writeBodyCompression = codec
            , writeDictHandling = if caseReplaceDicts c then DictReplaceOnChange else DictEmitOnce
            }
        base = caseName c ++ "." ++ variant
        outputs =
          (base ++ ".arrows", encodeArrowStream opts (caseSchema c) (caseBatches c))
            : if caseStreamOnly c then [] else [(base ++ ".arrow", encodeArrowFile opts (caseSchema c) (caseBatches c))]
    forM outputs $ \(name, bytes) -> do
      r <- try (evaluate (force bytes)) :: IO (Either SomeException ByteString)
      case r of
        Right bs -> do
          BS.writeFile (dir </> name) bs
          pure (Right name)
        Left e -> pure (Left (name, "wireform-arrow encoder threw: " ++ show e))
  let (failures, written) = partitionEithers (concat (concat results))
  pure (written, failures)


runPython :: FilePath -> FilePath -> [String] -> IO (Either String [String])
runPython python script args = do
  r <- try (readProcessWithExitCode python (script : args) "") :: IO (Either SomeException (ExitCode, String, String))
  pure $ case r of
    Left e -> Left ("could not run " ++ python ++ ": " ++ show e)
    Right (ExitSuccess, out, _) -> Right (lines out)
    Right (ExitFailure n, out, err) ->
      Left (python ++ " " ++ unwords (script : args) ++ " exited " ++ show n ++ ": " ++ err ++ out)


-- | Direction A: wireform-arrow writes, pyarrow reads.
directionA :: IORef Tally -> FilePath -> FilePath -> FilePath -> IO ()
directionA ref python script tmp = do
  let dir = tmp </> "wireform"
  (written, encodeFailures) <- writeHaskellFiles dir
  forM_ encodeFailures $ \(name, e) -> report ref "A" name (Left e)
  out <- runPython python script ["check", dir]
  case out of
    Left e -> report ref "A" "pyarrow checker" (Left e)
    Right ls -> do
      let verdicts =
            Map.fromList
              ( mapMaybe
                  ( \l -> case stripPrefix "PASS " l of
                      Just n -> Just (n, Right ())
                      Nothing -> case stripPrefix "FAIL " l of
                        Just rest -> let (n, msg) = break (== ':') rest in Just (n, Left (drop 2 msg))
                        Nothing -> Nothing
                  )
                  ls
              )
      forM_ written $ \name ->
        report ref "A" name (fromMaybe (Left ("pyarrow checker printed no verdict; output: " ++ unlines ls)) (Map.lookup name verdicts))


-- | Direction B: pyarrow writes, wireform-arrow reads.
directionB :: IORef Tally -> FilePath -> FilePath -> FilePath -> IO ()
directionB ref python script tmp = do
  out <- runPython python script ["write", dir]
  case out of
    Left e -> report ref "B" "pyarrow writer" (Left e)
    Right ls -> forM_ ls $ \l -> case words l of
      ("WROTE" : name : _) -> checkFile name
      ("SKIP" : name : _) -> skip ref "B" (takeWhile (/= ':') name) (drop 2 (dropWhile (/= ':') l))
      ("FAIL" : name : _) -> report ref "B" (takeWhile (/= ':') name) (Left (drop 2 (dropWhile (/= ':') l)))
      _ -> pure ()
  where
    byName = Map.fromList (map (\c -> (caseName c, c)) cases)
    checkFile name = case splitName name of
      Nothing -> report ref "B" name (Left "unexpected file name")
      Just (cn, variant, ext)
        | not (variantSupported variant) ->
            skip ref "B" name ("wireform-arrow built without -f+" ++ variant)
        | otherwise -> case Map.lookup cn byName of
            Nothing -> report ref "B" name (Left ("no Haskell-side case named " ++ cn))
            Just c -> do
              bytes <- BS.readFile (dir </> name)
              let decode = if ext == "arrow" then decodeArrowFile else decodeArrowStream
              r <- try (evaluate (force (decode bytes))) :: IO (Either SomeException (Either String (Schema, [V.Vector ColumnArray])))
              report ref "B" name $ case r of
                Left e -> Left ("wireform-arrow decoder threw: " ++ show e)
                Right (Left e) -> Left ("wireform-arrow failed to decode: " ++ e)
                Right (Right (sch, batches)) -> compareDecoded c sch batches
    dir = tmp </> "pyarrow"


splitName :: String -> Maybe (String, String, String)
splitName name = case splitOn '.' name of
  [c, v, e] -> Just (c, v, e)
  _ -> Nothing
  where
    splitOn ch s = case break (== ch) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn ch rest


-- | Case names must agree between the two definitions of the matrix.
checkCaseNames :: IORef Tally -> FilePath -> FilePath -> IO ()
checkCaseNames ref python script = do
  out <- runPython python script ["cases"]
  case out of
    Left e -> report ref "matrix" "case list" (Left e)
    Right ls -> do
      let py = Set.fromList (filter (not . null) ls)
          hs = Set.fromList (map caseName cases)
      forM_ (Set.toList (Set.difference py hs)) $ \n ->
        report ref "matrix" n (Left "case exists in pyarrow_interop.py but not in Main.hs")
      forM_ (Set.toList (Set.difference hs py)) $ \n ->
        report ref "matrix" n (Left "case exists in Main.hs but not in pyarrow_interop.py")
      when (py == hs) $ putStrLn ("[matrix] " ++ show (Set.size hs) ++ " cases defined on both sides")


findScript :: IO (Maybe FilePath)
findScript = do
  override <- lookupEnv "WIREFORM_ARROW_INTEROP_SCRIPT"
  let candidates =
        maybe [] pure override
          ++ ["test-interop/pyarrow_interop.py", "wireform-arrow/test-interop/pyarrow_interop.py"]
  firstExisting candidates
  where
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstExisting ps


main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  case args of
    ["--write", dir] -> do
      (written, failures) <- writeHaskellFiles dir
      putStrLn ("wrote " ++ show (length written) ++ " files to " ++ dir)
      forM_ failures $ \(n, e) -> putStrLn ("FAIL " ++ n ++ ": " ++ e)
      if null failures then exitSuccess else exitFailure
    [] -> runSuite
    _ -> do
      putStrLn "usage: wireform-arrow-pyarrow-interop [--write DIR]"
      exitFailure


runSuite :: IO ()
runSuite = do
  python <- fromMaybe "python3" <$> lookupEnv "WIREFORM_ARROW_PYTHON"
  required <- (== Just "1") <$> lookupEnv "WIREFORM_ARROW_REQUIRE_PYARROW"
  probe <- try (readProcessWithExitCode python ["-c", "import pyarrow; print(pyarrow.__version__)"] "") :: IO (Either SomeException (ExitCode, String, String))
  let unavailable why =
        if required
          then do
            putStrLn ("FAIL: pyarrow is required (WIREFORM_ARROW_REQUIRE_PYARROW=1) but unavailable: " ++ why)
            exitFailure
          else do
            putStrLn ("SKIP: pyarrow interop suite (" ++ why ++ "); set WIREFORM_ARROW_PYTHON to a Python with pyarrow, and WIREFORM_ARROW_REQUIRE_PYARROW=1 to make this a failure")
            exitSuccess
  version <- case probe of
    Left e -> unavailable ("cannot run " ++ python ++ ": " ++ show e)
    Right (ExitSuccess, out, _) -> pure (concat (take 1 (lines out)))
    Right (_, _, err) -> unavailable (python ++ " cannot import pyarrow: " ++ concat (take 1 (reverse (lines err))))
  script <-
    findScript >>= \case
      Just s -> pure s
      Nothing -> do
        putStrLn "FAIL: cannot find test-interop/pyarrow_interop.py (set WIREFORM_ARROW_INTEROP_SCRIPT)"
        exitFailure
  putStrLn ("pyarrow " ++ version ++ " via " ++ python ++ "; compression variants: " ++ unwords (map fst compressionVariants))
  ref <- newIORef (Tally 0 0 0 [])
  checkCaseNames ref python script
  withSystemTempDirectory "wireform-arrow-interop" $ \tmp -> do
    directionA ref python script tmp
    directionB ref python script tmp
  t <- readIORef ref
  putStrLn ""
  putStrLn ("pyarrow interop: " ++ show (tPass t) ++ " passed, " ++ show (tFail t) ++ " failed, " ++ show (tSkip t) ++ " skipped")
  unless (null (tFailures t)) $ do
    putStrLn "Failures:"
    mapM_ putStrLn (reverse (tFailures t))
    exitFailure
