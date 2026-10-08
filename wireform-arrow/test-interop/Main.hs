{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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

import Arrow.Column (
  ChildRange (..),
  Validity,
  ColumnArray,
  Float16 (..),
  IntervalDayTime (..),
  IntervalMonthDayNano (..),
  PrimType (..),
  asPrim,
  columnLength,
  decimal128FromInteger,
  decimal128ToInteger,
  decimal256FromInteger,
  decimal256ToInteger,
  dictKeyAt,
  expandDictionary,
  fromBools,
  fromByteStrings,
  fromMaybeBinaryView,
  fromMaybeBools,
  fromMaybeByteStrings,
  fromMaybeFixedSizeBinary,
  fromMaybeLargeByteStrings,
  fromMaybeLargeTexts,
  fromMaybeTexts,
  fromMaybeUtf8View,
  fromMaybes,
  fromTexts,
  isValidAt,
  listRange,
  mkDenseUnion,
  mkDictionary,
  mkFixedSizeList,
  mkLargeList,
  mkLargeListView,
  mkList,
  mkListView,
  mkMap,
  mkRunEndEncoded,
  mkSparseUnion,
  mkStruct,
  primColumn,
  toBoolVector,
  toBytesVector,
  toMaybeVector,
  toTextVector,
  validityFromBools,
  withPrim,
  pattern ColBinary,
  pattern ColBinaryView,
  pattern ColBool,
  pattern ColDenseUnion,
  pattern ColDictionary,
  pattern ColFixedSizeBinary,
  pattern ColFixedSizeList,
  pattern ColLargeBinary,
  pattern ColLargeList,
  pattern ColLargeListView,
  pattern ColLargeUtf8,
  pattern ColList,
  pattern ColListView,
  pattern ColMap,
  pattern ColNull,
  pattern ColPrim,
  pattern ColRunEndEncoded,
  pattern ColSparseUnion,
  pattern ColStruct,
  pattern ColUtf8,
  pattern ColUtf8View,
 )
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64, Int8)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sortOn, stripPrefix)
import Data.Either (partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
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


-- | Unwrap a checked fixture constructor.
ok :: Either String ColumnArray -> ColumnArray
ok = either (error . ("bad fixture: " ++)) id


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


-- | 'pair' for a fixed-width element type.
primPair :: String -> ArrowType -> PrimType a -> [Maybe a] -> [a] -> [Case]
primPair n ty p = withPrim p (pair n ty [] (primColumn p . VS.fromList) (fromMaybes p . V.fromList))


-- | 'pair' for a column built from a boxed vector of rows.
boxedPair :: String -> ArrowType -> (V.Vector a -> ColumnArray) -> (V.Vector (Maybe a) -> ColumnArray) -> [Maybe a] -> [a] -> [Case]
boxedPair n ty plain nullable = pair n ty [] (plain . V.fromList) (nullable . V.fromList)


-- | 'boxedPair' for constructors that only take optional rows.
maybePair :: String -> ArrowType -> (V.Vector (Maybe a) -> ColumnArray) -> [Maybe a] -> [a] -> [Case]
maybePair n ty nullable = boxedPair n ty (nullable . V.map Just) nullable


i32, i16, i64 :: [Int32] -> ColumnArray
i32 = primColumn PInt32 . VS.fromList
i16 = primColumn PInt16 . VS.fromList . map fromIntegral
i64 = primColumn PInt64 . VS.fromList . map fromIntegral


i32m :: [Maybe Int32] -> ColumnArray
i32m = fromMaybes PInt32 . V.fromList


utf8m :: [Maybe Text] -> ColumnArray
utf8m = fromMaybeTexts . V.fromList


-- | Validity from per-row flags (True = valid).
valid :: [Bool] -> Maybe Validity
valid = validityFromBools . V.fromList


cases :: [Case]
cases =
  concat
    [ primPair "int8" (AInt 8 True) PInt8 [Just 0, Just 1, Nothing, Just (-1), Just 127, Just (-128)] [42, -42]
    , primPair "int16" (AInt 16 True) PInt16 [Just 0, Just 1, Nothing, Just (-1), Just 32767, Just (-32768)] [1000, -1000]
    , primPair "int32" (AInt 32 True) PInt32 [Just 0, Just 1, Nothing, Just (-1), Just maxBound, Just minBound] [7, -7]
    , primPair "int64" (AInt 64 True) PInt64 [Just 0, Just 1, Nothing, Just (-1), Just maxBound, Just minBound] [123456789012, -5]
    , primPair "uint8" (AInt 8 False) PUInt8 [Just 0, Just 1, Nothing, Just maxBound] [128]
    , primPair "uint16" (AInt 16 False) PUInt16 [Just 0, Just 1, Nothing, Just maxBound] [256]
    , primPair "uint32" (AInt 32 False) PUInt32 [Just 0, Just 1, Nothing, Just maxBound] [65536]
    , primPair "uint64" (AInt 64 False) PUInt64 [Just 0, Just 1, Nothing, Just maxBound] [4294967296]
    , primPair "float16" (AFloatingPoint Half) PFloat16 (map (fmap Float16) [Just 0x0000, Just 0x3C00, Nothing, Just 0xC000, Just 0x7BFF, Just 0x7C00]) [Float16 0x3800]
    , primPair "float32" (AFloatingPoint Single) PFloat [Just 0, Just 1.5, Nothing, Just (-2.5), Just (1 / 0), Just 3.4028234663852886e38] [0.25]
    , primPair "float64" (AFloatingPoint DoublePrecision) PDouble [Just 0, Just 1.5, Nothing, Just (-2.5), Just (-1 / 0), Just 1e300, Just 5e-324] [3.141592653589793]
    , boxedPair "bool" ABool fromBools fromMaybeBools (map Just [True, False] ++ [Nothing] ++ map Just [True, True, False, False, True, False]) [False, True]
    , boxedPair "utf8" AUtf8 fromTexts fromMaybeTexts strs ["second batch"]
    , maybePair "large_utf8" ALargeUtf8 fromMaybeLargeTexts strs ["second batch"]
    , boxedPair "binary" ABinary fromByteStrings fromMaybeByteStrings bins ["z"]
    , maybePair "large_binary" ALargeBinary fromMaybeLargeByteStrings bins ["z"]
    , maybePair "fixed_size_binary4" (AFixedSizeBinary 4) (ok . fromMaybeFixedSizeBinary 4) [Just "abcd", Just "\0\0\0\0", Nothing, Just "\xff\xfe\xfd\xfc"] ["wxyz"]
    , maybePair "utf8_view" AUtf8View fromMaybeUtf8View views ["inline", "an out of line string value"]
    , maybePair "binary_view" ABinaryView fromMaybeBinaryView viewBytes ["inline", BS.replicate 13 0]
    , primPair "date32" (ADate DateDay) PDate32 [Just 0, Just 18000, Nothing, Just (-1), Just 19000] [20000]
    , primPair "date64" (ADate DateMillisecond) PDate64 [Just 0, Just 1555200000000, Nothing, Just (-86400000)] [86400000]
    , primPair "time32_s" (ATime Second 32) PTime32 [Just 0, Just 3600, Nothing, Just 86399] [60]
    , primPair "time32_ms" (ATime Millisecond 32) PTime32 [Just 0, Just 1000, Nothing, Just 86399999] [1]
    , primPair "time64_us" (ATime Microsecond 64) PTime64 [Just 0, Just 12345000000, Nothing, Just 86399999999] [1]
    , primPair "time64_ns" (ATime Nanosecond 64) PTime64 [Just 0, Just 1, Nothing, Just 86399999999999] [2]
    , concatMap timeUnitCases [(Second, "s"), (Millisecond, "ms"), (Microsecond, "us"), (Nanosecond, "ns")]
    , primPair "timestamp_us_new_york" (ATimestamp Microsecond (Just "America/New_York")) PTimestamp [Just 0, Just 1700000000000000, Nothing] [5]
    , decimalPair "decimal128_18_2" (ADecimal 18 2) (PDecimal128 18 2) decimal128FromInteger [Just 0, Just 123, Nothing, Just (-123), Just 999999999999999999] [1]
    , decimalPair "decimal128_38_10" (ADecimal 38 10) (PDecimal128 38 10) decimal128FromInteger [Just 123456789012345678901234567890, Nothing, Just (-1)] [10000000000]
    , decimalPair "decimal256_76_5" (ADecimal256 76 5) (PDecimal256 76 5) decimal256FromInteger [Just 100000, Nothing, Just (-1234567890123456789012345678901234567890123456789012345)] [1]
    , primPair
        "interval_month_day_nano"
        (AInterval MonthDayNano)
        PIntervalMonthDayNano
        [Just (IntervalMonthDayNano 1 2 3), Just (IntervalMonthDayNano 0 0 0), Nothing, Just (IntervalMonthDayNano (-1) (-2) (-3))]
        [IntervalMonthDayNano 12 30 1000000000]
    , primPair "interval_year_month" (AInterval YearMonth) PIntervalYearMonth [Just 0, Just 14, Nothing, Just (-3)] [5]
    , primPair
        "interval_day_time"
        (AInterval DayTime)
        PIntervalDayTime
        [Just (IntervalDayTime 0 0), Just (IntervalDayTime 1 1000), Nothing, Just (IntervalDayTime (-1) (-1))]
        [IntervalDayTime 2 2]
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
        [ primPair ("timestamp_" ++ s) (ATimestamp u Nothing) PTimestamp ts [42]
        , primPair ("timestamp_" ++ s ++ "_utc") (ATimestamp u (Just "UTC")) PTimestamp ts [42]
        , primPair ("duration_" ++ s) (ADuration u) PDuration [Just 0, Just 60, Nothing, Just (-1)] [1]
        ]
    ts = [Just 0, Just 1700000000, Nothing, Just (-1)]
    decimalPair :: String -> ArrowType -> PrimType d -> (Integer -> d) -> [Maybe Integer] -> [Integer] -> [Case]
    decimalPair n ty p conv b1 b2 = primPair n ty p (map (fmap conv) b1) (map conv b2)


structCases :: [Case]
structCases =
  [ simple
      "struct"
      (field "x" False AStruct kids)
      [ ok (mkStruct 3 Nothing (V.fromList [("i", i32 [1, 2, 3]), ("s", utf8m [Just "a", Nothing, Just "c"])]))
      , ok (mkStruct 1 Nothing (V.fromList [("i", i32 [4]), ("s", utf8m [Just "d"])]))
      ]
  , simple
      "struct_nullable"
      (field "x" True AStruct kids)
      [ ok
          ( mkStruct
              4
              (valid [True, False, True, True])
              (V.fromList [("i", i32 [1, 0, 2, 3]), ("s", utf8m [Just "a", Nothing, Nothing, Just "c"])])
          )
      , ok (mkStruct 1 (valid [True]) (V.fromList [("i", i32 [4]), ("s", utf8m [Just "d"])]))
      ]
  , -- pa.struct([]): the row count is all a fieldless struct carries.
    simple "struct_empty" (field "x" False AStruct []) [ok (mkStruct 3 Nothing V.empty), ok (mkStruct 1 Nothing V.empty)]
  , simple
      "struct_empty_nullable"
      (field "x" True AStruct [])
      [ok (mkStruct 4 (valid [True, False, True, True]) V.empty), ok (mkStruct 1 (valid [True]) V.empty)]
  ]
  where
    kids = [field "i" False (AInt 32 True) [], field "s" True AUtf8 []]


listCases :: [Case]
listCases =
  [ simple
      "list_int32"
      (field "x" False AList [item (AInt 32 True)])
      [ ok (mkList Nothing (VS.fromList [0, 2, 2, 5]) (i32m [Just 1, Just 2, Just 3, Nothing, Just 5]))
      , ok (mkList Nothing (VS.fromList [0, 1, 3]) (i32m [Just 6, Just 7, Just 8]))
      ]
  , simple
      "list_int32_nullable"
      (field "x" True AList [item (AInt 32 True)])
      [ ok (mkList (valid [True, False, True, True]) (VS.fromList [0, 2, 2, 2, 5]) (i32m [Just 1, Just 2, Just 3, Nothing, Just 5]))
      , ok (mkList (valid [True, True]) (VS.fromList [0, 1, 3]) (i32m [Just 6, Just 7, Just 8]))
      ]
  , simple
      "large_list_utf8"
      (field "x" False ALargeList [item AUtf8])
      [ ok (mkLargeList Nothing (VS.fromList [0, 2, 2, 3]) (utf8m [Just "a", Just "b", Just "c"]))
      , ok (mkLargeList Nothing (VS.fromList [0, 1]) (utf8m [Just "d"]))
      ]
  , simple
      "large_list_utf8_nullable"
      (field "x" True ALargeList [item AUtf8])
      [ ok (mkLargeList (valid [True, False, True, True]) (VS.fromList [0, 2, 2, 2, 3]) (utf8m [Just "a", Just "b", Just "c"]))
      , ok (mkLargeList (valid [True]) (VS.fromList [0, 1]) (utf8m [Just "d"]))
      ]
  , simple
      "fixed_size_list_int16_3"
      (field "x" False (AFixedSizeList 3) [item (AInt 16 True)])
      [ ok (mkFixedSizeList 3 2 Nothing (i16m [Just 1, Just 2, Just 3, Just 4, Nothing, Just 6]))
      , ok (mkFixedSizeList 3 1 Nothing (i16m [Just 7, Just 8, Just 9]))
      ]
  , simple
      "fixed_size_list_int16_3_nullable"
      (field "x" True (AFixedSizeList 3) [item (AInt 16 True)])
      [ ok (mkFixedSizeList 3 3 (valid [True, False, True]) (i16m [Just 1, Just 2, Just 3, Nothing, Nothing, Nothing, Just 4, Nothing, Just 6]))
      , ok (mkFixedSizeList 3 1 (valid [True]) (i16m [Just 7, Just 8, Just 9]))
      ]
  , -- pa.list_(pa.int32(), 0): rows of zero elements over an empty child.
    simple
      "fixed_size_list_int32_0"
      (field "x" False (AFixedSizeList 0) [item (AInt 32 True)])
      [ok (mkFixedSizeList 0 3 Nothing (i32m [])), ok (mkFixedSizeList 0 2 Nothing (i32m []))]
  , simple
      "fixed_size_list_int32_0_nullable"
      (field "x" True (AFixedSizeList 0) [item (AInt 32 True)])
      [ ok (mkFixedSizeList 0 4 (valid [True, False, True, True]) (i32m []))
      , ok (mkFixedSizeList 0 2 (valid [True, True]) (i32m []))
      ]
  , simple
      "map_utf8_int32"
      (field "x" False (AMap False) [entries])
      [ ok (mkMap Nothing (VS.fromList [0, 2, 2, 3]) (fromTexts (V.fromList ["a", "b", "c"])) (i32m [Just 1, Nothing, Just 3]))
      , ok (mkMap Nothing (VS.fromList [0, 1]) (fromTexts (V.fromList ["d"])) (i32m [Just 4]))
      ]
  , simple
      "map_utf8_int32_nullable"
      (field "x" True (AMap False) [entries])
      [ ok (mkMap (valid [True, False, True, True]) (VS.fromList [0, 2, 2, 2, 3]) (fromTexts (V.fromList ["a", "b", "c"])) (i32m [Just 1, Nothing, Just 3]))
      , ok (mkMap (valid [True]) (VS.fromList [0, 1]) (fromTexts (V.fromList ["d"])) (i32m [Just 4]))
      ]
  ]
  where
    i16m = fromMaybes PInt16 . V.fromList
    entries =
      field "entries" False AStruct [field "key" False AUtf8 [], field "value" True (AInt 32 True) []]


item :: ArrowType -> Field
item ty = field "item" True ty []


listViewCases :: [Case]
listViewCases =
  [ simple
      "list_view_int32"
      (field "x" False AListView [item (AInt 32 True)])
      [ ok (mkListView Nothing (VS.fromList [4, 0, 1, 0]) (VS.fromList [2, 3, 0, 1]) child)
      , ok (mkListView Nothing (VS.fromList [0]) (VS.fromList [1]) child2)
      ]
  , simple
      "list_view_int32_nullable"
      (field "x" True AListView [item (AInt 32 True)])
      [ ok (mkListView (valid [True, False, True, True, True]) (VS.fromList [4, 0, 1, 0, 2]) (VS.fromList [2, 0, 0, 1, 2]) child)
      , ok (mkListView (valid [True]) (VS.fromList [0]) (VS.fromList [1]) child2)
      ]
  , simple
      "large_list_view_int32"
      (field "x" False ALargeListView [item (AInt 32 True)])
      [ ok (mkLargeListView Nothing (VS.fromList [4, 0, 1, 0]) (VS.fromList [2, 3, 0, 1]) child)
      , ok (mkLargeListView Nothing (VS.fromList [0]) (VS.fromList [1]) child2)
      ]
  , simple
      "large_list_view_int32_nullable"
      (field "x" True ALargeListView [item (AInt 32 True)])
      [ ok (mkLargeListView (valid [True, False, True, True, True]) (VS.fromList [4, 0, 1, 0, 2]) (VS.fromList [2, 0, 0, 1, 2]) child)
      , ok (mkLargeListView (valid [True]) (VS.fromList [0]) (VS.fromList [1]) child2)
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
    tids = VS.fromList :: [Int8] -> VS.Vector Int8
    dense n codes nullable =
      simple
        n
        (ufield Dense codes nullable)
        [ ok
            ( mkDenseUnion
                (tids [0, 1, 0, 0, 1])
                (VS.fromList [0, 0, 1, 2, 1])
                (V.fromList [i32m [Just 1, Just 2, Nothing], utf8m [Just "a", Just "b"]])
            )
        , ok (mkDenseUnion (tids [1]) (VS.fromList [0]) (V.fromList [i32m [], utf8m [Just "z"]]))
        ]
    sparse n codes nullable =
      simple
        n
        (ufield Sparse codes nullable)
        [ ok
            ( mkSparseUnion
                (tids [0, 1, 0, 1])
                (V.fromList [i32m [Just 1, Just 0, Nothing, Just 0], utf8m [Nothing, Just "a", Just "x", Just "b"]])
            )
        , ok (mkSparseUnion (tids [1]) (V.fromList [i32m [Just 0], utf8m [Just "z"]]))
        ]


abc :: ColumnArray
abc = fromTexts (V.fromList ["a", "b", "c"])


-- | A dictionary column over @vals@ with the given keys column (at the index type's wire width).
dict :: Int64 -> ColumnArray -> ColumnArray -> ColumnArray
dict did keys vals = ok (mkDictionary did keys vals)


dictCases :: [Case]
dictCases =
  [ simple
      "dict_utf8"
      (dictField "x" False AUtf8 0 i32t)
      [dict 0 (ix [0, 1, 0, 2, 1]) abc, dict 0 (ix [2, 2, 0]) abc]
  , simple
      "dict_utf8_nullable"
      (dictField "x" True AUtf8 0 i32t)
      [ dict 0 (i32m [Just 0, Nothing, Just 1, Just 2]) abc
      , dict 0 (i32m [Just 1, Just 0]) abc
      ]
  , simple
      "dict_int8_index"
      (dictField "x" False AUtf8 0 (AInt 8 True))
      [dict 0 (ix8 [1, 0, 1]) xy, dict 0 (ix8 [0]) xy]
  , simple
      "dict_int64_values"
      (dictField "x" False (AInt 64 True) 0 i32t)
      [dict 0 (ix [1, 0, 1]) (i64 [100, 200]), dict 0 (ix [0]) (i64 [100, 200])]
  , (simple "dict_replacement" (dictField "x" False AUtf8 0 i32t) [dict 0 (ix [0, 1, 1]) ab, dict 0 (ix [2, 0, 1]) xyz])
      { caseStreamOnly = True
      , caseReplaceDicts = True
      }
  , (simple "dict_delta" (dictField "x" False AUtf8 0 i32t) [dict 0 (ix [0, 1]) ab, dict 0 (ix [2, 0]) abc])
      { caseStreamOnly = True
      , caseReplaceDicts = True
      }
  , simple
      "dict_in_struct"
      (field "x" False AStruct [dictField "d" True AUtf8 0 i32t])
      [ ok (mkStruct 3 Nothing (V.singleton ("d", dict 0 (i32m [Just 2, Just 0, Just 1]) abc)))
      , ok (mkStruct 1 Nothing (V.singleton ("d", dict 0 (i32m [Just 1]) abc)))
      ]
  , -- Dictionaries nested in dictionary values. pyarrow numbers dictionary
    -- ids in schema pre-order, so the outer dictionary is 0 and the inner 1.
    simple
      "dict_struct_of_dict"
      (nestedDict "x" False AStruct (dictField "d" True AUtf8 1 (AInt 16 True)) (AInt 8 True))
      [dict 0 (ix8 [2, 0, 1, 0]) (structOfDict [1, 0, 2] abc), dict 0 (ix8 [1]) (structOfDict [1, 0, 2] abc)]
  , simple
      "dict_list_of_dict"
      (nestedDict "x" True AList (dictField "item" True AUtf8 1 (AInt 8 True)) i32t)
      [ dict 0 (i32m [Just 1, Nothing, Just 0, Just 2]) listOfDict
      , dict 0 (i32m [Just 2]) listOfDict
      ]
  , -- The inner dictionary changes between batches, so both dictionaries are replaced.
    (simple
      "dict_nested_replacement"
      (nestedDict "x" False AStruct (dictField "d" True AUtf8 1 (AInt 16 True)) (AInt 8 True))
      [dict 0 (ix8 [0, 1]) (structOfDict [0, 1] abc), dict 0 (ix8 [1, 0]) (structOfDict [0, 0] xy)])
      { caseStreamOnly = True
      , caseReplaceDicts = True
      }
  , -- A nullable dictionary column whose rows are all null over an empty dictionary.
    simple
      "dict_all_null_empty"
      (dictField "x" True AUtf8 0 i32t)
      [dict 0 (i32m (replicate 3 Nothing)) (fromTexts V.empty), dict 0 (i32m [Nothing]) (fromTexts V.empty)]
  ]
  where
    i32t = AInt 32 True
    ix = primColumn PInt32 . VS.fromList
    ix8 = primColumn PInt8 . VS.fromList
    ix16 = primColumn PInt16 . VS.fromList
    xy = fromTexts (V.fromList ["x", "y"])
    ab = fromTexts (V.fromList ["a", "b"])
    xyz = fromTexts (V.fromList ["x", "y", "z"])
    nestedDict n nullable container inner indexTy =
      Field n nullable container (V.singleton inner) (Just (DictionaryEncoding 0 indexTy False)) V.empty
    structOfDict inner vals = ok (mkStruct (length inner) Nothing (V.singleton ("d", dict 1 (ix16 inner) vals)))
    listOfDict = ok (mkList Nothing (VS.fromList [0, 2, 2, 3]) (dict 1 (ix8 [0, 1, 1]) xy))


reeCases :: [Case]
reeCases =
  [ ree "ree_int32_int64" False (AInt 32 True) (AInt 64 True) [(i32 [3, 5, 8], int64m [Just 100, Just 200, Just 300]), (i32 [2], int64m [Just 7])]
  , ree "ree_int32_int64_nullable" True (AInt 32 True) (AInt 64 True) [(i32 [3, 5, 8], int64m [Just 100, Nothing, Just 300]), (i32 [2], int64m [Just 7])]
  , ree "ree_int16_utf8" True (AInt 16 True) AUtf8 [(i16 [1, 4], utf8m [Just "a", Just "bb"]), (i16 [3], utf8m [Just "c"])]
  , ree "ree_int64_float64" True (AInt 64 True) (AFloatingPoint DoublePrecision) [(i64 [2, 3], dblm [Just 1.5, Nothing]), (i64 [1], dblm [Just 2.5])]
  ]
  where
    int64m = fromMaybes PInt64 . V.fromList
    dblm = fromMaybes PDouble . V.fromList
    ree n nullable reTy valTy batches =
      simple
        n
        (field "x" nullable ARunEndEncoded [field "run_ends" False reTy [], field "values" True valTy []])
        (map (ok . uncurry mkRunEndEncoded) batches)


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
        , fromMaybeBools (V.fromList b)
        , ok (mkList (valid lv) (VS.fromList lo) (i32m lc))
        , ok (mkStruct (length sv) (valid sv) (V.singleton ("f", fromMaybes PDouble (V.fromList sf))))
        , dict 0 (i32m d) abc
        , fromMaybeUtf8View (V.fromList v)
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


opt :: (a -> LV) -> V.Vector (Maybe a) -> Rows
opt f = V.map (maybe LNull f)


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


-- | Rows of a fixed-width column, read through its typed view.
primRows :: PrimType a -> ColumnArray -> Either String Rows
primRows p col = case p of
  PInt8 -> go (LInt . toInteger)
  PInt16 -> go (LInt . toInteger)
  PInt32 -> go (LInt . toInteger)
  PInt64 -> go (LInt . toInteger)
  PUInt8 -> go (LInt . toInteger)
  PUInt16 -> go (LInt . toInteger)
  PUInt32 -> go (LInt . toInteger)
  PUInt64 -> go (LInt . toInteger)
  PFloat16 -> go (\(Float16 w) -> LInt (toInteger w))
  PFloat -> go (LDouble . float2Double)
  PDouble -> go LDouble
  PDate32 -> go (LInt . toInteger)
  PDate64 -> go (LInt . toInteger)
  PTime32 -> go (LInt . toInteger)
  PTime64 -> go (LInt . toInteger)
  PTimestamp -> go (LInt . toInteger)
  PDuration -> go (LInt . toInteger)
  PIntervalYearMonth -> go (LInt . toInteger)
  PIntervalDayTime -> go (\(IntervalDayTime d m) -> LList [LInt (toInteger d), LInt (toInteger m)])
  PIntervalMonthDayNano -> go (\(IntervalMonthDayNano m d n) -> LList [LInt (toInteger m), LInt (toInteger d), LInt (toInteger n)])
  PDecimal128 _ _ -> go (LInt . decimal128ToInteger)
  PDecimal256 _ _ -> go (LInt . decimal256ToInteger)
  where
    go conv = withPrim p (maybe (Left ("asPrim failed on " ++ show col)) (Right . opt conv . toMaybeVector) (asPrim p col))


rowsOf :: Field -> ColumnArray -> Either String Rows
rowsOf f col = case col of
  ColNull n -> Right (V.replicate n LNull)
  ColPrim p _ _ -> primRows p col
  ColBool {} -> opt LBool <$> toBoolVector col
  ColUtf8 {} -> texts
  ColLargeUtf8 {} -> texts
  ColUtf8View {} -> texts
  ColBinary {} -> bytes
  ColLargeBinary {} -> bytes
  ColFixedSizeBinary {} -> bytes
  ColBinaryView {} -> bytes
  ColStruct n v cs -> do
    crs <- V.imapM (\i (nm, c) -> (,) nm <$> childRows i c) cs
    V.generateM n $ \j ->
      if isValidAt v j
        then LStruct . V.toList <$> traverse (\(nm, rs) -> (,) nm <$> at rs j) crs
        else Right LNull
  ColList _ _ c -> lists c
  ColLargeList _ _ c -> lists c
  ColListView _ _ _ c -> lists c
  ColLargeListView _ _ _ c -> lists c
  ColFixedSizeList _ _ _ c -> lists c
  ColMap _ _ ks vs -> do
    entries <- childField f 0
    kf <- childField entries 0
    vf <- childField entries 1
    kr <- rowsOf kf ks
    vr <- rowsOf vf vs
    unless (V.length kr == V.length vr) (Left "map keys and values differ in length")
    perRow $ \(ChildRange s l) -> do
      kl <- sliceRows kr s l
      vl <- sliceRows vr s l
      Right (LMap (zip kl vl))
  ColDenseUnion tids offs cs -> do
    crs <- V.imapM childRows cs
    V.fromList
      <$> traverse
        (\(t, o) -> do
            rs <- maybe (Left ("union child index " ++ show t ++ " out of range")) Right (crs V.!? fromIntegral t)
            LUnion (typeCode t) <$> at rs (fromIntegral o)
        )
        (zip (VS.toList tids) (VS.toList offs))
  ColSparseUnion tids cs -> do
    crs <- V.imapM childRows cs
    V.fromList
      <$> traverse
        (\(i, t) -> do
            rs <- maybe (Left ("union child index " ++ show t ++ " out of range")) Right (crs V.!? fromIntegral t)
            LUnion (typeCode t) <$> at rs i
        )
        (zip [0 ..] (VS.toList tids))
  ColDictionary _ _ vals -> do
    vr <- rowsOf (f {fieldDictionary = Nothing}) vals
    V.generateM (columnLength col) (maybe (Right LNull) (at vr) . dictKeyAt col)
  ColRunEndEncoded off len ends vals -> do
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
    full <- V.fromList <$> expand (0 :: Int) endsI 0
    V.fromList <$> sliceRows full off len
  where
    texts = opt LText <$> toTextVector col
    bytes = opt LBytes <$> toBytesVector col
    childRows i c = do
      cf <- childField f i
      rowsOf cf c
    -- One row per list slot; 'listRange' answers Nothing for a null row.
    perRow row = V.generateM (columnLength col) (maybe (Right LNull) row . listRange col)
    lists c = do
      cr <- childRows 0 c
      perRow (\(ChildRange s l) -> LList <$> sliceRows cr s l)
    typeCode :: Int8 -> Int32
    typeCode t = case fieldType f of
      AUnion _ codes | not (V.null codes) -> fromMaybe (-1) (codes V.!? fromIntegral t)
      _ -> fromIntegral t


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


{- | Compare a decoded file with the case's expectation. Top-level
dictionary columns must also expand ('expandDictionary') to the same
rows.
-}
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
      when (isJust (fieldDictionary fl)) $ do
        expanded <- either (\e -> Left (ctx ++ "expandDictionary failed: " ++ e)) Right (expandDictionary g)
        er <- either (\e -> Left (ctx ++ "expanded column is malformed: " ++ e)) Right (rowsOf fl {fieldDictionary = Nothing} expanded)
        unless (er == gr) $
          Left (ctx ++ "expanded rows " ++ show (V.toList er) ++ " differ from " ++ show (V.toList gr))


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
      r <- try (evaluate (force bytes)) :: IO (Either SomeException (Either String ByteString))
      case r of
        Right (Right bs) -> do
          BS.writeFile (dir </> name) bs
          pure (Right name)
        Right (Left e) -> pure (Left (name, "wireform-arrow encoder rejected the case: " ++ e))
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
      exists <- doesFileExist p
      if exists then pure (Just p) else firstExisting ps


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
