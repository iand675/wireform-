{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Criterion harness for wireform-arrow IPC encode and decode.

Three summaries consume these reports (see @scripts/bench-manifest.json@):

* @arrow-encode-decode@: @encode/<workload>@ and @decode/<workload>@ at
  100k rows through "Arrow.Stream".
* @arrow-encode-decode-small@: the same workloads as a 100-row batch,
  under the @encode (100 rows)@ and @decode (100 rows)@ groups.
* @arrow-api-paths@: the mixed 6-column table at 100k rows through
  each public entry point (top-level benches named after the cell).

Every input (column vectors, encoded bytes, records) is built in 'env'
so only the codec call is timed. Decode results are forced with 'nf'.
-}
module Main (main) where

import Arrow.Column (ColumnArray (..))
import Arrow.File qualified as AF
import Arrow.Record (Table, decodeTable, encodeTable)
import Arrow.Record.TH (deriveTable)
import Arrow.Stream (
  decodeArrowStream,
  decodeArrowFile,
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowStream,
 )
import Arrow.Types (
  ArrowType (..),
  DictionaryEncoding (..),
  Field (..),
  Precision (..),
  Schema,
  defaultField,
  defaultLeafField,
  defaultSchema,
 )
import Arrow.Write qualified as AW
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import GHC.Generics (Generic)


-- | One benchmark input: a schema plus a single record batch.
data Workload = Workload
  { wlName :: !String
  , wlSchema :: !Schema
  , wlBatch :: !(V.Vector ColumnArray)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)


bigRows :: Int
bigRows = 100000


smallRows :: Int
smallRows = 100


-- | A short label per row, cycling through a fixed pool so string
-- lengths stay realistic (5 to 12 bytes) without per-row 'show'.
labelPool :: V.Vector Text
labelPool =
  V.fromList
    [ "alpha", "bravo", "charlie", "delta", "echo", "foxtrot"
    , "golf", "hotel", "india", "juliett", "kilo", "lima"
    , "mike", "november", "oscar", "papa-quebec"
    ]


label :: Int -> Text
label i = V.unsafeIndex labelPool (i `mod` V.length labelPool)


int64Col :: Int -> ColumnArray
int64Col n = ColInt64 (VP.generate n (\i -> fromIntegral i * 7919))


doubleCol :: Int -> ColumnArray
doubleCol n = ColDouble (VP.generate n (\i -> fromIntegral i * 1.25))


-- | Roughly 10% nulls: every tenth row.
nullableInt64Col :: Int -> ColumnArray
nullableInt64Col n =
  ColInt64Maybe
    ( V.generate n $ \i ->
        if i `mod` 10 == 0 then Nothing else Just (fromIntegral i)
    )


utf8Col :: Int -> ColumnArray
utf8Col n = ColUtf8 (V.generate n label)


nullableUtf8Col :: Int -> ColumnArray
nullableUtf8Col n =
  ColUtf8Maybe
    ( V.generate n $ \i ->
        if i `mod` 10 == 0 then Nothing else Just (label i)
    )


boolCol :: Int -> ColumnArray
boolCol n = ColBool (V.generate n even)


i64, f64, i32, utf8 :: ArrowType
i64 = AInt 64 True
f64 = AFloatingPoint DoublePrecision
i32 = AInt 32 True
utf8 = AUtf8


single :: String -> Field -> ColumnArray -> Workload
single name f c = Workload name (defaultSchema (V.singleton f)) (V.singleton c)


workloads :: Int -> [Workload]
workloads n =
  [ single "int64" (defaultLeafField "v" False i64) (int64Col n)
  , single "double" (defaultLeafField "v" False f64) (doubleCol n)
  , single "nullable int64" (defaultLeafField "v" True i64) (nullableInt64Col n)
  , single "utf8" (defaultLeafField "v" False utf8) (utf8Col n)
  , single "nullable utf8" (defaultLeafField "v" True utf8) (nullableUtf8Col n)
  , mixed n
  , listInt32 n
  , struct3 n
  , dictUtf8 n
  ]


mixedSchema :: Schema
mixedSchema =
  defaultSchema $
    V.fromList
      [ defaultLeafField "tradeId" False i64
      , defaultLeafField "tradePrice" False f64
      , defaultLeafField "tradeQty" True i64
      , defaultLeafField "tradeSymbol" False utf8
      , defaultLeafField "tradeNote" True utf8
      , defaultLeafField "tradeSettled" False ABool
      ]


mixed :: Int -> Workload
mixed n =
  Workload
    "mixed 6-col"
    mixedSchema
    ( V.fromList
        [ int64Col n
        , doubleCol n
        , nullableInt64Col n
        , utf8Col n
        , nullableUtf8Col n
        , boolCol n
        ]
    )


-- | Four int32 elements per row.
listInt32 :: Int -> Workload
listInt32 n =
  single
    "list<int32>"
    (defaultField "v" False AList (V.singleton (defaultLeafField "item" False i32)))
    ( ColList
        (VP.generate (n + 1) (\i -> fromIntegral (i * 4)))
        (ColInt32 (VP.generate (n * 4) fromIntegral))
    )


struct3 :: Int -> Workload
struct3 n =
  single
    "struct<int32,double,bool>"
    ( defaultField "v" False AStruct $
        V.fromList
          [ defaultLeafField "a" False i32
          , defaultLeafField "b" False f64
          , defaultLeafField "c" False ABool
          ]
    )
    ( ColStruct n $
        V.fromList
          [ ("a", ColInt32 (VP.generate n fromIntegral))
          , ("b", doubleCol n)
          , ("c", boolCol n)
          ]
    )


-- | Sixteen distinct values, indices cycling over them.
dictUtf8 :: Int -> Workload
dictUtf8 n =
  single
    "dictionary<utf8>"
    ( (defaultLeafField "v" False utf8)
        { fieldDictionary = Just (DictionaryEncoding 0 i32 False)
        }
    )
    ( ColDictionary
        0
        (VP.generate n (\i -> fromIntegral (i `mod` V.length labelPool)))
        (ColUtf8 labelPool)
    )


-- | Derived record matching 'mixedSchema' column for column.
data Trade = Trade
  { tradeId :: !Int64
  , tradePrice :: !Double
  , tradeQty :: !(Maybe Int64)
  , tradeSymbol :: !Text
  , tradeNote :: !(Maybe Text)
  , tradeSettled :: !Bool
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- Close the declaration group so 'deriveTable' can reify 'Trade'.
$(pure [])


tradeTable :: Table Trade
tradeTable = $(deriveTable ''Trade)


trades :: Int -> V.Vector Trade
trades n =
  V.generate n $ \i ->
    Trade
      { tradeId = fromIntegral i * 7919
      , tradePrice = fromIntegral i * 1.25
      , tradeQty = if i `mod` 10 == 0 then Nothing else Just (fromIntegral i)
      , tradeSymbol = label i
      , tradeNote = if i `mod` 10 == 0 then Nothing else Just (label i)
      , tradeSettled = even i
      }


typedDecode :: ByteString -> Either String (V.Vector Trade)
typedDecode bs = case decodeArrowStream bs of
  Left err -> Left err
  Right (sch, [cols]) -> decodeTable tradeTable sch cols
  Right (_, batches) -> Left ("expected one batch, got " <> show (length batches))


typedEncode :: V.Vector Trade -> Either String ByteString
typedEncode ts =
  let (sch, cols) = encodeTable tradeTable ts
   in encodeArrowStream defaultWriteOptions sch [cols]


-- | Encoding fails only on a schema mismatch; the workloads are built to match.
encodeW :: Workload -> Either String ByteString
encodeW w = encodeArrowStream defaultWriteOptions (wlSchema w) [wlBatch w]


-- | Fail fast at setup time rather than timing an error path.
checked :: String -> Either String a -> IO a
checked what = either (\e -> fail (what <> ": " <> e)) pure


codecGroups :: String -> Int -> [Benchmark]
codecGroups suffix n =
  [ bgroup ("encode" <> suffix) (map encodeBench (workloads n))
  , bgroup ("decode" <> suffix) (map decodeBench (workloads n))
  ]
 where
  encodeBench w = env (checked (wlName w) (encodeW w) >> pure w) $ \w' -> bench (wlName w) $ nf encodeW w'
  decodeBench w = env (decodeInput w) $ \bs -> bench (wlName w) $ nf decodeArrowStream bs
  decodeInput w = do
    bs <- checked (wlName w) (encodeW w)
    _ <- checked (wlName w) (decodeArrowStream bs)
    pure bs


apiPaths :: [Benchmark]
apiPaths =
  [ env (pure (mixed bigRows)) $ \w ->
      bench "stream encode (Arrow.Stream)" $ nf encodeW w
  , env (pure (mixed bigRows)) $ \w ->
      bench "stream encode (Arrow.Write)" $
        nf (AW.writeArrowStream (wlSchema w)) (V.singleton (wlBatch w))
  , env (checked "stream decode" (encodeW (mixed bigRows))) $ \bs ->
      bench "stream decode (Arrow.Stream)" $ nf decodeArrowStream bs
  , env (pure (mixed bigRows)) $ \w ->
      bench "file encode (Arrow.Stream)" $
        nf (encodeArrowFile defaultWriteOptions (wlSchema w)) [wlBatch w]
  , env specFileBytes $ \bs ->
      bench "file decode (Arrow.Stream)" $ nf decodeArrowFile bs
  , env specFileBytes $ \bs ->
      bench "file read (Arrow.File)" $
        nf (either (error . ("file read: " <>)) id . AF.readArrowFileColumns) bs
  , env (pure (trades bigRows)) $ \ts ->
      bench "typed encode (Arrow.Record)" $ nf typedEncode ts
  , env typedBytes $ \bs ->
      bench "typed decode (Arrow.Record)" $ nf typedDecode bs
  ]
 where
  specFileBytes = do
    let w = mixed bigRows
    bs <- checked "file encode" (encodeArrowFile defaultWriteOptions (wlSchema w) [wlBatch w])
    _ <- checked "file decode" (decodeArrowFile bs)
    pure bs
  typedBytes = do
    bs <- checked "typed encode" (typedEncode (trades bigRows))
    _ <- checked "typed decode" (typedDecode bs)
    pure bs


main :: IO ()
main =
  defaultMain $
    codecGroups "" bigRows
      <> codecGroups (" (" <> show smallRows <> " rows)") smallRows
      <> apiPaths
