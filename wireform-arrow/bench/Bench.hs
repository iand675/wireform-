{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Criterion harness for wireform-arrow IPC encode and decode.

Three summaries consume these reports (see @scripts/bench-manifest.json@):

* @arrow-encode-decode@: per workload at 100k rows through "Arrow.Stream":
  @encode/<workload>@ (one allocation for the whole stream),
  @encode lazy/<workload>@ (lazy chunks aliasing the column buffers),
  @decode/<workload>@ (zero-copy columns aliasing the input) and
  @decode + toVector/<workload>@ (decode, convert every column with the
  @to*Vector@ conversions to "Arrow.Vector" vectors, then 'VG.force'
  each into fresh buffers: the result owns its data and keeps nothing
  of the input alive, the same work as arrow-rs's @to_owned_column@
  copying into @Vec<Option<T>>@, @String@s and nested @Vec@s; the
  conversions alone alias the decoded buffers and cost O(rows) or less).
* @arrow-encode-decode-small@: the same four groups as a 100-row batch,
  suffixed @ (100 rows)@.
* @arrow-api-paths@: the mixed 6-column table at 100k rows through
  each public entry point (top-level benches named after the cell).

Every input (columns, encoded bytes, records) is built in 'env' with the
column builders, so only the codec call is timed. Results are forced with
'nf'. Run with a large nursery (the stanza bakes in @-A64m@): the decoders
allocate little, but the materializing rows and the typed paths are
dominated by minor GCs at the default 4 MB nursery.
-}
module Main (main) where

import Arrow.Column
import Arrow.File qualified as AF
import Arrow.Record (Table, decodeTable, encodeTable)
import Arrow.Record.TH (deriveTable)
import Arrow.Stream (
  decodeArrowFile,
  decodeArrowStream,
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowFileLazy,
  encodeArrowStream,
  encodeArrowStreamLazy,
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
import Arrow.Vector qualified as AV
import Arrow.Write qualified as AW
import Control.DeepSeq (NFData)
import Control.Monad.ST (ST, runST)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Storable qualified as VS
import Foreign.Storable (Storable)
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


-- | Every tenth row is null (roughly 10% nulls).
nullRow :: Int -> Bool
nullRow i = i `mod` 10 == 0


loopN :: Int -> (Int -> ST s ()) -> ST s ()
loopN n f = go 0
 where
  go !i
    | i >= n = pure ()
    | otherwise = f i >> go (i + 1)


primCol :: Storable a => PrimType a -> Int -> (Int -> Maybe a) -> ColumnArray
primCol ty n f = runST $ do
  b <- newPrimBuilder ty n
  loopN n (appendPrimMaybe b . f)
  freezeBuilder b


textCol :: Int -> (Int -> Maybe Text) -> ColumnArray
textCol n f = runST $ do
  b <- newUtf8Builder n
  loopN n (appendTextMaybe b . f)
  freezeBuilder b


boolCol :: Int -> ColumnArray
boolCol n = runST $ do
  b <- newBoolBuilder n
  loopN n (appendBool b . even)
  freezeBuilder b


int64Col, doubleCol, nullableInt64Col, utf8Col, nullableUtf8Col :: Int -> ColumnArray
int64Col n = primCol PInt64 n (\i -> Just (fromIntegral i * 7919))
doubleCol n = primCol PDouble n (\i -> Just (fromIntegral i * 1.25))
nullableInt64Col n = primCol PInt64 n (\i -> if nullRow i then Nothing else Just (fromIntegral i))
utf8Col n = textCol n (Just . label)
nullableUtf8Col n = textCol n (\i -> if nullRow i then Nothing else Just (label i))


-- | Setup-time construction failure is a harness bug; fail loudly.
built :: String -> Either String a -> a
built what = either (\e -> error (what <> ": " <> e)) id


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


-- | Workload names, known without building the data (criterion needs the
-- names outside 'env').
workloadNames :: [String]
workloadNames =
  [ "int64", "double", "nullable int64", "utf8", "nullable utf8"
  , "mixed 6-col", "list<int32>", "struct<int32,double,bool>", "dictionary<utf8>"
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
    ( built "list<int32>" $
        mkList
          Nothing
          (VS.generate (n + 1) (\i -> fromIntegral (i * 4)))
          (primCol PInt32 (n * 4) (Just . fromIntegral))
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
    ( built "struct" $
        mkStruct n Nothing $
          V.fromList
            [ ("a", primCol PInt32 n (Just . fromIntegral))
            , ("b", doubleCol n)
            , ("c", boolCol n)
            ]
    )


-- | Sixteen distinct values, int32 keys cycling over them.
dictUtf8 :: Int -> Workload
dictUtf8 n =
  single
    "dictionary<utf8>"
    ( (defaultLeafField "v" False utf8)
        { fieldDictionary = Just (DictionaryEncoding 0 i32 False)
        }
    )
    ( built "dictionary<utf8>" $
        mkDictionary
          0
          (primCol PInt32 n (\i -> Just (fromIntegral (i `mod` V.length labelPool))))
          (fromTexts labelPool)
    )


{- | A decoded column converted to "Arrow.Vector" vectors and detached
from the input, one constructor per shape the workloads use.
-}
data Owned
  = OInt32 !(AV.Vector (Maybe Int32))
  | OInt64 !(AV.Vector (Maybe Int64))
  | ODouble !(AV.Vector (Maybe Double))
  | OText !(AV.Vector (Maybe Text))
  | OBool !(AV.Vector (Maybe Bool))
  | OListInt32 !(AV.Vector (Maybe (AV.Vector (Maybe Int32))))
  | OStruct !(V.Vector Owned)
  deriving stock (Generic)
  deriving anyclass (NFData)


{- | The "toVector" half of the decode + toVector rows: convert (which
aliases the decoded buffers), then 'VG.force' into fresh buffers so the
result owns its data, as arrow-rs's @to_owned_column@ does with its
@Vec@s and @String@s.
-}
toOwned :: ColumnArray -> Either String Owned
toOwned c
  | Just p <- asPrim PInt64 c = Right (OInt64 (VG.force (toMaybeVector p)))
  | Just p <- asPrim PInt32 c = Right (OInt32 (VG.force (toMaybeVector p)))
  | Just p <- asPrim PDouble c = Right (ODouble (VG.force (toMaybeVector p)))
  | Just _ <- asUtf8 c = OText . VG.force <$> toTextVector c
  | Just _ <- asBool c = OBool . VG.force <$> toBoolVector c
toOwned c = case c of
  ColList _ _ child
    | Just _ <- asPrim PInt32 child -> OListInt32 . VG.force <$> toListVector int32s c
  -- The workloads build struct children exactly as long as the struct.
  ColStruct _ _ fs -> OStruct <$> traverse (toOwned . snd) fs
  -- String dictionaries convert their values once; the owned rows share
  -- one copy of the values instead of one string per row.
  ColDictionary _ _ vals | Just _ <- asUtf8 vals -> OText . VG.force <$> toTextVector c
  ColDictionary {} -> expandDictionary c >>= toOwned
  _ -> Left ("toOwned: unsupported column " <> columnTag c)
 where
  int32s ch = maybe (Left ("toOwned: list child " <> columnTag ch)) (Right . toMaybeVector) (asPrim PInt32 ch)


decodeToVector :: ByteString -> Either String [V.Vector Owned]
decodeToVector bs = do
  (_, batches) <- decodeArrowStream bs
  traverse (traverse toOwned) batches


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
      , tradeQty = if nullRow i then Nothing else Just (fromIntegral i)
      , tradeSymbol = label i
      , tradeNote = if nullRow i then Nothing else Just (label i)
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


encodeW :: Workload -> Either String ByteString
encodeW w = encodeArrowStream defaultWriteOptions (wlSchema w) [wlBatch w]


-- | 'nf' on a lazy 'BL.ByteString' forces the chunk spine, which is the
-- whole cost of the lazy encoder (the chunks alias the column buffers).
encodeLazyW :: Workload -> Either String BL.ByteString
encodeLazyW w = encodeArrowStreamLazy defaultWriteOptions (wlSchema w) [wlBatch w]


-- | Fail fast at setup time rather than timing an error path.
checked :: String -> Either String a -> IO a
checked what = either (\e -> fail (what <> ": " <> e)) pure


-- | Build the workload in 'env' and check it encodes.
workloadEnv :: Int -> String -> IO Workload
workloadEnv n name = case filter ((== name) . wlName) (workloads n) of
  [w] -> checked name (encodeW w) >> pure w
  _ -> fail ("unknown workload " <> name)


-- | Encoded bytes of the workload, checked to decode and materialize.
decodeEnv :: Int -> String -> IO ByteString
decodeEnv n name = do
  w <- workloadEnv n name
  bs <- checked name (encodeW w)
  _ <- checked name (decodeToVector bs)
  pure bs


codecGroups :: String -> Int -> [Benchmark]
codecGroups suffix n =
  [ bgroup ("encode" <> suffix) (map (encodeBench encodeW) workloadNames)
  , bgroup ("encode lazy" <> suffix) (map (encodeBench encodeLazyW) workloadNames)
  , bgroup ("decode" <> suffix) (map (decodeBench decodeArrowStream) workloadNames)
  , bgroup ("decode + toVector" <> suffix) (map (decodeBench decodeToVector) workloadNames)
  ]
 where
  encodeBench :: NFData b => (Workload -> b) -> String -> Benchmark
  encodeBench f name = env (workloadEnv n name) $ \w -> bench name $ nf f w
  decodeBench :: NFData b => (ByteString -> b) -> String -> Benchmark
  decodeBench f name = env (decodeEnv n name) $ \bs -> bench name $ nf f bs


apiPaths :: [Benchmark]
apiPaths =
  [ env mixedEnv $ \w ->
      bench "stream encode (Arrow.Stream)" $ nf encodeW w
  , env mixedEnv $ \w ->
      bench "stream encode lazy (Arrow.Stream)" $ nf encodeLazyW w
  , env mixedEnv $ \w ->
      bench "stream encode (Arrow.Write)" $
        nf (AW.writeArrowStream (wlSchema w)) (V.singleton (wlBatch w))
  , env (checked "stream decode" (encodeW (mixed bigRows))) $ \bs ->
      bench "stream decode (Arrow.Stream)" $ nf decodeArrowStream bs
  , env mixedEnv $ \w ->
      bench "file encode (Arrow.Stream)" $
        nf (encodeArrowFile defaultWriteOptions (wlSchema w)) [wlBatch w]
  , env mixedEnv $ \w ->
      bench "file encode lazy (Arrow.Stream)" $
        nf (encodeArrowFileLazy defaultWriteOptions (wlSchema w)) [wlBatch w]
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
  mixedEnv = workloadEnv bigRows "mixed 6-col"
  specFileBytes = do
    w <- mixedEnv
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
