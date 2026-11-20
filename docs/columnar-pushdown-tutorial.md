# Tutorial: predicate pushdown end-to-end

This walkthrough takes a 1M-row Parquet file through the
write side that populates the metadata pushdown reads need,
then the read side that exercises every layer.

## 1. Write a file with statistics + page index + bloom filters

```haskell
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Wireform.Columnar as Col
import qualified Parquet.HighLevel as PHL
import qualified Parquet.Types as P

let !schema = V.fromList
      [ P.SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
      , P.SchemaElement "id"     (Just P.Required) (Just P.PTInt64)  Nothing Nothing Nothing Nothing Nothing
      , P.SchemaElement "sku"    (Just P.Required) (Just P.PTByteArray) Nothing
          (Just P.CTUtf8) Nothing Nothing Nothing
      ]
    !rows  = V.fromList
      [ PHL.ColInt64     (VP.fromList ([1 .. 1_000_000] :: [Int64]))
      , PHL.ColByteArray (V.replicate 1_000_000 "AAA001")
      ]
    !opts  = PHL.defaultWriteOptions
               { PHL.writeCompression  = PHL.ZSTD
               , PHL.writePageVersion  = PHL.PageV2
               , PHL.writePageIndex    = True
               , PHL.writeBloomFilters = ["sku"]
               }
    !bytes = PHL.encodeParquet opts schema [rows]
```

The high-level writer:
- attaches per-column-chunk min/max/null_count statistics on every row group
- emits the trailing page-index region (`OffsetIndex` + `ColumnIndex`)
- builds a `Sbbf` bloom filter for `sku` from the actual values

## 2. Read with row-group pushdown

```haskell
import qualified Parquet.Predicate as Pred

let !p = Pred.PCol "id" (Pred.PEq (Pred.PVInt64 12345))

case Col.decodeFilteredIter Col.Parquet Col.defaultReadOptions p bytes of
  Left e -> putStrLn ("decode failed: " ++ e)
  Right (sch, total, dropped, it) -> do
    putStrLn $ "row groups: total=" ++ show total
                                ++ " dropped=" ++ show dropped
    case Col.iterToList it of
      Right batches -> mapM_ consume batches
      Left  e       -> putStrLn ("iter failed: " ++ e)
```

For our single-row-group file `(total=1, dropped=0)`. With a
1024-row-group file (one row group per 1024 rows) and the
predicate `id = 12345`, expect `dropped=1023`.

## 3. Read with page-level pushdown

```haskell
import qualified Parquet.Read as PR
import qualified Parquet.Arrow as PArrow

case PHL.decodeParquet PHL.defaultReadOptions bytes of
  Left e -> putStrLn ("decode footer failed: " ++ e)
  Right pf ->
    case PArrow.readParquetColumnWithPagePruning pf 0 0 idField
           (Pred.PEq (Pred.PVInt64 12345)) of
      Right (Just (kept, total), col) ->
        putStrLn $ "pages: " ++ show kept ++ "/" ++ show total
                    ++ " kept; column has " ++ show (AC.columnLength col)
                    ++ " rows"
      Right (Nothing, col) ->
        putStrLn "no page index — fell through to full chunk read"
      Left e -> putStrLn ("page pruning failed: " ++ e)
  where
    idField = AT.defaultLeafField "id" False (AT.AInt 64 True)
```

## 4. Read with bloom-filter check

```haskell
import qualified Parquet.BloomFilter as Bloom
import qualified Parquet.PageIndex as PI

-- skuSchemaElement is the matching Parquet schema leaf, including annotations.
case PI.readBloomFilter pf 0 0 of
  Right (Just sbbf) ->
    if Pred.evalBloomChunk skuSchemaElement sbbf
         (Pred.PEq (Pred.PVText "AAA001"))
       == Pred.PMaybeKeep
      then putStrLn "sku=AAA001 may be present (decode the chunk)"
      else putStrLn "sku=AAA001 definitely absent (skip)"
  Right Nothing -> putStrLn "no bloom filter"
  Left e        -> putStrLn ("bloom read: " ++ e)
```

## 5. Aggregation pushdown — no decode at all

```haskell
import qualified Parquet.Aggregate as Agg

let !rows = Agg.fileRowCount (PR.pfFooter pf)
let !maxId = Agg.columnMax (PR.pfFooter pf) 0  -- column 0 = id
putStrLn $ "rows=" ++ show rows ++ " max(id)=" ++ show maxId
```

Both calls touch only the footer; no column data is decoded.
`columnMax` returns `Nothing` when the footer lacks an exact maximum;
a compact upper bound can still prune data but cannot answer `max(id)`.
`fileRowCount` sums row groups, including older files with a stale file total.
Use `fileRowCountChecked` to require consistency with that total as well.

## 6. Stream a multi-GB file without OOM

```haskell
import qualified Columnar.Stream as IS

bracket (PR.openParquetReader "huge.parquet") (\_ -> pure ()) $ \mIt ->
  case mIt of
    Left e -> putStrLn ("open failed: " ++ e)
    Right (pf, rgIter) ->
      let !sch = PArrow.parquetFileArrowSchema pf
          decode i = case PArrow.parquetRowGroupToArrow sch pf i of
                       Right cols -> Right cols
                       Left  err  -> Left (show err)
          batches = IS.iterMapM decode (iterIOToIter rgIter)
      in IS.iterForM_ batches consume
```

`openParquetReader` reads the footer once, then yields
row-group indices on demand; the per-index decode happens
inside `iterMapM`. Pair with `IS.iterIOPrefetch 4` to overlap
decode with consumption.

## 7. Multi-file dataset with partition pruning

```haskell
files <-
  [ ("region=us-east/year=2024/data.parquet", bs1)
  , ("region=eu-west/year=2024/data.parquet", bs2)
  , ("region=us-east/year=2023/data.parquet", bs3)
  ] `forM` \(path, bs) -> pure (path, bs)

case Col.decodePartitionedDataset Col.Parquet Col.defaultReadOptions
       (\parts -> lookup "region" parts == Just (Col.PVText "us-east"))
       files of
  Right (sch, it) ->
    case Col.iterToList it of
      Right batches -> consume sch batches
      Left  e       -> putStrLn e
  Left e -> putStrLn e
```

The keep-predicate runs against the parsed Hive-style
partition values; only files in `region=us-east` get decoded.

## What gets pushed down where

| Tier             | Function                         | Granularity     |
|------------------|----------------------------------|-----------------|
| Row-group stats  | `Pred.evalRowGroup`              | row group       |
| Page-index       | `Pred.evalPagesByColumnIndex`    | page            |
| Bloom filter     | `Pred.evalBloomChunk`            | column chunk    |
| Aggregation      | `Agg.fileRowCount` / `columnMin` | whole file      |
| Partition path   | `parsePartitionPath`             | file            |
| Manifest list    | `pruneManifestFiles` (Iceberg)   | manifest        |

Every tier returns `PSkip` only when it can prove the slice
contains no matching rows; otherwise `PMaybeKeep` and the
caller decodes normally.
