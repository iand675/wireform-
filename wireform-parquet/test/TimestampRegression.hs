{-# LANGUAGE OverloadedStrings #-}

module TimestampRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Arrow qualified as Arrow
import Parquet.Nested qualified as N
import Parquet.Predicate qualified as Predicate
import Parquet.Read qualified as Read
import Parquet.Timestamp qualified as TS
import Parquet.Types qualified as P
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))


load :: Either String a -> IO a
load = either fail pure


check :: String -> Bool -> IO ()
check label = flip unless (fail ("Timestamp regression: " ++ label))


signed :: Integer -> IO BS.ByteString
signed value = TS.encodeTimestamp96 <$> load (TS.timestamp96FromInteger value)


julian :: Int32 -> Int64 -> BS.ByteString
julian day nanos = BL.toStrict (B.toLazyByteString (B.int64LE nanos <> B.int32LE day))


file :: N.LeafType -> V.Vector (Maybe BS.ByteString) -> IO BS.ByteString
file kind values = load (N.buildNestedFile (V.singleton ("t", N.NSOptional (N.NSPrimitive kind))) (V.singleton (V.map (maybe N.NRNull (N.NRLeaf . N.LvFixedBinary)) values)))


decodeFile :: BS.ByteString -> IO (Read.ParquetFile, A.Schema, C.ColumnArray)
decodeFile bytes = do
  input <- load (Read.loadParquetFile bytes)
  schema <- load (Arrow.parquetFileArrowSchemaChecked input)
  columns <- load (either (Left . show) Right (Arrow.parquetRowGroupToArrow schema input 0))
  pure (input, schema, V.head columns)


readTarget :: A.Schema -> Read.ParquetFile -> A.Field -> Either String C.ColumnArray
readTarget schema input target = V.head <$> either (Left . show) Right (Arrow.parquetRowGroupToArrow (schema {A.arrowFields = V.singleton target}) input 0)


counts :: C.ColumnArray -> Either String (V.Vector (Maybe Int64))
counts (C.ColTimestamp validity values) = Right (V.generate (S.length values) (\i -> if C.isValidAt validity i then Just (values S.! i) else Nothing))
counts _ = Left "expected Arrow timestamp column"


tests :: IO ()
tests = do
  output <- lookupEnv "WIREFORM_PARQUET_PRIMITIVE_OUTPUT"
  forM_ output (createDirectoryIfMissing True)
  let save name bytes = forM_ output (\path -> BS.writeFile (path </> name) bytes)
      unitCases = [(P.LtMillis, A.Millisecond, "millis"), (P.LtMicros, A.Microsecond, "micros"), (P.LtNanos, A.Nanosecond, "nanos")]
      bounds = V.fromList [negate (2 ^ (95 :: Int)), negate (2 ^ (63 :: Int)) - 1, negate (2 ^ (63 :: Int)), -1, 0, 2 ^ (63 :: Int) - 1, 2 ^ (63 :: Int), 2 ^ (95 :: Int) - 1]
  payloads <- V.mapM signed bounds
  let rows = V.map Just payloads `V.snoc` Nothing
  forM_ unitCases $ \(unit, arrowUnit, label) -> forM_ [False, True] $ \adjusted -> do
    bytes <- file (N.LtTimestamp96 adjusted unit) rows
    (input, schema, column) <- decodeFile bytes
    let field = V.head (A.arrowFields schema)
        target = field {A.fieldType = A.ATimestamp arrowUnit (if adjusted then Just "UTC" else Nothing), A.fieldMetadata = V.empty}
    check "modern96 annotation and units inferred losslessly" (TS.timestampEncoding field == Right (Just (TS.SignedTimestamp96 adjusted unit)))
    check "all96bits and nulls preserved" (V.generate (V.length rows) (C.anyBytesAt column) == rows)
    check "wide count cannot silently narrow" (isLeft (readTarget schema input target))
    let wrong = TS.timestampField "t" True (TS.SignedTimestamp96 (not adjusted) unit)
    check "same physical bytes cannot be relabeled with different timestamp semantics" (isLeft (readTarget schema input wrong))
    nestedSchema <- load (Arrow.arrowFieldToNestedSchema field)
    lowered <- load (Arrow.columnArrayToNestedRows field column)
    copy <- load (N.buildNestedFile (V.singleton ("t", nestedSchema)) (V.singleton lowered))
    (_, copiedSchema, copied) <- decodeFile copy
    check "Arrow lowering retains96bit storage and annotation" (A.arrowFields copiedSchema == A.arrowFields schema && V.generate (V.length rows) (C.anyBytesAt copied) == rows)
    save ("timestamp96-" ++ label ++ (if adjusted then "-utc" else "-local") ++ ".parquet") bytes
    small <- V.mapM (signed . toInteger) (V.fromList [minBound :: Int64, -1, 0, maxBound])
    narrowBytes <- file (N.LtTimestamp96 adjusted unit) (V.map Just small `V.snoc` Nothing)
    (narrowInput, narrowSchema, _) <- decodeFile narrowBytes
    projected <- load (readTarget narrowSchema narrowInput target >>= counts)
    check "exact signed64 boundaries project" (projected == V.fromList [Just minBound, Just (-1), Just 0, Just maxBound, Nothing])
    (_, pruned) <- load (Arrow.readParquetColumnWithPagePruning narrowInput 0 0 target Predicate.PIsNotNull)
    check "page pruning fallback shares checked conversion" (counts pruned == Right projected)
  let julianValues = V.fromList [Just (julian minBound 0), Just (julian (-1) 86399999999999), Just (julian 2440588 0), Nothing, Just (julian maxBound 86399999999999), Just (julian 2440588 minBound), Just (julian 2440588 maxBound)]
  legacy <- file N.LtInt96 julianValues
  (legacyInput, legacySchema, legacyColumn) <- decodeFile legacy
  check "legacy schema remains distinct from signed96" (TS.timestampEncoding (V.head (A.arrowFields legacySchema)) == Right (Just TS.JulianInt96))
  check "full legacy day domain preserved" (V.generate (V.length julianValues) (C.anyBytesAt legacyColumn) == julianValues)
  let nanoTarget = A.defaultField "t" True (A.ATimestamp A.Nanosecond Nothing) V.empty
  check "legacy overflow cannot wrap" (isLeft (readTarget legacySchema legacyInput nanoTarget))
  save "int96-full.parquet" legacy
  forM_ [(A.Second, 1000000000), (A.Millisecond, 1000000), (A.Microsecond, 1000), (A.Nanosecond, 1)] $ \(unit, factor) -> do
    let source = TS.timestampField "t" True TS.JulianInt96
        target = A.defaultField "t" True (A.ATimestamp unit Nothing) V.empty
        values = V.fromList [Just (julian 2440587 (86400000000000 - factor)), Nothing, Just (julian 2440588 factor)]
    bytes <- file N.LtInt96 values
    (input, schema, _) <- decodeFile bytes
    converted <- load (readTarget schema input target >>= counts)
    check "legacy exact negative and positive units" (converted == V.fromList [Just (-1), Nothing, Just 1])
    poison <- load (C.fromMaybeFixedSizeBinary 12 (V.fromList [Just (julian 2440588 factor), Nothing]))
    check "scalar column projection agrees" ((TS.projectTimestampColumn source target poison >>= counts) == Right (V.fromList [Just 1, Nothing]))
    if factor == 1
      then pure ()
      else do
        lossy <- load (C.fromMaybeFixedSizeBinary 12 (V.singleton (Just (julian 2440588 1))))
        check "legacy projection rejects sub-unit loss" (isLeft (TS.projectTimestampColumn source target lossy))
  forM_ [minBound, -1, 86400000000000, maxBound] $ \nanos -> do
    let bytes = julian 2440588 nanos
    check "legacy signed/non-normalized nanoseconds preserved" ((TS.timestamp96ToInteger <$> TS.decodeJulianInt96 bytes) == Right (toInteger nanos))
    check "legacy non-normalized values remain writable" (not (isLeft (N.shred (N.NSPrimitive N.LtInt96) (V.singleton (N.NRLeaf (N.LvFixedBinary bytes))))))
  check "scalar signed96 bounds checked" (isLeft (TS.timestamp96FromInteger (negate (2 ^ (95 :: Int)) - 1)) && isLeft (TS.timestamp96FromInteger (2 ^ (95 :: Int))))
  check "scalar twelve-byte bounds checked" (all (isLeft . TS.decodeTimestamp96 . (`BS.replicate` 0)) [0, 11, 13])
  let base = TS.timestampField "t" True (TS.SignedTimestamp96 False P.LtNanos)
      malformed = [base {A.fieldType = A.ABinary}, base {A.fieldMetadata = A.fieldMetadata base `V.snoc` ("ARROW:extension:name", "other")}, base {A.fieldMetadata = V.fromList [("ARROW:extension:name", "wireform.parquet.timestamp96"), ("ARROW:extension:metadata", "bad")]}]
  forM_ malformed (\field -> check "malformed extension rejected even for empty schema" (isLeft (Arrow.arrowFieldToNestedSchema field)))
  -- Reachability matters: the required timestamp child below a null struct
  -- can contain arbitrary bits; it must not trigger semantic validation.
  let child = TS.timestampField "t" False TS.JulianInt96
      parent = A.defaultField "s" True A.AStruct (V.singleton child)
  poisoned <- load (C.mkFixedSizeBinary 12 2 Nothing (julian 2440588 1 <> BS.replicate 12 255))
  structure <- load (C.mkStruct 2 (C.validityGenerate 2 (== 0)) (V.singleton ("t", poisoned)))
  parentSchema <- load (Arrow.arrowFieldToNestedSchema parent)
  parentRows <- load (Arrow.columnArrayToNestedRows parent structure)
  nested <- load (N.buildNestedFile (V.singleton ("s", parentSchema)) (V.singleton parentRows))
  (nestedInput, nestedSchema, _) <- decodeFile nested
  let nestedTarget = parent {A.fieldChildren = V.singleton (A.defaultField "t" False (A.ATimestamp A.Nanosecond Nothing) V.empty)}
  nestedColumns <- load (either (Left . show) Right (Arrow.parquetRowGroupToArrow (nestedSchema {A.arrowFields = V.singleton nestedTarget}) nestedInput 0))
  check "nested conversion retains parent null" (C.nullCount (V.head nestedColumns) == 1)
  ok <- H.check $ H.withTests 500 $ H.property $ do
    high <- H.forAll (Gen.choice [Gen.int32 Range.constantBounded, Gen.element [-1, 0]])
    low <- H.forAll (Gen.word64 Range.constantBounded)
    let value = TS.Timestamp96 high low
    encoded <- H.evalEither (TS.timestamp96FromInteger (TS.timestamp96ToInteger value))
    decoded <- H.evalEither (TS.decodeTimestamp96 (TS.encodeTimestamp96 encoded))
    decoded H.=== value
    let integer = toInteger high * 18446744073709551616 + toInteger low
        unsignedBytes = BS.foldr (\byte rest -> rest * 256 + toInteger byte) 0 (TS.encodeTimestamp96 encoded)
    unsignedBytes H.=== integer `mod` (2 ^ (96 :: Int))
    column <- H.evalEither (C.mkFixedSizeBinary 12 1 Nothing (TS.encodeTimestamp96 value))
    let source = TS.timestampField "t" False (TS.SignedTimestamp96 False P.LtNanos)
        target = A.defaultField "t" False (A.ATimestamp A.Nanosecond Nothing) V.empty
        projected = TS.projectTimestampColumn source target column >>= counts
    if integer >= toInteger (minBound :: Int64) && integer <= toInteger (maxBound :: Int64)
      then do
        actual <- H.evalEither projected
        actual H.=== V.singleton (Just (fromInteger integer))
      else H.assert (isLeft projected)
  check "generated96bit codec and independent checked-projection oracle" ok
