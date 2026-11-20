{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module NestedPrimitiveRegression (tests) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import Parquet.Arrow qualified as Arrow
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Mem (performMajorGC)


load :: Either String a -> IO a
load = either fail pure


check :: String -> Bool -> IO ()
check label ok = unless ok (fail ("Nested primitive regression: " ++ label))


fixed :: Int -> Integer -> BS.ByteString
fixed width value = BS.pack (map (\index -> fromInteger (value `shiftR` (8 * (width - 1 - index)))) [0 .. width - 1])


decimalRow :: Int -> Integer -> N.NestedRow
decimalRow precision value = N.NRLeaf $ if precision <= 9 then N.LvInt32 (fromInteger value) else if precision <= 18 then N.LvInt64 (fromInteger value) else N.LvFixedBinary (fixed (if precision <= 38 then 16 else 32) value)


tests :: IO ()
tests = do
  -- Exercise both sides of the payload-cache limit, retaining only the
  -- public bytes after the producer and its input rows have gone away.
  forM_ [False, True] $ \nested -> forM_ [131071, 131072, 131073] $ \count -> do
    bytes <- ownedNumericPayload nested count
    performMajorGC
    let expected = BL.toStrict (B.toLazyByteString (foldMap (B.int64LE . fromIntegral) [0 .. count - 1]))
    check "frozen payload retains exact bytes across GC and cache boundary" (bytes == expected)
  output <- lookupEnv "WIREFORM_PARQUET_PRIMITIVE_OUTPUT"
  forM_ output (createDirectoryIfMissing True)
  forM_ [1 .. 76] $ \precision -> do
    let bound = 10 ^ precision - 1
        rows = V.fromList [decimalRow precision (negate bound), N.NRNull, decimalRow precision 0, decimalRow precision bound]
        schema = N.NSOptional (N.NSPrimitive (N.LtDecimal (fromIntegral precision) (fromIntegral precision)))
    bytes <- load (N.buildNestedFile (V.singleton ("d", schema)) (V.singleton rows))
    file <- load (Read.loadParquetFile bytes)
    let leaf = P.fmSchema (Read.pfFooter file) V.! 1
        expectedPhysical = if precision <= 9 then P.PTInt32 else if precision <= 18 then P.PTInt64 else P.PTFixedLenByteArray
    check "decimal precision/scale and carrier survive footer" (P.seLogicalType leaf == Just (P.LTDecimal (fromIntegral precision) (fromIntegral precision)) && P.seType leaf == Just expectedPhysical && P.seTypeLength leaf == (if precision <= 18 then Nothing else Just (if precision <= 38 then 16 else 32)))
    arrowSchema <- load (Arrow.parquetFileArrowSchemaChecked file)
    columns <- load (either (Left . show) Right (Arrow.parquetRowGroupToArrow arrowSchema file 0))
    actual <- case V.head columns of
      C.ColDecimal128 p s validity numbers -> do
        check "decimal128 parameters retained" (p == precision && s == precision)
        pure (map (\i -> if C.isValidAt validity i then Just (C.decimal128ToInteger (numbers S.! i)) else Nothing) [0 .. S.length numbers - 1])
      C.ColDecimal256 p s validity numbers -> do
        check "decimal256 parameters retained" (p == precision && s == precision)
        pure (map (\i -> if C.isValidAt validity i then Just (C.decimal256ToInteger (numbers S.! i)) else Nothing) [0 .. S.length numbers - 1])
      other -> fail ("expected a decimal column, got " ++ C.columnTag other)
    check "signed extrema and nulls survive actual file decode" (actual == [Just (negate bound), Nothing, Just 0, Just bound])
    forM_ output $ \dir -> BS.writeFile (dir </> ("decimal-" ++ show precision ++ ".parquet")) bytes
    forM_ [negate bound - 1, bound + 1] $ \bad ->
      check "coefficient outside precision rejected" (isLeft (N.shred schema (V.singleton (decimalRow precision bad))))
  forM_ [(-1, 0), (0, 0), (77, 0), (10, -1), (10, 11), (maxBound, 0)] $ \(precision, scale) ->
    check "invalid decimal schema rejected even without rows" (isLeft (N.buildNestedFile (V.singleton ("d", N.NSPrimitive (N.LtDecimal precision scale))) (V.singleton V.empty)))
  forM_ [1, 2, 3, 16, 31, 32, 64] $ \width -> do
    let value = BS.pack (map fromIntegral [0 .. width - 1])
        rows = V.fromList [N.NRLeaf (N.LvFixedBinary value), N.NRNull, N.NRLeaf (N.LvFixedBinary (BS.replicate width 255))]
        schema = N.NSOptional (N.NSPrimitive (N.LtFixedBinary (fromIntegral width)))
    bytes <- load (N.buildNestedFile (V.singleton ("b", schema)) (V.singleton rows))
    file <- load (Read.loadParquetFile bytes)
    arrowSchema <- load (Arrow.parquetFileArrowSchemaChecked file)
    columns <- load (either (Left . show) Right (Arrow.parquetRowGroupToArrow arrowSchema file 0))
    check "fixed bytes have no BYTE_ARRAY prefix" (V.generate 3 (C.anyBytesAt (V.head columns)) == V.fromList [Just value, Nothing, Just (BS.replicate width 255)])
    check "wrong fixed width rejected" (isLeft (N.shred schema (V.singleton (N.NRLeaf (N.LvFixedBinary (BS.replicate (width + 1) 0))))))
    forM_ output $ \dir -> BS.writeFile (dir </> ("fixed-" ++ show width ++ ".parquet")) bytes
  let uuid = BS.pack [0 .. 15]
  uuidBytes <- load (N.buildNestedFile (V.singleton ("u", N.NSPrimitive N.LtUuid)) (V.singleton (V.singleton (N.NRLeaf (N.LvFixedBinary uuid)))))
  uuidFile <- load (Read.loadParquetFile uuidBytes)
  check "UUID has exact length16 and UUID annotation" (P.seLogicalType (P.fmSchema (Read.pfFooter uuidFile) V.! 1) == Just P.LTUUID)
  forM_ output $ \dir -> BS.writeFile (dir </> "uuid.parquet") uuidBytes
  forM_ [0, -1] $ \width -> check "invalid fixed width rejected without rows" (isLeft (N.buildNestedFile (V.singleton ("b", N.NSPrimitive (N.LtFixedBinary width))) (V.singleton V.empty)))
  -- Exercise the public Arrow -> nested writer entry points for every carrier.
  forM_ [(A.ADecimal32 9 2, C.primColumn (C.PDecimal32 9 2) (S.fromList [-999999999, 0, 123])), (A.ADecimal64 18 2, C.primColumn (C.PDecimal64 18 2) (S.fromList [-999999999999999999, 0, 123])), (A.ADecimal 38 2, C.primColumn (C.PDecimal128 38 2) (S.fromList (map C.decimal128FromInteger [-10 ^ 38 + 1, 0, 123]))), (A.ADecimal256 76 2, C.primColumn (C.PDecimal256 76 2) (S.fromList (map C.decimal256FromInteger [-10 ^ 76 + 1, 0, 123])))] $ \(typ, column) -> do
    let field = A.defaultLeafField "d" False typ
    schema <- load (Arrow.arrowFieldToNestedSchema field)
    rows <- load (Arrow.columnArrayToNestedRows field column)
    bytes <- load (N.buildNestedFile (V.singleton ("d", schema)) (V.singleton rows))
    forM_ output $ \dir -> BS.writeFile (dir </> ("arrow-flat-" ++ show (case typ of A.ADecimal32 {} -> 32 :: Int; A.ADecimal64 {} -> 64; A.ADecimal {} -> 128; _ -> 256) ++ ".parquet")) bytes
    list <- load (C.mkList Nothing (S.fromList [0, 1, 3]) column)
    let listField = A.defaultField "l" False A.AList (V.singleton field)
    listSchema <- load (Arrow.arrowFieldToNestedSchema listField)
    listRows <- load (Arrow.columnArrayToNestedRows listField list)
    listBytes <- load (N.buildNestedFile (V.singleton ("l", listSchema)) (V.singleton listRows))
    forM_ output $ \dir -> BS.writeFile (dir </> ("arrow-list-" ++ show (case typ of A.ADecimal32 {} -> 32 :: Int; A.ADecimal64 {} -> 64; A.ADecimal {} -> 128; _ -> 256) ++ ".parquet")) listBytes
    pure ()
  let badDecimal = C.primColumn (C.PDecimal32 1 0) (S.singleton 10)
  check "Arrow decimal conversion rejects out-of-precision values" (isLeft (Arrow.columnArrayToNestedRows (A.defaultLeafField "d" False (A.ADecimal32 1 0)) badDecimal))
  let poison = C.primColumn (C.PDecimal32 1 0) (S.fromList [10, 1])
      field = A.defaultLeafField "d" False (A.ADecimal32 1 0)
  parent <- load (C.mkStruct 2 (C.validityFromBools (V.fromList [False, True])) (V.singleton ("d", poison)))
  rows <- load (Arrow.columnArrayToNestedRows (A.defaultField "s" True A.AStruct (V.singleton field)) parent)
  check "null parent masks invalid decimal child" (rows == V.fromList [N.NRNull, N.NRStruct (V.singleton (N.NRLeaf (N.LvInt32 1)))])
  let uuidField = (A.defaultLeafField "u" False (A.AFixedSizeBinary 16)) {A.fieldMetadata = V.fromList [("ARROW:extension:name", "arrow.uuid"), ("ARROW:extension:metadata", "")]}
  uuidColumn <- load (C.fromMaybeFixedSizeBinary 16 (V.singleton (Just uuid)))
  uuidSchema <- load (Arrow.arrowFieldToNestedSchema uuidField)
  uuidRows <- load (Arrow.columnArrayToNestedRows uuidField uuidColumn)
  check "Arrow UUID maps to annotated fixed bytes" (uuidSchema == N.NSRequired (N.NSPrimitive N.LtUuid) && uuidRows == V.singleton (N.NRLeaf (N.LvFixedBinary uuid)))
  forM_ [uuidField {A.fieldMetadata = A.fieldMetadata uuidField V.++ V.singleton ("ARROW:extension:name", "arrow.uuid")}, uuidField {A.fieldMetadata = V.fromList [("ARROW:extension:name", "arrow.uuid"), ("ARROW:extension:metadata", "wrong")]}] $ \invalid ->
    check "UUID metadata is checked before writing" (isLeft (Arrow.arrowFieldToNestedSchema invalid))
  putStrLn "Nested decimal/fixed/UUID precision, byte order, null and shape regressions passed"


ownedNumericPayload :: Bool -> Int -> IO BS.ByteString
ownedNumericPayload nested count = do
  let values = V.generate count (N.NRLeaf . N.LvInt64 . fromIntegral)
      rows = if nested then V.singleton (N.NRList values) else values
      schema = if nested then N.NSOptional (N.NSList (N.NSOptional (N.NSPrimitive N.LtInt64))) else N.NSPrimitive N.LtInt64
  leaves <- load (N.shred schema rows)
  evaluate (N.nlValueBytes (V.head leaves))
{-# NOINLINE ownedNumericPayload #-}
