{-# LANGUAGE OverloadedStrings #-}

module Test.Arrow.Decimal (tests) where

import Arrow.Column qualified as C
import Arrow.File qualified as F
import Arrow.FlatBufferIPC qualified as IPC
import Arrow.Read.Columns (decodeRecordBatch, defaultReadOptions)
import Arrow.Stream qualified as S
import Arrow.Types
import Arrow.Write.Columns (validateColumns)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS


data DecimalCase = DecimalCase
  { decimalType :: !ArrowType
  , decimalColumn :: !C.ColumnArray
  , decimalWidth :: !Int
  }


cases :: [DecimalCase]
cases =
  [ DecimalCase (ADecimal32 9 2) (C.fromMaybes (C.PDecimal32 9 2) (V.fromList [Just (-999999999), Nothing, Just 0, Just 123, Just 999999999])) 4
  , DecimalCase (ADecimal64 18 2) (C.fromMaybes (C.PDecimal64 18 2) (V.fromList [Just (-999999999999999999), Nothing, Just 0, Just 123, Just 999999999999999999])) 8
  , DecimalCase (ADecimal 38 2) (C.fromMaybes (C.PDecimal128 38 2) (V.fromList (map (fmap C.decimal128FromInteger) [Just (negate (10 ^ (38 :: Int) - 1)), Nothing, Just 0, Just 123, Just (10 ^ (38 :: Int) - 1)]))) 16
  , DecimalCase (ADecimal256 76 2) (C.fromMaybes (C.PDecimal256 76 2) (V.fromList (map (fmap C.decimal256FromInteger) [Just (negate (10 ^ (76 :: Int) - 1)), Nothing, Just 0, Just 123, Just (10 ^ (76 :: Int) - 1)]))) 32
  ]


load :: Either String a -> IO a
load = either fail pure


check :: String -> Bool -> IO ()
check name condition = unless condition (fail ("Arrow decimal regression: " ++ name))


tests :: IO ()
tests = do
  forM_ cases $ \sample -> do
    let column = decimalColumn sample
        field = defaultLeafField "d" True (decimalType sample)
        schema = defaultSchema (V.singleton field)
    roundtrip field column
    roundtrip field (C.sliceColumnArray 1 3 column)
    structure <- load (C.mkStruct 5 Nothing (V.singleton ("d", column)))
    roundtrip (defaultField "s" True AStruct (V.singleton field)) structure
    list <- load (C.mkList (C.validityFromBools (V.fromList [True, False, True])) (VS.fromList [0, 2, 2, 5]) column)
    roundtrip (defaultField "l" True AList (V.singleton field)) list
    dictionary <- load (C.mkDictionary 42 (C.fromMaybes C.PInt8 (V.fromList [Just 0, Nothing, Just 4, Just 1])) column)
    let dictField = field {fieldDictionary = Just (DictionaryEncoding 42 (AInt 8 True) False)}
    encodedDictionary <- load (S.encodeArrowStream S.defaultWriteOptions (defaultSchema (V.singleton dictField)) [V.singleton dictionary])
    (_, dictBatches) <- load (S.decodeArrowStream defaultReadOptions encodedDictionary)
    expanded <- load (C.expandDictionary dictionary)
    actualExpanded <- load (C.expandDictionary (V.head (head dictBatches)))
    check "dictionary values retain decimal width/parameters/nulls" (actualExpanded == expanded)
    bytes <- load (S.encodeArrowStream S.defaultWriteOptions schema [V.singleton column])
    stream <- load (F.readArrowStream bytes)
    let (batch, body) = V.head (F.asBatches stream)
        Buffer off size = rbBuffers batch VS.! 1
        width = decimalWidth sample
        begin = fromIntegral off
        sizeInt = fromIntegral size
        swapped = BS.concat (map (\i -> BS.reverse (BS.take width (BS.drop (begin + i * width) body))) [0 .. C.columnLength column - 1])
        bigBody = BS.take begin body <> swapped <> BS.drop (begin + sizeInt) body
    check "wire buffer uses its declared physical width" (sizeInt == C.columnLength column * width)
    big <- load (decodeRecordBatch defaultReadOptions schema {arrowEndianness = Big} batch bigBody)
    check "big-endian signed decimals retain every bit" (V.head big == column)
    let short = batch {rbBuffers = rbBuffers batch VS.// [(1, Buffer off (size - 1))]}
    check "short data buffers reject using the actual decimal width" (isLeft (decodeRecordBatch defaultReadOptions schema short body))
  let widths = [(ADecimal32, 9), (ADecimal64, 18), (ADecimal, 38), (ADecimal256, 76)]
  forM_ widths $ \(make, maximumPrecision) -> do
    forM_ [-1, 0, maximumPrecision + 1, maxBound] $ \precision -> do
      let field = defaultLeafField "bad" True (make precision 0)
          schema = defaultSchema (V.singleton field)
          nested = defaultSchema (V.singleton (defaultField "parent" True AStruct (V.singleton field)))
      check "reader rejects invalid decimal precision before materialization" (isLeft (IPC.decodeSchemaMessage (IPC.buildSchemaMessage schema)))
      check "file/footer reader rejects invalid decimal precision" (isLeft (IPC.readFileIndex (IPC.writeArrowFileFBWithDicts schema [] [])))
      forM_ [schema, nested] $ \invalid -> do
        check "schema-only stream writer rejects invalid precision" (isLeft (S.encodeArrowStream S.defaultWriteOptions invalid []))
        check "schema-only lazy stream writer rejects invalid precision" (isLeft (S.encodeArrowStreamLazy S.defaultWriteOptions invalid []))
        check "schema-only file writer rejects invalid precision" (isLeft (S.encodeArrowFile S.defaultWriteOptions invalid []))
        check "schema-only lazy file writer rejects invalid precision" (isLeft (S.encodeArrowFileLazy S.defaultWriteOptions invalid []))
    forM_ [-2, maximumPrecision + 3, fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32)] $ \scale -> do
      let schema = defaultSchema (V.singleton (defaultLeafField "valid" True (make maximumPrecision scale)))
      metadata <- load (IPC.decodeSchemaMessage (IPC.buildSchemaMessage schema))
      check "negative/large signed32 scales are preserved" (metadata == schema)
      _ <- load (S.encodeArrowStream S.defaultWriteOptions schema [])
      pure ()
    forM_ [toInteger (minBound :: Int32) - 1, toInteger (maxBound :: Int32) + 1, 4294967298] $ \scale -> do
      let schema = defaultSchema (V.singleton (defaultLeafField "bad" True (make maximumPrecision (fromInteger scale))))
      check "scale cannot silently wrap when serialized" (isLeft (S.encodeArrowStream S.defaultWriteOptions schema []) && isLeft (S.encodeArrowFile S.defaultWriteOptions schema []))
  check "physical integers cannot masquerade as decimal columns" (isLeft (validateColumns (V.singleton (defaultLeafField "d" False (ADecimal32 9 2))) (V.singleton (C.primColumn C.PInt32 (VS.singleton 1)))))
  forM_ [("pa_decimal32", ADecimal32 9 2, decimalColumn (head cases)), ("pa_decimal64", ADecimal64 18 2, decimalColumn (cases !! 1))] $ \(name, ty, expected) -> do
    forM_ [".arrows", ".arrow"] $ \extension -> do
      bytes <- BS.readFile ("test/golden/" ++ name ++ extension)
      (schema, batches) <- load ((if extension == ".arrow" then S.decodeArrowFile else S.decodeArrowStream) defaultReadOptions bytes)
      check "independent PyArrow golden retains schema and values" (fieldType (V.head (arrowFields schema)) == ty && V.head (head batches) == expected)
  putStrLn "Arrow decimal width/precision/scale/nesting/endianness regressions passed"


roundtrip :: Field -> C.ColumnArray -> IO ()
roundtrip field column = do
  let schema = defaultSchema (V.singleton field)
      batch = [V.singleton column]
      encoders =
        [ S.encodeArrowStream S.defaultWriteOptions schema batch
        , BL.toStrict <$> S.encodeArrowStreamLazy S.defaultWriteOptions schema batch
        , S.encodeArrowFile S.defaultWriteOptions schema batch
        , BL.toStrict <$> S.encodeArrowFileLazy S.defaultWriteOptions schema batch
        ]
  forM_ (zip [False, False, True, True] encoders) $ \(file, encoded) -> do
    bytes <- load encoded
    (actualSchema, batches) <- load ((if file then S.decodeArrowFile else S.decodeArrowStream) defaultReadOptions bytes)
    check "schema and decimal values roundtrip without widening" (actualSchema == schema && length batches == 1 && V.head (head batches) == column)
