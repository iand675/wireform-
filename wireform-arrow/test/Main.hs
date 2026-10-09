{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip tests for the Arrow IPC writer + reader.

Two layers of coverage:

  1. Internal round-trips: every 'ColumnArray' constructor
     the writer emits must be parsed back correctly by
     'decodeArrowStream' + 'decodeRecordBatch'.
  2. Golden pyarrow interop: the bytes in
     @test/golden/pa_*.arrows@ were produced by pyarrow
     ('pa.ipc.new_stream') against the reference spec.
     'decodeArrowStream' must accept them verbatim.

Previously (PR #16 deferred list) the wireform IPC framing
was a simplified encoding that pyarrow couldn't read; after
'Arrow.FlatBufferIPC' landed with a real FlatBuffers layout
the bidirectional interop works and these tests pin it.
-}
module Main (main) where

import Arrow.Column
import Arrow.File (asBatches, asSchema, readArrowStream)
import Arrow.FlatBufferIPC (
  SparseTensor (..),
  Tensor (..),
  TensorDim (..),
  buildSchemaMessage,
  decodeSchemaMessage,
  decodeSparseTensorFrame,
  decodeTensorFrame,
  encodeSparseTensorFrame,
  encodeTensorFrame,
  decodeRecordBatch,
 )
import Arrow.Record qualified as AR
import Arrow.Stream (
  DictHandling (..),
  WriteOptions (..),
  decodeArrowStream,
  defaultWriteOptions,
  encodeArrowStream,
  openStreamReader,
  streamReaderIter,
  streamReaderNext,
  streamReaderProjected,
  streamReaderSchema,
  streamReaderToList,
 )
import Arrow.Types
import Arrow.Write (writeArrowStream)
import Columnar.Stream qualified as IS
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32, Word64, Word8)
import Test.Arrow.Core qualified as Core
import Test.Arrow.Malformed qualified as Malformed
import Test.Arrow.Props qualified as Props
import Test.Arrow.Vector qualified as Vector
import System.Exit (exitFailure)


-- Test records used by 'nestedStructRoundTrip'.
data Address = Address
  { cityF :: Text
  , zipF :: Text
  }
  deriving (Show, Eq)


data Customer = Customer
  { nameF :: Text
  , addrF :: Address
  , ageF :: Int32
  }
  deriving (Show, Eq)


{- | Customer with an optional address; exercises the
'encoderFromRowEncoder' / 'decoderFromRowDecoder' nullable
nested-struct path.
-}
data CustomerOpt = CustomerOpt
  { coNameF :: Text
  , maybeAddrF :: Maybe Address
  , coAgeF :: Int32
  }
  deriving (Show, Eq)


main :: IO ()
main = do
  putStrLn "wireform-arrow writer/reader round-trip suite"

  -- Every column: build a single-batch Arrow stream containing just
  -- that column, serialise it, parse it, materialise, and assert
  -- the recovered ColumnArray equals the one we put in.

  roundTripPrim "Int8" (primColumn PInt8 (VS.fromList [0, 1, -1, 100, -128, 127]))
  roundTripPrim "Int16" (primColumn PInt16 (VS.fromList [0, 1, -1, 32767, -32768]))
  roundTripPrim "Int32" (primColumn PInt32 (VS.fromList [0, 1, -1, maxBound, minBound]))
  roundTripPrim "Int64" (primColumn PInt64 (VS.fromList [0, 1, -1, maxBound, minBound]))
  roundTripPrim "UInt8" (primColumn PUInt8 (VS.fromList ([0, 255] :: [Word8])))
  roundTripPrim "UInt16" (primColumn PUInt16 (VS.fromList ([0, 65535] :: [Word16])))
  roundTripPrim "UInt32" (primColumn PUInt32 (VS.fromList ([0, maxBound] :: [Word32])))
  roundTripPrim "UInt64" (primColumn PUInt64 (VS.fromList ([0, maxBound] :: [Word64])))
  roundTripPrim
    "Float16"
    (primColumn PFloat16 (VS.fromList [Float16 0, Float16 0x3C00, Float16 0xBC00]))
  roundTripPrim "Float" (primColumn PFloat (VS.fromList [0.0, 1.5, -2.25, 3.14 :: Float]))
  roundTripPrim "Double" (primColumn PDouble (VS.fromList [0.0, 1.5, -2.25, 3.14159265 :: Double]))
  roundTripPrim "Bool" (fromBools (V.fromList [True, False, True, False, True]))

  roundTripPrim "Date32" (primColumn PDate32 (VS.fromList [0 :: Int32, 18000, -1]))
  roundTripPrim "Date64" (primColumn PDate64 (VS.fromList [0 :: Int64, 1700000000000]))
  roundTripPrim "Time32" (primColumn PTime32 (VS.fromList [0 :: Int32, 12345]))
  roundTripPrim "Time64" (primColumn PTime64 (VS.fromList [0 :: Int64, 12345000000]))
  roundTripPrim
    "Timestamp"
    (primColumn PTimestamp (VS.fromList [0 :: Int64, 1700000000_000_000_000]))
  roundTripPrim "Duration" (primColumn PDuration (VS.fromList [0 :: Int64, 60_000_000_000]))

  roundTripPrim
    "IntervalYearMonth"
    (primColumn PIntervalYearMonth (VS.fromList [0 :: Int32, 12, -6, 100]))
  roundTripPrim
    "IntervalDayTime"
    ( primColumn
        PIntervalDayTime
        ( VS.fromList
            [IntervalDayTime 1 500, IntervalDayTime 2 (-1), IntervalDayTime 30 0]
        )
    )
  roundTripPrim
    "IntervalMonthDayNano"
    ( primColumn
        PIntervalMonthDayNano
        ( VS.fromList
            [IntervalMonthDayNano 1 3 1000, IntervalMonthDayNano 2 4 (-500)]
        )
    )

  roundTripPrim
    "Decimal128"
    ( primColumn
        (PDecimal128 18 2)
        (VS.fromList [decimal128FromInteger 0, decimal128FromInteger 100])
    )
  roundTripPrim
    "Decimal256"
    ( primColumn
        (PDecimal256 38 4)
        (VS.fromList [decimal256FromInteger 0, decimal256FromInteger 42])
    )

  roundTripPrim
    "FixedSizeBinary"
    ( fixture $
        fromMaybeFixedSizeBinary
          4
          ( V.fromList
              [Just (BS.pack [0, 1, 2, 3]), Just (BS.pack [0xFF, 0xFE, 0xFD, 0xFC])]
          )
    )

  roundTripPrim "Utf8" (fromTexts (V.fromList ["alpha", "beta", "", "\xe2\x9a\xa1"]))
  roundTripPrim "Binary" (fromByteStrings (V.fromList [BS.pack [1, 2, 3], BS.empty, BS.pack [0xFF]]))
  roundTripPrim "LargeUtf8" (fromMaybeLargeTexts (V.fromList [Just "alpha", Just "beta"]))
  roundTripPrim
    "LargeBinary"
    (fromMaybeLargeByteStrings (V.fromList [Just (BS.pack [0, 1, 2]), Just (BS.pack [0xFF])]))

  -- Nullable variants (every fixture holds at least one null row).
  roundTripPrim
    "Int8Maybe"
    (fromMaybes PInt8 (V.fromList [Just 1, Nothing, Just (-1), Just 42]))
  roundTripPrim
    "Int16Maybe"
    (fromMaybes PInt16 (V.fromList [Just 100, Nothing, Just (-200)]))
  roundTripPrim
    "Int32Maybe"
    (fromMaybes PInt32 (V.fromList [Nothing, Just 0, Just maxBound]))
  roundTripPrim
    "Int64Maybe"
    (fromMaybes PInt64 (V.fromList [Just 0, Nothing, Just (-1)]))
  roundTripPrim
    "UInt8Maybe"
    (fromMaybes PUInt8 (V.fromList [Just 0, Just 255, Nothing]))
  roundTripPrim
    "UInt16Maybe"
    (fromMaybes PUInt16 (V.fromList [Just 0, Nothing]))
  roundTripPrim
    "UInt32Maybe"
    (fromMaybes PUInt32 (V.fromList [Just 0, Nothing, Just maxBound]))
  roundTripPrim
    "UInt64Maybe"
    (fromMaybes PUInt64 (V.fromList [Just 0, Nothing]))
  roundTripPrim
    "Float16Maybe"
    (fromMaybes PFloat16 (V.fromList [Just (Float16 0), Just (Float16 0x3C00), Nothing]))
  roundTripPrim
    "FloatMaybe"
    (fromMaybes PFloat (V.fromList [Just 1.5, Nothing, Just (-2.25)]))
  roundTripPrim
    "DoubleMaybe"
    (fromMaybes PDouble (V.fromList [Just 1.5, Nothing]))
  roundTripPrim
    "BoolMaybe"
    (fromMaybeBools (V.fromList [Just True, Nothing, Just False, Just True]))

  roundTripPrim
    "Utf8Maybe"
    (fromMaybeTexts (V.fromList [Just "alpha", Nothing, Just ""]))
  roundTripPrim
    "BinaryMaybe"
    (fromMaybeByteStrings (V.fromList [Just (BS.pack [1, 2]), Nothing]))
  roundTripPrim
    "LargeUtf8Maybe"
    (fromMaybeLargeTexts (V.fromList [Nothing, Just "beta"]))
  roundTripPrim
    "LargeBinaryMaybe"
    (fromMaybeLargeByteStrings (V.fromList [Just (BS.pack [0xFF]), Nothing]))
  roundTripPrim
    "FixedSizeBinaryMaybe"
    ( fixture $
        fromMaybeFixedSizeBinary
          3
          (V.fromList [Just (BS.pack [1, 2, 3]), Nothing, Just (BS.pack [4, 5, 6])])
    )

  roundTripPrim "Date32Maybe" (fromMaybes PDate32 (V.fromList [Just 0, Nothing]))
  roundTripPrim "Date64Maybe" (fromMaybes PDate64 (V.fromList [Just 0, Nothing]))
  roundTripPrim "Time32Maybe" (fromMaybes PTime32 (V.fromList [Just 0, Nothing]))
  roundTripPrim "Time64Maybe" (fromMaybes PTime64 (V.fromList [Just 0, Nothing]))
  roundTripPrim "TimestampMaybe" (fromMaybes PTimestamp (V.fromList [Just 0, Nothing]))
  roundTripPrim "DurationMaybe" (fromMaybes PDuration (V.fromList [Just 0, Nothing]))

  -- ============================================================
  -- Nested columns
  -- ============================================================

  -- Struct with two primitive children.
  roundTripNested
    "Struct"
    ( nestedField "s" False AStruct $
        V.fromList
          [ plainField "id" False (AInt 64 True)
          , plainField "name" False AUtf8
          ]
    )
    ( fixture . mkStruct 3 Nothing $
        V.fromList
          [ ("id", primColumn PInt64 (VS.fromList [1, 2, 3]))
          , ("name", fromTexts (V.fromList ["a", "b", "c"]))
          ]
    )

  roundTripNested
    "StructMaybe"
    ( nestedField "s" True AStruct $
        V.fromList
          [ plainField "id" False (AInt 32 True)
          , plainField "flag" False ABool
          ]
    )
    ( fixture $
        mkStruct
          3
          (validityFromBools (V.fromList [True, False, True]))
          ( V.fromList
              [ ("id", primColumn PInt32 (VS.fromList [1, 2, 3]))
              , ("flag", fromBools (V.fromList [True, False, True]))
              ]
          )
    )

  roundTripNested
    "List<int32>"
    ( nestedField "l" False AList $
        V.fromList
          [plainField "item" False (AInt 32 True)]
    )
    ( fixture $
        mkList
          Nothing
          (VS.fromList [0, 2, 2, 5])
          (primColumn PInt32 (VS.fromList [10, 20, 30, 40, 50]))
    )

  roundTripNested
    "ListMaybe<int32>"
    ( nestedField "l" True AList $
        V.fromList
          [plainField "item" False (AInt 32 True)]
    )
    ( fixture $
        mkList
          (validityFromBools (V.fromList [True, False, True]))
          (VS.fromList [0, 2, 2, 5])
          (primColumn PInt32 (VS.fromList [10, 20, 30, 40, 50]))
    )

  roundTripNested
    "LargeList<int32>"
    ( nestedField "l" False ALargeList $
        V.fromList
          [plainField "item" False (AInt 32 True)]
    )
    ( fixture $
        mkLargeList
          Nothing
          (VS.fromList [0, 2, 2, 5])
          (primColumn PInt32 (VS.fromList [1, 2, 3, 4, 5]))
    )

  roundTripNested
    "LargeListMaybe<int32>"
    ( nestedField "l" True ALargeList $
        V.fromList
          [plainField "item" False (AInt 32 True)]
    )
    ( fixture $
        mkLargeList
          (validityFromBools (V.fromList [True, False, True]))
          (VS.fromList [0, 2, 2, 5])
          (primColumn PInt32 (VS.fromList [1, 2, 3, 4, 5]))
    )

  roundTripNested
    "FixedSizeList<3 of int32>"
    ( nestedField "l" False (AFixedSizeList 3) $
        V.fromList
          [plainField "item" False (AInt 32 True)]
    )
    ( fixture $
        mkFixedSizeList
          3
          2
          Nothing
          (primColumn PInt32 (VS.fromList [1, 2, 3, 4, 5, 6]))
    )

  roundTripNested
    "FixedSizeListMaybe<2 of int32>"
    ( nestedField "l" True (AFixedSizeList 2) $
        V.fromList
          [plainField "item" False (AInt 32 True)]
    )
    ( fixture $
        mkFixedSizeList
          2
          3
          (validityFromBools (V.fromList [True, False, True]))
          (primColumn PInt32 (VS.fromList [1, 2, 3, 4, 5, 6]))
    )

  -- Map<string, int32>. Arrow encodes maps as a list of struct
  -- <key, value> pairs; the map field has one child (the struct).
  roundTripNested
    "Map<string, int32>"
    ( nestedField "m" False (AMap False) $
        V.fromList
          [ nestedField "entries" False AStruct $
              V.fromList
                [ plainField "key" False AUtf8
                , plainField "value" False (AInt 32 True)
                ]
          ]
    )
    ( fixture $
        mkMap
          Nothing
          (VS.fromList [0, 2, 2, 3])
          (fromTexts (V.fromList ["a", "b", "c"]))
          (primColumn PInt32 (VS.fromList [1, 2, 3]))
    )

  -- Dense union over (int32, utf8).
  roundTripNested
    "DenseUnion<int32, utf8>"
    ( nestedField "u" False (AUnion Dense (V.fromList [0, 1])) $
        V.fromList
          [ plainField "v_int" False (AInt 32 True)
          , plainField "v_text" False AUtf8
          ]
    )
    ( fixture $
        mkDenseUnion
          (VS.fromList [0, 1, 0])
          (VS.fromList [0, 0, 1])
          ( V.fromList
              [ primColumn PInt32 (VS.fromList [100, 200])
              , fromTexts (V.fromList ["hello"])
              ]
          )
    )

  -- Sparse union over (bool, int32).
  roundTripNested
    "SparseUnion<bool, int32>"
    ( nestedField "u" False (AUnion Sparse (V.fromList [0, 1])) $
        V.fromList
          [ plainField "flag" False ABool
          , plainField "value" False (AInt 32 True)
          ]
    )
    ( fixture $
        mkSparseUnion
          (VS.fromList [0, 1, 0])
          ( V.fromList
              [ fromBools (V.fromList [True, False, False])
              , primColumn PInt32 (VS.fromList [0, 42, 0])
              ]
          )
    )

  -- FlatBuffers reader / writer round-trip: build a typical
  -- multi-column batch with the FB writer, parse back with the FB
  -- reader, assert schema equality.
  flatBufRoundTrip
  flatBufSchemaSelfCheck

  -- Golden pyarrow interop: reference .arrows files produced
  -- by pyarrow's ipc.new_stream, checked into test/golden.
  -- Decoding them proves the FlatBuffers reader handles the
  -- shapes arrow-cpp emits (vs only what wireform's own
  -- writer produces).
  pyarrowGoldenRoundTrip

  -- Hedgehog property suites: column core, generated round-trips,
  -- malformed-input robustness and the Arrow-layout vectors. Each
  -- returns False on failure.
  coreOk <- Core.tests
  propsOk <- Props.tests
  malformedOk <- Malformed.tests
  vectorOk <- Vector.tests
  unless (coreOk && propsOk && malformedOk && vectorOk) $
    failTest "FAIL: wireform-arrow property suites"

  putStrLn "All wireform-arrow round-trip tests passed."


flatBufSchemaSelfCheck :: IO ()
flatBufSchemaSelfCheck = do
  let cases =
        [ Schema (V.fromList [plainField "a" False (AInt 32 True)]) Little V.empty V.empty
        , Schema
            ( V.fromList
                [ plainField "id" False (AInt 64 True)
                , plainField "name" True AUtf8
                , plainField "amount" False (ADecimal 12 4)
                , plainField "ts" True (ATimestamp Nanosecond (Just "UTC"))
                , plainField "blob" True ABinary
                , plainField "tag" False (AFixedSizeBinary 16)
                ]
            )
            Little
            V.empty
            V.empty
        , -- Post-V5 type tags (Utf8View / BinaryView / RunEndEncoded /
          -- ListView / LargeListView). Arrow.Column doesn't materialise
          -- their data buffers, but the schema flatbuffer round-trips
          -- so wireform can interoperate with newer Arrow producers.
          Schema
            ( V.fromList
                [ plainField "v" True AUtf8View
                , plainField "b" True ABinaryView
                , plainField "ree" True ARunEndEncoded
                , plainField "lv" True AListView
                , plainField "llv" True ALargeListView
                ]
            )
            Little
            V.empty
            V.empty
        ]
  mapM_
    ( \sch -> do
        let bs = buildSchemaMessage sch
        case decodeSchemaMessage bs of
          Right got
            | got == sch ->
                putStrLn $ "OK: FlatBuffers schema self-roundtrip: " ++ describe sch
          Right got ->
            failTest $
              "FB schema roundtrip mismatch:\n got: "
                ++ show got
                ++ "\n exp: "
                ++ show sch
          Left e ->
            failTest $ "FB schema roundtrip decode failed: " ++ e
    )
    cases
  where
    describe sch =
      "("
        ++ show (V.length (arrowFields sch))
        ++ " field"
        ++ (if V.length (arrowFields sch) == 1 then "" else "s")
        ++ ")"


flatBufRoundTrip :: IO ()
flatBufRoundTrip = do
  -- Multi-column round-trip exercising the high-level API in
  -- "Arrow.Stream": Schema + batches go in, bytes come out, and
  -- the inverse recovers the same shape.
  highLevelRoundTrip
    "Multi-column"
    ( Schema
        { arrowFields =
            V.fromList
              [ plainField "i" False (AInt 32 True)
              , plainField "s" True AUtf8
              ]
        , arrowEndianness = Little
        , arrowMetadata = V.empty
        , arrowFeatures = V.empty
        }
    )
    ( V.fromList
        [ primColumn PInt32 (VS.fromList ([1, 2, 3] :: [Int32]))
        , fromMaybeTexts (V.fromList [Just "x", Nothing, Just "z"])
        ]
    )

  -- Post-V5 columns: writer + reader byte-compatible end to end.
  highLevelRoundTrip
    "Utf8View"
    (Schema (V.singleton (plainField "v" True AUtf8View)) Little V.empty V.empty)
    ( V.singleton
        ( fromMaybeUtf8View
            ( V.fromList
                [ Just "short"
                , Nothing
                , Just "this string is definitely longer than twelve bytes"
                ]
            )
        )
    )

  highLevelRoundTrip
    "ListView<int32>"
    ( Schema
        ( V.singleton
            ( nestedField
                "lv"
                False
                AListView
                ( V.singleton
                    (plainField "item" False (AInt 32 True))
                )
            )
        )
        Little
        V.empty
        V.empty
    )
    ( V.singleton
        ( fixture $
            mkListView
              Nothing
              (VS.fromList ([0, 2, 5] :: [Int32]))
              (VS.fromList ([2, 3, 1] :: [Int32]))
              (primColumn PInt32 (VS.fromList ([10, 20, 30, 40, 50, 60] :: [Int32])))
        )
    )

  highLevelRoundTrip
    "RunEndEncoded(int32, int64?)"
    ( Schema
        ( V.singleton
            ( nestedField "ree" True ARunEndEncoded $
                V.fromList
                  [ plainField "run_ends" False (AInt 32 True)
                  , plainField "values" True (AInt 64 True)
                  ]
            )
        )
        Little
        V.empty
        V.empty
    )
    ( V.singleton
        ( fixture $
            mkRunEndEncoded
              (primColumn PInt32 (VS.fromList ([3, 5, 8] :: [Int32])))
              (fromMaybes PInt64 (V.fromList [Just 100, Nothing, Just 300]))
        )
    )

  -- Dictionary-encoded utf8: the high-level API auto-extracts
  -- the dictionary batch and auto-resolves on read.
  let dictField =
        Field
          "d"
          False
          AUtf8
          V.empty
          (Just (DictionaryEncoding 0 (AInt 32 True) False))
          V.empty
  highLevelRoundTrip
    "Dictionary<utf8>"
    (Schema (V.singleton dictField) Little V.empty V.empty)
    ( V.singleton
        ( fixture $
            mkDictionary
              0
              (primColumn PInt32 (VS.fromList ([0, 1, 0, 2, 1] :: [Int32])))
              (fromTexts (V.fromList ["a", "b", "c"]))
        )
    )

  -- ANull column: schema metadata round-trip + ColNull row count
  highLevelRoundTrip
    "Null"
    (Schema (V.singleton (plainField "n" False ANull)) Little V.empty V.empty)
    (V.singleton (ColNull 5))

  -- Custom metadata round-trip on schema + field
  customMetadataRoundTrip

  -- Nested struct via Arrow.Record.structE / structD
  nestedStructRoundTrip

  -- Nullable nested struct via encoderFromRowEncoder /
  -- decoderFromRowDecoder + Arrow.Record.nullable / nullableD.
  nullableNestedStructRoundTrip

  -- Schema fingerprint: determinism + structural equivalence.
  schemaFingerprintTests

  -- Record helpers: subsetTable / projectTable /
  -- columnDWithDefault / NameStrategy / validateMapKeysSorted.
  recordHelperTests

  -- Streaming reader: pull batches one at a time, then drain.
  streamingRoundTrip
    (Schema (V.fromList [plainField "n" False (AInt 32 True)]) Little V.empty V.empty)
    [ V.singleton (primColumn PInt32 (VS.fromList ([1, 2] :: [Int32])))
    , V.singleton (primColumn PInt32 (VS.fromList ([3] :: [Int32])))
    , V.singleton (primColumn PInt32 (VS.fromList ([4, 5, 6, 7] :: [Int32])))
    ]

  -- Column projection on a multi-column stream: a 3-column
  -- batch should narrow to exactly the requested columns in
  -- the requested order.
  projectionRoundTrip

  -- ZSTD body compression (writer + reader): exercises
  -- BodyCompression on a multi-column batch, asserting the
  -- decoded values match the source.
  bodyCompressionRoundTrip
    BodyZstd
    ( Schema
        ( V.fromList
            [ plainField "n" False (AInt 64 True)
            , plainField "s" False AUtf8
            ]
        )
        Little
        V.empty
        V.empty
    )
    ( V.fromList
        [ primColumn
            PInt64
            ( VS.fromList
                ([1 .. 1000] :: [Int64]) -- enough bytes that ZSTD shrinks
            )
        , fromTexts (V.replicate 1000 "highly-compressible-payload")
        ]
    )

  -- LZ4_FRAME body compression: same shape / sizing as the ZSTD
  -- case. Verifies the lz4-hs Codec.Lz4 frame compressor +
  -- decompressor round-trip through the full BodyCompression
  -- pipeline (per-buffer envelope, length prefix, offsets
  -- rewritten on decode, etc.).
  bodyCompressionRoundTrip
    LZ4Frame
    ( Schema
        ( V.fromList
            [ plainField "n" False (AInt 64 True)
            , plainField "s" False AUtf8
            ]
        )
        Little
        V.empty
        V.empty
    )
    ( V.fromList
        [ primColumn PInt64 (VS.fromList ([1 .. 1000] :: [Int64]))
        , fromTexts (V.replicate 1000 "highly-compressible-payload")
        ]
    )

  -- DictReplaceOnChange: two batches with the SAME dict id but
  -- different values. The writer should emit two dict batches;
  -- the reader should resolve each record batch against the
  -- most-recently-emitted dict for that id.
  dictReplacementRoundTrip

  -- Tensor message round-trip.
  tensorRoundTrip

  -- SparseTensor (COO) round-trip.
  sparseTensorRoundTrip


{- | Consume the golden .arrows files in @test/golden/@ and
assert 'decodeArrowStream' returns the ColumnArray values we
expect from the pyarrow-side generator.

The fixtures:
  pa_int32.arrows   : int32 column [1,2,3,4,5]
  pa_mixed.arrows   : (int64, nullable utf8, nullable bool), 3 rows
  pa_dict.arrows    : dictionary<utf8, int32> with values ["a","b","c"]
                       and indices [0,1,0,2,1]
-}
pyarrowGoldenRoundTrip :: IO ()
pyarrowGoldenRoundTrip = do
  goldenCheck
    "pa_int32.arrows"
    (V.singleton (primColumn PInt32 (VS.fromList [1, 2, 3, 4, 5 :: Int32])))

  goldenCheck
    "pa_mixed.arrows"
    ( V.fromList
        [ primColumn PInt64 (VS.fromList [10, 20, 30 :: Int64])
        , fromMaybeTexts (V.fromList [Just "alpha", Nothing, Just "gamma"])
        , fromMaybeBools (V.fromList [Just True, Just False, Nothing])
        ]
    )

  -- Dictionary-encoded batch: the decoder resolves
  -- ColDictionary's values against the dict batches pyarrow
  -- emitted ahead of the record batch.
  goldenDictCheck
    "pa_dict.arrows"
    [0, 1, 0, 2, 1]
    (fromTexts (V.fromList ["a", "b", "c"]))


goldenCheck :: FilePath -> V.Vector ColumnArray -> IO ()
goldenCheck name expected = do
  bs <- BS.readFile ("test/golden/" <> name)
  case decodeArrowStream bs of
    Left e -> failTest $ "golden " <> name <> ": decode: " <> e
    Right (_sch, batches)
      | [b] <- batches
      , b == expected ->
          putStrLn $ "OK: pyarrow golden " <> name
      | otherwise ->
          failTest $
            "golden "
              <> name
              <> " mismatch:\n got: "
              <> show batches
              <> "\n exp: "
              <> show [expected]


-- Dictionary-encoded batches need a bespoke matcher: the keys and
-- the resolved values are compared piecewise. pyarrow fields are
-- nullable, so the column decodes as a 'ColDictionary' whose keys
-- carry a validity slot (here without null rows).
goldenDictCheck
  :: FilePath -> [Int] -> ColumnArray -> IO ()
goldenDictCheck name expectedIndices expectedValues = do
  bs <- BS.readFile ("test/golden/" <> name)
  case decodeArrowStream bs of
    Left e -> failTest $ "golden " <> name <> ": decode: " <> e
    Right (_sch, batches) -> case batches of
      [b] | V.length b == 1 -> case V.head b of
        col@(ColDictionary _ _ vals)
          | hasValiditySlot col
          , nullCount col == 0
          , keysOf col == map Just expectedIndices
          , vals == expectedValues ->
              putStrLn $ "OK: pyarrow golden " <> name
          | otherwise ->
              failTest $
                "golden "
                  <> name
                  <> " dict mismatch:\n idx="
                  <> show (keysOf col)
                  <> " vals="
                  <> show vals
        other ->
          failTest $ "golden " <> name <> " expected ColDictionary, got " <> columnTag other
      _ -> failTest $ "golden " <> name <> " expected 1 batch with 1 column"
  where
    keysOf col = map (dictKeyAt col) [0 .. columnLength col - 1]


sparseTensorRoundTrip :: IO ()
sparseTensorRoundTrip = do
  -- Tiny 3x3 sparse int32 tensor with 2 non-zeros at (0,1) and
  -- (2,0). COO indices are Int64 pairs.
  let !idx =
        BS.pack
          [ 0
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0 -- row 0
          , 1
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0 -- col 1
          , 2
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0 -- row 2
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0
          , 0 -- col 0
          ]
      !vals =
        BS.pack
          [ 7
          , 0
          , 0
          , 0 -- value 7
          , 9
          , 0
          , 0
          , 0 -- value 9
          ]
      !st =
        SparseTensor
          { sparseTensorType = AInt 32 True
          , sparseTensorShape =
              V.fromList
                [TensorDim 3 "rows", TensorDim 3 "cols"]
          , sparseNonZeroLength = 2
          , sparseIndicesType = AInt 64 True
          , sparseIndicesBody = idx
          , sparseIndicesCanonical = True
          , sparseTensorBody = vals
          }
      !frame = encodeSparseTensorFrame st
  case decodeSparseTensorFrame frame of
    Left e -> failTest $ "decodeSparseTensorFrame: " ++ e
    Right (sout, _)
      | sparseTensorType sout == AInt 32 True
      , sparseNonZeroLength sout == 2
      , sparseIndicesBody sout == idx
      , sparseTensorBody sout == vals ->
          putStrLn "OK: SparseTensor message round-trip (COO, 3x3 int32, nnz=2)"
      | otherwise ->
          failTest $ "sparse tensor mismatch:\n got " ++ show sout


tensorRoundTrip :: IO ()
tensorRoundTrip = do
  -- 2x3 tensor of Int32: raw little-endian body, row-major.
  let !body =
        BS.pack
          [ 0x01
          , 0
          , 0
          , 0
          , 0x02
          , 0
          , 0
          , 0
          , 0x03
          , 0
          , 0
          , 0
          , 0x04
          , 0
          , 0
          , 0
          , 0x05
          , 0
          , 0
          , 0
          , 0x06
          , 0
          , 0
          , 0
          ]
      !tin =
        Tensor
          { tensorType = AInt 32 True
          , tensorShape =
              V.fromList
                [TensorDim 2 "rows", TensorDim 3 "cols"]
          , tensorStrides = V.empty
          , tensorBody = body
          }
      !frame = encodeTensorFrame tin
  case decodeTensorFrame frame of
    Left e -> failTest $ "decodeTensorFrame: " ++ e
    Right (tout, rest)
      | BS.null rest
      , tensorType tout == AInt 32 True
      , V.toList (tensorShape tout)
          == [TensorDim 2 "rows", TensorDim 3 "cols"]
      , tensorBody tout == body ->
          putStrLn "OK: Tensor message round-trip (2x3 int32)"
      | otherwise ->
          failTest $
            "tensor round-trip mismatch:\n got "
              ++ show tout
              ++ " rest="
              ++ show (BS.length rest)
              ++ "B"


dictReplacementRoundTrip :: IO ()
dictReplacementRoundTrip = do
  let !sch =
        Schema
          ( V.singleton
              ( Field
                  "d"
                  False
                  AUtf8
                  V.empty
                  (Just (DictionaryEncoding 0 (AInt 32 True) False))
                  V.empty
              )
          )
          Little
          V.empty
          V.empty
      !batch1 =
        V.singleton . fixture $
          mkDictionary
            0
            (primColumn PInt32 (VS.fromList [0, 1, 0]))
            (fromTexts (V.fromList ["a", "b"]))
      !batch2 =
        V.singleton . fixture $
          mkDictionary
            0
            (primColumn PInt32 (VS.fromList [0, 1, 0]))
            (fromTexts (V.fromList ["x", "y"]))
      !opts = defaultWriteOptions {writeDictHandling = DictReplaceOnChange}
      !bytes = encodeArrowStream opts sch [batch1, batch2]
  case bytes >>= decodeArrowStream of
    Left e -> failTest $ "dict-replace round-trip: " ++ e
    Right (_, batches)
      | [b1, b2] <- batches
      , V.length b1 == 1
      , V.length b2 == 1 ->
          let !c1 = V.unsafeIndex b1 0
              !c2 = V.unsafeIndex b2 0
          in case (c1, c2) of
               (ColDictionary _ ix1 v1, ColDictionary _ ix2 v2)
                 | valuesToList v1 == ["a", "b"]
                 , valuesToList v2 == ["x", "y"]
                 , keysToList ix1 == [Just 0, Just 1, Just 0]
                 , keysToList ix2 == [Just 0, Just 1, Just 0] ->
                     putStrLn "OK: dictionary replacement across batches"
               _ ->
                 failTest $
                   "dict-replace mismatch:\n got "
                     ++ show batches
      | otherwise ->
          failTest $
            "dict-replace expected 2 batches, got "
              ++ show (length batches)
  where
    valuesToList :: ColumnArray -> [Text]
    valuesToList c = either (const []) (VG.toList . VG.mapMaybe id) (toTextVector c)
    keysToList :: ColumnArray -> [Maybe Int32]
    keysToList c = maybe [] (VG.toList . toMaybeVector) (asPrim PInt32 c)


bodyCompressionRoundTrip :: BodyCompressionCodec -> Schema -> V.Vector ColumnArray -> IO ()
bodyCompressionRoundTrip codec sch cols = do
  let !opts = defaultWriteOptions {writeBodyCompression = Just codec}
      !encoded = encodeArrowStream opts sch [cols]
  case encoded >>= \bytes -> (,) bytes <$> decodeArrowStream bytes of
    Left e -> failTest $ "body-compression round-trip: " ++ e
    Right (bytes, (_sch', batches))
      | [got] <- batches
      , got == cols ->
          putStrLn $
            "OK: body compression "
              ++ show codec
              ++ " ("
              ++ show (BS.length bytes)
              ++ " bytes)"
      | otherwise ->
          failTest $
            "body-compression mismatch: got "
              ++ show batches


streamingRoundTrip :: Schema -> [V.Vector ColumnArray] -> IO ()
streamingRoundTrip sch batches = do
  let bytes = encodeArrowStream defaultWriteOptions sch batches
  case bytes >>= openStreamReader of
    Left e -> failTest $ "openStreamReader: " ++ e
    Right rd0 -> do
      when (streamReaderSchema rd0 /= sch) $
        failTest "streamReaderSchema mismatch"
      -- Pull first batch via streamReaderNext; drain rest via toList.
      case streamReaderNext rd0 of
        Left e -> failTest $ "streamReaderNext (first): " ++ e
        Right Nothing -> failTest "streamReaderNext: stream empty"
        Right (Just (cols0, rd1)) -> do
          when (Just cols0 /= listToMaybe batches) $
            failTest $
              "streamReaderNext (first) mismatch:\n got "
                ++ show cols0
                ++ "\n exp "
                ++ show (take 1 batches)
          case streamReaderToList rd1 of
            Left e -> failTest $ "streamReaderToList: " ++ e
            Right rest
              | rest == drop 1 batches ->
                  putStrLn "OK: streaming reader iterates all batches"
              | otherwise ->
                  failTest $
                    "streamReaderToList: tail mismatch\n got "
                      ++ show rest
                      ++ "\n exp "
                      ++ show (drop 1 batches)
  -- Iter-shaped variant: the same drain via Columnar.Stream.
  case bytes >>= openStreamReader of
    Left e -> failTest $ "openStreamReader (iter): " ++ e
    Right rd0 ->
      case IS.iterToList (streamReaderIter rd0) of
        Left e -> failTest $ "streamReaderIter drain: " ++ e
        Right got
          | got == batches ->
              putStrLn "OK: streamReaderIter drains all batches"
          | otherwise ->
              failTest $
                "streamReaderIter mismatch:\n got "
                  ++ show got
                  ++ "\n exp "
                  ++ show batches


{- | Schema-level + field-level @custom_metadata@ pairs survive
a full encode to decode round-trip via the FlatBuffers schema
writer + reader.
-}
customMetadataRoundTrip :: IO ()
customMetadataRoundTrip = do
  let !field =
        (plainField "n" False (AInt 32 True))
          { fieldMetadata = V.fromList [("description", "row id"), ("unit", "count")]
          }
      !sch =
        Schema
          { arrowFields = V.singleton field
          , arrowEndianness = Little
          , arrowMetadata =
              V.fromList
                [ ("pandas", "{}")
                , ("creator", "wireform-test")
                ]
          , arrowFeatures = V.empty
          }
      !batch = V.singleton (primColumn PInt32 (VS.fromList ([1, 2, 3] :: [Int32])))
      !bytes = encodeArrowStream defaultWriteOptions sch [batch]
  case bytes >>= decodeArrowStream of
    Left e -> failTest $ "customMetadata roundtrip: " ++ e
    Right (sch', _batches) -> do
      expect
        "schema custom_metadata roundtrips"
        (arrowMetadata sch' == arrowMetadata sch)
      let recoveredField = V.unsafeIndex (arrowFields sch') 0
      expect
        "field custom_metadata roundtrips"
        (fieldMetadata recoveredField == fieldMetadata field)


{- | Nested record via 'structE' + 'structD'. The inner record
(Address) becomes a 'ColStruct' column inside the outer
record (Customer); a round-trip through 'encodeTable' /
'decodeTable' must recover the exact value.
-}
nestedStructRoundTrip :: IO ()
nestedStructRoundTrip = do
  let !addrEnc =
        AR.fieldE "city" cityF AR.utf8E
          <> AR.fieldE "zip" zipF AR.utf8E
      !addrDec =
        Address
          <$> AR.columnD "city" AR.utf8D
          <*> AR.columnD "zip" AR.utf8D
      !custEnc =
        AR.fieldE "name" nameF AR.utf8E
          <> AR.structE "addr" addrF addrEnc
          <> AR.fieldE "age" ageF AR.int32E
      !custDec =
        Customer
          <$> AR.columnD "name" AR.utf8D
          <*> AR.structD "addr" addrDec
          <*> AR.columnD "age" AR.int32D
      !tbl = AR.table custEnc custDec
      !rows =
        V.fromList
          [ Customer "Alice" (Address "Atlantis" "00001") 30
          , Customer "Bob" (Address "Brisbane" "4000") 45
          , Customer "Carol" (Address "Calcutta" "700001") 28
          ]
      (!sch, !cols) = AR.encodeTable tbl rows
      !bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case bytes >>= decodeArrowStream of
    Left e -> failTest $ "nested struct decode: " ++ e
    Right (sch', batches) -> case batches of
      [batch] -> case AR.decodeTable tbl sch' batch of
        Left e -> failTest $ "nested struct decodeTable: " ++ e
        Right got
          | got == rows ->
              putStrLn "OK: nested struct via structE / structD"
          | otherwise ->
              failTest $
                "nested struct mismatch:\n got "
                  ++ show (V.toList got)
                  ++ "\n exp "
                  ++ show (V.toList rows)
      _ -> failTest "nested struct: expected 1 batch"


{- | Nullable nested record. Same shape as 'nestedStructRoundTrip'
but the @addr@ column is @Maybe Address@; the encoder builds
a nullable 'ColStruct' with a top-level validity mask, and the
decoder reconstructs the @Just@/@Nothing@ pattern.
-}
nullableNestedStructRoundTrip :: IO ()
nullableNestedStructRoundTrip = do
  let !addrEnc =
        AR.fieldE "city" cityF AR.utf8E
          <> AR.fieldE "zip" zipF AR.utf8E
      !addrDec =
        Address
          <$> AR.columnD "city" AR.utf8D
          <*> AR.columnD "zip" AR.utf8D
      !custEnc =
        AR.fieldE "name" coNameF AR.utf8E
          <> AR.structEMaybe "addr_opt" maybeAddrF addrEnc
          <> AR.fieldE "age" coAgeF AR.int32E
      !custDec =
        CustomerOpt
          <$> AR.columnD "name" AR.utf8D
          <*> AR.structDMaybe "addr_opt" addrDec
          <*> AR.columnD "age" AR.int32D
      !tbl = AR.table custEnc custDec :: AR.Table CustomerOpt
      !rows =
        V.fromList
          [ CustomerOpt "Alice" (Just (Address "Atlantis" "00001")) 30
          , CustomerOpt "Bob" Nothing 45
          , CustomerOpt "Carol" (Just (Address "Calcutta" "700001")) 28
          , CustomerOpt "Dave" Nothing 50
          ]
      (!sch, !cols) = AR.encodeTable tbl rows
      !bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case bytes >>= decodeArrowStream of
    Left e -> failTest $ "nullable nested struct decode: " ++ e
    Right (sch', batches) -> case batches of
      [batch] -> case AR.decodeTable tbl sch' batch of
        Left e -> failTest $ "nullable nested decodeTable: " ++ e
        Right got
          | got == rows ->
              putStrLn "OK: nullable nested struct via structEMaybe / structDMaybe"
          | otherwise ->
              failTest $
                "nullable nested mismatch:\n got "
                  ++ show (V.toList got)
                  ++ "\n exp "
                  ++ show (V.toList rows)
      _ -> failTest "nullable nested struct: expected 1 batch"


{- | 'schemaFingerprint' tests: determinism, equivalence-class
equality, and difference detection.
-}
schemaFingerprintTests :: IO ()
schemaFingerprintTests = do
  let !sch1 =
        Schema
          ( V.fromList
              [ plainField "id" False (AInt 64 True)
              , plainField "name" True AUtf8
              ]
          )
          Little
          V.empty
          V.empty
      !sch2 =
        Schema
          ( V.fromList
              [ plainField "id" False (AInt 64 True)
              , plainField "name" True AUtf8
              ]
          )
          Little
          (V.fromList [("creator", "wireform")]) -- different annotation
          (V.fromList [FeatureDictionaryReplacement]) -- different feature flag
      !sch3 =
        Schema
          ( V.fromList
              [ plainField "id" False (AInt 64 True)
              , plainField "name2" True AUtf8 -- different field name
              ]
          )
          Little
          V.empty
          V.empty
      !fp1 = schemaFingerprint sch1
      !fp2 = schemaFingerprint sch2
      !fp3 = schemaFingerprint sch3
  expect
    "fingerprint is deterministic across calls"
    (fp1 == schemaFingerprint sch1)
  expect
    "fingerprint ignores annotation fields"
    (fp1 == fp2)
  expect
    "fingerprint distinguishes different field names"
    (fp1 /= fp3)
  expect
    "schemaEquivalent matches fingerprint equality (1==2)"
    (schemaEquivalent sch1 sch2 == (fp1 == fp2))
  expect
    "schemaEquivalent matches fingerprint equality (1==3)"
    (schemaEquivalent sch1 sch3 == (fp1 == fp3))


{- | 'NameStrategy', 'columnDWithDefault', 'projectTable',
'subsetTable', and 'validateMapKeysSorted' tests.
-}
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b


recordHelperTests :: IO ()
recordHelperTests = do
  -- NameStrategy
  expect
    "NameAsIs is identity"
    (AR.applyNameStrategy AR.NameAsIs "userId" == "userId")
  expect
    "NameSnakeCase userId -> user_id"
    (AR.applyNameStrategy AR.NameSnakeCase "userId" == "user_id")
  expect
    "NameSnakeCase userIDValue -> user_id_value (acronym boundary)"
    (AR.applyNameStrategy AR.NameSnakeCase "userIDValue" == "user_id_value")
  expect
    "NameSnakeCase XMLHttpRequest -> xml_http_request"
    (AR.applyNameStrategy AR.NameSnakeCase "XMLHttpRequest" == "xml_http_request")
  expect
    "NameCamelCase user_id -> userId"
    (AR.applyNameStrategy AR.NameCamelCase "user_id" == "userId")
  expect
    "NameUpperSnakeCase userId -> USER_ID"
    (AR.applyNameStrategy AR.NameUpperSnakeCase "userId" == "USER_ID")

  -- validateMapKeysSorted
  -- Build a ColMap with sorted keys vs unsorted keys.
  let !sortedKeys = fromTexts (V.fromList ["a", "b", "c"])
      !unsortedKeys = fromTexts (V.fromList ["b", "a", "c"])
      !vals = primColumn PInt32 (VS.fromList [1, 2, 3 :: Int32])
      !offsets = VS.fromList [0, 3 :: Int32]
      !sortedMap = fixture (mkMap Nothing offsets sortedKeys vals)
      !unsortedMap = fixture (mkMap Nothing offsets unsortedKeys vals)
  case validateMapKeysSorted sortedMap of
    Right () -> putStrLn "OK: validateMapKeysSorted accepts sorted keys"
    Left e -> failTest $ "expected sorted accept, got " ++ e
  case validateMapKeysSorted unsortedMap of
    Left _ -> putStrLn "OK: validateMapKeysSorted rejects unsorted keys"
    Right () -> failTest "validateMapKeysSorted should have rejected unsorted"

  -- columnDWithDefault: missing column substitutes the default.
  -- Build a writer that emits only (name, age); the reader
  -- expects (name, age, opt) and falls back on the default
  -- for the missing 'opt' column.
  let !partialEnc =
        AR.fieldE "name" (fst3 :: (Text, Int32, Text) -> Text) AR.utf8E
          <> AR.fieldE "age" (snd3 :: (Text, Int32, Text) -> Int32) AR.int32E
      !partialDec =
        (\n a -> (n, a, "" :: Text))
          <$> AR.columnD "name" AR.utf8D
          <*> AR.columnD "age" AR.int32D
      !partialTbl =
        AR.table partialEnc partialDec
          :: AR.Table (Text, Int32, Text)
      !partialRows =
        V.fromList
          [ ("Alice" :: Text, 30 :: Int32, "ignored" :: Text)
          , ("Bob", 45, "ignored")
          ]
      (!partialSch, !partialCols) = AR.encodeTable partialTbl partialRows
      !fullDec =
        (\n a o -> (n, a, o))
          <$> AR.columnD "name" AR.utf8D
          <*> AR.columnD "age" AR.int32D
          <*> AR.columnDWithDefault "opt" ("default" :: Text) AR.utf8D
  case AR.runRowDecoder fullDec (arrowFields partialSch) partialCols of
    Right got
      | V.toList got == [("Alice", 30, "default"), ("Bob", 45, "default")] ->
          putStrLn "OK: columnDWithDefault substitutes for missing column"
      | otherwise ->
          failTest $ "columnDWithDefault wrong values: " ++ show (V.toList got)
    Left e -> failTest $ "columnDWithDefault decode: " ++ e

  -- projectTable: pick a subset of columns by name
  let (!schWide, !colsWide) =
        let !enc =
              AR.fieldE "a" (\(x, _, _) -> x :: Int32) AR.int32E
                <> AR.fieldE "b" (\(_, y, _) -> y :: Int32) AR.int32E
                <> AR.fieldE "c" (\(_, _, z) -> z :: Int32) AR.int32E
            !dec =
              (,,)
                <$> AR.columnD "a" AR.int32D
                <*> AR.columnD "b" AR.int32D
                <*> AR.columnD "c" AR.int32D
            !tbl = AR.table enc dec :: AR.Table (Int32, Int32, Int32)
            !rs = V.fromList [(1, 10, 100), (2, 20, 200)]
        in AR.encodeTable tbl rs
  case AR.projectTable ["c", "a"] schWide colsWide of
    Just (sch', cols') -> do
      let !names = V.toList (V.map fieldName (arrowFields sch'))
      expect
        ("projectTable preserves order: got " ++ show names)
        (names == ["c", "a"])
      expect
        "projectTable yields matching column count"
        (V.length cols' == 2)
    Nothing -> failTest "projectTable returned Nothing for present cols"
  case AR.projectTable ["c", "missing"] schWide colsWide of
    Nothing -> putStrLn "OK: projectTable returns Nothing for missing column"
    Just _ -> failTest "projectTable should have returned Nothing"

  -- subsetTable: build a Table whose encoder emits only some
  -- columns
  let !custTbl =
        AR.table
          ( AR.fieldE "name" (fst :: (Text, Int32) -> Text) AR.utf8E
              <> AR.fieldE "age" (snd :: (Text, Int32) -> Int32) AR.int32E
          )
          ( (,)
              <$> AR.columnD "name" AR.utf8D
              <*> AR.columnD "age" AR.int32D
          )
          :: AR.Table (Text, Int32)
  case AR.subsetTable ["name"] custTbl of
    Just sub -> do
      let !rsSub = V.fromList [("Alice" :: Text, 30 :: Int32), ("Bob", 45)]
          (!schSub, !colsSub) = AR.encodeTable sub rsSub
      expect
        "subsetTable schema has 1 field"
        (V.length (arrowFields schSub) == 1)
      expect
        "subsetTable schema field is 'name'"
        (V.toList (V.map fieldName (arrowFields schSub)) == ["name"])
      expect
        "subsetTable encoded 1 column"
        (V.length colsSub == 1)
    Nothing -> failTest "subsetTable returned Nothing for ['name']"
  case AR.subsetTable ["nope"] custTbl of
    Nothing -> putStrLn "OK: subsetTable returns Nothing for missing column"
    Just _ -> failTest "subsetTable should have returned Nothing"


projectionRoundTrip :: IO ()
projectionRoundTrip = do
  let !sch =
        Schema
          ( V.fromList
              [ plainField "a" False (AInt 32 True)
              , plainField "b" False (AInt 64 True)
              , plainField "c" False AUtf8
              ]
          )
          Little
          V.empty
          V.empty
      !batch =
        V.fromList
          [ primColumn PInt32 (VS.fromList ([1, 2, 3] :: [Int32]))
          , primColumn PInt64 (VS.fromList ([10, 20, 30] :: [Int64]))
          , fromTexts (V.fromList ["x", "y", "z"])
          ]
      !bytes = encodeArrowStream defaultWriteOptions sch [batch]
  case bytes >>= openStreamReader of
    Left e -> failTest $ "projection openStreamReader: " ++ e
    Right rd0 ->
      -- Ask for c then a, in that order: should drop b and reorder.
      case streamReaderProjected ["c", "a"] rd0 of
        Left e -> failTest $ "streamReaderProjected: " ++ e
        Right (projSch, batches')
          | length batches' == 1
          , [proj] <- batches'
          , V.length proj == 2
          , V.length (arrowFields projSch) == 2
          , fieldName (V.unsafeIndex (arrowFields projSch) 0) == "c"
          , fieldName (V.unsafeIndex (arrowFields projSch) 1) == "a"
          , V.unsafeIndex proj 0 == V.unsafeIndex batch 2
          , V.unsafeIndex proj 1 == V.unsafeIndex batch 0 ->
              putStrLn "OK: streamReaderProjected narrows + reorders"
          | otherwise ->
              failTest $
                "streamReaderProjected unexpected: "
                  ++ show batches'


{- | Generic single-batch round-trip helper for the high-level
'encodeArrowStream' / 'decodeArrowStream' API.
-}
highLevelRoundTrip :: String -> Schema -> V.Vector ColumnArray -> IO ()
highLevelRoundTrip label sch cols = do
  let bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case bytes >>= decodeArrowStream of
    Left e -> failTest $ label ++ ": decodeArrowStream: " ++ e
    Right (sch', batches)
      | sch' /= sch ->
          failTest $ label ++ ": schema mismatch"
      | [got] <- batches ->
          if got == cols
            then putStrLn $ "OK: high-level round-trip " ++ label
            else
              failTest $
                label
                  ++ ": column mismatch\n got: "
                  ++ show (V.toList got)
                  ++ "\n exp: "
                  ++ show (V.toList cols)
      | otherwise ->
          failTest $
            label
              ++ ": expected 1 batch, got "
              ++ show (length batches)


-- | Build a simple leaf field with no children.
plainField :: Text -> Bool -> ArrowType -> Field
plainField nm nullable ty = Field nm nullable ty V.empty Nothing V.empty


-- | Field with explicit children, no dictionary.
nestedField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
nestedField nm nullable ty children = Field nm nullable ty children Nothing V.empty


-- | Round-trip a pre-built Field/ColumnArray pair.
roundTripNested :: String -> Field -> ColumnArray -> IO ()
roundTripNested label field col = do
  let !schema =
        Schema
          { arrowEndianness = Little
          , arrowFields = V.singleton field
          , arrowMetadata = V.empty
          , arrowFeatures = V.empty
          }
      !stream = writeArrowStream schema (V.singleton (V.singleton col))
  case stream >>= readArrowStream of
    Left e -> failTest (label ++ ": readArrowStream: " ++ e)
    Right as -> do
      expect
        (label ++ ": batch count == 1")
        (V.length (asBatches as) == 1)
      let (rb, body) = V.unsafeIndex (asBatches as) 0
      case decodeRecordBatch (asSchema as) rb body of
        Left e -> failTest (label ++ ": materialize: " ++ e)
        Right cols -> do
          expect (label ++ ": column count == 1") (V.length cols == 1)
          let !got = V.unsafeIndex cols 0
          expect
            (label ++ ": null count matches")
            (nullCount got == nullCount col)
          when (got /= col) $
            failTest
              ( label
                  ++ ": got "
                  ++ show got
                  ++ ", expected "
                  ++ show col
              )
      expect (label ++ ": nested round-trip preserves column") True


{- | Round-trip a single flat (non-nested) column through a single-field
single-batch Arrow stream.
-}
roundTripPrim :: String -> ColumnArray -> IO ()
roundTripPrim label col = do
  let !ty = inferArrowType col
      !nullable = nullCount col > 0
      !schema =
        Schema
          { arrowEndianness = Little
          , arrowFields =
              V.singleton
                Field
                  { fieldName = T.pack label
                  , fieldNullable = nullable
                  , fieldType = ty
                  , fieldChildren = V.empty
                  , fieldDictionary = Nothing
                  , fieldMetadata = V.empty
                  }
          , arrowMetadata = V.empty
          , arrowFeatures = V.empty
          }
      !stream = writeArrowStream schema (V.singleton (V.singleton col))
  case stream >>= readArrowStream of
    Left e -> failTest (label ++ ": readArrowStream: " ++ e)
    Right as -> do
      expect
        (label ++ ": schema endianness")
        (arrowEndianness (asSchema as) == Little)
      expect
        (label ++ ": batch count == 1")
        (V.length (asBatches as) == 1)
      let (rb, body) = V.unsafeIndex (asBatches as) 0
      case decodeRecordBatch (asSchema as) rb body of
        Left e -> failTest (label ++ ": materialize: " ++ e)
        Right cols -> do
          expect (label ++ ": column count == 1") (V.length cols == 1)
          let !got = V.unsafeIndex cols 0
          expect
            (label ++ ": row count matches")
            (columnLength got == columnLength col)
          expect
            (label ++ ": null count and validity slot match")
            ( nullCount got == nullCount col
                && hasValiditySlot got == hasValiditySlot col
            )
          when (got /= col) $
            failTest (label ++ ": got " ++ show got ++ ", expected " ++ show col)
      expect (label ++ ": round-trip preserves column") True


{- | Derive the appropriate 'ArrowType' for a 'ColumnArray' variant
the test driver feeds in. Used only to build per-test schemas.
-}
inferArrowType :: ColumnArray -> ArrowType
inferArrowType = \case
  ColInt8 _ _ -> AInt 8 True
  ColInt16 _ _ -> AInt 16 True
  ColInt32 _ _ -> AInt 32 True
  ColInt64 _ _ -> AInt 64 True
  ColUInt8 _ _ -> AInt 8 False
  ColUInt16 _ _ -> AInt 16 False
  ColUInt32 _ _ -> AInt 32 False
  ColUInt64 _ _ -> AInt 64 False
  ColFloat16 _ _ -> AFloatingPoint Half
  ColFloat _ _ -> AFloatingPoint Single
  ColDouble _ _ -> AFloatingPoint DoublePrecision
  ColBool _ _ -> ABool
  ColUtf8 {} -> AUtf8
  ColBinary {} -> ABinary
  ColLargeUtf8 {} -> ALargeUtf8
  ColLargeBinary {} -> ALargeBinary
  ColFixedSizeBinary w _ _ _ -> AFixedSizeBinary w
  ColDate32 _ _ -> ADate DateDay
  ColDate64 _ _ -> ADate DateMillisecond
  ColTime32 _ _ -> ATime Second 32
  ColTime64 _ _ -> ATime Microsecond 64
  ColTimestamp _ _ -> ATimestamp Nanosecond Nothing
  ColDuration _ _ -> ADuration Nanosecond
  ColDecimal128 p s _ _ -> ADecimal p s
  ColDecimal256 p s _ _ -> ADecimal256 p s
  ColIntervalYearMonth _ _ -> AInterval YearMonth
  ColIntervalDayTime _ _ -> AInterval DayTime
  ColIntervalMonthDayNano _ _ -> AInterval MonthDayNano
  -- The test driver doesn't invoke inferArrowType for nested columns;
  -- those are fed through a dedicated roundTripNested helper below.
  other -> error ("inferArrowType: unsupported: " ++ columnTag other)


-- | Unwrap a validated fixture constructor, failing loudly on 'Left'.
fixture :: Either String ColumnArray -> ColumnArray
fixture = either (error . ("bad fixture: " ++)) id


expect :: String -> Bool -> IO ()
expect label True = putStrLn ("OK: " ++ label)
expect label False = failTest ("FAIL: " ++ label)


failTest :: String -> IO ()
failTest msg = do
  putStrLn msg
  exitFailure
