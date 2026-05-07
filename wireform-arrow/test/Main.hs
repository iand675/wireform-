{-# LANGUAGE OverloadedStrings #-}
-- | Round-trip tests for the Arrow IPC writer + reader.
--
-- Two layers of coverage:
--
--   1. Internal round-trips: every 'ColumnArray' constructor
--      the writer emits must be parsed back correctly by
--      'decodeArrowStream' + 'materializeRecordBatch'.
--   2. Golden pyarrow interop: the bytes in
--      @test/golden/pa_*.arrows@ were produced by pyarrow
--      ('pa.ipc.new_stream') against the reference spec.
--      'decodeArrowStream' must accept them verbatim.
--
-- Previously (PR #16 deferred list) the wireform IPC framing
-- was a simplified encoding that pyarrow couldn't read; after
-- 'Arrow.FlatBufferIPC' landed with a real FlatBuffers layout
-- the bidirectional interop works and these tests pin it.
module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Columnar.Bit (Bit (..))
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Data.Word (Word8, Word16, Word32, Word64)
import System.Exit (exitFailure)

import Arrow.Column
  ( ColumnArray (..)
  , columnLength
  , materializeRecordBatch
  , validateMapKeysSorted
  )
import Arrow.File (asBatches, asSchema, readArrowStream)
import Arrow.Stream
  ( BodyCompressionCodec (..)
  , DictHandling (..)
  , WriteOptions (..)
  , decodeArrowStream
  , defaultWriteOptions
  , encodeArrowStream
  , openStreamReader
  , streamReaderIter
  , streamReaderNext
  , streamReaderProjected
  , streamReaderSchema
  , streamReaderToList
  )
import qualified Columnar.Stream as IS
import qualified Arrow.Record as AR
import Arrow.FlatBufferIPC
  ( buildSchemaMessage
  , decodeSchemaMessage
  , Tensor (..)
  , TensorDim (..)
  , encodeTensorFrame
  , decodeTensorFrame
  , SparseTensor (..)
  , encodeSparseTensorFrame
  , decodeSparseTensorFrame
  )
import Arrow.Types
import Arrow.Write (writeArrowStream)

-- Test records used by 'nestedStructRoundTrip'.
data Address = Address
  { cityF :: Text
  , zipF  :: Text
  } deriving (Show, Eq)

data Customer = Customer
  { nameF :: Text
  , addrF :: Address
  , ageF  :: Int32
  } deriving (Show, Eq)

-- | Customer with an optional address — exercises the
-- 'encoderFromRowEncoder' / 'decoderFromRowDecoder' nullable
-- nested-struct path.
data CustomerOpt = CustomerOpt
  { coNameF     :: Text
  , maybeAddrF  :: Maybe Address
  , coAgeF      :: Int32
  } deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "wireform-arrow writer/reader round-trip suite"

  -- Every column: build a single-batch Arrow stream containing just
  -- that column, serialise it, parse it, materialise, and assert
  -- the recovered ColumnArray equals the one we put in.

  roundTripPrim "Int8"   (ColInt8   (VS.fromList [0, 1, -1, 100, -128, 127]))
  roundTripPrim "Int16"  (ColInt16  (VS.fromList [0, 1, -1, 32767, -32768]))
  roundTripPrim "Int32"  (ColInt32  (VS.fromList [0, 1, -1, maxBound, minBound]))
  roundTripPrim "Int64"  (ColInt64  (VS.fromList [0, 1, -1, maxBound, minBound]))
  roundTripPrim "UInt8"  (ColUInt8  (VS.fromList ([0, 255] :: [Word8])))
  roundTripPrim "UInt16" (ColUInt16 (VS.fromList ([0, 65535] :: [Word16])))
  roundTripPrim "UInt32" (ColUInt32 (VS.fromList ([0, maxBound] :: [Word32])))
  roundTripPrim "UInt64" (ColUInt64 (VS.fromList ([0, maxBound] :: [Word64])))
  roundTripPrim "Float16" (ColFloat16 (VS.fromList ([0, 0x3C00, 0xBC00] :: [Word16])))
  roundTripPrim "Float"  (ColFloat  (VS.fromList [0.0, 1.5, -2.25, 3.14 :: Float]))
  roundTripPrim "Double" (ColDouble (VS.fromList [0.0, 1.5, -2.25, 3.14159265 :: Double]))
  roundTripPrim "Bool"   (ColBool   (VU.fromList (map Bit [True, False, True, False, True])))

  roundTripPrim "Date32" (ColDate32 (VS.fromList [0 :: Int32, 18000, -1]))
  roundTripPrim "Date64" (ColDate64 (VS.fromList [0 :: Int64, 1700000000000]))
  roundTripPrim "Time32" (ColTime32 (VS.fromList [0 :: Int32, 12345]))
  roundTripPrim "Time64" (ColTime64 (VS.fromList [0 :: Int64, 12345000000]))
  roundTripPrim "Timestamp"
    (ColTimestamp (VS.fromList [0 :: Int64, 1700000000_000_000_000]))
  roundTripPrim "Duration" (ColDuration (VS.fromList [0 :: Int64, 60_000_000_000]))

  roundTripPrim "IntervalYearMonth"
    (ColIntervalYearMonth (VS.fromList [0 :: Int32, 12, -6, 100]))
  roundTripPrim "IntervalDayTime"
    (ColIntervalDayTime (VS.fromList [1 :: Int32, 2, 30]) (VS.fromList [500, -1, 0]))
  roundTripPrim "IntervalMonthDayNano"
    (ColIntervalMonthDayNano
        (VS.fromList [1 :: Int32, 2])
        (VS.fromList [3 :: Int32, 4])
        (VS.fromList [1000 :: Int64, -500]))

  roundTripPrim "Decimal128" (ColDecimal128 18 2 (V.fromList
    [ BS.replicate 16 0
    , BS.pack [0x64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]))
  roundTripPrim "Decimal256" (ColDecimal256 38 4 (V.fromList
    [ BS.replicate 32 0
    , BS.pack (0x2A : replicate 31 0)
    ]))

  roundTripPrim "FixedSizeBinary"
    (ColFixedSizeBinary 4 (V.fromList
      [ BS.pack [0, 1, 2, 3], BS.pack [0xFF, 0xFE, 0xFD, 0xFC] ]))

  roundTripPrim "Utf8"   (ColUtf8   (AV.utf8ViewFromVector (V.fromList ["alpha", "beta", "", "\xe2\x9a\xa1"])))
  roundTripPrim "Binary" (ColBinary (AV.binaryViewFromVector (V.fromList [BS.pack [1,2,3], BS.empty, BS.pack [0xFF]])))
  roundTripPrim "LargeUtf8"   (ColLargeUtf8   (AV.utf8ViewFromVector_l (V.fromList ["alpha", "beta"])))
  roundTripPrim "LargeBinary" (ColLargeBinary (AV.binaryViewFromVector_l (V.fromList [BS.pack [0,1,2], BS.pack [0xFF]])))

  -- Nullable variants.
  roundTripPrim "Int8Maybe"
    (ColInt8Maybe (AC.nvFromMaybeList 0 [Just 1, Nothing, Just (-1), Just 42]))
  roundTripPrim "Int16Maybe"
    (ColInt16Maybe (AC.nvFromMaybeList 0 [Just 100, Nothing, Just (-200)]))
  roundTripPrim "Int32Maybe"
    (ColInt32Maybe (AC.nvFromMaybeList 0 [Nothing, Just 0, Just maxBound]))
  roundTripPrim "Int64Maybe"
    (ColInt64Maybe (AC.nvFromMaybeList 0 [Just 0, Nothing, Just (-1)]))
  roundTripPrim "UInt8Maybe"
    (ColUInt8Maybe (AC.nvFromMaybeList 0 [Just (0 :: Word8), Just 255, Nothing]))
  roundTripPrim "UInt16Maybe"
    (ColUInt16Maybe (AC.nvFromMaybeList 0 [Just (0 :: Word16), Nothing]))
  roundTripPrim "UInt32Maybe"
    (ColUInt32Maybe (AC.nvFromMaybeList 0 [Just (0 :: Word32), Nothing, Just maxBound]))
  roundTripPrim "UInt64Maybe"
    (ColUInt64Maybe (AC.nvFromMaybeList 0 [Just (0 :: Word64), Nothing]))
  roundTripPrim "Float16Maybe"
    (ColFloat16Maybe (AC.nvFromMaybeList 0 [Just (0 :: Word16), Just 0x3C00, Nothing]))
  roundTripPrim "FloatMaybe"
    (ColFloatMaybe (AC.nvFromMaybeList 0 [Just 1.5, Nothing, Just (-2.25 :: Float)]))
  roundTripPrim "DoubleMaybe"
    (ColDoubleMaybe (AC.nvFromMaybeList 0 [Just 1.5, Nothing]))
  roundTripPrim "BoolMaybe"
    (ColBoolMaybe (V.fromList [Just True, Nothing, Just False, Just True]))

  roundTripPrim "Utf8Maybe"
    (ColUtf8Maybe (V.fromList [Just "alpha", Nothing, Just ""]))
  roundTripPrim "BinaryMaybe"
    (ColBinaryMaybe (V.fromList [Just (BS.pack [1, 2]), Nothing]))
  roundTripPrim "LargeUtf8Maybe"
    (ColLargeUtf8Maybe (V.fromList [Nothing, Just "beta"]))
  roundTripPrim "LargeBinaryMaybe"
    (ColLargeBinaryMaybe (V.fromList [Just (BS.pack [0xFF]), Nothing]))
  roundTripPrim "FixedSizeBinaryMaybe"
    (ColFixedSizeBinaryMaybe 3
      (V.fromList [Just (BS.pack [1, 2, 3]), Nothing, Just (BS.pack [4, 5, 6])]))

  roundTripPrim "Date32Maybe" (ColDate32Maybe (AC.nvFromMaybeList 0 [Just (0 :: Int32), Nothing]))
  roundTripPrim "Date64Maybe" (ColDate64Maybe (AC.nvFromMaybeList 0 [Just (0 :: Int64), Nothing]))
  roundTripPrim "Time32Maybe" (ColTime32Maybe (AC.nvFromMaybeList 0 [Just (0 :: Int32), Nothing]))
  roundTripPrim "Time64Maybe" (ColTime64Maybe (AC.nvFromMaybeList 0 [Just (0 :: Int64), Nothing]))
  roundTripPrim "TimestampMaybe" (ColTimestampMaybe (AC.nvFromMaybeList 0 [Just (0 :: Int64), Nothing]))
  roundTripPrim "DurationMaybe" (ColDurationMaybe (AC.nvFromMaybeList 0 [Just (0 :: Int64), Nothing]))

  -- ============================================================
  -- Nested columns
  -- ============================================================

  -- Struct with two primitive children.
  roundTripNested "Struct"
    (nestedField "s" False AStruct $ V.fromList
       [ plainField "id"   False (AInt 64 True)
       , plainField "name" False AUtf8
       ])
    (ColStruct $ V.fromList
       [ ("id",   ColInt64 (VS.fromList [1, 2, 3]))
       , ("name", ColUtf8  (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"])))
       ])

  roundTripNested "StructMaybe"
    (nestedField "s" True AStruct $ V.fromList
       [ plainField "id"   False (AInt 32 True)
       , plainField "flag" False ABool
       ])
    (ColStructMaybe
       (VU.fromList (map Bit [True, False, True]))
       (V.fromList
         [ ("id",   ColInt32 (VS.fromList [1, 2, 3]))
         , ("flag", ColBool  (VU.fromList (map Bit [True, False, True])))
         ]))

  roundTripNested "List<int32>"
    (nestedField "l" False AList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColList (VS.fromList [0, 2, 2, 5])
       (ColInt32 (VS.fromList [10, 20, 30, 40, 50])))

  roundTripNested "ListMaybe<int32>"
    (nestedField "l" True AList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColListMaybe
       (VU.fromList (map Bit [True, False, True]))
       (VS.fromList [0, 2, 2, 5])
       (ColInt32 (VS.fromList [10, 20, 30, 40, 50])))

  roundTripNested "LargeList<int32>"
    (nestedField "l" False ALargeList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColLargeList (VS.fromList [0, 2, 2, 5])
       (ColInt32 (VS.fromList [1, 2, 3, 4, 5])))

  roundTripNested "LargeListMaybe<int32>"
    (nestedField "l" True ALargeList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColLargeListMaybe
       (VU.fromList (map Bit [True, False, True]))
       (VS.fromList [0, 2, 2, 5])
       (ColInt32 (VS.fromList [1, 2, 3, 4, 5])))

  roundTripNested "FixedSizeList<3 of int32>"
    (nestedField "l" False (AFixedSizeList 3) $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColFixedSizeList 3
       (ColInt32 (VS.fromList [1, 2, 3, 4, 5, 6])))

  roundTripNested "FixedSizeListMaybe<2 of int32>"
    (nestedField "l" True (AFixedSizeList 2) $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColFixedSizeListMaybe 2
       (VU.fromList (map Bit [True, False, True]))
       (ColInt32 (VS.fromList [1, 2, 3, 4, 5, 6])))

  -- Map<string, int32>. Arrow encodes maps as a list of struct
  -- <key, value> pairs; the map field has one child (the struct).
  roundTripNested "Map<string, int32>"
    (nestedField "m" False (AMap False) $ V.fromList
      [ nestedField "entries" False AStruct $ V.fromList
          [ plainField "key"   False AUtf8
          , plainField "value" False (AInt 32 True)
          ]
      ])
    (ColMap
       (VS.fromList [0, 2, 2, 3])
       (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"])))
       (ColInt32 (VS.fromList [1, 2, 3])))

  -- Dense union over (int32, utf8).
  roundTripNested "DenseUnion<int32, utf8>"
    (nestedField "u" False (AUnion Dense (V.fromList [0, 1])) $ V.fromList
      [ plainField "v_int"  False (AInt 32 True)
      , plainField "v_text" False AUtf8
      ])
    (ColDenseUnion
       (VS.fromList [0, 1, 0])
       (VS.fromList [0, 0, 1])
       (V.fromList
         [ ColInt32 (VS.fromList [100, 200])
         , ColUtf8  (AV.utf8ViewFromVector (V.fromList ["hello"]))
         ]))

  -- Sparse union over (bool, int32).
  roundTripNested "SparseUnion<bool, int32>"
    (nestedField "u" False (AUnion Sparse (V.fromList [0, 1])) $ V.fromList
      [ plainField "flag"  False ABool
      , plainField "value" False (AInt 32 True)
      ])
    (ColSparseUnion
       (VS.fromList [0, 1, 0])
       (V.fromList
         [ ColBool  (VU.fromList (map Bit [True,  False, False]))
         , ColInt32 (VS.fromList [0, 42, 0])
         ]))

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

  -- Arrow.View: Utf8View / BinaryView / View typeclass.
  viewTypeclassRoundTrip

  putStrLn "All wireform-arrow round-trip tests passed."

viewTypeclassRoundTrip :: IO ()
viewTypeclassRoundTrip = do
  let !ts = V.fromList ["alpha", "", "beta", "gamma"] :: V.Vector Text
      !u  = AV.utf8ViewFromVector ts
  expect "utf8View length matches input" (AV.vLength u == V.length ts)
  expect "utf8View round-trips through vToList"
    (AV.vToList u == V.toList ts)
  expect "utf8View slice"
    (AV.vToList (AV.vSlice 1 2 u) == ["", "beta"])
  let !bs = V.fromList [BS.pack [1,2], BS.empty, BS.pack [3,4,5,6]]
      !b  = AV.binaryViewFromVector bs
  expect "binaryView round-trip" (AV.vToList b == V.toList bs)
  expect "binaryView length" (AV.vLength b == 3)

flatBufSchemaSelfCheck :: IO ()
flatBufSchemaSelfCheck = do
  let cases =
        [ Schema (V.fromList [plainField "a" False (AInt 32 True)]) Little V.empty V.empty
        , Schema (V.fromList
            [ plainField "id"     False (AInt 64 True)
            , plainField "name"   True  AUtf8
            , plainField "amount" False (ADecimal 12 4)
            , plainField "ts"     True  (ATimestamp Nanosecond (Just "UTC"))
            , plainField "blob"   True  ABinary
            , plainField "tag"    False (AFixedSizeBinary 16)
            ]) Little V.empty V.empty
        , -- Post-V5 type tags (Utf8View / BinaryView / RunEndEncoded /
          -- ListView / LargeListView). Arrow.Column doesn't materialise
          -- their data buffers, but the schema flatbuffer round-trips
          -- so wireform can interoperate with newer Arrow producers.
          Schema (V.fromList
            [ plainField "v"   True  AUtf8View
            , plainField "b"   True  ABinaryView
            , plainField "ree" True  ARunEndEncoded
            , plainField "lv"  True  AListView
            , plainField "llv" True  ALargeListView
            ]) Little V.empty V.empty
        ]
  mapM_ (\sch -> do
            let bs = buildSchemaMessage sch
            case decodeSchemaMessage bs of
              Right got | got == sch ->
                putStrLn $ "OK: FlatBuffers schema self-roundtrip: " ++ describe sch
              Right got ->
                failTest $ "FB schema roundtrip mismatch:\n got: "
                            ++ show got ++ "\n exp: " ++ show sch
              Left e ->
                failTest $ "FB schema roundtrip decode failed: " ++ e
        ) cases
  where
    describe sch =
      "(" ++ show (V.length (arrowFields sch)) ++ " field"
        ++ (if V.length (arrowFields sch) == 1 then "" else "s") ++ ")"

flatBufRoundTrip :: IO ()
flatBufRoundTrip = do
  -- Multi-column round-trip exercising the high-level API in
  -- "Arrow.Stream": Schema + batches go in, bytes come out, and
  -- the inverse recovers the same shape.
  highLevelRoundTrip "Multi-column"
    (Schema
       { arrowFields = V.fromList
           [ plainField "i" False (AInt 32 True)
           , plainField "s" True  AUtf8
           ]
       , arrowEndianness = Little
       , arrowMetadata   = V.empty
       , arrowFeatures = V.empty
       })
    (V.fromList
       [ ColInt32      (VS.fromList ([1, 2, 3] :: [Int32]))
       , ColUtf8Maybe  (V.fromList [Just "x", Nothing, Just "z"])
       ])

  -- Post-V5 columns: writer + reader byte-compatible end to end.
  highLevelRoundTrip "Utf8View"
    (Schema (V.singleton (plainField "v" True AUtf8View)) Little V.empty V.empty)
    (V.singleton (ColUtf8ViewMaybe (V.fromList
       [ Just "short"
       , Nothing
       , Just "this string is definitely longer than twelve bytes"
       ])))

  highLevelRoundTrip "ListView<int32>"
    (Schema
       (V.singleton
          (nestedField "lv" False AListView (V.singleton
             (plainField "item" False (AInt 32 True)))))
       Little V.empty V.empty)
    (V.singleton (ColListView
       (VS.fromList ([0, 2, 5] :: [Int32]))
       (VS.fromList ([2, 3, 1] :: [Int32]))
       (ColInt32 (VS.fromList ([10,20,30,40,50,60] :: [Int32])))))

  highLevelRoundTrip "RunEndEncoded(int32, int64?)"
    (Schema
       (V.singleton
          (nestedField "ree" True ARunEndEncoded $ V.fromList
             [ plainField "run_ends" False (AInt 32 True)
             , plainField "values"   True  (AInt 64 True)
             ]))
       Little V.empty V.empty)
    (V.singleton (ColRunEndEncoded
       (ColInt32 (VS.fromList ([3, 5, 8] :: [Int32])))
       (ColInt64Maybe (AC.nvFromMaybeList 0 [Just 100, Nothing, Just 300]))))

  -- Dictionary-encoded utf8 — the high-level API auto-extracts
  -- the dictionary batch and auto-resolves on read.
  let dictField = Field "d" True AUtf8 V.empty
                    (Just (DictionaryEncoding 0 (AInt 32 True) False))
                    V.empty
  highLevelRoundTrip "Dictionary<utf8>"
    (Schema (V.singleton dictField) Little V.empty V.empty)
    (V.singleton (ColDictionary 0
        (VS.fromList ([0, 1, 0, 2, 1] :: [Int32]))
        (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"])))))

  -- ANull column: schema metadata round-trip + ColNull row count
  highLevelRoundTrip "Null"
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
    [ V.singleton (ColInt32 (VS.fromList ([1, 2] :: [Int32])))
    , V.singleton (ColInt32 (VS.fromList ([3] :: [Int32])))
    , V.singleton (ColInt32 (VS.fromList ([4, 5, 6, 7] :: [Int32])))
    ]

  -- Column projection on a multi-column stream: a 3-column
  -- batch should narrow to exactly the requested columns in
  -- the requested order.
  projectionRoundTrip

  -- ZSTD body compression (writer + reader): exercises
  -- BodyCompression on a multi-column batch, asserting the
  -- decoded values match the source.
  bodyCompressionRoundTrip BodyZstd
    (Schema (V.fromList
       [ plainField "n" False (AInt 64 True)
       , plainField "s" False AUtf8
       ]) Little V.empty V.empty)
    (V.fromList
       [ ColInt64 (VS.fromList
            ([1..1000] :: [Int64]))   -- enough bytes that ZSTD shrinks
       , ColUtf8 (AV.utf8ViewFromVector (V.replicate 1000 "highly-compressible-payload"))
       ])

  -- LZ4_FRAME body compression: same shape / sizing as the ZSTD
  -- case. Verifies the lz4-hs Codec.Lz4 frame compressor +
  -- decompressor round-trip through the full BodyCompression
  -- pipeline (per-buffer envelope, length prefix, offsets
  -- rewritten on decode, etc.).
  bodyCompressionRoundTrip LZ4Frame
    (Schema (V.fromList
       [ plainField "n" False (AInt 64 True)
       , plainField "s" False AUtf8
       ]) Little V.empty V.empty)
    (V.fromList
       [ ColInt64 (VS.fromList ([1..1000] :: [Int64]))
       , ColUtf8 (AV.utf8ViewFromVector (V.replicate 1000 "highly-compressible-payload"))
       ])

  -- DictReplaceOnChange: two batches with the SAME dict id but
  -- different values. The writer should emit two dict batches;
  -- the reader should resolve each record batch against the
  -- most-recently-emitted dict for that id.
  dictReplacementRoundTrip

  -- Tensor message round-trip.
  tensorRoundTrip

  -- SparseTensor (COO) round-trip.
  sparseTensorRoundTrip

-- | Consume the golden .arrows files in @test/golden/@ and
-- assert 'decodeArrowStream' returns the ColumnArray values we
-- expect from the pyarrow-side generator.
--
-- The fixtures:
--   pa_int32.arrows   : int32 column [1,2,3,4,5]
--   pa_mixed.arrows   : (int64, nullable utf8, nullable bool), 3 rows
--   pa_dict.arrows    : dictionary<utf8, int32> with values ["a","b","c"]
--                        and indices [0,1,0,2,1]
pyarrowGoldenRoundTrip :: IO ()
pyarrowGoldenRoundTrip = do
  goldenCheck "pa_int32.arrows"
    (V.singleton (ColInt32 (VS.fromList [1, 2, 3, 4, 5 :: Int32])))

  goldenCheck "pa_mixed.arrows"
    (V.fromList
       [ ColInt64 (VS.fromList [10, 20, 30 :: Int64])
       , ColUtf8Maybe (V.fromList [Just "alpha", Nothing, Just "gamma"])
       , ColBoolMaybe (V.fromList [Just True, Just False, Nothing])
       ])

  -- Dictionary-encoded batch: the decoder resolves
  -- ColDictionary's values against the dict batches pyarrow
  -- emitted ahead of the record batch.
  goldenDictCheck "pa_dict.arrows"
    [0, 1, 0, 2, 1]
    (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"])))

goldenCheck :: FilePath -> V.Vector ColumnArray -> IO ()
goldenCheck name expected = do
  bs <- BS.readFile ("test/golden/" <> name)
  case decodeArrowStream bs of
    Left e -> failTest $ "golden " <> name <> ": decode: " <> e
    Right (_sch, batches)
      | [b] <- batches, b == expected ->
          putStrLn $ "OK: pyarrow golden " <> name
      | otherwise ->
          failTest $ "golden " <> name
                    <> " mismatch:\n got: " <> show batches
                    <> "\n exp: " <> show [expected]

-- Dictionary-encoded batches need a bespoke matcher because
-- 'ColDictionary' carries both the indices vector and the
-- resolved values vector; we compare them piecewise.
goldenDictCheck
  :: FilePath -> [Int32] -> ColumnArray -> IO ()
goldenDictCheck name expectedIndices expectedValues = do
  bs <- BS.readFile ("test/golden/" <> name)
  case decodeArrowStream bs of
    Left e -> failTest $ "golden " <> name <> ": decode: " <> e
    Right (_sch, batches) -> case batches of
      [b] | V.length b == 1 -> case V.head b of
        ColDictionary _ idx vals
          | VS.toList idx == expectedIndices
          , vals == expectedValues ->
              putStrLn $ "OK: pyarrow golden " <> name
          | otherwise ->
              failTest $ "golden " <> name
                        <> " dict mismatch:\n idx=" <> show (VS.toList idx)
                        <> " vals=" <> show vals
        other ->
          failTest $ "golden " <> name <> " expected ColDictionary, got " <> show other
      _ -> failTest $ "golden " <> name <> " expected 1 batch with 1 column"

sparseTensorRoundTrip :: IO ()
sparseTensorRoundTrip = do
  -- Tiny 3x3 sparse int32 tensor with 2 non-zeros at (0,1) and
  -- (2,0). COO indices are Int64 pairs.
  let !idx  = BS.pack
        [ 0,0,0,0,0,0,0,0  -- row 0
        , 1,0,0,0,0,0,0,0  -- col 1
        , 2,0,0,0,0,0,0,0  -- row 2
        , 0,0,0,0,0,0,0,0  -- col 0
        ]
      !vals = BS.pack
        [ 7,0,0,0    -- value 7
        , 9,0,0,0    -- value 9
        ]
      !st = SparseTensor
        { sparseTensorType       = AInt 32 True
        , sparseTensorShape      = V.fromList
            [ TensorDim 3 "rows", TensorDim 3 "cols" ]
        , sparseNonZeroLength    = 2
        , sparseIndicesType      = AInt 64 True
        , sparseIndicesBody      = idx
        , sparseIndicesCanonical = True
        , sparseTensorBody       = vals
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
  -- 2×3 tensor of Int32: raw little-endian body, row-major.
  let !body = BS.pack
        [ 0x01,0,0,0, 0x02,0,0,0, 0x03,0,0,0
        , 0x04,0,0,0, 0x05,0,0,0, 0x06,0,0,0
        ]
      !tin = Tensor
        { tensorType = AInt 32 True
        , tensorShape = V.fromList
            [ TensorDim 2 "rows", TensorDim 3 "cols" ]
        , tensorStrides = V.empty
        , tensorBody = body
        }
      !frame = encodeTensorFrame tin
  case decodeTensorFrame frame of
    Left e -> failTest $ "decodeTensorFrame: " ++ e
    Right (tout, rest)
      | BS.null rest
      , tensorType tout == AInt 32 True
      , V.toList (tensorShape tout) ==
          [ TensorDim 2 "rows", TensorDim 3 "cols" ]
      , tensorBody tout == body ->
          putStrLn "OK: Tensor message round-trip (2x3 int32)"
      | otherwise ->
          failTest $ "tensor round-trip mismatch:\n got "
                      ++ show tout ++ " rest="
                      ++ show (BS.length rest) ++ "B"

dictReplacementRoundTrip :: IO ()
dictReplacementRoundTrip = do
  let !sch = Schema
        (V.singleton
           (Field "d" True AUtf8 V.empty
              (Just (DictionaryEncoding 0 (AInt 32 True) False))
              V.empty))
        Little V.empty V.empty
      !batch1 = V.singleton $ ColDictionary 0
        (VS.fromList [0, 1, 0])
        (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a", "b"])))
      !batch2 = V.singleton $ ColDictionary 0
        (VS.fromList [0, 1, 0])
        (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["x", "y"])))
      !opts  = defaultWriteOptions { writeDictHandling = DictReplaceOnChange }
      !bytes = encodeArrowStream opts sch [batch1, batch2]
  case decodeArrowStream bytes of
    Left e -> failTest $ "dict-replace round-trip: " ++ e
    Right (_, batches)
      | [b1, b2] <- batches
      , V.length b1 == 1
      , V.length b2 == 1 ->
          let !c1 = V.unsafeIndex b1 0
              !c2 = V.unsafeIndex b2 0
          in case (c1, c2) of
               (ColDictionary _ ix1 v1, ColDictionary _ ix2 v2)
                 | V.toList (valuesToList v1) == ["a", "b"]
                 , V.toList (valuesToList v2) == ["x", "y"]
                 , VS.toList ix1 == [0, 1, 0]
                 , VS.toList ix2 == [0, 1, 0] ->
                     putStrLn "OK: dictionary replacement across batches"
               _ -> failTest $ "dict-replace mismatch:\n got "
                                ++ show batches
      | otherwise ->
          failTest $ "dict-replace expected 2 batches, got "
                      ++ show (length batches)
  where
    valuesToList (ColUtf8 v)          = AV.utf8ViewToVector v
    valuesToList (ColUtf8Maybe v)     = V.mapMaybe id v
    valuesToList _                    = V.empty

bodyCompressionRoundTrip :: BodyCompressionCodec -> Schema -> V.Vector ColumnArray -> IO ()
bodyCompressionRoundTrip codec sch cols = do
  let !opts  = defaultWriteOptions { writeBodyCompression = Just codec }
      !bytes = encodeArrowStream opts sch [cols]
  case decodeArrowStream bytes of
    Left e -> failTest $ "body-compression round-trip: " ++ e
    Right (_sch', batches)
      | [got] <- batches, got == cols ->
          putStrLn $ "OK: body compression " ++ show codec
                       ++ " (" ++ show (BS.length bytes) ++ " bytes)"
      | otherwise ->
          failTest $ "body-compression mismatch: got "
                       ++ show batches

streamingRoundTrip :: Schema -> [V.Vector ColumnArray] -> IO ()
streamingRoundTrip sch batches = do
  let bytes = encodeArrowStream defaultWriteOptions sch batches
  case openStreamReader bytes of
    Left e -> failTest $ "openStreamReader: " ++ e
    Right rd0 -> do
      when (streamReaderSchema rd0 /= sch) $
        failTest "streamReaderSchema mismatch"
      -- Pull first batch via streamReaderNext; drain rest via toList.
      case streamReaderNext rd0 of
        Left e -> failTest $ "streamReaderNext (first): " ++ e
        Right Nothing -> failTest "streamReaderNext: stream empty"
        Right (Just (cols0, rd1)) -> do
          when (cols0 /= head batches) $
            failTest $ "streamReaderNext (first) mismatch:\n got "
                        ++ show cols0 ++ "\n exp " ++ show (head batches)
          case streamReaderToList rd1 of
            Left e -> failTest $ "streamReaderToList: " ++ e
            Right rest
              | rest == drop 1 batches ->
                  putStrLn "OK: streaming reader iterates all batches"
              | otherwise ->
                  failTest $ "streamReaderToList: tail mismatch\n got "
                              ++ show rest
                              ++ "\n exp " ++ show (drop 1 batches)
  -- Iter-shaped variant: the same drain via Columnar.Stream.
  case openStreamReader bytes of
    Left e -> failTest $ "openStreamReader (iter): " ++ e
    Right rd0 ->
      case IS.iterToList (streamReaderIter rd0) of
        Left e -> failTest $ "streamReaderIter drain: " ++ e
        Right got
          | got == batches ->
              putStrLn "OK: streamReaderIter drains all batches"
          | otherwise ->
              failTest $ "streamReaderIter mismatch:\n got "
                          ++ show got ++ "\n exp " ++ show batches

-- | Schema-level + field-level @custom_metadata@ pairs survive
-- a full encode → decode round-trip via the FlatBuffers schema
-- writer + reader.
customMetadataRoundTrip :: IO ()
customMetadataRoundTrip = do
  let !field = (plainField "n" False (AInt 32 True))
        { fieldMetadata = V.fromList [("description", "row id"), ("unit", "count")]
        }
      !sch = Schema
        { arrowFields     = V.singleton field
        , arrowEndianness = Little
        , arrowMetadata   = V.fromList
            [ ("pandas", "{}")
            , ("creator", "wireform-test")
            ]
        , arrowFeatures   = V.empty
        }
      !batch = V.singleton (ColInt32 (VS.fromList ([1, 2, 3] :: [Int32])))
      !bytes = encodeArrowStream defaultWriteOptions sch [batch]
  case decodeArrowStream bytes of
    Left e -> failTest $ "customMetadata roundtrip: " ++ e
    Right (sch', _batches) -> do
      expect "schema custom_metadata roundtrips"
        (arrowMetadata sch' == arrowMetadata sch)
      let recoveredField = V.unsafeIndex (arrowFields sch') 0
      expect "field custom_metadata roundtrips"
        (fieldMetadata recoveredField == fieldMetadata field)

-- | Nested record via 'structE' + 'structD'. The inner record
-- (Address) becomes a 'ColStruct' column inside the outer
-- record (Customer); a round-trip through 'encodeTable' /
-- 'decodeTable' must recover the exact value.
nestedStructRoundTrip :: IO ()
nestedStructRoundTrip = do
  let !addrEnc = AR.fieldE "city" cityF AR.utf8E
              <> AR.fieldE "zip"  zipF  AR.utf8E
      !addrDec = Address
              <$> AR.columnD "city" AR.utf8D
              <*> AR.columnD "zip"  AR.utf8D
      !custEnc = AR.fieldE  "name" nameF        AR.utf8E
              <> AR.structE "addr" addrF         addrEnc
              <> AR.fieldE  "age"  ageF          AR.int32E
      !custDec = Customer
              <$> AR.columnD "name" AR.utf8D
              <*> AR.structD "addr" addrDec
              <*> AR.columnD "age"  AR.int32D
      !tbl = AR.table custEnc custDec
      !rows = V.fromList
        [ Customer "Alice" (Address "Atlantis" "00001") 30
        , Customer "Bob"   (Address "Brisbane" "4000")  45
        , Customer "Carol" (Address "Calcutta" "700001") 28
        ]
      (!sch, !cols) = AR.encodeTable tbl rows
      !bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case decodeArrowStream bytes of
    Left e -> failTest $ "nested struct decode: " ++ e
    Right (sch', batches) -> case batches of
      [batch] -> case AR.decodeTable tbl sch' batch of
        Left e -> failTest $ "nested struct decodeTable: " ++ e
        Right got
          | got == rows ->
              putStrLn "OK: nested struct via structE / structD"
          | otherwise ->
              failTest $ "nested struct mismatch:\n got "
                          ++ show (V.toList got)
                          ++ "\n exp " ++ show (V.toList rows)
      _ -> failTest "nested struct: expected 1 batch"

-- | Nullable nested record. Same shape as 'nestedStructRoundTrip'
-- but the @addr@ column is @Maybe Address@; the encoder builds
-- a 'ColStructMaybe' with a top-level validity mask, and the
-- decoder reconstructs the @Just@/@Nothing@ pattern.
nullableNestedStructRoundTrip :: IO ()
nullableNestedStructRoundTrip = do
  let !addrEnc = AR.fieldE "city" cityF AR.utf8E
              <> AR.fieldE "zip"  zipF  AR.utf8E
      !addrDec = Address
              <$> AR.columnD "city" AR.utf8D
              <*> AR.columnD "zip"  AR.utf8D
      !custEnc =     AR.fieldE      "name"     coNameF    AR.utf8E
                  <> AR.structEMaybe "addr_opt" maybeAddrF addrEnc
                  <> AR.fieldE      "age"      coAgeF     AR.int32E
      !custDec = CustomerOpt
              <$> AR.columnD      "name"     AR.utf8D
              <*> AR.structDMaybe "addr_opt" addrDec
              <*> AR.columnD      "age"      AR.int32D
      !tbl  = AR.table custEnc custDec :: AR.Table CustomerOpt
      !rows = V.fromList
        [ CustomerOpt "Alice" (Just (Address "Atlantis" "00001")) 30
        , CustomerOpt "Bob"   Nothing                              45
        , CustomerOpt "Carol" (Just (Address "Calcutta" "700001")) 28
        , CustomerOpt "Dave"  Nothing                              50
        ]
      (!sch, !cols) = AR.encodeTable tbl rows
      !bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case decodeArrowStream bytes of
    Left e -> failTest $ "nullable nested struct decode: " ++ e
    Right (sch', batches) -> case batches of
      [batch] -> case AR.decodeTable tbl sch' batch of
        Left e -> failTest $ "nullable nested decodeTable: " ++ e
        Right got
          | got == rows ->
              putStrLn "OK: nullable nested struct via structEMaybe / structDMaybe"
          | otherwise ->
              failTest $ "nullable nested mismatch:\n got "
                          ++ show (V.toList got)
                          ++ "\n exp " ++ show (V.toList rows)
      _ -> failTest "nullable nested struct: expected 1 batch"

-- | 'schemaFingerprint' tests: determinism, equivalence-class
-- equality, and difference detection.
schemaFingerprintTests :: IO ()
schemaFingerprintTests = do
  let !sch1 = Schema
        (V.fromList
          [ plainField "id"   False (AInt 64 True)
          , plainField "name" True  AUtf8
          ]) Little V.empty V.empty
      !sch2 = Schema
        (V.fromList
          [ plainField "id"   False (AInt 64 True)
          , plainField "name" True  AUtf8
          ])
        Little
        (V.fromList [("creator", "wireform")])  -- different annotation
        (V.fromList [FeatureDictionaryReplacement])  -- different feature flag
      !sch3 = Schema
        (V.fromList
          [ plainField "id"    False (AInt 64 True)
          , plainField "name2" True  AUtf8  -- different field name
          ]) Little V.empty V.empty
      !fp1 = schemaFingerprint sch1
      !fp2 = schemaFingerprint sch2
      !fp3 = schemaFingerprint sch3
  expect "fingerprint is deterministic across calls"
    (fp1 == schemaFingerprint sch1)
  expect "fingerprint ignores annotation fields"
    (fp1 == fp2)
  expect "fingerprint distinguishes different field names"
    (fp1 /= fp3)
  expect "schemaEquivalent matches fingerprint equality (1==2)"
    (schemaEquivalent sch1 sch2 == (fp1 == fp2))
  expect "schemaEquivalent matches fingerprint equality (1==3)"
    (schemaEquivalent sch1 sch3 == (fp1 == fp3))

-- | 'NameStrategy', 'columnDWithDefault', 'projectTable',
-- 'subsetTable', and 'validateMapKeysSorted' tests.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

recordHelperTests :: IO ()
recordHelperTests = do
  -- NameStrategy
  expect "NameAsIs is identity"
    (AR.applyNameStrategy AR.NameAsIs "userId" == "userId")
  expect "NameSnakeCase userId -> user_id"
    (AR.applyNameStrategy AR.NameSnakeCase "userId" == "user_id")
  expect "NameSnakeCase userIDValue -> user_id_value (acronym boundary)"
    (AR.applyNameStrategy AR.NameSnakeCase "userIDValue" == "user_id_value")
  expect "NameSnakeCase XMLHttpRequest -> xml_http_request"
    (AR.applyNameStrategy AR.NameSnakeCase "XMLHttpRequest" == "xml_http_request")
  expect "NameCamelCase user_id -> userId"
    (AR.applyNameStrategy AR.NameCamelCase "user_id" == "userId")
  expect "NameUpperSnakeCase userId -> USER_ID"
    (AR.applyNameStrategy AR.NameUpperSnakeCase "userId" == "USER_ID")

  -- validateMapKeysSorted
  -- Build a ColMap with sorted keys vs unsorted keys.
  let !sortedKeys = ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"]))
      !unsortedKeys = ColUtf8 (AV.utf8ViewFromVector (V.fromList ["b", "a", "c"]))
      !vals     = ColInt32 (VS.fromList [1, 2, 3 :: Int32])
      !offsets  = VS.fromList [0, 3 :: Int32]
      !sortedMap   = ColMap offsets sortedKeys vals
      !unsortedMap = ColMap offsets unsortedKeys vals
  case validateMapKeysSorted sortedMap of
    Right () -> putStrLn "OK: validateMapKeysSorted accepts sorted keys"
    Left e   -> failTest $ "expected sorted accept, got " ++ e
  case validateMapKeysSorted unsortedMap of
    Left _  -> putStrLn "OK: validateMapKeysSorted rejects unsorted keys"
    Right () -> failTest "validateMapKeysSorted should have rejected unsorted"

  -- columnDWithDefault: missing column substitutes the default.
  -- Build a writer that emits only (name, age); the reader
  -- expects (name, age, opt) and falls back on the default
  -- for the missing 'opt' column.
  let !partialEnc =
              AR.fieldE "name" (fst3 :: (Text, Int32, Text) -> Text)  AR.utf8E
           <> AR.fieldE "age"  (snd3 :: (Text, Int32, Text) -> Int32) AR.int32E
      !partialDec =
              (\n a -> (n, a, "" :: Text))
            <$> AR.columnD "name" AR.utf8D
            <*> AR.columnD "age"  AR.int32D
      !partialTbl = AR.table partialEnc partialDec
        :: AR.Table (Text, Int32, Text)
      !partialRows = V.fromList
        [ ("Alice" :: Text, 30 :: Int32, "ignored" :: Text)
        , ("Bob",          45,           "ignored")
        ]
      (!partialSch, !partialCols) = AR.encodeTable partialTbl partialRows
      !fullDec =
              (\n a o -> (n, a, o))
            <$> AR.columnD "name" AR.utf8D
            <*> AR.columnD "age"  AR.int32D
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
        let !enc = AR.fieldE "a" (\(x, _, _) -> x :: Int32) AR.int32E
                <> AR.fieldE "b" (\(_, y, _) -> y :: Int32) AR.int32E
                <> AR.fieldE "c" (\(_, _, z) -> z :: Int32) AR.int32E
            !dec = (,,) <$> AR.columnD "a" AR.int32D
                       <*> AR.columnD "b" AR.int32D
                       <*> AR.columnD "c" AR.int32D
            !tbl = AR.table enc dec :: AR.Table (Int32, Int32, Int32)
            !rs = V.fromList [(1, 10, 100), (2, 20, 200)]
        in AR.encodeTable tbl rs
  case AR.projectTable ["c", "a"] schWide colsWide of
    Just (sch', cols') -> do
      let !names = V.toList (V.map fieldName (arrowFields sch'))
      expect ("projectTable preserves order: got " ++ show names)
             (names == ["c", "a"])
      expect "projectTable yields matching column count"
             (V.length cols' == 2)
    Nothing -> failTest "projectTable returned Nothing for present cols"
  case AR.projectTable ["c", "missing"] schWide colsWide of
    Nothing -> putStrLn "OK: projectTable returns Nothing for missing column"
    Just _  -> failTest "projectTable should have returned Nothing"

  -- subsetTable: build a Table whose encoder emits only some
  -- columns
  let !custTbl = AR.table
        (    AR.fieldE "name" (fst :: (Text, Int32) -> Text) AR.utf8E
          <> AR.fieldE "age"  (snd :: (Text, Int32) -> Int32) AR.int32E)
        ((,) <$> AR.columnD "name" AR.utf8D
             <*> AR.columnD "age"  AR.int32D)
        :: AR.Table (Text, Int32)
  case AR.subsetTable ["name"] custTbl of
    Just sub -> do
      let !rsSub = V.fromList [("Alice" :: Text, 30 :: Int32), ("Bob", 45)]
          (!schSub, !colsSub) = AR.encodeTable sub rsSub
      expect "subsetTable schema has 1 field"
             (V.length (arrowFields schSub) == 1)
      expect "subsetTable schema field is 'name'"
             (V.toList (V.map fieldName (arrowFields schSub)) == ["name"])
      expect "subsetTable encoded 1 column"
             (V.length colsSub == 1)
    Nothing -> failTest "subsetTable returned Nothing for ['name']"
  case AR.subsetTable ["nope"] custTbl of
    Nothing -> putStrLn "OK: subsetTable returns Nothing for missing column"
    Just _  -> failTest "subsetTable should have returned Nothing"

projectionRoundTrip :: IO ()
projectionRoundTrip = do
  let !sch = Schema
        (V.fromList
           [ plainField "a" False (AInt 32 True)
           , plainField "b" False (AInt 64 True)
           , plainField "c" False AUtf8
           ])
        Little V.empty V.empty
      !batch = V.fromList
        [ ColInt32 (VS.fromList ([1, 2, 3] :: [Int32]))
        , ColInt64 (VS.fromList ([10, 20, 30] :: [Int64]))
        , ColUtf8  (AV.utf8ViewFromVector (V.fromList ["x", "y", "z"]))
        ]
      !bytes = encodeArrowStream defaultWriteOptions sch [batch]
  case openStreamReader bytes of
    Left e -> failTest $ "projection openStreamReader: " ++ e
    Right rd0 ->
      -- Ask for c then a, in that order — should drop b and reorder.
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
              failTest $ "streamReaderProjected unexpected: "
                          ++ show batches'

-- | Generic single-batch round-trip helper for the high-level
-- 'encodeArrowStream' / 'decodeArrowStream' API.
highLevelRoundTrip :: String -> Schema -> V.Vector ColumnArray -> IO ()
highLevelRoundTrip label sch cols = do
  let bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case decodeArrowStream bytes of
    Left e -> failTest $ label ++ ": decodeArrowStream: " ++ e
    Right (sch', batches)
      | length batches /= 1 ->
          failTest $ label ++ ": expected 1 batch, got "
                            ++ show (length batches)
      | sch' /= sch ->
          failTest $ label ++ ": schema mismatch"
      | otherwise ->
          let !got = head batches
          in  if got == cols
                then putStrLn $ "OK: high-level round-trip " ++ label
                else failTest $ label
                                 ++ ": column mismatch\n got: "
                                 ++ show (V.toList got)
                                 ++ "\n exp: " ++ show (V.toList cols)

-- | Build a simple leaf field with no children.
plainField :: Text -> Bool -> ArrowType -> Field
plainField nm nullable ty = Field nm nullable ty V.empty Nothing V.empty

-- | Field with explicit children, no dictionary.
nestedField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
nestedField nm nullable ty children = Field nm nullable ty children Nothing V.empty

-- | Round-trip a pre-built Field/ColumnArray pair.
roundTripNested :: String -> Field -> ColumnArray -> IO ()
roundTripNested label field col = do
  let !schema = Schema
        { arrowEndianness = Little
        , arrowFields = V.singleton field
        , arrowMetadata = V.empty
        , arrowFeatures = V.empty
        }
      !stream = writeArrowStream schema (V.singleton (V.singleton col))
  case readArrowStream stream of
    Left e -> failTest (label ++ ": readArrowStream: " ++ e)
    Right as -> do
      expect (label ++ ": batch count == 1")
        (V.length (asBatches as) == 1)
      let (rb, body) = V.unsafeIndex (asBatches as) 0
      case materializeRecordBatch (asSchema as) rb body of
        Left e -> failTest (label ++ ": materialize: " ++ e)
        Right cols -> do
          expect (label ++ ": column count == 1") (V.length cols == 1)
          let !got = V.unsafeIndex cols 0
          when (got /= col) $
            failTest (label ++ ": got " ++ show got
                             ++ ", expected " ++ show col)
      expect (label ++ ": nested round-trip preserves column") True

-- | Round-trip a single flat (non-nested) column through a single-field
-- single-batch Arrow stream.
roundTripPrim :: String -> ColumnArray -> IO ()
roundTripPrim label col = do
  let !ty       = inferArrowType col
      !nullable = isNullable col
      !schema   = Schema
        { arrowEndianness = Little
        , arrowFields = V.singleton Field
            { fieldName = T.pack label
            , fieldNullable = nullable
            , fieldType = ty
            , fieldChildren = V.empty
            , fieldDictionary = Nothing
            , fieldMetadata   = V.empty
            }
        , arrowMetadata = V.empty
        , arrowFeatures = V.empty
        }
      !stream = writeArrowStream schema (V.singleton (V.singleton col))
  case readArrowStream stream of
    Left e -> failTest (label ++ ": readArrowStream: " ++ e)
    Right as -> do
      expect (label ++ ": schema endianness")
        (arrowEndianness (asSchema as) == Little)
      expect (label ++ ": batch count == 1")
        (V.length (asBatches as) == 1)
      let (rb, body) = V.unsafeIndex (asBatches as) 0
      case materializeRecordBatch (asSchema as) rb body of
        Left e -> failTest (label ++ ": materialize: " ++ e)
        Right cols -> do
          expect (label ++ ": column count == 1") (V.length cols == 1)
          let !got = V.unsafeIndex cols 0
          expect (label ++ ": row count matches")
            (columnLength got == columnLength col)
          when (got /= col) $
            failTest (label ++ ": got " ++ show got ++ ", expected " ++ show col)
      expect (label ++ ": round-trip preserves column") True

-- | Derive the appropriate 'ArrowType' for a 'ColumnArray' variant
-- the test driver feeds in. Used only to build per-test schemas.
inferArrowType :: ColumnArray -> ArrowType
inferArrowType = \case
  ColInt8 _  -> AInt 8  True
  ColInt16 _ -> AInt 16 True
  ColInt32 _ -> AInt 32 True
  ColInt64 _ -> AInt 64 True
  ColUInt8 _  -> AInt 8  False
  ColUInt16 _ -> AInt 16 False
  ColUInt32 _ -> AInt 32 False
  ColUInt64 _ -> AInt 64 False
  ColFloat16 _ -> AFloatingPoint Half
  ColFloat _   -> AFloatingPoint Single
  ColDouble _  -> AFloatingPoint DoublePrecision
  ColBool _    -> ABool
  ColUtf8 _    -> AUtf8
  ColBinary _  -> ABinary
  ColLargeUtf8 _   -> ALargeUtf8
  ColLargeBinary _ -> ALargeBinary
  ColFixedSizeBinary w _ -> AFixedSizeBinary w
  ColDate32 _ -> ADate DateDay
  ColDate64 _ -> ADate DateMillisecond
  ColTime32 _ -> ATime Second 32
  ColTime64 _ -> ATime Microsecond 64
  ColTimestamp _ -> ATimestamp Nanosecond Nothing
  ColDuration _ -> ADuration Nanosecond
  ColDecimal128 p s _ -> ADecimal p s
  ColDecimal256 p s _ -> ADecimal256 p s
  ColIntervalYearMonth _ -> AInterval YearMonth
  ColIntervalDayTime _ _ -> AInterval DayTime
  ColIntervalMonthDayNano _ _ _ -> AInterval MonthDayNano

  ColInt8Maybe _  -> AInt 8  True
  ColInt16Maybe _ -> AInt 16 True
  ColInt32Maybe _ -> AInt 32 True
  ColInt64Maybe _ -> AInt 64 True
  ColUInt8Maybe _  -> AInt 8  False
  ColUInt16Maybe _ -> AInt 16 False
  ColUInt32Maybe _ -> AInt 32 False
  ColUInt64Maybe _ -> AInt 64 False
  ColFloat16Maybe _ -> AFloatingPoint Half
  ColFloatMaybe _   -> AFloatingPoint Single
  ColDoubleMaybe _  -> AFloatingPoint DoublePrecision
  ColBoolMaybe _    -> ABool
  ColUtf8Maybe _    -> AUtf8
  ColBinaryMaybe _  -> ABinary
  ColLargeUtf8Maybe _   -> ALargeUtf8
  ColLargeBinaryMaybe _ -> ALargeBinary
  ColFixedSizeBinaryMaybe w _ -> AFixedSizeBinary w
  ColDate32Maybe _ -> ADate DateDay
  ColDate64Maybe _ -> ADate DateMillisecond
  ColTime32Maybe _ -> ATime Second 32
  ColTime64Maybe _ -> ATime Microsecond 64
  ColTimestampMaybe _ -> ATimestamp Nanosecond Nothing
  ColDurationMaybe _ -> ADuration Nanosecond

  -- The test driver doesn't invoke inferArrowType for nested columns;
  -- those are fed through a dedicated roundTripNested helper below.
  other -> error ("inferArrowType: unsupported: " ++ show other)

isNullable :: ColumnArray -> Bool
isNullable = \case
  ColInt8Maybe _     -> True
  ColInt16Maybe _    -> True
  ColInt32Maybe _    -> True
  ColInt64Maybe _    -> True
  ColUInt8Maybe _    -> True
  ColUInt16Maybe _   -> True
  ColUInt32Maybe _   -> True
  ColUInt64Maybe _   -> True
  ColFloat16Maybe _  -> True
  ColFloatMaybe _    -> True
  ColDoubleMaybe _   -> True
  ColBoolMaybe _     -> True
  ColUtf8Maybe _     -> True
  ColBinaryMaybe _   -> True
  ColLargeUtf8Maybe _   -> True
  ColLargeBinaryMaybe _ -> True
  ColFixedSizeBinaryMaybe _ _ -> True
  ColDate32Maybe _   -> True
  ColDate64Maybe _   -> True
  ColTime32Maybe _   -> True
  ColTime64Maybe _   -> True
  ColTimestampMaybe _ -> True
  ColDurationMaybe _ -> True
  _                  -> False

expect :: String -> Bool -> IO ()
expect label True  = putStrLn ("OK: " ++ label)
expect label False = failTest ("FAIL: " ++ label)

failTest :: String -> IO ()
failTest msg = do
  putStrLn msg
  exitFailure
