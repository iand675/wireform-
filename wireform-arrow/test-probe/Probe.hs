{-# LANGUAGE OverloadedStrings #-}
-- | Writes candidate Arrow IPC streams to disk for external testing
-- with pyarrow / arrow-cpp / arrow-rs, and exercises the reader by
-- consuming any pyarrow-produced sample handed in via @--read PATH@.
module Main (main) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Arrow.Column (ColumnArray (..))
import Arrow.Types
  ( ArrowType (..), DateUnit (..), Precision (..)
  , TimeUnit (..), UnionMode (..)
  , Schema (..), Field (..), DictionaryEncoding (..), Endianness (..)
  )
import Arrow.Stream
  ( BodyCompressionCodec (..)
  , WriteOptions (..)
  , decodeArrowStream
  , defaultWriteOptions
  , encodeArrowFile
  , encodeArrowStream
  , encodeArrowStream
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--read" : path : _) -> readMode path
    _                     -> writeMode (case args of { (d:_) -> d; [] -> "/tmp" })

-- | Read mode: consume any spec-compliant Arrow IPC stream and
-- print one batch per line as a 'show'd ColumnArray vector. Used
-- to verify pyarrow-produced bytes round-trip through our
-- high-level reader.
readMode :: FilePath -> IO ()
readMode path = do
  bs <- BS.readFile path
  case decodeArrowStream bs of
    Left e -> do
      putStrLn ("read error: " ++ e)
      exitFailure
    Right (sch, batches) -> do
      putStrLn ("schema: " ++ show sch)
      putStrLn ("batches: " ++ show (length batches))
      mapM_ (\cols ->
               putStrLn $ "  " ++ show (V.toList cols)) batches

-- | Smart constructor: a Field with no dictionary encoding.
pField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
pField nm nullable ty children =
  Field nm nullable ty children Nothing V.empty

-- | Field with a dictionary encoding.
dField :: Text -> Bool -> ArrowType -> Int64 -> Field
dField nm nullable ty did =
  Field nm nullable ty V.empty
    (Just (DictionaryEncoding did (AInt 32 True) False)) V.empty

writeMode :: FilePath -> IO ()
writeMode outDir = do
  -- A representative gallery covering every column type the
  -- high-level API supports. The same input shape — Schema +
  -- [V.Vector ColumnArray] — works for primitive, variable-length,
  -- nullable, post-V5, and dictionary-encoded columns.
  let writeSample name sch batches = do
        BS.writeFile (outDir <> "/ours_" <> name <> ".arrows")
          (encodeArrowStream defaultWriteOptions sch batches)

  -- 1) Pure primitives.
  writeSample "int32"
    Schema
      { arrowFields = V.singleton (pField "a" False (AInt 32 True) V.empty)
      , arrowEndianness = Little
      , arrowMetadata = V.empty
      , arrowFeatures = V.empty
      }
    [V.singleton (ColInt32 (VS.fromList ([1,2,3,4,5] :: [Int32])))]

  -- 2) Mixed primitives + variable-length + nullable.
  writeSample "mixed"
    Schema
      { arrowFields = V.fromList
          [ pField "i" False (AInt 64 True) V.empty
          , pField "s" False AUtf8           V.empty
          , pField "b" True  ABool           V.empty
          ]
      , arrowEndianness = Little
      , arrowMetadata = V.empty
      , arrowFeatures = V.empty
      }
    [V.fromList
       [ ColInt64 (VS.fromList ([10,20,30] :: [Int64]))
       , ColUtf8  (AV.utf8ViewFromVector (V.fromList ["hello","world","!"]))
       , ColBoolMaybe (V.fromList [Just True, Nothing, Just False])
       ]]

  -- 3) Post-V5: Utf8View (inline + null + out-of-line).
  writeSample "utf8view"
    Schema
      { arrowFields = V.singleton (pField "v" True AUtf8View V.empty)
      , arrowEndianness = Little
      , arrowMetadata = V.empty
      , arrowFeatures = V.empty
      }
    [V.singleton (ColUtf8ViewMaybe (V.fromList
       [ Just "short"
       , Nothing
       , Just "this string is definitely longer than twelve bytes"
       ]))]

  -- 4) Post-V5: ListView<int32>.
  writeSample "listview"
    Schema
      { arrowFields = V.singleton $
          pField "lv" False AListView
            (V.singleton (pField "item" False (AInt 32 True) V.empty))
      , arrowEndianness = Little
      , arrowMetadata = V.empty
      , arrowFeatures = V.empty
      }
    [V.singleton (ColListView
        (VS.fromList ([0,2,5] :: [Int32]))
        (VS.fromList ([2,3,1] :: [Int32]))
        (ColInt32 (VS.fromList ([10,20,30,40,50,60] :: [Int32]))))]

  -- 5) Post-V5: RunEndEncoded.
  writeSample "ree"
    Schema
      { arrowFields = V.singleton $
          pField "ree" True ARunEndEncoded $ V.fromList
            [ pField "run_ends" False (AInt 32 True) V.empty
            , pField "values"   True  (AInt 64 True) V.empty
            ]
      , arrowEndianness = Little
      , arrowMetadata = V.empty
      , arrowFeatures = V.empty
      }
    [V.singleton (ColRunEndEncoded
        (ColInt32 (VS.fromList ([3,5,8] :: [Int32])))
        (ColInt64Maybe (AC.nvFromMaybeList 0 [Just 100, Nothing, Just 300])))]

  -- 6) Dictionary-encoded utf8 — handled automatically by the
  --    high-level API: no manual DictBatch construction.
  writeSample "dict"
    Schema
      { arrowFields = V.singleton (dField "d" True AUtf8 0)
      , arrowEndianness = Little
      , arrowMetadata = V.empty
      , arrowFeatures = V.empty
      }
    [V.singleton (ColDictionary 0
        (VS.fromList ([0,1,0,2,1] :: [Int32]))
        (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a","b","c"]))))]

  -- ZSTD body compression: a 500-row int64 column compressed
  -- per Arrow's BodyCompression spec.
  let zstdOpts    = defaultWriteOptions { writeBodyCompression = Just BodyZstd }
      zstdSchema  = Schema
        (V.singleton (pField "n" False (AInt 64 True) V.empty)) Little V.empty V.empty
      zstdBatch   = V.singleton (ColInt64 (VS.fromList ([1..500] :: [Int64])))
  BS.writeFile (outDir <> "/ours_zstd_compressed.arrows")
    (encodeArrowStream zstdOpts zstdSchema [zstdBatch])

  -- File format with the same data as the int32 stream.
  let intSchema = Schema
        { arrowFields = V.singleton (pField "a" False (AInt 32 True) V.empty)
        , arrowEndianness = Little
        , arrowMetadata = V.empty
        , arrowFeatures = V.empty
        }
      intBatch = V.singleton (ColInt32 (VS.fromList ([1,2,3,4,5] :: [Int32])))
  BS.writeFile (outDir <> "/ours_int32_batch.arrow")
    (encodeArrowFile defaultWriteOptions intSchema [intBatch])

  -- File format with a dictionary-encoded column. Exercises the
  -- footer's @dictionaries: [Block]@ slot.
  let dictSchema = Schema
        { arrowFields = V.singleton (dField "d" True AUtf8 0)
        , arrowEndianness = Little
        , arrowMetadata = V.empty
        , arrowFeatures = V.empty
        }
      dictBatch = V.singleton (ColDictionary 0
        (VS.fromList ([0,1,0,2,1] :: [Int32]))
        (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["a","b","c"]))))
  BS.writeFile (outDir <> "/ours_dict.arrow")
    (encodeArrowFile defaultWriteOptions dictSchema [dictBatch])

  -- 7) Every primitive integer width (signed + unsigned).
  let intWidths =
        [ ("int8",  ColInt8  (VS.fromList ([0, 1, -1, 127, -128] :: [Int8])))
        , ("int16", ColInt16 (VS.fromList ([0, 1, -1, 32767, -32768] :: [Int16])))
        , ("uint8",  ColUInt8  (VS.fromList ([0, 1, 255] :: [Word8])))
        , ("uint16", ColUInt16 (VS.fromList ([0, 1, 65535] :: [Word16])))
        , ("uint32", ColUInt32 (VS.fromList ([0, 1, maxBound] :: [Word32])))
        , ("uint64", ColUInt64 (VS.fromList ([0, 1, maxBound] :: [Word64])))
        ]
  mapM_
    (\(nm, col) ->
       writeSample nm
         (Schema
            (V.singleton (pField "x" False (colType col) V.empty))
            Little V.empty V.empty)
         [V.singleton col])
    intWidths

  -- 8) Float / Double (Float16 deferred — the spec stores it as
  --    a Word16 view; the reader handles it but writers in
  --    pyarrow rarely round-trip without explicit Half cast).
  writeSample "float"
    (Schema (V.singleton (pField "x" False (AFloatingPoint Single) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColFloat (VS.fromList [1.5, -2.5, 3.5]))]
  writeSample "double"
    (Schema (V.singleton (pField "x" False (AFloatingPoint DoublePrecision) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColDouble (VS.fromList [1.5, -2.5, 3.14159]))]

  -- 9) Binary (raw bytes) + FixedSizeBinary.
  writeSample "binary"
    (Schema (V.singleton (pField "b" False ABinary V.empty))
            Little V.empty V.empty)
    [V.singleton (ColBinary (AV.binaryViewFromVector (V.fromList
       ["\x00\x01\x02", "\xff", "" :: ByteString])))]
  writeSample "fixedbin16"
    (Schema (V.singleton (pField "u" False (AFixedSizeBinary 16) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColFixedSizeBinary 16 (V.fromList
       [ BS.replicate 16 0xAA
       , BS.replicate 16 0x55
       ]))]

  -- 10) Date / Time / Timestamp / Duration. Each one is a
  --     primitive integer under the hood with a logical type
  --     annotation.
  writeSample "date32"
    (Schema (V.singleton (pField "d" False (ADate DateDay) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColDate32 (VS.fromList [0, 18000, -1]))]
  writeSample "time64_us"
    (Schema (V.singleton (pField "t" False (ATime Microsecond 64) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColTime64 (VS.fromList [0, 12345000000]))]
  writeSample "timestamp_ns_utc"
    (Schema (V.singleton (pField "ts" False
                            (ATimestamp Nanosecond (Just "UTC")) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColTimestamp (VS.fromList
       [0, 1700000000_000_000_000]))]
  writeSample "duration_ns"
    (Schema (V.singleton (pField "d" False (ADuration Nanosecond) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColDuration (VS.fromList [0, 60_000_000_000]))]

  -- 11) Decimal128.
  writeSample "decimal128"
    (Schema (V.singleton (pField "d" False (ADecimal 18 2) V.empty))
            Little V.empty V.empty)
    [V.singleton (ColDecimal128 18 2 (V.fromList
       [ BS.replicate 16 0
       , BS.replicate 16 1
       ]))]

  -- 12) List<int32> — the classic non-view list shape.
  writeSample "list_int32"
    (Schema (V.singleton $
        pField "lst" False AList
          (V.singleton (pField "item" False (AInt 32 True) V.empty)))
       Little V.empty V.empty)
    [V.singleton (ColList
       (VS.fromList ([0,2,5,7] :: [Int32]))
       (ColInt32 (VS.fromList [10,20,30,40,50,60,70])))]

  -- 13) Struct<int32, utf8>.
  writeSample "struct"
    (Schema (V.singleton $
        pField "s" False AStruct $ V.fromList
          [ pField "i" False (AInt 32 True) V.empty
          , pField "n" False AUtf8           V.empty
          ])
       Little V.empty V.empty)
    [V.singleton (ColStruct (V.fromList
       [ ("i", ColInt32 (VS.fromList ([1, 2, 3] :: [Int32])))
       , ("n", ColUtf8  (AV.utf8ViewFromVector (V.fromList ["a", "b", "c"])))
       ]))]

  -- 14) Map<utf8, int32>. Per Arrow spec the Map is encoded
  -- as List<Struct<key, value>> with the parent type being
  -- AMap and the (single) child being a non-nullable struct
  -- of {key, value}.
  writeSample "map_utf8_int32"
    (Schema (V.singleton $
        pField "m" False (AMap False) $ V.singleton $
          pField "entries" False AStruct $ V.fromList
            [ pField "key"   False AUtf8           V.empty
            , pField "value" True  (AInt 32 True)  V.empty
            ])
       Little V.empty V.empty)
    [V.singleton (ColMap
        (VS.fromList ([0, 2, 5] :: [Int32]))
        (ColUtf8 (AV.utf8ViewFromVector (V.fromList ["k1", "k2", "k3", "k4", "k5"])))
        (ColInt32 (VS.fromList ([10, 20, 30, 40, 50] :: [Int32]))))]

  -- 15) LargeList<int32> — 64-bit offsets.
  writeSample "large_list_int32"
    (Schema (V.singleton $
        pField "lst" False ALargeList
          (V.singleton (pField "item" False (AInt 32 True) V.empty)))
       Little V.empty V.empty)
    [V.singleton (ColLargeList
        (VS.fromList ([0, 2, 5, 7] :: [Int64]))
        (ColInt32 (VS.fromList ([10,20,30,40,50,60,70] :: [Int32]))))]

  -- 16) FixedSizeList<int32, 3>.
  writeSample "fixed_size_list3_int32"
    (Schema (V.singleton $
        pField "fsl" False (AFixedSizeList 3)
          (V.singleton (pField "item" False (AInt 32 True) V.empty)))
       Little V.empty V.empty)
    [V.singleton (ColFixedSizeList 3
        (ColInt32 (VS.fromList ([1, 2, 3, 4, 5, 6] :: [Int32]))))]

  -- 17) LargeUtf8.
  writeSample "large_utf8"
    (Schema (V.singleton (pField "s" False ALargeUtf8 V.empty))
       Little V.empty V.empty)
    [V.singleton (ColLargeUtf8 (AV.utf8ViewFromVector_l (V.fromList ["alpha", "beta", "gamma"])))]

  -- 18) DenseUnion<int32, utf8>.
  --
  --     Arrow's union has type ids in [0, n_children) and an
  --     offsets buffer indexing into the per-child storage.
  --     Spec ref: format/Layout.rst Dense Union section.
  writeSample "dense_union_int32_utf8"
    (Schema (V.singleton $
        pField "u" False (AUnion Dense (V.fromList [0, 1])) $ V.fromList
          [ pField "i" False (AInt 32 True) V.empty
          , pField "s" False AUtf8           V.empty
          ])
       Little V.empty V.empty)
    [V.singleton (ColDenseUnion
        (VS.fromList ([0, 1, 0, 1, 0] :: [Int8]))    -- type ids
        (VS.fromList ([0, 0, 1, 1, 2] :: [Int32]))   -- per-child offsets
        (V.fromList
           [ ColInt32 (VS.fromList ([10, 20, 30] :: [Int32]))
           , ColUtf8  (AV.utf8ViewFromVector (V.fromList ["a", "b"]))
           ]))]

  putStrLn ("wrote probe outputs to " ++ outDir)

-- | Pick the right ArrowType for a ColumnArray. Used by the
-- table-driven integer-width gallery so we don't have to
-- repeat the type for each writeSample call.
colType :: ColumnArray -> ArrowType
colType = \case
  ColInt8 _   -> AInt 8 True
  ColInt16 _  -> AInt 16 True
  ColInt32 _  -> AInt 32 True
  ColInt64 _  -> AInt 64 True
  ColUInt8 _  -> AInt 8 False
  ColUInt16 _ -> AInt 16 False
  ColUInt32 _ -> AInt 32 False
  ColUInt64 _ -> AInt 64 False
  ColFloat _  -> AFloatingPoint Single
  ColDouble _ -> AFloatingPoint DoublePrecision
  ColBool _   -> ABool
  _           -> error "colType: unsupported"
