{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Malformed-input robustness for the Arrow IPC decoders.

The corpus is a set of valid encodings (wireform's stream and file
writers over flat, nullable, nested, dictionary, view, run-end-encoded
and list-view columns, plus the pyarrow goldens). Each property mutates
a corpus entry, or generates raw bytes, and feeds the result to every
public decode entry point. A decoder must return 'Left' or a 'Right'
that can be forced to normal form; it must never throw, hang, or
allocate past 'allocationLimitBytes'.

Decoded columns alias the input and their 'NFData' instance forces
only the outer constructor, so "forced to normal form" means more for
a column-producing decoder: every accessor is run over the column's
rows in bounded windows (children, dictionary values and run ends
included), the column must read back through the row model of
"Test.Arrow.Gen" without an out-of-range reference, and it must equal
itself and its 'copyColumn'. Windows matter because an all-null,
fieldless or run-end-encoded column can name billions of rows for a
few hundred input bytes; walking all of them would measure the test,
not the decoder.
-}
module Test.Arrow.Malformed (tests) where

import Arrow.Column (
  ChildRange (..),
  ColumnArray,
  PrimArray (..),
  PrimType (..),
  anyBytesAt,
  anyTextAt,
  boolAt,
  columnLength,
  copyColumn,
  dictKeyAt,
  expandDictionary,
  fromBools,
  fromByteStrings,
  fromMaybeBinaryView,
  fromMaybeBools,
  fromMaybeFixedSizeBinary,
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
  nullCount,
  primAt,
  primColumn,
  sliceColumnArray,
  toBoolVector,
  toBytesVector,
  toTextVector,
  validity,
  validityFromBools,
  withPrim,
  pattern ColNull,
  pattern ColPrim,
 )
import Arrow.Column.Internal qualified as I
import Arrow.File qualified as File
import Arrow.Read.Columns (ReadOptions (..), defaultReadOptions)
import Arrow.FlatBufferIPC (
  ArrowBlock (..),
  DictBatch (..),
  FileIndex (..),
  StreamFrame (..),
  buildDictionaryBatchMessage,
  buildRecordBatchMessage,
  buildSchemaMessage,
  bodyCompressionAvailable,
  compressBufferEither,
  decodeSparseTensorFrame,
  decodeTensorFrame,
  encapsulateMessage,
  readArrowFileFBWithDicts,
  readFileIndex,
  readArrowStreamFBInterleaved,
  writeArrowFileFBWithDicts,
 )
import Arrow.IPC (decodeIPCMessage, validateRecordBatchBuffers)
import Arrow.Stream (
  decodeArrowFile,
  decodeArrowStream,
  WriteOptions (..),
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowStream,
  fileReaderToList,
  openFileReader,
  openStreamReader,
  streamReaderSchema,
  streamReaderToList,
 )
import Arrow.Types
import Arrow.Vector qualified as AV
import Arrow.Write qualified as Write
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Bits (complementBit, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Storable qualified as VS
import Data.ByteString.Unsafe qualified as BSU
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Data.Word (Word32, Word64, Word8)
import FlatBuffers.Builder qualified as FB
import Hedgehog hiding (Seed)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Hedgehog.Internal.Property (CoverPercentage)
import System.Mem (disableAllocationLimit, enableAllocationLimit, setAllocationCounter)
import System.Timeout (timeout)
import Test.Arrow.Gen qualified as G


tests :: IO Bool
tests = do
  goldens <- traverse loadGolden ["pa_int32.arrows", "pa_mixed.arrows", "pa_dict.arrows"]
  let !corpus = builtCorpus ++ goldens
  checkParallel $
    Group
      "Arrow.Malformed"
      [ ("unmutated corpus decodes", prop_corpusDecodes corpus)
      , ("truncation at any point", prop_truncation corpus)
      , ("random bit flips", prop_bitFlips corpus)
      , ("random byte overwrites", prop_overwrites corpus)
      , ("spliced, deleted and duplicated ranges", prop_splices corpus)
      , ("hostile buffer descriptors and lengths", prop_descriptors corpus)
      , ("schema/type confusion over valid bodies", prop_schemaConfusion corpus)
      , ("pure random bytes", prop_randomBytes)
      , ("random bytes behind valid framing", prop_framedRandom)
      , ("aliased FlatBuffers offsets (DAG bomb)", prop_dagBomb)
      , ("schema nested past the depth limit", prop_deepSchema)
      , ("unbacked rows from a tiny header", prop_unbackedRows)
      , ("buffer descriptor validation bounds", prop_bufferValidator)
      , ("compressed buffer length claims", prop_decompressionClaims)
      , ("decode budgets bound rows, values, unbacked rows and decompression", prop_readerBudgets)
      , ("mutated writer output over zero-copy decode never throws from any accessor", prop_zeroCopyAccessorFuzz)
      ]


-- * Corpus

data Encoding = StreamEnc | FileEnc
  deriving stock (Show, Eq)


data Seed = Seed
  { seedName :: String
  , seedEncoding :: Encoding
  , seedBytes :: ByteString
  }


instance Show Seed where
  show s = seedName s ++ " (" ++ show (seedEncoding s) ++ ", " ++ show (BS.length (seedBytes s)) ++ " bytes)"


loadGolden :: FilePath -> IO Seed
loadGolden name = Seed ("golden " ++ name) StreamEnc <$> BS.readFile ("test/golden/" ++ name)


plain :: Text -> Bool -> ArrowType -> Field
plain nm nullable ty = Field nm nullable ty V.empty Nothing V.empty


nested :: Text -> Bool -> ArrowType -> [Field] -> Field
nested nm nullable ty cs = Field nm nullable ty (V.fromList cs) Nothing V.empty


schemaOf :: [Field] -> Schema
schemaOf fs = Schema (V.fromList fs) Little V.empty V.empty


-- | Every shape is encoded as a stream and as a file by the high-level
-- writer, uncompressed and with each body-compression codec; the flat
-- shapes also go through 'Arrow.Write'. The shapes match their
-- schemas, so an encoder 'Left' is a bug in the corpus and aborts.
builtCorpus :: [Seed]
builtCorpus = concatMap encodeShape shapes ++ concatMap viaWrite (take 2 shapes)
  where
    encodeShape (nm, sch, batches) = concatMap (encodeWith nm sch batches) codecs
    encodeWith nm sch batches (suffix, codec) =
      let opts = defaultWriteOptions {writeBodyCompression = codec}
      in [ Seed (nm ++ suffix) StreamEnc (built nm (encodeArrowStream opts sch batches))
         , Seed (nm ++ suffix) FileEnc (built nm (encodeArrowFile opts sch batches))
         ]
    codecs = ("", Nothing) : [(suffix, Just codec) | (suffix, codec) <- [(" (zstd)", BodyZstd), (" (lz4)", LZ4Frame)], bodyCompressionAvailable codec]
    viaWrite (nm, sch, batches) =
      [ Seed (nm ++ " via Arrow.Write") StreamEnc (built nm (Write.writeArrowStream sch (V.fromList batches)))
      , Seed (nm ++ " via Arrow.Write") FileEnc (built nm (Write.writeArrowFile sch (V.fromList batches)))
      ]
    built nm = either (\e -> error ("Test.Arrow.Malformed: shape " ++ nm ++ " does not encode: " ++ e)) id


shapes :: [(String, Schema, [V.Vector ColumnArray])]
shapes =
  [ ( "flat"
    , schemaOf
        [ plain "i32" False (AInt 32 True)
        , plain "i64" False (AInt 64 True)
        , plain "u16" False (AInt 16 False)
        , plain "f64" False (AFloatingPoint DoublePrecision)
        , plain "s" False AUtf8
        , plain "b" False ABinary
        , plain "flag" False ABool
        , plain "day" False (ADate DateDay)
        ]
    , [ V.fromList
          [ prim PInt32 [1, -2, 3]
          , prim PInt64 [10, 20, 30]
          , prim PUInt16 [7, 8, 9]
          , prim PDouble [1.5, 2.5, -0.5]
          , fromTexts (V.fromList ["alpha", "", "gamma"])
          , fromByteStrings (V.fromList ["\0\1", "\255", ""])
          , fromBools (V.fromList [True, False, True])
          , prim PDate32 [0, 19000, -1]
          ]
      , V.fromList
          [ prim PInt32 [4]
          , prim PInt64 [40]
          , prim PUInt16 [10]
          , prim PDouble [4.25]
          , fromTexts (V.fromList ["delta"])
          , fromByteStrings (V.fromList ["xyz"])
          , fromBools (V.fromList [False])
          , prim PDate32 [1]
          ]
      ]
    )
  , ( "nullable"
    , schemaOf
        [ plain "i32" True (AInt 32 True)
        , plain "s" True AUtf8
        , plain "flag" True ABool
        , plain "f64" True (AFloatingPoint DoublePrecision)
        , plain "ls" True ALargeUtf8
        , plain "fsb" True (AFixedSizeBinary 3)
        ]
    , [ V.fromList
          [ fromMaybes PInt32 (V.fromList [Just 1, Nothing, Just 3, Nothing])
          , fromMaybeTexts (V.fromList [Just "x", Nothing, Just "zz", Just ""])
          , fromMaybeBools (V.fromList [Nothing, Just True, Just False, Nothing])
          , fromMaybes PDouble (V.fromList [Just 0, Nothing, Nothing, Just 9.5])
          , fromMaybeLargeTexts (V.fromList [Just "large", Nothing, Just "u", Nothing])
          , ok (fromMaybeFixedSizeBinary 3 (V.fromList [Just "abc", Nothing, Just "def", Just "ghi"]))
          ]
      ]
    )
  , ( "struct"
    , schemaOf
        [ nested "s" False AStruct [plain "id" False (AInt 64 True), plain "name" False AUtf8]
        , nested "sm" True AStruct [plain "id" False (AInt 32 True), plain "flag" False ABool]
        ]
    , [ V.fromList
          [ ok (mkStruct 3 Nothing (V.fromList [("id", prim PInt64 [1, 2, 3]), ("name", fromTexts (V.fromList ["a", "b", "c"]))]))
          , ok
              ( mkStruct
                  3
                  (validityFromBools (V.fromList [True, False, True]))
                  (V.fromList [("id", prim PInt32 [1, 2, 3]), ("flag", fromBools (V.fromList [True, False, True]))])
              )
          ]
      ]
    )
  , ( "lists"
    , schemaOf
        [ nested "l" False AList [plain "item" False (AInt 32 True)]
        , nested "lm" True AList [plain "item" False (AInt 32 True)]
        , nested "ll" False ALargeList [plain "item" False AUtf8]
        , nested "fsl" False (AFixedSizeList 2) [plain "item" False (AInt 32 True)]
        , nested "fslm" True (AFixedSizeList 2) [plain "item" False (AInt 32 True)]
        ]
    , [ V.fromList
          [ ok (mkList Nothing (VS.fromList [0, 2, 2, 5]) (prim PInt32 [10, 20, 30, 40, 50]))
          , ok (mkList (validityFromBools (V.fromList [True, False, True])) (VS.fromList [0, 1, 1, 3]) (prim PInt32 [7, 8, 9]))
          , ok (mkLargeList Nothing (VS.fromList [0, 1, 3, 3]) (fromTexts (V.fromList ["p", "q", "r"])))
          , ok (mkFixedSizeList 2 3 Nothing (prim PInt32 [1, 2, 3, 4, 5, 6]))
          , ok (mkFixedSizeList 2 3 (validityFromBools (V.fromList [True, False, True])) (prim PInt32 [1, 2, 3, 4, 5, 6]))
          ]
      ]
    )
  , ( "map"
    , schemaOf
        [ nested "m" False (AMap False) [nested "entries" False AStruct [plain "key" False AUtf8, plain "value" False (AInt 32 True)]]
        ]
    , [V.singleton (ok (mkMap Nothing (VS.fromList [0, 2, 2, 3]) (fromTexts (V.fromList ["a", "b", "c"])) (prim PInt32 [1, 2, 3])))]
    )
  , ( "unions"
    , schemaOf
        [ nested "du" False (AUnion Dense (V.fromList [0, 1])) [plain "v_int" False (AInt 32 True), plain "v_text" False AUtf8]
        , nested "su" False (AUnion Sparse (V.fromList [0, 1])) [plain "flag" False ABool, plain "value" False (AInt 32 True)]
        ]
    , [ V.fromList
          [ ok (mkDenseUnion (VS.fromList [0, 1, 0]) (VS.fromList [0, 0, 1]) (V.fromList [prim PInt32 [100, 200], fromTexts (V.fromList ["hello"])]))
          , ok (mkSparseUnion (VS.fromList [0, 1, 0]) (V.fromList [fromBools (V.fromList [True, False, False]), prim PInt32 [0, 42, 0]]))
          ]
      ]
    )
  , ( "dictionary"
    , schemaOf [Field "d" False AUtf8 V.empty (Just (DictionaryEncoding 0 (AInt 32 True) False)) V.empty]
    , [V.singleton (ok (mkDictionary 0 (prim PInt32 [0, 1, 0, 2, 1]) (fromTexts (V.fromList ["a", "b", "c"]))))]
    )
  , ( "views"
    , schemaOf [plain "v" False AUtf8View, plain "bv" True ABinaryView]
    , [ V.fromList
          [ fromMaybeUtf8View (V.fromList (map Just ["short", "this string is definitely longer than twelve bytes", ""]))
          , fromMaybeBinaryView (V.fromList [Just "tiny", Nothing, Just "another payload well past the inline limit"])
          ]
      ]
    )
  , ( "run-end encoded"
    , schemaOf [nested "ree" True ARunEndEncoded [plain "run_ends" False (AInt 32 True), plain "values" True (AInt 64 True)]]
    , [V.singleton (ok (mkRunEndEncoded (prim PInt32 [3, 5, 8]) (fromMaybes PInt64 (V.fromList [Just 100, Nothing, Just 300]))))]
    )
  , ( "list views"
    , schemaOf
        [ nested "lv" False AListView [plain "item" False (AInt 32 True)]
        , nested "llv" False ALargeListView [plain "item" False (AInt 32 True)]
        ]
    , [ V.fromList
          [ ok (mkListView Nothing (VS.fromList [0, 2, 5]) (VS.fromList [2, 3, 1]) (prim PInt32 [10, 20, 30, 40, 50, 60]))
          , ok (mkLargeListView Nothing (VS.fromList [4, 0, 1]) (VS.fromList [2, 1, 3]) (prim PInt32 [1, 2, 3, 4, 5, 6]))
          ]
      ]
    )
  , ( "null"
    , schemaOf [plain "n" False ANull, plain "i" False (AInt 8 True)]
    , [V.fromList [ColNull 4, prim PInt8 [1, 2, 3, 4]]]
    )
  ]
  where
    prim :: VS.Storable a => PrimType a -> [a] -> ColumnArray
    prim t = primColumn t . VS.fromList
    ok = either (\e -> error ("Test.Arrow.Malformed: bad shape: " ++ e)) id


-- * Decoder harness

{- | Generous for the corpus (a decode of a few kilobytes allocates a
few megabytes at most), far below what a hostile length claim used to
request.
-}
allocationLimitBytes :: Int64
allocationLimitBytes = 512 * 1024 * 1024


{- | Evaluate a decoder result to normal form under an allocation limit
and a timeout. 'Nothing' means it behaved; 'Just' describes the crash.
-}
contained :: NFData a => a -> IO (Maybe String)
contained = containedWithin allocationLimitBytes


containedWithin :: NFData a => Int64 -> a -> IO (Maybe String)
containedWithin limit x = do
  r <- timeout 10000000 . try @SomeException $ do
    setAllocationCounter limit
    enableAllocationLimit
    _ <- evaluate (force x)
    disableAllocationLimit
  disableAllocationLimit
  pure $ case r of
    Nothing -> Just "did not finish within 10s"
    Just (Left e) -> Just ("threw " ++ displayException e)
    Just (Right ()) -> Nothing


{- | Allocation allowed for exercising the columns of one accepted
decode. A batch may legitimately claim up to a million rows that no
buffer backs (fieldless structs, null children), and the exercise
allocates per row; the decode itself stays under
'allocationLimitBytes'.
-}
exerciseLimitBytes :: Int64
exerciseLimitBytes = 4 * 1024 * 1024 * 1024


{- | Run a column-producing decoder: force its result under the decoder
limits, then exercise every column it returns ('exerciseColumn'); an
exception there is a crash, and a column the row model rejects is
reported like one.
-}
containedColumns :: NFData a => (a -> [ColumnArray]) -> Either String a -> IO (Maybe String)
containedColumns cols r = do
  crash <- contained r
  case (crash, r) of
    (Just e, _) -> pure (Just e)
    (Nothing, Left _) -> pure Nothing
    (Nothing, Right a) -> do
      let checked = traverse exerciseColumn (cols a)
      exerciseCrash <- containedWithin exerciseLimitBytes checked
      pure $ case (exerciseCrash, checked) of
        (Just e, _) -> Just ("accessor on an accepted column: " ++ e)
        (Nothing, Left e) -> Just ("accepted a column that does not read back: " ++ e)
        _ -> Nothing


{- | Every accessor over a column and, recursively, over its children:
typed element access, byte/text/list/dictionary/bool access, validity,
the O(n) conversions, dictionary expansion, a slice, 'Show' (its first
64 KiB), reflexive 'Eq' and 'copyColumn'. 'Left' when the column does
not read back through the row model or a copy differs from the
original; the 'Int' only exists to force everything.

Rows are walked in bounded windows ('columnWindows'): a run-end-encoded
array names its rows in run ends and an all-null or fieldless column
has nothing to read, so either can legitimately claim billions of rows
for a few hundred input bytes. The decoder must accept those, and no
per-row pass can walk them, so the head, middle and tail are exercised
instead.
-}
exerciseColumn :: ColumnArray -> Either String Int
exerciseColumn c = do
  kids <- traverse exerciseColumn (G.childColumns c)
  windows <- traverse exerciseWindow (columnWindows c)
  Right $!
    sum windows
      + length (take 65536 (show c))
      + nullCount c
      + sum kids


{- | Rows one at a time in one column: how many rows 'columnWindows'
walks one by one when a column is longer than that.
-}
exerciseWindowRows :: Int
exerciseWindowRows = 3 * 4096


{- | The rows 'exerciseColumn' walks: the column itself when it is that
short, otherwise equally sized head, middle and tail slices, so every
accessor still runs against both ends of each buffer and an interior
run.
-}
columnWindows :: ColumnArray -> [ColumnArray]
columnWindows c
  | n <= exerciseWindowRows = [c]
  | otherwise = map (\start -> sliceColumnArray start w c) [0, (n - w) `quot` 2, n - w]
  where
    !n = columnLength c
    !w = exerciseWindowRows `quot` 3


{- | Upper bound on the values the row model materializes for a column:
its own logical length plus every child's cost. Lists and maps name
child ranges without rebasing the child, so a short parent window can
still reference a huge child; run-end columns and nulls shrink with a
row slice. Cheap, O(columns).
-}
columnCost :: ColumnArray -> Integer
columnCost c = toInteger (columnLength c) + sum (map columnCost (G.childColumns c))


{- | Values one window may materialize through the row model. A window is
a few thousand rows, so this leaves headroom for wide nested rows while
keeping one accessor call to a few tens of megabytes.
-}
exerciseValueBudget :: Integer
exerciseValueBudget = 262144


{- | Every accessor over every row of one bounded window. The row model
is only materialized when 'columnCost' says the window is small enough:
a run-end column or a list over one can name billions of values in a few
input bytes, and expanding those would measure the test's memory, not
the decoder's behaviour. The per-row accessors still run over every row
of the window either way.
-}
exerciseWindow :: ColumnArray -> Either String Int
exerciseWindow c =
  if columnCost c > exerciseValueBudget
    then Right $! foldl' (\acc i -> acc + rowScore i) 0 [0 .. n - 1]
    else do
      vs <- G.columnValues c
      expanded <- either (const (Right 0)) (fmap valuesSize . G.columnValues) (expandDictionary c)
      sliced <- valuesSize <$> G.columnValues (sliceColumnArray 1 (n - 2) c)
      if c /= c
        then Left "a decoded column is not equal to itself"
        else
          if copyColumn c /= c
            then Left "copyColumn changed a decoded column"
            else
              Right $!
                foldl' (\acc i -> acc + rowScore i) 0 [0 .. n - 1]
                  + valuesSize vs
                  + conversions
                  + expanded
                  + sliced
  where
    !n = columnLength c
    primScore i = case c of
      ColPrim t v xs -> withPrim t (maybe 0 (const 1) (primAt (PrimArray v xs) i))
      _ -> 0
    rowScore i =
      maybe 0 BS.length (anyBytesAt c i)
        + maybe 0 T.length (anyTextAt c i)
        + maybe 0 (\r -> childStart r + childLength r) (listRange c i)
        + fromMaybe 0 (dictKeyAt c i)
        + maybe 0 fromEnum (boolAt c i)
        + fromEnum (isValidAt (validity c) i)
        + primScore i
    sized :: AV.Element a => (a -> Int) -> Either String (AV.Vector (Maybe a)) -> Int
    sized f = either (const 0) (VG.foldl' (\acc x -> acc + maybe 1 f x) 0)
    conversions =
      sized T.length (toTextVector c)
        + sized BS.length (toBytesVector c)
        + sized fromEnum (toBoolVector c)


-- | Force every row of the model, deeply.
valuesSize :: V.Vector G.Value -> Int
valuesSize = V.foldl' (\acc v -> acc + valueSize v) 0
  where
    valueSize = \case
      G.VNull -> 1
      G.VInt i -> fromIntegral (i `rem` 7)
      G.VF16 w -> fromIntegral w
      G.VF32 w -> fromIntegral w
      G.VF64 w -> fromIntegral (w `rem` 7)
      G.VBool b -> fromEnum b
      G.VText t -> T.length t
      G.VBytes b -> BS.length b
      G.VDecimal i -> fromIntegral (i `rem` 7)
      G.VPair a b -> fromIntegral a + fromIntegral b
      G.VTriple a b x -> fromIntegral a + fromIntegral b + fromIntegral x
      G.VList xs -> foldl' (\acc x -> acc + valueSize x) 1 xs
      G.VStruct kvs -> foldl' (\acc (k, x) -> acc + T.length k + valueSize x) 1 kvs
      G.VMap kvs -> foldl' (\acc (k, x) -> acc + valueSize k + valueSize x) 1 kvs
      G.VUnion t x -> fromIntegral t + valueSize x


-- | Every public entry point that accepts untrusted bytes.
decoders :: [(String, ByteString -> IO (Maybe String))]
decoders =
  [ ("Arrow.Stream.decodeArrowStream", containedColumns (concatMap V.toList . snd) . decodeArrowStream defaultReadOptions)
  , ("Arrow.Stream.decodeArrowFile", containedColumns (concatMap V.toList . snd) . decodeArrowFile defaultReadOptions)
  , ("Arrow.Stream.openStreamReader/streamReaderToList", containedColumns (concatMap V.toList . snd) . viaStreamReader)
  , ("Arrow.Stream.openFileReader/fileReaderToList", containedColumns (concatMap V.toList) . viaFileReader)
  , ("Arrow.FlatBufferIPC.readArrowStreamFBInterleaved", contained . fmap (fmap (map frameNF)) . readArrowStreamFBInterleaved)
  , ("Arrow.FlatBufferIPC.readArrowFileFBWithDicts", contained . fmap (\(s, ds, bs) -> (s, map dictNF ds, bs)) . readArrowFileFBWithDicts)
  , ("Arrow.FlatBufferIPC.decodeTensorFrame", contained . fmap (\(t, rest) -> (show t, rest)) . decodeTensorFrame)
  , ("Arrow.FlatBufferIPC.decodeSparseTensorFrame", contained . fmap (\(t, rest) -> (show t, rest)) . decodeSparseTensorFrame)
  , ("Arrow.File.readArrowStream", contained . fmap (\a -> (File.asSchema a, File.asBatches a)) . File.readArrowStream)
  , ("Arrow.File.readArrowFile", contained . fmap (\a -> (File.afSchema a, File.afBatches a)) . File.readArrowFile)
  , ("Arrow.File.readArrowFileColumns", containedColumns (concatMap V.toList . V.toList . snd) . File.readArrowFileColumns defaultReadOptions)
  , ("Arrow.IPC.decodeIPCMessage", contained . decodeIPCMessage)
  , ("Arrow.FlatBufferIPC.readFileIndex", contained . fmap indexNF . readFileIndex)
  , ("Arrow.File.readIPCMessage@0", \bs -> contained (File.readIPCMessage bs 0))
  , ("Arrow.File.readIPCMessage@8", \bs -> contained (File.readIPCMessage bs 8))
  ]
  where
    viaStreamReader bs = do
      rd <- openStreamReader defaultReadOptions bs
      batches <- streamReaderToList rd
      Right (streamReaderSchema rd, batches)
    viaFileReader bs = do
      rd <- openFileReader defaultReadOptions bs
      batches <- fileReaderToList rd
      Right (V.fromList batches)
    indexNF idx = (show (fiSchema idx), map blockNF (V.toList (fiDictBlocks idx)), map blockNF (V.toList (fiRecordBatches idx)))
    blockNF b = (abOffset b, abMetaLen b, abBodyLen b)
    frameNF = \case
      SFDict db -> Left (dictNF db)
      SFBatch rb body -> Right (rb, body)
    dictNF db = (dbId db, dbIsDelta db, dbData db, dbBody db)


-- | Run every decoder; fail with the decoder name and crash on the first misbehaviour.
noDecoderCrashes :: ByteString -> PropertyT IO ()
noDecoderCrashes bs = mapM_ one decoders
  where
    one (name, run) = do
      r <- evalIO (run bs)
      case r of
        Nothing -> success
        Just err -> do
          annotate (name ++ " " ++ err)
          failure

-- * Byte-level mutations

data Mutation
  = Truncate Int
  | FlipBits [(Int, Int)]
  | Overwrite [(Int, Word8)]
  | Insert Int ByteString
  | Delete Int Int
  | Duplicate Int Int
  deriving stock (Show)


applyMutation :: Mutation -> ByteString -> ByteString
applyMutation m bs = case m of
  Truncate k -> BS.take k bs
  FlipBits ps -> foldl (\acc (i, b) -> modifyAt i (`complementBit` b) acc) bs ps
  Overwrite ps -> foldl (\acc (i, w) -> modifyAt i (const w) acc) bs ps
  Insert i extra -> BS.take i bs <> extra <> BS.drop i bs
  Delete i n -> BS.take i bs <> BS.drop (i + n) bs
  Duplicate i n -> BS.take (i + n) bs <> BS.take n (BS.drop i bs) <> BS.drop (i + n) bs
  where
    modifyAt i f s
      | i < 0 || i >= BS.length s = s
      | otherwise = BS.take i s <> BS.singleton (f (BS.index s i)) <> BS.drop (i + 1) s


genPos :: Int -> Gen Int
genPos len = Gen.int (Range.constant 0 (max 0 (len - 1)))


-- | Byte values that tend to sit on decoder boundaries.
genByte :: Gen Word8
genByte = Gen.frequency [(3, Gen.element [0x00, 0x01, 0x07, 0x08, 0x0C, 0x7F, 0x80, 0xFE, 0xFF]), (2, Gen.word8 Range.constantBounded)]


genSeed :: [Seed] -> Gen Seed
genSeed = Gen.element


mutationProperty :: CoverPercentage -> [Seed] -> (Int -> Gen Mutation) -> Property
mutationProperty acceptPct corpus genM = withTests 3000 . property $ do
  seed <- forAll (genSeed corpus)
  m <- forAll (genM (BS.length (seedBytes seed)))
  let mutated = applyMutation m (seedBytes seed)
  noDecoderCrashes mutated
  coverDepth acceptPct mutated


-- Cutting a frame short is always rejected, so truncations only rarely
-- (at frame boundaries) leave something to accept.
prop_truncation :: [Seed] -> Property
prop_truncation corpus = mutationProperty 0 corpus $ \len -> Truncate <$> genPos len


prop_bitFlips :: [Seed] -> Property
prop_bitFlips corpus = mutationProperty 5 corpus $ \len ->
  FlipBits <$> Gen.list (Range.linear 1 8) ((,) <$> genPos len <*> Gen.int (Range.constant 0 7))


prop_overwrites :: [Seed] -> Property
prop_overwrites corpus = mutationProperty 5 corpus $ \len ->
  Overwrite <$> Gen.list (Range.linear 1 8) ((,) <$> genPos len <*> genByte)


prop_splices :: [Seed] -> Property
prop_splices corpus = mutationProperty 5 corpus $ \len ->
  Gen.choice
    [ Insert <$> genPos len <*> Gen.bytes (Range.linear 1 64)
    , Delete <$> genPos len <*> Gen.int (Range.linear 1 64)
    , Duplicate <$> genPos len <*> Gen.int (Range.linear 1 64)
    ]


-- * Structured mutations: record batch descriptors

data DescTarget
  = BufOffset
  | BufLength
  | NodeLength
  | NodeNullCount
  | VariadicCount
  | BatchLength
  | ClaimedBodyLength
  deriving stock (Show, Eq, Enum, Bounded)


data Value
  = Exactly Int64
  | Shift Int64
  | BodyPlus Int64
  deriving stock (Show)


data DescMutation = DescMutation
  { dmFrame :: Int
  , dmTarget :: DescTarget
  , dmIndex :: Int
  , dmValue :: Value
  }
  deriving stock (Show)


genValue :: Gen Value
genValue =
  Gen.choice
    [ Exactly <$> Gen.element [0, 1, -1, 7, 8, 13, 255, 2 ^ (31 :: Int) - 1, 2 ^ (31 :: Int), 2 ^ (32 :: Int), 2 ^ (40 :: Int), 2 ^ (62 :: Int), maxBound, minBound]
    , Shift <$> Gen.int64 (Range.linearFrom 0 (-64) 64)
    , BodyPlus <$> Gen.int64 (Range.linearFrom 0 (-16) 16)
    , Exactly <$> Gen.int64 Range.linearBounded
    ]


resolveValue :: Value -> Int64 -> Int64 -> Int64
resolveValue v original bodyLen = case v of
  Exactly x -> x
  Shift d -> original + d
  BodyPlus d -> bodyLen + d


-- | A decoded stream whose frames can be edited and re-serialized.
data Parsed = Parsed
  { pSchema :: Schema
  , pFrames :: [StreamFrame]
  }


parseSeed :: Seed -> Maybe Parsed
parseSeed s = case seedEncoding s of
  StreamEnc -> either (const Nothing) (\(sch, frames) -> Just (Parsed sch frames)) (readArrowStreamFBInterleaved (seedBytes s))
  FileEnc ->
    either
      (const Nothing)
      (\(sch, dicts, batches) -> Just (Parsed sch (map SFDict dicts ++ map (uncurry SFBatch) batches)))
      (readArrowFileFBWithDicts (seedBytes s))


-- | Re-serialize; @claimed@ overrides record batch body lengths in the metadata.
serialize :: Encoding -> Schema -> [(StreamFrame, Maybe Int64)] -> ByteString
serialize enc sch frames = case enc of
  StreamEnc ->
    BS.concat (encapsulateMessage (buildSchemaMessage sch) BS.empty : map frameBytes frames ++ [eos])
  FileEnc ->
    writeArrowFileFBWithDicts
      sch
      (mapMaybe (\(f, _) -> case f of SFDict db -> Just db; SFBatch {} -> Nothing) frames)
      (mapMaybe (\(f, _) -> case f of SFBatch rb body -> Just (rb, body); SFDict {} -> Nothing) frames)
  where
    eos = BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0]
    frameBytes = \case
      (SFDict db, _) -> encapsulateMessage (buildDictionaryBatchMessage db) (dbBody db)
      (SFBatch rb body, claimed) ->
        encapsulateMessage (buildRecordBatchMessage rb (maybe (fromIntegral (BS.length body)) id claimed)) body


applyDesc :: DescMutation -> [(StreamFrame, Maybe Int64)] -> [(StreamFrame, Maybe Int64)]
applyDesc dm frames = zipWith edit [0 ..] frames
  where
    nFrames = length frames
    target = if nFrames == 0 then -1 else dmFrame dm `mod` nFrames
    edit :: Int -> (StreamFrame, Maybe Int64) -> (StreamFrame, Maybe Int64)
    edit i fr@(frame, claimed)
      | i /= target = fr
      | otherwise = case frame of
          SFBatch rb body ->
            let bodyLen = fromIntegral (BS.length body)
            in case dmTarget dm of
                 ClaimedBodyLength -> (frame, Just (resolveValue (dmValue dm) (maybe bodyLen id claimed) bodyLen))
                 _ -> (SFBatch (editRb bodyLen rb) body, claimed)
          SFDict db ->
            let bodyLen = fromIntegral (BS.length (dbBody db))
            in (SFDict db {dbData = editRb bodyLen (dbData db)}, claimed)
    editRb bodyLen rb =
      let v orig = resolveValue (dmValue dm) orig bodyLen
          pick :: Int -> Int
          pick len = dmIndex dm `mod` len
          atIdx :: V.Vector a -> (a -> a) -> V.Vector a
          atIdx xs f
            | V.null xs = xs
            | otherwise = let k = pick (V.length xs) in xs V.// [(k, f (xs V.! k))]
          atIdxS :: VS.Storable a => VS.Vector a -> (a -> a) -> VS.Vector a
          atIdxS xs f
            | VS.null xs = xs
            | otherwise = let k = pick (VS.length xs) in xs VS.// [(k, f (xs VS.! k))]
      in case dmTarget dm of
           BufOffset -> rb {rbBuffers = atIdxS (rbBuffers rb) (\b -> b {bufOffset = v (bufOffset b)})}
           BufLength -> rb {rbBuffers = atIdxS (rbBuffers rb) (\b -> b {bufLength = v (bufLength b)})}
           NodeLength -> rb {rbNodes = atIdxS (rbNodes rb) (\n -> n {fnLength = v (fnLength n)})}
           NodeNullCount -> rb {rbNodes = atIdxS (rbNodes rb) (\n -> n {fnNullCount = v (fnNullCount n)})}
           VariadicCount -> rb {rbVariadicBufferCounts = atIdx (rbVariadicBufferCounts rb) v}
           BatchLength -> rb {rbLength = v (rbLength rb)}
           ClaimedBodyLength -> rb


-- | Seeds whose frames the structured mutators can edit.
parsedSeeds :: [Seed] -> [(Seed, Parsed)]
parsedSeeds = mapMaybe (\s -> (,) s <$> parseSeed s)


prop_descriptors :: [Seed] -> Property
prop_descriptors corpus = withTests 3000 . property $ do
  (seed, parsed) <- forAllWith (show . fst) (Gen.element (parsedSeeds corpus))
  muts <-
    forAll . Gen.list (Range.linear 1 3) $
      DescMutation
        <$> Gen.int (Range.constant 0 7)
        <*> Gen.enumBounded
        <*> Gen.int (Range.constant 0 31)
        <*> genValue
  let frames = foldr applyDesc (map (\f -> (f, Nothing)) (pFrames parsed)) muts
  let bytes = serialize (seedEncoding seed) (pSchema parsed) frames
  noDecoderCrashes bytes
  coverDepth 5 bytes


-- * Structured mutations: schema / type confusion

data SchemaMutation
  = SetType [Int] ArrowType
  | ToggleNullable [Int]
  | DropChildren [Int]
  | AddChild [Int] ArrowType
  | ToggleDictionary [Int] ArrowType
  deriving stock (Show)


confusingTypes :: [ArrowType]
confusingTypes =
  [ ANull
  , AInt 8 True
  , AInt 16 False
  , AInt 32 True
  , AInt 64 False
  , AInt 7 True
  , ABool
  , AUtf8
  , ABinary
  , ALargeUtf8
  , ALargeBinary
  , AFixedSizeBinary 0
  , AFixedSizeBinary 3
  , AFixedSizeBinary (-1)
  , ADecimal 10 2
  , ADecimal256 40 2
  , ADate DateDay
  , ATime Second 32
  , ATime Nanosecond 64
  , ATimestamp Nanosecond Nothing
  , ADuration Second
  , AInterval YearMonth
  , AInterval DayTime
  , AInterval MonthDayNano
  , AFloatingPoint Half
  , AFloatingPoint DoublePrecision
  , AList
  , ALargeList
  , AStruct
  , AFixedSizeList 0
  , AFixedSizeList 2
  , AFixedSizeList (-3)
  , AMap False
  , AUnion Dense (V.fromList [0, 1])
  , AUnion Sparse V.empty
  , AUnion Dense (V.fromList [5, 127])
  , ARunEndEncoded
  , AUtf8View
  , ABinaryView
  , AListView
  , ALargeListView
  ]


-- | Preorder paths of every field in a schema.
fieldPaths :: V.Vector Field -> [[Int]]
fieldPaths fs = concat (zipWith (\i f -> [i] : map (i :) (fieldPaths (fieldChildren f))) [0 ..] (V.toList fs))


editField :: [Int] -> (Field -> Field) -> V.Vector Field -> V.Vector Field
editField [] _ fs = fs
editField (i : rest) f fs = case fs V.!? i of
  Nothing -> fs
  Just fld ->
    let fld' = if null rest then f fld else fld {fieldChildren = editField rest f (fieldChildren fld)}
    in fs V.// [(i, fld')]


applySchemaMutation :: SchemaMutation -> Schema -> Schema
applySchemaMutation m sch = sch {arrowFields = editField path edit (arrowFields sch)}
  where
    (path, edit) = case m of
      SetType p ty -> (p, \f -> f {fieldType = ty})
      ToggleNullable p -> (p, \f -> f {fieldNullable = not (fieldNullable f)})
      DropChildren p -> (p, \f -> f {fieldChildren = V.empty})
      AddChild p ty -> (p, \f -> f {fieldChildren = V.snoc (fieldChildren f) (plain "extra" True ty)})
      ToggleDictionary p ty -> (p, \f -> f {fieldDictionary = maybe (Just (DictionaryEncoding 0 ty False)) (const Nothing) (fieldDictionary f)})


genSchemaMutation :: Schema -> Gen SchemaMutation
genSchemaMutation sch = do
  path <- Gen.element (case fieldPaths (arrowFields sch) of [] -> [[0]]; ps -> ps)
  ty <- Gen.element confusingTypes
  Gen.element [SetType path ty, ToggleNullable path, DropChildren path, AddChild path ty, ToggleDictionary path ty]


prop_schemaConfusion :: [Seed] -> Property
prop_schemaConfusion corpus = withTests 3000 . property $ do
  (seed, parsed) <- forAllWith (show . fst) (Gen.element (parsedSeeds corpus))
  muts <- forAll (Gen.list (Range.linear 1 3) (genSchemaMutation (pSchema parsed)))
  let sch = foldr applySchemaMutation (pSchema parsed) muts
  let bytes = serialize (seedEncoding seed) sch (map (\f -> (f, Nothing)) (pFrames parsed))
  noDecoderCrashes bytes
  coverDepth 5 bytes


{- | Both outcomes must be common: mutations that every decoder rejects
up front never reach the column materializers, and mutations that
never fail exercise nothing hostile.
-}
coverDepth :: CoverPercentage -> ByteString -> PropertyT IO ()
coverDepth acceptPct bs = do
  let accepted = case (decodeArrowStream defaultReadOptions bs, decodeArrowFile defaultReadOptions bs) of
        (Left _, Left _) -> False
        _ -> True
  cover acceptPct "a decoder accepted the mutated input" accepted
  cover 5 "every decoder rejected the mutated input" (not accepted)


-- * Random bytes

prop_randomBytes :: Property
prop_randomBytes = withTests 3000 . property $ do
  bs <- forAll (Gen.bytes (Range.linear 0 512))
  noDecoderCrashes bs


-- | Random payloads behind a valid continuation marker or file magic, so
-- they reach the metadata and footer parsers instead of failing at byte 0.
prop_framedRandom :: Property
prop_framedRandom = withTests 2000 . property $ do
  payload <- forAll (Gen.bytes (Range.linear 0 512))
  metaLen <- forAll (Gen.int32 (Range.linearFrom 0 (-8) 600))
  framing <- forAll (Gen.element [ContinuationFrame, FileMagic])
  noDecoderCrashes $ case framing of
    ContinuationFrame -> BS.pack [0xFF, 0xFF, 0xFF, 0xFF] <> le32 metaLen <> payload
    FileMagic -> "ARROW1\0\0" <> payload <> le32 metaLen <> "ARROW1"


data Framing = ContinuationFrame | FileMagic
  deriving stock (Show, Eq)


le32 :: Int32 -> ByteString
le32 n = BS.pack (map (\s -> fromIntegral (fromIntegral n `shiftR` s :: Word32)) [0, 8, 16, 24])


le64 :: Int64 -> ByteString
le64 n = BS.pack (map (\s -> fromIntegral (fromIntegral n `shiftR` s :: Word64)) [0, 8 .. 56])


-- * Hand-built hostile metadata

{- | A schema message whose struct fields each list the same child
twice, @depth@ levels deep: a few hundred bytes describing @2^depth@
fields once offsets are followed naively.
-}
aliasedSchemaStream :: Int -> IO ByteString
aliasedSchemaStream depth = do
  b <- FB.newBuilder
  structTy <- FB.writeTable b []
  let fieldTable kids =
        FB.writeTable
          b
          [ Nothing
          , Just (FB.scalar 1 (\bb -> FB.prependU8 bb 1))
          , Just (FB.scalar 1 (\bb -> FB.prependU8 bb 13))
          , Just (FB.voff structTy)
          , Nothing
          , FB.voff <$> kids
          ]
      grow 0 t = pure t
      grow k t = do
        kids <- FB.writeVectorOfOffsets b [t, t]
        t' <- fieldTable (Just kids)
        grow (k - 1 :: Int) t'
  leaf <- fieldTable Nothing
  top <- grow depth leaf
  fields <- FB.writeVectorOfOffsets b [top]
  sch <- FB.writeTable b [Nothing, Just (FB.voff fields)]
  msg <-
    FB.writeTable
      b
      [ Just (FB.scalar 2 (\bb -> FB.prependI16 bb 4))
      , Just (FB.scalar 1 (\bb -> FB.prependU8 bb 1))
      , Just (FB.voff sch)
      , Just (FB.scalar 8 (\bb -> FB.prependI64 bb 0))
      ]
  meta <- FB.finish b msg
  pure (encapsulateMessage meta BS.empty <> BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0])


-- | Every decoder survives the aliased schema and rejects it.
rejectsEverywhere :: ByteString -> PropertyT IO ()
rejectsEverywhere bs = do
  noDecoderCrashes bs
  case decodeArrowStream defaultReadOptions bs of
    Left _ -> success
    Right _ -> do
      annotate "decodeArrowStream accepted a hostile schema"
      failure


prop_dagBomb :: Property
prop_dagBomb = withTests 1 . property $ evalIO (aliasedSchemaStream 40) >>= rejectsEverywhere


prop_deepSchema :: Property
prop_deepSchema = withTests 1 . property $ do
  let deep = foldr (\_ f -> nested "s" False AStruct [f]) (plain "leaf" False (AInt 32 True)) [1 .. 100 :: Int]
  rejectsEverywhere (encapsulateMessage (buildSchemaMessage (schemaOf [deep])) BS.empty)


{- | A nullable struct over a null-typed child claiming @2^30@ rows with
no body: every row it would materialize is backed by nothing.
-}
prop_unbackedRows :: Property
prop_unbackedRows = withTests 1 . property $ do
  let sch = schemaOf [nested "s" True AStruct [plain "n" True ANull]]
      rows = 2 ^ (30 :: Int)
      rb = RecordBatchDef rows (VS.fromList [FieldNode rows 0, FieldNode rows rows]) (VS.singleton (Buffer 0 0)) V.empty Nothing
  rejectsEverywhere (serialize StreamEnc sch [(SFBatch rb BS.empty, Nothing)])


{- | The SIMD buffer validator: an offset and length that each fit but
whose sum overflows 'Int64' must be rejected, and offsets or lengths
between 2 and 4 GiB inside a large enough body must be accepted.
-}
prop_bufferValidator :: Property
prop_bufferValidator = withTests 1 . property $ do
  let batch bufs = RecordBatchDef 0 VS.empty (VS.fromList bufs) V.empty Nothing
      huge = 2 ^ (62 :: Int)
      threeGiB = 3 * 2 ^ (30 :: Int)
  -- One pair takes the scalar tail, two pairs the SIMD path.
  validateRecordBatchBuffers (batch [Buffer huge huge]) 1024 === False
  validateRecordBatchBuffers (batch [Buffer 0 8, Buffer huge huge]) 1024 === False
  validateRecordBatchBuffers (batch [Buffer threeGiB 8]) (threeGiB + 64) === True
  validateRecordBatchBuffers (batch [Buffer 0 8, Buffer threeGiB 8]) (threeGiB + 64) === True
  validateRecordBatchBuffers (batch [Buffer 0 threeGiB, Buffer threeGiB 8]) (threeGiB + 64) === True


{- | The body-compression codecs compiled into this build (cabal flags
@zstd@ / @lz4@). The compression properties quantify over these only,
so a build without the flags skips them instead of failing.
-}
availableCodecs :: [BodyCompressionCodec]
availableCodecs = filter bodyCompressionAvailable [BodyZstd, LZ4Frame]


{- | One non-null int64 column of 8 rows whose data buffer is a
compressed envelope claiming @claim@ uncompressed bytes; the payload
really holds 64 bytes of @0x07@.
-}
compressedClaim :: BodyCompressionCodec -> Int64 -> ByteString
compressedClaim codec claim =
  let payload = either (const BS.empty) id (compressBufferEither codec (BS.replicate 64 7))
      env = le64 claim <> payload
      envLen = fromIntegral (BS.length env)
      rb = RecordBatchDef 8 (VS.singleton (FieldNode 8 0)) (VS.fromList [Buffer 0 0, Buffer 0 envLen]) V.empty (Just codec)
  in serialize StreamEnc (schemaOf [plain "x" False (AInt 64 True)]) [(SFBatch rb env, Nothing)]


{- | A buffer's uncompressed-length header is a claim: one the payload
cannot expand to, or that disagrees with what it does expand to, is
rejected without inflating anything. The honest claim decodes.
-}
prop_decompressionClaims :: Property
prop_decompressionClaims = withTests 1 . property $ do
  mapM_ (\(codec, claim) -> rejectsEverywhere (compressedClaim codec claim)) $
    concatMap (\codec -> map ((,) codec) [2 ^ (40 :: Int), maxBound, 600, 65, 63, -2]) availableCodecs
  mapM_
    (\codec -> fmap snd (decodeArrowStream defaultReadOptions (compressedClaim codec 64)) === Right [V.singleton (primColumn PInt64 (VS.replicate 8 0x0707070707070707))])
    availableCodecs


{- | Two non-null int64 columns of 8 rows whose data buffers are
compressed envelopes honestly claiming @64@ uncompressed bytes each;
the payloads really hold 64 bytes of @0x07@.
-}
compressedTwo :: BodyCompressionCodec -> ByteString
compressedTwo codec =
  let payload = either (const BS.empty) id (compressBufferEither codec (BS.replicate 64 7))
      env = le64 64 <> payload
      envLen = fromIntegral (BS.length env)
      body = env <> env
      rb =
        RecordBatchDef
          8
          (VS.fromList [FieldNode 8 0, FieldNode 8 0])
          (VS.fromList [Buffer 0 0, Buffer 0 envLen, Buffer envLen 0, Buffer envLen envLen])
          V.empty
          (Just codec)
  in serialize StreamEnc (schemaOf [plain "x" False (AInt 64 True), plain "y" False (AInt 64 True)]) [(SFBatch rb body, Nothing)]


{- | Every 'ReadOptions' budget rejects a batch that claims past it and
accepts the same batch at its exact boundary: the declared row count,
the total declared values across the field nodes, the rows with no
buffer backing (a zero-width fixed-size binary names them all), and
the total uncompressed size a compressed body expands to.
-}
prop_readerBudgets :: Property
prop_readerBudgets = withTests 1 . property $ do
  let sch = schemaOf [plain "z" False (AFixedSizeBinary 0)]
      rows = 600 :: Int64
      rb = RecordBatchDef rows (VS.singleton (FieldNode rows 0)) (VS.fromList [Buffer 0 0, Buffer 0 0]) V.empty Nothing
      bytes = serialize StreamEnc sch [(SFBatch rb BS.empty, Nothing)]
      mustAccept what opts = case decodeArrowStream opts bytes of
        Left e -> annotate (what ++ " at its boundary was rejected: " ++ e) >> failure
        Right _ -> success
      mustReject what opts = case decodeArrowStream opts bytes of
        Left _ -> success
        Right _ -> annotate (what ++ " over budget was accepted") >> failure
      mustAcceptBytes what bs opts = case decodeArrowStream opts bs of
        Left e -> annotate (what ++ " at its boundary was rejected: " ++ e) >> failure
        Right _ -> success
      mustRejectBytes what bs opts = case decodeArrowStream opts bs of
        Left _ -> success
        Right _ -> annotate (what ++ " over budget was accepted") >> failure
  mustAccept "rows" defaultReadOptions
  mustAccept "rows" defaultReadOptions {roMaxRows = rows}
  mustReject "rows" defaultReadOptions {roMaxRows = rows - 1}
  mustAccept "values" defaultReadOptions {roMaxValues = rows}
  mustReject "values" defaultReadOptions {roMaxValues = rows - 1}
  mustAccept "unbacked rows" defaultReadOptions {roMaxUnbackedRows = rows}
  mustReject "unbacked rows" defaultReadOptions {roMaxUnbackedRows = rows - 1}
  mapM_
    ( \codec -> do
        let compressed = compressedTwo codec
        mustAcceptBytes "decompression" compressed defaultReadOptions
        mustAcceptBytes "decompression" compressed defaultReadOptions {roMaxDecompressedBytes = 2 * 64}
        mustRejectBytes "decompression" compressed defaultReadOptions {roMaxDecompressedBytes = 2 * 64 - 1}
    )
    availableCodecs


-- * Sanity: the corpus itself is valid

prop_corpusDecodes :: [Seed] -> Property
prop_corpusDecodes corpus = withTests 1 . property $ mapM_ checkSeed corpus
  where
    checkSeed seed = do
      let decoded = case seedEncoding seed of
            StreamEnc -> () <$ decodeArrowStream defaultReadOptions (seedBytes seed)
            FileEnc -> () <$ decodeArrowFile defaultReadOptions (seedBytes seed)
      case decoded of
        Right () -> noDecoderCrashes (seedBytes seed)
        Left e -> do
          annotate (show seed ++ " does not decode: " ++ e)
          failure


-- * Zero-copy decode under mutation

{- | Generated tables (every type, nested, sliced and denormalised
inputs) written by the stream and file writers, placed in an aligned
buffer so the decoders alias it, then mutated in place by bit flips and
byte overwrites. Every decoder must behave, and every accessor over
every row of an accepted column must neither throw nor read out of
range ('exerciseColumn').
-}
prop_zeroCopyAccessorFuzz :: Property
prop_zeroCopyAccessorFuzz = withTests 1000 . property $ do
  (sch, batches) <- forAll G.genTable
  enc <- forAll (Gen.element [StreamEnc, FileEnc])
  bytes <- evalEither $ case enc of
    StreamEnc -> encodeArrowStream defaultWriteOptions sch batches
    FileEnc -> encodeArrowFile defaultWriteOptions sch batches
  let len = BS.length bytes
  m <-
    forAll $
      Gen.choice
        [ FlipBits <$> Gen.list (Range.linear 1 8) ((,) <$> genPos len <*> Gen.int (Range.constant 0 7))
        , Overwrite <$> Gen.list (Range.linear 1 8) ((,) <$> genPos len <*> genByte)
        ]
  let mutated = alignedCopy (applyMutation m bytes)
  noDecoderCrashes mutated
  coverDepth 5 mutated


-- | The bytes in a fresh 64-byte aligned buffer (so decoders alias them).
alignedCopy :: ByteString -> ByteString
alignedCopy bs =
  I.createAligned (BS.length bs) $ \p ->
    BSU.unsafeUseAsCStringLen bs $ \(src, n) -> copyBytes p (castPtr src) n
