{-# LANGUAGE OverloadedStrings #-}

{- | Section-at-a-time reading of Arrow IPC containers: the footer
'FileIndex', the pure 'Arrow.Stream.FileReader', and the
semi-streaming 'Handle' iterators that decode one section live at a
time.

The parity tests compare every sectioning path against the eager
decoders over the same bytes; the corruption test proves the handle
iterator really works section by section (earlier batches arrive
before a broken later section is reported).
-}
module Test.Arrow.Sections (tests) where

import Arrow.Column (ColumnArray, PrimType (..), primColumn)
import Arrow.Column qualified as C
import Arrow.File (readIPCMessage)
import Arrow.FlatBufferIPC (ArrowBlock (..), FileIndex (..), readFileIndex)
import Arrow.FlatBufferIPC qualified as IPC
import Arrow.Stream (
  ReadOptions,
  decodeArrowFile,
  decodeArrowStream,
  defaultReadOptions,
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowStream,
  fileBatchesHandle,
  fileReaderSchema,
  fileReaderToList,
  openFileReader,
  openStreamReader,
  streamBatchesHandle,
  streamReaderToList,
 )
import Arrow.Types (ArrowType (..), Schema, defaultLeafField, defaultSchema)
import Arrow.Types qualified as A
import Columnar.Stream (IterIO, IterIOStep (..), iterIOStep, iterIOToList)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import FlatBuffers.Reader qualified as FB
import Hedgehog
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, SeekMode (..), hClose, hSeek, openBinaryTempFile)


tests :: IO Bool
tests =
  checkParallel $
    Group
      "Arrow.Sections"
      [ ("file sections decode to the eager batches", prop_fileSectionsMatchEager)
      , ("pyarrow file sections decode to the eager batches", prop_pyarrowFileSections)
      , ("handle iteration decodes one file section at a time", prop_fileBatchesHandle)
      , ("handle iteration decodes one stream frame at a time (dictionaries included)", prop_streamBatchesHandle)
      , ("a corrupt section fails only when its turn comes", prop_corruptSectionFailsLate)
      , ("Rust 64-byte aligned files use their footer index", prop_rustAlignedFile)
      , ("Rust empty 64-byte aligned files keep their schema", prop_rustEmptyFile)
      , ("footer blocks cannot address footer bytes", prop_blockCannotPointIntoFooter)
      , ("negative message body lengths fail every file reader", prop_negativeMessageBody)
      , ("PyArrow V4 and V5 unions decode through every container reader", prop_pyarrowUnionVersions)
      , ("unknown metadata versions fail in schemas, footers and batches", prop_unknownMetadataVersion)
      , ("V4 unions with top-level nulls fail explicitly", prop_legacyUnionNulls)
      ]


prop_rustAlignedFile :: Property
prop_rustAlignedFile = withTests 1 $ property $ do
  bytes <- evalIO (BS.readFile "test/golden/rs_file_alignment64.arrow")
  -- The old eager reader mistook this alignment padding for stream EOS.
  BS.take 4 (BS.drop 8 bytes) === BS.replicate 4 0
  let expected = [V.singleton (C.fromMaybes PInt32 (V.fromList [Just 1, Nothing, Just (-7), Just maxBound]))]
  (schema, batches) <- evalEither (decodeArrowFile defaultReadOptions bytes)
  V.map A.fieldName (A.arrowFields schema) === V.singleton "value"
  V.map A.fieldType (A.arrowFields schema) === V.singleton (AInt 32 True)
  batches === expected
  reader <- evalEither (openFileReader defaultReadOptions bytes)
  fileReaderSchema reader === schema
  streamed <- evalEither (fileReaderToList reader)
  streamed === expected


prop_rustEmptyFile :: Property
prop_rustEmptyFile = withTests 1 $ property $ do
  bytes <- evalIO (BS.readFile "test/golden/rs_file_empty_alignment64.arrow")
  (schema, batches) <- evalEither (decodeArrowFile defaultReadOptions bytes)
  V.map A.fieldName (A.arrowFields schema) === V.singleton "value"
  V.map A.fieldType (A.arrowFields schema) === V.singleton (AInt 32 True)
  batches === []
  reader <- evalEither (openFileReader defaultReadOptions bytes)
  fileReaderSchema reader === schema
  streamed <- evalEither (fileReaderToList reader)
  streamed === []


prop_blockCannotPointIntoFooter :: Property
prop_blockCannotPointIntoFooter = withTests 1 $ property $ do
  bytes <- evalIO (BS.readFile "test/golden/rs_file_alignment64.arrow")
  footerLength <- evalEither (FB.peekI32 bytes (BS.length bytes - 10))
  let footerStart = BS.length bytes - 10 - fromIntegral footerLength
      footer = BS.take (fromIntegral footerLength) (BS.drop footerStart bytes)
  root <- evalEither (FB.peekU32 footer 0)
  slots <- evalEither (FB.resolveTable footer (fromIntegral root))
  slot <- evalMaybe (slots 3)
  vector <- evalEither (FB.followUOffset footer slot)
  (_, positions) <- evalEither (FB.readVectorOfStructs footer vector 24)
  position <- evalMaybe (positions V.!? 0)
  let patchPosition = footerStart + position
      -- A tiny block fits within the whole file but lies in its footer.
      encoded = BL.toStrict (BB.toLazyByteString (BB.int64LE (fromIntegral footerStart :: Int64) <> BB.int32LE 8 <> BB.int32LE 0 <> BB.int64LE 0))
      corrupted = BS.take patchPosition bytes <> encoded <> BS.drop (patchPosition + 24) bytes
  case readFileIndex corrupted of
    Left _ -> success
    Right _ -> failure


prop_negativeMessageBody :: Property
prop_negativeMessageBody = withTests 1 $ property $ do
  bytes <- evalIO (BS.readFile "test/golden/rs_file_alignment64.arrow")
  index <- evalEither (readFileIndex bytes)
  descriptor <- evalMaybe (fiRecordBatches index V.!? 0)
  let metadataStart = fromIntegral (abOffset descriptor) + 8
      metadata = BS.take (fromIntegral (abMetaLen descriptor) - 8) (BS.drop metadataStart bytes)
  root <- evalEither (FB.peekU32 metadata 0)
  slots <- evalEither (FB.resolveTable metadata (fromIntegral root))
  bodyLengthSlot <- evalMaybe (slots 3)
  let position = metadataStart + bodyLengthSlot
      encoded = BL.toStrict (BB.toLazyByteString (BB.int64LE (-1)))
      corrupted = BS.take position bytes <> encoded <> BS.drop (position + 8) bytes
      rejected result = case result of Left _ -> True; Right _ -> False
  assert (rejected (decodeArrowFile defaultReadOptions corrupted))
  reader <- evalEither (openFileReader defaultReadOptions corrupted)
  assert (rejected (fileReaderToList reader))
  handled <- evalIO (withTempFile corrupted (drainFile defaultReadOptions))
  assert (rejected handled)

prop_pyarrowUnionVersions :: Property
prop_pyarrowUnionVersions = withTests 1 $ property $ do
  let ids = VS.fromList [0, 1, 0]
  dense <-
    evalEither $
      C.mkDenseUnion ids (VS.fromList [0, 0, 1]) $
        V.fromList [C.fromMaybes PInt32 (V.fromList [Just 21, Nothing]), C.fromTexts (V.singleton "text")]
  sparse <-
    evalEither $
      C.mkSparseUnion ids $
        V.fromList
          [ C.fromMaybes PInt32 (V.fromList [Just 21, Nothing, Nothing])
          , C.fromMaybeTexts (V.fromList [Nothing, Just "text", Nothing])
          ]
  dict <- evalEither (C.mkDictionary 0 (primColumn PInt8 (VS.fromList [1, 0, 2])) dense)
  let expected = [V.fromList [dense, sparse, dict, primColumn PInt32 (VS.fromList [11, 22, 33])]]
  forM_ ["v4", "v5"] $ \version -> do
    annotate version
    let base = "test/golden/pa_union_" ++ version
        opts = defaultReadOptions
    stream <- evalIO (BS.readFile (base ++ ".arrows"))
    file <- evalIO (BS.readFile (base ++ ".arrow"))
    (streamSchema, streamBatches) <- evalEither (decodeArrowStream opts stream)
    (fileSchema, fileBatches) <- evalEither (decodeArrowFile opts file)
    fileSchema === streamSchema
    streamBatches === expected
    fileBatches === expected
    streamReader <- evalEither (openStreamReader opts stream)
    evalEither (streamReaderToList streamReader) >>= (=== expected)
    fileReader <- evalEither (openFileReader opts file)
    evalEither (fileReaderToList fileReader) >>= (=== expected)
    evalIO (withTempFile stream (drainStream opts)) >>= (=== Right expected)
    evalIO (withTempFile file (drainFile opts)) >>= (=== Right expected)
    -- Reframing raw batches must preserve the version that describes their buffers.
    (rawSchema, dicts, batches) <- evalEither (IPC.readArrowStreamFBWithDicts stream)
    let reframed = IPC.writeArrowStreamFBWithDicts rawSchema dicts batches
    evalEither (decodeArrowStream opts reframed) >>= (=== (streamSchema, expected))


prop_unknownMetadataVersion :: Property
prop_unknownMetadataVersion = withTests 1 $ property $ do
  stream <- evalIO (BS.readFile "test/golden/pa_union_v5.arrows")
  file <- evalIO (BS.readFile "test/golden/pa_union_v5.arrow")
  (_, _, dictOffset) <- evalEither (readIPCMessage stream 0)
  (_, _, batchOffset) <- evalEither (readIPCMessage stream dictOffset)
  index <- evalEither (readFileIndex file)
  dictBlock <- evalMaybe (fiDictBlocks index V.!? 0)
  batchBlock <- evalMaybe (fiRecordBatches index V.!? 0)
  footerLength <- evalEither (FB.peekI32 file (BS.length file - 10))
  let opts = defaultReadOptions
      footerOffset = BS.length file - 10 - fromIntegral footerLength
      rejects result = case result of
        Left err -> "unsupported metadata version 99" `isInfixOf` err
        Right _ -> False
  forM_ [0, dictOffset, batchOffset] $ \offset -> do
    corrupted <- evalEither (unknownFrameVersion stream offset)
    assert (rejects (decodeArrowStream opts corrupted))
    assert (rejects (openStreamReader opts corrupted >>= streamReaderToList))
    evalIO (withTempFile corrupted (drainStream opts)) >>= assert . rejects
  badDict <- evalEither (unknownFrameVersion file (fromIntegral (abOffset dictBlock)))
  badBatch <- evalEither (unknownFrameVersion file (fromIntegral (abOffset batchBlock)))
  badFooter <- evalEither (unknownVersion file footerOffset)
  forM_ [badDict, badBatch, badFooter] $ \corrupted -> do
    assert (rejects (decodeArrowFile opts corrupted))
    assert (rejects (openFileReader opts corrupted >>= fileReaderToList))
    evalIO (withTempFile corrupted (drainFile opts)) >>= assert . rejects


unknownFrameVersion :: ByteString -> Int -> Either String ByteString
unknownFrameVersion bytes offset = do
  (_, meta, rest) <- IPC.readFrameHeader (BS.drop offset bytes)
  unknownVersion bytes (BS.length bytes - BS.length rest - BS.length meta)


unknownVersion :: ByteString -> Int -> Either String ByteString
unknownVersion bytes offset = do
  let meta = BS.drop offset bytes
  root <- FB.peekU32 meta 0
  slots <- FB.resolveTable meta (fromIntegral root)
  slot <- maybe (Left "test fixture has no metadata version") Right (slots 0)
  let position = offset + slot
  Right (BS.take position bytes <> BS.pack [99, 0] <> BS.drop (position + 2) bytes)


prop_legacyUnionNulls :: Property
prop_legacyUnionNulls = withTests 1 $ property $ do
  bytes <- evalIO (BS.readFile "test/golden/pa_union_v4.arrows")
  (schema, _, batches) <- evalEither (IPC.readArrowStreamFBWithDicts bytes)
  (batch, body) <- evalMaybe (case batches of [one] -> Just one; _ -> Nothing)
  first <- evalMaybe (A.rbNodes batch VS.!? 0)
  let malformed = batch {A.rbNodes = A.rbNodes batch VS.// [(0, first {A.fnNullCount = 1})]}
  case IPC.decodeRecordBatch defaultReadOptions schema malformed body of
    Left err -> assert ("V4 union has top-level nulls" `isInfixOf` err)
    Right _ -> failure


-- * Fixtures


-- | A single-column schema and three batches of different lengths.
smallBatches :: (Schema, [V.Vector ColumnArray])
smallBatches =
  ( defaultSchema (V.fromList [defaultLeafField "a" False (AInt 32 True)])
  ,
    [ V.singleton (primColumn PInt32 (VS.fromList [1, 2, 3]))
    , V.singleton (primColumn PInt32 (VS.fromList [4]))
    , V.singleton (primColumn PInt32 (VS.fromList [5, 6, 7, 8]))
    ]
  )


fileBytesOf :: (Schema, [V.Vector ColumnArray]) -> Either String ByteString
fileBytesOf (sch, batches) = encodeArrowFile defaultWriteOptions sch batches


-- * Properties


{- | The pure footer-indexed reader and the eager decoder agree on
every batch, and the footer's block table names one block per record
batch and no dictionary blocks.
-}
prop_fileSectionsMatchEager :: Property
prop_fileSectionsMatchEager = withTests 1 . property $ do
  let (sch, batches) = smallBatches
  bytes <- evalEither (fileBytesOf (sch, batches))
  (_, eager) <- evalEither (decodeArrowFile defaultReadOptions bytes)
  length eager === length batches
  idx <- evalEither (readFileIndex bytes)
  V.length (fiRecordBatches idx) === length batches
  V.null (fiDictBlocks idx) === True
  rd <- evalEither (openFileReader defaultReadOptions bytes)
  fileReaderSchema rd === sch
  got <- evalEither (fileReaderToList rd)
  got === eager


-- | The same parity against @test/golden/pa_file.arrow@, written by pyarrow.
prop_pyarrowFileSections :: Property
prop_pyarrowFileSections = withTests 1 . property $ do
  bytes <- evalIO (BS.readFile "test/golden/pa_file.arrow")
  (_, eager) <- evalEither (decodeArrowFile defaultReadOptions bytes)
  idx <- evalEither (readFileIndex bytes)
  V.length (fiRecordBatches idx) === length eager
  rd <- evalEither (openFileReader defaultReadOptions bytes)
  got <- evalEither (fileReaderToList rd)
  got === eager


-- | 'fileBatchesHandle' over a temp file yields exactly the eager batches.
prop_fileBatchesHandle :: Property
prop_fileBatchesHandle = withTests 1 . property $ do
  let (sch, batches) = smallBatches
  bytes <- evalEither (fileBytesOf (sch, batches))
  (_, eager) <- evalEither (decodeArrowFile defaultReadOptions bytes)
  got <- evalIO (withTempFile bytes (drainFile defaultReadOptions))
  got === Right eager


{- | 'streamBatchesHandle' over a temp stream yields exactly the eager
batches, including for a stream with dictionary batches (which the
reader must apply as it walks).
-}
prop_streamBatchesHandle :: Property
prop_streamBatchesHandle = withTests 1 . property $ do
  let (sch, batches) = smallBatches
  streamBytes <- evalEither (encodeArrowStream defaultWriteOptions sch batches)
  (_, eager) <- evalEither (decodeArrowStream defaultReadOptions streamBytes)
  got <- evalIO (withTempFile streamBytes (drainStream defaultReadOptions))
  got === Right eager
  dictBytes <- evalIO (BS.readFile "test/golden/pa_dict.arrows")
  (_, eagerDicts) <- evalEither (decodeArrowStream defaultReadOptions dictBytes)
  gotDicts <- evalIO (withTempFile dictBytes (drainStream defaultReadOptions))
  gotDicts === Right eagerDicts


{- | Zeroing the last section's message header costs nothing before
its turn: the handle iterator hands over every earlier batch and only
then reports the broken section.
-}
prop_corruptSectionFailsLate :: Property
prop_corruptSectionFailsLate = withTests 1 . property $ do
  let (sch, batches) = smallBatches
  bytes <- evalEither (fileBytesOf (sch, batches))
  (_, eager) <- evalEither (decodeArrowFile defaultReadOptions bytes)
  idx <- evalEither (readFileIndex bytes)
  let lastBlock = V.unsafeIndex (fiRecordBatches idx) (V.length (fiRecordBatches idx) - 1)
      off = fromIntegral (abOffset lastBlock)
      metaLen = fromIntegral (abMetaLen lastBlock)
      broken = BS.take off bytes <> BS.replicate metaLen 0 <> BS.drop (off + metaLen) bytes
  (tag, got) <- evalIO $ withTempFile broken $ \h -> do
    eiter <- fileBatchesHandle defaultReadOptions h
    case eiter of
      Left e -> pure ("open failed: " ++ e, [])
      Right it -> drainSteps it []
  take (length eager - 1) got === take (length eager - 1) eager
  length got === length eager - 1
  True === ("error" `isPrefixOf` tag)


-- * Helpers


-- | Step an iterator to its end, collecting the batches it yields.
drainSteps :: IterIO (V.Vector ColumnArray) -> [V.Vector ColumnArray] -> IO (String, [V.Vector ColumnArray])
drainSteps it acc = do
  estep <- iterIOStep it
  case estep of
    Left e -> pure ("error: " ++ e, reverse acc)
    Right IterIODone -> pure ("done", reverse acc)
    Right (IterIOYield batch next) -> drainSteps next (batch : acc)


-- | Write bytes to a temp file, position the handle at 0, run the action.
withTempFile :: ByteString -> (Handle -> IO a) -> IO a
withTempFile bs act =
  bracket
    (getTemporaryDirectory >>= \dir -> openBinaryTempFile dir "wf-arrow-sections.tmp")
    (\(path, h) -> hClose h >> removeFile path)
    (\(_path, h) -> BS.hPut h bs >> hSeek h AbsoluteSeek 0 >> act h)


-- | Drain a file iterator into the batches it yields.
drainFile :: ReadOptions -> Handle -> IO (Either String [V.Vector ColumnArray])
drainFile opts h = do
  eiter <- fileBatchesHandle opts h
  case eiter of
    Left e -> pure (Left e)
    Right it -> iterIOToList it


-- | Drain a stream iterator into the batches it yields.
drainStream :: ReadOptions -> Handle -> IO (Either String [V.Vector ColumnArray])
drainStream opts h = do
  eiter <- streamBatchesHandle opts h
  case eiter of
    Left e -> pure (Left e)
    Right it -> iterIOToList it
