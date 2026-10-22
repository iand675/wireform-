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
import Arrow.FlatBufferIPC (ArrowBlock (..), FileIndex (..), readFileIndex)
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
  streamBatchesHandle,
 )
import Arrow.Types (ArrowType (..), Schema, defaultLeafField, defaultSchema)
import Columnar.Stream (IterIO, IterIOStep (..), iterIOStep, iterIOToList)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
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
      ]


-- * Fixtures


-- | A single-column schema and three batches of different lengths.
smallBatches :: (Schema, [V.Vector ColumnArray])
smallBatches =
  ( defaultSchema (V.fromList [defaultLeafField "a" False (AInt 32 True)])
  , [ V.singleton (primColumn PInt32 (VS.fromList [1, 2, 3]))
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
