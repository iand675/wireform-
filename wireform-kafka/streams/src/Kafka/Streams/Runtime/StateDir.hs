{- |
Module      : Kafka.Streams.Runtime.StateDir
Description : Clean-shutdown marker + EOS state wipe for @state.dir@

Under exactly-once a task's local state is only trustworthy if the
process stopped at a commit boundary: anything a store absorbed
after the last committed transaction belongs to a transaction the
broker aborted, and replaying the changelog tail on top of it
double-applies those writes. The JVM runtime handles this by
writing its @.checkpoint@ only on a clean close and wiping task
state that lacks one. This module is the same contract for the
whole @state.dir@:

  * 'markCleanShutdown' drops 'cleanShutdownMarker' into the
    directory after a close that stopped at a commit boundary.
  * 'prepareStateDir' runs before any store is opened. It consumes
    the marker (so a crash from here on reads as unclean) and,
    under 'ExactlyOnceV2', deletes everything under the directory
    when the marker was missing. Stores then start empty and
    restore from their changelogs.

The directory itself is kept (it is often a volume mount point);
only its entries go. Stores must live under @state.dir@ for this
to cover them, which is the layout the containers guide already
prescribes.
-}
module Kafka.Streams.Runtime.StateDir (
  cleanShutdownMarker,
  prepareStateDir,
  markCleanShutdown,

  -- * Task checkpoints
  checkpointFile,
  readCheckpoint,
  writeCheckpoint,
  deleteCheckpoint,
) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Kafka.Streams.Config (ProcessingGuarantee (..))
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  removeFile,
  removePathForcibly,
  renameFile,
 )
import System.FilePath ((</>))


-- | File name of the clean-shutdown marker inside @state.dir@.
cleanShutdownMarker :: FilePath
cleanShutdownMarker = ".wireform-clean-shutdown"


{- | Call before opening any store. Removes the clean-shutdown
marker and, under 'ExactlyOnceV2', wipes the directory's contents
if the previous run did not leave one. Returns 'True' when it
wiped something. A missing or empty directory is left alone.
-}
prepareStateDir :: ProcessingGuarantee -> FilePath -> IO Bool
prepareStateDir guarantee dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure False
    else do
      let marker = dir </> cleanShutdownMarker
      clean <- doesFileExist marker
      when clean (removeFile marker)
      case guarantee of
        ExactlyOnceV2
          | not clean -> do
              entries <- listDirectory dir
              mapM_ (removePathForcibly . (dir </>)) entries
              pure (not (null entries))
        _ -> pure False


{- | Record that the state under @dir@ matches the last committed
transaction. Only call once every store has been flushed and
closed after the stream thread stopped at a commit boundary.
-}
markCleanShutdown :: FilePath -> IO ()
markCleanShutdown dir = do
  exists <- doesDirectoryExist dir
  when exists (writeFile (dir </> cleanShutdownMarker) "")


----------------------------------------------------------------------
-- Checkpoints
----------------------------------------------------------------------

{- | A task's @.checkpoint@: for each changelog partition, the offset
its persistent stores are known to hold everything before. Restore
starts there instead of at the log start. Written after a commit (at
least once) or a clean close (both guarantees); a task whose state
may be ahead of or behind it deletes it.
-}
checkpointFile :: FilePath -> FilePath
checkpointFile taskDir = taskDir </> ".checkpoint"


{- | The checkpoint in @taskDir@; empty when there is none or it does
not parse (restoring from the log start is always safe).
-}
readCheckpoint :: FilePath -> IO (Map (Text, Int32) Int64)
readCheckpoint taskDir = do
  r <- try (BS.readFile (checkpointFile taskDir)) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Left _ -> Map.empty
    Right bs -> case T.lines (TE.decodeUtf8Lenient bs) of
      ("0" : _count : entries) -> maybe Map.empty Map.fromList (traverse entry entries)
      _ -> Map.empty
  where
    entry ln = case T.words ln of
      [t, p, o] -> case (TR.decimal p, TR.signed TR.decimal o) of
        (Right (p', ""), Right (o', "")) -> Just ((t, p'), o')
        _ -> Nothing
      _ -> Nothing


-- | Replace the checkpoint in @taskDir@ (write, then rename).
writeCheckpoint :: FilePath -> Map (Text, Int32) Int64 -> IO ()
writeCheckpoint taskDir offs = do
  createDirectoryIfMissing True taskDir
  let path = checkpointFile taskDir
      tmp = path <> ".tmp"
      body =
        BB.string7 "0\n"
          <> BB.intDec (Map.size offs)
          <> BB.char7 '\n'
          <> Map.foldMapWithKey
            ( \(t, p) o ->
                BB.byteString (TE.encodeUtf8 t) <> BB.char7 ' ' <> BB.int32Dec p <> BB.char7 ' ' <> BB.int64Dec o <> BB.char7 '\n'
            )
            offs
  BSL.writeFile tmp (BB.toLazyByteString body)
  renameFile tmp path


deleteCheckpoint :: FilePath -> IO ()
deleteCheckpoint taskDir = do
  let path = checkpointFile taskDir
  exists <- doesFileExist path
  when exists (removeFile path)
