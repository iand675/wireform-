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
) where

import Control.Monad (when)
import Kafka.Streams.Config (ProcessingGuarantee (..))
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
  removeFile,
  removePathForcibly,
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
