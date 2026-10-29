{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Sinks.TwoPhase
Description : Two-phase-commit sink contract + EOS integration

Riffle §1 wants a Flink-class \"sink interface\" that survives the
same kinds of failures the producer/consumer protocol survives:
a sink should be able to /prepare/ a batch (a side-effect that is
visible to no consumer until commit), then /commit/ exactly when
the upstream Kafka transaction commits, or /abort/ when it aborts.

This module defines the contract a Riffle 2PC sink must satisfy, and
'twoPhaseParticipant' makes one take part in the runtime's commit
cycles ("Kafka.Streams.Runtime.Commit").

== Reference implementations

For deterministic in-process testing the module also ships a
handful of reference sinks. They have the same correctness
properties as the real adapter targets (JDBC / Iceberg / S3 /
HTTP), but live in pure IO so the chaos suite can drive them:

  * 'inMemoryTwoPhaseSink' — appends rows to an 'IORef',
    prepare-buffers them, and only flushes on commit. Used by the
    property tests.
  * 'filesystemTwoPhaseSink' — writes each prepared batch to a
    @prepared/\<txn\>.batch@ file, then renames into @committed/@
    on commit (atomic on POSIX). This mirrors what an Iceberg or
    S3 sink would do at the manifest / object-rename layer.
  * 'httpEchoTwoPhaseSink' — buffers each prepared batch, only
    fires the supplied @onCommit@ callback once the upstream
    commit succeeds. This is the shape a 2PC HTTP sink takes
    when the target endpoint supports prepare/commit semantics.

The real JDBC / Iceberg / S3 / HTTP adapters live in separate
packages (e.g. @wireform-jdbc@, @wireform-iceberg@) because they
depend on external runtimes; this module defines the contract
they satisfy.

== Commit-cycle wiring

Rows reach a sink through 'tpsStage' (the 'compileSinkTwoPhase'
processor). Register the sink with 'twoPhaseParticipant' and
'Kafka.Streams.Runtime.addCommitParticipant': each commit cycle it
prepares the staged rows, commits them once Kafka has committed the
cycle, and aborts them otherwise; prepared cycles left by a crash
are settled at the next start.
-}
module Kafka.Streams.Sinks.TwoPhase (
  -- * Contract
  TwoPhaseSink (..),
  SinkTxnId (..),
  SinkOutcome (..),
  RecoveryDecision (..),
  noopTwoPhaseSink,

  -- * Reference sinks
  inMemoryTwoPhaseSink,
  InMemorySinkState,
  readCommittedRows,
  readPreparedRows,
  filesystemTwoPhaseSink,
  httpEchoTwoPhaseSink,

  -- * Runtime wiring
  twoPhaseParticipant,

  -- * Direct driving (for tests / non-EOS callers)
  runSinkCommitCycle,

  -- * Topology compile path (used by the Free DSL Prim)
  compileSinkTwoPhase,
) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (
  IORef,
  atomicModifyIORef',
  atomicWriteIORef,
  modifyIORef',
  newIORef,
  readIORef,
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.KStream qualified as KS
import Kafka.Streams.Runtime.Commit (CommitParticipant (..))
import Kafka.Streams.Types (Record)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  listDirectory,
  removeFile,
  renameFile,
 )
import System.FilePath ((</>))


----------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------

{- | Identity of one in-flight sink transaction. The coordinator
hands a fresh id to every commit cycle so a sink can recover
prepared-but-not-committed state on startup.
-}
newtype SinkTxnId = SinkTxnId {unSinkTxnId :: Text}
  deriving stock (Eq, Ord, Show)


-- | Result of a 2PC step.
data SinkOutcome
  = SinkOK
  | {- | The downstream system is transiently unavailable. The
    coordinator will treat the cycle as 'CommitAborted' and the
    runtime will retry on the next cycle.
    -}
    SinkRetryable !Text
  | {- | The sink has lost consistency between the prepared batch
    and the downstream system in a way that the runtime can't
    recover from automatically. The cycle becomes 'CommitFatal'.
    -}
    SinkFatal !Text
  deriving stock (Eq, Show)


{- | What the runtime should do with a half-committed sink txn
discovered on startup via 'tpsRecover'. Mirrors Flink's
@TwoPhaseCommitSinkFunction.recoverAndCommit@ vs
@recoverAndAbort@ vs "give up" trichotomy.
-}
data RecoveryDecision
  = {- | The producer transaction had committed; finish the
    sink txn so the downstream sees the same data.
    -}
    CommitFromToken
  | {- | The producer transaction had aborted; roll back the
    sink txn so neither side keeps the data.
    -}
    AbortFromToken
  | {- | The runtime can't tell; log the token and leave the
    sink alone for an operator to resolve manually.
    -}
    UnknownLeaveAsIs
  deriving stock (Eq, Show)


{- | A sink with two-phase commit semantics, parameterised by the
row type @r@. The contract:

  * 'tpsPrepare' is invoked during the engine's pre-commit
    drain. The sink must record the rows but must NOT make them
    visible to downstream consumers.
  * 'tpsCommit' is invoked /after/ the upstream Kafka
    transaction commit succeeds. The sink atomically makes the
    prepared batch visible.
  * 'tpsAbort' is invoked when the upstream transaction aborts.
    The sink discards the prepared batch.
  * 'tpsRecover' is invoked once at startup. The sink should
    surface any txns it sees prepared on the downstream system
    so the runtime can either commit or roll them back depending
    on the committed-offset checkpoint.

The implementation is responsible for idempotence: any of the
four operations may be re-invoked on retry, so each should be a
no-op when called against an already-completed txn.
-}
data TwoPhaseSink r = TwoPhaseSink
  { tpsName :: !Text
  {- ^ Stable identifier (for logging / metrics / failure
  attribution).
  -}
  , tpsStage :: !(r -> IO ())
  {- ^ Per-record /stage/ hook. The topology's
  'Kafka.Streams.Topology.Free.SinkTwoPhase' processor
  calls this for every record on the stream as it arrives.
  The sink decides whether to buffer in memory and flush
  on 'tpsPrepare', or to write speculatively and verify on
  prepare. Reference sinks ship the buffering variant.
  -}
  , tpsPrepare :: !(SinkTxnId -> [r] -> IO SinkOutcome)
  {- ^ Transition the staged rows + the explicit @[r]@ batch
  to the /prepared/ state. The @[r]@ argument is the
  caller-side batch ('runSinkCommitCycle' passes one; the
  runtime passes @[]@, all its rows arrive via 'tpsStage').
  Sinks must merge the two sources before preparing.
  -}
  , tpsCommit :: !(SinkTxnId -> IO SinkOutcome)
  , tpsAbort :: !(SinkTxnId -> IO SinkOutcome)
  , tpsDiscardStaged :: !(IO ())
  {- ^ Drop every row staged since the last 'tpsPrepare': the runtime
  is rewinding its input to the last commit and will stage those
  rows again as it reprocesses them.
  -}
  , tpsRecover :: !(IO [SinkTxnId])
  {- ^ List of txns currently in the @prepared@ state on the
  downstream. The runtime should call this on startup, decide
  per-txn whether to commit or abort (the per-token decision
  is the sink's via 'RecoveryDecision').
  -}
  }


-- | Sink that ignores every operation. Useful as a default.
noopTwoPhaseSink :: Text -> TwoPhaseSink r
noopTwoPhaseSink nm =
  TwoPhaseSink
    { tpsName = nm
    , tpsStage = \_ -> pure ()
    , tpsPrepare = \_ _ -> pure SinkOK
    , tpsCommit = \_ -> pure SinkOK
    , tpsAbort = \_ -> pure SinkOK
    , tpsDiscardStaged = pure ()
    , tpsRecover = pure []
    }


----------------------------------------------------------------------
-- In-memory reference sink
----------------------------------------------------------------------

{- | Observable state of an 'inMemoryTwoPhaseSink'. The chaos
suite reads this directly to assert invariants.
-}
data InMemorySinkState r = InMemorySinkState
  { imsPrepared :: !(IORef (Map SinkTxnId [r]))
  , imsCommitted :: !(IORef [r])
  -- ^ Append-only history of committed rows in commit order.
  }


readCommittedRows :: InMemorySinkState r -> IO [r]
readCommittedRows = fmap reverse . readIORef . imsCommitted


-- Stored in reverse-order for O(1) prepend on commit; reverse on
-- read for caller-friendly order.

readPreparedRows :: InMemorySinkState r -> IO (Map SinkTxnId [r])
readPreparedRows = readIORef . imsPrepared


{- | A sink that buffers prepared rows in memory and atomically
promotes them to a committed history on commit. Aborts discard
the prepared batch.
-}
inMemoryTwoPhaseSink :: Text -> IO (TwoPhaseSink r, InMemorySinkState r)
inMemoryTwoPhaseSink nm = do
  prep <- newIORef Map.empty
  done <- newIORef []
  staged <- newIORef ([] :: [r])
  let s = InMemorySinkState prep done
  pure
    ( TwoPhaseSink
        { tpsName = nm
        , tpsStage = \r ->
            atomicModifyIORef' staged (\rs -> (r : rs, ()))
        , tpsPrepare = \txn extraRows -> do
            stagedRows <-
              atomicModifyIORef'
                staged
                (\rs -> ([], reverse rs))
            let rows = stagedRows ++ extraRows
            atomicModifyIORef'
              prep
              ( \m ->
                  (Map.insert txn rows m, ())
              )
            pure SinkOK
        , tpsCommit = \txn -> do
            mrows <-
              atomicModifyIORef'
                prep
                ( \m ->
                    (Map.delete txn m, Map.lookup txn m)
                )
            case mrows of
              Nothing -> pure SinkOK -- already committed: idempotent
              Just rs -> do
                modifyIORef' done (\xs -> reverse rs ++ xs)
                pure SinkOK
        , tpsAbort = \txn -> do
            atomicModifyIORef'
              prep
              ( \m ->
                  (Map.delete txn m, ())
              )
            pure SinkOK
        , tpsDiscardStaged = atomicWriteIORef staged []
        , tpsRecover = Map.keys <$> readIORef prep
        }
    , s
    )


----------------------------------------------------------------------
-- Filesystem reference sink
----------------------------------------------------------------------

{- | A sink that materialises each prepared batch as a file in
@\<root\>/prepared/@ and renames it to @\<root\>/committed/@ on
commit. The rename is atomic on POSIX, so a crash between
'tpsCommit' steps leaves the system in either the "prepared" or
"committed" state but never half-and-half. This is structurally
the same protocol Iceberg uses at the manifest layer and S3
sinks use at the object-naming layer.

The serialiser converts each row into a 'ByteString'; the on-disk
format is just the rows concatenated with newline separators.
Real Iceberg / S3 adapters override this with their wire format.
-}
filesystemTwoPhaseSink
  :: forall r
   . FilePath
  -> (r -> ByteString)
  -> Text
  -> IO (TwoPhaseSink r)
filesystemTwoPhaseSink root encode nm = do
  createDirectoryIfMissing True (root </> "prepared")
  createDirectoryIfMissing True (root </> "committed")
  staged <- newIORef ([] :: [r])
  pure
    TwoPhaseSink
      { tpsName = nm
      , tpsStage = \r ->
          atomicModifyIORef' staged (\rs -> (r : rs, ()))
      , tpsPrepare = \(SinkTxnId txn) extraRows -> do
          stagedRows <-
            atomicModifyIORef'
              staged
              (\rs -> ([], reverse rs))
          let rows = stagedRows ++ extraRows
              path = root </> "prepared" </> T.unpack txn <> ".batch"
              body = BS.intercalate "\n" (map encode rows)
          r <- try (BS.writeFile path body)
          case r of
            Right () -> pure SinkOK
            Left (e :: SomeException) ->
              pure (SinkRetryable ("write prepared: " <> T.pack (show e)))
      , tpsCommit = \(SinkTxnId txn) -> do
          let src = root </> "prepared" </> T.unpack txn <> ".batch"
              dst = root </> "committed" </> T.unpack txn <> ".batch"
          exists <- doesFileExist src
          if not exists
            then pure SinkOK -- already committed: idempotent
            else do
              r <- try (renameFile src dst)
              case r of
                Right () -> pure SinkOK
                Left (e :: SomeException) ->
                  pure (SinkRetryable ("rename: " <> T.pack (show e)))
      , tpsAbort = \(SinkTxnId txn) -> do
          let path = root </> "prepared" </> T.unpack txn <> ".batch"
          exists <- doesFileExist path
          when exists $ do
            r <- try (removeFile path)
            case r of
              Right () -> pure ()
              Left (_ :: SomeException) -> pure ()
          pure SinkOK
      , tpsDiscardStaged = atomicWriteIORef staged []
      , tpsRecover =
          -- strip the ".batch" suffix
          map (SinkTxnId . T.dropEnd 6 . T.pack) <$> listDirectory (root </> "prepared")
      }


----------------------------------------------------------------------
-- HTTP echo / deferred-callback reference sink
----------------------------------------------------------------------

{- | A sink whose 'tpsPrepare' just buffers the rows in memory and
whose 'tpsCommit' fires a caller-supplied @onCommit@ callback
with the full batch. This mirrors a 2PC HTTP sink that uses a
two-phase protocol over POST: prepare with @\/prepare\/{txn}@,
commit with @\/commit\/{txn}@.

Tests can supply an @onCommit@ that captures the rows into a
'TVar' or appends to an 'IORef' to assert delivery semantics.
-}
httpEchoTwoPhaseSink
  :: forall r
   . Text
  -> ([r] -> IO ())
  -> IO (TwoPhaseSink r)
httpEchoTwoPhaseSink nm onCommit = do
  buf <- newIORef (Map.empty :: Map SinkTxnId [r])
  staged <- newIORef ([] :: [r])
  pure
    TwoPhaseSink
      { tpsName = nm
      , tpsStage = \r ->
          atomicModifyIORef' staged (\rs -> (r : rs, ()))
      , tpsPrepare = \txn extraRows -> do
          stagedRows <-
            atomicModifyIORef'
              staged
              (\rs -> ([], reverse rs))
          let rows = stagedRows ++ extraRows
          atomicModifyIORef' buf (\m -> (Map.insert txn rows m, ()))
          pure SinkOK
      , tpsCommit = \txn -> do
          mrows <-
            atomicModifyIORef'
              buf
              ( \m ->
                  (Map.delete txn m, Map.lookup txn m)
              )
          case mrows of
            Nothing -> pure SinkOK -- already committed
            Just rs -> do
              r <- try (onCommit rs)
              case r of
                Right () -> pure SinkOK
                Left (e :: SomeException) ->
                  pure (SinkRetryable ("onCommit: " <> T.pack (show e)))
      , tpsAbort = \txn -> do
          atomicModifyIORef' buf (\m -> (Map.delete txn m, ()))
          pure SinkOK
      , tpsDiscardStaged = atomicWriteIORef staged []
      , tpsRecover = Map.keys <$> readIORef buf
      }


----------------------------------------------------------------------
-- Runtime wiring
----------------------------------------------------------------------

{- | The sink as a participant in the runtime's commit cycles (see
"Kafka.Streams.Runtime.Commit"): register it with
'Kafka.Streams.Runtime.addCommitParticipant' before starting.

Every cycle the sink prepares what 'tpsStage' collected since the last
one (after every task has flushed), commits it once the cycle is
committed in Kafka, and aborts it otherwise. A 'SinkRetryable' or
'SinkFatal' prepare aborts the cycle (the runtime rewinds and
reprocesses); a failed commit stops the stream thread, and the
cycle is committed by the recovery at the next start ('tpsRecover').
-}
twoPhaseParticipant :: TwoPhaseSink r -> CommitParticipant
twoPhaseParticipant sink =
  CommitParticipant
    { cpName = tpsName sink
    , cpPrepare = \cid -> outcome <$> tpsPrepare sink (SinkTxnId cid) []
    , cpCommit = \cid -> outcome <$> tpsCommit sink (SinkTxnId cid)
    , cpAbort = \cid -> () <$ tpsAbort sink (SinkTxnId cid)
    , cpDiscard = tpsDiscardStaged sink
    , cpRecover = map unSinkTxnId <$> tpsRecover sink
    }
  where
    outcome = \case
      SinkOK -> Right ()
      SinkRetryable e -> Left e
      SinkFatal e -> Left ("fatal: " <> e)


----------------------------------------------------------------------
-- Direct (no-EOS) sink driver for tests
----------------------------------------------------------------------

{- | Run a single 2PC cycle directly against a sink, for tests that
don't run the runtime. The supplied
@body@ produces the rows to prepare; the result of the upstream
"commit" determines whether the sink commits or aborts.
-}
runSinkCommitCycle
  :: TwoPhaseSink r
  -> SinkTxnId
  -> IO ([r], Bool)
  {- ^ @(rows, upstreamCommitOK)@. If 'True', the sink will be
  committed; if 'False', aborted.
  -}
  -> IO (Either Text ())
runSinkCommitCycle sink txn body = do
  (rows, ok) <- body
  p <- tpsPrepare sink txn rows
  case p of
    SinkRetryable e -> pure (Left ("prepare retry: " <> e))
    SinkFatal e -> pure (Left ("prepare fatal: " <> e))
    SinkOK
      | ok -> do
          c <- tpsCommit sink txn
          case c of
            SinkOK -> pure (Right ())
            SinkRetryable e -> pure (Left ("commit retry: " <> e))
            SinkFatal e -> pure (Left ("commit fatal: " <> e))
      | otherwise -> do
          _ <- tpsAbort sink txn
          pure (Right ())


----------------------------------------------------------------------
-- Compile path for the SinkTwoPhase Prim
----------------------------------------------------------------------

{- | Compile a 'TwoPhaseSink' attached to a 'KStream' into a
foreach processor: every record on the stream gets staged via
'tpsStage'. The sink's per-record buffer is consumed on the
next 'tpsPrepare' invocation (the runtime's commit cycle, see
'twoPhaseParticipant').

The sink commits with the runtime once registered through
'twoPhaseParticipant'; the compile path only owns the per-record
staging.
-}
compileSinkTwoPhase
  :: forall k v opaque
   . opaque
  {- ^ The topology builder. The compile path doesn't need it
  directly — 'KS.foreachStream' picks the builder out of the
  'KStream' — but the Free DSL passes it positionally, so
  we accept and ignore it here to keep the call shape
  uniform with 'Sink' \/ 'SinkExtracted'.
  -}
  -> TwoPhaseSink (Record k v)
  -> KS.KStream k v
  -> IO ()
compileSinkTwoPhase _ sink =
  KS.foreachStream (tpsStage sink)
