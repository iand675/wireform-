{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.Runtime.EOS
Description : Exactly-once-v2 commit orchestration for the Streams runtime

This module provides the /orchestration/ layer that drives the
transactional producer through the EOS-v2 commit cycle:

  1. On startup, the runtime calls 'eosInit' once.  It maps to
     @InitProducerId@ on the broker via the underlying
     'Kafka.Client.Transaction'.
  2. On every commit interval, the runtime calls 'eosBegin' →
     flushes the engine (writes records through the transactional
     producer) → 'eosCommitOffsets' (sends @TxnOffsetCommit@ for
     the consumer-group offsets it consumed since the previous
     commit) → 'eosCommit'.
  3. On any unrecoverable error, the runtime calls 'eosAbort'
     and either rejoins the consumer group or shuts down,
     depending on the error.

== Why a separate module

The orchestration is defined against an 'EOSCoordinator' record
rather than a hard dependency on 'Kafka.Client.Transaction', so
tests can inject a mock and verify the call sequence.
'newRealEOSCoordinator' wires it to the real client.
-}
module Kafka.Streams.Runtime.EOS (
  EOSCoordinator (..),
  noopEOSCoordinator,
  runCommitCycle,
  CommitOutcome (..),
  newRealEOSCoordinator,
  withTransactionalStores,
) where

import Control.Exception (SomeException, try)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Kafka.Client.Internal.TransactionCoordinator qualified as TC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Transaction qualified as KT


----------------------------------------------------------------------
-- Coordinator interface
----------------------------------------------------------------------

{- | Pluggable EOS coordinator. The Streams runtime calls these
callbacks; production wires them to 'Kafka.Client.Transaction',
tests wire them to a recorder.
-}
data EOSCoordinator = EOSCoordinator
  { initTxn :: !(IO (Either Text ()))
  , beginTxn :: !(IO (Either Text ()))
  , commitTxn :: !(IO (Either Text ()))
  , abortTxn :: !(IO (Either Text ()))
  , commitOffsets
      :: !( Text -- consumer group id
            -> HashMap KC.TopicPartition Int64
            -> IO (Either Text ())
          )
  , storeCommit :: !(IO (Either Text ()))
  {- ^ KIP-892: drain every transactional state store onto its
  underlying store. Called by 'runCommitCycle' AFTER the
  producer transaction commit succeeds, so the store
  write happens iff the wire-side commit was durable.
  Default ('noopEOSCoordinator'): pure (Right ()).
  -}
  , storeAbort :: !(IO (Either Text ()))
  {- ^ KIP-892: discard every transactional state store's
  buffered writes. Called when the producer transaction
  aborts so the store and the broker-side log stay
  consistent.
  -}
  , preCommit2PC :: !(IO (Either Text ()))
  {- ^ Riffle \xc2\xa74: two-phase-commit sink /prepare/ phase. Fired
  AFTER the per-task @flushBody@ has produced records and
  AFTER 'commitOffsets' but BEFORE 'commitTxn', so a
  failure here aborts the producer transaction. Default
  ('noopEOSCoordinator'): @pure (Right ())@.
  -}
  , commit2PC :: !(IO (Either Text ()))
  {- ^ Riffle \xc2\xa74: two-phase-commit sink /commit/ phase. Fired
  AFTER 'commitTxn' succeeds but BEFORE 'storeCommit'.
  Failure here is unrecoverable: the producer transaction
  is durable but the 2PC sink could not finalise, so the
  cycle returns 'CommitFatal' and the runtime's
  'tpsRecover' decides per-token on restart.
  -}
  , abort2PC :: !(IO ())
  {- ^ Riffle \xc2\xa74: best-effort cleanup for any sink that had
  been pre-committed in this cycle. Fired alongside
  'abortTxn' / 'storeAbort' on the recovery path. Return
  value intentionally discarded — failures here are
  logged but don't change the cycle outcome.
  -}
  }


{- | The do-nothing coordinator: every step succeeds without side
effects. Used by the at-least-once code path.
-}
noopEOSCoordinator :: EOSCoordinator
noopEOSCoordinator =
  EOSCoordinator
    { initTxn = pure (Right ())
    , beginTxn = pure (Right ())
    , commitTxn = pure (Right ())
    , abortTxn = pure (Right ())
    , commitOffsets = \_ _ -> pure (Right ())
    , storeCommit = pure (Right ())
    , storeAbort = pure (Right ())
    , preCommit2PC = pure (Right ())
    , commit2PC = pure (Right ())
    , abort2PC = pure ()
    }


-- | Result of one commit cycle.
data CommitOutcome
  = CommitSucceeded
  | -- | Aborted due to an error; runtime should rebalance
    CommitAborted !Text
  | -- | Fatal error; runtime should fail-fast
    CommitFatal !Text
  deriving stock (Eq, Show)


{- | Drive a single commit cycle. The supplied @flushBody@ is run
between 'eosBegin' and 'eosCommit'; it should drive the engine
and, if EOS is on, send all produced records via the transactional
producer.
-}
runCommitCycle
  :: EOSCoordinator
  -> Text -- consumer group id
  -> IO (HashMap KC.TopicPartition Int64) -- offsets to commit
  -> IO () -- flush body
  -> IO CommitOutcome
runCommitCycle coord groupId getOffsets flushBody = do
  step1 <- coord.beginTxn
  case step1 of
    Left err -> pure (CommitFatal ("begin: " <> err))
    Right () -> do
      bodyR <- try flushBody :: IO (Either SomeException ())
      case bodyR of
        Left e -> doAbort ("flush: " <> T.pack (show e))
        Right () -> do
          -- Read the committed offset snapshot. Unlike the
          -- coordinator callbacks, this is supplied by the
          -- caller (the engine's consumer position bookkeeping)
          -- and is not necessarily total — a closed consumer,
          -- a metadata fault, or an in-flight rebalance can all
          -- cause it to throw. The cycle treats any synchronous
          -- exception here as a commit failure and triggers the
          -- usual abort path so the producer transaction and
          -- store buffers don't leak across cycles.
          offsR <-
            try getOffsets
              :: IO
                   ( Either
                       SomeException
                       (HashMap KC.TopicPartition Int64)
                   )
          case offsR of
            Left e -> doAbort ("getOffsets: " <> T.pack (show e))
            Right offs -> do
              step2 <- coord.commitOffsets groupId offs
              case step2 of
                Left err -> doAbort ("commitOffsets: " <> err)
                Right () -> do
                  -- Riffle \xc2\xa74: 2PC sink pre-commit fires here.
                  -- The sinks have flushed their batches in the
                  -- @flushBody@ above; this step asks them to
                  -- transition to "prepared". A failure aborts
                  -- the producer txn too.
                  step3 <- coord.preCommit2PC
                  case step3 of
                    Left err -> doAbort ("preCommit2PC: " <> err)
                    Right () -> do
                      step4 <- coord.commitTxn
                      case step4 of
                        Left err -> doAbort ("commit: " <> err)
                        Right () -> do
                          -- Riffle \xc2\xa74: producer txn is now
                          -- durable; finalise the 2PC sinks.
                          -- A failure here is FATAL — the wire
                          -- side is committed but the sink is
                          -- still half-prepared. The runtime's
                          -- 'tpsRecover' decides per-token on
                          -- restart.
                          step5 <- coord.commit2PC
                          case step5 of
                            Left err ->
                              pure
                                ( CommitFatal
                                    ("commit2PC: " <> err)
                                )
                            Right () -> do
                              -- KIP-892: the producer commit
                              -- succeeded, so the changelog
                              -- records are durable; only now
                              -- is it safe to drain the
                              -- per-task TransactionalStore
                              -- buffers onto their underlying
                              -- stores.
                              step6 <- coord.storeCommit
                              case step6 of
                                Left err ->
                                  pure
                                    ( CommitFatal
                                        ("storeCommit: " <> err)
                                    )
                                Right () -> pure CommitSucceeded
  where
    doAbort reason = do
      _ <- coord.abortTxn
      -- KIP-892: matching abort on the store side discards
      -- buffered writes so a retry starts from a clean slate.
      _ <- coord.storeAbort
      -- Riffle \xc2\xa74: best-effort 2PC sink abort. Whatever was
      -- pre-committed gets rolled back; the result is
      -- intentionally ignored.
      coord.abort2PC
      pure (CommitAborted reason)


----------------------------------------------------------------------
-- Real-broker coordinator
----------------------------------------------------------------------

{- | Wire the coordinator to a real 'KT.Transaction'. Translates the
transaction client's 'TransactionError' into 'Text' for the
orchestrator.
-}
newRealEOSCoordinator :: KT.Transaction -> KC.Consumer -> EOSCoordinator
newRealEOSCoordinator txn consumer =
  EOSCoordinator
    { initTxn = wrapTE <$> KT.initTransactions txn
    , beginTxn = wrapTE <$> KT.beginTransaction txn
    , commitTxn = wrapTE <$> KT.commitTransaction txn
    , abortTxn = wrapTE <$> KT.abortTransaction txn
    , commitOffsets = \_gid offs -> do
        mCoord <- KC.groupCoordinator consumer
        meta <- KC.groupMetadata consumer
        case mCoord of
          Nothing -> pure (Left "no group coordinator known")
          Just coord ->
            wrapTE
              <$> KT.sendOffsetsToTransaction
                txn
                coord
                (toTxnMember meta)
                (HashMap.toList offs)
    , storeCommit = pure (Right ())
    , -- No transactional stores registered with this coordinator
      -- by default. Callers that materialise stores wrap the
      -- coordinator with 'withTransactionalStores' below.
      storeAbort = pure (Right ())
    , preCommit2PC = pure (Right ())
    , commit2PC = pure (Right ())
    , abort2PC = pure ()
    }
  where
    wrapTE = either (Left . T.pack . show) Right
    toTxnMember meta =
      TC.TxnGroupMember
        { TC.tgmGroupId = meta.cgmGroupId
        , TC.tgmGenerationId = meta.cgmGenerationId
        , TC.tgmMemberId = meta.cgmMemberId
        , TC.tgmGroupInstanceId = meta.cgmGroupInstanceId
        }


{- | Transactional-store wiring helper: take an existing coordinator and a
list of 'TransactionalStore'-like commit/abort actions and
thread them through 'eosStoreCommit' / 'eosStoreAbort'. The
callbacks are run in declaration order on commit; the first
'Left' short-circuits and the runtime promotes the cycle to
'CommitFatal'.

Callers typically build the list once at engine
construction time and reuse the coordinator across every
commit cycle.
-}
withTransactionalStores
  :: EOSCoordinator
  -> [IO ()] -- per-store commit actions
  -> [IO ()] -- per-store abort actions
  -> EOSCoordinator
withTransactionalStores base commits aborts =
  base
    { storeCommit = runActions commits "store-commit"
    , storeAbort = runActions aborts "store-abort"
    }
  where
    runActions xs label = do
      r <- try (sequence_ xs) :: IO (Either SomeException ())
      case r of
        Right () -> pure (Right ())
        Left e -> pure (Left (label <> ": " <> T.pack (show e)))
