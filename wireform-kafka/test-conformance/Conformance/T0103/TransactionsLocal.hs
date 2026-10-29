{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0103.TransactionsLocal
Description : librdkafka @tests\/0103-transactions_local.c@

librdkafka's @0103-transactions_local@ exercises the transactional
producer state machine with no broker. It checks:

  * @init_transactions@ is required before @begin_transaction@.
  * @begin_transaction@ is required before @send_offsets_to_transaction@,
    @commit_transaction@, @abort_transaction@.
  * Calling out-of-order operations returns a structured error and
    leaves the state machine well-defined.

Our analogue is the in-memory state machine in
'Kafka.Client.Transaction'. We exercise the same invariants without
talking to a coordinator.
-}
module Conformance.T0103.TransactionsLocal (tests) where

import Data.IORef (readIORef)
import Kafka.Client.Internal.Heartbeat qualified as HB
import Kafka.Client.Transaction qualified as Txn
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Test.Syd


{- | Build a Transaction handle without going to any broker. We
create the underlying state TVars but never call into the
coordinator; the state-machine guards we are testing fire before
any I/O.
-}
mkLocalTransaction :: IO Txn.Transaction
mkLocalTransaction = do
  connMgr <- Conn.createConnectionManager
  versionCache <- AV.createVersionCache
  Txn.createTransaction
    (Txn.TransactionalId "wireform-conformance-0103")
    connMgr
    Conn.defaultConnectionConfig
    versionCache
    "wireform-kafka"
    (Conn.BrokerAddress "127.0.0.1" 1)
    60000


tests :: Spec
tests =
  describe "0103-transactions_local" $
    sequence_
      [ it "fresh transaction is in Uninitialized state" $ do
          txn <- mkLocalTransaction
          st <- Txn.getTransactionState txn
          st `shouldBe` Txn.Uninitialized
      , it "valid state transitions: Uninitialized -> Ready -> InTransaction -> Committing -> Ready" $ do
          txn <- mkLocalTransaction
          ok1 <- Txn.transitionState txn Txn.Ready
          ok1 `shouldBe` True
          ok2 <- Txn.transitionState txn Txn.InTransaction
          ok2 `shouldBe` True
          ok3 <- Txn.transitionState txn Txn.Committing
          ok3 `shouldBe` True
          ok4 <- Txn.transitionState txn Txn.Ready
          ok4 `shouldBe` True
      , it "InTransaction is unreachable from Uninitialized (must initTransactions first)" $ do
          txn <- mkLocalTransaction
          ok <- Txn.transitionState txn Txn.InTransaction
          ok `shouldBe` False
          st <- Txn.getTransactionState txn
          st `shouldBe` Txn.Uninitialized
      , it "Ready cannot jump straight to Ready (must commit/abort)" $ do
          txn <- mkLocalTransaction
          _ <- Txn.transitionState txn Txn.Ready
          _ <- Txn.transitionState txn Txn.InTransaction
          ok <- Txn.transitionState txn Txn.Ready
          ok `shouldBe` False
      , it "producer ID and epoch start unset" $ do
          txn <- mkLocalTransaction
          pid <- readIORef (Txn.txnProducerId txn)
          ep <- readIORef (Txn.txnProducerEpoch txn)
          pid `shouldBe` Nothing
          ep `shouldBe` Nothing
      , it "transactional id is preserved" $ do
          txn <- mkLocalTransaction
          Txn.txnTransactionalId txn
            `shouldBe` Txn.TransactionalId "wireform-conformance-0103"
      , -- Suppress unused-import warning while we wait for HB to come back
        -- in scope (used by other ports).
        it "heartbeat module is importable (symbol smoke)" $ do
          let _ = HB.createHeartbeatState
          pure () :: IO ()
      ]
