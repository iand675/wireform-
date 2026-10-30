{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.Examples.Transaction
Description : Producer + transactional commit, the five-step recipe.

Mirrors the canonical JVM transactional-producer setup but in the
Haskell client's idiom:

  1. Configure the producer with a transactional id + idempotence on.
  2. Build the coordinator handle via 'Transaction.createTransaction'.
  3. 'initTransactions' once per process to fence any zombie holders
     of the same transactional id.
  4. Bind the producer to the transaction, then begin / send / commit.
  5. Repeat (4) for the next batch. One initialised transaction
     handle supports many begin/commit cycles.

> cabal run wireform-kafka-client-examples transaction
-}
module Kafka.Client.Examples.Transaction (runDemo) where

import Kafka qualified
import Kafka.Client.Transaction qualified as Transaction
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV


runDemo :: IO ()
runDemo = do
  let txnId = "demo-app-1"
  Kafka.withProducer
    ["localhost:9092"]
    Kafka.defaultProducerConfig
      { Kafka.producerTransactional = Just txnId
      , Kafka.producerIdempotent = True
      }
    $ \p -> do
      connMgr <- Conn.createConnectionManager
      vCache <- AV.createVersionCache
      txn <-
        Transaction.createTransaction
          (Transaction.TransactionalId txnId)
          connMgr
          Conn.defaultConnectionConfig
          vCache
          "demo-client"
          (Conn.BrokerAddress "localhost" 9092)
          60_000
      Right () <- Transaction.initTransactions txn
      Kafka.bindTransaction p txn

      Right () <- Transaction.beginTransaction txn
      _ <- Kafka.sendMessage p "events" Nothing "in-txn"
      Right () <- Transaction.commitTransaction txn
      putStrLn "transaction committed"
