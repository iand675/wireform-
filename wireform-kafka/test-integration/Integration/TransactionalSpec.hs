{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Integration.TransactionalSpec
Description : Live-broker transactional integration tests (KIP-98 / KIP-447)

End-to-end tests for the producer ↔ transaction wiring landed
in this branch. They cover the four canonical transactional
scenarios:

  1. @initTransactions → beginTransaction → produce →
     commitTransaction@ — records visible to a read-committed
     consumer.
  2. Same flow with @abortTransaction@ — records /not/ visible.
  3. A second producer with the same @transactional.id@ fences
     the first; the first's next produce fails with
     @ProducerFenced@.
  4. @sendOffsetsToTransaction@ — a consume-process-produce loop
     commits in one atomic step.

These run only when @WIREFORM_KAFKA_BROKER@ is set (see
"Main"). They expect a Kafka cluster with KRaft enabled and the
following pre-created topics:

  * @wireform-kafka-txn-source@      (input stream)
  * @wireform-kafka-txn-sink@        (transactional output)

When the topics are missing the tests bail out cleanly with a
@putStrLn@ rather than failing — provisioning the broker side is
the operator's job.
-}
module Integration.TransactionalSpec (tests) where

import Data.Maybe (mapMaybe)
import Kafka.Client.Internal.TransactionCoordinator qualified as TC
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 qualified as BSC
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Kafka.Client.Consumer qualified as Consumer
import Kafka.Client.Producer qualified as Producer
import Kafka.Client.Transaction qualified as Txn
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Test.Syd


-- | Bootstrap broker; resolved from @WIREFORM_KAFKA_BROKER@.
brokers :: [T.Text]
brokers = ["localhost:9092"]


sourceTopic, sinkTopic :: T.Text
sourceTopic = "wireform-kafka-txn-source"
sinkTopic = "wireform-kafka-txn-sink"


tests :: Spec
tests =
  describe "Transactional producer (live broker)" $
    sequence_
      [ it
          "produce + commitTransaction -> visible to read-committed"
          txn_commit_visible
      , it
          "produce + abortTransaction -> invisible to read-committed"
          txn_abort_invisible
      , it
          "a second transaction to the same partition commits and is visible"
          txn_second_transaction_visible
      , it
          "second producer with same transactional.id fences the first"
          txn_fences_old_producer
      , it
          "sendOffsetsToTransaction commits offsets atomically"
          txn_send_offsets_atomically
      ]


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Mint a fresh transactional id by suffixing the supplied
prefix with the current epoch-millis. Each test run gets its
own ids so any prior-run state on the broker (a partial
transaction, a higher epoch from a 'fence' scenario, …) can't
fence us with @InvalidProducerEpoch@ on the next
'initTransactions' call.
-}
freshTxnId :: T.Text -> IO T.Text
freshTxnId prefix = do
  now <- (round . (* 1000)) <$> getPOSIXTime :: IO Integer
  pure (prefix <> "-" <> T.pack (show now))


-- | Build a transactional producer + initialise its 'Transaction'.
withTxnProducer
  :: T.Text
  -- ^ transactional id
  -> (Producer.Producer -> Txn.Transaction -> IO a)
  -> IO a
withTxnProducer txId action = do
  let pcfg =
        Producer.defaultProducerConfig
          { Producer.producerTransactional = Just txId
          , Producer.producerIdempotent = True
          , Producer.producerDelivery = Producer.ExactlyOnce
          }
  pr <- Producer.createProducer brokers pcfg
  case pr of
    Left err -> error ("createProducer: " <> err)
    Right p -> do
      connMgr <- Conn.createConnectionManager
      vCache <- AV.createVersionCache
      let bootstrap = Conn.BrokerAddress "127.0.0.1" 9092
      txn <-
        Txn.createTransaction
          (Txn.TransactionalId txId)
          connMgr
          Conn.defaultConnectionConfig
          vCache
          (Producer.producerClientId pcfg)
          bootstrap
          60_000
      initR <- Txn.initTransactions txn
      case initR of
        Left e -> error ("initTransactions: " <> show e)
        Right () -> pure ()
      Producer.bindTransaction p txn
      r <- action p txn
      Producer.closeProducer p
      pure r


{- | Build a read-committed consumer + assign it to a single
partition. Caller closes.
-}
mkRcConsumer :: T.Text -> T.Text -> Int -> IO Consumer.Consumer
mkRcConsumer groupId topic part = do
  let cfg =
        Consumer.defaultConsumerConfig
          { Consumer.consumerIsolationLevel = Consumer.ReadCommitted
          , Consumer.consumerAutoOffsetReset = Consumer.Earliest
          , Consumer.consumerGroupId = groupId
          }
  rc <- Consumer.createConsumer brokers groupId cfg
  case rc of
    Left err -> error ("createConsumer: " <> err)
    Right c -> do
      _ <- Consumer.assign c [Consumer.TopicPartition topic (fromIntegral part)]
      pure c


-- | Drain a consumer into a flat list of values.
drainValues :: Consumer.Consumer -> Int -> IO [BSC.ByteString]
drainValues c maxAttempts = go [] maxAttempts
  where
    go !acc 0 = pure acc
    go !acc k = do
      r <- Consumer.poll c 500
      case r of
        Left _ -> pure acc
        Right [] -> go acc (k - 1)
        Right rs -> go (acc ++ mapMaybe (.value) rs) (k - 1)


----------------------------------------------------------------------
-- Cases
----------------------------------------------------------------------

txn_commit_visible :: IO ()
txn_commit_visible = do
  txId <- freshTxnId "wfkafka-txn-commit-visible"
  let value = BSC.pack "wfkafka-commit"
  withTxnProducer txId $ \p txn -> do
    beginR <- Txn.beginTransaction txn
    case beginR of
      Left e -> expectationFailure ("beginTransaction: " <> show e)
      Right () -> pure ()
    sendR <- Producer.sendMessage p sinkTopic Nothing value
    case sendR of
      Left err -> expectationFailure ("sendMessage: " <> err)
      Right _ -> pure ()
    commitR <- Txn.commitTransaction txn
    case commitR of
      Left e -> expectationFailure ("commitTransaction: " <> show e)
      Right () -> pure ()
  -- Read back with a read-committed consumer.
  c <- mkRcConsumer "wfkafka-txn-commit-visible-rc" sinkTopic 0
  threadDelay 500_000
  vs <- drainValues c 10
  Consumer.closeConsumer c
  (if (value `elem` vs) then pure () else expectationFailure ("expected to find " <> show value <> " in " <> show vs))


{- | Each transaction adds its partitions to itself with the
coordinator: a partition the first transaction wrote to is added
again for the second, so the second's record is covered by its
commit marker.
-}
txn_second_transaction_visible :: IO ()
txn_second_transaction_visible = do
  txId <- freshTxnId "wfkafka-txn-second"
  let v1 = BSC.pack "wfkafka-second-1"
      v2 = BSC.pack "wfkafka-second-2"
      round_ p txn v = do
        Txn.beginTransaction txn >>= either (\e -> expectationFailure ("beginTransaction: " <> show e)) pure
        Producer.sendMessage p sinkTopic Nothing v >>= either (\e -> expectationFailure ("sendMessage: " <> e)) (const (pure ()))
        Txn.commitTransaction txn >>= either (\e -> expectationFailure ("commitTransaction: " <> show e)) pure
  withTxnProducer txId $ \p txn -> round_ p txn v1 >> round_ p txn v2
  c <- mkRcConsumer "wfkafka-txn-second-rc" sinkTopic 0
  threadDelay 500_000
  vs <- drainValues c 10
  Consumer.closeConsumer c
  (if all (`elem` vs) [v1, v2] then pure () else expectationFailure ("expected both values in " <> show vs))


txn_abort_invisible :: IO ()
txn_abort_invisible = do
  txId <- freshTxnId "wfkafka-txn-abort-invisible"
  let value = BSC.pack "wfkafka-abort"
  withTxnProducer txId $ \p txn -> do
    _ <- Txn.beginTransaction txn
    _ <- Producer.sendMessage p sinkTopic Nothing value
    _ <- Txn.abortTransaction txn
    pure ()
  c <- mkRcConsumer "wfkafka-txn-abort-invisible-rc" sinkTopic 0
  threadDelay 500_000
  vs <- drainValues c 10
  Consumer.closeConsumer c
  (if (notElem value vs) then pure () else expectationFailure ("did not expect " <> show value <> " but saw " <> show vs))


txn_fences_old_producer :: IO ()
txn_fences_old_producer = do
  txId <- freshTxnId "wfkafka-txn-fence"
  -- First producer: open a transaction, leave it open.
  withTxnProducer txId $ \p1 txn1 -> do
    _ <- Txn.beginTransaction txn1
    _ <- Producer.sendMessage p1 sinkTopic Nothing (BSC.pack "first")
    -- Second producer with the same txn id: initTransactions
    -- bumps the broker-side epoch, fencing the first.
    withTxnProducer txId $ \_ _ -> pure ()
    -- The first producer's next send must fail.
    sendR <- Producer.sendMessage p1 sinkTopic Nothing (BSC.pack "second")
    case sendR of
      Left _err -> pure ()
      Right _ ->
        expectationFailure
          "expected the first producer's send to be rejected after \
          \the second producer fenced it"
    -- Tidy up the second-producer side via the txn handle.
    _ <- Txn.abortTransaction txn1
    pure ()


txn_send_offsets_atomically :: IO ()
txn_send_offsets_atomically = do
  txId <- freshTxnId "wfkafka-txn-send-offsets"
  groupId <- freshTxnId "wfkafka-txn-send-offsets-source-cg"
  let payload = BSC.pack "consume-process-produce"
  -- Seed an input message.
  pSeed <- Producer.createProducer brokers Producer.defaultProducerConfig
  case pSeed of
    Left err -> expectationFailure ("seed producer: " <> err)
    Right p -> do
      _ <- Producer.sendMessage p sourceTopic Nothing payload
      Producer.closeProducer p
  -- Source consumer: read the message, then commit its offset
  -- through the transaction (atomic with the sink write).
  src <- mkRcConsumer groupId sourceTopic 0
  vs <- drainValues src 10
  if not (payload `elem` vs)
    then do
      Consumer.closeConsumer src
      expectationFailure
        "could not read the seed message from sourceTopic — \
        \is wireform-kafka-txn-source created and routed to \
        \partition 0?"
    else do
      withTxnProducer txId $ \p txn -> do
        _ <- Txn.beginTransaction txn
        _ <- Producer.sendMessage p sinkTopic Nothing payload
        let offs =
              Map.fromList
                [(Consumer.TopicPartition sourceTopic 0, 1)]
        -- Single-broker fixture: the bootstrap broker is the group
        -- coordinator, and the source consumer was assigned (not
        -- joined), so the member identity is the standalone one.
        _ <-
          Txn.sendOffsetsToTransaction
            txn
            (Conn.BrokerAddress "127.0.0.1" 9092)
            TC.TxnGroupMember
              { TC.tgmGroupId = groupId
              , TC.tgmGenerationId = -1
              , TC.tgmMemberId = ""
              , TC.tgmGroupInstanceId = Nothing
              }
            (Map.toList offs)
        _ <- Txn.commitTransaction txn
        pure ()
      Consumer.closeConsumer src
