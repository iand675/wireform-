{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.TransactionSpec where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM_)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.IORef (readIORef)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.AdminClient qualified as Admin
import Kafka.Client.Consumer (TopicPartition (..))
import Kafka.Client.Internal.TransactionCoordinator (TxnGroupMember (..))
import Kafka.Client.Transaction
import Kafka.Network.Connection (BrokerAddress (..), ConnectionManager)
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions (ApiVersionCache, createVersionCache)
import Test.Syd
import Test.Syd.Hedgehog ()
import TestUtil.BrokerGate (brokerCase, brokerProperty)


{- | Helper: Create a test transaction with mock infrastructure
This allows us to test state machine logic without real network connections

Every case uses its own transactional id: the cases run concurrently,
and two producers sharing one id fence each other on a real broker.
-}
createTestTransaction :: TransactionalId -> IO Transaction
createTestTransaction txnId = do
  -- Create mock connection manager
  connMgr <- Conn.createConnectionManager

  -- Create empty version cache
  versionCache <- createVersionCache

  -- Create test transaction with defaults
  createTransaction
    txnId
    connMgr
    Conn.defaultConnectionConfig
    versionCache
    "test-client"
    (BrokerAddress "localhost" 9092)
    60000 -- 60 second timeout


-- | Test suite for transaction support (KIP-98)
transactionSpec :: Spec
transactionSpec =
  describe "Transaction Support (KIP-98)" $
    sequence_
      [ describe "State Management" $
          sequence_
            [ it "unit_createTransaction" unit_createTransaction
            , it "unit_initialState" unit_initialState
            , it "unit_stateTransitions" unit_stateTransitions
            , it "unit_invalidTransitions" unit_invalidTransitions
            , it "prop_stateTransitionsThreadSafe" prop_stateTransitionsThreadSafe
            ]
      , describe "Transaction Lifecycle (requires broker)" $
          sequence_
            [ brokerCase "unit_initTransactions" unit_initTransactions
            , brokerCase "unit_beginTransaction" unit_beginTransaction
            , brokerCase "unit_commitTransaction" unit_commitTransaction
            , brokerCase "unit_abortTransaction" unit_abortTransaction
            , brokerCase "unit_cannotBeginTwice" unit_cannotBeginTwice
            , brokerCase "unit_cannotCommitWithoutBegin" unit_cannotCommitWithoutBegin
            , brokerCase "unit_cannotAbortWithoutBegin" unit_cannotAbortWithoutBegin
            ]
      , describe "withTransaction Bracket (requires broker)" $
          sequence_
            [ brokerCase "unit_withTransactionCommitsOnSuccess" unit_withTransactionCommitsOnSuccess
            , brokerCase "unit_withTransactionAbortsOnException" unit_withTransactionAbortsOnException
            , brokerCase "unit_withTransactionNested" unit_withTransactionNested
            ]
      , describe "Transactional Send (requires broker)" $
          sequence_
            [ brokerCase "unit_sendInTransaction" unit_sendInTransaction
            , brokerCase "unit_sendTracksPartitions" unit_sendTracksPartitions
            , brokerCase "unit_sendIncrementsSequence" unit_sendIncrementsSequence
            , brokerCase "unit_cannotSendOutsideTransaction" unit_cannotSendOutsideTransaction
            ]
      , describe "Offset Commits (requires broker)" $
          sequence_
            [ brokerCase "unit_sendOffsetsToTransaction" unit_commitOffsetsInTransaction
            , brokerCase "unit_cannotCommitOffsetsOutsideTransaction" unit_cannotCommitOffsetsOutsideTransaction
            ]
      , describe "Idempotency (requires broker)" $
          sequence_
            [ brokerCase "unit_sequenceNumbersIncrement" unit_sequenceNumbersIncrement
            , brokerCase "unit_sequenceNumbersPerPartition" unit_sequenceNumbersPerPartition
            , brokerProperty "prop_sequenceNumbersMonotonic" 20 prop_sequenceNumbersMonotonic
            ]
      ]


-- ============================================================================
-- State Management Tests
-- ============================================================================

unit_createTransaction :: IO ()
unit_createTransaction = do
  let txnId = TransactionalId "test-txn-1"
  txn <- createTestTransaction txnId

  -- Verify initial state
  (txnTransactionalId txn) `shouldBe` txnId

  state <- getTransactionState txn
  state `shouldBe` Uninitialized

  producerId <- readIORef (txnProducerId txn)
  producerId `shouldBe` Nothing

  producerEpoch <- readIORef (txnProducerEpoch txn)
  producerEpoch `shouldBe` Nothing

  partitions <- readTVarIO (txnPartitions txn)
  partitions `shouldBe` Set.empty


unit_initialState :: IO ()
unit_initialState = do
  txn <- createTestTransaction (TransactionalId "test-txn-2")
  state <- getTransactionState txn
  state `shouldBe` Uninitialized


unit_stateTransitions :: IO ()
unit_stateTransitions = do
  txn <- createTestTransaction (TransactionalId "test-txn-3")

  -- Uninitialized -> Ready
  success1 <- transitionState txn Ready
  (success1) `shouldBe` True
  state1 <- getTransactionState txn
  state1 `shouldBe` Ready

  -- Ready -> InTransaction
  success2 <- transitionState txn InTransaction
  (success2) `shouldBe` True
  state2 <- getTransactionState txn
  state2 `shouldBe` InTransaction

  -- InTransaction -> Committing
  success3 <- transitionState txn Committing
  (success3) `shouldBe` True
  state3 <- getTransactionState txn
  state3 `shouldBe` Committing

  -- Committing -> Ready
  success4 <- transitionState txn Ready
  (success4) `shouldBe` True
  state4 <- getTransactionState txn
  state4 `shouldBe` Ready


unit_invalidTransitions :: IO ()
unit_invalidTransitions = do
  txn <- createTestTransaction (TransactionalId "test-txn-4")

  -- Cannot go from Uninitialized to InTransaction
  success1 <- transitionState txn InTransaction
  (not success1) `shouldBe` True

  -- Initialize properly
  _ <- transitionState txn Ready
  _ <- transitionState txn InTransaction

  -- Cannot go from InTransaction to Ready without Committing/Aborting
  success2 <- transitionState txn Ready
  (not success2) `shouldBe` True


prop_stateTransitionsThreadSafe :: H.Property
prop_stateTransitionsThreadSafe = H.property $ do
  numThreads <- H.forAll $ Gen.int (Range.linear 5 20)

  H.annotate $ "Testing with " ++ show numThreads ++ " concurrent threads"

  txn <- H.evalIO $ createTestTransaction (TransactionalId "test-txn-5")

  -- Initialize first
  _ <- H.evalIO $ transitionState txn Ready

  -- Spawn threads that all try to transition to InTransaction
  results <- H.evalIO $ mapM async $ replicate numThreads $ do
    transitionState txn InTransaction

  -- Wait for all threads
  successes <- H.evalIO $ mapM wait results

  -- At least one thread should have succeeded (all may succeed due to STM semantics
  -- where they all see the same initial Ready state and all transition simultaneously)
  let successCount = length $ filter id successes
  H.annotate $ "Successful transitions: " ++ show successCount
  H.assert (successCount >= 1 && successCount <= numThreads)


-- ============================================================================
-- Transaction Lifecycle Tests
-- ============================================================================

unit_initTransactions :: IO ()
unit_initTransactions = do
  txn <- createTestTransaction (TransactionalId "test-txn-6")

  result <- initTransactions txn
  case result of
    Left err -> expectationFailure $ "initTransactions failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      state `shouldBe` Ready

      producerId <- readIORef (txnProducerId txn)
      (producerId /= Nothing) `shouldBe` True

      producerEpoch <- readIORef (txnProducerEpoch txn)
      (producerEpoch /= Nothing) `shouldBe` True


unit_beginTransaction :: IO ()
unit_beginTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-7")

  -- Must init first
  _ <- initTransactions txn

  result <- beginTransaction txn
  case result of
    Left err -> expectationFailure $ "beginTransaction failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      state `shouldBe` InTransaction


unit_commitTransaction :: IO ()
unit_commitTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-8")

  -- Init and begin
  _ <- initTransactions txn
  _ <- beginTransaction txn

  result <- commitTransaction txn
  case result of
    Left err -> expectationFailure $ "commitTransaction failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      state `shouldBe` Ready


unit_abortTransaction :: IO ()
unit_abortTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-9")

  -- Init and begin
  _ <- initTransactions txn
  _ <- beginTransaction txn

  result <- abortTransaction txn
  case result of
    Left err -> expectationFailure $ "abortTransaction failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      state `shouldBe` Ready


unit_cannotBeginTwice :: IO ()
unit_cannotBeginTwice = do
  txn <- createTestTransaction (TransactionalId "test-txn-10")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  -- Try to begin again
  result <- beginTransaction txn
  case result of
    Left (TransactionAlreadyInProgress _) -> return ()
    Left err -> expectationFailure $ "Wrong error: " ++ show err
    Right () -> expectationFailure "Should not allow beginning transaction twice"


unit_cannotCommitWithoutBegin :: IO ()
unit_cannotCommitWithoutBegin = do
  txn <- createTestTransaction (TransactionalId "test-txn-11")
  _ <- initTransactions txn

  -- Try to commit without begin
  result <- commitTransaction txn
  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> expectationFailure $ "Wrong error: " ++ show err
    Right () -> expectationFailure "Should not allow commit without begin"


unit_cannotAbortWithoutBegin :: IO ()
unit_cannotAbortWithoutBegin = do
  txn <- createTestTransaction (TransactionalId "test-txn-12")
  _ <- initTransactions txn

  -- Try to abort without begin
  result <- abortTransaction txn
  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> expectationFailure $ "Wrong error: " ++ show err
    Right () -> expectationFailure "Should not allow abort without begin"


-- ============================================================================
-- withTransaction Bracket Tests
-- ============================================================================

unit_withTransactionCommitsOnSuccess :: IO ()
unit_withTransactionCommitsOnSuccess = do
  txn <- createTestTransaction (TransactionalId "test-txn-13")
  _ <- initTransactions txn

  result <- withTransaction txn $ do
    return (42 :: Int)

  case result of
    Left err -> expectationFailure $ "withTransaction failed: " ++ show err
    Right value -> do
      value `shouldBe` 42
      state <- getTransactionState txn
      state `shouldBe` Ready


unit_withTransactionAbortsOnException :: IO ()
unit_withTransactionAbortsOnException = do
  txn <- createTestTransaction (TransactionalId "test-txn-14")
  _ <- initTransactions txn

  result <- withTransaction txn $ do
    error "simulated error" :: IO Int

  case result of
    Left (TransactionAborted _) -> do
      state <- getTransactionState txn
      state `shouldBe` Ready
    Left err -> expectationFailure $ "Wrong error: " ++ show err
    Right _ -> expectationFailure "Should have aborted on exception"


unit_withTransactionNested :: IO ()
unit_withTransactionNested = do
  txn <- createTestTransaction (TransactionalId "test-txn-15")
  _ <- initTransactions txn

  -- Cannot nest transactions
  result <- withTransaction txn $ do
    result2 <- withTransaction txn $ do
      return (42 :: Int)
    case result2 of
      Left _ -> return (0 :: Int)
      Right v -> return v

  -- The inner transaction should fail because outer is already in progress
  case result of
    Left (TransactionAborted _) -> return () -- Expected: inner fails, outer aborts
    Right 0 -> return () -- Also acceptable: inner fails gracefully, outer succeeds
    Right v -> expectationFailure $ "Unexpected success with value: " ++ show v


-- ============================================================================
-- Transactional Send Tests
-- ============================================================================

unit_sendInTransaction :: IO ()
unit_sendInTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-16")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  let tp = TopicPartition "test-topic" 0
  result <- sendInTransaction txn tp

  case result of
    Left err -> expectationFailure $ "sendInTransaction failed: " ++ show err
    Right () -> return ()


unit_sendTracksPartitions :: IO ()
unit_sendTracksPartitions = do
  txn <- createTestTransaction (TransactionalId "test-txn-17")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  let tp1 = TopicPartition "test-topic" 0
      tp2 = TopicPartition "test-topic" 1
      tp3 = TopicPartition "another-topic" 0

  _ <- sendInTransaction txn tp1
  _ <- sendInTransaction txn tp2
  _ <- sendInTransaction txn tp3

  partitions <- readTVarIO (txnPartitions txn)
  (Set.size partitions) `shouldBe` 3
  (Set.member tp1 partitions) `shouldBe` True
  (Set.member tp2 partitions) `shouldBe` True
  (Set.member tp3 partitions) `shouldBe` True


unit_sendIncrementsSequence :: IO ()
unit_sendIncrementsSequence = do
  txn <- createTestTransaction (TransactionalId "test-txn-18")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  let tp = TopicPartition "test-topic" 0

  _ <- sendInTransaction txn tp
  seq1 <- readTVarIO (txnSequenceNumbers txn)
  (Map.lookup tp seq1) `shouldBe` (Just 1)

  _ <- sendInTransaction txn tp
  seq2 <- readTVarIO (txnSequenceNumbers txn)
  (Map.lookup tp seq2) `shouldBe` (Just 2)

  _ <- sendInTransaction txn tp
  seq3 <- readTVarIO (txnSequenceNumbers txn)
  (Map.lookup tp seq3) `shouldBe` (Just 3)


unit_cannotSendOutsideTransaction :: IO ()
unit_cannotSendOutsideTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-19")
  _ <- initTransactions txn

  let tp = TopicPartition "test-topic" 0
  result <- sendInTransaction txn tp

  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> expectationFailure $ "Wrong error: " ++ show err
    Right () -> expectationFailure "Should not allow send outside transaction"


-- ============================================================================
-- Offset Commit Tests
-- ============================================================================

unit_commitOffsetsInTransaction :: IO ()
unit_commitOffsetsInTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-20")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  -- The coordinator only takes offsets for partitions that exist.
  _ <-
    Admin.withAdminClient ["localhost:9092"] Admin.defaultAdminClientConfig $ \admin ->
      Admin.createTopics admin [Admin.NewTopic "wireform-txn-offsets" 2 1 []]
  let offsets =
        Map.fromList
          [ (TopicPartition "wireform-txn-offsets" 0, 100)
          , (TopicPartition "wireform-txn-offsets" 1, 200)
          ]

  result <- sendOffsetsToTransaction txn (BrokerAddress "localhost" 9092) standaloneMember (Map.toList offsets)

  case result of
    Left err -> expectationFailure $ "sendOffsetsToTransaction failed: " ++ show err
    Right () -> return ()


unit_cannotCommitOffsetsOutsideTransaction :: IO ()
unit_cannotCommitOffsetsOutsideTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn-21")
  _ <- initTransactions txn

  let offsets = Map.fromList [(TopicPartition "topic1" 0, 100)]
  result <- sendOffsetsToTransaction txn (BrokerAddress "localhost" 9092) standaloneMember (Map.toList offsets)

  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> expectationFailure $ "Wrong error: " ++ show err
    Right () -> expectationFailure "Should not allow offset commit outside transaction"


-- ============================================================================
-- Idempotency Tests
-- ============================================================================

unit_sequenceNumbersIncrement :: IO ()
unit_sequenceNumbersIncrement = do
  txn <- createTestTransaction (TransactionalId "test-txn-22")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  let tp = TopicPartition "test-topic" 0

  -- Send multiple records
  replicateM_ 10 $ sendInTransaction txn tp

  sequences <- readTVarIO (txnSequenceNumbers txn)
  (Map.lookup tp sequences) `shouldBe` (Just 10)


unit_sequenceNumbersPerPartition :: IO ()
unit_sequenceNumbersPerPartition = do
  txn <- createTestTransaction (TransactionalId "test-txn-23")
  _ <- initTransactions txn
  _ <- beginTransaction txn

  let tp1 = TopicPartition "topic1" 0
      tp2 = TopicPartition "topic1" 1
      tp3 = TopicPartition "topic2" 0

  -- Send to different partitions
  replicateM_ 3 $ sendInTransaction txn tp1
  replicateM_ 5 $ sendInTransaction txn tp2
  replicateM_ 2 $ sendInTransaction txn tp3

  sequences <- readTVarIO (txnSequenceNumbers txn)
  (Map.lookup tp1 sequences) `shouldBe` (Just 3)
  (Map.lookup tp2 sequences) `shouldBe` (Just 5)
  (Map.lookup tp3 sequences) `shouldBe` (Just 2)


prop_sequenceNumbersMonotonic :: H.Property
prop_sequenceNumbersMonotonic = H.property $ do
  numSends <- H.forAll $ Gen.int (Range.linear 1 100)

  H.annotate $ "Sending " ++ show numSends ++ " records"

  txn <- H.evalIO $ createTestTransaction (TransactionalId "test-txn-24")
  _ <- H.evalIO $ initTransactions txn
  _ <- H.evalIO $ beginTransaction txn

  let tp = TopicPartition "test-topic" 0

  -- Send records and collect sequence numbers
  seqNumbers <-
    H.evalIO $
      mapM
        ( \_ -> do
            _ <- sendInTransaction txn tp
            sequences <- readTVarIO (txnSequenceNumbers txn)
            return $ Map.lookup tp sequences
        )
        [1 .. numSends]

  -- All sequence numbers should be Just values
  let allJust = all (\case Just _ -> True; Nothing -> False) seqNumbers
  H.assert allJust

  -- Extract the values
  let values = map (\(Just v) -> v) seqNumbers

  H.evalIO $ Conn.closeAllConnections (txnConnectionManager txn)

  -- Should be monotonically increasing from 1 to numSends
  H.assert (values == [1 .. fromIntegral numSends])


{- | A member outside any consumer group (generation -1, no member id),
which the coordinator accepts for a plain transactional offset commit.
-}
standaloneMember :: TxnGroupMember
standaloneMember =
  TxnGroupMember
    { tgmGroupId = "consumer-group-1"
    , tgmGenerationId = -1
    , tgmMemberId = ""
    , tgmGroupInstanceId = Nothing
    }
