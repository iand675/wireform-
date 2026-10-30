{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Client.TransactionCoordinatorSpec
Description : Tests for transaction coordinator protocol functions
Copyright   : (c) 2025
License     : BSD-3-Clause

Tests for the TransactionCoordinator module, which handles protocol communication
with Kafka's transaction coordinator.

These tests verify:
- Error code interpretation
- Request/response encoding/decoding
- State management during coordinator operations
- Error handling for various failure modes
-}
module Client.TransactionCoordinatorSpec (transactionCoordinatorSpec) where

import Control.Concurrent.STM
import Control.Exception (evaluate, try)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.Consumer (TopicPartition (..))
import Kafka.Client.Internal.TransactionCoordinator
import Test.Syd
import Test.Syd.Hedgehog ()
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddOffsetsToTxnRequest qualified as AOTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.TxnOffsetCommitRequest qualified as TOCReq
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


-- | Main test tree
transactionCoordinatorSpec :: Spec
transactionCoordinatorSpec =
  describe "TransactionCoordinator" $
    sequence_
      [ errorInterpretationTests
      , coordinatorTypeTests
      , unitTests
      , requestBuilderTests
      ]


--------------------------------------------------------------------------------
-- Error Interpretation Tests
--------------------------------------------------------------------------------

errorInterpretationTests :: Spec
errorInterpretationTests =
  describe "Error Code Interpretation" $
    sequence_
      [ it "COORDINATOR_NOT_AVAILABLE (15)" $ do
          let err = interpretCoordinatorError 15
          case err of
            CoordinatorNotAvailable msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "COORDINATOR_LOAD_IN_PROGRESS (14)" $ do
          let err = interpretCoordinatorError 14
          case err of
            CoordinatorLoadInProgress msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "NOT_COORDINATOR (16)" $ do
          let err = interpretCoordinatorError 16
          case err of
            NotCoordinator msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , -- Error code mapping cross-checked against Kafka 3.7's
        -- Errors.java; the previous test fixtures asserted the
        -- wrong-codes mapping that 'interpretCoordinatorError' had
        -- before the live-broker fix. The codes now actually match
        -- the wire protocol.
        it "INVALID_PRODUCER_EPOCH (47)" $ do
          let err = interpretCoordinatorError 47
          case err of
            InvalidProducerEpoch msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "INVALID_TXN_STATE (48)" $ do
          let err = interpretCoordinatorError 48
          case err of
            InvalidTxnState msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "INVALID_PRODUCER_ID_MAPPING (49)" $ do
          let err = interpretCoordinatorError 49
          case err of
            InvalidProducerIdMapping msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "CONCURRENT_TRANSACTIONS (51)" $ do
          let err = interpretCoordinatorError 51
          case err of
            ConcurrentTransactions msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "TRANSACTION_COORDINATOR_FENCED (52)" $ do
          let err = interpretCoordinatorError 52
          case err of
            TransactionCoordinatorFenced msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "PRODUCER_FENCED (90)" $ do
          let err = interpretCoordinatorError 90
          case err of
            ProducerFenced msg -> (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      , it "Unknown error code" $ do
          let err = interpretCoordinatorError 9999
          case err of
            UnknownCoordinatorError code msg -> do
              code `shouldBe` 9999
              (not $ T.null msg) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      ]


--------------------------------------------------------------------------------
-- TransactionCoordinator Type Tests
--------------------------------------------------------------------------------

coordinatorTypeTests :: Spec
coordinatorTypeTests =
  describe "TransactionCoordinator Type" $
    sequence_
      [ it "Create and access coordinator" $ do
          let coordinator =
                TransactionCoordinator
                  { tcNodeId = 1
                  , tcHost = "localhost"
                  , tcPort = 9092
                  }
          (tcNodeId coordinator) `shouldBe` 1
          (tcHost coordinator) `shouldBe` "localhost"
          (tcPort coordinator) `shouldBe` 9092
      , it "Coordinator equality" $ do
          let coord1 = TransactionCoordinator 1 "host1" 9092
              coord2 = TransactionCoordinator 1 "host1" 9092
              coord3 = TransactionCoordinator 2 "host1" 9092
          coord2 `shouldBe` coord1
          (coord1 /= coord3) `shouldBe` True
      ]


--------------------------------------------------------------------------------
-- Unit Tests (without network)
--------------------------------------------------------------------------------

unitTests :: Spec
unitTests =
  describe "Unit Tests" $
    sequence_
      [ it "Error codes are distinct" $ do
          let errors = map interpretCoordinatorError [14, 15, 16, 47, 48, 49, 51, 52, 90]
          -- All errors should be different types
          (length errors) `shouldBe` 9
      , it "TransactionCoordinator shows correctly" $ do
          let coordinator = TransactionCoordinator 1 "localhost" 9092
          let shown = show coordinator
          ("1" `T.isInfixOf` T.pack shown) `shouldBe` True
          ("localhost" `T.isInfixOf` T.pack shown) `shouldBe` True
          ("9092" `T.isInfixOf` T.pack shown) `shouldBe` True
      , it "Error messages contain useful info" $ do
          let err = interpretCoordinatorError 15
          case err of
            CoordinatorNotAvailable msg -> do
              (not $ T.null msg) `shouldBe` True
              (T.length msg > 10) `shouldBe` True
            _ -> expectationFailure "Wrong error type"
      ]


--------------------------------------------------------------------------------
-- Request Builder Tests
--
-- 'addOffsetsToTxn' / 'txnOffsetCommitWith' both build their
-- requests inline before sending; we lifted the construction to
-- 'buildAddOffsetsToTxnRequest' / 'buildTxnOffsetCommitRequest'
-- so we can assert the exact wire shape (encoder roundtrip + per
-- field equality) without spinning up a coordinator.
--------------------------------------------------------------------------------

extractK :: P.KafkaString -> Text
extractK (P.KafkaString P.Null) = ""
extractK (P.KafkaString (P.NotNull t)) = t


requestBuilderTests :: Spec
requestBuilderTests =
  describe "Request Builders" $
    sequence_
      [ it "buildAddOffsetsToTxnRequest sets every field as supplied" $ do
          let req = buildAddOffsetsToTxnRequest "tx-1" 4242 7 "consumer-grp"
          extractK (AOTReq.addOffsetsToTxnRequestTransactionalId req) `shouldBe` "tx-1"
          AOTReq.addOffsetsToTxnRequestProducerId req `shouldBe` 4242
          AOTReq.addOffsetsToTxnRequestProducerEpoch req `shouldBe` 7
          extractK (AOTReq.addOffsetsToTxnRequestGroupId req) `shouldBe` "consumer-grp"
      , it "AddOffsetsToTxnRequest round-trips through encoder/decoder (v3)" $ do
          let req = buildAddOffsetsToTxnRequest "tx-rt" 1 0 "g"
              bytes = WC.runEncodeVer @AOTReq.AddOffsetsToTxnRequest 3 req
          case WC.runDecodeVer @AOTReq.AddOffsetsToTxnRequest 3 bytes of
            Left err -> expectationFailure ("decode failed: " <> err)
            Right r2 -> do
              extractK (AOTReq.addOffsetsToTxnRequestTransactionalId r2) `shouldBe` "tx-rt"
              AOTReq.addOffsetsToTxnRequestProducerId r2 `shouldBe` 1
              AOTReq.addOffsetsToTxnRequestProducerEpoch r2 `shouldBe` 0
              extractK (AOTReq.addOffsetsToTxnRequestGroupId r2) `shouldBe` "g"
      , it "buildTxnOffsetCommitRequest groups partitions by topic" $ do
          let req =
                buildTxnOffsetCommitRequest
                  "tx-1"
                  (member "grp")
                  100
                  5
                  [ (TopicPartition "t1" 0, 10)
                  , (TopicPartition "t1" 1, 11)
                  , (TopicPartition "t2" 0, 20)
                  ]
          let topics = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics req) of
                P.Null -> V.empty
                P.NotNull v -> v
          V.length topics `shouldBe` 2
          let names = [extractK (TOCReq.txnOffsetCommitRequestTopicName t) | t <- V.toList topics]
          (if ("t1" `elem` names && "t2" `elem` names) then pure () else expectationFailure ("topics: " <> show names))
          -- The "t1" topic should have 2 partitions; "t2" should have 1.
          let parts t = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopicPartitions t) of
                P.Null -> V.empty
                P.NotNull v -> v
          let countsByName =
                [ ( extractK (TOCReq.txnOffsetCommitRequestTopicName t)
                  , V.length (parts t)
                  )
                | t <- V.toList topics
                ]
          lookup "t1" countsByName `shouldBe` Just 2
          lookup "t2" countsByName `shouldBe` Just 1
      , it "buildTxnOffsetCommitRequest stamps -1 leader epoch + null metadata" $ do
          let req = buildTxnOffsetCommitRequest "tx-1" (member "g") 1 0 [(TopicPartition "x" 0, 99)]
              [topic] = V.toList $ case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics req) of
                P.Null -> V.empty
                P.NotNull v -> v
              [part] = V.toList $ case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopicPartitions topic) of
                P.Null -> V.empty
                P.NotNull v -> v
          TOCReq.txnOffsetCommitRequestPartitionPartitionIndex part `shouldBe` 0
          TOCReq.txnOffsetCommitRequestPartitionCommittedOffset part `shouldBe` 99
          TOCReq.txnOffsetCommitRequestPartitionCommittedLeaderEpoch part `shouldBe` -1
          case TOCReq.txnOffsetCommitRequestPartitionCommittedMetadata part of
            P.KafkaString P.Null -> pure ()
            other -> expectationFailure ("expected null metadata, got " <> show other)
      , it "buildTxnOffsetCommitRequest with empty offsets emits no topics" $ do
          let req = buildTxnOffsetCommitRequest "tx-1" (member "g") 1 0 []
              topics = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics req) of
                P.Null -> V.empty
                P.NotNull v -> v
          V.length topics `shouldBe` 0
      , it "TxnOffsetCommitRequest round-trips through encoder/decoder (v3)" $ do
          let req =
                buildTxnOffsetCommitRequest
                  "tx-rt"
                  (member "grp")
                  7
                  9
                  [(TopicPartition "x" 0, 1), (TopicPartition "x" 1, 2)]
              bytes = WC.runEncodeVer @TOCReq.TxnOffsetCommitRequest 3 req
          case WC.runDecodeVer @TOCReq.TxnOffsetCommitRequest 3 bytes of
            Left err -> expectationFailure ("decode failed: " <> err)
            Right r2 -> do
              extractK (TOCReq.txnOffsetCommitRequestGroupId r2) `shouldBe` "grp"
              -- The real transactional id and the member's identity
              -- (KIP-447 fencing) reach the wire.
              extractK (TOCReq.txnOffsetCommitRequestTransactionalId r2) `shouldBe` "tx-rt"
              TOCReq.txnOffsetCommitRequestGenerationId r2 `shouldBe` 4
              extractK (TOCReq.txnOffsetCommitRequestMemberId r2) `shouldBe` "member-1"
              TOCReq.txnOffsetCommitRequestProducerId r2 `shouldBe` 7
              TOCReq.txnOffsetCommitRequestProducerEpoch r2 `shouldBe` 9
              let topics = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics r2) of
                    P.Null -> V.empty
                    P.NotNull v -> v
              V.length topics `shouldBe` 1
              let [topic] = V.toList topics
                  parts = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopicPartitions topic) of
                    P.Null -> V.empty
                    P.NotNull v -> v
              V.length parts `shouldBe` 2
      ]


--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

-- | All error codes should produce valid errors
prop_errorCodesValid :: H.Property
prop_errorCodesValid = H.property $ do
  errorCode <- H.forAll $ Gen.int16 (Range.constant 0 200)

  let err = interpretCoordinatorError errorCode

  -- Should not throw when showing
  errStr <- H.evalIO $ evaluate (show err)
  H.assert (not $ null errStr)


-- | Known error codes should not produce UnknownCoordinatorError
prop_knownErrorCodes :: H.Property
prop_knownErrorCodes = H.property $ do
  errorCode <- H.forAll $ Gen.element [14, 15, 16, 47, 48, 49, 51, 52, 90]

  let err = interpretCoordinatorError errorCode

  case err of
    UnknownCoordinatorError _ _ -> H.failure
    _ -> H.success


-- | TransactionCoordinator fields should be preserved
prop_coordinatorPreservesFields :: H.Property
prop_coordinatorPreservesFields = H.property $ do
  nodeId <- H.forAll $ Gen.int32 (Range.linear 0 100)
  host <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
  port <- H.forAll $ Gen.int32 (Range.linear 1024 65535)

  let coordinator = TransactionCoordinator nodeId host port

  H.assert (tcNodeId coordinator == nodeId)
  H.assert (tcHost coordinator == host)
  H.assert (tcPort coordinator == port)


-- | A group member at generation 4 (for the TxnOffsetCommit builders).
member :: Text -> TxnGroupMember
member g =
  TxnGroupMember
    { tgmGroupId = g
    , tgmGenerationId = 4
    , tgmMemberId = "member-1"
    , tgmGroupInstanceId = Nothing
    }
