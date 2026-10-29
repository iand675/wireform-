{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the @Kafka.Client.Transaction@ ↔
@Kafka.Client.Producer@ wiring.

The end-to-end story (transactional records become visible to a
read-committed consumer iff the producer commits) requires a
live broker and lives in
@test-integration/Integration/BasicSpec.hs@; what we cover here
are the pure pieces that drove the integration:

  * 'producerTxnGate' — the state guard that rejects
    'sendMessage' calls outside @InTransaction@;
  * 'BatchAccumulator.appendRecordStamped' — that a freshly
    created batch gets stamped with the supplied
    'BatchStamp', including the @isTransactional@ bit;
  * 'ProducerSender.buildRecordBatch' — that a batch carrying
    @batchIsTransactional = True@ encodes a wire-level
    'Attributes' word with the transactional bit set.
-}
module Client.ProducerTransactionWiringSpec (tests) where

import Control.Concurrent.STM
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Client.Producer qualified as Producer
import Kafka.Client.Transaction qualified as Txn
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Test.Syd


tests :: Spec
tests =
  describe "Producer ↔ Transaction wiring" $
    sequence_
      [ describe "producerTxnGate" $
          sequence_
            [ gate_uninitialized
            , gate_ready_rejects
            , gate_in_txn_with_pid_passes
            , gate_in_txn_without_pid_rejects
            , gate_fenced
            , gate_aborting
            , gate_error
            ]
      , describe "BatchAccumulator stamping" $
          sequence_
            [ stamped_records_carry_stamp_into_new_batch
            , stamped_records_share_existing_batch_stamp
            , noStamp_matches_default_sentinels
            , distinct_partitions_get_distinct_stamps
            ]
      , describe "RecordBatch transactional flag" $
          sequence_
            [ buildRecordBatch_propagates_isTransactional
            , buildRecordBatch_default_is_non_transactional
            ]
      ]


----------------------------------------------------------------------
-- producerTxnGate
----------------------------------------------------------------------

gate_uninitialized :: Spec
gate_uninitialized = it
  "Uninitialized: rejects with 'must call initTransactions'"
  $ case Producer.producerTxnGate Txn.Uninitialized Nothing Nothing of
    Left msg -> (if ("initTransactions" `infix'` msg) then pure () else expectationFailure ("got: " <> msg))
    Right _ -> expectationFailure "expected rejection"


gate_ready_rejects :: Spec
gate_ready_rejects = it
  "Ready: rejects with 'must call beginTransaction'"
  $ case Producer.producerTxnGate
    Txn.Ready
    (Just (Txn.ProducerId 1))
    (Just (Txn.ProducerEpoch 0)) of
    Left msg -> (if ("beginTransaction" `infix'` msg) then pure () else expectationFailure ("got: " <> msg))
    Right _ -> expectationFailure "expected rejection"


gate_in_txn_with_pid_passes :: Spec
gate_in_txn_with_pid_passes =
  it
    "InTransaction + (pid, epoch): returns the pair to stamp the batch"
    $ Producer.producerTxnGate
      Txn.InTransaction
      (Just (Txn.ProducerId 12345))
      (Just (Txn.ProducerEpoch 7))
      `shouldBe` Right (12345, 7)


gate_in_txn_without_pid_rejects :: Spec
gate_in_txn_without_pid_rejects = it
  "InTransaction + Nothing pid: rejects (initTransactions never ran)"
  $ case Producer.producerTxnGate Txn.InTransaction Nothing Nothing of
    Left _ -> pure ()
    Right _ -> expectationFailure "expected rejection without populated pid/epoch"


gate_fenced :: Spec
gate_fenced = it
  "Fenced: surfaces 'producer fenced'"
  $ case Producer.producerTxnGate
    Txn.Fenced
    (Just (Txn.ProducerId 1))
    (Just (Txn.ProducerEpoch 0)) of
    Left msg -> (if ("fenced" `infix'` msg) then pure () else expectationFailure ("got: " <> msg))
    Right _ -> expectationFailure "expected rejection"


gate_aborting :: Spec
gate_aborting = it
  "Aborting: rejects (commit/abort already in flight)"
  $ case Producer.producerTxnGate
    Txn.Aborting
    (Just (Txn.ProducerId 1))
    (Just (Txn.ProducerEpoch 0)) of
    Left _ -> pure ()
    Right _ -> expectationFailure "expected rejection"


gate_error :: Spec
gate_error = it
  "Error: surfaces the underlying message"
  $ case Producer.producerTxnGate
    (Txn.Error "boom")
    (Just (Txn.ProducerId 1))
    (Just (Txn.ProducerEpoch 0)) of
    Left msg -> (if ("boom" `infix'` msg) then pure () else expectationFailure ("got: " <> msg))
    Right _ -> expectationFailure "expected rejection"


----------------------------------------------------------------------
-- BatchAccumulator stamping
----------------------------------------------------------------------

mkAcc :: IO BA.BatchAccumulator
mkAcc =
  BA.createBatchAccumulator
    16384 -- batch size
    100000 -- linger ms (very high so nothing rolls over by time)
    Compression.NoCompression
    (Compression.defaultLevel Compression.NoCompression)


stamped_records_carry_stamp_into_new_batch :: Spec
stamped_records_carry_stamp_into_new_batch = it
  "appendRecordStamped: a freshly created batch carries the stamp's \
  \(producerId, epoch, baseSeq, isTransactional)"
  $ do
    acc <- mkAcc
    let tp = BA.TopicPartition "t" 0
        stamp =
          BA.BatchStamp
            { BA.stampProducerId = 999
            , BA.stampProducerEpoch = 3
            , BA.stampBaseSequence = 17
            , BA.stampIsTransactional = True
            }
        rec_ = RB.Record 0 0 Nothing (Just "value") []
    ok <- BA.appendRecordStamped acc tp rec_ BA.NoRecordCallback stamp
    ok `shouldBe` True
    BA.closeBatchAccumulator acc
    batches <- BA.drainReadyBatches acc
    case batches of
      [b] -> do
        BA.batchProducerId b `shouldBe` 999
        BA.batchProducerEpoch b `shouldBe` 3
        BA.batchBaseSequence b `shouldBe` 17
        BA.batchIsTransactional b `shouldBe` True
        V.length (BA.batchRecords b) `shouldBe` 1
      _ -> expectationFailure $ "expected exactly one ready batch, got " <> show (length batches)


stamped_records_share_existing_batch_stamp :: Spec
stamped_records_share_existing_batch_stamp = it
  "appendRecordStamped: records appended to a /filling/ batch \
  \inherit that batch's stamp; producer-side seq advancement is the \
  \producer's responsibility"
  $ do
    acc <- mkAcc
    let tp = BA.TopicPartition "t" 0
        stampA = BA.BatchStamp 999 3 17 True
        stampB = BA.BatchStamp 999 3 18 True -- next per-producer seq
        stampC = BA.BatchStamp 999 3 19 True
        rec_ = RB.Record 0 0 Nothing (Just "v") []
    _ <- BA.appendRecordStamped acc tp rec_ BA.NoRecordCallback stampA
    _ <- BA.appendRecordStamped acc tp rec_ BA.NoRecordCallback stampB
    _ <- BA.appendRecordStamped acc tp rec_ BA.NoRecordCallback stampC
    BA.closeBatchAccumulator acc
    batches <- BA.drainReadyBatches acc
    case batches of
      [b] -> do
        -- All three records ended up in the single filling batch;
        -- the batch's base_sequence is whatever the /first/ stamp
        -- carried.
        V.length (BA.batchRecords b) `shouldBe` 3
        BA.batchBaseSequence b `shouldBe` 17
        BA.batchIsTransactional b `shouldBe` True
      other ->
        expectationFailure $
          "expected one batch, got " <> show (length other)


noStamp_matches_default_sentinels :: Spec
noStamp_matches_default_sentinels = it
  "noStamp produces no-producer-id / no-epoch / no-sequence sentinels"
  $ do
    BA.stampProducerId BA.noStamp `shouldBe` RB.noProducerId
    BA.stampProducerEpoch BA.noStamp `shouldBe` RB.noProducerEpoch
    BA.stampBaseSequence BA.noStamp `shouldBe` RB.noSequence
    BA.stampIsTransactional BA.noStamp `shouldBe` False


distinct_partitions_get_distinct_stamps :: Spec
distinct_partitions_get_distinct_stamps = it
  "Stamps on distinct (topic, partition) tracks are independent"
  $ do
    acc <- mkAcc
    let tp1 = BA.TopicPartition "t" 0
        tp2 = BA.TopicPartition "t" 1
        stamp1 = BA.BatchStamp 999 3 0 True
        stamp2 = BA.BatchStamp 999 3 17 True
        rec_ = RB.Record 0 0 Nothing (Just "v") []
    _ <- BA.appendRecordStamped acc tp1 rec_ BA.NoRecordCallback stamp1
    _ <- BA.appendRecordStamped acc tp2 rec_ BA.NoRecordCallback stamp2
    BA.closeBatchAccumulator acc
    batches <- BA.drainReadyBatches acc
    -- One batch per partition; both should be transactional with their
    -- own base sequence.
    let baseSeqs =
          Seq.sort $
            Seq.fromList [BA.batchBaseSequence b | b <- batches]
    toList baseSeqs `shouldBe` [0, 17]
    (all BA.batchIsTransactional batches) `shouldBe` True


----------------------------------------------------------------------
-- buildRecordBatch
----------------------------------------------------------------------

mkBatchWith :: Bool -> BA.ProducerBatch
mkBatchWith isTxn =
  BA.ProducerBatch
    { BA.batchTopicPartition = BA.TopicPartition "t" 0
    , BA.batchRecords =
        V.fromList
          [RB.Record 0 0 Nothing (Just "v") []]
    , BA.batchSizeBytes = 1
    , BA.batchCreateTime = 0
    , BA.batchBaseTimestamp = 0
    , BA.batchState = BA.Ready
    , BA.batchCompression = Compression.NoCompression
    , BA.batchCompressionLevel =
        Compression.defaultLevel Compression.NoCompression
    , BA.batchCallbacks = V.empty
    , BA.batchAttempts = 0
    , BA.batchProducerId = if isTxn then 12345 else RB.noProducerId
    , BA.batchProducerEpoch = if isTxn then 7 else RB.noProducerEpoch
    , BA.batchBaseSequence = if isTxn then 42 else RB.noSequence
    , BA.batchIsTransactional = isTxn
    }


buildRecordBatch_propagates_isTransactional :: Spec
buildRecordBatch_propagates_isTransactional = it
  "buildRecordBatch: batchIsTransactional flips attrIsTransactional \
  \and the producer-id / epoch / base-seq carry through"
  $ do
    let rb = Sender.buildRecordBatch (mkBatchWith True)
    RB.attrIsTransactional (RB.batchAttributes rb) `shouldBe` True
    RB.batchProducerId rb `shouldBe` 12345
    RB.batchProducerEpoch rb `shouldBe` 7
    RB.batchBaseSequence rb `shouldBe` 42


buildRecordBatch_default_is_non_transactional :: Spec
buildRecordBatch_default_is_non_transactional = it
  "buildRecordBatch: default batch encodes attrIsTransactional = False \
  \and the no-producer-id / no-epoch / no-seq sentinels"
  $ do
    let rb = Sender.buildRecordBatch (mkBatchWith False)
    RB.attrIsTransactional (RB.batchAttributes rb) `shouldBe` False
    RB.batchProducerId rb `shouldBe` RB.noProducerId
    RB.batchProducerEpoch rb `shouldBe` RB.noProducerEpoch
    RB.batchBaseSequence rb `shouldBe` RB.noSequence


----------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------

{- | Tiny @isInfixOf@ for 'String' so the tests don't need
'Data.List.Extra' / 'Data.Text'.
-}
infix' :: String -> String -> Bool
infix' needle haystack =
  any
    (\i -> take (length needle) (drop i haystack) == needle)
    [0 .. length haystack - length needle]
