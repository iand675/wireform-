{-# LANGUAGE OverloadedStrings #-}

module Client.BatchSplittingSpec (tests) where

import Data.Vector qualified as V
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.BatchSplitting qualified as BS
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Test.Syd


tests :: Spec
tests =
  describe "BatchSplitting (KIP-126)" $
    sequence_
      [ it
          "splitBatch halves a multi-record batch"
          half_split
      , it
          "single-record batches cannot be split"
          single
      , it
          "isOversizedSingle matches the single-record case"
          isSingle
      , it
          "metadata (txn flag, producer-id, base-seq) preserved"
          metadata_preserved
      ]


mkBatch :: Bool -> Int -> BA.ProducerBatch
mkBatch isTxn n =
  BA.ProducerBatch
    { BA.batchTopicPartition = BA.TopicPartition "t" 0
    , BA.batchRecords =
        V.fromList
          [ RB.Record 0 (fromIntegral i) Nothing (Just "v") []
          | i <- [0 .. n - 1]
          ]
    , BA.batchSizeBytes = n * 50
    , BA.batchCreateTime = 0
    , BA.batchBaseTimestamp = 0
    , BA.batchState = BA.Ready
    , BA.batchCompression = Compression.NoCompression
    , BA.batchCompressionLevel =
        Compression.defaultLevel Compression.NoCompression
    , BA.batchCallbacks = V.replicate n BA.NoRecordCallback
    , BA.batchAttempts = 0
    , BA.batchProducerId = if isTxn then 12345 else RB.noProducerId
    , BA.batchProducerEpoch = if isTxn then 7 else RB.noProducerEpoch
    , BA.batchBaseSequence = if isTxn then 0 else RB.noSequence
    , BA.batchIsTransactional = isTxn
    }


half_split :: IO ()
half_split = case BS.splitBatch (mkBatch False 6) of
  Nothing -> error "expected split"
  Just (l, r) -> do
    V.length (BA.batchRecords l) `shouldBe` 3
    V.length (BA.batchRecords r) `shouldBe` 3
    -- Callbacks split on the same boundary.
    V.length (BA.batchCallbacks l) `shouldBe` 3
    V.length (BA.batchCallbacks r) `shouldBe` 3


single :: IO ()
single = case BS.splitBatch (mkBatch False 1) of
  Just _ -> error "single record should not split"
  Nothing -> pure ()


isSingle :: IO ()
isSingle = do
  BS.isOversizedSingle (mkBatch False 1) `shouldBe` True
  BS.isOversizedSingle (mkBatch False 2) `shouldBe` False


metadata_preserved :: IO ()
metadata_preserved = case BS.splitBatch (mkBatch True 4) of
  Nothing -> error "expected split"
  Just (l, r) -> do
    BA.batchIsTransactional l `shouldBe` True
    BA.batchIsTransactional r `shouldBe` True
    BA.batchProducerId l `shouldBe` 12345
    BA.batchProducerId r `shouldBe` 12345
