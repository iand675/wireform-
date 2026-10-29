{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Which fetched records the consumer hands back
('Kafka.Client.Consumer.selectFetchedRecords'): control batches are
dropped, @read_committed@ drops exactly the aborted transaction's
batches (and keeps that producer's later, committed ones), and
records below the fetch offset are dropped because the broker sends
whole batches.
-}
module Client.FetchSelectionSpec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int16, Int64)
import Data.Vector qualified as V
import Kafka.Client.Consumer (
  ConsumerRecord (..),
  IsolationLevel (..),
  decodeAllBatchesSliced,
  selectFetchedRecords,
 )
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW
import Test.Syd


tests :: Spec
tests =
  describe "Consumer fetch selection" $ do
    it "read_committed drops the aborted transaction only" $ do
      offs <- selected ReadCommitted [(7, 0)] 0
      offs `shouldBe` [3, 4, 6, 7]
    it "read_uncommitted keeps every data batch, never control batches" $ do
      offs <- selected ReadUncommitted [(7, 0)] 0
      offs `shouldBe` [0, 1, 3, 4, 6, 7]
    it "drops records below the fetch offset" $ do
      offs <- selected ReadCommitted [(7, 0)] 4
      offs `shouldBe` [4, 6, 7]
    it "an abort that starts mid-log only drops from its first offset" $ do
      -- pid 7's first transaction committed; only the second aborted.
      bytes <- pure (BS.concat (map RBW.encodeRecordBatchWire secondAborts))
      Right bs <- decodeAllBatchesSliced bytes
      let recs = selectFetchedRecords ReadCommitted [(7, 3)] 0 "t" 0 bs
      map (.offset) (V.toList recs) `shouldBe` [0, 1, 6]


-- | Offsets of the records selected from 'txnLog'.
selected :: IsolationLevel -> [(Int64, Int64)] -> Int64 -> IO [Int64]
selected iso aborted fetchOffset = do
  Right bs <- decodeAllBatchesSliced (BS.concat (map RBW.encodeRecordBatchWire txnLog))
  pure (map (.offset) (V.toList (selectFetchedRecords iso aborted fetchOffset "t" 0 bs)))


{- | pid 7 writes 0-1 in a transaction and aborts (marker at 2), then
writes 3-4 and commits (marker at 5); 6-7 are non-transactional.
-}
txnLog :: [RB.RecordBatch]
txnLog =
  [ dataBatch True 7 0 2
  , marker 7 2 abortMarker
  , dataBatch True 7 3 2
  , marker 7 5 commitMarker
  , dataBatch False (-1) 6 2
  ]


{- | pid 7 writes 0-1 and commits (marker at 2), then 3-4 and aborts
(marker at 5); 6 is non-transactional.
-}
secondAborts :: [RB.RecordBatch]
secondAborts =
  [ dataBatch True 7 0 2
  , marker 7 2 commitMarker
  , dataBatch True 7 3 2
  , marker 7 5 abortMarker
  , dataBatch False (-1) 6 1
  ]


attrs :: Bool -> Bool -> RB.Attributes
attrs txn control =
  RB.mkAttributes Compression.NoCompression RB.CreateTime txn control False


dataBatch :: Bool -> Int64 -> Int64 -> Int -> RB.RecordBatch
dataBatch txn pid base n =
  RB.mkRecordBatch
    base
    0
    (attrs txn False)
    1000
    pid
    0
    (if pid < 0 then -1 else 0)
    ( V.fromList
        ( map
            ( \i ->
                RB.Record
                  { RB.recordTimestampDelta = 0
                  , RB.recordOffsetDelta = fromIntegral i
                  , RB.recordKey = Nothing
                  , RB.recordValue = Just (BSC.pack (show (base + fromIntegral i)))
                  , RB.recordHeaders = []
                  }
            )
            [0 .. n - 1]
        )
    )


marker :: Int64 -> Int64 -> Int16 -> RB.RecordBatch
marker pid off kind =
  RB.mkRecordBatch
    off
    0
    (attrs True True)
    1000
    pid
    0
    (-1)
    ( V.singleton
        RB.Record
          { RB.recordTimestampDelta = 0
          , RB.recordOffsetDelta = 0
          , RB.recordKey = Just (BS.pack [0, 0, 0, fromIntegral kind])
          , RB.recordValue = Just (BS.pack [0, 0, 0, 0, 0, 0])
          , RB.recordHeaders = []
          }
    )


abortMarker, commitMarker :: Int16
abortMarker = 0
commitMarker = 1
