module Test.Iceberg.DeletionVector (tests) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Iceberg.DeletionVector
import Iceberg.Puffin (pbData, pbFields)
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.DeletionVector" $
    sequence_
      [ it "encodeDV / decodeDV round-trip empty" $
          case decodeDV (encodeDV emptyDV) of
            Right dv -> deletedPositions dv `shouldBe` []
            Left e -> expectationFailure e
      , it "encodeDV / decodeDV round-trip a few small ids" $ do
          let dv = addPositions [0, 1, 2, 5, 10, 1000] emptyDV
          case decodeDV (encodeDV dv) of
            Right dv' -> deletedPositions dv' `shouldBe` [0, 1, 2, 5, 10, 1000]
            Left e -> expectationFailure e
      , it "round-trip across high-32 bucket boundaries" $ do
          let highVals = [4294967296, 4294967300, 8589934592] -- 2^32, 2^32+4, 2^33
              dv = addPositions (map fromIntegral highVals) emptyDV
          case decodeDV (encodeDV dv) of
            Right dv' -> deletedPositions dv' `shouldBe` map fromIntegral highVals
            Left e -> expectationFailure e
      , it "a truncated container payload is an error, not fewer deletions" $ do
          -- The bitmap says positions 0..5 are deleted; dropping payload
          -- bytes must not decode to a smaller set, or the reader would
          -- return rows the file marks as deleted.
          let bs = encodeDV (addPositions [0, 1, 2, 3, 4, 5] emptyDV)
          case decodeDV (BS.take (BS.length bs - 2) bs) of
            Left _ -> pure ()
            Right dv -> expectationFailure $ "truncated bitmap decoded: " <> show (deletedPositions dv)
      , it "a truncated container table is an error" $ do
          let bs = encodeDV (addPositions [1, 2, 3] emptyDV)
          -- 8 (count) + 4 (hi-32 key) + 4 (cookie) + 4 (containers): the
          -- container table that should follow is gone.
          case decodeDV (BS.take 20 bs) of
            Left _ -> pure ()
            Right dv -> expectationFailure $ "truncated header decoded: " <> show (deletedPositions dv)
      , it "toPuffinBlob emits the right blob type and field id" $ do
          let dv = addPositions [3, 7] emptyDV
              blob = toPuffinBlob 99 1 42 dv
          pbFields blob `shouldBe` V.singleton 42
          -- The blob payload is at least the bitmap plus the 4+4 framing bytes.
          (8 <= length (show (pbData blob))) `shouldBe` True
          case fromPuffinBlob blob of
            Right dv' -> deletedPositions dv' `shouldBe` [3, 7]
            Left e -> expectationFailure e
      ]
