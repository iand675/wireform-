{-# LANGUAGE OverloadedStrings #-}

module LZORegression (tests) where

import Columnar.LZO qualified as LZO
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Word (Word32)
import Hedgehog (forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Mem (performGC)
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Columnar.LZO raw blocks" $
    sequence_
      [ it "matches independent literal streams and unaligned roundtrips" $ H.withTests 300 $ property $ do
          bytes <- forAll (Gen.bytes (Range.linear 0 8192))
          alignment <- forAll (Gen.int (Range.linear 0 31))
          let container = BS.replicate alignment 165 <> bytes <> BS.replicate 13 90
              source = BS.take (BS.length bytes) (BS.drop alignment container)
              packed = LZO.compress source
          LZO.decompress (BS.length bytes) (literalBlock source) === Right bytes
          (packed >>= LZO.decompress (BS.length bytes)) === Right bytes
          container === BS.replicate alignment 165 <> bytes <> BS.replicate 13 90
          case packed of
            Left err -> H.annotateShow err >> H.failure
            Right output -> do
              isLeft (LZO.decompress (BS.length bytes) (output <> BS.singleton 0)) === True
              if BS.null bytes
                then pure ()
                else isLeft (LZO.decompress (BS.length bytes - 1) output) === True
      , it "checks capacity with unbounded arithmetic across host/codec limits" $ do
          let integers = [-1, 0, 1, 16, 17, 65536, 2147483647, 4294967294, 4294967295, 4294967296, toInteger (maxBound :: Int)]
          forM_ (filter (\n -> n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int)) integers) $ \n -> do
            let bound = n + n `quot` 16 + 67
                expected = if n < 0 || n > 4294967295 || bound > toInteger (maxBound :: Int) then -1 else fromInteger bound
            LZO.compressBound (fromInteger n) `shouldBe` expected
      , it "validates huge budgets before allocating and rejects incomplete streams" $ do
          LZO.decompress maxBound BS.empty `shouldBe` Left (LZO.LZOFailure 1)
          LZO.decompress maxBound (BS.pack [17, 0, 0]) `shouldBe` Right BS.empty
          LZO.decompress (-1) BS.empty `shouldBe` Left LZO.LZOInvalidOutputLimit
          LZO.decompress 0 (BS.pack [17, 0, 0, 0]) `shouldBe` Left (LZO.LZOFailure 4)
          LZO.decompress 4096 (BS.pack [18, 42, 0, 255, 17, 0, 0]) `shouldBe` Left (LZO.LZOFailure 3)
          forM_ [0 .. 33] $ \n -> do
            let bytes = BS.replicate n 7
                packed = literalBlock bytes
            forM_ [0 .. BS.length packed - 1] $ \cut ->
              isLeft (LZO.decompress n (BS.take cut packed)) `shouldBe` True
      , it "keeps input and output alive across concurrent GC and safe FFI calls" $ do
          stop <- newEmptyMVar
          done <- newEmptyMVar
          let collect = do
                flag <- tryReadMVar stop
                case flag of
                  Just () -> pure ()
                  Nothing -> performGC >> yield >> collect
          _ <- forkIO (collect `finally` putMVar done ())
          ( forM_ [0 .. 15 :: Word32] $ \seed -> do
              let bytes = fst (BS.unfoldrN 65537 (\state -> Just (fromIntegral state, state * 1664525 + 1013904223)) seed)
              (LZO.compress bytes >>= LZO.decompress (BS.length bytes)) `shouldBe` Right bytes
            )
            `finally` (putMVar stop () >> takeMVar done)
      ]


-- A separate literal-only writer supplies wire bytes independently of the
-- compressor's dictionary/match logic. All bytes are little-endian LZO1X.
literalBlock :: BS.ByteString -> BS.ByteString
literalBlock bytes = header <> bytes <> BS.pack [17, 0, 0]
  where
    n = BS.length bytes
    header
      | n == 0 = BS.empty
      | n <= 238 = BS.singleton (fromIntegral (n + 17))
      | otherwise = BS.pack (0 : replicate ((n - 19) `quot` 255) 0 ++ [fromIntegral ((n - 19) `rem` 255 + 1)])
