{-# LANGUAGE OverloadedStrings #-}

-- | Proves the @wireform-dst@ fault vocabulary drives the @wireform-network@
-- transport seam: clean transfer plus each fault (drop, partition/heal, cut,
-- corruption, latency) at the raw 'ReceiveFn'/'SendFn' contract every protocol
-- shares, and a real magic-ring send-transport round-trip over the link.
module Main (main) where

import Data.Bits (popCount, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Sim.Net.Link
import System.Timeout (timeout)
import Test.Syd
import Wireform.Network (DuplexTransport (..), closeDuplexTransport)
import Wireform.Transport.Send (sendByteString)

main :: IO ()
main =
  sydTest $
    describe "wireform-dst-net" $
      sequence_
        [ rawSeamTests
        , duplexRoundTrip
        ]

-- Send a strict ByteString through a LinkEnd's raw SendFn (the universal seam).
sendRaw :: LinkEnd -> ByteString -> IO ()
sendRaw end bs =
  BSU.unsafeUseAsCStringLen bs $ \(p, n) -> do
    _ <- leSendFn end (castPtr p :: Ptr Word8) n
    pure ()

-- Blocking read of up to n bytes via readN, wrapped in a timeout so a
-- stalled/partitioned/dropped delivery surfaces as Nothing instead of hanging.
readWithin :: Int -> LinkEnd -> Int -> IO (Maybe ByteString)
readWithin us end n = timeout us (leReadN end n)

rawSeamTests :: Spec
rawSeamTests =
  describe "Sim.Net.Link raw ReceiveFn/SendFn seam" $
    sequence_
      [ it "clean link delivers bytes unchanged" $ do
          l <- newSimLink (Seed 1)
          sendRaw (slClient l) "hello"
          got <- readWithin 500000 (slServer l) 64
          got `shouldBe` Just "hello"
      , it "drop=1.0 loses the chunk (receiver stalls)" $ do
          l <- newSimLink (Seed 2)
          setDrop (slControl l) ClientToServer 1.0
          sendRaw (slClient l) "hello"
          got <- readWithin 100000 (slServer l) 64
          got `shouldBe` Nothing
      , it "partition stalls delivery; heal releases held bytes" $ do
          l <- newSimLink (Seed 3)
          partition (slControl l) ClientToServer
          sendRaw (slClient l) "held"
          stalled <- readWithin 100000 (slServer l) 64
          stalled `shouldBe` Nothing
          heal (slControl l) ClientToServer
          released <- readWithin 500000 (slServer l) 64
          released `shouldBe` Just "held"
      , it "cut delivers EOF to the receiver" $ do
          l <- newSimLink (Seed 4)
          sendRaw (slClient l) "gone"
          cut (slControl l)
          got <- readWithin 500000 (slServer l) 64
          got `shouldBe` Just "" -- EOF (empty)
      , it "corruptNext flips exactly one bit, preserving length" $ do
          l <- newSimLink (Seed 5)
          corruptNext (slControl l) ClientToServer
          sendRaw (slClient l) "ABCDEFGH"
          got <- readWithin 500000 (slServer l) 64
          case got of
            Just bs -> do
              BS.length bs `shouldBe` 8
              (bs /= "ABCDEFGH") `shouldBe` True
              popcountDiff bs "ABCDEFGH" `shouldBe` 1
            Nothing -> expectationFailure "expected corrupted bytes, got timeout"
      , it "latency still delivers the bytes intact" $ do
          l <- newSimLink (Seed 6)
          setLatency (slControl l) ClientToServer (UniformMs 1 3)
          sendRaw (slClient l) "slow"
          got <- readWithin 1000000 (slServer l) 64
          got `shouldBe` Just "slow"
      ]

-- A real magic-ring send transport (the production send path) running over the
-- fault link: build the DuplexTransports, send via sendByteString, flush on
-- close, and read the bytes out the far end.
duplexRoundTrip :: Spec
duplexRoundTrip =
  describe "real DuplexTransport over the fault link" $
    it "sendByteString + close flushes bytes across the link" $ do
      l <- newSimLink (Seed 7)
      sendByteString (duplexSend (leDuplex (slClient l))) "PINGPONG"
      -- closing the client duplex drains its send ring into the link and EOFs
      closeDuplexTransport (leDuplex (slClient l))
      got <- readAll (slServer l)
      got `shouldBe` "PINGPONG"

-- Drain a LinkEnd until EOF (empty), concatenating chunks.
readAll :: LinkEnd -> IO ByteString
readAll end = go []
  where
    go acc = do
      chunk <- leReadN end 4096
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (chunk : acc)

-- Count differing bits between two equal-length ByteStrings.
popcountDiff :: ByteString -> ByteString -> Int
popcountDiff a b = sum (map bitdiff [0 .. BS.length a - 1])
  where
    bitdiff i = popCount (BS.index a i `xor` BS.index b i)
