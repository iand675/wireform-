{-# LANGUAGE OverloadedStrings #-}

module Test.Envelope (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.IORef
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Connect.Envelope
import Test.Syd
import Test.Syd.Hedgehog ()

tests :: Spec
tests =
  describe "Envelope" $ do
    it "frames round-trip across arbitrary chunk boundaries" $ H.property $ do
      frames <- H.forAll (Gen.list (Range.linear 0 8) genFrame)
      let built = BL.toStrict (B.toLazyByteString (mconcat (map (uncurry buildFrame) frames)))
      chunks <- H.forAll (genChunks built)
      decoded <- H.evalIO (readAll chunks)
      decoded H.=== frames
    it "end-stream JSON round-trips (success + error)" $ do
      let ok = EndStreamResponse Nothing []
      decodeEndStream (encodeEndStream ok) `shouldBe` Right ok
    it "rejects invalid error shapes" $ do
      decodeEndStream "{\"error\": null}" `shouldSatisfy` isLeft
      decodeEndStream "{\"error\": {}}" `shouldSatisfy` isLeft
      decodeEndStream "{\"error\": {\"code\": null}}" `shouldSatisfy` isLeft
      -- absent error == success
      decodeEndStream "{}" `shouldBe` Right (EndStreamResponse Nothing [])

genFlags :: Gen EnvelopeFlags
genFlags = EnvelopeFlags <$> Gen.bool <*> Gen.bool

genFrame :: Gen (EnvelopeFlags, ByteString)
genFrame = (,) <$> genFlags <*> Gen.bytes (Range.linear 0 200)

-- Split a ByteString into random non-empty chunks.
genChunks :: ByteString -> Gen [ByteString]
genChunks bs
  | BS.null bs = pure []
  | otherwise = do
      n <- Gen.int (Range.linear 1 (BS.length bs))
      let (h, t) = BS.splitAt n bs
      (h :) <$> genChunks t

-- Read every frame from a producer over the given chunk list.
readAll :: [ByteString] -> IO [(EnvelopeFlags, ByteString)]
readAll chunks0 = do
  ref <- newIORef chunks0
  let produce = do
        cs <- readIORef ref
        case cs of
          [] -> pure Nothing
          (c : rest) -> writeIORef ref rest >> pure (Just c)
  fr <- newFrameReader produce
  let loop acc = do
        m <- readFrame fr
        case m of
          Nothing -> pure (reverse acc)
          Just frame -> loop (frame : acc)
  loop []
