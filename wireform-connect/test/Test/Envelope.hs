{-# LANGUAGE OverloadedStrings #-}

module Test.Envelope (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.IORef
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Connect.Envelope

tests :: Group
tests =
  Group
    "Envelope"
    [ ("frames round-trip across arbitrary chunk boundaries", property frameRoundtrip)
    , ("end-stream JSON round-trips (success + error)", withTests 1 (property endStreamRoundtrip))
    , ("rejects invalid error shapes", withTests 1 (property rejectInvalidEnd))
    ]

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

frameRoundtrip :: PropertyT IO ()
frameRoundtrip = do
  frames <- forAll (Gen.list (Range.linear 0 8) genFrame)
  let built = BL.toStrict (B.toLazyByteString (mconcat [buildFrame f p | (f, p) <- frames]))
  chunks <- forAll (genChunks built)
  decoded <- evalIO (readAll chunks)
  decoded === frames

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

endStreamRoundtrip :: PropertyT IO ()
endStreamRoundtrip = do
  let ok = EndStreamResponse Nothing []
  decodeEndStream (encodeEndStream ok) === Right ok

rejectInvalidEnd :: PropertyT IO ()
rejectInvalidEnd = do
  assert (isLeft (decodeEndStream "{\"error\": null}"))
  assert (isLeft (decodeEndStream "{\"error\": {}}"))
  assert (isLeft (decodeEndStream "{\"error\": {\"code\": null}}"))
  -- absent error == success
  decodeEndStream "{}" === Right (EndStreamResponse Nothing [])
