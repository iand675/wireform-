{-# LANGUAGE BangPatterns #-}

{- | Boundary properties for the ring-direct builder sink.

Which code path a builder primitive takes depends on how its
request compares with three sizes: the ring (a page multiple, so
host dependent), 'byteString''s copy/insert threshold
(@maximalCopySize@, ~8 KiB) and the sink's default chunk (32 KiB).
These properties cluster payload lengths at each of those and run
them against rings of 1x, 2x and 4x the page size, so every
crossing reachable on the host is exercised, whatever the page size.
-}
module Wireform.Transport.SendBoundaryTest (spec) where

import Control.Exception (evaluate, try)
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra qualified as X
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Hedgehog (Gen, annotateShow, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder
import Wireform.Transport.Send
import Wireform.Transport.SendTest (readChunks, ringSizeAt, withTestTransport)


-- | The builder primitives whose ring-sink paths differ.
data Prim
  = PByteString
  | PByteStringCopy
  | PByteStringInsert
  | PCStringLen
  | {- | @ensureBytes n@ then an unchecked write: the shape of
    every primitive that reserves its whole length up front (e.g.
    the proto string-field writer).
    -}
    PEnsureThenWrite
  deriving stock (Show, Eq, Enum, Bounded)


-- | One primitive call, preceded by @itemLead@ single-byte writes.
data Item = Item
  { itemPrim :: !Prim
  , itemLead :: !Int
  , itemLen :: !Int
  }
  deriving stock (Show)


spec :: Spec
spec = describe "SendTransport boundaries" $ do
  it "builder primitives round-trip at every ring/threshold/chunk boundary" $
    property $ do
      page <- evalIO (ringSizeAt 1)
      mult <- forAll (Gen.element [1, 2, 4])
      ringSz <- evalIO (ringSizeAt (mult * page))
      annotateShow ringSz
      items <- forAll (Gen.list (Range.linear 1 4) (genItem ringSz))
      corked <- forAll Gen.bool
      let payloads = zipWith itemPayload [0 ..] items
          expected =
            BS.concat
              (zipWith (\item p -> BS.replicate (itemLead item) leadByte <> p) items payloads)
      (bytes, _) <- evalIO $ withTestTransport ringSz $ \t ->
        withItems (zip items payloads) $ \b ->
          if corked
            then withSendCork t (`sendBuilderDirect` b)
            else sendBuilderDirect t b
      readChunks bytes === expected

  describe "FixedBuffer overflow" $ do
    it "getBytes past the end throws FixedBufferOverflow" $
      try (evaluate (toStrictByteStringExact 4 (word64BE 1)))
        `shouldReturn` Left (FixedBufferOverflow {fixedBufferRequested = 8, fixedBufferAvailable = 4})

    it "byteStringInsert past the end throws FixedBufferOverflow" $
      try (evaluate (toStrictByteStringExact 16 (byteStringInsert (BS.replicate 32 1))))
        `shouldReturn` Left (FixedBufferOverflow {fixedBufferRequested = 32, fixedBufferAvailable = 16})


genItem :: Int -> Gen Item
genItem ringSz =
  Item
    <$> Gen.enumBounded
    <*> Gen.int (Range.linear 0 3)
    <*> Gen.frequency
      [ (3, Gen.element (boundaryLens ringSz))
      , (1, Gen.int (Range.linear 0 (4 * ringSz)))
      ]


-- | Every interesting size, and one either side of it.
boundaryLens :: Int -> [Int]
boundaryLens ringSz =
  filter (>= 0) $
    concatMap
      (\b -> fmap (b +) [-1, 0, 1])
      [0, ringSz, 2 * ringSz, maximalCopySize, defaultDirectChunk]
  where
    -- 'byteString' copies below this and inserts at or above it.
    maximalCopySize = 2 * X.smallChunkSize
    -- 'sendBuilderDirect''s default region size.
    defaultDirectChunk = 32768


-- | Distinct content per item so misordered chunks are caught.
itemPayload :: Int -> Item -> BS.ByteString
itemPayload i item = fst (BS.unfoldrN (itemLen item) step (i * 37))
  where
    step !k = Just (fromIntegral (k `rem` 251) :: Word8, k + 1)


leadByte :: Word8
leadByte = 0xA5


{- | Build all items into one 'Builder' and run the action with it.
'PCStringLen' borrows the payload's buffer, so the action runs
inside every 'BSU.unsafeUseAsCStringLen' scope.
-}
withItems :: [(Item, BS.ByteString)] -> (Builder -> IO a) -> IO a
withItems [] k = k mempty
withItems ((item, p) : rest) k =
  withPrim (itemPrim item) p $ \b ->
    withItems rest $ \bs ->
      k (mconcat (replicate (itemLead item) (word8 leadByte)) <> b <> bs)


withPrim :: Prim -> BS.ByteString -> (Builder -> IO a) -> IO a
withPrim prim p k = case prim of
  PByteString -> k (byteString p)
  PByteStringCopy -> k (byteStringCopy p)
  PByteStringInsert -> k (byteStringInsert p)
  PCStringLen -> BSU.unsafeUseAsCStringLen p (k . unsafeCStringLen)
  PEnsureThenWrite -> k (ensureBytes (BS.length p) <> byteStringCopyNoCheck p)
