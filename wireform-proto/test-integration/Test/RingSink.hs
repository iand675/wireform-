{-# LANGUAGE BangPatterns #-}

{- | Proto encoders through the ring-direct send sink.

'putText' reserves a string field's whole UTF-8 length with one
'ensureBytes', so a field longer than the send ring is a request
no contiguous ring region can satisfy.  These properties drive
such fields through 'sendBuilderDirect' and check the bytes match
the reference ('B.toStrictByteString') encoding.
-}
module Test.RingSink (ringSinkTests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.Ptr (castPtr, plusPtr)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Encode (buildMessage, encodeFieldString)
import Proto.Google.Protobuf.Wrappers (StringValue (..))
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B
import Wireform.Ring.Internal (ringBase, ringMask, ringSize, withMagicRing)
import Wireform.Transport.Send


ringSinkTests :: Spec
ringSinkTests = describe "RingSink (sendBuilderDirect)" $ do
  it "string field at ring boundaries matches the reference encoding" $
    property $ do
      ringSz <- genRingSize
      n <- forAll (Gen.element (boundaryLens ringSz))
      corked <- forAll Gen.bool
      let txt = asciiText n
      sent <- evalIO (sendVia ringSz corked (encodeFieldString 1 txt))
      sent === B.toStrictByteString (encodeFieldString 1 txt)

  it "generated StringValue with mixed-width UTF-8 matches the reference encoding" $
    property $ do
      ringSz <- genRingSize
      n <-
        forAll $
          Gen.frequency
            [ (3, Gen.element (boundaryLens ringSz))
            , (1, Gen.int (Range.linear 0 (3 * ringSz)))
            ]
      seed <- forAll (Gen.word64 Range.constantBounded)
      corked <- forAll Gen.bool
      let txt = mixedText n seed
      let msg = StringValue {stringValueValue = txt, stringValueUnknownFields = []}
      sent <- evalIO (sendVia ringSz corked (buildMessage msg))
      sent === B.toStrictByteString (buildMessage msg)


-- | A ring of 1x, 2x or 4x the host page size.
genRingSize :: PropertyT IO Int
genRingSize = do
  page <- evalIO (withMagicRing 1 (pure . ringSize))
  mult <- forAll (Gen.element [1, 2, 4])
  ringSz <- evalIO (withMagicRing (mult * page) (pure . ringSize))
  annotateShow ringSz
  pure ringSz


-- | The ring size, its doubles and the sink's 32 KiB chunk, each ±1.
boundaryLens :: Int -> [Int]
boundaryLens ringSz =
  filter (>= 0) $
    concatMap (\b -> fmap (b +) [-1, 0, 1]) [ringSz, 2 * ringSz, 32768]


{- | Text of exactly @n@ UTF-8 bytes mixing 1- to 4-byte code
points, drawn from @seed@.  Built directly rather than via
'Gen.text': generating and shrink-tracking tens of thousands of
characters costs ~70 MB per case, while this costs one 'Text'.
-}
mixedText :: Int -> Word64 -> Text
mixedText n seed0 = T.unfoldr step (n, seed0)
  where
    step (!left, !s)
      | left <= 0 = Nothing
      | otherwise =
          let !s' = s * 6364136223846793005 + 1442695040888963407
              !r = fromIntegral (s' `shiftR` 33) :: Int
              !w = 1 + r `rem` min 4 left
              !c = case w of
                1 -> chr (0x61 + r `rem` 26) -- a-z
                2 -> chr (0xE0 + r `rem` 0x100) -- U+00E0..U+01DF
                3 -> chr (0x4E00 + r `rem` 0x1000) -- CJK, no surrogates
                _ -> chr (0x1F600 + r `rem` 0x40) -- emoji
          in Just (c, (left - w, s'))


-- | @n@ ASCII characters, so the UTF-8 length is exactly @n@.
asciiText :: Int -> Text
asciiText n = T.unfoldrN n (\k -> Just (chr (97 + k `rem` 26), k + 1)) (0 :: Int)


{- | Run a builder through 'sendBuilderDirect' over an in-memory
ring whose consumer drains inline on every publish, and return
everything it drained.
-}
sendVia :: Int -> Bool -> B.Builder -> IO ByteString
sendVia requested corked b = withMagicRing requested $ \ring -> do
  let !base = ringBase ring
      !sz = ringSize ring
      !msk = ringMask ring
  chunksRef <- newIORef ([] :: [ByteString])
  headRef <- newIORef (0 :: Word64)
  tailRef <- newIORef (0 :: Word64)
  let drainTo !hd = readIORef tailRef >>= go
        where
          go !cur
            | cur >= hd = pure ()
            | otherwise = do
                let !off = fromIntegral cur .&. msk
                    !want = min (fromIntegral (hd - cur)) (sz - off)
                chunk <- BS.packCStringLen (castPtr (base `plusPtr` off), want)
                modifyIORef' chunksRef (chunk :)
                let !next = cur + fromIntegral want
                writeIORef tailRef next
                go next
      t =
        SendTransport
          { sendRingBase = base
          , sendRingSize = sz
          , sendRingMask = msk
          , sendLoadTail = readIORef tailRef
          , sendLoadHead = readIORef headRef
          , sendPublishHead = \h -> writeIORef headRef h >> drainTo h
          , sendWaitSpace = \_ -> SendSpaceAvailable <$> readIORef tailRef
          , sendFlush = readIORef headRef >>= drainTo
          , sendShutdownWrite = pure ()
          , sendClose = pure ()
          }
  if corked
    then withSendCork t (`sendBuilderDirect` b)
    else sendBuilderDirect t b
  BS.concat . reverse <$> readIORef chunksRef
