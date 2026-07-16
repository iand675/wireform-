{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wireform.Transport.SendTest (spec) where

import Control.Exception (catch)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word64, Word8)
import Foreign.Ptr (castPtr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Syd
import Wireform.Builder (
  Builder,
  byteString,
  byteStringCopy,
  byteStringInsert,
  toStrictByteString,
  word32BE,
  word64BE,
  word8,
 )
import Wireform.Ring.Internal (ringBase, ringMask, ringSize, withMagicRing)
import Wireform.Transport.Send


------------------------------------------------------------------------
-- Helpers: generate non-trivial byte patterns
------------------------------------------------------------------------

patternBytes :: Int -> BS.ByteString
patternBytes n = BS.pack (fmap (\i -> fromIntegral (i `mod` 251) :: Word8) [0 .. n - 1])


patternBuilder :: Int -> Builder
patternBuilder n = byteString (patternBytes n)


mixedBuilder :: Int -> Builder
mixedBuilder n =
  let header = word32BE 0xDEADBEEF <> word64BE (fromIntegral n)
      payload = byteString (patternBytes (max 0 (n - 12)))
  in header <> payload


mixedBuilderBS :: Int -> BS.ByteString
mixedBuilderBS n = toStrictByteString (mixedBuilder n)


------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "SendTransport" $ do
  -- ----------------------------------------------------------------
  -- sendBuilderDirect: basic correctness
  -- ----------------------------------------------------------------
  describe "sendBuilderDirect" $ do
    it "sends empty builder" $ do
      (bytes, pubs) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t mempty
      readChunks bytes `shouldBe` BS.empty
      readIORef pubs `shouldReturn` 0

    it "sends a single byte" $ do
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (word8 0x42)
      readChunks bytes `shouldBe` BS.singleton 0x42

    it "sends two bytes" $ do
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (word8 0x42 <> word8 0x43)
      readChunks bytes `shouldBe` BS.pack [0x42, 0x43]

    it "sends a pattern payload (100 bytes)" $ do
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (patternBuilder 100)
      readChunks bytes `shouldBe` patternBytes 100

    it "sends exactly ringSize bytes" $ do
      sz <- ringSizeAt 4096
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (patternBuilder sz)
      readChunks bytes `shouldBe` patternBytes sz

    it "sends ringSize - 1 bytes" $ do
      sz <- ringSizeAt 4096
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (patternBuilder (sz - 1))
      readChunks bytes `shouldBe` patternBytes (sz - 1)

    it "sends ringSize + 1 bytes (forces overflow)" $ do
      sz <- ringSizeAt 16384
      (bytes, _) <- withTestTransport 16384 $ \t ->
        sendBuilderDirect t (patternBuilder (sz + 1))
      readChunks bytes `shouldBe` patternBytes (sz + 1)

    it "sends 2x ringSize bytes" $ do
      sz <- ringSizeAt 4096
      let n = sz * 2
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (patternBuilder n)
      readChunks bytes `shouldBe` patternBytes n

    it "sends 5x ringSize bytes" $ do
      sz <- ringSizeAt 4096
      let n = sz * 5
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (patternBuilder n)
      readChunks bytes `shouldBe` patternBytes n

    it "sends 10x ringSize bytes" $ do
      sz <- ringSizeAt 4096
      let n = sz * 10
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (patternBuilder n)
      readChunks bytes `shouldBe` patternBytes n

    it "sends via byteString insert (large BS, > threshold)" $ do
      let payload = patternBytes 12000
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (byteString payload)
      readChunks bytes `shouldBe` payload

    it "sends via byteString insert larger than ring" $ do
      let payload = patternBytes 50000
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (byteString payload)
      readChunks bytes `shouldBe` payload

    it "sends mixed builder (header + payload)" $ do
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (mixedBuilder 500)
      readChunks bytes `shouldBe` mixedBuilderBS 500

    it "sends many small builders concatenated" $ do
      let builders = fmap (\i -> word8 (fromIntegral (i :: Int))) [0 .. 255]
          b = mconcat builders
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t b
      readChunks bytes `shouldBe` BS.pack [0 .. 255]

    it "sends builder with explicit byteStringInsert" $ do
      let payload = patternBytes 20000
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendBuilderDirect t (byteStringInsert payload)
      readChunks bytes `shouldBe` payload

  -- ----------------------------------------------------------------
  -- sendBuilderDirect vs sendBuilderViaByteString equivalence
  -- ----------------------------------------------------------------
  describe "sendBuilderDirect vs sendBuilderViaByteString" $ do
    it "identical output for small payload" $ do
      let b = mixedBuilder 200
      (directBytes, _) <- withTestTransport 65536 $ \t ->
        sendBuilderDirect t b
      (viaBytes, _) <- withTestTransport 65536 $ \t ->
        sendBuilderViaByteString t b
      readChunks directBytes `shouldBe` readChunks viaBytes

    it "identical output for medium payload" $ do
      let b = mixedBuilder 5000
      (directBytes, _) <- withTestTransport 65536 $ \t ->
        sendBuilderDirect t b
      (viaBytes, _) <- withTestTransport 65536 $ \t ->
        sendBuilderViaByteString t b
      readChunks directBytes `shouldBe` readChunks viaBytes

    it "identical output for large payload (multi-overflow)" $ do
      let b = mixedBuilder 40000
      (directBytes, _) <- withTestTransport 65536 $ \t ->
        sendBuilderDirect t b
      (viaBytes, _) <- withTestTransport 65536 $ \t ->
        sendBuilderViaByteString t b
      readChunks directBytes `shouldBe` readChunks viaBytes

    it "identical output for byteString insert path" $ do
      let b = byteString (patternBytes 25000)
      (directBytes, _) <- withTestTransport 32768 $ \t ->
        sendBuilderDirect t b
      (viaBytes, _) <- withTestTransport 32768 $ \t ->
        sendBuilderViaByteString t b
      readChunks directBytes `shouldBe` readChunks viaBytes

  -- ----------------------------------------------------------------
  -- sendByteString
  -- ----------------------------------------------------------------
  describe "sendByteString" $ do
    it "sends empty BS (no-op)" $ do
      (bytes, pubs) <- withTestTransport 4096 $ \t ->
        sendByteString t BS.empty
      readChunks bytes `shouldBe` BS.empty
      readIORef pubs `shouldReturn` 0

    it "sends 1 byte" $ do
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendByteString t (BS.singleton 0xAB)
      readChunks bytes `shouldBe` BS.singleton 0xAB

    it "sends exactly ringSize bytes" $ do
      sz <- ringSizeAt 4096
      (bytes, _) <- withTestTransport 4096 $ \t ->
        sendByteString t (patternBytes sz)
      readChunks bytes `shouldBe` patternBytes sz

    it "sendByteStringMany coalesces" $ do
      let bss = [patternBytes 100, patternBytes 200, patternBytes 50]
      (bytes, pubs) <- withTestTransport 4096 $ \t ->
        sendByteStringMany t bss
      readChunks bytes `shouldBe` BS.concat bss
      readIORef pubs `shouldReturn` 1

  -- ----------------------------------------------------------------
  -- sendByteString: reservation too large
  -- ----------------------------------------------------------------
  describe "sendByteString limits" $ do
    it "throws SendReservationTooLarge for BS > ringSize" $ do
      threw <- withMagicRing 4096 $ \ring -> do
        let payload = patternBytes (ringSize ring + 1)
        headRef' <- newIORef (0 :: Word64)
        tailRef' <- newIORef (0 :: Word64)
        let t' =
              SendTransport
                { sendRingBase = ringBase ring
                , sendRingSize = ringSize ring
                , sendRingMask = ringMask ring
                , sendLoadTail = readIORef tailRef'
                , sendLoadHead = readIORef headRef'
                , sendPublishHead = \h -> writeIORef headRef' h >> writeIORef tailRef' h
                , sendWaitSpace = \_ -> SendSpaceAvailable <$> readIORef tailRef'
                , sendFlush = pure ()
                , sendShutdownWrite = pure ()
                , sendClose = pure ()
                }
        (sendByteString t' payload >> pure False)
          `catch` (\(SendReservationTooLarge _ _) -> pure True)
      threw `shouldBe` True

  -- ----------------------------------------------------------------
  -- withSendCork: batching
  -- ----------------------------------------------------------------
  describe "withSendCork" $ do
    it "empty cork: zero publishes, zero bytes" $ do
      (bytes, pubs) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \_ -> pure ()
      readChunks bytes `shouldBe` BS.empty
      readIORef pubs `shouldReturn` 0

    it "single send inside cork: one publish" $ do
      (bytes, pubs) <- withTestTransport 65536 $ \t ->
        withSendCork t $ \corked ->
          sendByteString corked (patternBytes 100)
      readChunks bytes `shouldBe` patternBytes 100
      readIORef pubs `shouldReturn` 1

    it "batches two sendByteString into one publish" $ do
      let a = patternBytes 100
          b = patternBytes 200
      (bytes, pubs) <- withTestTransport 65536 $ \t ->
        withSendCork t $ \corked -> do
          sendByteString corked a
          sendByteString corked b
      readChunks bytes `shouldBe` BS.append a b
      readIORef pubs `shouldReturn` 1

    it "batches three sendByteString into one publish" $ do
      let a = patternBytes 100
          b = patternBytes 200
          c = patternBytes 300
      (bytes, pubs) <- withTestTransport 65536 $ \t ->
        withSendCork t $ \corked -> do
          sendByteString corked a
          sendByteString corked b
          sendByteString corked c
      readChunks bytes `shouldBe` BS.concat [a, b, c]
      readIORef pubs `shouldReturn` 1

    it "batches builder + bytestring into one publish" $ do
      let header = word8 0x01 <> word32BE 0x12345678
          body = patternBytes 50
      (bytes, pubs) <- withTestTransport 65536 $ \t ->
        withSendCork t $ \corked -> do
          sendBuilderDirect corked header
          sendByteString corked body
      let result = readChunks bytes
      BS.take 5 result `shouldBe` BS.pack [0x01, 0x12, 0x34, 0x56, 0x78]
      BS.drop 5 result `shouldBe` body
      readIORef pubs `shouldReturn` 1

    it "batches sendBuilderDirect + sendBuilderDirect" $ do
      (bytes, pubs) <- withTestTransport 65536 $ \t ->
        withSendCork t $ \corked -> do
          sendBuilderDirect corked (mixedBuilder 200)
          sendBuilderDirect corked (mixedBuilder 300)
      readChunks bytes
        `shouldBe` BS.append (mixedBuilderBS 200) (mixedBuilderBS 300)
      readIORef pubs `shouldReturn` 1

    it "corked payload exactly ring size" $ do
      sz <- ringSizeAt 4096
      (bytes, _) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          sendBuilderDirect corked (patternBuilder sz)
      readChunks bytes `shouldBe` patternBytes sz

  -- ----------------------------------------------------------------
  -- withSendCork: backpressure
  -- ----------------------------------------------------------------
  describe "withSendCork backpressure" $ do
    it "builder payload 2x ring" $ do
      sz <- ringSizeAt 4096
      let n = sz * 2
      (bytes, pubs) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          sendBuilderDirect corked (patternBuilder n)
      readChunks bytes `shouldBe` patternBytes n
      readIORef pubs >>= (`shouldSatisfy` (> 1))

    it "builder payload 5x ring" $ do
      sz <- ringSizeAt 4096
      let n = sz * 5
      (bytes, _) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          sendBuilderDirect corked (patternBuilder n)
      readChunks bytes `shouldBe` patternBytes n

    it "builder payload 10x ring" $ do
      sz <- ringSizeAt 4096
      let n = sz * 10
      (bytes, _) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          sendBuilderDirect corked (patternBuilder n)
      readChunks bytes `shouldBe` patternBytes n

    it "byteString insert > ring inside cork" $ do
      sz <- ringSizeAt 4096
      let payload = patternBytes (sz + 1)
      (bytes, pubs) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          sendBuilderDirect corked (byteString payload)
      readChunks bytes `shouldBe` payload
      readIORef pubs >>= (`shouldSatisfy` (> 1))

    it "many small sends exceeding ring" $ do
      sz <- ringSizeAt 4096
      let chunk = patternBytes 500
          numChunks = sz `quot` BS.length chunk + 1
      (bytes, _) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          mapM_ (\_ -> sendByteString corked chunk) [1 .. numChunks]
      readChunks bytes `shouldBe` BS.concat (replicate numChunks chunk)

    it "many small builders exceeding ring" $ do
      sz <- ringSizeAt 4096
      let numBuilders = sz `quot` 4 + 1
      (bytes, _) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          mapM_
            (sendBuilderDirect corked . word32BE . fromIntegral)
            [0 .. numBuilders - 1]
      let expected =
            toStrictByteString $
              mconcat (fmap (word32BE . fromIntegral) [0 .. numBuilders - 1 :: Int])
      readChunks bytes `shouldBe` expected

    it "interleaved builder + BS exceeding ring" $ do
      sz <- ringSizeAt 4096
      let n = sz `quot` 204 + 1
      (bytes, _) <- withTestTransport 4096 $ \t ->
        withSendCork t $ \corked ->
          mapM_
            ( \i -> do
                sendBuilderDirect corked (word32BE (fromIntegral i))
                sendByteString corked (patternBytes 200)
            )
            [0 .. n - 1]
      BS.length (readChunks bytes) `shouldBe` n * (4 + 200)

  -- ----------------------------------------------------------------
  -- Sequential cork + uncork operations
  -- ----------------------------------------------------------------
  describe "sequential cork/uncork" $ do
    it "cork then uncorked send" $ do
      (bytes, _) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \corked ->
          sendByteString corked (BS.pack [0x01, 0x02])
        sendByteString t (BS.pack [0x03, 0x04])
      readChunks bytes `shouldBe` BS.pack [0x01, 0x02, 0x03, 0x04]

    it "two sequential corks" $ do
      (bytes, pubs) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \corked -> do
          sendByteString corked (BS.pack [0x01])
          sendByteString corked (BS.pack [0x02])
        withSendCork t $ \corked -> do
          sendByteString corked (BS.pack [0x03])
          sendByteString corked (BS.pack [0x04])
      readChunks bytes `shouldBe` BS.pack [0x01, 0x02, 0x03, 0x04]
      readIORef pubs `shouldReturn` 2

    it "three sequential corks with varying sizes" $ do
      (bytes, pubs) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \c -> sendBuilderDirect c (patternBuilder 100)
        withSendCork t $ \c -> sendBuilderDirect c (patternBuilder 500)
        withSendCork t $ \c -> sendBuilderDirect c (patternBuilder 1000)
      readChunks bytes
        `shouldBe` BS.concat [patternBytes 100, patternBytes 500, patternBytes 1000]
      readIORef pubs `shouldReturn` 3

    it "cork then many uncorked sends" $ do
      (bytes, _) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \c ->
          sendByteString c (patternBytes 50)
        mapM_ (sendByteString t . BS.singleton . fromIntegral) [0 .. 9 :: Int]
      readChunks bytes
        `shouldBe` BS.append (patternBytes 50) (BS.pack [0 .. 9])

  -- ----------------------------------------------------------------
  -- Property-based: arbitrary payload sizes
  -- ----------------------------------------------------------------
  describe "property-based" $ do
    it "sendBuilderDirect produces correct output for arbitrary sizes" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let sz = min n 100000
        (bytes, _) <- run $ withTestTransport 4096 $ \t ->
          sendBuilderDirect t (patternBuilder sz)
        assert (readChunks bytes == patternBytes sz)

    it "sendBuilderDirect == sendBuilderViaByteString for arbitrary builders" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let sz = min n 50000
            b = mixedBuilder sz
        (directBytes, _) <- run $ withTestTransport 65536 $ \t ->
          sendBuilderDirect t b
        (viaBytes, _) <- run $ withTestTransport 65536 $ \t ->
          sendBuilderViaByteString t b
        assert (readChunks directBytes == readChunks viaBytes)

    it "corked sendBuilderDirect produces correct output for arbitrary sizes" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let sz = min n 100000
        (bytes, _) <- run $ withTestTransport 4096 $ \t ->
          withSendCork t $ \corked ->
            sendBuilderDirect corked (patternBuilder sz)
        assert (readChunks bytes == patternBytes sz)

    it "corked multi-send produces correct output" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let numSends = min n 50
            chunk = patternBytes 100
        (bytes, _) <- run $ withTestTransport 4096 $ \t ->
          withSendCork t $ \corked ->
            mapM_ (\_ -> sendByteString corked chunk) [1 .. numSends]
        assert (readChunks bytes == BS.concat (replicate numSends chunk))


------------------------------------------------------------------------
-- Test infrastructure
------------------------------------------------------------------------

ringSizeAt :: Int -> IO Int
ringSizeAt requested = withMagicRing requested (pure . ringSize)


withTestTransport
  :: Int
  -> (SendTransport -> IO a)
  -> IO (IORef [BS.ByteString], IORef Int)
withTestTransport requestedRingSz action =
  withMagicRing requestedRingSz $ \ring -> do
    let !base = ringBase ring
        !sz = ringSize ring
        !msk = ringMask ring
    chunksRef <- newIORef ([] :: [BS.ByteString])
    publishCount <- newIORef (0 :: Int)
    headRef <- newIORef (0 :: Word64)
    tailRef <- newIORef (0 :: Word64)
    stateRef <- newIORef True

    let drainTo !hd = do
          tl <- readIORef tailRef
          drainLoop tl
          where
            drainLoop !cur
              | cur >= hd = pure ()
              | otherwise = do
                  let !off = fromIntegral cur .&. msk
                      !want = min (fromIntegral (hd - cur)) (sz - off)
                      !ptr = base `plusPtr` off
                  bs <- BS.packCStringLen (castPtr ptr, want)
                  modifyIORef' chunksRef (bs :)
                  let !newTail = cur + fromIntegral want
                  writeIORef tailRef newTail
                  drainLoop newTail

    let publish h = do
          modifyIORef' publishCount (+ 1)
          writeIORef headRef h
          drainTo h

    let waitSpace _pos = do
          isOpen <- readIORef stateRef
          if isOpen
            then do
              tl <- readIORef tailRef
              pure (SendSpaceAvailable tl)
            else pure SendPeerClosed

    let transport =
          SendTransport
            { sendRingBase = base
            , sendRingSize = sz
            , sendRingMask = msk
            , sendLoadTail = readIORef tailRef
            , sendLoadHead = readIORef headRef
            , sendPublishHead = publish
            , sendWaitSpace = waitSpace
            , sendFlush = readIORef headRef >>= drainTo
            , sendShutdownWrite = pure ()
            , sendClose = writeIORef stateRef False
            }
    _ <- action transport
    pure (chunksRef, publishCount)


readChunks :: IORef [BS.ByteString] -> BS.ByteString
readChunks ref = unsafePerformIO $ do
  chunks <- readIORef ref
  pure (BS.concat (reverse chunks))
{-# NOINLINE readChunks #-}
