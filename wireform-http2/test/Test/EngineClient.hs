{-# LANGUAGE RankNTypes #-}
module Test.EngineClient (tests) where

import Control.Concurrent.STM
import Control.Exception (try)
import Data.ByteString qualified as BS
import Foreign.Ptr (castPtr)
import Network.HTTP2.Client (ClientStreamError (..))
import Network.HTTP2.Engine.Run.Client
import Network.HTTP2.Engine.Types
import Network.HTTP2.Frame
import Network.HTTP2.Types
import System.Timeout (timeout)
import Test.Syd

tests :: Spec
tests = describe "Engine client failures" $ do
  it "wakes a request reset before response headers" $
    withinDeadline $ withPeer (\sid -> [reset sid]) $ \send -> do
      result <- try (send request (const (pure ())))
      expectReset result
  it "wakes header waiters after a fatal HPACK error" $
    withinDeadline $ withPeer (\sid -> [headers sid (BS.singleton 0x80)]) $ \send -> do
      -- Indexed representation with index zero is invalid (RFC 7541 6.1).
      result <- try (send request (const (pure ())))
      case result of
        Left ClientStreamConnectionClosed -> pure ()
        other -> expectationFailure ("Expected connection failure, got " ++ show (other :: Either ClientStreamError ()))
  it "does not block other streams when resetting an abandoned full body queue" $
    withinDeadline $ withPeer responses $ \send ->
      send request $ \first -> do
        -- The first body remains unread while the next request waits for
        -- headers. Dispatching stream 1's reset must not block on its queue.
        result <- try (send request (const (pure ())))
        expectReset result
        firstResult <- try (inpObjBody first >> pure ())
        expectReset firstResult
  where
    responses sid
      | sid == 1 = headers sid (BS.singleton 0x88)
          : replicate 64 (Frame (FrameHeader 1 FrameData 0 sid) (DataFrame "x"))
          ++ [reset sid]
      | otherwise = [reset sid]

withinDeadline :: IO () -> IO ()
withinDeadline action = do
  result <- timeout 5000000 action
  result `shouldBe` Just ()

expectReset :: Either ClientStreamError () -> IO ()
expectReset (Left (ClientStreamReset Cancel)) = pure ()
expectReset other = expectationFailure ("Expected CANCEL reset, got " ++ show other)

request :: OutObj
request = OutObj [(":method", "GET"), (":path", "/")] OutBodyNone defaultTrailersMaker

reset :: StreamId -> Frame
reset sid = Frame (FrameHeader 4 FrameRSTStream 0 sid) (RSTStreamFrame Cancel)

headers :: StreamId -> BS.ByteString -> Frame
headers sid block = Frame
  (FrameHeader (fromIntegral (BS.length block)) FrameHeaders flagEndHeaders sid)
  (HeadersFrame Nothing block)

-- Drive the real engine over a scripted byte peer. Replies become visible
-- only after request HEADERS have been published; STM provides all ordering.
withPeer
  :: (StreamId -> [Frame])
  -> ((forall r. OutObj -> (InpObj -> IO r) -> IO r) -> IO a)
  -> IO a
withPeer response action = do
  input <- newTVarIO BS.empty
  let send ptr len = do
        bytes <- BS.packCStringLen (castPtr ptr, len)
        case decodeFrameHeader bytes of
          Right h | fhType h == FrameHeaders -> atomically $
            modifyTVar' input (<> BS.concat (map encodeFrame (response (fhStreamId h))))
          _ -> pure ()
        pure len
      readN n = atomically $ do
        bytes <- readTVar input
        check (BS.length bytes >= n)
        let (prefix, rest) = BS.splitAt n bytes
        writeTVar input rest
        pure prefix
      env = RunEnv "localhost" send readN 65535 65535 (Just 100) "http"
  runClient env $ \sendRequest _ -> action sendRequest
