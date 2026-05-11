{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the io_uring transport
-- (@Kafka.Network.IoUring@).
--
-- Two scenarios:
--
--   * 'isIoUringSupported' is allowed to return either value
--     and the test never fails for "the kernel didn't accept
--     io_uring_setup" — that's a deployment property, not a
--     code property. We only assert the function returns
--     without throwing.
--   * If support /is/ available, we open a one-entry ring,
--     hand it both ends of a loopback TCP pair via
--     'mkIoUringTransport', round-trip a few bytes through it,
--     and assert each side sees the other's payload. This
--     exercises both the recv and send io_uring paths through
--     real syscalls.
--
-- When the @io-uring@ cabal flag is off, this module is still
-- compiled (it's listed in @other-modules@), but every case
-- is a no-op asserting the obvious. We do this so the test
-- suite layout is identical between the two build profiles
-- and CI doesn't have to thread the flag through.
module Network.IoUringSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

#ifdef WIREFORM_HAS_IOURING

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import qualified Network.Socket as Socket
import Test.Tasty.HUnit ((@?=), assertFailure)

import qualified Kafka.Network.IoUring as IoUring
import qualified Kafka.Network.Transport as Transport

#endif

tests :: TestTree
tests = testGroup "Kafka.Network.IoUring"
  [ testCase "isIoUringSupported returns a Bool without throwing"
      probe_does_not_throw
  , testCase "loopback round-trip via io_uring transport (if supported)"
      loopback_round_trip
  ]

#ifdef WIREFORM_HAS_IOURING

probe_does_not_throw :: IO ()
probe_does_not_throw = do
  -- We don't assert on the value: support depends on the
  -- kernel version + sysctl + sandbox profile, none of which
  -- the test can control. The point is that the FFI call
  -- itself doesn't blow up.
  _b <- IoUring.isIoUringSupported
  pure ()

loopback_round_trip :: IO ()
loopback_round_trip = do
  ok <- IoUring.isIoUringSupported
  if not ok
    then -- Kernel said no. Treat as a soft pass so the suite
         -- stays green on hardened CI runners with
         -- @kernel.io_uring_disabled@ set.
         pure ()
    else IoUring.withIoUringRing 8 $ \ring -> do
      (clientSock, serverSock) <- loopbackPair
      bracket
        (pure ())
        (\_ -> do
            _ <- try (Socket.close clientSock) :: IO (Either SomeException ())
            _ <- try (Socket.close serverSock) :: IO (Either SomeException ())
            pure ())
        $ \_ -> do
          cliT <- IoUring.mkIoUringTransport ring clientSock "iour:client"
          srvT <- IoUring.mkIoUringTransport ring serverSock "iour:server"

          -- Client → server.
          Right () <- Transport.transportWrite cliT "io_uring-hello"
          got1 <- awaitTransportBytes srvT (BS.length "io_uring-hello")
          got1 @?= "io_uring-hello"

          -- Server → client.
          Right () <- Transport.transportWrite srvT "back-at-you"
          got2 <- awaitTransportBytes cliT (BS.length "back-at-you")
          got2 @?= "back-at-you"

-- | Drain reads until we've accumulated at least @n@ bytes.
-- Each 'transportRead' returns whatever the kernel had ready,
-- so partial reads are expected on a fresh socket pair.
awaitTransportBytes :: Transport.Transport -> Int -> IO BS.ByteString
awaitTransportBytes t n = go BS.empty
  where
    go acc
      | BS.length acc >= n = pure acc
      | otherwise = do
          r <- Transport.transportRead t (n - BS.length acc)
          case r of
            Right bs
              | BS.null bs -> assertFailure "unexpected EOF on io_uring transport"
              | otherwise  -> go (acc <> bs)
            Left err -> assertFailure ("io_uring transport read failed: " <> err)

-- | Build a (client, server) socket pair connected via
-- loopback TCP. We don't use 'Socket.socketPair' / @AF_UNIX@
-- here because the io_uring SEND opcode is happiest on a
-- stream socket bound through accept (the kernel takes a
-- slightly different path otherwise).
loopbackPair :: IO (Socket.Socket, Socket.Socket)
loopbackPair = do
  srv <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.setSocketOption srv Socket.ReuseAddr 1
  Socket.bind srv
    (Socket.SockAddrInet 0
       (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen srv 1
  sa <- Socket.getSocketName srv

  acceptedVar <- newEmptyTMVarIO
  _ <- forkIO $ do
    r <- try (Socket.accept srv) :: IO (Either SomeException (Socket.Socket, Socket.SockAddr))
    Socket.close srv
    case r of
      Right (cli, _) -> atomically (putTMVar acceptedVar (Right cli))
      Left e         -> atomically (putTMVar acceptedVar (Left (show e)))

  cli <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.connect cli sa
  ar <- atomically (takeTMVar acceptedVar)
  case ar of
    Right server -> pure (cli, server)
    Left  err    -> do
      Socket.close cli
      error ("loopbackPair: accept failed: " <> err)

#else  -- !WIREFORM_HAS_IOURING

probe_does_not_throw :: IO ()
probe_does_not_throw =
  -- Flag off — the module isn't part of the build. The test
  -- exists so the @other-modules@ list is identical across
  -- both build profiles.
  assertBool "io-uring build flag is off; skipping" True

loopback_round_trip :: IO ()
loopback_round_trip =
  assertBool "io-uring build flag is off; skipping" True

#endif
