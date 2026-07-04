{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Paired send + receive magic-ring transport over one underlying
byte stream.  This is the shape downstream connection-layer
objects (the Kafka 'Connection', the HTTP/1 @Connection@, the
HTTP/2 @Connection@) build on.

One 'DuplexTransport' owns two magic rings (one per direction)
and one underlying fd / TLS context.  The two rings are
independent — sizing, backpressure, half-close — but the close
action ties them together so callers don't have to remember to
close each side independently.
-}
module Wireform.Network.Transport.Duplex (
  DuplexTransport (..),
  withDuplexTransport,
  newDuplexTransport,
  withDuplexBufTransport,
  newDuplexBufTransport,
  newDuplexBufTransportPooled,
  closeDuplexTransport,
) where

import Control.Exception (SomeException, try)
import Control.Monad (join)
import Data.IORef (atomicModifyIORef', newIORef)
import Network.Socket (Socket)
import Network.Socket qualified as S
import Wireform.Network.Transport.Receive (
  ReceiveFn,
  buildReceiveTransport,
 )
import Wireform.Network.Transport.Send (
  SendFn,
  buildSendTransport,
 )
import Wireform.Ring.Internal
import Wireform.Ring.Pool (RingPool, acquireRing, releaseRing)
import Wireform.Transport.Config
import Wireform.Transport.Receive
import Wireform.Transport.Send


{- | A bidirectional magic-ring transport.

Owns two independent magic rings + the underlying byte stream.
'duplexClose' tears down both rings, half-closes the wire (in
the appropriate direction for the underlying transport), and is
idempotent.
-}
data DuplexTransport = DuplexTransport
  { duplexReceive :: !ReceiveTransport
  , duplexSend :: !SendTransport
  , duplexClose :: !(IO ())
  }


{- | Bracket-scoped duplex transport over a 'Socket'.  The socket
is NOT closed by 'duplexClose'; caller owns its lifetime
(same convention as 'withReceiveTransport').
-}
withDuplexTransport
  :: TransportConfig
  -> Socket
  -> (DuplexTransport -> IO a)
  -> IO a
withDuplexTransport cfg sock action =
  withDuplexBufTransport
    cfg
    (\p n -> S.recvBuf sock p n)
    (\p n -> S.sendBuf sock p n)
    (S.shutdown sock S.ShutdownSend)
    action


{- | IO-style duplex over a 'Socket'.  Caller is responsible for
'closeDuplexTransport' (releases both rings) and for closing the
socket separately.
-}
newDuplexTransport :: TransportConfig -> Socket -> IO DuplexTransport
newDuplexTransport cfg sock =
  newDuplexBufTransport
    cfg
    (\p n -> S.recvBuf sock p n)
    (\p n -> S.sendBuf sock p n)
    (S.shutdown sock S.ShutdownSend)


{- | Bracket-scoped duplex over an arbitrary 'ReceiveFn' + 'SendFn'
pair — the path TLS / in-memory / test fixtures take.
-}
withDuplexBufTransport
  :: TransportConfig
  -> ReceiveFn
  -> SendFn
  -> IO ()
  -- ^ shutdownWrite (e.g. SHUT_WR / TLS close_notify)
  -> (DuplexTransport -> IO a)
  -> IO a
withDuplexBufTransport cfg recvFn sendFn shut action =
  withMagicRing (ringSizeHint cfg) \rxRing ->
    withMagicRing (ringSizeHint cfg) \txRing -> do
      rx <- buildReceiveTransport rxRing recvFn
      tx <- buildSendTransport txRing sendFn shut
      let d =
            DuplexTransport
              { duplexReceive = rx
              , duplexSend = tx
              , duplexClose = closeBoth tx rx
              }
      action d


{- | IO-style duplex over an arbitrary 'ReceiveFn' + 'SendFn' pair.
'closeDuplexTransport' also unmaps the two rings.
-}
newDuplexBufTransport
  :: TransportConfig
  -> ReceiveFn
  -> SendFn
  -> IO ()
  -> IO DuplexTransport
newDuplexBufTransport cfg recvFn sendFn shut = do
  rxRing <- newMagicRing (ringSizeHint cfg)
  txRing <- newMagicRing (ringSizeHint cfg)
  rx0 <- buildReceiveTransport rxRing recvFn
  tx0 <- buildSendTransport txRing sendFn shut
  -- Guard the ring teardown so it fires at most once even under
  -- re-entrant / concurrent close: 'destroyMagicRing' munmaps its base
  -- pointer unconditionally, so a second close would munmap an address
  -- that a fresh ring may already have been mapped at -> heap corruption
  -- / SIGSEGV. (Delivers the idempotency this module's haddock promises.)
  destroyRx <- mkDestroyOnce (destroyMagicRing rxRing)
  destroyTx <- mkDestroyOnce (destroyMagicRing txRing)
  let rx = rx0 {receiveClose = receiveClose rx0 *> destroyRx}
      tx = tx0 {sendClose = sendClose tx0 *> destroyTx}
  pure
    DuplexTransport
      { duplexReceive = rx
      , duplexSend = tx
      , duplexClose = closeBoth tx rx
      }


{- | Like 'newDuplexBufTransport' but acquires rings from a
'RingPool' instead of allocating fresh ones. On close, rings are
returned to the pool for reuse rather than destroyed.
-}
newDuplexBufTransportPooled
  :: RingPool
  -> TransportConfig
  -> ReceiveFn
  -> SendFn
  -> IO ()
  -> IO DuplexTransport
newDuplexBufTransportPooled pool cfg recvFn sendFn shut = do
  rxRing <- acquireRing pool (ringSizeHint cfg)
  txRing <- acquireRing pool (ringSizeHint cfg)
  rx0 <- buildReceiveTransport rxRing recvFn
  tx0 <- buildSendTransport txRing sendFn shut
  -- Release each ring back to the pool at most once (see 'newDuplexBufTransport');
  -- a double release would hand the same ring out twice.
  releaseRx <- mkDestroyOnce (releaseRing pool rxRing)
  releaseTx <- mkDestroyOnce (releaseRing pool txRing)
  let rx = rx0 {receiveClose = receiveClose rx0 *> releaseRx}
      tx = tx0 {sendClose = sendClose tx0 *> releaseTx}
  pure
    DuplexTransport
      { duplexReceive = rx
      , duplexSend = tx
      , duplexClose = closeBoth tx rx
      }


{- | Coordinated tear-down: flush the send ring (sendClose drains
everything published), signal end-of-write to the peer
(sendShutdownWrite fires shutdown(SHUT_WR) / close_notify / sets
the in-memory pipe's WR-closed flag — that's what makes the
peer's next recv return 0 = EOF), then release the receive ring.
Each step is best-effort + idempotent so re-entrancy on a failing
transport doesn't leave half-released state.
-}
closeBoth :: SendTransport -> ReceiveTransport -> IO ()
closeBoth tx rx = do
  _ <- try @SomeException (sendClose tx)
  _ <- try @SomeException (sendShutdownWrite tx)
  _ <- try @SomeException (receiveClose rx)
  pure ()

{- | Wrap a teardown action so it runs at most once, atomically, no matter
how many times (or from how many threads) the resulting action is invoked.
Used to make ring destroy / pool-release idempotent under re-entrant and
concurrent 'closeBoth'.
-}
mkDestroyOnce :: IO () -> IO (IO ())
mkDestroyOnce act = do
  ref <- newIORef False
  pure (join (atomicModifyIORef' ref (\done -> (True, if done then pure () else act))))


{- | Synonym for 'duplexClose' (record-field accesses can be awkward
at use sites; the standalone function reads cleaner).
-}
closeDuplexTransport :: DuplexTransport -> IO ()
closeDuplexTransport = duplexClose
