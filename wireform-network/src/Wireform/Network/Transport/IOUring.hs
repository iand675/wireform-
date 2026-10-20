{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Linux io_uring-based magic-ring receive transport.
--
-- Receives data directly into the magic ring buffer via io_uring
-- async recv.  Integrates with the GHC IO manager via eventfd.
--
-- __Requires__: Linux kernel 5.1+, liburing-dev.
-- Build with @cabal build -f iouring@.
module Wireform.Network.Transport.IOUring
  (
#if defined(HAVE_IOURING)
    withIOUringReceiveTransport
#endif
  ) where

#if defined(HAVE_IOURING)

import Control.Concurrent (threadWaitRead)
import Control.Exception (SomeException, bracket, toException, throwIO, try)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, mallocBytes, free, callocBytes)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek)
import Network.Socket (Socket, withFdSocket)
import System.Posix.Types (Fd (..))

import Wireform.Ring.Internal
import Wireform.Transport.Receive
import Wireform.Transport.Config

------------------------------------------------------------------------
-- C struct size — must match cbits/iouring_glue.c struct hs_iouring
------------------------------------------------------------------------

-- sizeof(struct io_uring) is ~160 bytes on x86_64 with liburing 2.x.
-- struct hs_iouring adds ~56 bytes of fields around it.
-- We allocate generously and zero-initialize.
hsIouringSize :: Int
hsIouringSize = 512

------------------------------------------------------------------------
-- C FFI bindings
------------------------------------------------------------------------

data IOUringState

foreign import ccall unsafe "hs_iouring_create"
  c_iouring_create :: CInt -> CInt -> CInt
                   -> Ptr Word8 -> CSize
                   -> Ptr IOUringState -> IO CInt

foreign import ccall unsafe "hs_iouring_submit_recv"
  c_iouring_submit_recv :: Ptr IOUringState -> CSize -> IO CInt

foreign import ccall safe "hs_iouring_wait_cqe"
  c_iouring_wait_cqe :: Ptr IOUringState -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "hs_iouring_peek_cqe"
  c_iouring_peek_cqe :: Ptr IOUringState -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "hs_iouring_get_eventfd"
  c_iouring_get_eventfd :: Ptr IOUringState -> IO CInt

foreign import ccall unsafe "hs_iouring_get_head"
  c_iouring_get_head :: Ptr IOUringState -> IO Word64

foreign import ccall unsafe "hs_iouring_destroy"
  c_iouring_destroy :: Ptr IOUringState -> IO ()

------------------------------------------------------------------------
-- Transport state
------------------------------------------------------------------------

data IOUringTState
  = IOpen
  | IClosedEof
  | IClosedErr !SomeException

------------------------------------------------------------------------
-- Transport implementation
------------------------------------------------------------------------

withIOUringReceiveTransport :: TransportConfig -> Socket -> (ReceiveTransport -> IO a) -> IO a
withIOUringReceiveTransport cfg sock action =
  withMagicRing (ringSizeHint cfg) \ring -> do
    let !base = ringBase ring
        !sz   = ringSize ring
        !msk  = ringMask ring
        !depth = ioUringQueueDepth (ioUring cfg)
        !sqpollIdle = case ioUringSQPoll (ioUring cfg) of
              NoSQPoll -> 0
              SQPollWithIdle ms -> ms

    -- Allocate C-side state (zero-initialized)
    bracket (callocBytes hsIouringSize) free \rawPtr -> do
      let uringPtr = castPtr rawPtr :: Ptr IOUringState

      -- Create io_uring instance
      rc <- withFdSocket sock \fd ->
        c_iouring_create (fromIntegral fd)
                         (fromIntegral depth)
                         (fromIntegral sqpollIdle)
                         base (fromIntegral sz)
                         uringPtr
      if rc < 0
        then throwIO (userError $ "io_uring_create failed with code " <> show rc)
        else do
          destroyedRef <- newIORef False
          let destroy = do
                alreadyDestroyed <- readIORef destroyedRef
                if alreadyDestroyed
                  then pure ()
                  else do
                    writeIORef destroyedRef True
                    c_iouring_destroy uringPtr

          eventFd <- c_iouring_get_eventfd uringPtr
          stateRef <- newIORef IOpen
          tailRef  <- newIORef (0 :: Word64)

          -- Whether a recv SQE is outstanding. A completion whose ring
          -- space is all unconsumed data submits no replacement, so a
          -- later wait has to submit one itself or it would park on an
          -- eventfd no completion can ever signal.
          pendingRef <- newIORef False

          -- Submit initial recv SQE
          initRc <- c_iouring_submit_recv uringPtr (fromIntegral sz)
          if initRc < 0
            then writeIORef stateRef (IClosedErr (toException (userError
                   ("io_uring submit failed with code " <> show initRc))))
            else writeIORef pendingRef True

          let loadHead :: IO Word64
              loadHead = c_iouring_get_head uringPtr

              advanceTail :: Word64 -> IO ()
              advanceTail pos = writeIORef tailRef pos

              -- Ring space the consumer has freed at head `h`.
              ringSpace :: Word64 -> IO Word64
              ringSpace h = do
                t <- readIORef tailRef
                let !size' = fromIntegral sz :: Word64
                    !used = if h > t then h - t else 0
                pure (if used < size' then size' - used else 0)

              -- Keep exactly one recv in flight while there is space for
              -- one; a submit failure is reported as a transport error
              -- rather than left to hang the next wait.
              submitIfIdle :: Word64 -> IO ()
              submitIfIdle h = do
                pending <- readIORef pendingRef
                unless pending $ do
                  space <- ringSpace h
                  when (space > 0) $ do
                    rc' <- submitRetrying (fromIntegral space)
                    if rc' < 0
                      then writeIORef stateRef
                             (IClosedErr (toException (userError
                               ("io_uring submit failed with code " <> show rc'))))
                      else writeIORef pendingRef True

              submitRetrying :: CSize -> IO CInt
              submitRetrying n = go2
                where
                  go2 = do
                    rc' <- c_iouring_submit_recv uringPtr n
                    -- -EINTR: the SQE is still queued; retry the submit.
                    if rc' == eIntr then go2 else pure rc'

              eIntr :: CInt
              eIntr = -4

              waitData :: Word64 -> IO ReceiveWait
              waitData pos = doWait pos

              doWait :: Word64 -> IO ReceiveWait
              doWait pos = do
                st <- readIORef stateRef
                case st of
                  IClosedEof   -> pure ReceiveEndOfInput
                  IClosedErr e -> pure (ReceiveFailed e)
                  IOpen        -> do
                    h <- loadHead
                    if h > pos
                      then pure (ReceiveMoreData h)
                      else do
                        submitIfIdle h
                        st' <- readIORef stateRef
                        case st' of
                          IClosedErr e -> pure (ReceiveFailed e)
                          _ -> alloca \headPtr -> do
                            -- Try non-blocking peek first; parking is only
                            -- safe once something can complete, which
                            -- submitIfIdle just ensured.
                            peekRc <- c_iouring_peek_cqe uringPtr headPtr
                            if peekRc > 0
                              then handleCompletion headPtr
                              else if peekRc == 0
                                then do
                                  writeIORef stateRef IClosedEof
                                  pure ReceiveEndOfInput
                                else do
                                  -- No completion ready: park until the
                                  -- kernel signals the eventfd (which
                                  -- consume_cqe drains), then look again.
                                  threadWaitRead (Fd eventFd)
                                  doWait pos

              handleCompletion :: Ptr Word64 -> IO ReceiveWait
              handleCompletion headPtr = do
                newH <- peek headPtr
                writeIORef pendingRef False
                -- Submit a replacement recv bounded by ring space; when
                -- the consumer lags a whole ring there is none, and the
                -- next wait submits one after the tail moves.
                submitIfIdle newH
                pure (ReceiveMoreData newH)

              transport = ReceiveTransport
                { receiveRingBase    = base
                , receiveRingSize    = sz
                , receiveRingMask    = msk
                , receiveLoadHead    = loadHead
                , receiveAdvanceTail = advanceTail
                , receiveWaitData    = waitData
                , receiveClose       = do
                    writeIORef stateRef IClosedEof
                    destroy
                }

          action transport `finally` destroy
  where
    finally :: IO a -> IO () -> IO a
    finally a cleanup = do
      r <- try @SomeException a
      cleanup
      case r of
        Left e  -> throwIO e
        Right v -> pure v

    when :: Bool -> IO () -> IO ()
    when True  m = m
    when False _ = pure ()

    unless :: Bool -> IO () -> IO ()
    unless cond m = when (not cond) m

    void :: IO a -> IO ()
    void m = m >> pure ()

#endif
