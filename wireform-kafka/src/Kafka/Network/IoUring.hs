{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Network.IoUring
Description : io_uring-backed I/O for the Kafka client (Linux 5.1+)
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

Optional io_uring backend for the broker socket layer.
Replaces the @recv@/@send@ syscall pair that the GHC RTS
otherwise issues (via its epoll-based IO manager) with a single
@io_uring_enter@ per round-trip.

The benefit is workload-shaped:

  * High-rate small ops (consumer heartbeats, idempotent
    producer @InitProducerId@/@AddPartitions@ chatter,
    transactional coordinator pings) see ~1 syscall less per
    request, which matters when each request is a few hundred
    bytes.
  * Bulk produce/fetch traffic is dominated by copy cost, not
    syscall cost; io_uring still helps marginally by avoiding
    the epoll round-trip but the upside is smaller.

The integration here is /opt-in/ and /runtime-detected/. To
enable it:

  1. Build wireform-kafka with the @io-uring@ cabal flag
     (@cabal build -fio-uring@). Without the flag the FFI is
     still wired but every call goes through the
     @wireform_iouring_stub.c@ implementation that reports
     "not supported".
  2. At runtime, call 'isIoUringSupported' to confirm the
     kernel accepts @io_uring_setup@. Hardened kernels and
     containers running with @kernel.io_uring_disabled=2@
     (Linux 6.6+) will report 'False'.
  3. Open a ring with 'withIoUringRing'.
  4. Wrap a connected 'NS.Socket' as a
     'Kafka.Network.Transport.Transport' via
     'mkIoUringTransport'. The resulting 'Transport' is a
     drop-in for the in-memory pipe transport used by the test
     suite.

The deeper integration — feeding this transport into
'Kafka.Client.Pipeline' so the consumer / producer hot path
benefits — is staged behind the same flag and tracked
separately.

== Thread safety

The C shim shares a single submission slot per ring, so the
Haskell side serialises every call through an 'MVar'. Multiple
Haskell threads that want concurrent io_uring submissions
should each hold their own ring (this is also how the kernel
intends @io_uring@ to be used — rings are cheap, ~16 KiB of
mmap'd memory each).
-}
module Kafka.Network.IoUring
  ( -- * Availability
    isIoUringSupported
    -- * Ring lifecycle
  , IoUringRing
  , openIoUringRing
  , closeIoUringRing
  , withIoUringRing
    -- * Transport
  , mkIoUringTransport
    -- * Errors
  , IoUringError (..)
  ) where

import Control.Concurrent.MVar
import Control.Exception (Exception, bracket, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.C.Error (Errno (..), errnoToIOError)
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import qualified Network.Socket as NS

import qualified Kafka.Network.Transport as Transport

----------------------------------------------------------------------
-- FFI
----------------------------------------------------------------------

-- | Opaque C-side ring handle. We never look at the bytes;
-- the C shim owns the lifecycle.
data CRing

foreign import ccall unsafe "wireform_iouring.h wf_iouring_is_supported"
  c_wf_iouring_is_supported :: IO CInt

foreign import ccall unsafe "wireform_iouring.h wf_iouring_open"
  c_wf_iouring_open :: CUInt -> IO (Ptr CRing)

foreign import ccall unsafe "wireform_iouring.h wf_iouring_close"
  c_wf_iouring_close :: Ptr CRing -> IO ()

-- | @send@ is marked 'safe' so the GHC RTS releases the
-- capability while the io_uring_enter call blocks waiting on
-- the CQE. Without that the entire HEC would stall on a slow
-- broker write.
foreign import ccall safe "wireform_iouring.h wf_iouring_send"
  c_wf_iouring_send
    :: Ptr CRing -> CInt -> Ptr Word8 -> CSize -> IO CLLong

foreign import ccall safe "wireform_iouring.h wf_iouring_recv"
  c_wf_iouring_recv
    :: Ptr CRing -> CInt -> Ptr Word8 -> CSize -> IO CLLong

----------------------------------------------------------------------
-- Availability
----------------------------------------------------------------------

-- | Does the running kernel accept @io_uring_setup@?
--
-- Returns 'False' on:
--
--   * any non-Linux host (the stub C file is compiled);
--   * Linux <5.1 (no syscall);
--   * Linux \>=6.6 with @sysctl kernel.io_uring_disabled=2@;
--   * any host where seccomp / AppArmor / SELinux denies
--     @io_uring_setup@.
--
-- The probe opens a 1-entry ring and immediately closes it, so
-- the check is cheap enough to do at client startup but not so
-- cheap you want to call it in a hot loop.
isIoUringSupported :: IO Bool
isIoUringSupported = do
  r <- c_wf_iouring_is_supported
  pure (r /= 0)

----------------------------------------------------------------------
-- Ring
----------------------------------------------------------------------

-- | A handle for one io_uring ring plus the access mutex that
-- serialises submissions through the single-slot C shim.
data IoUringRing = IoUringRing
  { iurPtr  :: !(Ptr CRing)
  , iurLock :: !(MVar ())
  }

-- | Allocate a fresh ring with @entries@ submission slots
-- (rounded up to a power of two by the kernel). 8 is a fine
-- default; raising it only matters when this module gains a
-- batched API.
--
-- Throws 'IoUringError' on failure — either the kernel
-- doesn't support io_uring at all (use 'isIoUringSupported' to
-- check first) or the process is out of file descriptors / mmap
-- regions.
openIoUringRing :: Int -> IO IoUringRing
openIoUringRing entries = do
  p <- c_wf_iouring_open (fromIntegral entries)
  if p == nullPtr
    then throwIO IoUringRingSetupFailed
    else do
      lock <- newMVar ()
      pure IoUringRing { iurPtr = p, iurLock = lock }

closeIoUringRing :: IoUringRing -> IO ()
closeIoUringRing IoUringRing{iurPtr} = c_wf_iouring_close iurPtr

withIoUringRing :: Int -> (IoUringRing -> IO a) -> IO a
withIoUringRing entries = bracket (openIoUringRing entries) closeIoUringRing

----------------------------------------------------------------------
-- Transport
----------------------------------------------------------------------

-- | Wrap a connected 'NS.Socket' as a
-- 'Kafka.Network.Transport.Transport' whose read / write
-- operations go through io_uring rather than the regular @recv@
-- / @send@ syscalls.
--
-- The caller retains ownership of the socket — closing the
-- 'Transport' /also/ closes the socket so the kernel reclaims
-- the fd promptly. The 'IoUringRing' is not closed by
-- 'transportClose' (it can be shared between transports — call
-- 'closeIoUringRing' yourself when done).
mkIoUringTransport
  :: IoUringRing
  -> NS.Socket
  -> String            -- ^ display name (for log lines)
  -> IO Transport.Transport
mkIoUringTransport ring sock label = do
  -- Cache the file descriptor; 'NS.withFdSocket' takes the
  -- per-socket lock each call, which we don't want on the hot
  -- path. The socket isn't shared between threads in our usage
  -- so caching is safe.
  fd <- NS.unsafeFdSocket sock
  let cfd = fromIntegral fd :: CInt
  pure Transport.Transport
    { Transport.transportName  = label
    , Transport.transportRead  = doRead ring cfd
    , Transport.transportWrite = doWrite ring cfd
    , Transport.transportClose = NS.close sock
    }

-- | Read up to @n@ bytes via io_uring. Returns an empty
-- 'ByteString' on EOF, matching the contract of the other
-- 'Transport' implementations.
doRead :: IoUringRing -> CInt -> Int -> IO (Either String ByteString)
doRead ring cfd n
  | n <= 0    = pure (Right BS.empty)
  | otherwise = withRingLock ring $ \rp -> do
      buf <- mallocBytes n
      r <- c_wf_iouring_recv rp cfd buf (fromIntegral n)
      case () of
        _ | r > 0 -> do
              -- Take a copy into a managed ByteString and free
              -- the raw buffer. The single extra copy is the
              -- price of not exposing the FFI buffer to GC.
              bs <- BS.packCStringLen (castPtr buf, fromIntegral r)
              free buf
              pure (Right bs)
          | r == 0 -> do
              free buf
              pure (Right BS.empty)   -- EOF
          | otherwise -> do
              free buf
              pure (Left (errnoString (negate (fromIntegral r)) "io_uring recv"))

-- | Write the full payload. The CQE we get back tells us how
-- many bytes the kernel actually shipped; if it's a short write
-- we loop until everything is on the wire (mirroring the
-- semantics of 'Network.Connection.connectionPut').
doWrite :: IoUringRing -> CInt -> ByteString -> IO (Either String ())
doWrite ring cfd bs = withRingLock ring $ \rp ->
  -- Pin the bytestring's payload so the kernel can DMA out of
  -- it directly. 'BSU.unsafeUseAsCStringLen' gives us the
  -- underlying buffer + length without copying.
  BSU.unsafeUseAsCStringLen bs $ \(ptr0, len0) ->
    loop rp (castPtr ptr0) len0
  where
    loop :: Ptr CRing -> Ptr Word8 -> Int -> IO (Either String ())
    loop _  _   0   = pure (Right ())
    loop rp ptr len = do
      r <- c_wf_iouring_send rp cfd ptr (fromIntegral len)
      let written = fromIntegral r :: Int
      case () of
        _ | r > 0 ->
              loop rp (ptr `plusPtr` written) (len - written)
          | r == 0 -> pure (Left "io_uring send: zero-byte write (peer closed?)")
          | otherwise ->
              pure (Left (errnoString (negate (fromIntegral r)) "io_uring send"))

----------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------

withRingLock :: IoUringRing -> (Ptr CRing -> IO a) -> IO a
withRingLock IoUringRing{iurPtr, iurLock} act =
  withMVar iurLock (\_ -> act iurPtr)

-- | Render a positive errno as a short error string. We use
-- 'errnoToIOError' under the hood so callers see the same
-- text the GHC base library would produce for a failed syscall.
errnoString :: CInt -> String -> String
errnoString errno ctx =
  let ioe = errnoToIOError ctx (Errno errno) Nothing Nothing
   in ctx <> ": " <> show ioe

----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

data IoUringError
  = IoUringRingSetupFailed
    -- ^ Kernel rejected our 'io_uring_setup' call. The most
    --   likely causes are documented on 'isIoUringSupported'.
  deriving stock (Show)

instance Exception IoUringError
