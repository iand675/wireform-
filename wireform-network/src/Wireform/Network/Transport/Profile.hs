{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

{- | Profile realization: turns a 'Profile' into concrete OS-level
tuning actions (CPU pinning, huge pages, mlock, NUMA placement).
-}
module Wireform.Network.Transport.Profile (
  realizeProfile,
  applyPinning,
  applyMemoryLocking,
) where

import Control.Exception (SomeException, try)
import Data.Word (Word8)
import Foreign.C.Types (CSize (..))
import Foreign.Ptr (Ptr)
import System.IO (hPutStrLn, stderr)
import Wireform.Transport.Capabilities
import Wireform.Transport.Config


{- | Given a profile and detected capabilities, produce a config
that respects what the system can actually do.
-}
realizeProfile :: Profile -> SystemCapabilities -> TransportConfig
realizeProfile p caps = adjustForCapabilities caps (profileConfig p)


adjustForCapabilities :: SystemCapabilities -> TransportConfig -> TransportConfig
adjustForCapabilities caps cfg =
  cfg
    { pages = adjustPages caps (pages cfg)
    , ioUring = adjustIOUring caps (ioUring cfg)
    }


adjustPages :: SystemCapabilities -> PagePolicy -> PagePolicy
adjustPages caps RequireHugePages
  | null (capHugePageSizes caps) = StandardPages
adjustPages _ p = p


adjustIOUring :: SystemCapabilities -> IOUringConfig -> IOUringConfig
adjustIOUring caps ioc =
  ioc
    { ioUringProvidedBuffers =
        ioUringProvidedBuffers ioc
          && ioUringFeatureProvidedBuffers (capIOUringFeatures caps)
    , ioUringSQPoll =
        if ioUringFeatureSQPoll (capIOUringFeatures caps)
          then ioUringSQPoll ioc
          else NoSQPoll
    }


------------------------------------------------------------------------
-- Pinning
------------------------------------------------------------------------

-- | Apply the configured pinning policy to the current thread.
applyPinning :: PinningPolicy -> CapabilityAction -> IO ()
applyPinning NoPinning _ = pure ()
applyPinning (PinToCore core) onFail = pinToCore core onFail
applyPinning (PinToCoreSet cores) onFail =
  case cores of
    (c : _) -> pinToCore c onFail
    [] -> pure ()
applyPinning PinNearFd _onFail = pure ()
applyPinning PinIsolated onFail = do
  caps <- detectCapabilities
  case capIsolatedCores caps of
    (c : _) -> pinToCore c onFail
    [] -> warnCapability onFail "no isolated cores available"


pinToCore :: Int -> CapabilityAction -> IO ()
#if defined(linux_HOST_OS)
pinToCore core onFail = do
  result <- try @SomeException (c_pin_thread core)
  case result of
    Right 0 -> pure ()
    _       -> warnCapability onFail ("failed to pin to core " <> show core)
#else
pinToCore _core onFail =
  warnCapability onFail "CPU pinning not supported on this platform"
#endif

#if defined(linux_HOST_OS)
foreign import ccall unsafe "sched_setaffinity_single"
  c_pin_thread :: Int -> IO Int
#endif


------------------------------------------------------------------------
-- Memory locking
------------------------------------------------------------------------

-- | Lock the ring buffer's memory pages (prevent swapping).
applyMemoryLocking :: Ptr Word8 -> Int -> CapabilityAction -> IO ()
applyMemoryLocking ptr size onFail = do
#if defined(linux_HOST_OS) || defined(darwin_HOST_OS) || defined(freebsd_HOST_OS)
  result <- try @SomeException (c_mlock ptr (fromIntegral size))
  case result of
    Right 0 -> pure ()
    _       -> warnCapability onFail "mlock failed (check RLIMIT_MEMLOCK)"
#else
  warnCapability onFail "mlock not supported on this platform"
#endif

#if defined(linux_HOST_OS) || defined(darwin_HOST_OS) || defined(freebsd_HOST_OS)
-- mlock's second parameter is size_t. `size` is a byte count (the ring
-- buffer's length, never negative), so Int -> CSize cannot narrow.
foreign import ccall unsafe "mlock"
  c_mlock :: Ptr Word8 -> CSize -> IO Int
#endif


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

warnCapability :: CapabilityAction -> String -> IO ()
warnCapability SilentlyIgnore _ = pure ()
warnCapability LogAndContinue msg =
  hPutStrLn stderr ("[wireform] " <> msg)
warnCapability FailHard msg = error ("[wireform] " <> msg)
