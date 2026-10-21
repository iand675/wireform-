module Wireform.Transport.Config (
  -- * Profiles
  Profile (..),
  profileConfig,

  -- * Full configuration
  TransportConfig (..),
  defaultTransportConfig,

  -- * Wait policy
  WaitPolicy (..),
  SpinBudget (..),

  -- * Pinning
  PinningPolicy (..),

  -- * Pages
  PagePolicy (..),

  -- * NUMA
  NumaPolicy (..),

  -- * io_uring
  IOUringConfig (..),
  SQPollPolicy (..),
  CompletionWaitMode (..),
  defaultIOUringConfig,

  -- * Capability handling
  CapabilityAction (..),
) where


------------------------------------------------------------------------
-- Profiles
------------------------------------------------------------------------

{- | High-level performance profile.  Most users pick one of these
and never touch 'TransportConfig' directly.
-}
data Profile
  = -- | IO-manager-parked.  Good citizen on shared systems.
    Throughput
  | -- | Brief spin before parking, pinned thread, huge pages where available.
    LowLatency
  | {- | Pure busy-poll, pinned to isolated core, huge pages, mlock.
    Burns one CPU core permanently.
    -}
    UltraLowLatency
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)


-- | Expand a profile into a full 'TransportConfig'.
profileConfig :: Profile -> TransportConfig
profileConfig Throughput = defaultTransportConfig
profileConfig LowLatency =
  defaultTransportConfig
    { waitPolicy = WaitSpinThenPark (SpinNanos 5000)
    , pinning = PinNearFd
    , pages = PreferHugePages
    , memoryLocking = True
    , numaPlacement = NumaAutoFromFd
    }
profileConfig UltraLowLatency =
  defaultTransportConfig
    { waitPolicy = WaitBusyPoll
    , pinning = PinIsolated
    , pages = PreferHugePages
    , memoryLocking = True
    , numaPlacement = NumaAutoFromFd
    , ioUring =
        defaultIOUringConfig
          { ioUringQueueDepth = 1024
          , ioUringSQPoll = SQPollWithIdle 100
          , ioUringCompletionWait = WaitViaBusyPoll
          }
    }


------------------------------------------------------------------------
-- Full configuration
------------------------------------------------------------------------

data TransportConfig = TransportConfig
  { ringSizeHint :: !Int
  {- ^ Requested ring size in bytes.  Rounded up to page-size multiple
  and power of two.  Default: 1 MiB.  The ring is mapped twice, so the
  ceiling is 4 GiB (1 GiB on a 32-bit target); a larger request fails
  with @ENOMEM@ instead of being silently clamped.
  -}
  , waitPolicy :: !WaitPolicy
  , pinning :: !PinningPolicy
  , pages :: !PagePolicy
  , memoryLocking :: !Bool
  -- ^ @mlock@ the ring.  Requires sufficient RLIMIT_MEMLOCK.
  , numaPlacement :: !NumaPolicy
  , ioUring :: !IOUringConfig
  -- ^ Linux io_uring tuning (silently ignored elsewhere).
  , onCapabilityLimit :: !CapabilityAction
  -- ^ What to do when a knob cannot be applied on this platform.
  }
  deriving stock (Show)


defaultTransportConfig :: TransportConfig
defaultTransportConfig =
  TransportConfig
    { ringSizeHint = 1024 * 1024
    , waitPolicy = WaitParkImmediately
    , pinning = NoPinning
    , pages = StandardPages
    , memoryLocking = False
    , numaPlacement = NoNumaPreference
    , ioUring = defaultIOUringConfig
    , onCapabilityLimit = LogAndContinue
    }


------------------------------------------------------------------------
-- Wait policy
------------------------------------------------------------------------

data WaitPolicy
  = -- | Park on the IO manager as soon as caught up.
    WaitParkImmediately
  | -- | Busy-spin for the budget, then park.
    WaitSpinThenPark !SpinBudget
  | -- | Never park.  Burns a core.
    WaitBusyPoll
  deriving stock (Show)


data SpinBudget
  = SpinIterations !Int
  | SpinNanos !Int
  | SpinUntilPaused
  deriving stock (Show)


------------------------------------------------------------------------
-- Pinning
------------------------------------------------------------------------

data PinningPolicy
  = NoPinning
  | PinToCore !Int
  | PinToCoreSet ![Int]
  | -- | Auto-detect: pin near the NIC / fd's NUMA node.
    PinNearFd
  | -- | Pick an isolated core (Linux @isolcpus=@); fall back to 'PinNearFd'.
    PinIsolated
  deriving stock (Show)


------------------------------------------------------------------------
-- Page policy
------------------------------------------------------------------------

data PagePolicy
  = StandardPages
  | -- | Request huge pages; fall back to standard if unavailable.
    PreferHugePages
  | -- | Fail at ring creation if huge pages are unavailable.
    RequireHugePages
  deriving stock (Show)


------------------------------------------------------------------------
-- NUMA
------------------------------------------------------------------------

data NumaPolicy
  = NoNumaPreference
  | NumaNode !Int
  | NumaAutoFromFd
  | NumaAutoFromCurrentCore
  deriving stock (Show)


------------------------------------------------------------------------
-- io_uring
------------------------------------------------------------------------

data IOUringConfig = IOUringConfig
  { ioUringQueueDepth :: !Int
  , ioUringSQPoll :: !SQPollPolicy
  , ioUringProvidedBuffers :: !Bool
  , ioUringCompletionWait :: !CompletionWaitMode
  }
  deriving stock (Show)


defaultIOUringConfig :: IOUringConfig
defaultIOUringConfig =
  IOUringConfig
    { ioUringQueueDepth = 128
    , ioUringSQPoll = NoSQPoll
    , ioUringProvidedBuffers = True
    , ioUringCompletionWait = WaitViaEventFd
    }


data SQPollPolicy
  = NoSQPoll
  | SQPollWithIdle !Int
  deriving stock (Show)


data CompletionWaitMode
  = WaitViaEventFd
  | WaitViaBusyPoll
  deriving stock (Show)


------------------------------------------------------------------------
-- Capability action
------------------------------------------------------------------------

data CapabilityAction
  = SilentlyIgnore
  | LogAndContinue
  | FailHard
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)
