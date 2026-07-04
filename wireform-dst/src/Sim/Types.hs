{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The contract hub for @wireform-dst@: the identifier newtypes, the three
-- controllable 'Decision's, the adversary fault vocabulary ('FaultOp' /
-- 'FaultKind' / 'NemesisConfig'), the assertion kinds, the observable
-- 'SimEvent' stream, and the terminal 'FailReason'. Every other @Sim.*@
-- module imports this and nothing here imports them.
--
-- No logic lives here beyond deriving; the semantics of each type are
-- realized in "Sim.Interp" ('Decision', 'SimEvent'), "Sim.Fault"
-- ('FaultOp', 'FaultKind', 'NemesisConfig'), and "Sim.Assert"
-- ('AssertKind').
module Sim.Types
  ( -- * Identifiers
    NodeId (..)
  , FiberId (..)
  , MsgId (..)
  , SimTime (..)
  , Seed (..)
  , SiteId (..)
  , StateKey (..)
  , Key (..)

    -- * Decisions
  , Decision (..)
  , DecisionPath

    -- * Faults
  , FaultOp (..)
  , FaultKind (..)
  , NemesisConfig (..)

    -- * Assertions
  , AssertKind (..)

    -- * Events & failures
  , SimEvent (..)
  , FailReason (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.String (IsString)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | A simulated node (machine) identity.
newtype NodeId = NodeId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | A logical fiber (actor / green thread) identity.
newtype FiberId = FiberId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, ToJSON, FromJSON)

-- | An in-flight message identity.
newtype MsgId = MsgId Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, ToJSON, FromJSON)

-- | Virtual (simulated) time in nanoseconds. The sim never reads a wall clock.
newtype SimTime = SimTime Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | The PRNG seed a run is derived from.
newtype Seed = Seed Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, ToJSON, FromJSON)

-- | A stable call-site label (coverage edge, assertion, buggify site).
newtype SiteId = SiteId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

-- | An observation fingerprint (see 'Sim.Entropy.mixKey').
newtype StateKey = StateKey Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | A durable/volatile storage key on a node.
newtype Key = Key ByteString
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | The three controllable decisions the scheduler / search picks from.
--
--   * @'SchedNext' f@ — step the runnable fiber @f@ through its inline work
--     until it blocks or finishes (the yield-point rule).
--   * @'DeliverMsg' m@ — deliver the in-flight message @m@ to the waiting
--     @Recv@ on its destination node.
--   * @'FireFault' i@ — apply the @i@-th fault from
--     @'Sim.Fault.availableFaults' (wNemesis w) w@. That list is a pure
--     function of the 'Sim.World.World', so the index is replay-stable and
--     there is no stored fault registry.
--
-- Note: buggify enablement is /not/ a decision — it is drawn from the run
-- PRNG on first hit and overridden per-run by @RunInput.riBuggify@.
data Decision = SchedNext !FiberId | DeliverMsg !MsgId | FireFault !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | An ordered list of decisions; the reproducible spine of a run.
type DecisionPath = [Decision]

-- | A concrete adversary action applied to the 'Sim.World.World'.
data FaultOp
  = DropMessage !MsgId
  | Partition !NodeId !NodeId -- ^ directed cut (asymmetric)
  | Heal !NodeId !NodeId
  | Clog !NodeId !NodeId !SimTime -- ^ freeze link until the given time
  | CrashNode !NodeId -- ^ drop volatile state + un-fsynced writes
  | RebootNode !NodeId -- ^ restart from durable store
  | PauseNode !NodeId !SimTime
  | ClockSkew !NodeId !Int -- ^ signed ns offset applied to node clock
  | CorruptWrite !NodeId -- ^ flip a bit in the next durable write
  | TearWrite !NodeId -- ^ next write stays volatile (lost on crash)
  | DiskLatency !NodeId !SimTime
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The kind of adversary move, used to gate what the nemesis may propose
-- ('NemesisConfig') and to key the UCB1 bandit in "Sim.Search".
data FaultKind
  = KDrop | KPartition | KClog | KCrash | KReboot | KPause
  | KSkew | KCorrupt | KTear | KDiskLatency
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Bounds on what faults a run may fire.
data NemesisConfig = NemesisConfig
  { ncBudget :: !Int
  -- ^ max faults this run may fire
  , ncKinds :: ![FaultKind]
  -- ^ which adversary moves are enabled
  , ncQuiesceAt :: !SimTime
  -- ^ no faults proposed at/after this virtual time
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The five assertion "lightpost" kinds (Antithesis vocabulary).
data AssertKind = Always | AlwaysOrUnreachable | Sometimes | Reachable | Unreachable
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | The observable event stream produced by stepping the world.
data SimEvent
  = EvStep !FiberId
  | EvSend !NodeId !NodeId !MsgId
  | EvRecv !NodeId !MsgId
  | EvFault !FaultOp
  | EvAssert !SiteId !AssertKind !Bool -- ^ Bool = predicate held
  | EvCover !SiteId
  | EvObserve !StateKey
  | EvLog !NodeId !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Why a run failed.
data FailReason
  = AssertViolated !SiteId !AssertKind
  | InvariantBroken !Text
  | Deadlock
  | Panic !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)
