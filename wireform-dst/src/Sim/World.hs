-- | The immutable simulation snapshot: 'World' and its constituents. A 'World'
-- is the unit of snapshot/branch — because it is an ordinary immutable value
-- built from persistent containers, "taking a snapshot" is a pointer copy and
-- exploring N futures shares all untouched structure.
--
-- Fiber resumption is a sum ('Resume') so a fiber blocked on @Recv@/@Delay@
-- carries the continuation needed to wake it. @'StrictData'@ (a package-wide
-- default) keeps every field WHNF-strict; note a /deep/ 'Control.DeepSeq.NFData'
-- is intentionally not provided because 'NodeState' holds 'Data.Dynamic.Dynamic'
-- cells and 'Resume' holds continuation functions, neither of which is
-- deep-forcible — WHNF strictness is the achievable and sufficient guarantee.
module Sim.World
  ( Resume (..)
  , FiberStatus (..)
  , Fiber (..)
  , InFlight (..)
  , LinkPolicy (..)
  , defaultLink
  , NodeState (..)
  , freshNode
  , World (..)
  , LatencyDist (..)
  ) where

import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Sim.Entropy (Gen, LatencyDist (..))
import Sim.Prog (Prog)
import Sim.Types
  ( Key
  , NemesisConfig
  , NodeId
  , SimTime (..)
  , SiteId
  )

-- | How a fiber resumes when next scheduled / woken.
data Resume
  = -- | Ready to run its next step.
    Ready !(Prog ())
  | -- | Blocked in @Recv@; resumes with the delivered @(from, body)@.
    AwaitRecv ((NodeId, ByteString) -> Prog ())
  | -- | Blocked in @Delay@ until the given wake time.
    AwaitTimer !SimTime !(Prog ())

-- | A fiber's scheduling status (a cheap projection of 'Resume' the scheduler
-- reads without pattern-matching the continuation).
data FiberStatus
  = FRunnable
  | FBlockedRecv
  | FBlockedTimer !SimTime
  | FDead
  deriving stock (Eq, Show)

-- | One logical actor: the node it lives on, how it resumes, its status.
data Fiber = Fiber
  { fbNode :: !NodeId
  , fbResume :: !Resume
  , fbStatus :: !FiberStatus
  }

-- | A message in the network: source, destination, body, and the earliest
-- virtual time it may be delivered (@clock + sampled latency@ at send).
data InFlight = InFlight
  { ifFrom :: !NodeId
  , ifTo :: !NodeId
  , ifBody :: !ByteString
  , ifDueAt :: !SimTime
  }
  deriving stock (Eq, Show)

-- | Per-directed-link delivery policy.
data LinkPolicy = LinkPolicy
  { lpLatency :: !LatencyDist
  , lpDrop :: !Double
  , lpPartitioned :: !Bool
  , lpClogUntil :: !(Maybe SimTime)
  }
  deriving stock (Eq, Show)

-- | A healthy link with the given latency: no drop, not partitioned, not clogged.
defaultLink :: LatencyDist -> LinkPolicy
defaultLink lat = LinkPolicy {lpLatency = lat, lpDrop = 0, lpPartitioned = False, lpClogUntil = Nothing}

-- | Per-node mutable-but-snapshotted state.
--
-- Storage has two tiers: 'nsDurable' survives a crash, 'nsVolatile' does not.
-- A 'Sim.Types.TearWrite' arms 'nsTearArmed' so the next @StoreWrite@ lands in
-- volatile; 'Sim.Types.Fsync' promotes volatile → durable. 'nsCorruptArmed'
-- flips a bit of the next durable write. 'nsPausedUntil' models
-- 'Sim.Types.PauseNode' / 'Sim.Types.DiskLatency' stalls (the node makes no
-- progress until the clock passes it).
data NodeState = NodeState
  { nsAlive :: !Bool
  , nsSkewNs :: !Int
  , nsDurable :: !(Map Key ByteString)
  , nsVolatile :: !(Map Key ByteString)
  , nsHeap :: !(IntMap Dynamic)
  , nsHeapCtr :: !Int
  , nsTearArmed :: !Bool
  , nsCorruptArmed :: !Bool
  , nsPausedUntil :: !(Maybe SimTime)
  }

-- | A pristine, alive node with empty storage/heap and no armed faults.
freshNode :: NodeState
freshNode =
  NodeState
    { nsAlive = True
    , nsSkewNs = 0
    , nsDurable = Map.empty
    , nsVolatile = Map.empty
    , nsHeap = IntMap.empty
    , nsHeapCtr = 0
    , nsTearArmed = False
    , nsCorruptArmed = False
    , nsPausedUntil = Nothing
    }

-- | The whole simulated universe at one instant. All maps are persistent, so a
-- @let w' = step w d@ shares every untouched sub-structure with @w@.
data World = World
  { wClock :: !SimTime
  , wGen :: !Gen
  , wSeqCtr :: !Int
  -- ^ monotonic tie-breaker bumped on each fiber step (event sequencing)
  , wFibers :: !(IntMap Fiber)
  , wNextFiber :: !Int
  , wInFlight :: !(IntMap InFlight)
  , wNextMsg :: !Int
  , wLinks :: !(Map (NodeId, NodeId) LinkPolicy)
  , wNodes :: !(IntMap NodeState)
  , wBuggify :: !(Map SiteId Bool)
  -- ^ sites already decided this run (PRNG-drawn on first hit)
  , wBuggifyOverride :: !(Map SiteId Bool)
  -- ^ forced enablement from @RunInput.riBuggify@ (search / ddmin)
  , wNemesis :: !NemesisConfig
  , wNodePrograms :: !(IntMap (Prog ()))
  -- ^ node id → root program; 'Sim.Types.RebootNode' respawns from here
  , wFaultsFired :: !Int
  -- ^ faults fired so far (against @ncBudget@)
  }
