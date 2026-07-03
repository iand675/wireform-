{-# LANGUAGE StrictData #-}

{- | The value-level statechart structure.

Everything in this module is /structure/, not behavior: the node tree,
transitions, triggers, and the pure tree algebra (ancestors, descendants,
least common compound ancestor, document order) that the step semantics
("StateMachine.Step"), the renderers ("StateMachine.Render.Mermaid" et al.),
and persistence ("StateMachine.Persist") all consult.

An 'RChart' is normally obtained by demoting a type-level 'StateMachine.Spec.ChartSpec'
via 'StateMachine.Reify.KnownChart' — never built by hand — so every
'RChart' in the wild has already passed the compile-time well-formedness
checks in "StateMachine.Validate" (unique names, resolvable targets,
present initial states).

State names are globally unique within a chart, so a 'NodeName' is a
complete address. The synthetic root ('rootName') is the parent of the
chart's top-level states and never appears in a user configuration.
-}
module StateMachine.Runtime (
  -- * Names
  NodeName,
  rootName,

  -- * Structure
  RChart (..),
  RNode (..),
  RNodeKind (..),
  HistoryKind (..),
  RTrans (..),
  RTrigger (..),
  RInvoke (..),

  -- * Event identity
  EventKey (..),
  triggerMatches,

  -- * Effect requests
  TimerKey (..),
  EffectReq (..),
  armsFor,
  cancelsFor,

  -- * Configurations
  Config,

  -- * Tree algebra
  lookupNode,
  nodeKindOf,
  parentOf,
  childrenOf,
  properAncestors,
  descendantsOf,
  isDescendantOf,
  isAtomic,
  isFinal,
  isCompound,
  isParallel,
  isHistory,
  docOrder,
  sortByDocOrder,
  sortByExitOrder,
  lcca,
  atomicOf,
  transitionsOf,
) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | A state's globally unique name. Uniqueness is enforced at compile time
-- by "StateMachine.Validate", which makes a bare name a complete address —
-- no paths needed.
type NodeName = Text

-- | The synthetic root node: parent of the chart's top-level states.
-- Never part of a user configuration; user states cannot claim this name
-- (the compile-time duplicate check seeds the name list with it).
rootName :: NodeName
rootName = "#root"

-- | Shallow history remembers the immediate child of the parent that was
-- active on exit; deep history remembers the full atomic configuration
-- beneath the parent.
data HistoryKind = Shallow | Deep
  deriving stock (Show, Eq, Ord)

-- | What kind of state a node is.
data RNodeKind
  = -- | A leaf state.
    RAtomic
  | -- | A state with children, exactly one active at a time; the field is
    -- the initial child.
    RCompound NodeName
  | -- | A state whose children are orthogonal regions, all active
    -- simultaneously.
    RParallel
  | -- | A final state. Entering it (transitively) completes the parent and
    -- raises a done event.
    RFinal
  | -- | A history pseudo-state; the field is the default target used when
    -- no history has been recorded ('Nothing' falls back to the parent's
    -- initial).
    RHistory HistoryKind (Maybe NodeName)
  deriving stock (Show, Eq)

-- | What causes a transition to be considered.
data RTrigger
  = -- | A named (external or raised) event.
    TOn Text
  | -- | Any named event (XState @\"*\"@). Does /not/ match done, timer, or
    -- invoke lifecycle events.
    TWildcard
  | -- | Eventless (\"always\") transition: considered after every
    -- microstep while its state is active.
    TAlways
  | -- | Delayed transition: fires when the named timer (scheduled on state
    -- entry) elapses. The field is the delay in milliseconds; identity is
    -- 'TimerKey'.
    TAfter Int
  | -- | @done.state@: fires when the named compound/parallel state
    -- completes (all regions reached final states).
    TDone NodeName
  | -- | @done.invoke@: the invocation with this id resolved successfully.
    TInvokeDone Text
  | -- | @error.invoke@: the invocation with this id failed.
    TInvokeError Text
  deriving stock (Show, Eq)

-- | One transition, attached to its source node.
data RTrans = RTrans
  { rtTrigger :: RTrigger
  , rtGuard :: Maybe Text
  -- ^ Name of a guard in the chart's guard registry.
  , rtTargets :: [NodeName]
  -- ^ Empty list = targetless transition: actions run, no states are
  -- exited or entered.
  , rtActions :: [Text]
  -- ^ Names of actions in the chart's action registry, run between exit
  -- and entry.
  , rtInternal :: Bool
  -- ^ Internal transitions whose targets are descendants of the source do
  -- not exit (and re-enter) the source itself.
  , rtIndex :: Int
  -- ^ Document-order priority among the transitions of one node.
  }
  deriving stock (Show, Eq)

-- | An invoked service: started when the owning state is entered, cancelled
-- when it is exited. Completion/failure surface as 'TInvokeDone' /
-- 'TInvokeError' transitions (flattened into the node's transition list at
-- reification).
data RInvoke = RInvoke
  { riId :: Text
  -- ^ Unique invocation id (unique chart-wide, checked at compile time).
  , riSrc :: Text
  -- ^ Name of the service in the chart's service registry.
  }
  deriving stock (Show, Eq)

-- | One state of the chart.
data RNode = RNode
  { rnName :: NodeName
  , rnKind :: RNodeKind
  , rnParent :: NodeName
  -- ^ 'rootName' for top-level states.
  , rnChildren :: [NodeName]
  -- ^ In declaration order.
  , rnTransitions :: [RTrans]
  -- ^ In document order (includes flattened invoke onDone/onError and
  -- @after@ transitions).
  , rnEntry :: [Text]
  -- ^ Entry action names, in order.
  , rnExit :: [Text]
  -- ^ Exit action names, in order.
  , rnInvokes :: [RInvoke]
  , rnDoneData :: Maybe Text
  -- ^ For final states: name of an output producer in the chart's output
  -- registry; its result becomes the payload of the parent's done event.
  , rnOrder :: Int
  -- ^ Preorder document position, used for entry ordering and conflict
  -- resolution.
  , rnDepth :: Int
  -- ^ Distance from root (top-level states have depth 1).
  }
  deriving stock (Show, Eq)

-- | The whole chart. Obtain via 'StateMachine.Reify.reifyChart'.
data RChart = RChart
  { rcName :: Text
  , rcInitial :: NodeName
  -- ^ Initial top-level state.
  , rcNodes :: Map NodeName RNode
  -- ^ Every node except the synthetic root.
  , rcTopLevel :: [NodeName]
  -- ^ Children of the root, in declaration order.
  , rcEvents :: [(Text, Text)]
  -- ^ Event name → rendered payload type name (for visualization and
  -- fingerprinting).
  , rcRootTransitions :: [RTrans]
  -- ^ Root-level transitions (global handlers): considered after every
  -- state's own transitions, from any state.
  , rcRootInvokes :: [RInvoke]
  -- ^ Root-level invocations: started at initialization, live for the
  -- machine's lifetime.
  , rcRootEntry :: [Text]
  -- ^ Root-level entry actions, run once at initialization.
  }
  deriving stock (Show, Eq)

{- | The identity of an event instance as the step algorithm sees it.
Payloads travel separately (they are typed; see "StateMachine.Event").
-}
data EventKey
  = -- | A named user event (external or raised by an action).
    KNamed Text
  | -- | A delay elapsed.
    KTimer TimerKey
  | -- | @done.state.{node}@ — raised internally when a compound/parallel
    -- state completes.
    KDone NodeName
  | -- | The invocation with this id resolved.
    KInvokeDone Text
  | -- | The invocation with this id failed.
    KInvokeError Text
  | -- | The pseudo-event driving chart initialization. Matches no
    -- trigger; entry actions of initial states observe it.
    KInit
  deriving stock (Show, Eq, Ord)

-- | Does a transition of node @src@ fire for an event? Timer events only
-- match the exact transition (node + delay + document index) whose entry
-- scheduled them.
triggerMatches :: NodeName -> RTrans -> EventKey -> Bool
triggerMatches src tr k = case (rtTrigger tr, k) of
  (TOn n, KNamed n') -> n == n'
  (TWildcard, KNamed _) -> True
  (TAfter ms, KTimer (TimerKey node ms' ix)) ->
    node == src && ms == ms' && ix == rtIndex tr
  (TDone n, KDone n') -> n == n'
  (TInvokeDone i, KInvokeDone i') -> i == i'
  (TInvokeError i, KInvokeError i') -> i == i'
  (TAlways, _) -> False -- eventless transitions are selected by the microstep loop, never by events
  _ -> False

-- | Identity of a scheduled delay: owning node, delay, and the transition's
-- document index (two @after 100@ on one node stay distinct).
data TimerKey = TimerKey
  { tkNode :: NodeName
  , tkDelayMs :: Int
  , tkIndex :: Int
  }
  deriving stock (Show, Eq, Ord)

{- | Side-effect /requests/ emitted by the pure step. The IO interpreter
("StateMachine.Interpret") executes them; the simulator
("StateMachine.Debug") lets tests execute them deterministically.
-}
data EffectReq
  = ReqStartTimer TimerKey
  | ReqCancelTimer TimerKey
  | -- | Start invocation @id@ of service @src@ (owning node recorded for
    -- diagnostics).
    ReqStartInvoke Text Text NodeName
  | ReqCancelInvoke Text
  deriving stock (Show, Eq)

-- | An active configuration: every active state, ancestors included
-- (the synthetic root is implicit and never a member).
type Config = Set NodeName

-- | Look up a node. Total for names produced by reification; 'Nothing'
-- only for foreign names (e.g. out of a stale snapshot).
lookupNode :: RChart -> NodeName -> Maybe RNode
lookupNode c n = Map.lookup n (rcNodes c)

nodeKindOf :: RChart -> NodeName -> Maybe RNodeKind
nodeKindOf c = fmap rnKind . lookupNode c

-- | Parent of a node ('rootName' for top-level states).
parentOf :: RChart -> NodeName -> Maybe NodeName
parentOf c = fmap rnParent . lookupNode c

childrenOf :: RChart -> NodeName -> [NodeName]
childrenOf c n
  | n == rootName = rcTopLevel c
  | otherwise = maybe [] rnChildren (lookupNode c n)

-- | Ancestors, closest first, ending with 'rootName'. Excludes the node.
properAncestors :: RChart -> NodeName -> [NodeName]
properAncestors c = go
 where
  go n = case parentOf c n of
    Nothing -> []
    Just p
      | p == rootName -> [rootName]
      | otherwise -> p : go p

-- | All descendants (children, grandchildren, …) in document order.
descendantsOf :: RChart -> NodeName -> [NodeName]
descendantsOf c n = concatMap (\k -> k : descendantsOf c k) (childrenOf c n)

-- | Is @a@ a proper descendant of @b@?
isDescendantOf :: RChart -> NodeName -> NodeName -> Bool
isDescendantOf c a b
  | b == rootName = True
  | otherwise = b `elem` properAncestors c a

isAtomic, isFinal, isCompound, isParallel, isHistory :: RChart -> NodeName -> Bool
isAtomic c n = case nodeKindOf c n of
  Just RAtomic -> True
  Just RFinal -> True
  _ -> False
isFinal c n = nodeKindOf c n == Just RFinal
isCompound c n = case nodeKindOf c n of
  Just (RCompound _) -> True
  _ -> False
isParallel c n = nodeKindOf c n == Just RParallel
isHistory c n = case nodeKindOf c n of
  Just (RHistory _ _) -> True
  _ -> False

-- | Preorder document position ('maxBound' for unknown names, which sorts
-- them last and harmlessly).
docOrder :: RChart -> NodeName -> Int
docOrder c n = maybe maxBound rnOrder (lookupNode c n)

-- | Entry order: document order (parents before children).
sortByDocOrder :: RChart -> [NodeName] -> [NodeName]
sortByDocOrder c = sortOn (docOrder c)

-- | Exit order: reverse document order (children before parents).
sortByExitOrder :: RChart -> [NodeName] -> [NodeName]
sortByExitOrder c = sortOn (Down . docOrder c)

{- | Least common compound ancestor: the deepest node that is a compound
state (or the root) and a proper ancestor of every given node.
-}
lcca :: RChart -> [NodeName] -> NodeName
lcca _ [] = rootName
lcca c (n : ns) = go (filter viable (properAncestors c n))
 where
  viable a = a == rootName || isCompound c a || isParallel c a
  go [] = rootName
  go (a : rest)
    | all (\x -> isDescendantOf c x a) ns = a
    | otherwise = go rest

-- | The active atomic states within a configuration.
atomicOf :: RChart -> Config -> [NodeName]
atomicOf c = filter (isAtomic c) . Set.toList

-- | Transitions of a node, document order ('rcRootTransitions' for the
-- synthetic root).
transitionsOf :: RChart -> NodeName -> [RTrans]
transitionsOf c n
  | n == rootName = rcRootTransitions c
  | otherwise = maybe [] rnTransitions (lookupNode c n)

-- | Timer and invocation arming requests for an entered state (also used
-- to re-arm on snapshot restore).
armsFor :: RChart -> NodeName -> [EffectReq]
armsFor chart n = timers ++ invokes
 where
  timers = mapMaybe timer (transitionsOf chart n)
  timer t = case rtTrigger t of
    TAfter ms -> Just (ReqStartTimer (TimerKey n ms (rtIndex t)))
    _ -> Nothing
  invokes =
    map
      (\iv -> ReqStartInvoke (riId iv) (riSrc iv) n)
      (maybe [] rnInvokes (lookupNode chart n))

-- | Timer and invocation cancellation requests for an exited state.
cancelsFor :: RChart -> NodeName -> [EffectReq]
cancelsFor chart n = timers ++ invokes
 where
  timers = mapMaybe timer (transitionsOf chart n)
  timer t = case rtTrigger t of
    TAfter ms -> Just (ReqCancelTimer (TimerKey n ms (rtIndex t)))
    _ -> Nothing
  invokes = map (ReqCancelInvoke . riId) (maybe [] rnInvokes (lookupNode chart n))
