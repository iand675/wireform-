{-# LANGUAGE AllowAmbiguousTypes #-}
-- 'HasState' (in 'matches') and 'ValidChart' (in 'chartImpl') are proof
-- obligations, not dictionaries: they exist to force a compile-time check
-- (state exists / chart is well-formed) and are never "used" in the body,
-- which is exactly what -Wredundant-constraints flags.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | The running machine value and the public assembly point.

A @'Machine' spec@ is the live state of a statechart: the active
configuration, the typed context, recorded history, and the run status.
The constructor is exported for the package's own layers ("StateMachine.Step",
"StateMachine.Persist"); /users/ obtain machines exclusively through
'StateMachine.Step.initialize', 'StateMachine.Step.step', and
'StateMachine.Persist.restore' — which is what keeps every configuration
in the wild a legal one. The umbrella "StateMachine" module re-exports
only the read-only surface.

'chartImpl' is where a chart specification meets its implementation:
it demands 'StateMachine.Validate.ValidChart' (the spec is well-formed),
demotes the spec once via 'StateMachine.Reify.KnownChart', and
completeness-checks all four registries ("StateMachine.Registry").
-}
module StateMachine.Machine (
  -- * The machine value
  Machine (..),
  Status (..),

  -- * Read-only surface
  config,
  context,
  status,
  isFinished,

  -- * Compile-checked state queries
  matches,
  matchesKey,
  activeStates,
  activeKeys,
  availableEvents,
  availableEventKeys,

  -- * Assembly
  chartImpl,
) where

import Data.Proxy (Proxy (..))
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import StateMachine.Key (KeyKind, KnownKey, demoteKey, keyName, keyNameOf, parseKey, withSomeKey)

import StateMachine.Reify (KnownChart, reifyChart)
import StateMachine.Registry (
  ActionE,
  ChartImpl (..),
  CompleteReg,
  GuardE,
  OutputE,
  Reg,
  RegNames,
  ServiceE,
  chartImplWith,
 )
import StateMachine.Runtime (
  Config,
  NodeName,
  RTrans (..),
  RTrigger (..),
  rootName,
  transitionsOf,
 )
import StateMachine.Spec (
  ChartActionNames,
  ChartGuardNames,
  ChartOutputNames,
  ChartServiceNames,
  ChartSpec,
  Ctx,
  HasState,
  Output,
 )
import StateMachine.Validate (ValidChart)

{-------------------------------------------------------------------------------
  The machine value
-------------------------------------------------------------------------------}

-- | The live state of a statechart. Fields are internal; see the module
-- header for the construction discipline.
data Machine (spec :: ChartSpec st ev g act svc inv out) = Machine
  { mConfig :: !Config
  -- ^ Active states, ancestors included.
  , mCtx :: Ctx spec
  , mHistory :: !(Map NodeName (Set NodeName))
  -- ^ Recorded history, keyed by history /pseudo-state/ name.
  , mStatus :: !(Status spec)
  }

-- | Whether the machine is still running.
data Status (spec :: ChartSpec st ev g act svc inv out)
  = Running
  | -- | A top-level final state was reached; carries the machine's typed
    -- output.
    Finished (Output spec)

-- | The active configuration (every active state, ancestors included).
config :: Machine spec -> Set NodeName
config = mConfig

-- | The machine's typed context.
context :: Machine spec -> Ctx spec
context = mCtx

status :: Machine spec -> Status spec
status = mStatus

isFinished :: Machine spec -> Bool
isFinished m = case mStatus m of
  Running -> False
  Finished _ -> True

{-------------------------------------------------------------------------------
  Compile-checked state queries
-------------------------------------------------------------------------------}

-- | Is the named state active? The name is compile-checked:
-- @matches \@\'Loading m@ fails to compile if the chart has no
-- @\'Loading@ state.
matches ::
  forall s spec.
  (KnownKey s, HasState spec s) =>
  Machine spec ->
  Bool
matches m = keyNameOf @s `Set.member` mConfig m

-- | 'matches' with the state passed as an ordinary value —
-- @matchesKey Loading m@. Any key of the chart's state kind names a
-- known state, so nothing is left to check at compile time.
matchesKey ::
  forall st ev g act svc inv out (spec :: ChartSpec st ev g act svc inv out).
  (KeyKind st) =>
  st ->
  Machine spec ->
  Bool
matchesKey s m = keyName s `Set.member` mConfig m

-- | The names of all active states (ancestors included), useful for
-- display; prefer 'matches' for logic.
activeStates :: Machine spec -> [NodeName]
activeStates = Set.toList . mConfig

-- | The active states as /values/ of the chart's state kind — reified
-- from the configuration, so the result pattern-matches exhaustively:
-- @case activeKeys m of …@. (The synthetic root, which is not a key, is
-- not included.)
activeKeys ::
  forall st ev g act svc inv out (spec :: ChartSpec st ev g act svc inv out).
  (KeyKind st) =>
  Machine spec ->
  [st]
activeKeys = mapMaybe (fmap toVal . parseKey) . activeStates
 where
  toVal sk = withSomeKey sk demoteKey

{- | The named events that could cause a transition right now: for every
active state (and the root), the 'TOn' triggers of its transitions.
Guards are /not/ evaluated — this answers \"what is worth offering\",
e.g. for debugging or UI affordances.
-}
availableEvents :: ChartImpl m spec -> Machine spec -> [Text]
availableEvents impl m =
  dedup $ concatMap triggersOf (rootName : Set.toList (mConfig m))
 where
  triggersOf n = mapMaybe named (transitionsOf (ciChart impl) n)
  named t = case rtTrigger t of
    TOn e -> Just e
    _ -> Nothing
  dedup = Set.toList . Set.fromList

-- | 'availableEvents' as values of the chart's event kind.
availableEventKeys ::
  forall st ev g act svc inv out (spec :: ChartSpec st ev g act svc inv out) m.
  (KeyKind ev) =>
  ChartImpl m spec ->
  Machine spec ->
  [ev]
availableEventKeys impl m = mapMaybe (fmap toVal . parseKey) (availableEvents impl m)
 where
  toVal sk = withSomeKey sk demoteKey

{-------------------------------------------------------------------------------
  Assembly
-------------------------------------------------------------------------------}

{- | Bundle a chart's implementation: all guards, actions, services, and
done-data producers the spec names (in any order; missing, duplicate, or
foreign entries are compile errors naming the offender), plus the
machine's output function.

This is also where the spec itself is checked ('ValidChart') and demoted
(once — the resulting 'ChartImpl' carries the runtime chart).
-}
chartImpl ::
  forall spec m gs as ss os.
  ( Functor m
  , ValidChart spec
  , KnownChart spec
  , CompleteReg "guard" (ChartGuardNames spec) gs
  , RegNames (ChartGuardNames spec)
  , CompleteReg "action" (ChartActionNames spec) as
  , RegNames (ChartActionNames spec)
  , CompleteReg "service" (ChartServiceNames spec) ss
  , RegNames (ChartServiceNames spec)
  , CompleteReg "output" (ChartOutputNames spec) os
  , RegNames (ChartOutputNames spec)
  ) =>
  Reg (GuardE spec) gs ->
  Reg (ActionE m spec) as ->
  Reg (ServiceE m spec) ss ->
  Reg (OutputE spec) os ->
  (Ctx spec -> Output spec) ->
  ChartImpl m spec
chartImpl = chartImplWith (reifyChart @spec)
