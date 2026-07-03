{- | Deterministic simulation, trace rendering, and static chart lints.

The IO interpreter ("StateMachine.Interpret") runs a machine against real
clocks and real services — which is exactly what makes statecharts painful
to test. This module is the debugging counterpart:

* 'simulate' drives a machine from a /script/ of 'SimCommand's with a
  virtual clock. Time only passes when the script says so ('SimAdvance'),
  and invoked services never run — they sit armed until the script settles
  them with 'SimResolve' \/ 'SimReject'. Every timer race, invoke-vs-timeout
  ordering, and re-entry delay reset becomes a deterministic, repeatable
  assertion.

* 'prettyTrace' \/ 'prettyStepped' render the 'MicroTrace's the pure step
  already produces into a compact, human-readable transcript — what fired,
  what was exited and entered, which actions ran.

* 'unreachableStates' \/ 'deadEnds' are /advisory/ structural lints over an
  'RChart', catching the two chart bugs the compile-time validator cannot
  see (it checks well-formedness, not connectivity): states no transition
  path can ever enter, and states no transition can ever leave.
-}
module StateMachine.Debug (
  -- * Deterministic simulation
  SimCommand (..),
  simResolve,
  simReject,
  SimResult (..),
  SimFailure (..),
  simulate,

  -- * Trace rendering
  prettyTrace,
  prettyStepped,

  -- * Static analysis
  unreachableStates,
  deadEnds,
) where

import Data.Aeson (Value)
import Data.Dynamic (Dynamic, Typeable, toDyn)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import StateMachine.Event (EventCodec, EventVal, StepEvent (..), decodeEvent)
import StateMachine.Machine (Machine, activeStates, isFinished)
import StateMachine.Registry (ChartImpl (..))
import StateMachine.Runtime
import StateMachine.Spec (ChartSpec, Ctx)
import StateMachine.Step (MicroTrace (..), StepFault, Stepped (..), completionOf, initialize, step)

{-------------------------------------------------------------------------------
  Simulation commands and results
-------------------------------------------------------------------------------}

-- | One step of a simulation script.
data SimCommand (spec :: ChartSpec)
  = -- | Deliver a typed external event (@'StateMachine.Event.mkEvent' \@\"NAME\" payload@).
    SimSend (EventVal spec)
  | -- | Deliver an event by name with a JSON payload — the dynamic
    -- boundary, exactly as an actor send or a wire message would arrive.
    -- An unknown name or a payload that fails to decode is a
    -- 'SimDecodeFailure', never a bottom.
    SimSendNamed Text Value
  | -- | Advance the virtual clock by this many milliseconds, firing every
    -- due timer in order (see 'simulate' for the exact semantics).
    SimAdvance Int
  | -- | Resolve the invocation with this id (its @onDone@ transitions
    -- fire) with a /typed/ value — build with 'simResolve'. Fails with
    -- 'SimInvokeNotActive' if no such invocation is armed, a deliberate
    -- strictness that catches typo'd invoke ids and scripts that resolve
    -- an invocation the machine already cancelled.
    SimResolve Text Dynamic
  | -- | Fail the invocation with this id (its @onError@ transitions fire)
    -- with a typed error value — build with 'simReject'. Same strictness
    -- as 'SimResolve'.
    SimReject Text Dynamic
  deriving stock (Show)

-- | Resolve an armed invocation with a typed output value:
-- @simResolve \"getUser\" user@. The @onDone@ handler recovers it with
-- @'StateMachine.Event.invokeOutput' \@User@.
simResolve :: (Typeable a) => Text -> a -> SimCommand spec
simResolve i = SimResolve i . toDyn

-- | Fail an armed invocation with a typed error value:
-- @simReject \"getUser\" err@. The @onError@ handler recovers it with
-- @'StateMachine.Event.invokeError' \@Err@.
simReject :: (Typeable a) => Text -> a -> SimCommand spec
simReject i = SimReject i . toDyn

{- | Where a simulation ended up: the machine, everything that happened
(traces and effect requests, in order), and what is still armed.
-}
data SimResult (spec :: ChartSpec) = SimResult
  { simMachine :: Machine spec
  , simTrace :: [MicroTrace]
  -- ^ Every microstep of the whole run (initialization included), in
  -- order — feed to 'prettyTrace'.
  , simEffectLog :: [EffectReq]
  -- ^ Every effect request the steps emitted, in emission order — the
  -- full timer\/invocation lifecycle for auditing.
  , simPendingTimers :: [(TimerKey, Int)]
  -- ^ Timers still armed, with milliseconds remaining, in arm order.
  , simActiveInvokes :: [(Text, Text)]
  -- ^ Invocations still awaiting settlement, as @(invoke id, service
  -- name)@, in arm order.
  }

-- | Why a simulation script could not be completed.
data SimFailure
  = -- | The underlying step faulted (e.g. an eventless-transition loop).
    SimStepFault StepFault
  | -- | A malformed or divergent script step: a negative 'SimAdvance', or
    -- an advance in which timers kept firing without consuming the window
    -- (a zero-delay timer re-armed in a cycle).
    SimBadEvent String
  | -- | 'SimResolve' \/ 'SimReject' named an invocation that is not
    -- currently armed.
    SimInvokeNotActive Text
  | -- | 'SimSendNamed' could not decode the event: the name with the
    -- decoder's explanation.
    SimDecodeFailure Text String
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  The simulator
-------------------------------------------------------------------------------}

{- | Run a machine from its initial context through a script, with a
virtual clock and manually settled invocations.

The machine is 'initialize'd first; its effects arm the initial timers and
invocations. Then each command is applied in order:

* 'SimSend' \/ 'SimSendNamed' perform one macrostep with the external event.

* 'SimAdvance' @dt@ moves the clock forward @dt@ milliseconds: while any
  armed timer is due within the remaining window, the one with the least
  time remaining fires (ties go to the earliest armed) and its macrostep's
  effects apply /immediately/ — a timer armed mid-advance starts with its
  full delay and may itself fire within the same advance if it fits.
  Whatever survives the window keeps its reduced remaining time, so two
  @SimAdvance 50@ are exactly one @SimAdvance 100@.

* 'SimResolve' \/ 'SimReject' settle an armed invocation, disarming it and
  stepping with the corresponding @done.invoke@ \/ @error.invoke@ event.
  Invocations never run on their own: starting one merely arms it, which is
  what makes service\/timeout races scriptable.

After every macrostep the step's effect requests are applied to the
simulated world: cancels disarm, starts arm (re-arming a live timer resets
its full delay, matching the re-entry semantics of the interpreter).

If the machine finishes mid-script, the rest of the script is ignored and
the result is still 'Right' — the machine's own finish already cancelled
everything armed, so the pending sets are empty and 'simMachine' carries
the output. Scripts may therefore end with a generous @SimAdvance@ without
fear of poking a finished machine.

Cross-actor sends ('StateMachine.Registry.SendReq') are outside the
simulated world; they are ignored here (drive each actor's simulation
separately).
-}
simulate ::
  forall m spec.
  (Monad m, EventCodec spec) =>
  ChartImpl m spec ->
  Ctx spec ->
  [SimCommand spec] ->
  m (Either SimFailure (SimResult spec))
simulate impl ctx0 script = do
  r0 <- initialize impl ctx0
  case r0 of
    Left f -> pure (Left (SimStepFault f))
    Right stepped0 -> do
      let blank =
            SimResult
              { simMachine = sMachine stepped0
              , simTrace = []
              , simEffectLog = []
              , simPendingTimers = []
              , simActiveInvokes = []
              }
      go (applyStepped blank stepped0) script
 where
  go st [] = pure (Right st)
  go st (c : rest)
    | isFinished (simMachine st) = pure (Right st)
    | otherwise = do
        r <- runCommand impl st c
        case r of
          Left f -> pure (Left f)
          Right st' -> go st' rest

-- | Apply one script command.
runCommand ::
  (Monad m, EventCodec spec) =>
  ChartImpl m spec ->
  SimResult spec ->
  SimCommand spec ->
  m (Either SimFailure (SimResult spec))
runCommand impl st = \case
  SimSend ev -> stepSim impl st (EvExternal ev)
  SimSendNamed name payload -> case decodeEvent name payload of
    Left err -> pure (Left (SimDecodeFailure name err))
    Right ev -> stepSim impl st (EvExternal ev)
  SimAdvance dt
    | dt < 0 -> pure (Left (SimBadEvent ("SimAdvance " <> show dt <> ": negative duration")))
    | otherwise -> advanceBy impl advanceFuel dt st
  SimResolve i v -> settle impl st i (EvInvokeDone i v)
  SimReject i v -> settle impl st i (EvInvokeError i v)

-- | Settle an armed invocation: disarm it, then step with its lifecycle
-- event. Settling an invocation that is not armed is a script error.
settle ::
  (Monad m) =>
  ChartImpl m spec ->
  SimResult spec ->
  Text ->
  StepEvent spec ->
  m (Either SimFailure (SimResult spec))
settle impl st i ev
  | any ((== i) . fst) (simActiveInvokes st) =
      stepSim impl st{simActiveInvokes = filter ((/= i) . fst) (simActiveInvokes st)} ev
  | otherwise = pure (Left (SimInvokeNotActive i))

-- | One macrostep, folding its results into the simulated world.
stepSim ::
  (Monad m) =>
  ChartImpl m spec ->
  SimResult spec ->
  StepEvent spec ->
  m (Either SimFailure (SimResult spec))
stepSim impl st ev = do
  r <- step impl (simMachine st) ev
  pure $ case r of
    Left f -> Left (SimStepFault f)
    Right stepped -> Right (applyStepped st stepped)

{- | Fire every timer due within @dt@ milliseconds, least-remaining first
(arm order breaks ties), applying each macrostep's effects before looking
for the next due timer; then age the survivors by the elapsed window.
-}
advanceBy ::
  (Monad m) =>
  ChartImpl m spec ->
  Int ->
  Int ->
  SimResult spec ->
  m (Either SimFailure (SimResult spec))
advanceBy impl fuel dt st
  | fuel <= 0 =
      pure . Left . SimBadEvent $
        "SimAdvance: "
          <> show advanceFuel
          <> " timer firings without exhausting the window (zero-delay timer cycle?)"
  | isFinished (simMachine st) = pure (Right st)
  | otherwise = case dueTimer dt (simPendingTimers st) of
      Nothing ->
        pure (Right st{simPendingTimers = age dt (simPendingTimers st)})
      Just (key, remaining) -> do
        let survivors = age remaining (filter ((/= key) . fst) (simPendingTimers st))
        r <- stepSim impl st{simPendingTimers = survivors} (EvTimer key)
        case r of
          Left f -> pure (Left f)
          Right st' -> advanceBy impl (fuel - 1) (dt - remaining) st'
 where
  age elapsed = map (\(k, r) -> (k, r - elapsed))

-- | The next timer due within the window: smallest remaining time wins,
-- and among equals the earliest armed (strict comparison keeps the first).
dueTimer :: Int -> [(TimerKey, Int)] -> Maybe (TimerKey, Int)
dueTimer dt timers = foldl' pick Nothing (filter ((<= dt) . snd) timers)
 where
  pick acc cand = case acc of
    Nothing -> Just cand
    Just best
      | snd cand < snd best -> Just cand
      | otherwise -> acc

-- | Bound on timer firings within one 'SimAdvance'; beyond it the advance
-- is reported divergent instead of spinning.
advanceFuel :: Int
advanceFuel = 10000

-- | Fold one macrostep's outcome into the simulated world: adopt the
-- machine, log the trace and effects, and apply arms\/cancels (a start of
-- an already-armed timer or invocation re-arms it afresh).
applyStepped :: SimResult spec -> Stepped spec -> SimResult spec
applyStepped st stepped =
  foldl'
    applyEffect
    st
      { simMachine = sMachine stepped
      , simTrace = simTrace st ++ sTrace stepped
      , simEffectLog = simEffectLog st ++ sEffects stepped
      }
    (sEffects stepped)

applyEffect :: SimResult spec -> EffectReq -> SimResult spec
applyEffect st = \case
  ReqStartTimer k ->
    st{simPendingTimers = filter ((/= k) . fst) (simPendingTimers st) ++ [(k, tkDelayMs k)]}
  ReqCancelTimer k ->
    st{simPendingTimers = filter ((/= k) . fst) (simPendingTimers st)}
  ReqStartInvoke i src _ ->
    st{simActiveInvokes = filter ((/= i) . fst) (simActiveInvokes st) ++ [(i, src)]}
  ReqCancelInvoke i ->
    st{simActiveInvokes = filter ((/= i) . fst) (simActiveInvokes st)}

{-------------------------------------------------------------------------------
  Trace rendering
-------------------------------------------------------------------------------}

{- | Render microsteps as a compact numbered transcript. One block per
microstep: the driving event, then the selected transitions (@->@, as
@source #index@), exited states (@-@), entered states (@+@), and actions
run (@!@) — empty sections are omitted, and a microstep that did nothing
at all is a dropped (unhandled) event:

@
1. #init
   +  work, race
2. after 100ms in race
   -> race #0
   -  race
   +  fast
   !  noteFast
3. BOGUS  (dropped)
@
-}
prettyTrace :: [MicroTrace] -> Text
prettyTrace [] = "(no microsteps)"
prettyTrace ts = T.intercalate "\n" (concat (zipWith block [1 :: Int ..] ts))
 where
  width = length (show (length ts))
  indent = T.replicate (width + 2) " "
  block i mt = header : body
   where
    header =
      T.justifyRight width ' ' (T.pack (show i))
        <> ". "
        <> mtEvent mt
        <> (if dropped then "  (dropped)" else "")
    dropped =
      null (mtSelected mt) && null (mtExited mt) && null (mtEntered mt) && null (mtActions mt)
    body =
      section "-> " (map arrow (mtSelected mt))
        ++ section "-  " (mtExited mt)
        ++ section "+  " (mtEntered mt)
        ++ section "!  " (mtActions mt)
    arrow (src, ix) = src <> " #" <> T.pack (show ix)
    section _ [] = []
    section marker xs = [indent <> marker <> T.intercalate ", " xs]

{- | Render one macrostep result: the machine's active states (or its
finished status), the microstep transcript, and the effect requests.
-}
prettyStepped :: Stepped spec -> Text
prettyStepped s = T.intercalate "\n" (statusLine : prettyTrace (sTrace s) : effectLines)
 where
  statusLine
    | isFinished (sMachine s) = "machine: finished"
    | otherwise = "active: " <> T.intercalate ", " (activeStates (sMachine s))
  effectLines = case sEffects s of
    [] -> []
    es -> "effects:" : map (\e -> "   " <> effectText e) es

-- | One effect request, one line.
effectText :: EffectReq -> Text
effectText = \case
  ReqStartTimer k -> "start  timer  " <> timerText k
  ReqCancelTimer k -> "cancel timer  " <> timerText k
  ReqStartInvoke i src n -> "start  invoke " <> i <> " (" <> src <> ") in " <> n
  ReqCancelInvoke i -> "cancel invoke " <> i

timerText :: TimerKey -> Text
timerText (TimerKey n ms ix) = n <> "@" <> T.pack (show ms) <> "ms#" <> T.pack (show ix)

{-------------------------------------------------------------------------------
  Static analysis
-------------------------------------------------------------------------------}

{- | States no run of the machine can ever enter — an /advisory/ lint
(compile-time validation guarantees well-formedness, not connectivity).

Reachability is the least fixpoint from the chart's initial completion,
closed under: the targets of every reachable state's transitions (root
transitions included — they can fire from anywhere); the ancestors of
every reachable state (entering a state enters its ancestors); the initial
child of every reachable compound; every region of a reachable parallel;
and, for a targeted history pseudo-state, its default target plus the
parent's initial child (history can only ever restore states that were
reachable in the first place, so this resolution loses nothing).

The closure over-approximates slightly — a compound only ever entered via
an explicit descendant target still counts its initial child as reachable
— so a flagged state is /certainly/ orphaned, while a silent one is merely
plausibly wired.
-}
unreachableStates :: RChart -> [NodeName]
unreachableStates chart =
  sortByDocOrder chart (filter (`Set.notMember` reachable) (Map.keys (rcNodes chart)))
 where
  reachable = fixpoint seed
  seed =
    Set.union
      (completionOf chart Map.empty [rcInitial chart])
      (Set.fromList (concatMap rtTargets (rcRootTransitions chart)))
  fixpoint acc =
    let acc' = Set.union acc (Set.fromList (concatMap expand (Set.toList acc)))
     in if acc' == acc then acc else fixpoint acc'
  expand n = ancestors ++ entered ++ targets
   where
    ancestors = filter (/= rootName) (properAncestors chart n)
    entered = case nodeKindOf chart n of
      Just (RCompound ini) -> [ini]
      Just RParallel -> childrenOf chart n
      Just (RHistory _ mdef) -> maybeToList mdef ++ parentInitial
      _ -> []
    parentInitial = case parentOf chart n >>= nodeKindOf chart of
      Just (RCompound ini) -> [ini]
      _ -> []
    targets = concatMap rtTargets (transitionsOf chart n)

{- | Atomic non-final states the machine can never leave once entered — an
/advisory/ lint.

A state is a dead end when neither it nor any of its ancestors (the root's
global handlers included) has a transition /with targets/: targetless
transitions run actions but exit nothing, so a state whose only
transitions are targetless is still trapped. Any targeted transition
counts as an exit, whatever its trigger — @on@, wildcard, eventless,
delayed, done, or invoke lifecycle.

Final states are deliberately excluded (being terminal is their job), as
are compound\/parallel states (their children are judged individually).
-}
deadEnds :: RChart -> [NodeName]
deadEnds chart = sortByDocOrder chart (filter trapped (Map.keys (rcNodes chart)))
 where
  trapped n =
    isAtomic chart n
      && not (isFinal chart n)
      && not (any hasExit (n : properAncestors chart n))
  hasExit x = not (all (null . rtTargets) (transitionsOf chart x))
