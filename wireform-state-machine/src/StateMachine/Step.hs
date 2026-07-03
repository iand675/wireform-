{- | The pure statechart step semantics.

This is a faithful adaptation of the SCXML\/W3C statechart algorithm (the
same semantics XState implements): transition selection with descendant
preemption, exit\/entry set computation through the least common compound
ancestor, history recording and restoration, completion (done) events,
eventless (\"always\") transitions run to quiescence, and an internal
queue for events raised by actions — one call to 'step' is one complete
/macrostep/.

The step is pure with respect to the outside world: timers and invoked
services surface as 'EffectReq' /requests/ in the result, and cross-actor
messages as 'StateMachine.Registry.SendReq's. The IO interpreter
("StateMachine.Interpret") executes them; the simulator
("StateMachine.Debug") executes them deterministically in tests.

Nothing here can fail on a well-formed chart /except/ a genuinely dynamic
condition: an eventless-transition cycle, reported as 'EventlessLoop'
rather than a hang.
-}
module StateMachine.Step (
  -- * Results
  Stepped (..),
  MicroTrace (..),
  StepFault (..),

  -- * Driving
  initialize,
  step,

  -- * Configuration completion (for restore\/recovery)
  completionOf,
) where

import Control.Monad (foldM)
import Data.Dynamic (Dynamic)
import Data.Bifunctor (second)
import Data.Foldable (asum)
import Data.List (find, foldl', nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import StateMachine.Event (StepEvent (..), stepEventLabel, stepEventKey)
import StateMachine.Machine (Machine (..), Status (..))
import StateMachine.Registry (ActionOutcome (..), ChartImpl (..), SendReq)
import StateMachine.Runtime
import StateMachine.Spec (ChartSpec, Ctx)

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

-- | The result of one macrostep.
data Stepped (spec :: ChartSpec) = Stepped
  { sMachine :: Machine spec
  , sEffects :: [EffectReq]
  -- ^ Timer\/invocation lifecycle requests, in emission order.
  , sSends :: [SendReq spec]
  -- ^ Cross-actor sends requested by actions, in emission order.
  , sTrace :: [MicroTrace]
  -- ^ One entry per microstep, in order — the debugger's raw material.
  }

-- | What one microstep did. An entry with no selected transitions records
-- a dropped (unhandled) event.
data MicroTrace = MicroTrace
  { mtEvent :: Text
  -- ^ A short human-readable label for the event that drove the microstep.
  , mtSelected :: [(NodeName, Int)]
  -- ^ Selected transitions as (source state, document index).
  , mtExited :: [NodeName]
  , mtEntered :: [NodeName]
  , mtActions :: [Text]
  -- ^ Action names in execution order (exit, transition, entry).
  }
  deriving stock (Show, Eq)

-- | Dynamic failures of a step. (Static failures don't exist: the chart
-- was validated at compile time and the registries are complete.)
data StepFault
  = -- | Eventless transitions (or raised-event cycles) failed to reach
    -- quiescence; the field is the active atomic states when the bound
    -- was hit.
    EventlessLoop [NodeName]
  | -- | An internal invariant was violated — reachable only through a
    -- hand-forged 'RChart', never from 'StateMachine.Machine.chartImpl'.
    InternalFault Text
  deriving stock (Show, Eq)

-- | Iteration bound for the quiescence loop; beyond it we report
-- 'EventlessLoop' instead of spinning.
maxMicrosteps :: Int
maxMicrosteps = 10000

{-------------------------------------------------------------------------------
  Loop state
-------------------------------------------------------------------------------}

data Loop (spec :: ChartSpec) = Loop
  { lConfig :: !Config
  , lCtx :: Ctx spec
  , lHistory :: !(Map NodeName (Set NodeName))
  , lEffects :: [EffectReq]
  , lSends :: [SendReq spec]
  , lTrace :: [MicroTrace]
  , lActed :: [Text]
  -- ^ Action names run in the current microstep.
  , lQueue :: !(Seq (StepEvent spec))
  , lRootDone :: !Bool
  }

fromMachine :: Machine spec -> Loop spec
fromMachine m =
  Loop
    { lConfig = mConfig m
    , lCtx = mCtx m
    , lHistory = mHistory m
    , lEffects = []
    , lSends = []
    , lTrace = []
    , lActed = []
    , lQueue = Seq.empty
    , lRootDone = False
    }

addEffects :: Loop spec -> [EffectReq] -> Loop spec
addEffects lp es = lp{lEffects = lEffects lp ++ es}

nodeOf :: RChart -> NodeName -> RNode
nodeOf chart n =
  fromMaybe (error ("StateMachine.Step: unknown node " <> show n)) (lookupNode chart n)

{-------------------------------------------------------------------------------
  Driving
-------------------------------------------------------------------------------}

{- | Start a machine: run root entry actions, arm root invocations, enter
the chart's initial configuration, and run to quiescence. Entry actions
observe 'EvInit'.
-}
initialize ::
  forall m spec.
  (Monad m) =>
  ChartImpl m spec ->
  Ctx spec ->
  m (Either StepFault (Stepped spec))
initialize impl ctx0 = do
  let chart = ciChart impl
      loop0 =
        Loop
          { lConfig = Set.empty
          , lCtx = ctx0
          , lHistory = Map.empty
          , lEffects = []
          , lSends = []
          , lTrace = []
          , lActed = []
          , lQueue = Seq.empty
          , lRootDone = False
          }
  l1 <- runActions impl EvInit (rcRootEntry chart) loop0
  let l2 =
        addEffects
          l1
          (map (\iv -> ReqStartInvoke (riId iv) (riSrc iv) rootName) (rcRootInvokes chart))
      entrySet = addDescendants chart Map.empty (rcInitial chart) Set.empty
      entryList = sortByDocOrder chart (Set.toList entrySet)
  l3 <- enterStates impl EvInit entryList l2
  let l4 =
        l3
          { lTrace =
              lTrace l3
                ++ [MicroTrace (stepEventLabel (EvInit @spec)) [] [] entryList (lActed l3)]
          , lActed = []
          }
  fmap (finalize impl) <$> quiesce impl EvInit 0 l4

{- | Process one event: a complete macrostep. On a 'Finished' machine this
is a no-op (the event is ignored, with an empty trace).
-}
step ::
  (Monad m) =>
  ChartImpl m spec ->
  Machine spec ->
  StepEvent spec ->
  m (Either StepFault (Stepped spec))
step impl m ev = case mStatus m of
  Finished _ -> pure (Right (Stepped m [] [] []))
  Running -> do
    let loop0 = fromMachine m
        sel = selectFor impl loop0 ev
    loop1 <- microstep impl ev sel loop0
    fmap (finalize impl) <$> quiesce impl ev 0 loop1

-- | Run eventless transitions and the internal queue to quiescence.
quiesce ::
  (Monad m) =>
  ChartImpl m spec ->
  StepEvent spec ->
  Int ->
  Loop spec ->
  m (Either StepFault (Loop spec))
quiesce impl current n loop
  | lRootDone loop = pure (Right loop)
  | n > maxMicrosteps =
      pure (Left (EventlessLoop (atomicOf (ciChart impl) (lConfig loop))))
  | otherwise =
      case selectEventless impl loop current of
        sel@(_ : _) ->
          microstep impl current sel loop >>= quiesce impl current (n + 1)
        [] -> case lQueue loop of
          Seq.Empty -> pure (Right loop)
          e :<| rest -> do
            let loop' = loop{lQueue = rest}
                sel = selectFor impl loop' e
            loop'' <- microstep impl e sel loop'
            quiesce impl e (n + 1) loop''

-- | Assemble the final 'Stepped'; on root completion, compute the typed
-- output and cancel everything still armed.
finalize :: ChartImpl m spec -> Loop spec -> Stepped spec
finalize impl loop
  | lRootDone loop =
      let chart = ciChart impl
          cancels =
            concatMap (cancelsFor chart) (Set.toList (lConfig loop))
              ++ map (ReqCancelInvoke . riId) (rcRootInvokes chart)
       in (mk (Finished (ciFinal impl (lCtx loop))))
            { sEffects = lEffects loop ++ cancels
            }
  | otherwise = mk Running
 where
  mk st =
    Stepped
      { sMachine =
          Machine
            { mConfig = lConfig loop
            , mCtx = lCtx loop
            , mHistory = lHistory loop
            , mStatus = st
            }
      , sEffects = lEffects loop
      , sSends = lSends loop
      , sTrace = lTrace loop
      }

{-------------------------------------------------------------------------------
  Transition selection
-------------------------------------------------------------------------------}

guardPasses :: ChartImpl m spec -> Ctx spec -> StepEvent spec -> RTrans -> Bool
guardPasses impl ctx ev t = case rtGuard t of
  Nothing -> True
  Just g -> case Map.lookup g (ciGuards impl) of
    Just f -> f ctx ev
    Nothing -> False -- unreachable via chartImpl (registry completeness)

-- | Select the transitions an event enables.
selectFor :: ChartImpl m spec -> Loop spec -> StepEvent spec -> [(NodeName, RTrans)]
selectFor impl loop ev =
  selectWith impl loop $ \s t ->
    triggerMatches s t (stepEventKey ev) && guardPasses impl (lCtx loop) ev t

-- | Select enabled eventless (\"always\") transitions; guards observe the
-- most recently processed event.
selectEventless :: ChartImpl m spec -> Loop spec -> StepEvent spec -> [(NodeName, RTrans)]
selectEventless impl loop ev =
  selectWith impl loop $ \_ t ->
    rtTrigger t == TAlways && guardPasses impl (lCtx loop) ev t

{- | The SCXML selection walk: for each active atomic state (document
order), the first enabled transition on the state or its ancestors wins;
then descendant transitions preempt conflicting ancestor ones.
-}
selectWith ::
  ChartImpl m spec ->
  Loop spec ->
  (NodeName -> RTrans -> Bool) ->
  [(NodeName, RTrans)]
selectWith impl loop enabled =
  removeConflicts chart (lConfig loop) (lHistory loop) picks
 where
  chart = ciChart impl
  atomics = sortByDocOrder chart (atomicOf chart (lConfig loop))
  picks = nubBy sameTrans (mapMaybe pick atomics)
  pick a = asum (map firstMatch (a : properAncestors chart a))
  firstMatch s = (,) s <$> find (enabled s) (transitionsOf chart s)
  sameTrans (s1, t1) (s2, t2) = s1 == s2 && rtIndex t1 == rtIndex t2

{- | SCXML @removeConflictingTransitions@: transitions whose exit sets
intersect conflict; the one selected for a descendant source preempts the
ancestor's.
-}
removeConflicts ::
  RChart ->
  Config ->
  Map NodeName (Set NodeName) ->
  [(NodeName, RTrans)] ->
  [(NodeName, RTrans)]
removeConflicts chart cfg hist = go []
 where
  exitOf (s, t) = exitSetFor chart cfg hist s t
  go kept [] = reverse kept
  go kept (t1 : rest)
    | preempted = go kept rest
    | otherwise = go (t1 : filter (`notElem` losers) kept) rest
   where
    x1 = exitOf t1
    clash t2 = not (Set.disjoint x1 (exitOf t2))
    (preempted, losers) = foldl' judge (False, []) kept
    judge (True, ls) _ = (True, ls)
    judge (False, ls) t2
      | clash t2 =
          if isDescendantOf chart (fst t1) (fst t2)
            then (False, t2 : ls)
            else (True, ls)
      | otherwise = (False, ls)

{-------------------------------------------------------------------------------
  Exit and entry sets
-------------------------------------------------------------------------------}

-- | Resolve history pseudo-states to their stored (or default) targets.
effectiveTargets :: RChart -> Map NodeName (Set NodeName) -> [NodeName] -> [NodeName]
effectiveTargets chart hist = concatMap resolve
 where
  resolve n = case nodeKindOf chart n of
    Just (RHistory _ mdef) -> historyTargets chart hist n mdef
    _ -> [n]

historyTargets :: RChart -> Map NodeName (Set NodeName) -> NodeName -> Maybe NodeName -> [NodeName]
historyTargets chart hist h mdef =
  case Map.lookup h hist of
    Just stored | not (Set.null stored) -> Set.toList stored
    _ -> case mdef of
      Just d -> [d]
      Nothing ->
        let parent = fromMaybe rootName (parentOf chart h)
         in case nodeKindOf chart parent of
              Just (RCompound ini) -> [ini]
              Just RParallel -> filter (not . isHistory chart) (childrenOf chart parent)
              _ -> []

-- | SCXML @getTransitionDomain@.
domainFor :: RChart -> Map NodeName (Set NodeName) -> NodeName -> RTrans -> NodeName
domainFor chart hist src t
  | rtInternal t
  , isCompound chart src
  , all (\tt -> isDescendantOf chart tt src) targets =
      src
  | otherwise = lcca chart (src : targets)
 where
  targets = effectiveTargets chart hist (rtTargets t)

-- | The active states a transition exits (empty for targetless).
exitSetFor ::
  RChart ->
  Config ->
  Map NodeName (Set NodeName) ->
  NodeName ->
  RTrans ->
  Set NodeName
exitSetFor chart cfg hist s t
  | null (rtTargets t) = Set.empty
  | otherwise =
      let d = domainFor chart hist s t
       in Set.filter (\x -> isDescendantOf chart x d) cfg

-- | The states a transition enters (SCXML @computeEntrySet@).
entrySetFor ::
  RChart ->
  Map NodeName (Set NodeName) ->
  NodeName ->
  RTrans ->
  Set NodeName
entrySetFor chart hist src t
  | null (rtTargets t) = Set.empty
  | otherwise =
      let d = domainFor chart hist src t
          withTargets = foldl' (flip (addDescendants chart hist)) Set.empty (rtTargets t)
          eff = effectiveTargets chart hist (rtTargets t)
       in foldl' (\acc s' -> addAncestors chart hist s' d acc) withTargets eff

-- | SCXML @addDescendantStatesToEnter@.
addDescendants :: RChart -> Map NodeName (Set NodeName) -> NodeName -> Set NodeName -> Set NodeName
addDescendants chart hist n acc = case nodeKindOf chart n of
  Just (RHistory _ mdef) ->
    let ts = historyTargets chart hist n mdef
        parent = fromMaybe rootName (parentOf chart n)
        acc' = foldl' (flip (addDescendants chart hist)) acc ts
     in foldl' (\a s -> addAncestors chart hist s parent a) acc' ts
  Just (RCompound ini) ->
    let acc' = addDescendants chart hist ini (Set.insert n acc)
     in addAncestors chart hist ini n acc'
  Just RParallel ->
    completeRegions chart hist n (Set.insert n acc)
  _ -> Set.insert n acc

-- | SCXML @addAncestorStatesToEnter@: ancestors of @n@ strictly below
-- @upTo@, completing the regions of any parallel ancestor.
addAncestors :: RChart -> Map NodeName (Set NodeName) -> NodeName -> NodeName -> Set NodeName -> Set NodeName
addAncestors chart hist n upTo acc0 =
  foldl' one acc0 (takeWhile (/= upTo) (properAncestors chart n))
 where
  one acc anc
    | anc == rootName = acc
    | isParallel chart anc = completeRegions chart hist anc (Set.insert anc acc)
    | otherwise = Set.insert anc acc

-- | Enter every region of a parallel state that has no entrant yet.
completeRegions :: RChart -> Map NodeName (Set NodeName) -> NodeName -> Set NodeName -> Set NodeName
completeRegions chart hist p acc =
  foldl' enterRegion acc (filter (not . isHistory chart) (childrenOf chart p))
 where
  enterRegion a r
    | any (\x -> x == r || isDescendantOf chart x r) (Set.toList a) = a
    | otherwise = addDescendants chart hist r a

{-------------------------------------------------------------------------------
  Microsteps
-------------------------------------------------------------------------------}

-- | Execute one microstep: exit, transition actions, entry, completion
-- bookkeeping, trace.
microstep ::
  (Monad m) =>
  ChartImpl m spec ->
  StepEvent spec ->
  [(NodeName, RTrans)] ->
  Loop spec ->
  m (Loop spec)
microstep impl ev selected loop0 = do
  let chart = ciChart impl
      cfg0 = lConfig loop0
      exitSet =
        Set.unions (map (uncurry (exitSetFor chart cfg0 (lHistory loop0))) selected)
      exitList = sortByExitOrder chart (Set.toList exitSet)
      -- record history before exiting (SCXML order)
      hist' = foldl' (recordHistory chart cfg0) (lHistory loop0) exitList
      loop1 =
        addEffects loop0{lHistory = hist'} (concatMap (cancelsFor chart) exitList)
  -- exit actions, then drop the exit set from the configuration
  loop2 <- foldM (\lp n -> runActions impl ev (rnExit (nodeOf chart n)) lp) loop1 exitList
  let loop3 = loop2{lConfig = lConfig loop2 `Set.difference` exitSet}
  -- transition actions
  loop4 <- foldM (\lp (_, t) -> runActions impl ev (rtActions t) lp) loop3 selected
  -- entry
  let entrySet =
        Set.unions (map (uncurry (entrySetFor chart (lHistory loop4))) selected)
      entryList = sortByDocOrder chart (Set.toList entrySet)
  loop5 <- enterStates impl ev entryList loop4
  let trace =
        MicroTrace
          { mtEvent = stepEventLabel ev
          , mtSelected = map (second rtIndex) selected
          , mtExited = exitList
          , mtEntered = entryList
          , mtActions = lActed loop5
          }
  pure loop5{lTrace = lTrace loop5 ++ [trace], lActed = []}

-- | Add states to the configuration in entry order: entry actions, timer
-- and invocation arming, completion (done-event) bookkeeping.
enterStates ::
  (Monad m) =>
  ChartImpl m spec ->
  StepEvent spec ->
  [NodeName] ->
  Loop spec ->
  m (Loop spec)
enterStates impl ev entryList loop0 = do
  loop1 <- foldM enterOne loop0 entryList
  pure (foldl' finalBookkeeping loop1 entryList)
 where
  chart = ciChart impl
  enterOne lp n = do
    let lp1 = lp{lConfig = Set.insert n (lConfig lp)}
    lp2 <- runActions impl ev (rnEntry (nodeOf chart n)) lp1
    pure (addEffects lp2 (armsFor chart n))
  finalBookkeeping lp n
    | not (isFinal chart n) = lp
    | otherwise =
        let parent = rnParent (nodeOf chart n)
         in if parent == rootName
              then lp{lRootDone = True}
              else
                let dd = doneDataFor impl ev (lCtx lp) n
                    lp' = lp{lQueue = lQueue lp |> EvDone parent dd}
                    gp = rnParent (nodeOf chart parent)
                 in if gp /= rootName
                      && isParallel chart gp
                      && all
                        (inFinalState chart (lConfig lp'))
                        (filter (not . isHistory chart) (childrenOf chart gp))
                      then lp'{lQueue = lQueue lp' |> EvDone gp Nothing}
                      else lp'

-- | The done-data payload of a final state, if it declares a producer.
doneDataFor :: ChartImpl m spec -> StepEvent spec -> Ctx spec -> NodeName -> Maybe Dynamic
doneDataFor impl ev ctx n = do
  prod <- rnDoneData (nodeOf (ciChart impl) n)
  f <- Map.lookup prod (ciOutputs impl)
  pure (f ctx ev)

-- | SCXML @isInFinalState@.
inFinalState :: RChart -> Config -> NodeName -> Bool
inFinalState chart cfg s = case nodeKindOf chart s of
  Just (RCompound _) ->
    any (\c -> isFinal chart c && Set.member c cfg) (childrenOf chart s)
  Just RParallel ->
    all (inFinalState chart cfg) (filter (not . isHistory chart) (childrenOf chart s))
  _ -> False

-- | Record history for every history child of an exited state, from the
-- configuration as it stood before the exit.
recordHistory :: RChart -> Config -> Map NodeName (Set NodeName) -> NodeName -> Map NodeName (Set NodeName)
recordHistory chart cfg hist exited = foldl' store hist historyChildren
 where
  historyChildren =
    mapMaybe
      ( \h -> case nodeKindOf chart h of
          Just (RHistory k _) -> Just (h, k)
          _ -> Nothing
      )
      (childrenOf chart exited)
  store hi (h, k) =
    let val = case k of
          Shallow -> Set.fromList (filter (`Set.member` cfg) (childrenOf chart exited))
          Deep ->
            Set.fromList
              (filter (\d -> Set.member d cfg && isAtomic chart d) (descendantsOf chart exited))
     in Map.insert h val hi

{- | The full configuration obtained by entering the given states: their
completion (initial children, parallel regions, resolved history) plus
all ancestors. Restore\/recovery uses this to normalize a configuration.
-}
completionOf :: RChart -> Map NodeName (Set NodeName) -> [NodeName] -> Set NodeName
completionOf chart hist targets =
  foldl'
    (\acc s -> addAncestors chart hist s rootName acc)
    (foldl' (flip (addDescendants chart hist)) Set.empty targets)
    (effectiveTargets chart hist targets)

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

-- | Run a list of named actions in order, threading the context and
-- collecting raised events and sends.
runActions ::
  (Monad m) =>
  ChartImpl m spec ->
  StepEvent spec ->
  [Text] ->
  Loop spec ->
  m (Loop spec)
runActions impl ev names loop = foldM one loop names
 where
  one lp nm = case Map.lookup nm (ciActions impl) of
    Nothing -> pure lp -- unreachable via chartImpl (registry completeness)
    Just act -> do
      out <- act (lCtx lp) ev
      pure
        lp
          { lCtx = aoCtx out
          , lQueue = foldl' (\q e -> q |> EvExternal e) (lQueue lp) (aoRaised out)
          , lSends = lSends lp ++ aoSends out
          , lActed = lActed lp ++ [nm]
          }
