-- | NAIL: localize and minimize a 'Bug' found by "Sim.Search".
--
--   * 'ddminInput' delta-debugs a 'RunInput' (its decision path, then its
--     buggify overrides) to a 1-minimal reproducer against a "still fails the
--     same way" oracle (Zeller's ddmin).
--   * 'bisectInvariant' binary-searches the decision prefix for the first index
--     at which the scenario invariant flips true → false.
--   * 'siblingDiff' names a proximate cause from a pass/fail sibling pair.
--   * 'ochiai' ranks call-sites by spectrum-based suspiciousness.
--   * 'explainBug' ties them together into a 'BugReport'.
--
-- Everything here is a pure function of 'replay' (deterministic), so a report
-- is itself reproducible.
module Sim.Localize
  ( bisectInvariant
  , ddminInput
  , ddminList
  , siblingDiff
  , ochiai
  , BugReport (..)
  , explainBug
  ) where

import Data.Aeson (ToJSON)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Sim.Coverage (CovSet (..), covFromEvents)
import Sim.Interp
import Sim.Search (Bug (..), SearchConfig, SearchOutcome (..), Snapshot (..), pathId)
import Sim.Types
import Sim.Fault (availableFaults)
import Sim.World (World, wBuggifyOverride, wNemesis)

-- * Prefix replay ---------------------------------------------------------

-- The world after applying exactly the first @k@ controlled decisions of a
-- run's path (advancing the clock when a decision is not yet applicable), with
-- NO drive-to-quiescence afterwards — the state precisely at that decision.
stateAfterPrefix :: Scenario -> RunInput -> Int -> World
stateAfterPrefix scen ri k =
  let w0 = (initWorld (riSeed ri) scen) {wBuggifyOverride = riBuggify ri}
   in applyN w0 (take k (riPath ri))
  where
    applyN w [] = w
    applyN w (d : ds)
      | d `elem` runnable w = applyN (srWorld (step w d)) ds
      | canAdvanceTime w = applyN (advanceClock w) (d : ds)
      | otherwise = applyN w ds

-- | The first decision index at which the invariant flips true → false, found
-- by binary search over the prefix length (assumes a monotone invariant, as in
-- the acceptance test). Returns @length (riPath ri)@ if it never breaks.
bisectInvariant :: Scenario -> RunInput -> Int
bisectInvariant scen ri = go 0 n
  where
    n = length (riPath ri)
    broken k = not (scenInvariant scen (stateAfterPrefix scen ri k))
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
           in if broken mid then go lo mid else go (mid + 1) hi

-- * Delta debugging -------------------------------------------------------

-- | Zeller's ddmin: reduce @xs@ (on which @p@ holds) to a 1-minimal sublist
-- that still satisfies @p@.
ddminList :: ([a] -> Bool) -> [a] -> [a]
ddminList p xs0 = go xs0 2
  where
    go c n
      | length c <= 1 = c
      | otherwise =
          let parts = splitInto n c
           in case filter (\s -> not (null s) && p s) parts of
                (s : _) -> go s 2
                [] ->
                  let comps = [concatExcept i parts | i <- [0 .. length parts - 1]]
                   in case filter p comps of
                        (cmp : _) -> go cmp (max (n - 1) 2)
                        [] -> if n < length c then go c (min (2 * n) (length c)) else c

splitInto :: Int -> [a] -> [[a]]
splitInto n xs =
  let len = length xs
      sz = max 1 ((len + n - 1) `div` n)
      chunk [] = []
      chunk ys = let (a, b) = splitAt sz ys in a : chunk b
   in filter (not . null) (chunk xs)

concatExcept :: Int -> [[a]] -> [a]
concatExcept i parts = concat [p | (j, p) <- zip [0 ..] parts, j /= i]

-- | Reduce a 'RunInput' to a 1-minimal reproducer against @oracle@: first
-- delta-debug the decision path, then the buggify overrides.
ddminInput :: (RunInput -> Bool) -> RunInput -> RunInput
ddminInput oracle ri0 =
  let path1 = ddminList (\p -> oracle ri0 {riPath = p}) (riPath ri0)
      ri1 = ri0 {riPath = path1}
      bug1 =
        Map.fromList
          (ddminList (\kvs -> oracle ri1 {riBuggify = Map.fromList kvs}) (Map.toList (riBuggify ri1)))
   in ri1 {riBuggify = bug1}

-- * Content-aware minimization (fault-index-robust) ----------------------

-- @FireFault@ is an /index/ into the deterministically-derived fault menu, so
-- removing any earlier decision renumbers every later fault — which cripples a
-- naive path-level ddmin. We instead minimize over decisions whose faults are
-- resolved to their concrete 'FaultOp' content ('OpDecision'), re-resolving the
-- index at each step. Removing an unrelated decision no longer changes what a
-- surviving fault /means/, so ddmin shrinks fault-heavy paths effectively.
data OpDecision = DSched !FiberId | DDeliver !MsgId | DFault !FaultOp
  deriving stock (Eq, Show)

-- Resolve a run's concrete decision path into content-tagged 'OpDecision's.
toOps :: Scenario -> RunInput -> [OpDecision]
toOps scen ri = go (initW ri scen) (riPath ri) []
  where
    go _ [] acc = reverse acc
    go w (d : ds) acc
      | d `elem` runnable w =
          let StepResult w' _ = step w d in go w' ds (toOp w d : acc)
      | canAdvanceTime w = go (advanceClock w) (d : ds) acc
      | otherwise = go w ds acc
    toOp _ (SchedNext f) = DSched f
    toOp _ (DeliverMsg m) = DDeliver m
    toOp w (FireFault i) = DFault (availableFaults (wNemesis w) w !! i)

initW :: RunInput -> Scenario -> World
initW ri scen = (initWorld (riSeed ri) scen) {wBuggifyOverride = riBuggify ri}

resolveOp :: World -> OpDecision -> Maybe Decision
resolveOp w = \case
  DSched f -> if SchedNext f `elem` runnable w then Just (SchedNext f) else Nothing
  DDeliver m -> if DeliverMsg m `elem` runnable w then Just (DeliverMsg m) else Nothing
  DFault op -> FireFault <$> elemIndex op (availableFaults (wNemesis w) w)

-- Execute an 'OpDecision' subset (resolving faults by content, driving the
-- remaining non-fault decisions to quiescence afterwards) and report the
-- outcome. This is the ddmin oracle's engine.
runOps :: Scenario -> Seed -> Map.Map SiteId Bool -> [OpDecision] -> RunResult
runOps scen seed bug ops0 = go w0 ops0 [] (200000 :: Int)
  where
    inv = scenInvariant scen
    w0 = (initWorld seed scen) {wBuggifyOverride = bug}
    go w ops acc fuel
      | fuel <= 0 = RunResult w (reverse acc) Nothing
      | not (inv w) = RunResult w (reverse acc) (Just (InvariantBroken "scenario invariant"))
      | otherwise = case ops of
          (o : os) -> case resolveOp w o of
            Just d -> stepOn w d os acc fuel
            Nothing ->
              if canAdvanceTime w
                then go (advanceClock w) ops acc (fuel - 1)
                else go w os acc fuel
          [] -> case filter (not . isFaultDecision) (runnable w) of
            (d : _) -> stepOn w d [] acc fuel
            [] ->
              if canAdvanceTime w
                then go (advanceClock w) [] acc (fuel - 1)
                else
                  if deadlocked w
                    then RunResult w (reverse acc) (Just Deadlock)
                    else RunResult w (reverse acc) Nothing
    stepOn w d os acc fuel =
      let StepResult w' evs = step w d
          acc' = reverse evs ++ acc
       in case firstAssertFail evs of
            Just r -> RunResult w' (reverse acc') (Just r)
            Nothing -> go w' os acc' (fuel - 1)

-- Convert a minimal 'OpDecision' list back to a concrete decision path (only the
-- resolved ops, no drive) — normal 'replay' will auto-drive the remainder.
opsToPath :: Scenario -> Seed -> Map.Map SiteId Bool -> [OpDecision] -> DecisionPath
opsToPath scen seed bug ops0 = go ((initWorld seed scen) {wBuggifyOverride = bug}) ops0 []
  where
    go _ [] acc = reverse acc
    go w (o : os) acc = case resolveOp w o of
      Just d -> let StepResult w' _ = step w d in go w' os (d : acc)
      Nothing ->
        if canAdvanceTime w then go (advanceClock w) (o : os) acc else go w os acc

isFaultDecision :: Decision -> Bool
isFaultDecision (FireFault _) = True
isFaultDecision _ = False

firstAssertFail :: [SimEvent] -> Maybe FailReason
firstAssertFail = foldr (\e acc -> maybe acc Just (eventFail e)) Nothing

-- | Content-aware 1-minimal reproducer for a bug against its scenario. Robust
-- to @FireFault@ index renumbering, unlike the path-level 'ddminInput'.
minimizeBug :: Scenario -> Bug -> RunInput
minimizeBug scen bug =
  let ri = bugInput bug
      reason = bugReason bug
      ops0 = toOps scen ri
      oracle sub = rrFail (runOps scen (riSeed ri) (riBuggify ri) sub) == Just reason
      minOps = ddminList oracle ops0
   in ri {riPath = opsToPath scen (riSeed ri) (riBuggify ri) minOps}

-- * Sibling diff ----------------------------------------------------------

-- | From the corpus, find a snapshot that shares the failing run's parent (a
-- "sibling") and report the diverging decision (the failing path's last step)
-- plus the index at which it diverges. A proximate-cause hint; 'Nothing' when
-- no sibling was retained.
siblingDiff :: [Snapshot] -> Bug -> Maybe (Decision, Int)
siblingDiff snaps bug =
  case reverse (riPath (bugInput bug)) of
    [] -> Nothing
    (lastD : revParent) ->
      let parentPath = reverse revParent
          parentKey = Just (pathId parentPath)
          sibs = [s | s <- snaps, snParent s == parentKey]
       in case sibs of
            (_ : _) -> Just (lastD, length parentPath)
            [] -> Nothing

-- * Spectrum --------------------------------------------------------------

-- | Ochiai spectrum-based suspiciousness over runs labelled @True@ = failed.
-- A site covered only in failing runs scores near 1; a site covered in every
-- run scores lower. Returned sorted by descending suspiciousness.
ochiai :: [(CovSet, Bool)] -> [(SiteId, Double)]
ochiai runs =
  sortOn (Down . snd) [(s, score s) | s <- sites]
  where
    sites = Set.toList (Set.unions [csEdges c | (c, _) <- runs])
    totalFailed = length [() | (_, f) <- runs, f]
    score s =
      let failed = length [() | (c, f) <- runs, f, Set.member s (csEdges c)]
          passed = length [() | (c, f) <- runs, not f, Set.member s (csEdges c)]
          denom = sqrt (fromIntegral totalFailed * fromIntegral (failed + passed))
       in if denom == 0 then 0 else fromIntegral failed / denom

-- * Top level -------------------------------------------------------------

-- | A localized, minimized explanation of a bug.
data BugReport = BugReport
  { brMinimal :: !RunInput
  -- ^ 1-minimal reproducer
  , brFirstBreak :: !Int
  -- ^ decision index where the invariant first breaks
  , brProximate :: !(Maybe Decision)
  -- ^ proximate-cause decision (sibling diff)
  , brSuspicious :: ![(SiteId, Double)]
  -- ^ suspiciousness-ranked sites
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Minimize and explain a bug: shrink it, locate the invariant break, name the
-- proximate cause, and rank suspicious sites via the pass/fail spectrum drawn
-- from the campaign corpus (passing) and every found bug (failing).
explainBug :: SearchConfig -> Scenario -> SearchOutcome -> Bug -> BugReport
explainBug _cfg scen outcome bug =
  BugReport
    { brMinimal = minimal
    , brFirstBreak = bisectInvariant scen minimal
    , brProximate = fmap fst (siblingDiff (soCorpus outcome) bug)
    , brSuspicious = ochiai spectrum
    }
  where
    minimal = minimizeBug scen bug
    failing = [(covFromEvents (rrEvents (replay scen (bugInput b))), True) | b <- soBugs outcome]
    passing = [(snCov s, False) | s <- soCorpus outcome]
    spectrum = failing ++ passing
