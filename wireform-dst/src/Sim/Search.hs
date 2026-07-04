-- | FIND: guided multiverse search for bugs. A best-first frontier of world
-- 'Snapshot's is expanded by (a) enumerating the legal 'Decision's at a
-- snapshot, (b) choosing a bounded fan-out — 'FireFault' choices ranked by a
-- UCB1 bandit over 'FaultKind', the rest round-robin — and (c) rolling each
-- fork forward to score it by coverage 'novelty' plus a first-time beacon bonus.
-- Any assertion violation, invariant break, or deadlock encountered becomes a
-- 'Bug' carrying the exact 'RunInput' that reproduces it.
--
-- __Determinism.__ The control loop is single-threaded and folds results in a
-- fixed order, so a campaign is a pure function of @(seed, scenario, config)@ —
-- re-running yields byte-identical bugs and minimal paths (a hard requirement:
-- reproduction is the whole point). Parallelism comes from evaluating each
-- expansion's independent, /pure/ forks concurrently ('mapConcurrently'), whose
-- results are order-preserving; racing workers over a shared frontier would
-- forfeit determinism and are deliberately not used.
module Sim.Search
  ( Snapshot (..)
  , SearchConfig (..)
  , defaultSearchConfig
  , Bug (..)
  , SearchOutcome (..)
  , search
  , fingerprintWorld
  , pathId
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Data.Aeson (FromJSON, ToJSON)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl', partition, sortOn, uncons)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Sim.Assert (AssertReport, emptyAssertReport, foldAsserts, runFailFromAsserts)
import Sim.Coverage
import Sim.Entropy (Gen, mixKey, newGen, nextDouble, uniformR)
import Sim.Fault (availableFaults)
import Sim.Interp
import Sim.Types
import Sim.World

-- | A point in the multiverse: the world, the decisions that reached it, its
-- cumulative coverage, its depth, and (a fingerprint of) its parent's path for
-- sibling diffing in "Sim.Localize".
data Snapshot = Snapshot
  { snWorld :: !World
  , snPath :: !DecisionPath
  , snCov :: !CovSet
  , snDepth :: !Int
  , snParent :: !(Maybe Int)
  }

-- | Search tuning. All defaults are in 'defaultSearchConfig'.
data SearchConfig = SearchConfig
  { scForkWidth :: !Int
  -- ^ children per expansion
  , scHorizon :: !Int
  -- ^ steps rolled out per fork before scoring
  , scMaxRuns :: !Int
  -- ^ campaign budget (expansions)
  , scNovBeta :: !Double
  -- ^ novelty weight
  , scWorkers :: !Int
  -- ^ concurrency for the per-expansion fork batch
  }
  deriving stock (Eq, Show)

-- | @scForkWidth=8@, @scHorizon=64@, @scMaxRuns=5000@, @scNovBeta=1.0@,
-- @scWorkers=4@.
defaultSearchConfig :: SearchConfig
defaultSearchConfig =
  SearchConfig
    { scForkWidth = 8
    , scHorizon = 64
    , scMaxRuns = 5000
    , scNovBeta = 1.0
    , scWorkers = 4
    }

-- | A reproducible bug: the input that triggers it and the failure it triggers.
data Bug = Bug
  { bugInput :: !RunInput
  , bugReason :: !FailReason
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The result of a campaign.
data SearchOutcome = SearchOutcome
  { soBugs :: ![Bug]
  , soCorpus :: ![Snapshot]
  , soCov :: !CovSet
  , soReport :: !AssertReport
  , soRuns :: !Int
  -- ^ number of expansions performed
  }

-- * Fingerprints ----------------------------------------------------------

-- | A replay-stable fingerprint of a world (for POR-lite frontier dedup):
-- clock, faults fired, per-fiber (id, status), and in-flight message ids.
fingerprintWorld :: World -> Word64
fingerprintWorld w =
  let StateKey k = mixKey (clk : ff : fiberSig ++ msgSig) in k
  where
    SimTime clk = wClock w
    ff = fromIntegral (wFaultsFired w)
    fiberSig = concatMap (\(i, fb) -> [fromIntegral i, statusCode (fbStatus fb)]) (IntMap.toAscList (wFibers w))
    msgSig = map fromIntegral (IntMap.keys (wInFlight w))
    statusCode = \case
      FRunnable -> 0
      FBlockedRecv -> 1
      FBlockedTimer _ -> 2
      FDead -> 3

-- | A stable id for a decision path (parent identity for sibling diffing).
pathId :: DecisionPath -> Int
pathId ds = let StateKey k = mixKey (concatMap decCode ds) in fromIntegral k
  where
    decCode = \case
      SchedNext (FiberId f) -> [0, fromIntegral f]
      DeliverMsg (MsgId m) -> [1, fromIntegral m]
      FireFault i -> [2, fromIntegral i]

-- * Fault kinds -----------------------------------------------------------

faultKindOf :: FaultOp -> FaultKind
faultKindOf = \case
  DropMessage{} -> KDrop
  Partition{} -> KPartition
  Heal{} -> KPartition
  Clog{} -> KClog
  CrashNode{} -> KCrash
  RebootNode{} -> KReboot
  PauseNode{} -> KPause
  ClockSkew{} -> KSkew
  CorruptWrite{} -> KCorrupt
  TearWrite{} -> KTear
  DiskLatency{} -> KDiskLatency

decisionKind :: World -> Decision -> Maybe FaultKind
decisionKind w (FireFault i) =
  let fs = availableFaults (wNemesis w) w
   in if i >= 0 && i < length fs then Just (faultKindOf (fs !! i)) else Nothing
decisionKind _ _ = Nothing

-- * Forks -----------------------------------------------------------------

-- Probability the nemesis fires an available fault at a rollout step (rather
-- than driving normally). The rollout's fault coin-flips use a search-local
-- PRNG (not the world's @wGen@), so the world evolves identically under
-- 'replay' of the recorded decisions.
faultProb :: Double
faultProb = 0.4

data ForkResult = ForkResult
  { frChildWorld :: !World
  , frChildPath :: !DecisionPath
  , frChildCov :: !CovSet
  , frLookCov :: !CovSet
  -- ^ cumulative coverage incl. look-ahead, for the novelty reward
  , frFail :: !(Maybe FailReason)
  , frFailPath :: !DecisionPath
  -- ^ full reproducing path (valid when 'frFail' is @Just@)
  , frLookEvents :: ![SimEvent]
  , frKind :: !(Maybe FaultKind)
  , frFP :: !Word64
  , frDepth :: !Int
  }

-- Force the NFData-able outputs (which drives the look-ahead computation) so
-- the concurrent evaluation actually happens off the control thread.
forceFork :: ForkResult -> ForkResult
forceFork fr =
  frChildCov fr `deepseq`
    frLookCov fr `deepseq`
      frLookEvents fr `deepseq`
        frFail fr `deepseq`
          frFailPath fr `deepseq`
            frChildPath fr `deepseq`
              (frFP fr `seq` frChildWorld fr `seq` fr)

isFaultD :: Decision -> Bool
isFaultD (FireFault _) = True
isFaultD _ = False

-- A nemesis-driven look-ahead: roll @w@ forward up to @n@ steps, at each step
-- either firing a random available fault (probability 'faultProb', local PRNG)
-- or driving the first non-fault runnable decision, recording every decision so
-- a discovered failure is exactly reproducible. Returns the decisions taken,
-- the events, and the first failure (if any).
rollout :: Scenario -> Gen -> Int -> World -> (DecisionPath, [SimEvent], Maybe FailReason)
rollout scen = go [] []
  where
    inv = scenInvariant scen
    go accDs accEv g n w
      | n <= 0 = (accDs, accEv, Nothing)
      | not (inv w) = (accDs, accEv, Just (InvariantBroken "scenario invariant"))
      | otherwise =
          let rs = runnable w
              faults = filter isFaultD rs
              nonfaults = filter (not . isFaultD) rs
              (coin, g1) = nextDouble g
              (mchoice, g2)
                | null nonfaults && null faults = (Nothing, g1)
                | null nonfaults = pickFault faults g1
                | not (null faults) && coin < faultProb = pickFault faults g1
                | otherwise = (listToMaybe nonfaults, g1)
           in case mchoice of
                Just d ->
                  let StepResult w' evs = step w d
                   in case runFailFromAsserts evs of
                        Just r -> (accDs ++ [d], accEv ++ evs, Just r)
                        Nothing -> go (accDs ++ [d]) (accEv ++ evs) g2 (n - 1) w'
                Nothing ->
                  if canAdvanceTime w
                    then go accDs accEv g2 (n - 1) (advanceClock w)
                    else
                      if deadlocked w
                        then (accDs, accEv, Just Deadlock)
                        else (accDs, accEv, Nothing)
    pickFault fs g = let (i, g') = uniformR (0, length fs - 1) g in (Just (fs !! i), g')

-- Apply one chosen decision to a snapshot (that is the enqueued child), then
-- run a nemesis look-ahead to score it and to catch deep multi-fault bugs.
evalFork :: Scenario -> SearchConfig -> Snapshot -> Decision -> ForkResult
evalFork scen cfg snap d =
  let StepResult w1 e1 = step (snWorld snap) d
      kind = decisionKind (snWorld snap) d
      childPath = snPath snap ++ [d]
      childCov = mergeCov (snCov snap) (covFromEvents e1)
      base mf failPath lookEvs =
        ForkResult
          { frChildWorld = w1
          , frChildPath = childPath
          , frChildCov = childCov
          , frLookCov = mergeCov (snCov snap) (covFromEvents lookEvs)
          , frFail = mf
          , frFailPath = failPath
          , frLookEvents = lookEvs
          , frKind = kind
          , frFP = fingerprintWorld w1
          , frDepth = snDepth snap + 1
          }
   in case runFailFromAsserts e1 of
        Just r -> base (Just r) childPath e1
        Nothing
          | not (scenInvariant scen w1) -> base (Just (InvariantBroken "scenario invariant")) childPath e1
          | otherwise ->
              let g = newGen (fromIntegral (pathId childPath))
                  (rollDs, rollEv, mf) = rollout scen g (scHorizon cfg) w1
               in base mf (childPath ++ rollDs) (e1 ++ rollEv)

-- * Decision selection ----------------------------------------------------

-- Choose up to @scForkWidth@ decisions: faults ranked by UCB1 over their kind,
-- scheds and delivers round-robined, then interleaved.
chooseDecisions :: SearchConfig -> Map FaultKind (Int, Double) -> Int -> World -> [Decision] -> [Decision]
chooseDecisions cfg bandit tPulls w rs =
  take (scForkWidth cfg) (interleave [scheds, delivers, faultRanked])
  where
    (scheds, rest) = partition isSched rs
    (delivers, faults) = partition isDeliver rest
    faultRanked = sortOn (Down . ucb) faults
    ucb d = case decisionKind w d of
      Nothing -> 0
      Just k ->
        let (n, tot) = Map.findWithDefault (0, 0) k bandit
         in if n == 0
              then 1 / 0 -- unexplored kind: maximally attractive
              else tot / fromIntegral n + sqrt (2 * log (fromIntegral (max 1 tPulls)) / fromIntegral n)
    isSched (SchedNext _) = True
    isSched _ = False
    isDeliver (DeliverMsg _) = True
    isDeliver _ = False

interleave :: [[a]] -> [a]
interleave xss = case mapMaybe uncons xss of
  [] -> []
  ps -> map fst ps ++ interleave (map snd ps)

-- * The loop --------------------------------------------------------------

data LoopState = LoopState
  { lsFrontier :: !(Map (Down Double, Int) (Int, Snapshot))
  , lsCounts :: !GlobalCounts
  , lsBugs :: ![Bug]
  , lsSeenFP :: !(Set Word64)
  , lsSeenBug :: !(Set Int)
  , lsBandit :: !(Map FaultKind (Int, Double))
  , lsBeacons :: !(Set SiteId)
  , lsReport :: !AssertReport
  , lsCorpus :: ![Snapshot]
  , lsCov :: !CovSet
  , lsSeq :: !Int
  , lsRuns :: !Int
  }

beaconBonus :: Double
beaconBonus = 5.0

-- Stop recording new bugs past this many distinct ones (bounds ddmin cost).
maxBugs :: Int
maxBugs = 20

-- | Run a guided search campaign. Deterministic in @(seed, scenario, config)@.
search :: SearchConfig -> Seed -> Scenario -> IO SearchOutcome
search cfg seed scen = do
  let w0 = initWorld seed scen
      root = Snapshot w0 [] emptyCov 0 Nothing
      s0 =
        LoopState
          { lsFrontier = Map.singleton (Down 0, 0) (0, root)
          , lsCounts = noNovelty
          , lsBugs = []
          , lsSeenFP = Set.singleton (fingerprintWorld w0)
          , lsSeenBug = Set.empty
          , lsBandit = Map.empty
          , lsBeacons = Set.empty
          , lsReport = emptyAssertReport
          , lsCorpus = []
          , lsCov = emptyCov
          , lsSeq = 1
          , lsRuns = 0
          }
  final <- loop s0
  pure
    SearchOutcome
      { soBugs = reverse (lsBugs final)
      , soCorpus = reverse (lsCorpus final)
      , soCov = lsCov final
      , soReport = lsReport final
      , soRuns = lsRuns final
      }
  where
    loop s
      | lsRuns s >= scMaxRuns cfg = pure s
      | Map.null (lsFrontier s) = pure s
      | otherwise = do
          let (((Down _, _), (pid, snap)), frontier') = Map.deleteFindMin (lsFrontier s)
              rs = runnable (snWorld snap)
              chosen = chooseDecisions cfg (lsBandit s) (lsRuns s + 1) (snWorld snap) rs
          results <- mapConcurrently (evaluate . forceFork . evalFork scen cfg snap) chosen
          let s1 = s {lsFrontier = frontier', lsRuns = lsRuns s + 1}
              s2 = foldl' (integrate pid) s1 results
          loop s2

    -- Fold one fork result into the loop state (deterministic).
    integrate parentId s fr =
      let reward0 = novelty (scNovBeta cfg) (lsCounts s) (frLookCov fr)
          newBeacons = [b | b <- beaconSites (frLookEvents fr), not (Set.member b (lsBeacons s))]
          reward = reward0 + beaconBonus * fromIntegral (length newBeacons)
          bandit' = case frKind fr of
            Nothing -> lsBandit s
            Just k -> Map.insertWith addPull k (1, reward) (lsBandit s)
          sBase =
            s
              { lsBandit = bandit'
              , lsReport = foldAsserts (frLookEvents fr) (lsReport s)
              , lsCov = mergeCov (lsCov s) (frLookCov fr)
              , lsCounts = bumpCounts (frChildCov fr) (lsCounts s)
              , lsBeacons = foldr Set.insert (lsBeacons s) newBeacons
              }
       in case frFail fr of
            Just r ->
              let fp = pathId (frFailPath fr)
               in if Set.member fp (lsSeenBug sBase) || length (lsBugs sBase) >= maxBugs
                    then sBase
                    else
                      sBase
                        { lsBugs = Bug (RunInput seed Map.empty (frFailPath fr)) r : lsBugs sBase
                        , lsSeenBug = Set.insert fp (lsSeenBug sBase)
                        }
            Nothing ->
              if Set.member (frFP fr) (lsSeenFP sBase)
                then sBase
                else
                  let child = Snapshot (frChildWorld fr) (frChildPath fr) (frChildCov fr) (frDepth fr) (Just parentId)
                      myId = pathId (frChildPath fr)
                      key = (Down reward, lsSeq sBase)
                      corpus' = if reward > 0 then child : lsCorpus sBase else lsCorpus sBase
                   in sBase
                        { lsFrontier = Map.insert key (myId, child) (lsFrontier sBase)
                        , lsSeenFP = Set.insert (frFP fr) (lsSeenFP sBase)
                        , lsSeq = lsSeq sBase + 1
                        , lsCorpus = corpus'
                        }

    addPull (n1, r1) (n2, r2) = (n1 + n2, r1 + r2)

-- Beacon sites that fired true in this event trace.
beaconSites :: [SimEvent] -> [SiteId]
beaconSites = mapMaybe f
  where
    f (EvAssert s Reachable True) = Just s
    f (EvAssert s Sometimes True) = Just s
    f _ = Nothing
