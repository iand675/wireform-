{-# LANGUAGE OverloadedStrings #-}

-- | Acceptance suite for @wireform-dst@: one group per phase plus a FIND → NAIL
-- integration test proving the pipeline finds, minimizes, and reproduces a real
-- durability bug end-to-end and deterministically.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog (forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Sim
import Sim.Coverage
import Sim.Assert (campaignAssertFailures, emptyAssertReport, foldAsserts, runFailFromAsserts)
import Sim.Localize (bisectInvariant, ddminList, ochiai)
import Sim.Types (Decision (..), MsgId (..), SimEvent (..), StateKey (..))
import Sim.Entropy (newGen, nextWord64, splitGen)
import Sim.Interp (StepResult (..), initWorld, runnable, step)
import Sim.Search (Bug (..), search, soBugs)
import Sim.World (World, nsDurable, wClock, wLinks, wNodes)
import Test.Syd
import Test.Syd.Hedgehog ()

main :: IO ()
main =
  sydTest $
    describe "wireform-dst" $
      sequence_
        [ entropyProps
        , interpUnits
        , coverageUnits
        , assertUnits
        , searchIntegration
        , localizeUnits
        , registerCampaign
        ]

-- ============================================================
-- Phase 0 — entropy
-- ============================================================

entropyProps :: Spec
entropyProps =
  describe "Sim.Entropy" $
    it "splitGen yields two reproducible, differing streams" $
      property $ do
        s <- forAll (Gen.word64 (Range.linearBounded))
        let (ga, gb) = splitGen (newGen s)
            (a, _) = nextWord64 ga
            (b, _) = nextWord64 gb
            (a', _) = nextWord64 (fst (splitGen (newGen s)))
        -- reproducible
        a === a'
        -- the two split streams differ
        H.assert (a /= b)

-- ============================================================
-- Phase 1 — interpreter
-- ============================================================

noNemesis :: NemesisConfig
noNemesis = NemesisConfig 0 [] (SimTime maxBound)

twoFiber :: Prog ()
twoFiber = do
  _ <- fork (do (_, body) <- recv; storeWrite (Key "got") body)
  send (NodeId 0) "hello"

interpUnits :: Spec
interpUnits =
  describe "Sim.Interp" $
    sequence_
      [ it "step is deterministic in (world, decision)" $ do
          let scen = Scenario noNemesis Map.empty [(NodeId 0, twoFiber)] (const True)
              w0 = initWorld (Seed 9) scen
              d0 = head (runnable w0)
              StepResult wa ea = step w0 d0
              StepResult wb eb = step w0 d0
          ea `shouldBe` eb
          wClock wa `shouldBe` wClock wb
      , it "a forked child receives the parent's message" $ do
          let scen = Scenario noNemesis Map.empty [(NodeId 0, twoFiber)] (const True)
              ri =
                RunInput
                  (Seed 1)
                  Map.empty
                  [SchedNext (FiberId 0), SchedNext (FiberId 1), DeliverMsg (MsgId 0), SchedNext (FiberId 1)]
              rr = replay scen ri
          rrFail rr `shouldBe` Nothing
          durKey rr (NodeId 0) "got" `shouldBe` Just "hello"
      , it "Delay then advanceClock advances the clock and wakes the fiber" $ do
          let prog = do delay (SimTime 5); storeWrite (Key "woke") "1"
              scen = Scenario noNemesis Map.empty [(NodeId 0, prog)] (const True)
              rr = replay scen (RunInput (Seed 1) Map.empty [SchedNext (FiberId 0)])
          rrFail rr `shouldBe` Nothing
          wClock (rrFinal rr) `shouldBe` SimTime 5
          durKey rr (NodeId 0) "woke" `shouldBe` Just "1"
      ]

durKey :: RunResult -> NodeId -> ByteString -> Maybe ByteString
durKey rr (NodeId n) k = IntMap.lookup n (wNodes (rrFinal rr)) >>= Map.lookup (Key k) . nsDurable



-- ============================================================
-- Phase 2 — coverage + assertions
-- ============================================================

coverageUnits :: Spec
coverageUnits =
  describe "Sim.Coverage" $
    sequence_
      [ it "novelty is 0 for a fully-seen CovSet, positive for a new state" $ do
          let cov = covFromEvents [EvObserve (StateKey 7), EvCover (SiteId "e1")]
              seen = bumpCounts cov noNovelty
              covNew = covFromEvents [EvObserve (StateKey 7), EvObserve (StateKey 8)]
          novelty 1.0 seen cov `shouldBe` 0.0
          (novelty 1.0 seen covNew > 0) `shouldBe` True
      , it "log templates collapse numeric runs into one rare feature" $ do
          let cov = covFromEvents [EvLog (NodeId 0) "commit 41", EvLog (NodeId 0) "commit 999"]
          Map.size (csRare cov) `shouldBe` 1
      ]

assertUnits :: Spec
assertUnits =
  describe "Sim.Assert" $
    sequence_
      [ it "an Always that is false once yields a FailReason" $
          runFailFromAsserts [EvAssert (SiteId "a") Always False]
            `shouldBe` Just (AssertViolated (SiteId "a") Always)
      , it "a Sometimes never true across runs is a campaign failure" $ do
          let rep = foldAsserts [EvAssert (SiteId "s") Sometimes False] (foldAsserts [EvAssert (SiteId "s") Sometimes False] emptyAssertReport)
          campaignAssertFailures rep `shouldBe` [AssertViolated (SiteId "s") Sometimes]
          let rep2 = foldAsserts [EvAssert (SiteId "s") Sometimes True] rep
          campaignAssertFailures rep2 `shouldBe` []
      ]

-- ============================================================
-- Phase 3 — FIND
-- ============================================================

-- Invariant reachable only after Partition(1,0) + write + delivery.
syntheticScenario :: Scenario
syntheticScenario =
  Scenario
    (NemesisConfig 5 [KPartition] (SimTime maxBound))
    Map.empty
    [(NodeId 0, prog0), (NodeId 1, prog1)]
    inv
  where
    prog0 = do storeWrite (Key "x") "1"; send (NodeId 1) "m"
    prog1 = do (_, b) <- recv; storeWrite (Key "y") b
    inv w =
      let hasX = maybe False (Map.member (Key "x") . nsDurable) (IntMap.lookup 0 (wNodes w))
          hasY = maybe False (Map.member (Key "y") . nsDurable) (IntMap.lookup 1 (wNodes w))
          part = maybe False lpPartitioned (Map.lookup (NodeId 1, NodeId 0) (wLinks w))
       in not (hasX && hasY && part)


searchIntegration :: Spec
searchIntegration =
  describe "Sim.Search" $
    it "finds a reproducing bug on the synthetic partition scenario" $ do
      outcome <- search defaultSearchConfig {scMaxRuns = 2000} (Seed 1) syntheticScenario
      let bugs = soBugs outcome
      length bugs `shouldSatisfy` (> 0)
      all (\b -> rrFail (replay syntheticScenario (bugInput b)) == Just (bugReason b)) bugs
        `shouldBe` True

-- ============================================================
-- Phase 4 — NAIL
-- ============================================================

localizeUnits :: Spec
localizeUnits =
  describe "Sim.Localize" $
    sequence_
      [ it "ddminList reduces to the 1-minimal load-bearing subset" $ do
          let p xs = (3 `elem` xs) && (7 `elem` xs)
              mn = ddminList p [0 .. 9 :: Int]
          length mn `shouldBe` 2
          p mn `shouldBe` True
          p (filter (/= 3) mn) `shouldBe` False
          p (filter (/= 7) mn) `shouldBe` False
      , it "bisectInvariant finds the exact flip index of a monotone invariant" $ do
          let oneStep = do
                mb <- storeRead (Key "c")
                let n = maybe 0 BS.head mb
                storeWrite (Key "c") (BS.singleton (n + 1))
                yield
              prog = oneStep >> oneStep >> oneStep >> oneStep >> oneStep
              ctr w = case IntMap.lookup 0 (wNodes w) >>= Map.lookup (Key "c") . nsDurable of
                Just b | not (BS.null b) -> BS.head b
                _ -> 0
              scen = Scenario noNemesis Map.empty [(NodeId 0, prog)] (\w -> ctr w < 3)
              ri = RunInput (Seed 1) Map.empty (replicate 5 (SchedNext (FiberId 0)))
          bisectInvariant scen ri `shouldBe` 3
      , it "ochiai ranks failing-only sites above always-covered sites" $ do
          let cov ss = CovSet (Set.fromList (map SiteId ss)) Set.empty Map.empty
              ranked = ochiai [(cov ["a", "b"], True), (cov ["b"], False)]
          fmap fst (safeHead ranked) `shouldBe` Just (SiteId "a")
          (lookup (SiteId "a") ranked > lookup (SiteId "b") ranked) `shouldBe` True
      ]

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

-- ============================================================
-- Phase 5 — FIND → NAIL integration (replicated register durability bug)
-- ============================================================

registerWorkload :: Workload
registerWorkload = Workload "replicated-register" registerScenario

registerScenario :: Scenario
registerScenario =
  Scenario
    (NemesisConfig 5 [KTear, KCrash, KReboot] (SimTime maxBound))
    Map.empty
    [(NodeId 0, primaryLoop), (NodeId 1, backupLoop), (NodeId 99, clientProg)]
    inv
  where
    inv w =
      let dur (NodeId n) k = IntMap.lookup n (wNodes w) >>= Map.lookup (Key k) . nsDurable
          numB b = if BS.null b then 0 else BS.head b
       in case (dur (NodeId 99) "ack", dur (NodeId 99) "read") of
            (Just a, Just r) -> numB r >= numB a
            _ -> True

primaryLoop :: Prog ()
primaryLoop = do
  (from, msg) <- recv
  let tag = BS.take 1 msg
      val = BS.drop 1 msg
  if tag == "W"
    then do
      storeWrite (Key "reg") val
      cover "primary-write"
      send (NodeId 1) ("R" <> val)
      send from ("A" <> val)
    else
      if tag == "G"
        then do
          mv <- storeRead (Key "reg")
          cover "primary-read"
          send from ("V" <> maybe BS.empty id mv)
        else pure ()
  primaryLoop

backupLoop :: Prog ()
backupLoop = do
  (_, msg) <- recv
  storeWrite (Key "reg") (BS.drop 1 msg)
  fsync
  cover "backup-replicate"
  backupLoop

clientProg :: Prog ()
clientProg = do
  send (NodeId 0) ("W" <> BS.singleton 1)
  (_, ack) <- recv
  storeWrite (Key "ack") (BS.drop 1 ack)
  reachable "client-acked"
  send (NodeId 0) "G"
  (_, val) <- recv
  let v = BS.drop 1 val
  storeWrite (Key "read") (if BS.null v then BS.singleton 0 else v)
  cover "client-read"

registerCampaign :: Spec
registerCampaign =
  describe "FIND -> NAIL integration" $
    it "finds, minimizes, and reproduces the register durability bug" $ do
      result <- runCampaign defaultSearchConfig {scMaxRuns = 3000, scWorkers = 2} (Seed 7) registerWorkload
      let invReports =
            filter
              (\r -> case rrFail (replay registerScenario (brMinimal r)) of Just (InvariantBroken _) -> True; _ -> False)
              (crReports result)
      length invReports `shouldSatisfy` (> 0)
      case invReports of
        (report : _) -> do
          let rr = replay registerScenario (brMinimal report)
          case rrFail rr of
            Just (InvariantBroken _) -> pure ()
            other -> expectationFailure ("expected InvariantBroken, got " ++ show other)
        [] -> expectationFailure "no invariant-break report"
