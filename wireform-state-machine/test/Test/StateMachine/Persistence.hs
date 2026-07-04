{-# LANGUAGE TemplateHaskell #-}

-- | Snapshots and restore: JSON roundtrips, re-arm effects, every
-- rejection mode, sanitization warnings, recovery strategies, and the
-- structural fingerprint.
module Test.StateMachine.Persistence (tests) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Syd

import StateMachine hiding (context)
import StateMachine.Machine (Machine (..))
import StateMachine.Runtime (EffectReq (..), TimerKey (..), rootName)

import Test.StateMachine.Support

data PersState = P | A | B | Ph | W | R1 | R1a | R2 | R2a | Fin
data PersEvent = GO | TOW | END
data PersAction = Bump
data PersService = Fetch
data PersInvoke = IvA | IvRoot
deriveKeyKind ''PersState
deriveKeyKind ''PersEvent
deriveKeyKind ''PersAction
deriveKeyKind ''PersService
deriveKeyKind ''PersInvoke

{- | A chart exercising everything a snapshot stores: a compound with a
timer, an invocation, and shallow history; a parallel state; a top-level
final reachable through a root handler; and a root-level invocation.
-}
type PersChart :: ChartSpec PersState PersEvent NoKey PersAction PersService PersInvoke NoKey
type PersChart =
  ChartWith
    "pers"
    Int
    Int
    '[ 'GO ::: ()
     , 'TOW ::: ()
     , 'END ::: ()
     ]
    '[ Compound
        'P
        'A
        '[ State
            'A
            '[ After 100 ==> To 'B
             , Invoke 'IvA 'Fetch '[OnDone ==> To 'B] '[]
             , On 'GO ==> To 'B ! '[ 'Bump]
             ]
         , State 'B '[]
         , Hist 'Ph
         ]
        '[On 'TOW ==> To 'W]
     , Parallel
        'W
        '[ Compound 'R1 'R1a '[State 'R1a '[]] '[]
         , Compound 'R2 'R2a '[State 'R2a '[]] '[]
         ]
        '[]
     , Final 'Fin
     ]
    'P
    '[On 'END ==> To 'Fin, Invoke 'IvRoot 'Fetch '[] '[]]

fetchSvc :: ServiceE LogM PersChart 'Fetch
fetchSvc = mkService @'Fetch (\_ _ -> pure (Right () :: Either () ()))

persImpl :: ChartImpl LogM PersChart
persImpl =
  chartImpl
    RNil
    (assign @'Bump (\c _ -> c + 1) :& RNil)
    (fetchSvc :& RNil)
    RNil
    id

-- | A pristine snapshot of the freshly initialized machine (@P/A@).
baseSnap :: Snapshot
baseSnap = snapshot persImpl (boot persImpl 0)

expectRight :: (Show e) => Either e a -> a
expectRight = either (error . show) id

expectLeft :: Either RestoreError (Restored spec) -> RestoreError
expectLeft = either id (\_ -> error "expected restore to fail")

-- | The rejection must be IllegalConfiguration and its reason must point
-- at the right violation.
shouldRejectConfig :: [Text] -> Text -> IO ()
shouldRejectConfig cfg fragment =
  case expectLeft (restore persImpl baseSnap{snapConfig = cfg}) of
    IllegalConfiguration reason fpm -> do
      fpm `shouldBe` True
      reason `shouldSatisfy` T.isInfixOf fragment
    other -> expectationFailure ("expected IllegalConfiguration, got " <> show other)

{-------------------------------------------------------------------------------
  Fingerprint charts: identical structure vs one extra transition
-------------------------------------------------------------------------------}

data FpState = FpS | FpT
data FpEvent = FpE
deriveKeyKind ''FpState
deriveKeyKind ''FpEvent

type FpA :: ChartSpec FpState FpEvent NoKey NoKey NoKey NoKey NoKey
type FpA =
  Chart
    "fp"
    ()
    ()
    '[ 'FpE ::: ()]
    '[ State 'FpS '[On 'FpE ==> To 'FpT]
     , State 'FpT '[]
     ]
    'FpS

type FpB :: ChartSpec FpState FpEvent NoKey NoKey NoKey NoKey NoKey
type FpB =
  Chart
    "fp"
    ()
    ()
    '[ 'FpE ::: ()]
    '[ State 'FpS '[On 'FpE ==> To 'FpT, On 'FpE ==> To 'FpS]
     , State 'FpT '[]
     ]
    'FpS

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "persistence" $ do
  it "snapshot roundtrips through JSON bytes and restore reproduces config, context, history, status" $ do
    let m1 =
          feed
            persImpl
            (boot persImpl 0)
            [EvExternal (mkEvent_ @'GO), EvExternal (mkEvent_ @'TOW)]
        snap = snapshot persImpl m1
        snap' = expectRight (Aeson.eitherDecode (Aeson.encode snap))
    snap' `shouldBe` snap
    let r = expectRight (restore persImpl snap')
        m2 = restoredMachine r
    restoredWarnings r `shouldBe` []
    config m1 `shouldBe` Set.fromList ["W", "R1", "R1a", "R2", "R2a"]
    config m2 `shouldBe` config m1
    ctxOf m2 `shouldBe` 1
    mHistory m1 `shouldBe` Map.fromList [("Ph", Set.fromList ["B"])]
    mHistory m2 `shouldBe` mHistory m1
    isFinished m2 `shouldBe` False

  it "restoring a running machine re-arms the configuration's timers and invokes plus root invokes" $ do
    let r = expectRight (restore persImpl baseSnap)
    sortOn show (restoredEffects r)
      `shouldBe` sortOn
        show
        [ ReqStartTimer (TimerKey "A" 100 0)
        , ReqStartInvoke "IvA" "Fetch" "A"
        , ReqStartInvoke "IvRoot" "Fetch" rootName
        ]

  it "restoring a finished machine re-arms nothing" $ do
    let m1 = advance persImpl (boot persImpl 0) (EvExternal (mkEvent_ @'END))
        r = expectRight (restore persImpl (snapshot persImpl m1))
    restoredEffects r `shouldBe` []
    case status (restoredMachine r) of
      Finished out -> out `shouldBe` 0
      Running -> expectationFailure "expected the restored machine to be Finished"

  describe "rejections" $ do
    it "unknown configuration members are rejected, fingerprint matched" $ do
      case expectLeft (restore persImpl baseSnap{snapConfig = ["P", "A", "zzz"]}) of
        UnknownStates unknown _ fpm -> do
          unknown `shouldBe` ["zzz"]
          fpm `shouldBe` True
        other -> expectationFailure ("expected UnknownStates, got " <> show other)

    it "unknown configuration members report a fingerprint mismatch after a tamper" $ do
      let tampered = baseSnap{snapConfig = ["P", "A", "zzz"], snapFingerprint = "0000000000000000"}
      case expectLeft (restore persImpl tampered) of
        UnknownStates unknown _ fpm -> do
          unknown `shouldBe` ["zzz"]
          fpm `shouldBe` False
        other -> expectationFailure ("expected UnknownStates, got " <> show other)

    it "two active children of one compound are an illegal configuration" $
      shouldRejectConfig ["P", "A", "B"] "exactly one active child"

    it "a parallel member with a missing region is an illegal configuration" $
      shouldRejectConfig ["W", "R1", "R1a"] "missing active regions"

    it "a history pseudo-state in the configuration is illegal" $
      shouldRejectConfig ["P", "A", "Ph"] "history pseudo-state"

    it "a member with missing ancestors is an illegal configuration" $
      shouldRejectConfig ["P", "A", "R1a"] "missing from configuration"

    it "a context that no longer parses is BadContext" $ do
      case expectLeft (restore persImpl baseSnap{snapContext = String "nope"}) of
        BadContext _ fpm -> fpm `shouldBe` True
        other -> expectationFailure ("expected BadContext, got " <> show other)

    it "a snapshot from another chart is WrongChart" $ do
      case expectLeft (restore persImpl baseSnap{snapChart = "other"}) of
        err -> err `shouldBe` WrongChart{reExpected = "pers", reGot = "other"}

    it "an unreadable snapshot version is UnsupportedVersion" $ do
      expectLeft (restore persImpl baseSnap{snapVersion = 9})
        `shouldBe` UnsupportedVersion 9

  describe "warnings" $ do
    it "stored history naming a removed state is dropped with a warning" $ do
      let r = expectRight (restore persImpl baseSnap{snapHistory = Map.fromList [("Ph", ["ghost"])]})
      restoredWarnings r `shouldBe` [DroppedHistory "Ph" ["ghost"]]
      mHistory (restoredMachine r) `shouldBe` Map.empty

    it "a history key that is not a history node is dropped with a warning" $ do
      let r = expectRight (restore persImpl baseSnap{snapHistory = Map.fromList [("B", ["A"])]})
      restoredWarnings r `shouldBe` [DroppedHistory "B" ["A"]]
      mHistory (restoredMachine r) `shouldBe` Map.empty

    it "a changed fingerprint that still validates restores with a warning" $ do
      let r = expectRight (restore persImpl baseSnap{snapFingerprint = "0000000000000000"})
      restoredWarnings r
        `shouldBe` [FingerprintChanged "0000000000000000" (chartFingerprint (reifyChart @PersChart))]
      config (restoredMachine r) `shouldBe` Set.fromList ["P", "A"]

  describe "recovery" $ do
    let broken = baseSnap{snapConfig = ["P", "zzz"]}

    it "restartRecovery restores a broken snapshot as a freshly initialized machine" $ do
      let (r, _) = runLog (restoreWith persImpl (restartRecovery 0) broken)
      case expectRight r of
        RecoveredByRestart stepped err -> do
          config (sMachine stepped) `shouldBe` Set.fromList ["P", "A"]
          case err of
            UnknownStates{} -> pure ()
            other -> expectationFailure ("expected UnknownStates as the cause, got " <> show other)
        Intact _ -> expectationFailure "expected recovery, not an intact restore"
        RecoveredByResume _ _ -> expectationFailure "expected a restart, not a resume"

    it "ResumeAt normalizes to a complete legal configuration (parallel regions filled)" $ do
      let rec = noRecovery{onUnknownStates = \_ _ -> Just (ResumeAt ["W"] 5)}
          (r, _) = runLog (restoreWith persImpl rec broken)
      case expectRight r of
        RecoveredByResume restored _ -> do
          config (restoredMachine restored) `shouldBe` Set.fromList ["W", "R1", "R1a", "R2", "R2a"]
          ctxOf (restoredMachine restored) `shouldBe` 5
          restoredEffects restored `shouldBe` [ReqStartInvoke "IvRoot" "Fetch" rootName]
        Intact _ -> expectationFailure "expected recovery, not an intact restore"
        RecoveredByRestart _ _ -> expectationFailure "expected a resume, not a restart"

    it "ResumeAt normalizes to a complete legal configuration (ancestors filled)" $ do
      let rec = noRecovery{onUnknownStates = \_ _ -> Just (ResumeAt ["B"] 5)}
          (r, _) = runLog (restoreWith persImpl rec broken)
      case expectRight r of
        RecoveredByResume restored _ ->
          config (restoredMachine restored) `shouldBe` Set.fromList ["P", "B"]
        Intact _ -> expectationFailure "expected recovery, not an intact restore"
        RecoveredByRestart _ _ -> expectationFailure "expected a resume, not a restart"

    it "ResumeAt naming an unknown state fails with RecoveryFailed" $ do
      let rec = noRecovery{onUnknownStates = \_ _ -> Just (ResumeAt ["nope"] 5)}
          (r, _) = runLog (restoreWith persImpl rec broken)
      case r of
        Left (RecoveryFailed _) -> pure ()
        Left other -> expectationFailure ("expected RecoveryFailed, got " <> show other)
        Right _ -> expectationFailure "expected the recovery to fail"

  describe "fingerprint" $ do
    it "an independently demoted identical chart fingerprints identically" $
      -- baseSnap's fingerprint came from the RChart stored by chartImpl;
      -- reifyChart here demotes the spec afresh. Same structure, same hash.
      snapFingerprint baseSnap `shouldBe` chartFingerprint (reifyChart @PersChart)

    it "adding one transition changes the fingerprint" $
      chartFingerprint (reifyChart @FpA) `shouldNotBe` chartFingerprint (reifyChart @FpB)
