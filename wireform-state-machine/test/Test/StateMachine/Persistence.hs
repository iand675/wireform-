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

{- | A chart exercising everything a snapshot stores: a compound with a
timer, an invocation, and shallow history; a parallel state; a top-level
final reachable through a root handler; and a root-level invocation.
-}
type PersChart =
  ChartWith
    "pers"
    Int
    Int
    '[ "GO" ::: ()
     , "TOW" ::: ()
     , "END" ::: ()
     ]
    '[ Compound
        "p"
        "a"
        '[ State
            "a"
            '[ After 100 ==> To "b"
             , Invoke "ivA" "fetch" '[OnDone ==> To "b"] '[]
             , On "GO" ==> To "b" ! '["bump"]
             ]
         , State "b" '[]
         , Hist "ph"
         ]
        '[On "TOW" ==> To "w"]
     , Parallel
        "w"
        '[ Compound "r1" "r1a" '[State "r1a" '[]] '[]
         , Compound "r2" "r2a" '[State "r2a" '[]] '[]
         ]
        '[]
     , Final "fin"
     ]
    "p"
    '[On "END" ==> To "fin", Invoke "ivRoot" "fetch" '[] '[]]

fetchSvc :: ServiceE LogM PersChart "fetch"
fetchSvc = mkService @"fetch" (\_ _ -> pure (Right () :: Either () ()))

persImpl :: ChartImpl LogM PersChart
persImpl =
  chartImpl
    RNil
    (assign @"bump" (\c _ -> c + 1) :& RNil)
    (fetchSvc :& RNil)
    RNil
    id

-- | A pristine snapshot of the freshly initialized machine (@p/a@).
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

type FpA =
  Chart
    "fp"
    ()
    ()
    '["E" ::: ()]
    '[ State "s" '[On "E" ==> To "t"]
     , State "t" '[]
     ]
    "s"

type FpB =
  Chart
    "fp"
    ()
    ()
    '["E" ::: ()]
    '[ State "s" '[On "E" ==> To "t", On "E" ==> To "s"]
     , State "t" '[]
     ]
    "s"

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
            [EvExternal (mkEvent_ @"GO"), EvExternal (mkEvent_ @"TOW")]
        snap = snapshot persImpl m1
        snap' = expectRight (Aeson.eitherDecode (Aeson.encode snap))
    snap' `shouldBe` snap
    let r = expectRight (restore persImpl snap')
        m2 = restoredMachine r
    restoredWarnings r `shouldBe` []
    config m1 `shouldBe` Set.fromList ["w", "r1", "r1a", "r2", "r2a"]
    config m2 `shouldBe` config m1
    ctxOf m2 `shouldBe` 1
    mHistory m1 `shouldBe` Map.fromList [("ph", Set.fromList ["b"])]
    mHistory m2 `shouldBe` mHistory m1
    isFinished m2 `shouldBe` False

  it "restoring a running machine re-arms the configuration's timers and invokes plus root invokes" $ do
    let r = expectRight (restore persImpl baseSnap)
    sortOn show (restoredEffects r)
      `shouldBe` sortOn
        show
        [ ReqStartTimer (TimerKey "a" 100 0)
        , ReqStartInvoke "ivA" "fetch" "a"
        , ReqStartInvoke "ivRoot" "fetch" rootName
        ]

  it "restoring a finished machine re-arms nothing" $ do
    let m1 = advance persImpl (boot persImpl 0) (EvExternal (mkEvent_ @"END"))
        r = expectRight (restore persImpl (snapshot persImpl m1))
    restoredEffects r `shouldBe` []
    case status (restoredMachine r) of
      Finished out -> out `shouldBe` 0
      Running -> expectationFailure "expected the restored machine to be Finished"

  describe "rejections" $ do
    it "unknown configuration members are rejected, fingerprint matched" $ do
      case expectLeft (restore persImpl baseSnap{snapConfig = ["p", "a", "zzz"]}) of
        UnknownStates unknown _ fpm -> do
          unknown `shouldBe` ["zzz"]
          fpm `shouldBe` True
        other -> expectationFailure ("expected UnknownStates, got " <> show other)

    it "unknown configuration members report a fingerprint mismatch after a tamper" $ do
      let tampered = baseSnap{snapConfig = ["p", "a", "zzz"], snapFingerprint = "0000000000000000"}
      case expectLeft (restore persImpl tampered) of
        UnknownStates unknown _ fpm -> do
          unknown `shouldBe` ["zzz"]
          fpm `shouldBe` False
        other -> expectationFailure ("expected UnknownStates, got " <> show other)

    it "two active children of one compound are an illegal configuration" $
      shouldRejectConfig ["p", "a", "b"] "exactly one active child"

    it "a parallel member with a missing region is an illegal configuration" $
      shouldRejectConfig ["w", "r1", "r1a"] "missing active regions"

    it "a history pseudo-state in the configuration is illegal" $
      shouldRejectConfig ["p", "a", "ph"] "history pseudo-state"

    it "a member with missing ancestors is an illegal configuration" $
      shouldRejectConfig ["p", "a", "r1a"] "missing from configuration"

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
      let r = expectRight (restore persImpl baseSnap{snapHistory = Map.fromList [("ph", ["ghost"])]})
      restoredWarnings r `shouldBe` [DroppedHistory "ph" ["ghost"]]
      mHistory (restoredMachine r) `shouldBe` Map.empty

    it "a history key that is not a history node is dropped with a warning" $ do
      let r = expectRight (restore persImpl baseSnap{snapHistory = Map.fromList [("b", ["a"])]})
      restoredWarnings r `shouldBe` [DroppedHistory "b" ["a"]]
      mHistory (restoredMachine r) `shouldBe` Map.empty

    it "a changed fingerprint that still validates restores with a warning" $ do
      let r = expectRight (restore persImpl baseSnap{snapFingerprint = "0000000000000000"})
      restoredWarnings r
        `shouldBe` [FingerprintChanged "0000000000000000" (chartFingerprint (reifyChart @PersChart))]
      config (restoredMachine r) `shouldBe` Set.fromList ["p", "a"]

  describe "recovery" $ do
    let broken = baseSnap{snapConfig = ["p", "zzz"]}

    it "restartRecovery restores a broken snapshot as a freshly initialized machine" $ do
      let (r, _) = runLog (restoreWith persImpl (restartRecovery 0) broken)
      case expectRight r of
        RecoveredByRestart stepped err -> do
          config (sMachine stepped) `shouldBe` Set.fromList ["p", "a"]
          case err of
            UnknownStates{} -> pure ()
            other -> expectationFailure ("expected UnknownStates as the cause, got " <> show other)
        Intact _ -> expectationFailure "expected recovery, not an intact restore"
        RecoveredByResume _ _ -> expectationFailure "expected a restart, not a resume"

    it "ResumeAt normalizes to a complete legal configuration (parallel regions filled)" $ do
      let rec = noRecovery{onUnknownStates = \_ _ -> Just (ResumeAt ["w"] 5)}
          (r, _) = runLog (restoreWith persImpl rec broken)
      case expectRight r of
        RecoveredByResume restored _ -> do
          config (restoredMachine restored) `shouldBe` Set.fromList ["w", "r1", "r1a", "r2", "r2a"]
          ctxOf (restoredMachine restored) `shouldBe` 5
          restoredEffects restored `shouldBe` [ReqStartInvoke "ivRoot" "fetch" rootName]
        Intact _ -> expectationFailure "expected recovery, not an intact restore"
        RecoveredByRestart _ _ -> expectationFailure "expected a resume, not a restart"

    it "ResumeAt normalizes to a complete legal configuration (ancestors filled)" $ do
      let rec = noRecovery{onUnknownStates = \_ _ -> Just (ResumeAt ["b"] 5)}
          (r, _) = runLog (restoreWith persImpl rec broken)
      case expectRight r of
        RecoveredByResume restored _ ->
          config (restoredMachine restored) `shouldBe` Set.fromList ["p", "b"]
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
