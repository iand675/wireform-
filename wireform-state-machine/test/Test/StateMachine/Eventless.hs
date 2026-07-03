-- | Eventless (always) transitions and events raised by actions: same
-- macrostep processing, guarded cascades, and loop detection.
module Test.StateMachine.Eventless (tests) where

import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

type AlwaysChart =
  Chart
    "always"
    Bool
    ()
    '[ "GO" ::: ()
     , "RAISE" ::: ()
     , "PING" ::: ()
     , "LOOP" ::: ()
     ]
    '[ State
        "s0"
        '[ On "GO" ==> To "s1"
         , On "RAISE" ==> Stay ! '["raisePing"]
         , On "PING" ==> To "pinged"
         , On "LOOP" ==> To "la"
         ]
     , State "s1" '[Always ?: "pass" ==> To "s2"]
     , State "s2" '[Always ==> To "s3"]
     , State "s3" '[]
     , State "pinged" '[]
     , State "la" '[Always ==> To "lb"]
     , State "lb" '[Always ==> To "la"]
     ]
    "s0"

alwaysImpl :: ChartImpl LogM AlwaysChart
alwaysImpl =
  chartImpl
    (mkGuard @"pass" (\ctx _ -> ctx) :& RNil)
    (raiseEvent @"raisePing" (\_ _ -> mkEvent_ @"PING") :& RNil)
    RNil
    RNil
    (const ())

tests :: Spec
tests = describe "eventless and raised events" $ do
  it "a guarded Always fires on entry when its guard passes, cascading in one macrostep" $ do
    -- s1's guarded always passes, s2's unguarded always chains: GO lands
    -- in s3 without further external events.
    let m = advance alwaysImpl (boot alwaysImpl True) (EvExternal (mkEvent_ @"GO"))
    config m `shouldBe` Set.fromList ["s3"]

  it "a guarded Always does not fire while its guard fails" $ do
    let m = advance alwaysImpl (boot alwaysImpl False) (EvExternal (mkEvent_ @"GO"))
    config m `shouldBe` Set.fromList ["s1"]

  it "events raised by actions are processed before the macrostep returns" $ do
    let m = advance alwaysImpl (boot alwaysImpl True) (EvExternal (mkEvent_ @"RAISE"))
    config m `shouldBe` Set.fromList ["pinged"]

  it "an unguarded Always cycle reports EventlessLoop instead of hanging" $ do
    let m0 = boot alwaysImpl True
        (r, _) = runLog (step alwaysImpl m0 (EvExternal (mkEvent_ @"LOOP")))
    case r of
      Left (EventlessLoop _) -> pure ()
      Left other -> expectationFailure ("expected EventlessLoop, got " <> show other)
      Right _ -> expectationFailure "expected the step to fault with EventlessLoop"
