{-# LANGUAGE TemplateHaskell #-}

-- | Eventless (always) transitions and events raised by actions: same
-- macrostep processing, guarded cascades, and loop detection.
module Test.StateMachine.Eventless (tests) where

import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

data AState = S0 | S1 | S2 | S3 | Pinged | La | Lb
data AEvent = GO | RAISE | PING | LOOP
data AGuard = Pass
data AAction = RaisePing
deriveKeyKind ''AState
deriveKeyKind ''AEvent
deriveKeyKind ''AGuard
deriveKeyKind ''AAction

type AlwaysChart :: ChartSpec AState AEvent AGuard AAction NoKey NoKey NoKey
type AlwaysChart =
  Chart
    "always"
    Bool
    ()
    '[ 'GO ::: ()
     , 'RAISE ::: ()
     , 'PING ::: ()
     , 'LOOP ::: ()
     ]
    '[ State
        'S0
        '[ On 'GO ==> To 'S1
         , On 'RAISE ==> Stay ! '[ 'RaisePing ]
         , On 'PING ==> To 'Pinged
         , On 'LOOP ==> To 'La
         ]
     , State 'S1 '[Always ?: 'Pass ==> To 'S2]
     , State 'S2 '[Always ==> To 'S3]
     , State 'S3 '[]
     , State 'Pinged '[]
     , State 'La '[Always ==> To 'Lb]
     , State 'Lb '[Always ==> To 'La]
     ]
    'S0

alwaysImpl :: ChartImpl LogM AlwaysChart
alwaysImpl =
  chartImpl
    (mkGuard @'Pass const :& RNil)
    (raiseEvent @'RaisePing (\_ _ -> mkEvent_ @'PING) :& RNil)
    RNil
    RNil
    (const ())

tests :: Spec
tests = describe "eventless and raised events" $ do
  it "a guarded Always fires on entry when its guard passes, cascading in one macrostep" $ do
    -- S1's guarded always passes, S2's unguarded always chains: GO lands
    -- in S3 without further external events.
    let m = advance alwaysImpl (boot alwaysImpl True) (EvExternal (mkEvent_ @'GO))
    config m `shouldBe` Set.fromList ["S3"]

  it "a guarded Always does not fire while its guard fails" $ do
    let m = advance alwaysImpl (boot alwaysImpl False) (EvExternal (mkEvent_ @'GO))
    config m `shouldBe` Set.fromList ["S1"]

  it "events raised by actions are processed before the macrostep returns" $ do
    let m = advance alwaysImpl (boot alwaysImpl True) (EvExternal (mkEvent_ @'RAISE))
    config m `shouldBe` Set.fromList ["Pinged"]

  it "an unguarded Always cycle reports EventlessLoop instead of hanging" $ do
    let m0 = boot alwaysImpl True
        (r, _) = runLog (step alwaysImpl m0 (EvExternal (mkEvent_ @'LOOP)))
    case r of
      Left (EventlessLoop _) -> pure ()
      Left other -> expectationFailure ("expected EventlessLoop, got " <> show other)
      Right _ -> expectationFailure "expected the step to fault with EventlessLoop"
