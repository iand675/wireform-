-- | Parallel (orthogonal) regions: simultaneous entry, one macrostep
-- firing non-conflicting transitions in several regions, preemption of
-- conflicting selections, and child-first exit of all regions.
module Test.StateMachine.Parallel (tests) where

import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

type ParChart =
  Chart
    "par"
    ()
    ()
    '[ "T" ::: ()
     , "C" ::: ()
     , "X" ::: ()
     ]
    '[ Parallel
        "p"
        '[ Compound
            "r1"
            "a1"
            '[ State "a1" '[Exit '["exitA1"], On "T" ==> To "b1", On "C" ==> To "b1"]
             , State "b1" '[]
             ]
            '[Exit '["exitR1"]]
         , Compound
            "r2"
            "a2"
            '[ State "a2" '[Exit '["exitA2"], On "T" ==> To "b2"]
             , State "b2" '[]
             ]
            '[Exit '["exitR2"]]
         ]
        '[Exit '["exitP"], On "C" ==> To "out", On "X" ==> To "out"]
     , State "out" '[]
     ]
    "p"

parImpl :: ChartImpl LogM ParChart
parImpl =
  chartImpl
    RNil
    ( logAct @"exitA1"
        :& logAct @"exitA2"
        :& logAct @"exitR1"
        :& logAct @"exitR2"
        :& logAct @"exitP"
        :& RNil
    )
    RNil
    RNil
    (const ())

tests :: Spec
tests = describe "parallel regions" $ do
  it "entering a parallel state enters every region's initial" $ do
    let m = boot parImpl ()
    config m `shouldBe` Set.fromList ["p", "r1", "a1", "r2", "a2"]

  it "one event fires non-conflicting transitions in several regions in one macrostep" $ do
    let m0 = boot parImpl ()
        s = stepOnce parImpl m0 (EvExternal (mkEvent_ @"T"))
    config (sMachine s) `shouldBe` Set.fromList ["p", "r1", "b1", "r2", "b2"]
    case sTrace s of
      [mt] -> mtSelected mt `shouldBe` [("a1", 0), ("a2", 0)]
      other -> expectationFailure ("expected one microstep, got " <> show (length other))

  it "conflicting selections resolve by preemption: the descendant's transition wins" $ do
    -- a1 handles C itself; the parallel state also handles C by leaving.
    -- Their exit sets clash, so the descendant's selection preempts.
    let m = advance parImpl (boot parImpl ()) (EvExternal (mkEvent_ @"C"))
    config m `shouldBe` Set.fromList ["p", "r1", "b1", "r2", "a2"]

  it "exiting the parallel exits every region, children before parents" $ do
    let m0 = boot parImpl ()
        (s, lg) = stepLog parImpl m0 (EvExternal (mkEvent_ @"X"))
    lg `shouldBe` ["exitA2", "exitR2", "exitA1", "exitR1", "exitP"]
    config (sMachine s) `shouldBe` Set.fromList ["out"]
