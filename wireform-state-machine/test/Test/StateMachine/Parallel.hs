{-# LANGUAGE TemplateHaskell #-}

-- | Parallel (orthogonal) regions: simultaneous entry, one macrostep
-- firing non-conflicting transitions in several regions, preemption of
-- conflicting selections, and child-first exit of all regions.
module Test.StateMachine.Parallel (tests) where

import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

data ParState = P | R1 | A1 | B1 | R2 | A2 | B2 | Out
data ParEvent = T | C | X
data ParAction = ExitA1 | ExitA2 | ExitR1 | ExitR2 | ExitP
deriveKeyKind ''ParState
deriveKeyKind ''ParEvent
deriveKeyKind ''ParAction

type ParChart :: ChartSpec ParState ParEvent NoKey ParAction NoKey NoKey NoKey
type ParChart =
  Chart
    "par"
    ()
    ()
    '[ 'T ::: ()
     , 'C ::: ()
     , 'X ::: ()
     ]
    '[ Parallel
        'P
        '[ Compound
            'R1
            'A1
            '[ State 'A1 '[Exit '[ 'ExitA1], On 'T ==> To 'B1, On 'C ==> To 'B1]
             , State 'B1 '[]
             ]
            '[Exit '[ 'ExitR1]]
         , Compound
            'R2
            'A2
            '[ State 'A2 '[Exit '[ 'ExitA2], On 'T ==> To 'B2]
             , State 'B2 '[]
             ]
            '[Exit '[ 'ExitR2]]
         ]
        '[Exit '[ 'ExitP], On 'C ==> To 'Out, On 'X ==> To 'Out]
     , State 'Out '[]
     ]
    'P

parImpl :: ChartImpl LogM ParChart
parImpl =
  chartImpl
    RNil
    ( logAct @'ExitA1
        :& logAct @'ExitA2
        :& logAct @'ExitR1
        :& logAct @'ExitR2
        :& logAct @'ExitP
        :& RNil
    )
    RNil
    RNil
    (const ())

tests :: Spec
tests = describe "parallel regions" $ do
  it "entering a parallel state enters every region's initial" $ do
    let m = boot parImpl ()
    config m `shouldBe` Set.fromList ["P", "R1", "A1", "R2", "A2"]

  it "one event fires non-conflicting transitions in several regions in one macrostep" $ do
    let m0 = boot parImpl ()
        s = stepOnce parImpl m0 (EvExternal (mkEvent_ @'T))
    config (sMachine s) `shouldBe` Set.fromList ["P", "R1", "B1", "R2", "B2"]
    case sTrace s of
      [mt] -> mtSelected mt `shouldBe` [("A1", 0), ("A2", 0)]
      other -> expectationFailure ("expected one microstep, got " <> show (length other))

  it "conflicting selections resolve by preemption: the descendant's transition wins" $ do
    -- A1 handles C itself; the parallel state also handles C by leaving.
    -- Their exit sets clash, so the descendant's selection preempts.
    let m = advance parImpl (boot parImpl ()) (EvExternal (mkEvent_ @'C))
    config m `shouldBe` Set.fromList ["P", "R1", "B1", "R2", "A2"]

  it "exiting the parallel exits every region, children before parents" $ do
    let m0 = boot parImpl ()
        (s, lg) = stepLog parImpl m0 (EvExternal (mkEvent_ @'X))
    lg `shouldBe` ["ExitA2", "ExitR2", "ExitA1", "ExitR1", "ExitP"]
    config (sMachine s) `shouldBe` Set.fromList ["Out"]
