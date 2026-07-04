{-# LANGUAGE TemplateHaskell #-}

-- | Transition selection: guard fallthrough in document order,
-- descendant priority, root-handler shadowing, wildcard scope, and
-- dropped (unhandled) events.
module Test.StateMachine.Selection (tests) where

import Data.Dynamic (toDyn)
import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)
import StateMachine.Runtime (TimerKey (..))

import Test.StateMachine.Support

{-------------------------------------------------------------------------------
  Guards and document order
-------------------------------------------------------------------------------}

data FallState = S | T1 | T2 | T3
data FallEvent = FE
data FallGuard = G1 | G2
deriveKeyKind ''FallState
deriveKeyKind ''FallEvent
deriveKeyKind ''FallGuard

type FallChart :: ChartSpec FallState FallEvent FallGuard NoKey NoKey NoKey NoKey
type FallChart =
  Chart
    "fall"
    (Bool, Bool)
    ()
    '[ 'FE ::: ()]
    '[ State
        'S
        '[ On 'FE ?: 'G1 ==> To 'T1
         , On 'FE ?: 'G2 ==> To 'T2
         , On 'FE ==> To 'T3
         ]
     , State 'T1 '[]
     , State 'T2 '[]
     , State 'T3 '[]
     ]
    'S

fallImpl :: ChartImpl LogM FallChart
fallImpl =
  chartImpl
    ( mkGuard @'G1 (\ctx _ -> fst ctx)
        :& mkGuard @'G2 (\ctx _ -> snd ctx)
        :& RNil
    )
    RNil
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  Descendant priority and root handlers
-------------------------------------------------------------------------------}

data PrioState = C | Kid | Mute | KidWin | MuteWin | AncWin | RootWin
data PrioEvent = E | G | SWAP
deriveKeyKind ''PrioState
deriveKeyKind ''PrioEvent

type PrioChart :: ChartSpec PrioState PrioEvent NoKey NoKey NoKey NoKey NoKey
type PrioChart =
  ChartWith
    "prio"
    ()
    ()
    '[ 'E ::: ()
     , 'G ::: ()
     , 'SWAP ::: ()
     ]
    '[ Compound
        'C
        'Kid
        '[ State 'Kid '[On 'E ==> To 'KidWin, On 'SWAP ==> To 'Mute]
         , State 'Mute '[On 'G ==> To 'MuteWin]
         , State 'KidWin '[]
         , State 'MuteWin '[]
         ]
        '[On 'E ==> To 'AncWin]
     , State 'AncWin '[]
     , State 'RootWin '[]
     ]
    'C
    '[On 'G ==> To 'RootWin]

prioImpl :: ChartImpl LogM PrioChart
prioImpl = chartImpl RNil RNil RNil RNil (const ())

{-------------------------------------------------------------------------------
  Wildcard
-------------------------------------------------------------------------------}

data WildState = WS | W
data WildEvent = A | B
deriveKeyKind ''WildState
deriveKeyKind ''WildEvent

type WildChart :: ChartSpec WildState WildEvent NoKey NoKey NoKey NoKey NoKey
type WildChart =
  Chart
    "wild"
    ()
    ()
    '[ 'A ::: ()
     , 'B ::: Int
     ]
    '[ State 'WS '[Wildcard ==> To 'W]
     , State 'W '[]
     ]
    'WS

wildImpl :: ChartImpl LogM WildChart
wildImpl = chartImpl RNil RNil RNil RNil (const ())

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | A step that selected nothing: the machine did not move and the trace
-- records exactly one empty-selection microstep.
shouldDrop :: Machine spec -> Stepped spec -> IO ()
shouldDrop prior s = do
  config (sMachine s) `shouldBe` config prior
  case sTrace s of
    [mt] -> do
      mtSelected mt `shouldBe` []
      mtExited mt `shouldBe` []
      mtEntered mt `shouldBe` []
    other -> expectationFailure ("expected one microstep, got " <> show (length other))

tests :: Spec
tests = describe "transition selection" $ do
  describe "guards and document order" $ do
    it "a failing guard falls through to the next transition in document order" $ do
      let m = advance fallImpl (boot fallImpl (False, True)) (EvExternal (mkEvent_ @'FE))
      config m `shouldBe` Set.fromList ["T2"]

    it "a passing guard shadows every later transition for the same event" $ do
      let m = advance fallImpl (boot fallImpl (True, True)) (EvExternal (mkEvent_ @'FE))
      config m `shouldBe` Set.fromList ["T1"]

    it "all guards failing selects the unguarded transition" $ do
      let m = advance fallImpl (boot fallImpl (False, False)) (EvExternal (mkEvent_ @'FE))
      config m `shouldBe` Set.fromList ["T3"]

  describe "descendant priority" $ do
    it "a descendant's own handler beats the ancestor's for the same event" $ do
      let m = advance prioImpl (boot prioImpl ()) (EvExternal (mkEvent_ @'E))
      config m `shouldBe` Set.fromList ["C", "KidWin"]

    it "the ancestor handler fires when no descendant handler matches" $ do
      let m0 = feed prioImpl (boot prioImpl ()) [EvExternal (mkEvent_ @'SWAP)]
          m = advance prioImpl m0 (EvExternal (mkEvent_ @'E))
      config m `shouldBe` Set.fromList ["AncWin"]

  describe "root-level handlers" $ do
    it "a root handler fires from a state with no handler of its own" $ do
      let m = advance prioImpl (boot prioImpl ()) (EvExternal (mkEvent_ @'G))
      config m `shouldBe` Set.fromList ["RootWin"]

    it "an active state's own handler shadows the root handler" $ do
      let m0 = feed prioImpl (boot prioImpl ()) [EvExternal (mkEvent_ @'SWAP)]
          m = advance prioImpl m0 (EvExternal (mkEvent_ @'G))
      config m `shouldBe` Set.fromList ["C", "MuteWin"]

  describe "wildcard" $ do
    it "matches a payload-less declared event" $ do
      let m = advance wildImpl (boot wildImpl ()) (EvExternal (mkEvent_ @'A))
      config m `shouldBe` Set.fromList ["W"]

    it "matches a declared event with a payload" $ do
      let m = advance wildImpl (boot wildImpl ()) (EvExternal (mkEvent @'B 3))
      config m `shouldBe` Set.fromList ["W"]

    it "does not match a timer event" $ do
      let m0 = boot wildImpl ()
      shouldDrop m0 (stepOnce wildImpl m0 (EvTimer (TimerKey "WS" 100 0)))

    it "does not match a done event" $ do
      let m0 = boot wildImpl ()
      shouldDrop m0 (stepOnce wildImpl m0 (EvDone "WS" Nothing))

    it "does not match invoke lifecycle events" $ do
      let m0 = boot wildImpl ()
      shouldDrop m0 (stepOnce wildImpl m0 (EvInvokeDone "x" (toDyn ())))
      shouldDrop m0 (stepOnce wildImpl m0 (EvInvokeError "x" (toDyn ())))

  describe "dropped events" $ do
    it "an event no active state handles leaves the machine unchanged with an empty selection" $ do
      -- AncWin is a top-level leaf: neither it nor the root handles E.
      let m0 =
            feed
              prioImpl
              (boot prioImpl ())
              [EvExternal (mkEvent_ @'SWAP), EvExternal (mkEvent_ @'E)]
      config m0 `shouldBe` Set.fromList ["AncWin"]
      shouldDrop m0 (stepOnce prioImpl m0 (EvExternal (mkEvent_ @'E)))
