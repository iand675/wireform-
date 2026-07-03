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

type FallChart =
  Chart
    "fall"
    (Bool, Bool)
    ()
    '["E" ::: ()]
    '[ State
        "s"
        '[ On "E" ?: "g1" ==> To "t1"
         , On "E" ?: "g2" ==> To "t2"
         , On "E" ==> To "t3"
         ]
     , State "t1" '[]
     , State "t2" '[]
     , State "t3" '[]
     ]
    "s"

fallImpl :: ChartImpl LogM FallChart
fallImpl =
  chartImpl
    ( mkGuard @"g1" (\ctx _ -> fst ctx)
        :& mkGuard @"g2" (\ctx _ -> snd ctx)
        :& RNil
    )
    RNil
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  Descendant priority and root handlers
-------------------------------------------------------------------------------}

type PrioChart =
  ChartWith
    "prio"
    ()
    ()
    '[ "E" ::: ()
     , "G" ::: ()
     , "SWAP" ::: ()
     ]
    '[ Compound
        "c"
        "kid"
        '[ State "kid" '[On "E" ==> To "kidWin", On "SWAP" ==> To "mute"]
         , State "mute" '[On "G" ==> To "muteWin"]
         , State "kidWin" '[]
         , State "muteWin" '[]
         ]
        '[On "E" ==> To "ancWin"]
     , State "ancWin" '[]
     , State "rootWin" '[]
     ]
    "c"
    '[On "G" ==> To "rootWin"]

prioImpl :: ChartImpl LogM PrioChart
prioImpl = chartImpl RNil RNil RNil RNil (const ())

{-------------------------------------------------------------------------------
  Wildcard
-------------------------------------------------------------------------------}

type WildChart =
  Chart
    "wild"
    ()
    ()
    '[ "A" ::: ()
     , "B" ::: Int
     ]
    '[ State "s" '[Wildcard ==> To "w"]
     , State "w" '[]
     ]
    "s"

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
      let m = advance fallImpl (boot fallImpl (False, True)) (EvExternal (mkEvent_ @"E"))
      config m `shouldBe` Set.fromList ["t2"]

    it "a passing guard shadows every later transition for the same event" $ do
      let m = advance fallImpl (boot fallImpl (True, True)) (EvExternal (mkEvent_ @"E"))
      config m `shouldBe` Set.fromList ["t1"]

    it "all guards failing selects the unguarded transition" $ do
      let m = advance fallImpl (boot fallImpl (False, False)) (EvExternal (mkEvent_ @"E"))
      config m `shouldBe` Set.fromList ["t3"]

  describe "descendant priority" $ do
    it "a descendant's own handler beats the ancestor's for the same event" $ do
      let m = advance prioImpl (boot prioImpl ()) (EvExternal (mkEvent_ @"E"))
      config m `shouldBe` Set.fromList ["c", "kidWin"]

    it "the ancestor handler fires when no descendant handler matches" $ do
      let m0 = feed prioImpl (boot prioImpl ()) [EvExternal (mkEvent_ @"SWAP")]
          m = advance prioImpl m0 (EvExternal (mkEvent_ @"E"))
      config m `shouldBe` Set.fromList ["ancWin"]

  describe "root-level handlers" $ do
    it "a root handler fires from a state with no handler of its own" $ do
      let m = advance prioImpl (boot prioImpl ()) (EvExternal (mkEvent_ @"G"))
      config m `shouldBe` Set.fromList ["rootWin"]

    it "an active state's own handler shadows the root handler" $ do
      let m0 = feed prioImpl (boot prioImpl ()) [EvExternal (mkEvent_ @"SWAP")]
          m = advance prioImpl m0 (EvExternal (mkEvent_ @"G"))
      config m `shouldBe` Set.fromList ["c", "muteWin"]

  describe "wildcard" $ do
    it "matches a payload-less declared event" $ do
      let m = advance wildImpl (boot wildImpl ()) (EvExternal (mkEvent_ @"A"))
      config m `shouldBe` Set.fromList ["w"]

    it "matches a declared event with a payload" $ do
      let m = advance wildImpl (boot wildImpl ()) (EvExternal (mkEvent @"B" 3))
      config m `shouldBe` Set.fromList ["w"]

    it "does not match a timer event" $ do
      let m0 = boot wildImpl ()
      shouldDrop m0 (stepOnce wildImpl m0 (EvTimer (TimerKey "s" 100 0)))

    it "does not match a done event" $ do
      let m0 = boot wildImpl ()
      shouldDrop m0 (stepOnce wildImpl m0 (EvDone "s" Nothing))

    it "does not match invoke lifecycle events" $ do
      let m0 = boot wildImpl ()
      shouldDrop m0 (stepOnce wildImpl m0 (EvInvokeDone "x" (toDyn ())))
      shouldDrop m0 (stepOnce wildImpl m0 (EvInvokeError "x" (toDyn ())))

  describe "dropped events" $ do
    it "an event no active state handles leaves the machine unchanged with an empty selection" $ do
      -- ancWin is a top-level leaf: neither it nor the root handles E.
      let m0 =
            feed
              prioImpl
              (boot prioImpl ())
              [EvExternal (mkEvent_ @"SWAP"), EvExternal (mkEvent_ @"E")]
      config m0 `shouldBe` Set.fromList ["ancWin"]
      shouldDrop m0 (stepOnce prioImpl m0 (EvExternal (mkEvent_ @"E")))
