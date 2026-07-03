-- | The typed machine surface: compile-checked state queries,
-- availableEvents, and typed payload projection in guards and actions.
module Test.StateMachine.Surface (tests) where

import Data.Maybe (fromMaybe)
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

type SurfChart =
  ChartWith
    "surf"
    Int
    ()
    '[ "AE" ::: ()
     , "PE" ::: ()
     , "R" ::: ()
     , "BE" ::: ()
     , "SET" ::: Int
     ]
    '[ Compound
        "p"
        "a"
        '[ State
            "a"
            '[ On "AE" ==> Stay
             , On "R" ==> Stay
             , After 500 ==> To "b"
             , Always ?: "never" ==> To "b"
             , On "SET" ?: "positive" ==> Stay ! '["store"]
             ]
         , State "b" '[On "BE" ==> Stay]
         ]
        '[On "PE" ==> Stay]
     ]
    "p"
    '[On "R" ==> Stay]

surfImpl :: ChartImpl LogM SurfChart
surfImpl =
  chartImpl
    ( mkGuard @"never" (\_ _ -> False)
        :& mkGuard @"positive" (\_ ev -> maybe False (> 0) (onEvent @"SET" ev))
        :& RNil
    )
    (assign @"store" (\ctx ev -> fromMaybe ctx (onEvent @"SET" ev)) :& RNil)
    RNil
    RNil
    (const ())

tests :: Spec
tests = describe "typed surface" $ do
  it "matches reflects the configuration, ancestors included" $ do
    let m = boot surfImpl 0
    matches @"p" m `shouldBe` True
    matches @"a" m `shouldBe` True
    matches @"b" m `shouldBe` False

  it "availableEvents lists exactly the named triggers of active states and the root, deduplicated" $ do
    -- Active: a (AE, R, SET — After and Always excluded) and p (PE);
    -- the root contributes R again (deduplicated); inactive b's BE is absent.
    availableEvents surfImpl (boot surfImpl 0) `shouldBe` ["AE", "PE", "R", "SET"]

  it "a typed payload flows through guard and action via onEvent" $ do
    let m = advance surfImpl (boot surfImpl 0) (EvExternal (mkEvent @"SET" 5))
    ctxOf m `shouldBe` 5

  it "a payload rejected by the guard changes nothing" $ do
    let s = stepOnce surfImpl (boot surfImpl 0) (EvExternal (mkEvent @"SET" (-3)))
    ctxOf (sMachine s) `shouldBe` 0
    case sTrace s of
      [mt] -> mtSelected mt `shouldBe` []
      other -> expectationFailure ("expected one microstep, got " <> show (length other))

  it "matchEvent projects the payload for the matching name only" $ do
    let e = mkEvent @"SET" 7 :: EventVal SurfChart
    matchEvent @"SET" e `shouldBe` Just 7
    matchEvent @"AE" e `shouldBe` Nothing
