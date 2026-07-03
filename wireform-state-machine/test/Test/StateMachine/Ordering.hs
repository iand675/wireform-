-- | Entry\/exit ordering, LCCA scoping, and internal-vs-external
-- transition semantics — each asserted as an exact action log.
module Test.StateMachine.Ordering (tests) where

import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)

import Test.StateMachine.Support

{-------------------------------------------------------------------------------
  Entry/exit ordering
-------------------------------------------------------------------------------}

type OrderChart =
  Chart
    "order"
    ()
    ()
    '[ "CROSS" ::: ()
     , "SELF" ::: ()
     , "HOLD" ::: ()
     ]
    '[ Compound
        "p"
        "a"
        '[ State
            "a"
            '[ Entry '["enterA"]
             , Exit '["exitA"]
             , On "CROSS" ==> To "b" ! '["during"]
             , On "SELF" ==> To "a" ! '["selfAct"]
             , On "HOLD" ==> Stay ! '["holdAct"]
             ]
         ]
        '[Entry '["enterP"], Exit '["exitP"]]
     , Compound
        "q"
        "b"
        '[State "b" '[Entry '["enterB"]]]
        '[Entry '["enterQ"]]
     ]
    "p"

orderImpl :: ChartImpl LogM OrderChart
orderImpl =
  chartImpl
    RNil
    ( logAct @"enterA"
        :& logAct @"exitA"
        :& logAct @"during"
        :& logAct @"selfAct"
        :& logAct @"holdAct"
        :& logAct @"enterP"
        :& logAct @"exitP"
        :& logAct @"enterB"
        :& logAct @"enterQ"
        :& RNil
    )
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  LCCA scoping
-------------------------------------------------------------------------------}

type LccaChart =
  Chart
    "lcca"
    ()
    ()
    '[ "SIB" ::: ()
     , "OUT" ::: ()
     ]
    '[ Compound
        "p"
        "a"
        '[ State "a" '[Exit '["exitA"], On "SIB" ==> To "b", On "OUT" ==> To "c"]
         , State "b" '[Entry '["enterB"]]
         ]
        '[Entry '["enterP"], Exit '["exitP"]]
     , State "c" '[Entry '["enterC"]]
     ]
    "p"

lccaImpl :: ChartImpl LogM LccaChart
lccaImpl =
  chartImpl
    RNil
    ( logAct @"exitA"
        :& logAct @"enterB"
        :& logAct @"enterP"
        :& logAct @"exitP"
        :& logAct @"enterC"
        :& RNil
    )
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  Internal vs external
-------------------------------------------------------------------------------}

type IntExtChart =
  Chart
    "intext"
    ()
    ()
    '[ "INT" ::: ()
     , "EXT" ::: ()
     ]
    '[ Compound
        "p"
        "a"
        '[ State "a" '[Exit '["exitA"]]
         , State "b" '[Entry '["enterB"]]
         ]
        '[ Entry '["enterP"]
         , Exit '["exitP"]
         , On "INT" ==> Inside "b"
         , On "EXT" ==> To "b"
         ]
     ]
    "p"

intExtImpl :: ChartImpl LogM IntExtChart
intExtImpl =
  chartImpl
    RNil
    ( logAct @"exitA"
        :& logAct @"enterB"
        :& logAct @"enterP"
        :& logAct @"exitP"
        :& RNil
    )
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "entry/exit ordering" $ do
  it "initialization enters parent before child" $ do
    let (_, lg) = bootLog orderImpl ()
    lg `shouldBe` ["enterP", "enterA"]

  it "external transition between compounds: exits child-first, actions, enters parent-first" $ do
    let m0 = boot orderImpl ()
        (s, lg) = stepLog orderImpl m0 (EvExternal (mkEvent_ @"CROSS"))
    lg `shouldBe` ["exitA", "exitP", "during", "enterQ", "enterB"]
    config (sMachine s) `shouldBe` Set.fromList ["q", "b"]

  it "external self-transition re-runs exit and entry of the source, not its parent" $ do
    let m0 = boot orderImpl ()
        (s, lg) = stepLog orderImpl m0 (EvExternal (mkEvent_ @"SELF"))
    lg `shouldBe` ["exitA", "selfAct", "enterA"]
    config (sMachine s) `shouldBe` Set.fromList ["p", "a"]

  it "targetless Stay transition runs only its actions" $ do
    let m0 = boot orderImpl ()
        (s, lg) = stepLog orderImpl m0 (EvExternal (mkEvent_ @"HOLD"))
    lg `shouldBe` ["holdAct"]
    config (sMachine s) `shouldBe` Set.fromList ["p", "a"]
    case sTrace s of
      [mt] -> do
        mtExited mt `shouldBe` []
        mtEntered mt `shouldBe` []
      other -> expectationFailure ("expected one microstep, got " <> show (length other))

  describe "LCCA scoping" $ do
    it "sibling transition inside a compound does not exit the compound" $ do
      let m0 = boot lccaImpl ()
          (s, lg) = stepLog lccaImpl m0 (EvExternal (mkEvent_ @"SIB"))
      lg `shouldBe` ["exitA", "enterB"]
      config (sMachine s) `shouldBe` Set.fromList ["p", "b"]

    it "transition crossing the compound boundary exits the compound" $ do
      let m0 = boot lccaImpl ()
          (s, lg) = stepLog lccaImpl m0 (EvExternal (mkEvent_ @"OUT"))
      lg `shouldBe` ["exitA", "exitP", "enterC"]
      config (sMachine s) `shouldBe` Set.fromList ["c"]

  describe "internal vs external" $ do
    it "Inside on a compound enters the child without exiting the compound" $ do
      let m0 = boot intExtImpl ()
          (s, lg) = stepLog intExtImpl m0 (EvExternal (mkEvent_ @"INT"))
      lg `shouldBe` ["exitA", "enterB"]
      config (sMachine s) `shouldBe` Set.fromList ["p", "b"]

    it "the equivalent external To exits and re-enters the compound" $ do
      let m0 = boot intExtImpl ()
          (s, lg) = stepLog intExtImpl m0 (EvExternal (mkEvent_ @"EXT"))
      lg `shouldBe` ["exitA", "exitP", "enterP", "enterB"]
      config (sMachine s) `shouldBe` Set.fromList ["p", "b"]
