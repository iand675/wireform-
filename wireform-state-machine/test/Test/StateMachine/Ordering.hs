{-# LANGUAGE TemplateHaskell #-}

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

data OrderState = P | A | B | Q
data OrderEvent = CROSS | SELF | HOLD
data OrderAction
  = EnterA
  | ExitA
  | During
  | SelfAct
  | HoldAct
  | EnterP
  | ExitP
  | EnterB
  | EnterQ

deriveKeyKind ''OrderState
deriveKeyKind ''OrderEvent
deriveKeyKind ''OrderAction

type OrderChart :: ChartSpec OrderState OrderEvent NoKey OrderAction NoKey NoKey NoKey
type OrderChart =
  Chart
    "order"
    ()
    ()
    '[ 'CROSS ::: ()
     , 'SELF ::: ()
     , 'HOLD ::: ()
     ]
    '[ Compound
        'P
        'A
        '[ State
            'A
            '[ Entry '[ 'EnterA]
             , Exit '[ 'ExitA]
             , On 'CROSS ==> To 'B ! '[ 'During]
             , On 'SELF ==> To 'A ! '[ 'SelfAct]
             , On 'HOLD ==> Stay ! '[ 'HoldAct]
             ]
         ]
        '[Entry '[ 'EnterP], Exit '[ 'ExitP]]
     , Compound
        'Q
        'B
        '[State 'B '[Entry '[ 'EnterB]]]
        '[Entry '[ 'EnterQ]]
     ]
    'P

orderImpl :: ChartImpl LogM OrderChart
orderImpl =
  chartImpl
    RNil
    ( logAct @'EnterA
        :& logAct @'ExitA
        :& logAct @'During
        :& logAct @'SelfAct
        :& logAct @'HoldAct
        :& logAct @'EnterP
        :& logAct @'ExitP
        :& logAct @'EnterB
        :& logAct @'EnterQ
        :& RNil
    )
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  LCCA scoping
-------------------------------------------------------------------------------}

data LccaState = LP | LA | LB | LC
data LccaEvent = SIB | OUT
data LccaAction = LExitA | LEnterB | LEnterP | LExitP | LEnterC

deriveKeyKind ''LccaState
deriveKeyKind ''LccaEvent
deriveKeyKind ''LccaAction

type LccaChart :: ChartSpec LccaState LccaEvent NoKey LccaAction NoKey NoKey NoKey
type LccaChart =
  Chart
    "lcca"
    ()
    ()
    '[ 'SIB ::: ()
     , 'OUT ::: ()
     ]
    '[ Compound
        'LP
        'LA
        '[ State 'LA '[Exit '[ 'LExitA], On 'SIB ==> To 'LB, On 'OUT ==> To 'LC]
         , State 'LB '[Entry '[ 'LEnterB]]
         ]
        '[Entry '[ 'LEnterP], Exit '[ 'LExitP]]
     , State 'LC '[Entry '[ 'LEnterC]]
     ]
    'LP

lccaImpl :: ChartImpl LogM LccaChart
lccaImpl =
  chartImpl
    RNil
    ( logAct @'LExitA
        :& logAct @'LEnterB
        :& logAct @'LEnterP
        :& logAct @'LExitP
        :& logAct @'LEnterC
        :& RNil
    )
    RNil
    RNil
    (const ())

{-------------------------------------------------------------------------------
  Internal vs external
-------------------------------------------------------------------------------}

-- IXB rather than IB: deriveKeyKind's singleton for IB would be SIB,
-- colliding with the LccaEvent constructor above.
data IntExtState = IP | IA | IXB
data IntExtEvent = INT | EXT
data IntExtAction = IExitA | IEnterB | IEnterP | IExitP

deriveKeyKind ''IntExtState
deriveKeyKind ''IntExtEvent
deriveKeyKind ''IntExtAction

type IntExtChart :: ChartSpec IntExtState IntExtEvent NoKey IntExtAction NoKey NoKey NoKey
type IntExtChart =
  Chart
    "intext"
    ()
    ()
    '[ 'INT ::: ()
     , 'EXT ::: ()
     ]
    '[ Compound
        'IP
        'IA
        '[ State 'IA '[Exit '[ 'IExitA]]
         , State 'IXB '[Entry '[ 'IEnterB]]
         ]
        '[ Entry '[ 'IEnterP]
         , Exit '[ 'IExitP]
         , On 'INT ==> Inside 'IXB
         , On 'EXT ==> To 'IXB
         ]
     ]
    'IP

intExtImpl :: ChartImpl LogM IntExtChart
intExtImpl =
  chartImpl
    RNil
    ( logAct @'IExitA
        :& logAct @'IEnterB
        :& logAct @'IEnterP
        :& logAct @'IExitP
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
    lg `shouldBe` ["EnterP", "EnterA"]

  it "external transition between compounds: exits child-first, actions, enters parent-first" $ do
    let m0 = boot orderImpl ()
        (s, lg) = stepLog orderImpl m0 (EvExternal (mkEvent_ @'CROSS))
    lg `shouldBe` ["ExitA", "ExitP", "During", "EnterQ", "EnterB"]
    config (sMachine s) `shouldBe` Set.fromList ["Q", "B"]

  it "external self-transition re-runs exit and entry of the source, not its parent" $ do
    let m0 = boot orderImpl ()
        (s, lg) = stepLog orderImpl m0 (EvExternal (mkEvent_ @'SELF))
    lg `shouldBe` ["ExitA", "SelfAct", "EnterA"]
    config (sMachine s) `shouldBe` Set.fromList ["P", "A"]

  it "targetless Stay transition runs only its actions" $ do
    let m0 = boot orderImpl ()
        (s, lg) = stepLog orderImpl m0 (EvExternal (mkEvent_ @'HOLD))
    lg `shouldBe` ["HoldAct"]
    config (sMachine s) `shouldBe` Set.fromList ["P", "A"]
    case sTrace s of
      [mt] -> do
        mtExited mt `shouldBe` []
        mtEntered mt `shouldBe` []
      other -> expectationFailure ("expected one microstep, got " <> show (length other))

  describe "LCCA scoping" $ do
    it "sibling transition inside a compound does not exit the compound" $ do
      let m0 = boot lccaImpl ()
          (s, lg) = stepLog lccaImpl m0 (EvExternal (mkEvent_ @'SIB))
      lg `shouldBe` ["LExitA", "LEnterB"]
      config (sMachine s) `shouldBe` Set.fromList ["LP", "LB"]

    it "transition crossing the compound boundary exits the compound" $ do
      let m0 = boot lccaImpl ()
          (s, lg) = stepLog lccaImpl m0 (EvExternal (mkEvent_ @'OUT))
      lg `shouldBe` ["LExitA", "LExitP", "LEnterC"]
      config (sMachine s) `shouldBe` Set.fromList ["LC"]

  describe "internal vs external" $ do
    it "Inside on a compound enters the child without exiting the compound" $ do
      let m0 = boot intExtImpl ()
          (s, lg) = stepLog intExtImpl m0 (EvExternal (mkEvent_ @'INT))
      lg `shouldBe` ["IExitA", "IEnterB"]
      config (sMachine s) `shouldBe` Set.fromList ["IP", "IXB"]

    it "the equivalent external To exits and re-enters the compound" $ do
      let m0 = boot intExtImpl ()
          (s, lg) = stepLog intExtImpl m0 (EvExternal (mkEvent_ @'EXT))
      lg `shouldBe` ["IExitA", "IExitP", "IEnterP", "IEnterB"]
      config (sMachine s) `shouldBe` Set.fromList ["IP", "IXB"]
