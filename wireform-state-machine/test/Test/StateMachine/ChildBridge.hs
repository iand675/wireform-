{-# LANGUAGE TemplateHaskell #-}

-- | The typed child-chart bridge ('mkServiceChart' \/ 'ChildBridge') under
-- the real IO interpreter. Three contracts, all with typed payloads and no
-- JSON:
--
--   (a) a parent invokes a child actor; the child completes and the parent
--       recovers the child's typed 'Output' at that exact type;
--   (b) a parent @sendChild@ is translated by @bridgeToChild@ into a child
--       event that advances the child;
--   (c) a child @sendParent@ is translated by @bridgeToParent@ into a parent
--       event that advances the parent.
--
-- Each scenario is arranged so the parent reaches a top-level final iff the
-- bridge did its job, so the observable is the parent's typed output. Every
-- wait is bounded by a timeout: a bridge that drops or mistranslates an
-- event fails the test loudly instead of hanging.
module Test.StateMachine.ChildBridge (tests) where

import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Test.Syd

import StateMachine hiding (context)

{-------------------------------------------------------------------------------
  A child's typed, non-JSON output
-------------------------------------------------------------------------------}

-- | The child's 'Output' type — deliberately a bespoke Haskell value, so a
-- passing test proves the output crossed the invoke boundary as a real
-- typed value rather than through a serialized proxy.
newtype ChildOut = ChildOut Int
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  (a) child completes on its own; parent recovers its typed Output
-------------------------------------------------------------------------------}

data ACState = ACStart | ACFin
deriveKeyKind ''ACState

data PAState = PAWait | PADone
data PAAction = GrabA
data PAService = RunChild
data PAInvoke = KidA
deriveKeyKind ''PAState
deriveKeyKind ''PAAction
deriveKeyKind ''PAService
deriveKeyKind ''PAInvoke

-- | Runs to its top-level final on the initial macrostep (the eventless
-- transition fires on entry), producing @ChildOut 9@.
type AutoChild :: ChartSpec ACState NoKey NoKey NoKey NoKey NoKey NoKey
type AutoChild =
  Chart
    "autochild"
    ()
    ChildOut
    '[]
    '[ State 'ACStart '[Always ==> To 'ACFin]
     , Final 'ACFin
     ]
    'ACStart

autoChildImpl :: ChartImpl IO AutoChild
autoChildImpl = chartImpl RNil RNil RNil RNil (const (ChildOut 9))

type ParentA :: ChartSpec PAState NoKey NoKey PAAction PAService PAInvoke NoKey
type ParentA =
  Chart
    "parenta"
    (Maybe Int)
    Int
    '[]
    '[ State
        'PAWait
        '[Invoke 'KidA 'RunChild '[OnDone ==> To 'PADone ! '[ 'GrabA]] '[]]
     , Final 'PADone
     ]
    'PAWait

parentAImpl :: ChartImpl IO ParentA
parentAImpl =
  chartImpl
    RNil
    (assign @'GrabA (\_ ev -> fmap (\(ChildOut n) -> n) (invokeOutput @ChildOut ev)) :& RNil)
    (mkServiceChart @'RunChild autoChildImpl autoBridge :& RNil)
    RNil
    (fromMaybe 0)

-- | The child needs nothing from the parent and vice versa, so both
-- translations drop everything; only 'bridgeCtx' is exercised.
autoBridge :: ChildBridge ParentA AutoChild
autoBridge =
  ChildBridge
    { bridgeCtx = \_ _ -> ()
    , bridgeToChild = const Nothing
    , bridgeToParent = const Nothing
    }

{-------------------------------------------------------------------------------
  (b) parent sendChild -> bridgeToChild -> child advances
-------------------------------------------------------------------------------}

data WCState = WCIdle | WCFin
data WCEvent = GO
deriveKeyKind ''WCState
deriveKeyKind ''WCEvent

data PBState = PBWait | PBDone
data PBEvent = PING
data PBAction = GrabB | PokeChild
data PBService = RunWait
data PBInvoke = KidB
deriveKeyKind ''PBState
deriveKeyKind ''PBEvent
deriveKeyKind ''PBAction
deriveKeyKind ''PBService
deriveKeyKind ''PBInvoke

-- | Sits idle until it receives @GO@, then completes with @ChildOut 42@.
type WaitChild :: ChartSpec WCState WCEvent NoKey NoKey NoKey NoKey NoKey
type WaitChild =
  Chart
    "waitchild"
    ()
    ChildOut
    '[ 'GO ::: ()]
    '[ State 'WCIdle '[On 'GO ==> To 'WCFin]
     , Final 'WCFin
     ]
    'WCIdle

waitChildImpl :: ChartImpl IO WaitChild
waitChildImpl = chartImpl RNil RNil RNil RNil (const (ChildOut 42))

type ParentB :: ChartSpec PBState PBEvent NoKey PBAction PBService PBInvoke NoKey
type ParentB =
  Chart
    "parentb"
    (Maybe Int)
    Int
    '[ 'PING ::: ()]
    '[ State
        'PBWait
        '[ Invoke 'KidB 'RunWait '[OnDone ==> To 'PBDone ! '[ 'GrabB]] '[]
         , On 'PING ==> Stay ! '[ 'PokeChild]
         ]
     , Final 'PBDone
     ]
    'PBWait

parentBImpl :: ChartImpl IO ParentB
parentBImpl =
  chartImpl
    RNil
    ( assign @'GrabB (\_ ev -> fmap (\(ChildOut n) -> n) (invokeOutput @ChildOut ev))
        :& mkAction @'PokeChild (\ctx _ -> pure (outcome ctx){aoSends = [sendChild "KidB" (mkEvent_ @'PING)]})
        :& RNil
    )
    (mkServiceChart @'RunWait waitChildImpl waitBridge :& RNil)
    RNil
    (fromMaybe 0)

-- | A parent @PING@ becomes a child @GO@; nothing else crosses.
waitBridge :: ChildBridge ParentB WaitChild
waitBridge =
  ChildBridge
    { bridgeCtx = \_ _ -> ()
    , bridgeToChild = \pev -> mkEvent_ @'GO <$ matchEvent @'PING pev
    , bridgeToParent = const Nothing
    }

{-------------------------------------------------------------------------------
  (c) child sendParent -> bridgeToParent -> parent advances
-------------------------------------------------------------------------------}

data RCState = RCStart
data RCEvent = REPORT
data RCAction = TellParent
deriveKeyKind ''RCState
deriveKeyKind ''RCEvent
deriveKeyKind ''RCAction

data PCState = PCWait | PCDone
data PCEvent = GOTREPORT
data PCAction = Mark
data PCService = RunReport
data PCInvoke = KidC
deriveKeyKind ''PCState
deriveKeyKind ''PCEvent
deriveKeyKind ''PCAction
deriveKeyKind ''PCService
deriveKeyKind ''PCInvoke

-- | On entry it reports to its parent (a @REPORT@ child event) and then
-- rests; it never finishes on its own — the parent finishing cancels it.
type ReportChild :: ChartSpec RCState RCEvent NoKey RCAction NoKey NoKey NoKey
type ReportChild =
  Chart
    "reportchild"
    ()
    ChildOut
    '[ 'REPORT ::: ()]
    '[State 'RCStart '[Entry '[ 'TellParent]]]
    'RCStart

reportChildImpl :: ChartImpl IO ReportChild
reportChildImpl =
  chartImpl
    RNil
    (mkAction @'TellParent (\ctx _ -> pure (outcome ctx){aoSends = [sendParent (mkEvent_ @'REPORT)]}) :& RNil)
    RNil
    RNil
    (const (ChildOut 0))

type ParentC :: ChartSpec PCState PCEvent NoKey PCAction PCService PCInvoke NoKey
type ParentC =
  Chart
    "parentc"
    Int
    Int
    '[ 'GOTREPORT ::: ()]
    '[ State
        'PCWait
        '[ Invoke 'KidC 'RunReport '[] '[]
         , On 'GOTREPORT ==> To 'PCDone ! '[ 'Mark]
         ]
     , Final 'PCDone
     ]
    'PCWait

parentCImpl :: ChartImpl IO ParentC
parentCImpl =
  chartImpl
    RNil
    (assign @'Mark (\_ _ -> 7) :& RNil)
    (mkServiceChart @'RunReport reportChildImpl reportBridge :& RNil)
    RNil
    id

-- | A child @REPORT@ becomes a parent @GOTREPORT@; nothing crosses downward.
reportBridge :: ChildBridge ParentC ReportChild
reportBridge =
  ChildBridge
    { bridgeCtx = \_ _ -> ()
    , bridgeToChild = const Nothing
    , bridgeToParent = \cev -> mkEvent_ @'GOTREPORT <$ matchEvent @'REPORT cev
    }

{-------------------------------------------------------------------------------
  Drivers
-------------------------------------------------------------------------------}

-- | Milliseconds a machine is given to finish before the test fails loudly.
budgetMicros :: Int
budgetMicros = 5_000_000

-- | Start a parent machine, failing the test if its initial step faults.
startParent :: (EventCodec spec, HasCallStack) => ChartImpl IO spec -> Ctx spec -> IO (Interpreter spec)
startParent impl ctx =
  interpret impl ctx
    >>= either (\f -> expectationFailure ("interpreter failed to start: " <> show f)) pure

-- | Wait (bounded) for the machine to reach a terminal state, then halt it.
-- A timeout is a loud failure, never a hang.
awaitOutput :: (HasCallStack) => Interpreter spec -> IO (Either StepFault (Output spec))
awaitOutput itp = do
  r <- timeout budgetMicros (waitFinished itp)
  halt itp
  maybe (expectationFailure "machine did not finish within the time budget") pure r

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "typed child-chart bridge" $ do
  it "recovers the child's typed Output at its exact type across the invoke boundary" $ do
    itp <- startParent parentAImpl Nothing
    out <- awaitOutput itp
    out `shouldBe` Right 9

  it "translates a parent sendChild through bridgeToChild, advancing the child to completion" $ do
    itp <- startParent parentBImpl Nothing
    accepted <- send itp (mkEvent_ @'PING)
    accepted `shouldBe` True
    out <- awaitOutput itp
    out `shouldBe` Right 42

  it "translates a child sendParent through bridgeToParent, advancing the parent" $ do
    itp <- startParent parentCImpl 0
    out <- awaitOutput itp
    out `shouldBe` Right 7
