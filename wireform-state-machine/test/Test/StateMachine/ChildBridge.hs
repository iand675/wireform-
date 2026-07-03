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

-- | Runs to its top-level final on the initial macrostep (the eventless
-- transition fires on entry), producing @ChildOut 9@.
type AutoChild =
  Chart
    "autochild"
    ()
    ChildOut
    '[]
    '[ State "cstart" '[Always ==> To "cfin"]
     , Final "cfin"
     ]
    "cstart"

autoChildImpl :: ChartImpl IO AutoChild
autoChildImpl = chartImpl RNil RNil RNil RNil (const (ChildOut 9))

type ParentA =
  Chart
    "parenta"
    (Maybe Int)
    Int
    '[]
    '[ State
        "pwait"
        '[Invoke "kid" "runChild" '[OnDone ==> To "pdone" ! '["grab"]] '[]]
     , Final "pdone"
     ]
    "pwait"

parentAImpl :: ChartImpl IO ParentA
parentAImpl =
  chartImpl
    RNil
    (assign @"grab" (\_ ev -> fmap (\(ChildOut n) -> n) (invokeOutput @ChildOut ev)) :& RNil)
    (mkServiceChart @"runChild" autoChildImpl autoBridge :& RNil)
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

-- | Sits idle until it receives @GO@, then completes with @ChildOut 42@.
type WaitChild =
  Chart
    "waitchild"
    ()
    ChildOut
    '["GO" ::: ()]
    '[ State "cidle" '[On "GO" ==> To "cfin"]
     , Final "cfin"
     ]
    "cidle"

waitChildImpl :: ChartImpl IO WaitChild
waitChildImpl = chartImpl RNil RNil RNil RNil (const (ChildOut 42))

type ParentB =
  Chart
    "parentb"
    (Maybe Int)
    Int
    '["PING" ::: ()]
    '[ State
        "pwait"
        '[ Invoke "kid" "runWait" '[OnDone ==> To "pdone" ! '["grab"]] '[]
         , On "PING" ==> Stay ! '["pokeChild"]
         ]
     , Final "pdone"
     ]
    "pwait"

parentBImpl :: ChartImpl IO ParentB
parentBImpl =
  chartImpl
    RNil
    ( assign @"grab" (\_ ev -> fmap (\(ChildOut n) -> n) (invokeOutput @ChildOut ev))
        :& mkAction @"pokeChild" (\ctx _ -> pure (outcome ctx){aoSends = [sendChild "kid" (mkEvent_ @"PING")]})
        :& RNil
    )
    (mkServiceChart @"runWait" waitChildImpl waitBridge :& RNil)
    RNil
    (fromMaybe 0)

-- | A parent @PING@ becomes a child @GO@; nothing else crosses.
waitBridge :: ChildBridge ParentB WaitChild
waitBridge =
  ChildBridge
    { bridgeCtx = \_ _ -> ()
    , bridgeToChild = \pev -> case eventName pev of
        "PING" -> Just (mkEvent_ @"GO")
        _ -> Nothing
    , bridgeToParent = const Nothing
    }

{-------------------------------------------------------------------------------
  (c) child sendParent -> bridgeToParent -> parent advances
-------------------------------------------------------------------------------}

-- | On entry it reports to its parent (a @REPORT@ child event) and then
-- rests; it never finishes on its own — the parent finishing cancels it.
type ReportChild =
  Chart
    "reportchild"
    ()
    ChildOut
    '["REPORT" ::: ()]
    '[State "cstart" '[Entry '["tellParent"]]]
    "cstart"

reportChildImpl :: ChartImpl IO ReportChild
reportChildImpl =
  chartImpl
    RNil
    (mkAction @"tellParent" (\ctx _ -> pure (outcome ctx){aoSends = [sendParent (mkEvent_ @"REPORT")]}) :& RNil)
    RNil
    RNil
    (const (ChildOut 0))

type ParentC =
  Chart
    "parentc"
    Int
    Int
    '["GOTREPORT" ::: ()]
    '[ State
        "pwait"
        '[ Invoke "kid" "runReport" '[] '[]
         , On "GOTREPORT" ==> To "pdone" ! '["mark"]
         ]
     , Final "pdone"
     ]
    "pwait"

parentCImpl :: ChartImpl IO ParentC
parentCImpl =
  chartImpl
    RNil
    (assign @"mark" (\_ _ -> 7) :& RNil)
    (mkServiceChart @"runReport" reportChildImpl reportBridge :& RNil)
    RNil
    id

-- | A child @REPORT@ becomes a parent @GOTREPORT@; nothing crosses downward.
reportBridge :: ChildBridge ParentC ReportChild
reportBridge =
  ChildBridge
    { bridgeCtx = \_ _ -> ()
    , bridgeToChild = const Nothing
    , bridgeToParent = \cev -> case eventName cev of
        "REPORT" -> Just (mkEvent_ @"GOTREPORT")
        _ -> Nothing
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
    accepted <- send itp (mkEvent_ @"PING")
    accepted `shouldBe` True
    out <- awaitOutput itp
    out `shouldBe` Right 42

  it "translates a child sendParent through bridgeToParent, advancing the parent" $ do
    itp <- startParent parentCImpl 0
    out <- awaitOutput itp
    out `shouldBe` Right 7
