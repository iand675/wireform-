{-# LANGUAGE TemplateHaskell #-}

-- | Completion (done) semantics: done events raised within the same
-- macrostep, done-data payloads, nested parallel completion, and root
-- completion with typed output plus cancellation of everything armed.
module Test.StateMachine.Completion (tests) where

import Data.Set qualified as Set
import Test.Syd

import StateMachine hiding (context)
import StateMachine.Runtime (EffectReq (..), TimerKey (..))

import Test.StateMachine.Support

{-------------------------------------------------------------------------------
  Done data and root completion
-------------------------------------------------------------------------------}

data DState = C | Work | F | Next | Fin
data DEvent = FIN | END
data DAction = ReadDone
data DService = Svc
data DInvoke = RootSvc
data DOutput = Prod
deriveKeyKind ''DState
deriveKeyKind ''DEvent
deriveKeyKind ''DAction
deriveKeyKind ''DService
deriveKeyKind ''DInvoke
deriveKeyKind ''DOutput

{- | Context stores the last observed done-data payload; the machine
output distinguishes whether one was ever seen. @Next@ keeps a timer
armed and the root keeps an invocation alive so finishing has something
to cancel.
-}
type DoneChart :: ChartSpec DState DEvent NoKey DAction DService DInvoke DOutput
type DoneChart =
  ChartWith
    "done"
    (Maybe Int)
    Int
    '[ 'FIN ::: ()
     , 'END ::: ()
     ]
    '[ Compound
        'C
        'Work
        '[ State 'Work '[On 'FIN ==> To 'F]
         , FinalWith 'F 'Prod
         ]
        '[OnDoneOf 'C ==> To 'Next ! '[ 'ReadDone]]
     , State 'Next '[After 300 ==> To 'Next, On 'END ==> To 'Fin]
     , Final 'Fin
     ]
    'C
    '[Invoke 'RootSvc 'Svc '[] '[]]

svcImpl :: ServiceE LogM DoneChart 'Svc
svcImpl = mkService @'Svc (\_ _ -> pure (Right () :: Either () ()))

doneImpl :: ChartImpl LogM DoneChart
doneImpl =
  chartImpl
    RNil
    (assign @'ReadDone (\_ ev -> doneData @Int ev) :& RNil)
    (svcImpl :& RNil)
    (mkOutput @'Prod (\_ _ -> (42 :: Int)) :& RNil)
    (maybe 0 (const 7))

{-------------------------------------------------------------------------------
  Nested completion through a parallel state
-------------------------------------------------------------------------------}

data NState = Big | Ra | Na | Wa | Fna | Fa | Rb | Wb | Fb | AfterBig
data NEvent = FA | FB
deriveKeyKind ''NState
deriveKeyKind ''NEvent

{- | Region @Ra@ holds a nested compound @Na@; finishing @Na@ cascades to
@Ra@'s final, and once @Rb@ is also final the parallel itself completes —
all within the macrostep of the final external event.
-}
type NestChart :: ChartSpec NState NEvent NoKey NoKey NoKey NoKey NoKey
type NestChart =
  Chart
    "nest"
    ()
    ()
    '[ 'FA ::: ()
     , 'FB ::: ()
     ]
    '[ Parallel
        'Big
        '[ Compound
            'Ra
            'Na
            '[ Compound
                'Na
                'Wa
                '[ State 'Wa '[On 'FA ==> To 'Fna]
                 , Final 'Fna
                 ]
                '[OnDoneOf 'Na ==> To 'Fa]
             , Final 'Fa
             ]
            '[]
         , Compound
            'Rb
            'Wb
            '[ State 'Wb '[On 'FB ==> To 'Fb]
             , Final 'Fb
             ]
            '[]
         ]
        '[OnDoneOf 'Big ==> To 'AfterBig]
     , State 'AfterBig '[]
     ]
    'Big

nestImpl :: ChartImpl LogM NestChart
nestImpl = chartImpl RNil RNil RNil RNil (const ())

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "completion (done) semantics" $ do
  it "entering a final child fires the parent's OnDoneOf within the same macrostep" $ do
    let m0 = boot doneImpl Nothing
        m = advance doneImpl m0 (EvExternal (mkEvent_ @'FIN))
    config m `shouldBe` Set.fromList ["Next"]

  it "the FinalWith output producer runs and its value is the done event's payload" $ do
    let m = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @'FIN))
    ctxOf m `shouldBe` Just (42 :: Int)

  it "nested completion cascades: inner done, region final, all regions final, parallel done — one macrostep" $ do
    let m1 = advance nestImpl (boot nestImpl ()) (EvExternal (mkEvent_ @'FB))
    -- Rb is final but Ra is not: the parallel is still running.
    config m1 `shouldBe` Set.fromList ["Big", "Ra", "Na", "Wa", "Rb", "Fb"]
    let m2 = advance nestImpl m1 (EvExternal (mkEvent_ @'FA))
    config m2 `shouldBe` Set.fromList ["AfterBig"]

  it "reaching a top-level final yields Finished with the output computed from the final context" $ do
    let m1 = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @'FIN))
        s = stepOnce doneImpl m1 (EvExternal (mkEvent_ @'END))
    case status (sMachine s) of
      Finished out -> out `shouldBe` 7
      Running -> expectationFailure "expected the machine to be Finished"

  it "the finishing step cancels every armed timer and invocation" $ do
    let m1 = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @'FIN))
        s = stepOnce doneImpl m1 (EvExternal (mkEvent_ @'END))
    sEffects s
      `shouldBe` [ ReqCancelTimer (TimerKey "Next" 300 0)
                 , ReqCancelInvoke "RootSvc"
                 ]

  it "stepping a Finished machine is a no-op with an empty trace" $ do
    let m1 = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @'FIN))
        m2 = advance doneImpl m1 (EvExternal (mkEvent_ @'END))
        s = stepOnce doneImpl m2 (EvExternal (mkEvent_ @'FIN))
    sTrace s `shouldBe` []
    sEffects s `shouldBe` []
    config (sMachine s) `shouldBe` config m2
    case status (sMachine s) of
      Finished out -> out `shouldBe` 7
      Running -> expectationFailure "a Finished machine must stay Finished"
