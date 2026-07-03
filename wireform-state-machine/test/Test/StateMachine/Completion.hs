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

{- | Context stores the last observed done-data payload; the machine
output distinguishes whether one was ever seen. @next@ keeps a timer
armed and the root keeps an invocation alive so finishing has something
to cancel.
-}
type DoneChart =
  ChartWith
    "done"
    (Maybe Int)
    Int
    '[ "FIN" ::: ()
     , "END" ::: ()
     ]
    '[ Compound
        "c"
        "work"
        '[ State "work" '[On "FIN" ==> To "f"]
         , FinalWith "f" "prod"
         ]
        '[OnDoneOf "c" ==> To "next" ! '["readDone"]]
     , State "next" '[After 300 ==> To "next", On "END" ==> To "fin"]
     , Final "fin"
     ]
    "c"
    '[Invoke "rootSvc" "svc" '[] '[]]

svcImpl :: ServiceE LogM DoneChart "svc"
svcImpl = mkService @"svc" (\_ _ -> pure (Right () :: Either () ()))

doneImpl :: ChartImpl LogM DoneChart
doneImpl =
  chartImpl
    RNil
    (assign @"readDone" (\_ ev -> doneData @Int ev) :& RNil)
    (svcImpl :& RNil)
    (mkOutput @"prod" (\_ _ -> (42 :: Int)) :& RNil)
    (maybe 0 (const 7))

{-------------------------------------------------------------------------------
  Nested completion through a parallel state
-------------------------------------------------------------------------------}

{- | Region @ra@ holds a nested compound @na@; finishing @na@ cascades to
@ra@'s final, and once @rb@ is also final the parallel itself completes —
all within the macrostep of the final external event.
-}
type NestChart =
  Chart
    "nest"
    ()
    ()
    '[ "FA" ::: ()
     , "FB" ::: ()
     ]
    '[ Parallel
        "big"
        '[ Compound
            "ra"
            "na"
            '[ Compound
                "na"
                "wa"
                '[ State "wa" '[On "FA" ==> To "fna"]
                 , Final "fna"
                 ]
                '[OnDoneOf "na" ==> To "fa"]
             , Final "fa"
             ]
            '[]
         , Compound
            "rb"
            "wb"
            '[ State "wb" '[On "FB" ==> To "fb"]
             , Final "fb"
             ]
            '[]
         ]
        '[OnDoneOf "big" ==> To "afterBig"]
     , State "afterBig" '[]
     ]
    "big"

nestImpl :: ChartImpl LogM NestChart
nestImpl = chartImpl RNil RNil RNil RNil (const ())

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "completion (done) semantics" $ do
  it "entering a final child fires the parent's OnDoneOf within the same macrostep" $ do
    let m0 = boot doneImpl Nothing
        m = advance doneImpl m0 (EvExternal (mkEvent_ @"FIN"))
    config m `shouldBe` Set.fromList ["next"]

  it "the FinalWith output producer runs and its value is the done event's payload" $ do
    let m = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @"FIN"))
    ctxOf m `shouldBe` Just (42 :: Int)

  it "nested completion cascades: inner done, region final, all regions final, parallel done — one macrostep" $ do
    let m1 = advance nestImpl (boot nestImpl ()) (EvExternal (mkEvent_ @"FB"))
    -- rb is final but ra is not: the parallel is still running.
    config m1 `shouldBe` Set.fromList ["big", "ra", "na", "wa", "rb", "fb"]
    let m2 = advance nestImpl m1 (EvExternal (mkEvent_ @"FA"))
    config m2 `shouldBe` Set.fromList ["afterBig"]

  it "reaching a top-level final yields Finished with the output computed from the final context" $ do
    let m1 = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @"FIN"))
        s = stepOnce doneImpl m1 (EvExternal (mkEvent_ @"END"))
    case status (sMachine s) of
      Finished out -> out `shouldBe` 7
      Running -> expectationFailure "expected the machine to be Finished"

  it "the finishing step cancels every armed timer and invocation" $ do
    let m1 = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @"FIN"))
        s = stepOnce doneImpl m1 (EvExternal (mkEvent_ @"END"))
    sEffects s
      `shouldBe` [ ReqCancelTimer (TimerKey "next" 300 0)
                 , ReqCancelInvoke "rootSvc"
                 ]

  it "stepping a Finished machine is a no-op with an empty trace" $ do
    let m1 = advance doneImpl (boot doneImpl Nothing) (EvExternal (mkEvent_ @"FIN"))
        m2 = advance doneImpl m1 (EvExternal (mkEvent_ @"END"))
        s = stepOnce doneImpl m2 (EvExternal (mkEvent_ @"FIN"))
    sTrace s `shouldBe` []
    sEffects s `shouldBe` []
    config (sMachine s) `shouldBe` config m2
    case status (sMachine s) of
      Finished out -> out `shouldBe` 7
      Running -> expectationFailure "a Finished machine must stay Finished"
