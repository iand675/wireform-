-- | Timers and invocations as pure effect requests: arming on entry,
-- cancelling on exit, exact-key timer matching, and invoke lifecycle
-- events with observable payloads.
module Test.StateMachine.Effects (tests) where

import Data.Dynamic (toDyn)
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Syd

import StateMachine hiding (context)
import StateMachine.Runtime (EffectReq (..), TimerKey (..))

import Test.StateMachine.Support

{-------------------------------------------------------------------------------
  Timers
-------------------------------------------------------------------------------}

type TimerChart =
  Chart
    "timer"
    ()
    ()
    '["GO" ::: ()]
    '[ State
        "w"
        '[ After 100 ==> To "t1"
         , After 200 ==> To "t2"
         , On "GO" ==> To "idle"
         ]
     , State "t1" '[]
     , State "t2" '[]
     , State "idle" '[]
     ]
    "w"

timerImpl :: ChartImpl LogM TimerChart
timerImpl = chartImpl RNil RNil RNil RNil (const ())

-- | The two delayed transitions of @w@, by document index.
key100, key200 :: TimerKey
key100 = TimerKey "w" 100 0
key200 = TimerKey "w" 200 1

{-------------------------------------------------------------------------------
  Invokes
-------------------------------------------------------------------------------}

type InvChart =
  Chart
    "inv"
    (Maybe Int, Maybe Text)
    ()
    '["GO" ::: ()]
    '[ State
        "load"
        '[ Invoke
            "call1"
            "fetch"
            '[OnDone ==> To "ok" ! '["saveOut"]]
            '[OnError ==> To "err" ! '["saveErr"]]
         , On "GO" ==> To "idle"
         ]
     , State "ok" '[]
     , State "err" '[]
     , State "idle" '[]
     ]
    "load"

fetchSvc :: ServiceE LogM InvChart "fetch"
fetchSvc = mkService @"fetch" (\_ _ -> pure (Right () :: Either () ()))

invImpl :: ChartImpl LogM InvChart
invImpl =
  chartImpl
    RNil
    ( assign @"saveOut" (\c ev -> (invokeOutput @Int ev, snd c))
        :& assign @"saveErr" (\c ev -> (fst c, invokeError @Text ev))
        :& RNil
    )
    (fetchSvc :& RNil)
    RNil
    (const ())

{-------------------------------------------------------------------------------
  A custom (non-JSON) invoke output
-------------------------------------------------------------------------------}

-- | A payload type that is deliberately not JSON-shaped: its identity
-- survives the invoke queue only because results travel as real typed
-- values, recovered by the consumer's expected type.
data User = User {uid :: Int}
  deriving stock (Eq, Show)

type UserChart =
  Chart
    "userinv"
    (Maybe User)
    ()
    '["GO" ::: ()]
    '[ State
        "load"
        '[Invoke "getUser" "fetchUser" '[OnDone ==> To "ready" ! '["saveUser"]] '[]]
     , State "ready" '[]
     ]
    "load"

userImpl :: ChartImpl LogM UserChart
userImpl =
  chartImpl
    RNil
    (assign @"saveUser" (\_ ev -> invokeOutput @User ev) :& RNil)
    (mkService @"fetchUser" (\_ _ -> pure (Right (User 7) :: Either () User)) :& RNil)
    RNil
    (const ())

-- | A step whose event was dropped: nothing selected, machine unchanged.
shouldDrop :: Machine spec -> Stepped spec -> IO ()
shouldDrop prior s = do
  config (sMachine s) `shouldBe` config prior
  case sTrace s of
    [mt] -> mtSelected mt `shouldBe` []
    other -> expectationFailure ("expected one microstep, got " <> show (length other))

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "effects" $ do
  describe "timers" $ do
    it "entering a state with After transitions arms one timer per delay, keyed by document index" $ do
      let s = bootStepped timerImpl ()
      sEffects s `shouldBe` [ReqStartTimer key100, ReqStartTimer key200]

    it "exiting the state cancels every armed timer" $ do
      let s = stepOnce timerImpl (boot timerImpl ()) (EvExternal (mkEvent_ @"GO"))
      sEffects s `shouldBe` [ReqCancelTimer key100, ReqCancelTimer key200]
      config (sMachine s) `shouldBe` Set.fromList ["idle"]

    it "the exact timer key fires its transition (and exiting cancels the sibling timer)" $ do
      let s = stepOnce timerImpl (boot timerImpl ()) (EvTimer key100)
      config (sMachine s) `shouldBe` Set.fromList ["t1"]
      sEffects s `shouldBe` [ReqCancelTimer key100, ReqCancelTimer key200]

    it "a timer key with the wrong index is dropped" $ do
      let m0 = boot timerImpl ()
      shouldDrop m0 (stepOnce timerImpl m0 (EvTimer (TimerKey "w" 100 1)))

    it "a timer key with the wrong delay is dropped" $ do
      let m0 = boot timerImpl ()
      shouldDrop m0 (stepOnce timerImpl m0 (EvTimer (TimerKey "w" 150 0)))

    it "a timer key for another node is dropped" $ do
      let m0 = boot timerImpl ()
      shouldDrop m0 (stepOnce timerImpl m0 (EvTimer (TimerKey "t1" 100 0)))

  describe "invokes" $ do
    it "entry starts the invocation with its declared id, service, and owning node" $ do
      let s = bootStepped invImpl (Nothing, Nothing)
      sEffects s `shouldBe` [ReqStartInvoke "call1" "fetch" "load"]

    it "exit cancels the invocation" $ do
      let s = stepOnce invImpl (boot invImpl (Nothing, Nothing)) (EvExternal (mkEvent_ @"GO"))
      sEffects s `shouldBe` [ReqCancelInvoke "call1"]

    it "EvInvokeDone takes the onDone transition with the payload observable via invokeOutput" $ do
      let m = advance invImpl (boot invImpl (Nothing, Nothing)) (EvInvokeDone "call1" (toDyn (5 :: Int)))
      config m `shouldBe` Set.fromList ["ok"]
      ctxOf m `shouldBe` (Just 5, Nothing)

    it "EvInvokeError takes the onError transition with the payload observable via invokeError" $ do
      let m = advance invImpl (boot invImpl (Nothing, Nothing)) (EvInvokeError "call1" (toDyn ("boom" :: Text)))
      config m `shouldBe` Set.fromList ["err"]
      ctxOf m `shouldBe` (Nothing, Just "boom")

    it "an EvInvokeDone for an invocation whose state is not active is dropped" $ do
      let m0 = advance invImpl (boot invImpl (Nothing, Nothing)) (EvExternal (mkEvent_ @"GO"))
      shouldDrop m0 (stepOnce invImpl m0 (EvInvokeDone "call1" (toDyn (5 :: Int))))

    it "recovers a resolved invocation's output at its real, non-JSON Haskell type" $ do
      let (r, _) = runLog (simulate userImpl Nothing [simResolve "getUser" (User 7)])
      case r of
        Left f -> expectationFailure ("simulation failed: " <> show f)
        Right res -> do
          config (simMachine res) `shouldBe` Set.fromList ["ready"]
          ctxOf (simMachine res) `shouldBe` Just (User 7)
