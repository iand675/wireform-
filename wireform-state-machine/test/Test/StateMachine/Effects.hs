{-# LANGUAGE TemplateHaskell #-}

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

data TimerState = W | T1 | T2 | Idle
data TimerEvent = GO

deriveKeyKind ''TimerState
deriveKeyKind ''TimerEvent

type TimerChart :: ChartSpec TimerState TimerEvent NoKey NoKey NoKey NoKey NoKey
type TimerChart =
  Chart
    "timer"
    ()
    ()
    '[ 'GO ::: ()]
    '[ State
        'W
        '[ After 100 ==> To 'T1
         , After 200 ==> To 'T2
         , On 'GO ==> To 'Idle
         ]
     , State 'T1 '[]
     , State 'T2 '[]
     , State 'Idle '[]
     ]
    'W

timerImpl :: ChartImpl LogM TimerChart
timerImpl = chartImpl RNil RNil RNil RNil (const ())

-- | The two delayed transitions of @W@, by document index.
key100, key200 :: TimerKey
key100 = TimerKey "W" 100 0
key200 = TimerKey "W" 200 1

{-------------------------------------------------------------------------------
  Invokes
-------------------------------------------------------------------------------}

-- Constructors share the module's namespace, so this chart's @GO@ and
-- @Idle@ are spelled @InvGO@ / @InvIdle@ to keep them apart from
-- 'TimerChart''s.
data InvState = Load | Ok | Err | InvIdle
data InvEvent = InvGO
data InvAction = SaveOut | SaveErr
data InvService = Fetch
data InvId = Call1

deriveKeyKind ''InvState
deriveKeyKind ''InvEvent
deriveKeyKind ''InvAction
deriveKeyKind ''InvService
deriveKeyKind ''InvId

type InvChart :: ChartSpec InvState InvEvent NoKey InvAction InvService InvId NoKey
type InvChart =
  Chart
    "inv"
    (Maybe Int, Maybe Text)
    ()
    '[ 'InvGO ::: ()]
    '[ State
        'Load
        '[ Invoke
            'Call1
            'Fetch
            '[OnDone ==> To 'Ok ! '[ 'SaveOut]]
            '[OnError ==> To 'Err ! '[ 'SaveErr]]
         , On 'InvGO ==> To 'InvIdle
         ]
     , State 'Ok '[]
     , State 'Err '[]
     , State 'InvIdle '[]
     ]
    'Load

fetchSvc :: ServiceE LogM InvChart 'Fetch
fetchSvc = mkService @'Fetch (\_ _ -> pure (Right () :: Either () ()))

invImpl :: ChartImpl LogM InvChart
invImpl =
  chartImpl
    RNil
    ( assign @'SaveOut (\c ev -> (invokeOutput @Int ev, snd c))
        :& assign @'SaveErr (\c ev -> (fst c, invokeError @Text ev))
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
newtype User = User {uid :: Int}
  deriving stock (Eq, Show)

-- @LoadUser@ / @UserGO@: disambiguated from 'InvChart''s @Load@ and @InvGO@.
data UserState = LoadUser | Ready
data UserEvent = UserGO
data UserAction = SaveUser
data UserService = FetchUser
data UserInvoke = GetUser

deriveKeyKind ''UserState
deriveKeyKind ''UserEvent
deriveKeyKind ''UserAction
deriveKeyKind ''UserService
deriveKeyKind ''UserInvoke

type UserChart :: ChartSpec UserState UserEvent NoKey UserAction UserService UserInvoke NoKey
type UserChart =
  Chart
    "userinv"
    (Maybe User)
    ()
    '[ 'UserGO ::: ()]
    '[ State
        'LoadUser
        '[Invoke 'GetUser 'FetchUser '[OnDone ==> To 'Ready ! '[ 'SaveUser]] '[]]
     , State 'Ready '[]
     ]
    'LoadUser

userImpl :: ChartImpl LogM UserChart
userImpl =
  chartImpl
    RNil
    (assign @'SaveUser (\_ ev -> invokeOutput @User ev) :& RNil)
    (mkService @'FetchUser (\_ _ -> pure (Right (User 7) :: Either () User)) :& RNil)
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
      let s = stepOnce timerImpl (boot timerImpl ()) (EvExternal (mkEvent_ @'GO))
      sEffects s `shouldBe` [ReqCancelTimer key100, ReqCancelTimer key200]
      config (sMachine s) `shouldBe` Set.fromList ["Idle"]

    it "the exact timer key fires its transition (and exiting cancels the sibling timer)" $ do
      let s = stepOnce timerImpl (boot timerImpl ()) (EvTimer key100)
      config (sMachine s) `shouldBe` Set.fromList ["T1"]
      sEffects s `shouldBe` [ReqCancelTimer key100, ReqCancelTimer key200]

    it "a timer key with the wrong index is dropped" $ do
      let m0 = boot timerImpl ()
      shouldDrop m0 (stepOnce timerImpl m0 (EvTimer (TimerKey "W" 100 1)))

    it "a timer key with the wrong delay is dropped" $ do
      let m0 = boot timerImpl ()
      shouldDrop m0 (stepOnce timerImpl m0 (EvTimer (TimerKey "W" 150 0)))

    it "a timer key for another node is dropped" $ do
      let m0 = boot timerImpl ()
      shouldDrop m0 (stepOnce timerImpl m0 (EvTimer (TimerKey "T1" 100 0)))

  describe "invokes" $ do
    it "entry starts the invocation with its declared id, service, and owning node" $ do
      let s = bootStepped invImpl (Nothing, Nothing)
      sEffects s `shouldBe` [ReqStartInvoke "Call1" "Fetch" "Load"]

    it "exit cancels the invocation" $ do
      let s = stepOnce invImpl (boot invImpl (Nothing, Nothing)) (EvExternal (mkEvent_ @'InvGO))
      sEffects s `shouldBe` [ReqCancelInvoke "Call1"]

    it "EvInvokeDone takes the onDone transition with the payload observable via invokeOutput" $ do
      let m = advance invImpl (boot invImpl (Nothing, Nothing)) (EvInvokeDone "Call1" (toDyn (5 :: Int)))
      config m `shouldBe` Set.fromList ["Ok"]
      ctxOf m `shouldBe` (Just 5, Nothing)

    it "EvInvokeError takes the onError transition with the payload observable via invokeError" $ do
      let m = advance invImpl (boot invImpl (Nothing, Nothing)) (EvInvokeError "Call1" (toDyn ("boom" :: Text)))
      config m `shouldBe` Set.fromList ["Err"]
      ctxOf m `shouldBe` (Nothing, Just "boom")

    it "an EvInvokeDone for an invocation whose state is not active is dropped" $ do
      let m0 = advance invImpl (boot invImpl (Nothing, Nothing)) (EvExternal (mkEvent_ @'InvGO))
      shouldDrop m0 (stepOnce invImpl m0 (EvInvokeDone "Call1" (toDyn (5 :: Int))))

    it "recovers a resolved invocation's output at its real, non-JSON Haskell type" $ do
      let (r, _) = runLog (simulate userImpl Nothing [simResolve "GetUser" (User 7)])
      case r of
        Left f -> expectationFailure ("simulation failed: " <> show f)
        Right res -> do
          config (simMachine res) `shouldBe` Set.fromList ["Ready"]
          ctxOf (simMachine res) `shouldBe` Just (User 7)
