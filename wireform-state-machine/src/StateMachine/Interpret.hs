{- | The real-time interpreter: runs a statechart in 'IO'.

"StateMachine.Step" is pure — timers and invocations surface as
'EffectReq' /requests/ and cross-actor messages as 'SendReq's. This
module executes them: 'interpret' starts a machine, 'send' feeds it
events, 'waitFinished' blocks until it produces its typed output.

== Architecture

* __One driver thread per machine__ owns the 'Machine' value and consumes
  a queue of internal events. User guards and actions run on it (inside
  'StateMachine.Step.step'), effect requests are executed by it, and
  subscriber callbacks are invoked from it — nothing else touches the
  machine, so there are no locks and no torn steps. Timers, invoked
  services, and child actors run as separate 'Async's that only ever
  /enqueue/ back into the driver.

* __Generation counters defeat staleness.__ Every arm /and/ cancel of a
  timer (by 'TimerKey') or invocation (by invoke id) bumps a driver-local
  generation counter, and a firing timer or resolving invocation carries
  the generation it was armed under. The driver drops results whose
  generation is no longer current. This closes the race where a timer
  fires concurrently with its own cancellation: the stale timer event is
  already queued when its state is exited and re-entered, and — because
  the re-entered state arms the /same/ 'TimerKey' — transition selection
  alone would accept it, firing the fresh delay far too early. Only the
  generation tells old from new.

== Lifecycle and failure

* A machine that reaches a top-level final state processes that step's
  effects (they already cancel everything armed), notifies subscribers
  with the final 'NotifyStepped', resolves 'waitFinished' with its typed
  output, and then behaves as halted: 'send' returns 'False'.

* A 'StepFault' stores the fault (visible via 'waitFinished'), notifies
  'NotifyFault', cancels everything armed, and halts.

* An exception thrown by a user guard\/action inside a step is caught on
  the driver thread and surfaced exactly like a fault:
  @'NotifyFault' ('InternalFault' (show e))@, then halt. (Exceptions
  thrown while 'interpretWith' runs the /initial/ step propagate to its
  caller instead — no interpreter exists yet.)

* 'halt' is idempotent and synchronous: it stops the driver and cancels
  all timers, invocations, and child actors before returning. Events
  already accepted into the queue are still processed first.

== Sends

'ToSelf' sends re-enter this machine's external queue as a fresh
macrostep; 'ToChild' sends are decoded with the /child's/ codec and
routed to the live invocation of that id; 'ToParent' sends go to the
'optSendParent' hook. A send that cannot be delivered — no such live
child, no parent hook, or a payload the target chart cannot decode — is
dropped; subscribers can observe every requested send in the 'sSends' of
the corresponding 'NotifyStepped'.
-}
module StateMachine.Interpret (
  -- * Options
  InterpretOptions (..),
  defaultInterpretOptions,

  -- * Starting
  Interpreter,
  interpret,
  interpretWith,

  -- * Interacting
  send,
  sendNamed,
  machineView,
  waitFinished,
  halt,

  -- * Observing
  Notification (..),
  subscribe,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (
  TQueue,
  TVar,
  atomically,
  check,
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVar,
  readTVarIO,
  retry,
  writeTQueue,
  writeTVar,
 )
import Control.Exception (
  SomeAsyncException (..),
  SomeException,
  catch,
  finally,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (void, when)
import Data.Aeson (Value)
import Data.Dynamic (Dynamic, toDyn)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T

import StateMachine.Event (EventCodec, EventVal, StepEvent (..), decodeEvent)
import StateMachine.Machine (Machine (..), Status (..))
import StateMachine.Registry (ChartImpl (..), ChildBridge (..), SendReq (..), SendTarget (..), SomeService (..))
import StateMachine.Runtime (EffectReq (..), TimerKey (..))
import StateMachine.Spec (ChartSpec, Ctx, Output)
import StateMachine.Step (StepFault (..), Stepped (..), initialize, step)

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Environment knobs for 'interpretWith'.
data InterpretOptions (spec :: ChartSpec) = InterpretOptions
  { optDelay :: Int -> IO ()
  -- ^ How to wait out a delayed ('StateMachine.Spec.After') transition,
  -- given milliseconds. The default is 'threadDelay'; tests inject a
  -- controllable gate here to make timer scenarios deterministic.
  , optSendParent :: Maybe (EventVal spec -> IO ())
  -- ^ Where 'ToParent' sends go, as a /typed/ event of this chart.
  -- 'Nothing' for a top-level machine (such sends are dropped); the
  -- interpreter wires this itself for invoked child charts, translating
  -- through the invocation's 'StateMachine.Registry.ChildBridge'.
  }

-- | Real time ('threadDelay'), no parent.
defaultInterpretOptions :: InterpretOptions spec
defaultInterpretOptions =
  InterpretOptions
    { optDelay = \ms -> threadDelay (ms * 1000)
    , optSendParent = Nothing
    }

{-------------------------------------------------------------------------------
  The interpreter handle
-------------------------------------------------------------------------------}

{- | A handle to a running machine. Obtain one with 'interpret' \/
'interpretWith'; interact through 'send', 'machineView', 'waitFinished',
'subscribe', and 'halt'.
-}
data Interpreter (spec :: ChartSpec) = Interpreter
  { iQueue :: TQueue (IEvent spec)
  , iMachine :: TVar (Machine spec)
  -- ^ Last committed machine state; written only by the driver.
  , iOutcome :: TVar (Maybe (Either StepFault (Output spec)))
  -- ^ Terminal outcome, once there is one.
  , iAccepting :: TVar Bool
  -- ^ 'False' once finished, faulted, or halted; gates 'send'.
  , iDone :: TVar Bool
  -- ^ 'True' once the driver thread has exited (all resources released).
  , iSubscribers :: TVar (Map Int (Notification spec -> IO ()))
  , iNextSubId :: TVar Int
  , iDecode :: Maybe (Text -> Value -> Either String (EventVal spec))
  -- ^ The chart's event codec for the external-input boundary
  -- ('sendNamed'), captured at construction. 'Nothing' for an invoked
  -- child chart — it is reached only through its typed 'ChildBridge', so
  -- it needs no codec, and 'sendNamed' on it reports that.
  }

{- | What subscribers observe. Callbacks run /on the driver thread/, so
they must be cheap and non-blocking (in particular they must never call
'halt' or 'waitFinished' on this interpreter — that would deadlock the
driver against itself). A subscriber that throws is ignored.
-}
data Notification (spec :: ChartSpec)
  = -- | A macrostep was committed (including the initial one and the
    -- final one, whose machine is 'Finished').
    NotifyStepped (Stepped spec)
  | -- | The machine stopped on a fault; terminal.
    NotifyFault StepFault
  | -- | The machine was stopped by 'halt'; terminal.
    NotifyHalted

{-------------------------------------------------------------------------------
  Starting
-------------------------------------------------------------------------------}

-- | 'interpretWith' under 'defaultInterpretOptions'.
interpret ::
  (EventCodec spec) =>
  ChartImpl IO spec ->
  Ctx spec ->
  IO (Either StepFault (Interpreter spec))
interpret = interpretWith defaultInterpretOptions

{- | Start a machine: run 'initialize' (on the calling thread — a fault is
returned, an exception from an entry action propagates), then hand its
effects and all subsequent events to a fresh driver thread.
-}
interpretWith ::
  forall spec.
  (EventCodec spec) =>
  InterpretOptions spec ->
  ChartImpl IO spec ->
  Ctx spec ->
  IO (Either StepFault (Interpreter spec))
interpretWith = interpretCore (Just (decodeEvent @spec))

-- | The engine behind 'interpretWith', with the event decoder passed as a
-- value rather than a constraint. 'Nothing' produces an interpreter with
-- no external-input codec — what invoked child charts use, since they are
-- driven only through their typed 'ChildBridge'.
interpretCore ::
  forall spec.
  Maybe (Text -> Value -> Either String (EventVal spec)) ->
  InterpretOptions spec ->
  ChartImpl IO spec ->
  Ctx spec ->
  IO (Either StepFault (Interpreter spec))
interpretCore decoder opts impl ctx0 = do
  first <- initialize impl ctx0
  case first of
    Left f -> pure (Left f)
    Right stepped0 -> do
      queue <- newTQueueIO
      machineVar <- newTVarIO (sMachine stepped0)
      outcomeVar <- newTVarIO Nothing
      acceptingVar <- newTVarIO True
      doneVar <- newTVarIO False
      subsVar <- newTVarIO Map.empty
      subIdVar <- newTVarIO 0
      let itp =
            Interpreter
              { iQueue = queue
              , iMachine = machineVar
              , iOutcome = outcomeVar
              , iAccepting = acceptingVar
              , iDone = doneVar
              , iSubscribers = subsVar
              , iNextSubId = subIdVar
              , iDecode = decoder
              }
      _ <- async (runDriver opts impl itp stepped0)
      pure (Right itp)

{-------------------------------------------------------------------------------
  Interacting
-------------------------------------------------------------------------------}

{- | Send a typed event. Returns 'False' — and does not enqueue — once the
machine has finished, faulted, or been halted.
-}
send :: Interpreter spec -> EventVal spec -> IO Bool
send itp ev = atomically $ do
  open <- readTVar (iAccepting itp)
  when open (writeTQueue (iQueue itp) (IExternal (EvExternal ev)))
  pure open

{- | Send an event across the dynamic boundary: name and JSON payload,
decoded with the chart's codec. 'Left' explains a decode failure;
@'Right' 'False'@ means the event decoded but the machine no longer
accepts input.
-}
sendNamed :: Interpreter spec -> Text -> Value -> IO (Either String Bool)
sendNamed itp name payload = case iDecode itp of
  Nothing -> pure (Left "this machine has no event codec (it is an invoked child; send it typed events through its bridge)")
  Just dec -> case dec name payload of
    Left err -> pure (Left err)
    Right ev -> Right <$> send itp ev

-- | The last committed machine state (a snapshot; the driver may already
-- be processing the next event).
machineView :: Interpreter spec -> IO (Machine spec)
machineView = readTVarIO . iMachine

{- | Block until the machine reaches a terminal state: @'Right' output@
when a top-level final state was reached, @'Left' fault@ on a
'StepFault'. A machine halted before finishing yields
@'Left' ('InternalFault' \"halted\")@.
-}
waitFinished :: Interpreter spec -> IO (Either StepFault (Output spec))
waitFinished itp = atomically (readTVar (iOutcome itp) >>= maybe retry pure)

{- | Stop the machine: the driver processes events already accepted, then
cancels every live timer, invocation, and child actor, notifies
'NotifyHalted', and exits. Blocks until that cleanup is complete.
Idempotent — and a no-op beyond the wait if the machine already
terminated. Must not be called from a subscriber callback.
-}
halt :: Interpreter spec -> IO ()
halt itp = do
  atomically $ do
    writeTVar (iAccepting itp) False
    writeTQueue (iQueue itp) IStop
  atomically (readTVar (iDone itp) >>= check)

{-------------------------------------------------------------------------------
  Observing
-------------------------------------------------------------------------------}

{- | Register a callback for every 'Notification'; returns the unsubscribe
action. See 'Notification' for the (driver-thread) execution contract.
-}
subscribe :: Interpreter spec -> (Notification spec -> IO ()) -> IO (IO ())
subscribe itp callback = atomically $ do
  i <- readTVar (iNextSubId itp)
  writeTVar (iNextSubId itp) (i + 1)
  modifyTVar' (iSubscribers itp) (Map.insert i callback)
  pure (atomically (modifyTVar' (iSubscribers itp) (Map.delete i)))

{-------------------------------------------------------------------------------
  Driver internals
-------------------------------------------------------------------------------}

-- | What the driver consumes. Timer and invocation results carry the
-- generation they were armed under; stale generations are dropped.
data IEvent (spec :: ChartSpec)
  = IExternal (StepEvent spec)
  | ITimer TimerKey Int
  | IInvokeDone Text Int Dynamic
  | IInvokeError Text Int Dynamic
  | IStop

-- | A live invocation: a plain worker 'Async', or a child interpreter.
data Run (spec :: ChartSpec)
  = RunAsync (Async ())
  | RunChild (ChildHandle spec)

-- | A running child chart, with its own spec erased behind the parent's.
-- 'chDeliver' takes a /parent/ event and translates it to the child via
-- the invocation's 'ChildBridge' before sending; 'chHalt' stops it; the
-- waiter awaits the child's output.
data ChildHandle (spec :: ChartSpec) = ChildHandle
  { chDeliver :: EventVal spec -> IO ()
  , chHalt :: IO ()
  , chWaiter :: Async ()
  }

-- | Driver-local bookkeeping. Generation maps are monotonic (never
-- shrunk); the async\/run maps hold only live entries.
data Armed (spec :: ChartSpec) = Armed
  { armedTimerGens :: !(Map TimerKey Int)
  , armedTimers :: !(Map TimerKey (Async ()))
  , armedInvokeGens :: !(Map Text Int)
  , armedRuns :: !(Map Text (Run spec))
  }

emptyArmed :: Armed spec
emptyArmed = Armed Map.empty Map.empty Map.empty Map.empty

-- | Should the driver keep consuming the queue?
data Continue = Continue | Stop

runDriver ::
  forall spec.
  InterpretOptions spec ->
  ChartImpl IO spec ->
  Interpreter spec ->
  Stepped spec ->
  IO ()
runDriver opts impl itp stepped0 = do
  ref <- newIORef emptyArmed
  let boot = do
        c <- applyStepped ref EvInit stepped0
        case c of
          Continue -> loop ref
          Stop -> pure ()
  (boot `catch` emergency ref)
    `finally` atomically (writeTVar (iDone itp) True)
 where
  enqueue :: IEvent spec -> IO ()
  enqueue = atomically . writeTQueue (iQueue itp)

  -- External events from services/children respect the intake gate.
  enqueueEvent :: EventVal spec -> IO ()
  enqueueEvent ev = atomically $ do
    open <- readTVar (iAccepting itp)
    when open (writeTQueue (iQueue itp) (IExternal (EvExternal ev)))

  notify :: Notification spec -> IO ()
  notify n = do
    subs <- readTVarIO (iSubscribers itp)
    mapM_ (\callback -> void (trySync (callback n))) (Map.elems subs)

  -- Close the intake and record the outcome (first writer wins).
  -- Returns whether this call set it.
  settle :: Either StepFault (Output spec) -> IO Bool
  settle out = atomically $ do
    writeTVar (iAccepting itp) False
    existing <- readTVar (iOutcome itp)
    when (isNothing existing) (writeTVar (iOutcome itp) (Just out))
    pure (isNothing existing)

  loop :: IORef (Armed spec) -> IO ()
  loop ref = do
    ie <- atomically (readTQueue (iQueue itp))
    c <- dispatch ref ie
    case c of
      Continue -> loop ref
      Stop -> pure ()

  dispatch :: IORef (Armed spec) -> IEvent spec -> IO Continue
  dispatch ref = \case
    IExternal ev -> stepWith ref ev
    ITimer key gen -> do
      armed <- readIORef ref
      if Map.lookup key (armedTimerGens armed) == Just gen
        then do
          writeIORef ref armed{armedTimers = Map.delete key (armedTimers armed)}
          stepWith ref (EvTimer key)
        else pure Continue -- armed anew or cancelled since it fired: stale
    IInvokeDone invokeId gen v -> invokeResult ref invokeId gen (EvInvokeDone invokeId v)
    IInvokeError invokeId gen v -> invokeResult ref invokeId gen (EvInvokeError invokeId v)
    IStop -> do
      _ <- settle (Left (InternalFault "halted"))
      releaseAll ref
      notify NotifyHalted
      pure Stop

  invokeResult :: IORef (Armed spec) -> Text -> Int -> StepEvent spec -> IO Continue
  invokeResult ref invokeId gen ev = do
    armed <- readIORef ref
    if Map.lookup invokeId (armedInvokeGens armed) == Just gen
      then do
        writeIORef ref armed{armedRuns = Map.delete invokeId (armedRuns armed)}
        stepWith ref ev
      else pure Continue -- restarted or cancelled since: stale

  stepWith :: IORef (Armed spec) -> StepEvent spec -> IO Continue
  stepWith ref ev = do
    m <- readTVarIO (iMachine itp)
    r <- trySync (step impl m ev)
    case r of
      Left e -> do
        faultOut ref (InternalFault ("action threw: " <> T.pack (show e)))
        pure Stop
      Right (Left f) -> do
        faultOut ref f
        pure Stop
      Right (Right stepped) -> applyStepped ref ev stepped

  faultOut :: IORef (Armed spec) -> StepFault -> IO ()
  faultOut ref f = do
    _ <- settle (Left f)
    releaseAll ref
    notify (NotifyFault f)

  -- Commit one macrostep: machine state, then effects (cancels precede
  -- starts within the list), then sends (so a send may target a child
  -- started by this very step), then subscribers.
  applyStepped :: IORef (Armed spec) -> StepEvent spec -> Stepped spec -> IO Continue
  applyStepped ref ev stepped = do
    let m = sMachine stepped
    atomically (writeTVar (iMachine itp) m)
    mapM_ (applyEffect ref (mCtx m) ev) (sEffects stepped)
    armed <- readIORef ref
    mapM_ (applySend armed) (sSends stepped)
    notify (NotifyStepped stepped)
    case mStatus m of
      Running -> pure Continue
      Finished out -> do
        _ <- settle (Right out)
        releaseAll ref -- the step's own effects already cancelled; belt and braces
        pure Stop

  applyEffect :: IORef (Armed spec) -> Ctx spec -> StepEvent spec -> EffectReq -> IO ()
  applyEffect ref ctx ev req = do
    armed <- readIORef ref
    armed' <- case req of
      ReqStartTimer key -> startTimer armed key
      ReqCancelTimer key -> cancelTimer armed key
      ReqStartInvoke invokeId src _node -> startInvoke armed invokeId src ctx ev
      ReqCancelInvoke invokeId -> cancelInvoke armed invokeId
    writeIORef ref armed'

  startTimer :: Armed spec -> TimerKey -> IO (Armed spec)
  startTimer armed key = do
    let gen = 1 + Map.findWithDefault 0 key (armedTimerGens armed)
    mapM_ cancel (Map.lookup key (armedTimers armed))
    a <- async $ do
      optDelay opts (tkDelayMs key)
      enqueue (ITimer key gen)
    pure
      armed
        { armedTimerGens = Map.insert key gen (armedTimerGens armed)
        , armedTimers = Map.insert key a (armedTimers armed)
        }

  cancelTimer :: Armed spec -> TimerKey -> IO (Armed spec)
  cancelTimer armed key = do
    -- Bump even though the async is cancelled: it may already have fired
    -- into the queue, and only the generation catches that.
    let gen = 1 + Map.findWithDefault 0 key (armedTimerGens armed)
    mapM_ cancel (Map.lookup key (armedTimers armed))
    pure
      armed
        { armedTimerGens = Map.insert key gen (armedTimerGens armed)
        , armedTimers = Map.delete key (armedTimers armed)
        }

  startInvoke :: Armed spec -> Text -> Text -> Ctx spec -> StepEvent spec -> IO (Armed spec)
  startInvoke armed invokeId src ctx ev = do
    let gen = 1 + Map.findWithDefault 0 invokeId (armedInvokeGens armed)
    mapM_ stopRun (Map.lookup invokeId (armedRuns armed))
    run <- case Map.lookup src (ciServices impl) of
      Nothing -> do
        -- Unreachable via 'chartImpl' (service registries are complete);
        -- fail the invocation rather than crash on a hand-forged impl.
        enqueue (IInvokeError invokeId gen (toDyn ("unknown service: " <> src)))
        pure Nothing
      Just (SomePromise f) ->
        Just . RunAsync <$> serviceAsync invokeId gen (f ctx ev)
      Just (SomeCallback f) ->
        Just . RunAsync <$> serviceAsync invokeId gen (f ctx ev enqueueEvent)
      Just (SomeChart childImpl bridge outToDyn) ->
        startChild invokeId gen childImpl bridge outToDyn (bridgeCtx bridge ctx ev)
    pure
      armed
        { armedInvokeGens = Map.insert invokeId gen (armedInvokeGens armed)
        , armedRuns = Map.alter (const run) invokeId (armedRuns armed)
        }

  -- Promise/callback semantics: Right resolves, Left fails, a synchronous
  -- exception fails with the rendered exception (as a 'String') as the
  -- error value. Typed results arrive already boxed to 'Dynamic'.
  serviceAsync :: Text -> Int -> IO (Either Dynamic Dynamic) -> IO (Async ())
  serviceAsync invokeId gen act = async $ do
    r <- trySync act
    enqueue $ case r of
      Left e -> IInvokeError invokeId gen (toDyn (show e))
      Right (Left err) -> IInvokeError invokeId gen err
      Right (Right v) -> IInvokeDone invokeId gen v

  startChild ::
    forall child.
    Text ->
    Int ->
    ChartImpl IO child ->
    ChildBridge spec child ->
    (Output child -> Dynamic) ->
    Ctx child ->
    IO (Maybe (Run spec))
  startChild invokeId gen childImpl bridge outToDyn childCtx = do
    let childOpts =
          InterpretOptions
            { optDelay = optDelay opts
            , -- A child 'ToParent' send is a typed child event; translate
              -- it to a parent event through the bridge, then inject it.
              optSendParent = Just $ \childEv ->
                maybe (pure ()) enqueueEvent (bridgeToParent bridge childEv)
            }
    started <- trySync (interpretCore Nothing childOpts childImpl childCtx)
    case started of
      Left e -> do
        enqueue (IInvokeError invokeId gen (toDyn (show e)))
        pure Nothing
      Right (Left f) -> do
        enqueue (IInvokeError invokeId gen (toDyn (show f)))
        pure Nothing
      Right (Right child) -> do
        waiter <- async $ do
          r <- waitFinished child
          enqueue $ case r of
            Right out -> IInvokeDone invokeId gen (outToDyn out)
            Left f -> IInvokeError invokeId gen (toDyn (show f))
        pure . Just . RunChild $
          ChildHandle
            { -- A parent 'ToChild' send is a typed parent event; translate
              -- it to a child event through the bridge, then send it.
              chDeliver =
                maybe (pure ()) (void . send child) . bridgeToChild bridge
            , chHalt = halt child
            , chWaiter = waiter
            }

  cancelInvoke :: Armed spec -> Text -> IO (Armed spec)
  cancelInvoke armed invokeId = do
    let gen = 1 + Map.findWithDefault 0 invokeId (armedInvokeGens armed)
    mapM_ stopRun (Map.lookup invokeId (armedRuns armed))
    pure
      armed
        { armedInvokeGens = Map.insert invokeId gen (armedInvokeGens armed)
        , armedRuns = Map.delete invokeId (armedRuns armed)
        }

  stopRun :: Run spec -> IO ()
  stopRun = \case
    RunAsync a -> cancel a
    RunChild child -> do
      cancel (chWaiter child) -- first, so it cannot report the halt as a result
      chHalt child

  releaseAll :: IORef (Armed spec) -> IO ()
  releaseAll ref = do
    armed <- readIORef ref
    writeIORef ref emptyArmed
    mapM_ cancel (Map.elems (armedTimers armed))
    mapM_ stopRun (Map.elems (armedRuns armed))

  applySend :: Armed spec -> SendReq spec -> IO ()
  applySend armed sr = case srTarget sr of
    ToSelf -> enqueueEvent (srEvent sr)
    ToChild childId -> case Map.lookup childId (armedRuns armed) of
      Just (RunChild child) -> void (trySync (chDeliver child (srEvent sr)))
      _ -> pure () -- no live child under that id; dropped
    ToParent -> case optSendParent opts of
      Just deliver -> void (trySync (deliver (srEvent sr)))
      Nothing -> pure () -- top-level machine; dropped (see module notes)

  -- Last-resort net for interpreter bugs and external kills of the driver
  -- thread: release everything and make sure waiters wake up.
  emergency :: IORef (Armed spec) -> SomeException -> IO ()
  emergency ref e = do
    let f = InternalFault ("interpreter died: " <> T.pack (show e))
    fresh <- settle (Left f)
    releaseAll ref
    when fresh (notify (NotifyFault f))

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | 'try' for synchronous exceptions only: asynchronous exceptions
-- (thread cancellation, in particular) are rethrown so 'cancel' works.
trySync :: IO a -> IO (Either SomeException a)
trySync act =
  try act >>= \case
    Left e
      | Just (SomeAsyncException _) <- fromException e -> throwIO e
      | otherwise -> pure (Left e)
    Right a -> pure (Right a)
