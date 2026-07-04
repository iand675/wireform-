{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

{- | Completeness-checked implementation registries.

A chart /spec/ ("StateMachine.Spec") names its guards, actions, services,
and done-data producers; this module is where implementations are
registered against those names. Registration is:

* __Order-insensitive__ — entries are plucked into the chart's canonical
  order at bundle time.

* __Complete__ — a missing entry, a duplicate, or an entry whose name the
  chart never mentions is a /compile-time/ error naming the offender. A
  typo'd guard name cannot reach runtime.

The bundled 'ChartImpl' erases the type-level indices into name-keyed
maps for the step algorithm; totality of those lookups is exactly the
completeness that was just proven.
-}
module StateMachine.Registry (
  -- * Named-entry registries
  Reg (..),
  CompleteReg (..),
  Pluck (..),
  RegNames (..),
  regMap,

  -- * Guards
  GuardE (..),
  mkGuard,

  -- * Actions
  ActionE (..),
  ActionOutcome (..),
  SendReq (..),
  SendTarget (..),
  sendSelf,
  sendChild,
  sendParent,
  outcome,
  mkAction,
  assign,
  effect,
  raiseEvent,

  -- * Services
  ServiceE (..),
  mkService,
  mkServiceCallback,
  mkServiceChart,
  ChildBridge (..),
  SomeService (..),

  -- * Done-data producers
  OutputE (..),
  mkOutput,

  -- * Bundled implementations
  ChartImpl (..),
  chartImplWith,
) where

import Data.Dynamic (Dynamic, Typeable, toDyn)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (Symbol)

import StateMachine.Key (KnownKey, keyNameOf)

import StateMachine.Event (EventVal, StepEvent)
import StateMachine.Runtime (RChart)
import StateMachine.Spec (
  ChartActionNames,
  ChartGuardNames,
  ChartOutputNames,
  ChartServiceNames,
  ChartSpec,
  Ctx,
  Output,
 )

{-------------------------------------------------------------------------------
  Named-entry registries
-------------------------------------------------------------------------------}

-- | A heterogeneous list of named entries, in any order. @f@ is the entry
-- shape (e.g. @'GuardE' spec@), indexed by the entry's name — a key of
-- the role's kind, so registering (say) an action under a guard's name is
-- a kind error.
data Reg (f :: k -> Type) (names :: [k]) where
  RNil :: Reg f '[]
  (:&) :: f n -> Reg f names -> Reg f (n ': names)

infixr 5 :&

{- | Proof that @have@ is a permutation of @want@; @canonReg@ reorders.
@what@ is the registry's human name (\"guard\", \"action\", …), used in
error messages. You never write instances of this class.
-}
type CompleteReg :: forall {k}. Symbol -> [k] -> [k] -> Constraint
class CompleteReg (what :: Symbol) (want :: [k]) (have :: [k]) where
  canonReg :: Reg f have -> Reg f want

instance CompleteReg what '[] '[] where
  canonReg _ = RNil

instance
  ( TypeError
      ( 'Text "Registered a " ':<>: 'Text what ':<>: 'Text " named " ':<>: 'ShowType extra
          ':$$: 'Text "that the chart does not mention (duplicate or typo?)"
      )
  ) =>
  CompleteReg what '[] (extra ': more)
  where
  canonReg = error "unreachable"

instance
  {-# OVERLAPPABLE #-}
  ( Pluck what n have rest
  , CompleteReg what ns rest
  ) =>
  CompleteReg what (n ': ns) have
  where
  canonReg h =
    let (x, r) = pluckReg @what @n h
     in x :& canonReg @what @ns r

{- | Extract the entry named @n@ from a registry, returning the remainder.
Drives 'CompleteReg'; a missing entry is a 'TypeError'.
-}
type Pluck :: forall {k}. Symbol -> k -> [k] -> [k] -> Constraint
class
  Pluck (what :: Symbol) (n :: k) (have :: [k]) (rest :: [k])
    | n have -> rest
  where
  pluckReg :: Reg f have -> (f n, Reg f rest)

instance
  ( TypeError
      ('Text "Missing " ':<>: 'Text what ':<>: 'Text " " ':<>: 'ShowType n)
  , rest ~ '[]
  ) =>
  Pluck what n '[] rest
  where
  pluckReg = error "unreachable"

instance
  (Pluck' (EqKey h n) what n (h ': t) rest) =>
  Pluck what n (h ': t) rest
  where
  pluckReg = pluckReg' @(EqKey h n) @what @n

-- | Dispatch on whether the head entry is the one we want.
type Pluck' :: forall {k}. Bool -> Symbol -> k -> [k] -> [k] -> Constraint
class
  Pluck' (match :: Bool) (what :: Symbol) (n :: k) (have :: [k]) (rest :: [k])
    | match n have -> rest
  where
  pluckReg' :: Reg f have -> (f n, Reg f rest)

instance Pluck' 'True what n (n ': t) t where
  pluckReg' (x :& r) = (x, r)

instance
  (Pluck what n t rest) =>
  Pluck' 'False what n (h ': t) (h ': rest)
  where
  pluckReg' (x :& r) =
    let (y, r') = pluckReg @what @n r
     in (y, x :& r')

type family EqKey (a :: k) (b :: k) :: Bool where
  EqKey a a = 'True
  EqKey a b = 'False

-- | Demote a registry's names alongside its entries.
type RegNames :: forall {k}. [k] -> Constraint
class RegNames (names :: [k]) where
  regFold :: (forall (n :: k). (KnownKey n) => Proxy n -> f n -> a) -> Reg f names -> [(Text, a)]

instance RegNames '[] where
  regFold _ RNil = []

instance (KnownKey n, RegNames ns) => RegNames (n ': ns) where
  regFold k (x :& r) = (keyNameOf @n, k (Proxy @n) x) : regFold k r

-- | Erase a registry to a name-keyed map.
regMap :: (RegNames names) => (forall n. (KnownKey n) => f n -> a) -> Reg f names -> Map Text a
regMap k = Map.fromList . regFold (\_ x -> k x)

{-------------------------------------------------------------------------------
  Guards
-------------------------------------------------------------------------------}

-- | The implementation of the guard named @n@: a /pure/ predicate over
-- context and the triggering event.
newtype GuardE (spec :: ChartSpec st ev g act svc inv out) (n :: g)
  = GuardE (Ctx spec -> StepEvent spec -> Bool)

-- | Register a guard: @mkGuard \@\'OutOfRetries (\\ctx _ -> retries ctx >= 3)@.
mkGuard :: forall n spec. (Ctx spec -> StepEvent spec -> Bool) -> GuardE spec n
mkGuard = GuardE

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

{- | What running an action produced: the new context, events raised into
the /current/ macrostep's internal queue, and sends across actor
boundaries (parent, invoked children, or the machine's own external
queue).
-}
data ActionOutcome (spec :: ChartSpec st ev g act svc inv out) = ActionOutcome
  { aoCtx :: Ctx spec
  , aoRaised :: [EventVal spec]
  , aoSends :: [SendReq spec]
  }

-- | A minimal outcome: new context, nothing raised, nothing sent.
outcome :: Ctx spec -> ActionOutcome spec
outcome ctx = ActionOutcome{aoCtx = ctx, aoRaised = [], aoSends = []}

{- | A send across an actor boundary, executed by the interpreter. The
event is a /typed/ 'EventVal' of the sender's chart — no serialization.
A cross-chart target ('ToChild' \/ 'ToParent') is translated to the peer's
event type by that invocation's 'ChildBridge'.
-}
data SendReq (spec :: ChartSpec st ev g act svc inv out) = SendReq
  { srTarget :: SendTarget
  , srEvent :: EventVal spec
  }

instance Show (SendReq spec) where
  showsPrec d (SendReq t ev) =
    showParen (d > 10) $
      showString "SendReq " . showsPrec 11 t . showString " " . showsPrec 11 ev

data SendTarget
  = -- | The parent machine (when running as an invoked child); translated
    -- by the parent's 'ChildBridge' ('bridgeToParent').
    ToParent
  | -- | A live invocation of this machine, by invoke id; translated by
    -- that child's 'ChildBridge' ('bridgeToChild').
    ToChild Text
  | -- | This machine's own external queue (a fresh macrostep, unlike
    -- raising, which joins the current one).
    ToSelf
  deriving stock (Show, Eq)

-- | Send a typed event to this machine's own external queue (a fresh
-- macrostep).
sendSelf :: EventVal spec -> SendReq spec
sendSelf = SendReq ToSelf

-- | Send a typed event to a live child invocation (by invoke id); the
-- child's 'ChildBridge' translates it to the child's event type.
sendChild :: Text -> EventVal spec -> SendReq spec
sendChild i = SendReq (ToChild i)

-- | Send a typed event to the parent machine (when running as an invoked
-- child); the parent's 'ChildBridge' translates it.
sendParent :: EventVal spec -> SendReq spec
sendParent = SendReq ToParent

-- | The implementation of the action named @n@, in monad @m@.
newtype ActionE m (spec :: ChartSpec st ev g act svc inv out) (n :: act)
  = ActionE (Ctx spec -> StepEvent spec -> m (ActionOutcome spec))

-- | Register an action with full control over the outcome.
mkAction ::
  forall n m spec.
  (Ctx spec -> StepEvent spec -> m (ActionOutcome spec)) ->
  ActionE m spec n
mkAction = ActionE

-- | A pure context update (XState @assign@).
assign ::
  forall n m spec.
  (Applicative m) =>
  (Ctx spec -> StepEvent spec -> Ctx spec) ->
  ActionE m spec n
assign f = ActionE (\ctx ev -> pure (outcome (f ctx ev)))

-- | A fire-and-forget effect that leaves the context alone.
effect ::
  forall n m spec.
  (Applicative m) =>
  (Ctx spec -> StepEvent spec -> m ()) ->
  ActionE m spec n
effect f = ActionE (\ctx ev -> outcome ctx <$ f ctx ev)

-- | Raise an event into the current macrostep (XState @raise@).
raiseEvent ::
  forall n m spec.
  (Applicative m) =>
  (Ctx spec -> StepEvent spec -> EventVal spec) ->
  ActionE m spec n
raiseEvent f =
  ActionE (\ctx ev -> pure (outcome ctx){aoRaised = [f ctx ev]})

{-------------------------------------------------------------------------------
  Services
-------------------------------------------------------------------------------}

{- | The implementation of the service named @n@: what an
'StateMachine.Spec.Invoke' starts on state entry. Results are ordinary
typed Haskell values; they reach the @onDone@ \/ @onError@ handlers as
'Data.Dynamic.Dynamic' and are recovered there with
'StateMachine.Event.invokeOutput' \/ 'StateMachine.Event.invokeError'.
-}
data ServiceE m (spec :: ChartSpec st ev g act svc inv out) (n :: svc) where
  -- | Promise semantics: run to completion; 'Right' resolves the
  -- invocation (onDone), 'Left' fails it (onError). The output and error
  -- are arbitrary typed values.
  ServiceFn ::
    (Typeable out, Typeable err) =>
    (Ctx spec -> StepEvent spec -> m (Either err out)) ->
    ServiceE m spec n
  -- | Callback semantics: like 'ServiceFn', but may send typed events
  -- back into the machine while running.
  ServiceCallback ::
    (Typeable out, Typeable err) =>
    (Ctx spec -> StepEvent spec -> (EventVal spec -> m ()) -> m (Either err out)) ->
    ServiceE m spec n
  -- | Invoke another chart as a child actor, bridged type-safely: the
  -- 'ChildBridge' derives the child's context and translates events in
  -- both directions between the parent's and child's event types. The
  -- child's typed 'Output' becomes the invocation's onDone payload
  -- (recovered with @invokeOutput \@(Output child)@).
  ServiceChart ::
    (Typeable (Output child)) =>
    ChartImpl m child ->
    ChildBridge spec child ->
    ServiceE m spec n

{- | The type-safe bridge between a parent chart and an invoked child
chart. Both directions are total, typed translations — nothing is
serialized. A translation returning 'Nothing' drops that cross-boundary
event (e.g. a child event the parent does not care about).
-}
data ChildBridge (parent :: ChartSpec st ev g act svc inv out) (child :: ChartSpec st' ev' g' act' svc' inv' out') = ChildBridge
  { bridgeCtx :: Ctx parent -> StepEvent parent -> Ctx child
  -- ^ Derive the child's initial context when the invocation starts.
  , bridgeToChild :: EventVal parent -> Maybe (EventVal child)
  -- ^ Translate a parent 'ToChild' send into a child event.
  , bridgeToParent :: EventVal child -> Maybe (EventVal parent)
  -- ^ Translate a child 'ToParent' send into a parent event.
  }

-- | Register a promise service with its name pinned:
-- @mkService \@\'HttpGet (\\ctx ev -> …)@. (The bare 'ServiceFn'
-- constructor leaves the name index ambiguous at registration sites.)
mkService ::
  forall n out err m spec.
  (Typeable out, Typeable err) =>
  (Ctx spec -> StepEvent spec -> m (Either err out)) ->
  ServiceE m spec n
mkService = ServiceFn

-- | Register a callback service with its name pinned.
mkServiceCallback ::
  forall n out err m spec.
  (Typeable out, Typeable err) =>
  (Ctx spec -> StepEvent spec -> (EventVal spec -> m ()) -> m (Either err out)) ->
  ServiceE m spec n
mkServiceCallback = ServiceCallback

-- | Register a child-chart actor with its name pinned, bridged type-safely.
mkServiceChart ::
  forall n child m spec.
  (Typeable (Output child)) =>
  ChartImpl m child ->
  ChildBridge spec child ->
  ServiceE m spec n
mkServiceChart = ServiceChart

-- | 'ServiceE' with its name index erased (what 'ChartImpl' stores).
-- Typed results are boxed to 'Dynamic' here, where each service's static
-- output\/error types are still in scope.
data SomeService m (spec :: ChartSpec st ev g act svc inv out)
  = SomePromise (Ctx spec -> StepEvent spec -> m (Either Dynamic Dynamic))
  | SomeCallback (Ctx spec -> StepEvent spec -> (EventVal spec -> m ()) -> m (Either Dynamic Dynamic))
  | forall child.
    SomeChart (ChartImpl m child) (ChildBridge spec child) (Output child -> Dynamic)

eraseService :: (Functor m) => ServiceE m spec n -> SomeService m spec
eraseService = \case
  ServiceFn f -> SomePromise (\ctx ev -> boxEither <$> f ctx ev)
  ServiceCallback f -> SomeCallback (\ctx ev emit -> boxEither <$> f ctx ev emit)
  ServiceChart impl bridge -> SomeChart impl bridge toDyn
 where
  boxEither :: (Typeable out, Typeable err) => Either err out -> Either Dynamic Dynamic
  boxEither = either (Left . toDyn) (Right . toDyn)

{-------------------------------------------------------------------------------
  Done-data producers
-------------------------------------------------------------------------------}

-- | The implementation of the done-data producer named @n@ (referenced by
-- 'StateMachine.Spec.FinalWith'): computes the payload of the parent's
-- done event when the final state is entered. The payload is an ordinary
-- typed value, recovered by the @'StateMachine.Spec.OnDoneOf'@ handler
-- with 'StateMachine.Event.doneData'.
newtype OutputE (spec :: ChartSpec st ev g act svc inv out) (n :: out)
  = OutputE (Ctx spec -> StepEvent spec -> Dynamic)

mkOutput ::
  forall n a spec.
  (Typeable a) =>
  (Ctx spec -> StepEvent spec -> a) ->
  OutputE spec n
mkOutput f = OutputE (\ctx ev -> toDyn (f ctx ev))

{-------------------------------------------------------------------------------
  Bundled implementations
-------------------------------------------------------------------------------}

{- | A complete implementation of chart @spec@ in monad @m@: the demoted
chart structure plus every guard, action, service, and done-data producer
the chart names, erased to total name-keyed maps (totality is what
'chartImplWith' proved).

Build with 'StateMachine.Machine.chartImpl'.
-}
data ChartImpl m (spec :: ChartSpec st ev g act svc inv out) = ChartImpl
  { ciChart :: RChart
  , ciGuards :: Map Text (Ctx spec -> StepEvent spec -> Bool)
  , ciActions :: Map Text (Ctx spec -> StepEvent spec -> m (ActionOutcome spec))
  , ciServices :: Map Text (SomeService m spec)
  , ciOutputs :: Map Text (Ctx spec -> StepEvent spec -> Dynamic)
  , ciFinal :: Ctx spec -> Output spec
  -- ^ The machine's output, computed when a top-level final state is
  -- reached.
  }

{- | Bundle registries into a 'ChartImpl' against an already-demoted chart.
Prefer 'StateMachine.Machine.chartImpl', which supplies the chart from the
spec; this variant exists so the demotion happens exactly once.
-}
chartImplWith ::
  forall m spec gs as ss os.
  ( Functor m
  , CompleteReg "guard" (ChartGuardNames spec) gs
  , RegNames (ChartGuardNames spec)
  , CompleteReg "action" (ChartActionNames spec) as
  , RegNames (ChartActionNames spec)
  , CompleteReg "service" (ChartServiceNames spec) ss
  , RegNames (ChartServiceNames spec)
  , CompleteReg "output" (ChartOutputNames spec) os
  , RegNames (ChartOutputNames spec)
  ) =>
  RChart ->
  Reg (GuardE spec) gs ->
  Reg (ActionE m spec) as ->
  Reg (ServiceE m spec) ss ->
  Reg (OutputE spec) os ->
  (Ctx spec -> Output spec) ->
  ChartImpl m spec
chartImplWith chart gs as ss os final =
  ChartImpl
    { ciChart = chart
    , ciGuards = regMap (\(GuardE g) -> g) (canonReg @"guard" @(ChartGuardNames spec) gs)
    , ciActions = regMap (\(ActionE a) -> a) (canonReg @"action" @(ChartActionNames spec) as)
    , ciServices = regMap eraseService (canonReg @"service" @(ChartServiceNames spec) ss)
    , ciOutputs = regMap (\(OutputE o) -> o) (canonReg @"output" @(ChartOutputNames spec) os)
    , ciFinal = final
    }
