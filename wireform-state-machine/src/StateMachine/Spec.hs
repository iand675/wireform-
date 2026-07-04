{- | The type-level statechart specification language.

A chart is a /type/ of kind 'ChartSpec'. Writing it as a type means the
compiler checks it: dangling transition targets, duplicate state names,
missing initial states, references to undeclared events — all
'GHC.TypeError.TypeError's (see "StateMachine.Validate"), and the guard\/
action\/service names the spec mentions become /canonical registries/ that
implementations must satisfy completely ("StateMachine.Registry").

Every name in a chart is a promoted constructor of an ordinary sum type,
one type per role ("StateMachine.Key"): using an event where a state
belongs is a /kind/ error. Roles a chart does not use are pinned to
'StateMachine.Key.NoKey' by the chart's kind signature.

== Example

@
data FetchState = Idle | Loading | CanRetry | GaveUp | Failed | Succeeded
data FetchEvent = FETCH | CANCEL | RETRY
data FetchGuard = OutOfRetries
data FetchAction = LogStart | Save
data FetchService = HttpGet
data FetchInvoke = GetUser

'StateMachine.Key.deriveKeyKind' ''FetchState
-- … and the other five

type FetchChart ::
  'ChartSpec' FetchState FetchEvent FetchGuard FetchAction FetchService FetchInvoke 'StateMachine.Key.NoKey'
type FetchChart =
  Chart \"fetch\" FetchCtx FetchOut
    -- events, with typed payloads
    '[ \'FETCH  ::: Url
     , \'CANCEL ::: ()
     , \'RETRY  ::: ()
     ]
    -- states
    '[ State \'Idle
         '[ On \'FETCH ==> To \'Loading ]
     , State \'Loading
         '[ Entry '[ \'LogStart ]
          , Invoke \'GetUser \'HttpGet
              '[ OnDone ==> To \'Succeeded ! '[ \'Save ] ]
              '[ OnError ==> To \'Failed ]
          , On \'CANCEL ==> To \'Idle
          , After 30000 ==> To \'Failed
          ]
     , Compound \'Failed \'CanRetry
         '[ State \'CanRetry '[ On \'RETRY ==> To \'Loading ]
          , Final \'GaveUp
          ]
         '[ Always ?: \'OutOfRetries ==> To \'GaveUp ]
     , Final \'Succeeded
     ]
    \'Idle
@

State names are globally unique (checked), so a bare name is a complete
address — no path syntax.
-}
module StateMachine.Spec (
  -- * Chart
  ChartSpec (..),
  Chart,
  ChartWith,

  -- * Events
  EventSpec (..),
  type (:::),

  -- * States
  NodeSpec (..),
  State,
  Compound,
  Parallel,
  Final,
  FinalWith,
  Hist,
  HistDeep,
  HistWith,
  HistoryKind (..),

  -- * Node features
  Feature (..),
  Entry,
  Exit,
  Invoke,

  -- * Transitions
  TransSpec (..),
  TriggerSpec (..),
  TransKind (..),
  TriggerG (..),
  TargetG (..),
  On,
  Wildcard,
  Always,
  After,
  OnDoneOf,
  OnDone,
  OnError,
  type (?:),
  To,
  ToAll,
  Inside,
  Stay,
  type (==>),
  type (!),

  -- * Lookups
  ChartName,
  Ctx,
  Output,
  ChartEvents,
  ChartStates,
  ChartRoot,
  ChartInitial,
  EventPayload,
  HasEvent,
  HasState,

  -- * Canonical name collections
  ChartStateNames,
  ChartEventNames,
  ChartGuardNames,
  ChartActionNames,
  ChartServiceNames,
  ChartOutputNames,

  -- * Type-level utilities
  Nub,
  type (++),
) where

import Data.Kind (Constraint, Type)
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (Nat, Symbol)

import StateMachine.Runtime (HistoryKind (..))

{-------------------------------------------------------------------------------
  Kinds

  Seven kind parameters, one per name role:

    st   state names
    ev   event names
    g    guard names
    act  action names
    svc  service names
    inv  invoke ids
    out  done-data producer names

  Users never write them: the surface synonyms below leave them implicit,
  and a chart pins them all with one standalone kind signature.
-------------------------------------------------------------------------------}

-- | Kind of chart specifications. Construct via the 'Chart' \/ 'ChartWith'
-- synonyms rather than the promoted constructor.
data ChartSpec st ev g act svc inv out = MkChart
  { csName :: Symbol
  , csContext :: Type
  , csOutput :: Type
  , csEvents :: [EventSpec ev]
  , csStates :: [NodeSpec st ev g act svc inv out]
  , csInitial :: st
  , csRootFeatures :: [Feature st ev g act svc inv]
  }

-- | A chart with no root-level features.
type Chart name ctx out events states (initial :: st) =
  'MkChart name ctx out events states initial '[]

-- | A chart with root-level features (e.g. global event handlers that any
-- state responds to).
type ChartWith name ctx out events states (initial :: st) rootFeatures =
  'MkChart name ctx out events states initial rootFeatures

-- | An event declaration: name and payload type.
data EventSpec ev = MkEvent ev Type

-- | Declare an event: @\'FETCH ::: Url@.
type (:::) (name :: ev) (payload :: Type) = 'MkEvent name payload

infix 5 :::

-- | Kind of state declarations.
data NodeSpec st ev g act svc inv out
  = MkAtomic st [Feature st ev g act svc inv]
  | MkCompound st st [NodeSpec st ev g act svc inv out] [Feature st ev g act svc inv]
  | MkParallel st [NodeSpec st ev g act svc inv out] [Feature st ev g act svc inv]
  | MkFinal st (Maybe out)
  | MkHistory st HistoryKind (Maybe st)

-- | An atomic (leaf) state.
type State (name :: st) features = 'MkAtomic name features

-- | A compound state: exactly one child active at a time, starting at the
-- named initial child.
type Compound (name :: st) (initial :: st) children features =
  'MkCompound name initial children features

-- | A parallel state: every child is an orthogonal region, all active
-- simultaneously.
type Parallel (name :: st) regions features = 'MkParallel name regions features

-- | A final state. Entering it completes the parent (raising its done
-- event); entering a top-level final state completes the machine.
type Final (name :: st) = 'MkFinal name 'Nothing

-- | A final state whose done event carries data: the named entry of the
-- output registry produces the payload.
type FinalWith (name :: st) (outputName :: out) = 'MkFinal name ('Just outputName)

-- | A shallow history pseudo-state (falls back to the parent's initial
-- when no history has been recorded).
type Hist (name :: st) = 'MkHistory name 'Shallow 'Nothing

-- | A deep history pseudo-state.
type HistDeep (name :: st) = 'MkHistory name 'Deep 'Nothing

-- | A history pseudo-state with an explicit kind and default target.
type HistWith (name :: st) kind (def :: st) = 'MkHistory name kind ('Just def)

{-------------------------------------------------------------------------------
  Node features
-------------------------------------------------------------------------------}

-- | Kind of the things that can appear in a state's feature list.
data Feature st ev g act svc inv
  = FTrans (TransSpec st ev g act)
  | FEntry [act]
  | FExit [act]
  | FInvoke inv svc [Feature st ev g act svc inv] [Feature st ev g act svc inv]

-- | Entry actions, run in order when the state is entered.
type Entry (actions :: [act]) = 'FEntry actions

-- | Exit actions, run in order when the state is exited.
type Exit (actions :: [act]) = 'FExit actions

{- | Invoke a service while this state is active:
started on entry, cancelled on exit.

@Invoke id serviceName onDone onError@ — @onDone@ \/ @onError@ are
transition lists using the 'OnDone' \/ 'OnError' triggers:

@
Invoke \'GetUser \'HttpGet
  '[ 'OnDone' ==> To \'Succeeded ]
  '[ 'OnError' ==> To \'Failed ]
@
-}
type Invoke (invokeId :: inv) (serviceName :: svc) onDone onError =
  'FInvoke invokeId serviceName onDone onError

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

-- | Kind of fully assembled transitions (produced by '==>').
data TransSpec st ev g act
  = MkTrans (TriggerSpec st ev) (Maybe g) [st] [act] TransKind

-- | What sets a transition off.
data TriggerSpec st ev
  = TrOn ev
  | TrWildcard
  | TrAlways
  | TrAfter Nat
  | TrDone st
  | -- | Placeholder inside an 'Invoke' onDone list; resolved to the
    -- invocation id at reification.
    TrInvokeDone
  | -- | Placeholder inside an 'Invoke' onError list.
    TrInvokeError

-- | External transitions exit and re-enter their source when targeting a
-- descendant; internal ones do not.
data TransKind = External | Internal

-- | A trigger together with an optional guard (left-hand side of '==>').
data TriggerG st ev g = MkTriggerG (TriggerSpec st ev) (Maybe g)

-- | Fire on the named event.
type On (event :: ev) = 'MkTriggerG ('TrOn event) 'Nothing

-- | Fire on /any/ named event (XState @\"*\"@). Does not match done,
-- timer, or invoke lifecycle events.
type Wildcard = 'MkTriggerG 'TrWildcard 'Nothing

-- | Eventless transition: taken as soon as its guard passes while the
-- state is active.
type Always = 'MkTriggerG 'TrAlways 'Nothing

-- | Delayed transition: taken when the state has been active for the
-- given number of milliseconds.
type After (ms :: Nat) = 'MkTriggerG ('TrAfter ms) 'Nothing

-- | Fire when the named compound\/parallel state completes (all regions
-- reached final states).
type OnDoneOf (state :: st) = 'MkTriggerG ('TrDone state) 'Nothing

-- | Inside an 'Invoke' onDone list: the invocation resolved.
type OnDone = 'MkTriggerG 'TrInvokeDone 'Nothing

-- | Inside an 'Invoke' onError list: the invocation failed.
type OnError = 'MkTriggerG 'TrInvokeError 'Nothing

-- | Attach a guard to a trigger: @On \'PAY ?: \'HasBalance ==> …@.
type family (tg :: TriggerG st ev g) ?: (gd :: g) :: TriggerG st ev g where
  'MkTriggerG t 'Nothing ?: gd = 'MkTriggerG t ('Just gd)
  'MkTriggerG t ('Just g0) ?: gd =
    TypeError
      ( 'Text "A transition can have at most one guard; got both "
          ':<>: 'ShowType g0
          ':<>: 'Text " and "
          ':<>: 'ShowType gd
      )

infixl 6 ?:

-- | Targets plus actions (right-hand side of '==>').
data TargetG st = MkTargetG [st] TransKind

-- | Transition to the named state.
type To (state :: st) = 'MkTargetG '[state] 'External

-- | Transition to several states at once (targets in different regions of
-- a parallel state).
type ToAll (states :: [st]) = 'MkTargetG states 'External

-- | Internal transition to a descendant of the source: the source itself
-- is not exited and re-entered.
type Inside (state :: st) = 'MkTargetG '[state] 'Internal

-- | A targetless transition: runs its actions without exiting or entering
-- any state.
type Stay = 'MkTargetG '[] 'Internal

-- | Assemble a transition: @On \'FETCH ==> To \'Loading@.
type family (tg :: TriggerG st ev g) ==> (target :: TargetG st) :: Feature st ev g act svc inv where
  'MkTriggerG t gd ==> 'MkTargetG targets kind =
    'FTrans ('MkTrans t gd targets '[] kind)

infix 4 ==>

-- | Attach actions to a transition:
-- @On \'FETCH ==> To \'Loading ! '[ \'Log ]@.
type family (f :: Feature st ev g act svc inv) ! (actions :: [act]) :: Feature st ev g act svc inv where
  'FTrans ('MkTrans t gd targets '[] kind) ! actions =
    'FTrans ('MkTrans t gd targets actions kind)
  'FTrans ('MkTrans t gd targets as kind) ! actions =
    TypeError ('Text "Transition already has actions " ':<>: 'ShowType as)
  f ! actions =
    TypeError ('Text "(!) attaches actions to a transition (use it after ==>)")

infixl 3 !

{-------------------------------------------------------------------------------
  Lookups
-------------------------------------------------------------------------------}

type family ChartName (spec :: ChartSpec st ev g act svc inv out) :: Symbol where
  ChartName ('MkChart n _ _ _ _ _ _) = n

-- | The machine context type of a chart.
type family Ctx (spec :: ChartSpec st ev g act svc inv out) :: Type where
  Ctx ('MkChart _ ctx _ _ _ _ _) = ctx

-- | The output type produced when the machine completes.
type family Output (spec :: ChartSpec st ev g act svc inv out) :: Type where
  Output ('MkChart _ _ o _ _ _ _) = o

type family ChartEvents (spec :: ChartSpec st ev g act svc inv out) :: [EventSpec ev] where
  ChartEvents ('MkChart _ _ _ evs _ _ _) = evs

type family ChartStates (spec :: ChartSpec st ev g act svc inv out) :: [NodeSpec st ev g act svc inv out] where
  ChartStates ('MkChart _ _ _ _ sts _ _) = sts

type family ChartInitial (spec :: ChartSpec st ev g act svc inv out) :: st where
  ChartInitial ('MkChart _ _ _ _ _ ini _) = ini

type family ChartRoot (spec :: ChartSpec st ev g act svc inv out) :: [Feature st ev g act svc inv] where
  ChartRoot ('MkChart _ _ _ _ _ _ fs) = fs

-- | Payload type of a declared event; a 'TypeError' naming the known
-- events if the event is not declared.
type family EventPayload (spec :: ChartSpec st ev g act svc inv out) (e :: ev) :: Type where
  EventPayload ('MkChart n _ _ evs _ _ _) e = LookupEvent n e evs evs

type family LookupEvent (chart :: Symbol) (e :: ev) (evs :: [EventSpec ev]) (all' :: [EventSpec ev]) :: Type where
  LookupEvent chart e ('MkEvent e p ': _) _ = p
  LookupEvent chart e ('MkEvent _ _ ': rest) all' = LookupEvent chart e rest all'
  LookupEvent chart e '[] all' =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text " has no event " ':<>: 'ShowType e
          ':$$: 'Text "Declared events: " ':<>: 'ShowType (EventNames all')
      )

-- | Constraint form of 'EventPayload' existence.
type family HasEvent (spec :: ChartSpec st ev g act svc inv out) (e :: ev) :: Constraint where
  HasEvent ('MkChart n _ _ evs _ _ _) e = HasEventIn n e evs

type family HasEventIn (chart :: Symbol) (e :: ev) (evs :: [EventSpec ev]) :: Constraint where
  HasEventIn chart e ('MkEvent e _ ': _) = ()
  HasEventIn chart e ('MkEvent _ _ ': rest) = HasEventIn chart e rest
  HasEventIn chart e '[] =
    TypeError
      ('Text "Chart " ':<>: 'ShowType chart ':<>: 'Text " has no event " ':<>: 'ShowType e)

type family EventNames (evs :: [EventSpec ev]) :: [ev] where
  EventNames '[] = '[]
  EventNames ('MkEvent n _ ': rest) = n ': EventNames rest

-- | Constraint form of state existence: compiles only if the chart has a
-- state with this name.
type family HasState (spec :: ChartSpec st ev g act svc inv out) (s :: st) :: Constraint where
  HasState spec s = HasStateIn (ChartName spec) s (ChartStateNames spec) (ChartStateNames spec)

type family HasStateIn (chart :: Symbol) (s :: st) (names :: [st]) (all' :: [st]) :: Constraint where
  HasStateIn chart s (s ': _) _ = ()
  HasStateIn chart s (_ ': rest) all' = HasStateIn chart s rest all'
  HasStateIn chart s '[] all' =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text " has no state " ':<>: 'ShowType s
          ':$$: 'Text "Known states: " ':<>: 'ShowType all'
      )

{-------------------------------------------------------------------------------
  Canonical name collections
-------------------------------------------------------------------------------}

-- | Every state name in the chart, preorder.
type family ChartStateNames (spec :: ChartSpec st ev g act svc inv out) :: [st] where
  ChartStateNames spec = NodesNames (ChartStates spec)

type family NodesNames (ns :: [NodeSpec st ev g act svc inv out]) :: [st] where
  NodesNames '[] = '[]
  NodesNames (n ': rest) = NodeNames n ++ NodesNames rest

type family NodeNames (n :: NodeSpec st ev g act svc inv out) :: [st] where
  NodeNames ('MkAtomic n _) = '[n]
  NodeNames ('MkCompound n _ children _) = n ': NodesNames children
  NodeNames ('MkParallel n children _) = n ': NodesNames children
  NodeNames ('MkFinal n _) = '[n]
  NodeNames ('MkHistory n _ _) = '[n]

-- | Every declared event name.
type family ChartEventNames (spec :: ChartSpec st ev g act svc inv out) :: [ev] where
  ChartEventNames spec = EventNames (ChartEvents spec)

-- | Every guard name the chart references (deduplicated): the canonical
-- registry an implementation must provide, in this order.
type family ChartGuardNames (spec :: ChartSpec st ev g act svc inv out) :: [g] where
  ChartGuardNames spec =
    Nub (FeaturesGuards (ChartRoot spec) ++ NodesGuards (ChartStates spec))

type family NodesGuards (ns :: [NodeSpec st ev g act svc inv out]) :: [g] where
  NodesGuards '[] = '[]
  NodesGuards (n ': rest) = NodeGuards n ++ NodesGuards rest

type family NodeGuards (n :: NodeSpec st ev g act svc inv out) :: [g] where
  NodeGuards ('MkAtomic _ fs) = FeaturesGuards fs
  NodeGuards ('MkCompound _ _ children fs) = FeaturesGuards fs ++ NodesGuards children
  NodeGuards ('MkParallel _ children fs) = FeaturesGuards fs ++ NodesGuards children
  NodeGuards ('MkFinal _ _) = '[]
  NodeGuards ('MkHistory _ _ _) = '[]

type family FeaturesGuards (fs :: [Feature st ev g act svc inv]) :: [g] where
  FeaturesGuards '[] = '[]
  FeaturesGuards ('FTrans ('MkTrans _ ('Just gd) _ _ _) ': rest) = gd ': FeaturesGuards rest
  FeaturesGuards ('FInvoke _ _ onD onE ': rest) =
    FeaturesGuards onD ++ FeaturesGuards onE ++ FeaturesGuards rest
  FeaturesGuards (_ ': rest) = FeaturesGuards rest

-- | Every action name the chart references (deduplicated): entry, exit,
-- and transition actions.
type family ChartActionNames (spec :: ChartSpec st ev g act svc inv out) :: [act] where
  ChartActionNames spec =
    Nub (FeaturesActions (ChartRoot spec) ++ NodesActions (ChartStates spec))

type family NodesActions (ns :: [NodeSpec st ev g act svc inv out]) :: [act] where
  NodesActions '[] = '[]
  NodesActions (n ': rest) = NodeActions n ++ NodesActions rest

type family NodeActions (n :: NodeSpec st ev g act svc inv out) :: [act] where
  NodeActions ('MkAtomic _ fs) = FeaturesActions fs
  NodeActions ('MkCompound _ _ children fs) = FeaturesActions fs ++ NodesActions children
  NodeActions ('MkParallel _ children fs) = FeaturesActions fs ++ NodesActions children
  NodeActions ('MkFinal _ _) = '[]
  NodeActions ('MkHistory _ _ _) = '[]

type family FeaturesActions (fs :: [Feature st ev g act svc inv]) :: [act] where
  FeaturesActions '[] = '[]
  FeaturesActions ('FTrans ('MkTrans _ _ _ as _) ': rest) = as ++ FeaturesActions rest
  FeaturesActions ('FEntry as ': rest) = as ++ FeaturesActions rest
  FeaturesActions ('FExit as ': rest) = as ++ FeaturesActions rest
  FeaturesActions ('FInvoke _ _ onD onE ': rest) =
    FeaturesActions onD ++ FeaturesActions onE ++ FeaturesActions rest

-- | Every service name the chart invokes (deduplicated).
type family ChartServiceNames (spec :: ChartSpec st ev g act svc inv out) :: [svc] where
  ChartServiceNames spec =
    Nub (FeaturesServices (ChartRoot spec) ++ NodesServices (ChartStates spec))

type family NodesServices (ns :: [NodeSpec st ev g act svc inv out]) :: [svc] where
  NodesServices '[] = '[]
  NodesServices (n ': rest) = NodeServices n ++ NodesServices rest

type family NodeServices (n :: NodeSpec st ev g act svc inv out) :: [svc] where
  NodeServices ('MkAtomic _ fs) = FeaturesServices fs
  NodeServices ('MkCompound _ _ children fs) = FeaturesServices fs ++ NodesServices children
  NodeServices ('MkParallel _ children fs) = FeaturesServices fs ++ NodesServices children
  NodeServices ('MkFinal _ _) = '[]
  NodeServices ('MkHistory _ _ _) = '[]

type family FeaturesServices (fs :: [Feature st ev g act svc inv]) :: [svc] where
  FeaturesServices '[] = '[]
  FeaturesServices ('FInvoke _ src _ _ ': rest) = src ': FeaturesServices rest
  FeaturesServices (_ ': rest) = FeaturesServices rest

-- | Every done-data producer name mentioned by final states (deduplicated).
type family ChartOutputNames (spec :: ChartSpec st ev g act svc inv out) :: [out] where
  ChartOutputNames spec = Nub (NodesOutputs (ChartStates spec))

type family NodesOutputs (ns :: [NodeSpec st ev g act svc inv out]) :: [out] where
  NodesOutputs '[] = '[]
  NodesOutputs (n ': rest) = NodeOutputs n ++ NodesOutputs rest

type family NodeOutputs (n :: NodeSpec st ev g act svc inv out) :: [out] where
  NodeOutputs ('MkAtomic _ _) = '[]
  NodeOutputs ('MkCompound _ _ children _) = NodesOutputs children
  NodeOutputs ('MkParallel _ children _) = NodesOutputs children
  NodeOutputs ('MkFinal _ ('Just o)) = '[o]
  NodeOutputs ('MkFinal _ 'Nothing) = '[]
  NodeOutputs ('MkHistory _ _ _) = '[]

{-------------------------------------------------------------------------------
  Type-level utilities
-------------------------------------------------------------------------------}

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++

-- | Remove duplicates, keeping first occurrences.
type family Nub (xs :: [k]) :: [k] where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Remove x xs)

type family Remove (x :: k) (xs :: [k]) :: [k] where
  Remove _ '[] = '[]
  Remove x (x ': xs) = Remove x xs
  Remove x (y ': xs) = y ': Remove x xs
