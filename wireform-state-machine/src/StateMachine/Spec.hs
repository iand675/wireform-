{- | The type-level statechart specification language.

A chart is a /type/ of kind 'ChartSpec'. Writing it as a type means the
compiler checks it: dangling transition targets, duplicate state names,
missing initial states, references to undeclared events — all
'GHC.TypeError.TypeError's (see "StateMachine.Validate"), and the guard\/
action\/service names the spec mentions become /canonical registries/ that
implementations must satisfy completely ("StateMachine.Registry").

== Example

@
type FetchCtx = ...   -- machine context (any type)
type FetchOut = ...   -- output produced when the machine reaches a top-level final state

type FetchChart =
  Chart \"fetch\" FetchCtx FetchOut
    -- events, with typed payloads
    '[ \"FETCH\"  ::: Url
     , \"CANCEL\" ::: ()
     , \"RETRY\"  ::: ()
     ]
    -- states
    '[ State \"idle\"
         '[ On \"FETCH\" ==> To \"loading\" ]
     , State \"loading\"
         '[ Entry '[\"logStart\"]
          , Invoke \"getUser\" \"httpGet\"
              '[ OnDone ==> To \"success\" ! '[\"save\"] ]
              '[ OnError ==> To \"failure\" ]
          , On \"CANCEL\" ==> To \"idle\"
          , After 30000 ==> To \"failure\"
          ]
     , Compound \"failure\" \"canRetry\"
         '[ State \"canRetry\" '[ On \"RETRY\" ==> To \"loading\" ]
          , Final \"gaveUp\"
          ]
         '[ Always ?: \"outOfRetries\" ==> To \"gaveUp\" ]
     , Final \"success\"
     ]
    \"idle\"
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
-------------------------------------------------------------------------------}

-- | Kind of chart specifications. Construct via the 'Chart' \/ 'ChartWith'
-- synonyms rather than the promoted constructor.
data ChartSpec = MkChart
  { csName :: Symbol
  , csContext :: Type
  , csOutput :: Type
  , csEvents :: [EventSpec]
  , csStates :: [NodeSpec]
  , csInitial :: Symbol
  , csRootFeatures :: [Feature]
  }

-- | A chart with no root-level features.
type Chart name ctx out events states initial =
  'MkChart name ctx out events states initial '[]

-- | A chart with root-level features (e.g. global event handlers that any
-- state responds to).
type ChartWith name ctx out events states initial rootFeatures =
  'MkChart name ctx out events states initial rootFeatures

-- | An event declaration: name and payload type.
data EventSpec = MkEvent Symbol Type

-- | Declare an event: @\"FETCH\" ::: Url@.
type (:::) (name :: Symbol) (payload :: Type) = 'MkEvent name payload

infix 5 :::

-- | Kind of state declarations.
data NodeSpec
  = MkAtomic Symbol [Feature]
  | MkCompound Symbol Symbol [NodeSpec] [Feature]
  | MkParallel Symbol [NodeSpec] [Feature]
  | MkFinal Symbol (Maybe Symbol)
  | MkHistory Symbol HistoryKind (Maybe Symbol)

-- | An atomic (leaf) state.
type State name features = 'MkAtomic name features

-- | A compound state: exactly one child active at a time, starting at the
-- named initial child.
type Compound name initial children features =
  'MkCompound name initial children features

-- | A parallel state: every child is an orthogonal region, all active
-- simultaneously.
type Parallel name regions features = 'MkParallel name regions features

-- | A final state. Entering it completes the parent (raising its done
-- event); entering a top-level final state completes the machine.
type Final name = 'MkFinal name 'Nothing

-- | A final state whose done event carries data: the named entry of the
-- output registry produces the payload.
type FinalWith name outputName = 'MkFinal name ('Just outputName)

-- | A shallow history pseudo-state (falls back to the parent's initial
-- when no history has been recorded).
type Hist name = 'MkHistory name 'Shallow 'Nothing

-- | A deep history pseudo-state.
type HistDeep name = 'MkHistory name 'Deep 'Nothing

-- | A history pseudo-state with an explicit kind and default target.
type HistWith name kind def = 'MkHistory name kind ('Just def)

{-------------------------------------------------------------------------------
  Node features
-------------------------------------------------------------------------------}

-- | Kind of the things that can appear in a state's feature list.
data Feature
  = FTrans TransSpec
  | FEntry [Symbol]
  | FExit [Symbol]
  | FInvoke Symbol Symbol [Feature] [Feature]

-- | Entry actions, run in order when the state is entered.
type Entry actions = 'FEntry actions

-- | Exit actions, run in order when the state is exited.
type Exit actions = 'FExit actions

{- | Invoke a service while this state is active:
started on entry, cancelled on exit.

@Invoke id serviceName onDone onError@ — @onDone@ \/ @onError@ are
transition lists using the 'OnDone' \/ 'OnError' triggers:

@
Invoke \"getUser\" \"httpGet\"
  '[ 'OnDone' ==> To \"success\" ]
  '[ 'OnError' ==> To \"failure\" ]
@
-}
type Invoke invokeId serviceName onDone onError =
  'FInvoke invokeId serviceName onDone onError

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

-- | Kind of fully assembled transitions (produced by '==>').
data TransSpec = MkTrans TriggerSpec (Maybe Symbol) [Symbol] [Symbol] TransKind

-- | What sets a transition off.
data TriggerSpec
  = TrOn Symbol
  | TrWildcard
  | TrAlways
  | TrAfter Nat
  | TrDone Symbol
  | -- | Placeholder inside an 'Invoke' onDone list; resolved to the
    -- invocation id at reification.
    TrInvokeDone
  | -- | Placeholder inside an 'Invoke' onError list.
    TrInvokeError

-- | External transitions exit and re-enter their source when targeting a
-- descendant; internal ones do not.
data TransKind = External | Internal

-- | A trigger together with an optional guard (left-hand side of '==>').
data TriggerG = MkTriggerG TriggerSpec (Maybe Symbol)

-- | Fire on the named event.
type On event = 'MkTriggerG ('TrOn event) 'Nothing

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
type OnDoneOf state = 'MkTriggerG ('TrDone state) 'Nothing

-- | Inside an 'Invoke' onDone list: the invocation resolved.
type OnDone = 'MkTriggerG 'TrInvokeDone 'Nothing

-- | Inside an 'Invoke' onError list: the invocation failed.
type OnError = 'MkTriggerG 'TrInvokeError 'Nothing

-- | Attach a guard to a trigger: @On \"PAY\" ?: \"hasBalance\" ==> …@.
type family (tg :: TriggerG) ?: (g :: Symbol) :: TriggerG where
  'MkTriggerG t 'Nothing ?: g = 'MkTriggerG t ('Just g)
  'MkTriggerG t ('Just g0) ?: g =
    TypeError
      ( 'Text "A transition can have at most one guard; got both "
          ':<>: 'ShowType g0
          ':<>: 'Text " and "
          ':<>: 'ShowType g
      )

infixl 6 ?:

-- | Targets plus actions (right-hand side of '==>').
data TargetG = MkTargetG [Symbol] TransKind

-- | Transition to the named state.
type To state = 'MkTargetG '[state] 'External

-- | Transition to several states at once (targets in different regions of
-- a parallel state).
type ToAll states = 'MkTargetG states 'External

-- | Internal transition to a descendant of the source: the source itself
-- is not exited and re-entered.
type Inside state = 'MkTargetG '[state] 'Internal

-- | A targetless transition: runs its actions without exiting or entering
-- any state.
type Stay = 'MkTargetG '[] 'Internal

-- | Assemble a transition: @On \"FETCH\" ==> To \"loading\"@.
type family (tg :: TriggerG) ==> (target :: TargetG) :: Feature where
  'MkTriggerG t g ==> 'MkTargetG targets kind =
    'FTrans ('MkTrans t g targets '[] kind)

infix 4 ==>

-- | Attach actions to a transition:
-- @On \"FETCH\" ==> To \"loading\" ! '[\"log\"]@.
type family (f :: Feature) ! (actions :: [Symbol]) :: Feature where
  'FTrans ('MkTrans t g targets '[] kind) ! actions =
    'FTrans ('MkTrans t g targets actions kind)
  'FTrans ('MkTrans t g targets as kind) ! actions =
    TypeError ('Text "Transition already has actions " ':<>: 'ShowType as)
  f ! actions =
    TypeError ('Text "(!) attaches actions to a transition (use it after ==>)")

infixl 3 !

{-------------------------------------------------------------------------------
  Lookups
-------------------------------------------------------------------------------}

type family ChartName (spec :: ChartSpec) :: Symbol where
  ChartName ('MkChart n _ _ _ _ _ _) = n

-- | The machine context type of a chart.
type family Ctx (spec :: ChartSpec) :: Type where
  Ctx ('MkChart _ ctx _ _ _ _ _) = ctx

-- | The output type produced when the machine completes.
type family Output (spec :: ChartSpec) :: Type where
  Output ('MkChart _ _ out _ _ _ _) = out

type family ChartEvents (spec :: ChartSpec) :: [EventSpec] where
  ChartEvents ('MkChart _ _ _ evs _ _ _) = evs

type family ChartStates (spec :: ChartSpec) :: [NodeSpec] where
  ChartStates ('MkChart _ _ _ _ sts _ _) = sts

type family ChartInitial (spec :: ChartSpec) :: Symbol where
  ChartInitial ('MkChart _ _ _ _ _ ini _) = ini

type family ChartRoot (spec :: ChartSpec) :: [Feature] where
  ChartRoot ('MkChart _ _ _ _ _ _ fs) = fs

-- | Payload type of a declared event; a 'TypeError' naming the known
-- events if the event is not declared.
type family EventPayload (spec :: ChartSpec) (e :: Symbol) :: Type where
  EventPayload ('MkChart n _ _ evs _ _ _) e = LookupEvent n e evs evs

type family LookupEvent (chart :: Symbol) (e :: Symbol) (evs :: [EventSpec]) (all' :: [EventSpec]) :: Type where
  LookupEvent chart e ('MkEvent e p ': _) _ = p
  LookupEvent chart e ('MkEvent _ _ ': rest) all' = LookupEvent chart e rest all'
  LookupEvent chart e '[] all' =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text " has no event " ':<>: 'ShowType e
          ':$$: 'Text "Declared events: " ':<>: 'ShowType (EventNames all')
      )

-- | Constraint form of 'EventPayload' existence.
type family HasEvent (spec :: ChartSpec) (e :: Symbol) :: Constraint where
  HasEvent ('MkChart n _ _ evs _ _ _) e = HasEventIn n e evs

type family HasEventIn (chart :: Symbol) (e :: Symbol) (evs :: [EventSpec]) :: Constraint where
  HasEventIn chart e ('MkEvent e _ ': _) = ()
  HasEventIn chart e ('MkEvent _ _ ': rest) = HasEventIn chart e rest
  HasEventIn chart e '[] =
    TypeError
      ('Text "Chart " ':<>: 'ShowType chart ':<>: 'Text " has no event " ':<>: 'ShowType e)

type family EventNames (evs :: [EventSpec]) :: [Symbol] where
  EventNames '[] = '[]
  EventNames ('MkEvent n _ ': rest) = n ': EventNames rest

-- | Constraint form of state existence: compiles only if the chart has a
-- state with this name.
type family HasState (spec :: ChartSpec) (s :: Symbol) :: Constraint where
  HasState spec s = HasStateIn (ChartName spec) s (ChartStateNames spec) (ChartStateNames spec)

type family HasStateIn (chart :: Symbol) (s :: Symbol) (names :: [Symbol]) (all' :: [Symbol]) :: Constraint where
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
type family ChartStateNames (spec :: ChartSpec) :: [Symbol] where
  ChartStateNames spec = NodesNames (ChartStates spec)

type family NodesNames (ns :: [NodeSpec]) :: [Symbol] where
  NodesNames '[] = '[]
  NodesNames (n ': rest) = NodeNames n ++ NodesNames rest

type family NodeNames (n :: NodeSpec) :: [Symbol] where
  NodeNames ('MkAtomic n _) = '[n]
  NodeNames ('MkCompound n _ children _) = n ': NodesNames children
  NodeNames ('MkParallel n children _) = n ': NodesNames children
  NodeNames ('MkFinal n _) = '[n]
  NodeNames ('MkHistory n _ _) = '[n]

-- | Every declared event name.
type family ChartEventNames (spec :: ChartSpec) :: [Symbol] where
  ChartEventNames spec = EventNames (ChartEvents spec)

-- | Every guard name the chart references (deduplicated): the canonical
-- registry an implementation must provide, in this order.
type family ChartGuardNames (spec :: ChartSpec) :: [Symbol] where
  ChartGuardNames spec =
    Nub (FeaturesGuards (ChartRoot spec) ++ NodesGuards (ChartStates spec))

type family NodesGuards (ns :: [NodeSpec]) :: [Symbol] where
  NodesGuards '[] = '[]
  NodesGuards (n ': rest) = NodeGuards n ++ NodesGuards rest

type family NodeGuards (n :: NodeSpec) :: [Symbol] where
  NodeGuards ('MkAtomic _ fs) = FeaturesGuards fs
  NodeGuards ('MkCompound _ _ children fs) = FeaturesGuards fs ++ NodesGuards children
  NodeGuards ('MkParallel _ children fs) = FeaturesGuards fs ++ NodesGuards children
  NodeGuards ('MkFinal _ _) = '[]
  NodeGuards ('MkHistory _ _ _) = '[]

type family FeaturesGuards (fs :: [Feature]) :: [Symbol] where
  FeaturesGuards '[] = '[]
  FeaturesGuards ('FTrans ('MkTrans _ ('Just g) _ _ _) ': rest) = g ': FeaturesGuards rest
  FeaturesGuards ('FInvoke _ _ onD onE ': rest) =
    FeaturesGuards onD ++ FeaturesGuards onE ++ FeaturesGuards rest
  FeaturesGuards (_ ': rest) = FeaturesGuards rest

-- | Every action name the chart references (deduplicated): entry, exit,
-- and transition actions.
type family ChartActionNames (spec :: ChartSpec) :: [Symbol] where
  ChartActionNames spec =
    Nub (FeaturesActions (ChartRoot spec) ++ NodesActions (ChartStates spec))

type family NodesActions (ns :: [NodeSpec]) :: [Symbol] where
  NodesActions '[] = '[]
  NodesActions (n ': rest) = NodeActions n ++ NodesActions rest

type family NodeActions (n :: NodeSpec) :: [Symbol] where
  NodeActions ('MkAtomic _ fs) = FeaturesActions fs
  NodeActions ('MkCompound _ _ children fs) = FeaturesActions fs ++ NodesActions children
  NodeActions ('MkParallel _ children fs) = FeaturesActions fs ++ NodesActions children
  NodeActions ('MkFinal _ _) = '[]
  NodeActions ('MkHistory _ _ _) = '[]

type family FeaturesActions (fs :: [Feature]) :: [Symbol] where
  FeaturesActions '[] = '[]
  FeaturesActions ('FTrans ('MkTrans _ _ _ as _) ': rest) = as ++ FeaturesActions rest
  FeaturesActions ('FEntry as ': rest) = as ++ FeaturesActions rest
  FeaturesActions ('FExit as ': rest) = as ++ FeaturesActions rest
  FeaturesActions ('FInvoke _ _ onD onE ': rest) =
    FeaturesActions onD ++ FeaturesActions onE ++ FeaturesActions rest

-- | Every service name the chart invokes (deduplicated).
type family ChartServiceNames (spec :: ChartSpec) :: [Symbol] where
  ChartServiceNames spec =
    Nub (FeaturesServices (ChartRoot spec) ++ NodesServices (ChartStates spec))

type family NodesServices (ns :: [NodeSpec]) :: [Symbol] where
  NodesServices '[] = '[]
  NodesServices (n ': rest) = NodeServices n ++ NodesServices rest

type family NodeServices (n :: NodeSpec) :: [Symbol] where
  NodeServices ('MkAtomic _ fs) = FeaturesServices fs
  NodeServices ('MkCompound _ _ children fs) = FeaturesServices fs ++ NodesServices children
  NodeServices ('MkParallel _ children fs) = FeaturesServices fs ++ NodesServices children
  NodeServices ('MkFinal _ _) = '[]
  NodeServices ('MkHistory _ _ _) = '[]

type family FeaturesServices (fs :: [Feature]) :: [Symbol] where
  FeaturesServices '[] = '[]
  FeaturesServices ('FInvoke _ src _ _ ': rest) = src ': FeaturesServices rest
  FeaturesServices (_ ': rest) = FeaturesServices rest

-- | Every done-data producer name mentioned by final states (deduplicated).
type family ChartOutputNames (spec :: ChartSpec) :: [Symbol] where
  ChartOutputNames spec = Nub (NodesOutputs (ChartStates spec))

type family NodesOutputs (ns :: [NodeSpec]) :: [Symbol] where
  NodesOutputs '[] = '[]
  NodesOutputs (n ': rest) = NodeOutputs n ++ NodesOutputs rest

type family NodeOutputs (n :: NodeSpec) :: [Symbol] where
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
