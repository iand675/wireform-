{- | Compile-time well-formedness checking of chart specifications.

'ValidChart' walks a 'ChartSpec' entirely at the type level and reduces to
the empty constraint when the chart is well-formed, or to a 'TypeError'
naming the chart and the offending name when it is not. It is demanded by
'StateMachine.Machine.chartImpl' (alongside demotion,
'StateMachine.Reify.KnownChart'), so every
'StateMachine.Runtime.RChart' obtained from a type-level spec has already
passed these checks:

1. State names are globally unique; @\"#root\"@ is reserved for the
   synthetic root and rejected as a user state name.

2. Event names are unique.

3. The chart initial is one of the /top-level/ state names and is not a
   history pseudo-state.

4. Every compound state has at least one child, and its declared initial
   names one of its /direct/ children.

5. Every transition target — including targets inside 'StateMachine.Spec.Invoke'
   onDone\/onError lists and in the root feature list — names an existing
   state.

6. Every @'On' e@ trigger references a declared event.

7. Every @'OnDoneOf' s@ trigger references an existing compound or
   parallel state (only those complete).

8. Invoke ids are unique across the whole chart, root features included.

9. The 'OnDone'\/'OnError' placeholder triggers appear /only/ inside an
   invoke's onDone\/onError lists, and those lists contain nothing else:
   every onDone entry is an @'OnDone' ==> …@ transition, every onError
   entry an @'OnError' ==> …@ one.

10. History default targets name an existing state.

11. Root features contain no 'Exit' actions (the root is never exited) and
    no 'After' transitions (the root is never re-entered, so a delay there
    would never fire).
-}
module StateMachine.Validate (
  -- * Validation
  ValidChart,
) where

import Data.Kind (Constraint)
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (Symbol)

import StateMachine.Spec (
  ChartEventNames,
  ChartInitial,
  ChartName,
  ChartRoot,
  ChartSpec,
  ChartStateNames,
  ChartStates,
  Feature (..),
  NodeSpec (..),
  TransSpec (..),
  TriggerSpec (..),
  type (++),
 )

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

{- | Well-formedness of a chart specification: reduces to the empty
constraint for a well-formed chart and to a 'TypeError' naming the chart
('ChartName') and the offender otherwise. See the module header for the
full list of checks.
-}
type family ValidChart (spec :: ChartSpec) :: Constraint where
  ValidChart spec =
    ( UniqueStateNames spec
    , UniqueEventNames spec
    , InitialStateOK spec
    , CompoundStatesOK spec
    , TransitionTargetsOK spec
    , EventTriggersOK spec
    , DoneTriggersOK spec
    , UniqueInvokeIds spec
    , InvokeTriggersPlaced spec
    , HistoryDefaultsOK spec
    , RootFeaturesOK spec
    )

{-------------------------------------------------------------------------------
  Check 1–2: unique state and event names
-------------------------------------------------------------------------------}

-- | State names are globally unique. The seen-list is seeded with
-- @\"#root\"@ ('StateMachine.Runtime.rootName') so a user state claiming
-- the synthetic root's name is also rejected.
type family UniqueStateNames (spec :: ChartSpec) :: Constraint where
  UniqueStateNames spec =
    NoDupState
      (ChartName spec)
      (FirstDup '["#root"] (ChartStateNames spec))
      (ChartStateNames spec)

type family NoDupState (chart :: Symbol) (dup :: Maybe Symbol) (names :: [Symbol]) :: Constraint where
  NoDupState _ 'Nothing _ = ()
  NoDupState chart ('Just s) names =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": duplicate state name " ':<>: 'ShowType s
          ':$$: 'Text "State names must be globally unique, and " ':<>: 'ShowType "#root"
            ':<>: 'Text " is reserved for the synthetic root."
          ':$$: 'Text "All states: " ':<>: 'ShowType names
      )

-- | Event names are unique.
type family UniqueEventNames (spec :: ChartSpec) :: Constraint where
  UniqueEventNames spec =
    NoDupEvent
      (ChartName spec)
      (FirstDup '[] (ChartEventNames spec))
      (ChartEventNames spec)

type family NoDupEvent (chart :: Symbol) (dup :: Maybe Symbol) (names :: [Symbol]) :: Constraint where
  NoDupEvent _ 'Nothing _ = ()
  NoDupEvent chart ('Just e) names =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": duplicate event name " ':<>: 'ShowType e
          ':$$: 'Text "Declared events: " ':<>: 'ShowType names
      )

{-------------------------------------------------------------------------------
  Check 3: chart initial
-------------------------------------------------------------------------------}

-- | The chart initial names a top-level state that is not a history
-- pseudo-state.
type family InitialStateOK (spec :: ChartSpec) :: Constraint where
  InitialStateOK spec =
    InitialIn (ChartName spec) (ChartInitial spec) (ChartStates spec) (ChartStates spec)

type family InitialIn (chart :: Symbol) (ini :: Symbol) (ns :: [NodeSpec]) (all' :: [NodeSpec]) :: Constraint where
  InitialIn chart ini '[] all' =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": initial state " ':<>: 'ShowType ini
          ':<>: 'Text " is not a top-level state"
          ':$$: 'Text "Top-level states: " ':<>: 'ShowType (DirectNames all')
      )
  InitialIn chart ini ('MkHistory ini _ _ ': _) _ =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": initial state " ':<>: 'ShowType ini
          ':<>: 'Text " is a history pseudo-state"
          ':$$: 'Text "The chart initial must be a real state, not a history node."
      )
  InitialIn _ ini ('MkAtomic ini _ ': _) _ = ()
  InitialIn _ ini ('MkCompound ini _ _ _ ': _) _ = ()
  InitialIn _ ini ('MkParallel ini _ _ ': _) _ = ()
  InitialIn _ ini ('MkFinal ini _ ': _) _ = ()
  InitialIn chart ini (_ ': rest) all' = InitialIn chart ini rest all'

{-------------------------------------------------------------------------------
  Check 4: compound states
-------------------------------------------------------------------------------}

-- | Every compound state has at least one child, and its declared initial
-- names one of its direct children.
type family CompoundStatesOK (spec :: ChartSpec) :: Constraint where
  CompoundStatesOK spec = NodesCompoundsOK (ChartName spec) (ChartStates spec)

type family NodesCompoundsOK (chart :: Symbol) (ns :: [NodeSpec]) :: Constraint where
  NodesCompoundsOK _ '[] = ()
  NodesCompoundsOK chart ('MkCompound n _ '[] _ ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": compound state " ':<>: 'ShowType n
          ':<>: 'Text " has no children"
          ':$$: 'Text "A compound state needs at least one child (its initial)."
      )
  NodesCompoundsOK chart ('MkCompound n ini children _ ': rest) =
    ( CompoundInitialOK chart n ini (DirectNames children) (Elem ini (DirectNames children))
    , NodesCompoundsOK chart children
    , NodesCompoundsOK chart rest
    )
  NodesCompoundsOK chart ('MkParallel _ children _ ': rest) =
    (NodesCompoundsOK chart children, NodesCompoundsOK chart rest)
  NodesCompoundsOK chart (_ ': rest) = NodesCompoundsOK chart rest

type family CompoundInitialOK (chart :: Symbol) (n :: Symbol) (ini :: Symbol) (kids :: [Symbol]) (found :: Bool) :: Constraint where
  CompoundInitialOK _ _ _ _ 'True = ()
  CompoundInitialOK chart n ini kids 'False =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": compound state " ':<>: 'ShowType n
          ':<>: 'Text " declares initial " ':<>: 'ShowType ini
          ':<>: 'Text ", which is not one of its direct children"
          ':$$: 'Text "Direct children: " ':<>: 'ShowType kids
      )

{-------------------------------------------------------------------------------
  Check 5: transition targets
-------------------------------------------------------------------------------}

-- | Every transition target — in every node's features, recursively,
-- including targets inside invoke onDone\/onError lists and in the root
-- feature list — names an existing state.
type family TransitionTargetsOK (spec :: ChartSpec) :: Constraint where
  TransitionTargetsOK spec =
    ( FeatureTargets (ChartName spec) "#root" (ChartStateNames spec) (ChartRoot spec)
    , NodeTargets (ChartName spec) (ChartStateNames spec) (ChartStates spec)
    )

type family NodeTargets (chart :: Symbol) (known :: [Symbol]) (ns :: [NodeSpec]) :: Constraint where
  NodeTargets _ _ '[] = ()
  NodeTargets chart known ('MkAtomic n fs ': rest) =
    (FeatureTargets chart n known fs, NodeTargets chart known rest)
  NodeTargets chart known ('MkCompound n _ children fs ': rest) =
    ( FeatureTargets chart n known fs
    , NodeTargets chart known children
    , NodeTargets chart known rest
    )
  NodeTargets chart known ('MkParallel n children fs ': rest) =
    ( FeatureTargets chart n known fs
    , NodeTargets chart known children
    , NodeTargets chart known rest
    )
  NodeTargets chart known (_ ': rest) = NodeTargets chart known rest

type family FeatureTargets (chart :: Symbol) (src :: Symbol) (known :: [Symbol]) (fs :: [Feature]) :: Constraint where
  FeatureTargets _ _ _ '[] = ()
  FeatureTargets chart src known ('FTrans ('MkTrans _ _ targets _ _) ': rest) =
    (EachTarget chart src known targets, FeatureTargets chart src known rest)
  FeatureTargets chart src known ('FInvoke _ _ onD onE ': rest) =
    ( FeatureTargets chart src known onD
    , FeatureTargets chart src known onE
    , FeatureTargets chart src known rest
    )
  FeatureTargets chart src known (_ ': rest) = FeatureTargets chart src known rest

type family EachTarget (chart :: Symbol) (src :: Symbol) (known :: [Symbol]) (ts :: [Symbol]) :: Constraint where
  EachTarget _ _ _ '[] = ()
  EachTarget chart src known (t ': rest) =
    (KnownTarget chart src known t (Elem t known), EachTarget chart src known rest)

type family KnownTarget (chart :: Symbol) (src :: Symbol) (known :: [Symbol]) (t :: Symbol) (found :: Bool) :: Constraint where
  KnownTarget _ _ _ _ 'True = ()
  KnownTarget chart src known t 'False =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": transition in " ':<>: 'ShowType src
          ':<>: 'Text " targets unknown state " ':<>: 'ShowType t
          ':$$: 'Text "Known states: " ':<>: 'ShowType known
      )

{-------------------------------------------------------------------------------
  Check 6: event triggers
-------------------------------------------------------------------------------}

-- | Every @'On' e@ trigger references a declared event.
type family EventTriggersOK (spec :: ChartSpec) :: Constraint where
  EventTriggersOK spec =
    ( FeatureEvents (ChartName spec) "#root" (ChartEventNames spec) (ChartRoot spec)
    , NodeEvents (ChartName spec) (ChartEventNames spec) (ChartStates spec)
    )

type family NodeEvents (chart :: Symbol) (evs :: [Symbol]) (ns :: [NodeSpec]) :: Constraint where
  NodeEvents _ _ '[] = ()
  NodeEvents chart evs ('MkAtomic n fs ': rest) =
    (FeatureEvents chart n evs fs, NodeEvents chart evs rest)
  NodeEvents chart evs ('MkCompound n _ children fs ': rest) =
    ( FeatureEvents chart n evs fs
    , NodeEvents chart evs children
    , NodeEvents chart evs rest
    )
  NodeEvents chart evs ('MkParallel n children fs ': rest) =
    ( FeatureEvents chart n evs fs
    , NodeEvents chart evs children
    , NodeEvents chart evs rest
    )
  NodeEvents chart evs (_ ': rest) = NodeEvents chart evs rest

type family FeatureEvents (chart :: Symbol) (src :: Symbol) (evs :: [Symbol]) (fs :: [Feature]) :: Constraint where
  FeatureEvents _ _ _ '[] = ()
  FeatureEvents chart src evs ('FTrans ('MkTrans ('TrOn e) _ _ _ _) ': rest) =
    (KnownEvent chart src evs e (Elem e evs), FeatureEvents chart src evs rest)
  FeatureEvents chart src evs ('FInvoke _ _ onD onE ': rest) =
    ( FeatureEvents chart src evs onD
    , FeatureEvents chart src evs onE
    , FeatureEvents chart src evs rest
    )
  FeatureEvents chart src evs (_ ': rest) = FeatureEvents chart src evs rest

type family KnownEvent (chart :: Symbol) (src :: Symbol) (evs :: [Symbol]) (e :: Symbol) (found :: Bool) :: Constraint where
  KnownEvent _ _ _ _ 'True = ()
  KnownEvent chart src evs e 'False =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": transition in " ':<>: 'ShowType src
          ':<>: 'Text " fires on undeclared event " ':<>: 'ShowType e
          ':$$: 'Text "Declared events: " ':<>: 'ShowType evs
      )

{-------------------------------------------------------------------------------
  Check 7: done triggers
-------------------------------------------------------------------------------}

-- | Every @'OnDoneOf' s@ trigger references an existing compound or
-- parallel state — only those raise done events.
type family DoneTriggersOK (spec :: ChartSpec) :: Constraint where
  DoneTriggersOK spec =
    ( FeatureDones (ChartName spec) "#root" (CompositeNames (ChartStates spec)) (ChartRoot spec)
    , NodeDones (ChartName spec) (CompositeNames (ChartStates spec)) (ChartStates spec)
    )

type family NodeDones (chart :: Symbol) (comps :: [Symbol]) (ns :: [NodeSpec]) :: Constraint where
  NodeDones _ _ '[] = ()
  NodeDones chart comps ('MkAtomic n fs ': rest) =
    (FeatureDones chart n comps fs, NodeDones chart comps rest)
  NodeDones chart comps ('MkCompound n _ children fs ': rest) =
    ( FeatureDones chart n comps fs
    , NodeDones chart comps children
    , NodeDones chart comps rest
    )
  NodeDones chart comps ('MkParallel n children fs ': rest) =
    ( FeatureDones chart n comps fs
    , NodeDones chart comps children
    , NodeDones chart comps rest
    )
  NodeDones chart comps (_ ': rest) = NodeDones chart comps rest

type family FeatureDones (chart :: Symbol) (src :: Symbol) (comps :: [Symbol]) (fs :: [Feature]) :: Constraint where
  FeatureDones _ _ _ '[] = ()
  FeatureDones chart src comps ('FTrans ('MkTrans ('TrDone s) _ _ _ _) ': rest) =
    (KnownDone chart src comps s (Elem s comps), FeatureDones chart src comps rest)
  FeatureDones chart src comps ('FInvoke _ _ onD onE ': rest) =
    ( FeatureDones chart src comps onD
    , FeatureDones chart src comps onE
    , FeatureDones chart src comps rest
    )
  FeatureDones chart src comps (_ ': rest) = FeatureDones chart src comps rest

type family KnownDone (chart :: Symbol) (src :: Symbol) (comps :: [Symbol]) (s :: Symbol) (found :: Bool) :: Constraint where
  KnownDone _ _ _ _ 'True = ()
  KnownDone chart src comps s 'False =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": transition in " ':<>: 'ShowType src
          ':<>: 'Text " waits for completion of " ':<>: 'ShowType s
          ':<>: 'Text ", which is not a compound or parallel state"
          ':$$: 'Text "Compound/parallel states: " ':<>: 'ShowType comps
      )

-- | The names of every compound and parallel state, preorder.
type family CompositeNames (ns :: [NodeSpec]) :: [Symbol] where
  CompositeNames '[] = '[]
  CompositeNames ('MkCompound n _ children _ ': rest) =
    n ': (CompositeNames children ++ CompositeNames rest)
  CompositeNames ('MkParallel n children _ ': rest) =
    n ': (CompositeNames children ++ CompositeNames rest)
  CompositeNames (_ ': rest) = CompositeNames rest

{-------------------------------------------------------------------------------
  Check 8: invoke ids
-------------------------------------------------------------------------------}

-- | Invoke ids are unique across the whole chart, root features included.
type family UniqueInvokeIds (spec :: ChartSpec) :: Constraint where
  UniqueInvokeIds spec =
    NoDupInvoke
      (ChartName spec)
      (FirstDup '[] (FeaturesInvokeIds (ChartRoot spec) ++ NodesInvokeIds (ChartStates spec)))

type family NodesInvokeIds (ns :: [NodeSpec]) :: [Symbol] where
  NodesInvokeIds '[] = '[]
  NodesInvokeIds ('MkAtomic _ fs ': rest) =
    FeaturesInvokeIds fs ++ NodesInvokeIds rest
  NodesInvokeIds ('MkCompound _ _ children fs ': rest) =
    FeaturesInvokeIds fs ++ NodesInvokeIds children ++ NodesInvokeIds rest
  NodesInvokeIds ('MkParallel _ children fs ': rest) =
    FeaturesInvokeIds fs ++ NodesInvokeIds children ++ NodesInvokeIds rest
  NodesInvokeIds (_ ': rest) = NodesInvokeIds rest

type family FeaturesInvokeIds (fs :: [Feature]) :: [Symbol] where
  FeaturesInvokeIds '[] = '[]
  FeaturesInvokeIds ('FInvoke i _ onD onE ': rest) =
    i ': (FeaturesInvokeIds onD ++ FeaturesInvokeIds onE ++ FeaturesInvokeIds rest)
  FeaturesInvokeIds (_ ': rest) = FeaturesInvokeIds rest

type family NoDupInvoke (chart :: Symbol) (dup :: Maybe Symbol) :: Constraint where
  NoDupInvoke _ 'Nothing = ()
  NoDupInvoke chart ('Just i) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": duplicate invoke id " ':<>: 'ShowType i
          ':$$: 'Text "Invoke ids must be unique across the whole chart."
      )

{-------------------------------------------------------------------------------
  Check 9: invoke trigger placement
-------------------------------------------------------------------------------}

-- | The 'StateMachine.Spec.OnDone'\/'StateMachine.Spec.OnError'
-- placeholder triggers appear only inside an invoke's onDone\/onError
-- lists, and those lists contain nothing else.
type family InvokeTriggersPlaced (spec :: ChartSpec) :: Constraint where
  InvokeTriggersPlaced spec =
    ( FeaturePlacement (ChartName spec) "#root" (ChartRoot spec)
    , NodePlacement (ChartName spec) (ChartStates spec)
    )

type family NodePlacement (chart :: Symbol) (ns :: [NodeSpec]) :: Constraint where
  NodePlacement _ '[] = ()
  NodePlacement chart ('MkAtomic n fs ': rest) =
    (FeaturePlacement chart n fs, NodePlacement chart rest)
  NodePlacement chart ('MkCompound n _ children fs ': rest) =
    ( FeaturePlacement chart n fs
    , NodePlacement chart children
    , NodePlacement chart rest
    )
  NodePlacement chart ('MkParallel n children fs ': rest) =
    ( FeaturePlacement chart n fs
    , NodePlacement chart children
    , NodePlacement chart rest
    )
  NodePlacement chart (_ ': rest) = NodePlacement chart rest

type family FeaturePlacement (chart :: Symbol) (src :: Symbol) (fs :: [Feature]) :: Constraint where
  FeaturePlacement _ _ '[] = ()
  FeaturePlacement chart src ('FTrans ('MkTrans 'TrInvokeDone _ _ _ _) ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": OnDone transition outside an Invoke, in "
          ':<>: 'ShowType src
          ':$$: 'Text "OnDone only makes sense inside an Invoke's onDone list."
      )
  FeaturePlacement chart src ('FTrans ('MkTrans 'TrInvokeError _ _ _ _) ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": OnError transition outside an Invoke, in "
          ':<>: 'ShowType src
          ':$$: 'Text "OnError only makes sense inside an Invoke's onError list."
      )
  FeaturePlacement chart src ('FInvoke i _ onD onE ': rest) =
    ( OnDoneListOK chart i onD
    , OnErrorListOK chart i onE
    , FeaturePlacement chart src rest
    )
  FeaturePlacement chart src (_ ': rest) = FeaturePlacement chart src rest

type family OnDoneListOK (chart :: Symbol) (i :: Symbol) (fs :: [Feature]) :: Constraint where
  OnDoneListOK _ _ '[] = ()
  OnDoneListOK chart i ('FTrans ('MkTrans 'TrInvokeDone _ _ _ _) ': rest) =
    OnDoneListOK chart i rest
  OnDoneListOK chart i (_ ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": invoke " ':<>: 'ShowType i
          ':<>: 'Text " has a non-OnDone entry in its onDone list"
          ':$$: 'Text "Every onDone entry must be an OnDone ==> ... transition."
      )

type family OnErrorListOK (chart :: Symbol) (i :: Symbol) (fs :: [Feature]) :: Constraint where
  OnErrorListOK _ _ '[] = ()
  OnErrorListOK chart i ('FTrans ('MkTrans 'TrInvokeError _ _ _ _) ': rest) =
    OnErrorListOK chart i rest
  OnErrorListOK chart i (_ ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": invoke " ':<>: 'ShowType i
          ':<>: 'Text " has a non-OnError entry in its onError list"
          ':$$: 'Text "Every onError entry must be an OnError ==> ... transition."
      )

{-------------------------------------------------------------------------------
  Check 10: history defaults
-------------------------------------------------------------------------------}

-- | History default targets name an existing state.
type family HistoryDefaultsOK (spec :: ChartSpec) :: Constraint where
  HistoryDefaultsOK spec =
    NodeHistories (ChartName spec) (ChartStateNames spec) (ChartStates spec)

type family NodeHistories (chart :: Symbol) (known :: [Symbol]) (ns :: [NodeSpec]) :: Constraint where
  NodeHistories _ _ '[] = ()
  NodeHistories chart known ('MkHistory n _ ('Just def) ': rest) =
    ( KnownHistoryDefault chart n known def (Elem def known)
    , NodeHistories chart known rest
    )
  NodeHistories chart known ('MkCompound _ _ children _ ': rest) =
    (NodeHistories chart known children, NodeHistories chart known rest)
  NodeHistories chart known ('MkParallel _ children _ ': rest) =
    (NodeHistories chart known children, NodeHistories chart known rest)
  NodeHistories chart known (_ ': rest) = NodeHistories chart known rest

type family KnownHistoryDefault (chart :: Symbol) (n :: Symbol) (known :: [Symbol]) (def :: Symbol) (found :: Bool) :: Constraint where
  KnownHistoryDefault _ _ _ _ 'True = ()
  KnownHistoryDefault chart n known def 'False =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": history state " ':<>: 'ShowType n
          ':<>: 'Text " has unknown default target " ':<>: 'ShowType def
          ':$$: 'Text "Known states: " ':<>: 'ShowType known
      )

{-------------------------------------------------------------------------------
  Check 11: root features
-------------------------------------------------------------------------------}

-- | Root features contain no 'StateMachine.Spec.Exit' actions and no
-- 'StateMachine.Spec.After' transitions: the root is never exited nor
-- re-entered, so neither could ever run.
type family RootFeaturesOK (spec :: ChartSpec) :: Constraint where
  RootFeaturesOK spec = RootFeatures (ChartName spec) (ChartRoot spec)

type family RootFeatures (chart :: Symbol) (fs :: [Feature]) :: Constraint where
  RootFeatures _ '[] = ()
  RootFeatures chart ('FExit as ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": root features contain Exit actions "
          ':<>: 'ShowType as
          ':$$: 'Text "The root is never exited, so its Exit actions would never run."
      )
  RootFeatures chart ('FTrans ('MkTrans ('TrAfter ms) _ _ _ _) ': _) =
    TypeError
      ( 'Text "Chart " ':<>: 'ShowType chart ':<>: 'Text ": root features contain an After "
          ':<>: 'ShowType ms
          ':<>: 'Text " transition"
          ':$$: 'Text "The root is never re-entered, so a delayed transition there would never fire."
      )
  RootFeatures chart (_ ': rest) = RootFeatures chart rest

{-------------------------------------------------------------------------------
  Shared helpers
-------------------------------------------------------------------------------}

-- | Is the symbol in the list?
type family Elem (x :: Symbol) (xs :: [Symbol]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': rest) = Elem x rest

-- | The first name occurring twice (or already in the seed list), if any.
type family FirstDup (seen :: [Symbol]) (xs :: [Symbol]) :: Maybe Symbol where
  FirstDup _ '[] = 'Nothing
  FirstDup seen (x ': rest) = FirstDup1 (Elem x seen) x seen rest

type family FirstDup1 (dup :: Bool) (x :: Symbol) (seen :: [Symbol]) (rest :: [Symbol]) :: Maybe Symbol where
  FirstDup1 'True x _ _ = 'Just x
  FirstDup1 'False x seen rest = FirstDup (x ': seen) rest

-- | The names of the nodes in a list, without descending into children.
type family DirectNames (ns :: [NodeSpec]) :: [Symbol] where
  DirectNames '[] = '[]
  DirectNames (n ': rest) = NodeNameOf n ': DirectNames rest

-- | A node's own name.
type family NodeNameOf (n :: NodeSpec) :: Symbol where
  NodeNameOf ('MkAtomic n _) = n
  NodeNameOf ('MkCompound n _ _ _) = n
  NodeNameOf ('MkParallel n _ _) = n
  NodeNameOf ('MkFinal n _) = n
  NodeNameOf ('MkHistory n _ _) = n
