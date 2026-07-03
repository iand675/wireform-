{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Demoting a type-level chart to its value-level structure.

A statechart is /written/ as a type of kind 'ChartSpec' ("StateMachine.Spec")
but /executed/ as a value of type 'RChart' ("StateMachine.Runtime"). This
module bridges the two: 'reifyChart' walks the spec with a family of
demotion classes (one per spec kind) and assembles the runtime tree —
parent links, preorder document positions, node depths, and per-node
transition lists with invoke lifecycle placeholders resolved to their
invocation ids.

The only vocabulary users need is 'KnownChart': it has a single catch-all
instance, so every concrete chart type satisfies it and signatures stay as
short as @'KnownChart' spec => …@.

No validation happens here — 'reifyChart' faithfully demotes whatever it is
given. Well-formedness (unique names, resolvable targets, present initial
states, …) is enforced separately by "StateMachine.Validate", which
composes with this module at the use site (see
'StateMachine.Machine.chartImpl').
-}
module StateMachine.Reify (
  -- * Reification
  KnownChart (..),
) where

import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)

import StateMachine.Runtime (
  HistoryKind (..),
  NodeName,
  RChart (..),
  RInvoke (..),
  RNode (..),
  RNodeKind (..),
  RTrans (..),
  RTrigger (..),
  rootName,
 )
import StateMachine.Spec (
  ChartEvents,
  ChartInitial,
  ChartName,
  ChartRoot,
  ChartSpec,
  ChartStates,
  EventSpec (..),
  Feature (..),
  NodeSpec (..),
  TransKind (..),
  TransSpec (..),
  TriggerSpec (..),
 )

{-------------------------------------------------------------------------------
  Reification
-------------------------------------------------------------------------------}

{- | Charts whose specification can be demoted to a value-level 'RChart'.

There is a single catch-all instance, so a user-facing signature never
needs more than @'KnownChart' spec@ — the per-component demotion
constraints are solved structurally from the spec type.
-}
class KnownChart (spec :: ChartSpec) where
  {- | The value-level chart: @'reifyChart' \@MyChart@.

  Each use re-assembles the whole structure, so evaluate it once and share
  the result — 'StateMachine.Registry.ChartImpl' does exactly that,
  storing the chart at construction ('StateMachine.Machine.chartImpl')
  rather than demoting per step.
  -}
  reifyChart :: RChart

instance
  ( KnownSymbol (ChartName spec)
  , KnownSymbol (ChartInitial spec)
  , KnownEvents (ChartEvents spec)
  , KnownNodes (ChartStates spec)
  , KnownFeatures (ChartRoot spec)
  ) =>
  KnownChart spec
  where
  reifyChart =
    assemble
      (symText @(ChartName spec))
      (symText @(ChartInitial spec))
      (eventsVal @(ChartEvents spec))
      (nodesVal @(ChartStates spec))
      (featuresVal @(ChartRoot spec))

{-------------------------------------------------------------------------------
  Intermediate tree

  What the demotion classes produce: structure demoted verbatim, before the
  assembly pass adds everything positional (parents, preorder, depth,
  transition indices) and resolves invoke placeholder triggers.
-------------------------------------------------------------------------------}

-- | A demoted node: no parent, order, or depth yet; features unflattened.
data PNode = PNode
  { pnName :: NodeName
  , pnKind :: RNodeKind
  , pnFeatures :: [PFeature]
  , pnChildren :: [PNode]
  , pnDoneData :: Maybe Text
  }

-- | A demoted feature. Invoke handler lists are already reduced to their
-- transitions.
data PFeature
  = PFTrans PTrans
  | PFEntry [Text]
  | PFExit [Text]
  | -- | Invocation id, service name, onDone transitions, onError
    -- transitions.
    PFInvoke Text Text [PTrans] [PTrans]

-- | A demoted transition: trigger possibly still a placeholder, no
-- document index yet.
data PTrans = PTrans
  { ptTrigger :: PTrigger
  , ptGuard :: Maybe Text
  , ptTargets :: [NodeName]
  , ptActions :: [Text]
  , ptInternal :: Bool
  }

-- | A demoted trigger: either final, or an invoke lifecycle placeholder
-- resolved against the enclosing invocation id during flattening.
data PTrigger
  = PResolved RTrigger
  | PInvokeDone
  | PInvokeError

{-------------------------------------------------------------------------------
  Demotion classes
-------------------------------------------------------------------------------}

-- | A 'Symbol' as 'Text'.
symText :: forall s. KnownSymbol s => Text
symText = Text.pack (symbolVal (Proxy @s))

-- | Demote a promoted 'Symbol' list.
class KnownSymbols (ss :: [Symbol]) where
  symbolsVal :: [Text]

instance KnownSymbols '[] where
  symbolsVal = []

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
  symbolsVal = symText @s : symbolsVal @ss

-- | Demote a promoted @Maybe Symbol@.
class KnownMaybeSymbol (ms :: Maybe Symbol) where
  maybeSymbolVal :: Maybe Text

instance KnownMaybeSymbol 'Nothing where
  maybeSymbolVal = Nothing

instance KnownSymbol s => KnownMaybeSymbol ('Just s) where
  maybeSymbolVal = Just (symText @s)

-- | Demote a promoted 'HistoryKind' back to its value.
class KnownHistoryKind (k :: HistoryKind) where
  historyKindVal :: HistoryKind

instance KnownHistoryKind 'Shallow where
  historyKindVal = Shallow

instance KnownHistoryKind 'Deep where
  historyKindVal = Deep

-- | Demote a 'TransKind' to the 'rtInternal' flag.
class KnownTransKind (k :: TransKind) where
  isInternalVal :: Bool

instance KnownTransKind 'External where
  isInternalVal = False

instance KnownTransKind 'Internal where
  isInternalVal = True

-- | Demote a trigger. Invoke lifecycle triggers demote to placeholders;
-- the enclosing 'FInvoke' supplies the invocation id during flattening.
class KnownTrigger (t :: TriggerSpec) where
  triggerVal :: PTrigger

instance KnownSymbol e => KnownTrigger ('TrOn e) where
  triggerVal = PResolved (TOn (symText @e))

instance KnownTrigger 'TrWildcard where
  triggerVal = PResolved TWildcard

instance KnownTrigger 'TrAlways where
  triggerVal = PResolved TAlways

instance KnownNat ms => KnownTrigger ('TrAfter ms) where
  triggerVal = PResolved (TAfter (fromIntegral (natVal (Proxy @ms))))

instance KnownSymbol s => KnownTrigger ('TrDone s) where
  triggerVal = PResolved (TDone (symText @s))

instance KnownTrigger 'TrInvokeDone where
  triggerVal = PInvokeDone

instance KnownTrigger 'TrInvokeError where
  triggerVal = PInvokeError

-- | Demote one transition.
class KnownTrans (t :: TransSpec) where
  transVal :: PTrans

instance
  ( KnownTrigger tr
  , KnownMaybeSymbol g
  , KnownSymbols targets
  , KnownSymbols actions
  , KnownTransKind kind
  ) =>
  KnownTrans ('MkTrans tr g targets actions kind)
  where
  transVal =
    PTrans
      { ptTrigger = triggerVal @tr
      , ptGuard = maybeSymbolVal @g
      , ptTargets = symbolsVal @targets
      , ptActions = symbolsVal @actions
      , ptInternal = isInternalVal @kind
      }

-- | Demote one feature.
class KnownFeature (f :: Feature) where
  featureVal :: PFeature

instance KnownTrans t => KnownFeature ('FTrans t) where
  featureVal = PFTrans (transVal @t)

instance KnownSymbols as => KnownFeature ('FEntry as) where
  featureVal = PFEntry (symbolsVal @as)

instance KnownSymbols as => KnownFeature ('FExit as) where
  featureVal = PFExit (symbolsVal @as)

instance
  (KnownSymbol i, KnownSymbol src, KnownFeatures onDone, KnownFeatures onError) =>
  KnownFeature ('FInvoke i src onDone onError)
  where
  featureVal =
    PFInvoke
      (symText @i)
      (symText @src)
      (transitionsOnly (featuresVal @onDone))
      (transitionsOnly (featuresVal @onError))

-- | Keep only the transitions of a feature list. Invoke handler lists are
-- transition lists by construction ('StateMachine.Spec.Invoke'); anything
-- else in one is left to "StateMachine.Validate" to reject.
transitionsOnly :: [PFeature] -> [PTrans]
transitionsOnly fs = [t | PFTrans t <- fs]

-- | Demote a feature list, preserving order.
class KnownFeatures (fs :: [Feature]) where
  featuresVal :: [PFeature]

instance KnownFeatures '[] where
  featuresVal = []

instance (KnownFeature f, KnownFeatures fs) => KnownFeatures (f ': fs) where
  featuresVal = featureVal @f : featuresVal @fs

-- | Demote one node declaration, children included.
class KnownNode (n :: NodeSpec) where
  nodeVal :: PNode

instance (KnownSymbol n, KnownFeatures fs) => KnownNode ('MkAtomic n fs) where
  nodeVal =
    PNode
      { pnName = symText @n
      , pnKind = RAtomic
      , pnFeatures = featuresVal @fs
      , pnChildren = []
      , pnDoneData = Nothing
      }

instance
  (KnownSymbol n, KnownSymbol ini, KnownNodes children, KnownFeatures fs) =>
  KnownNode ('MkCompound n ini children fs)
  where
  nodeVal =
    PNode
      { pnName = symText @n
      , pnKind = RCompound (symText @ini)
      , pnFeatures = featuresVal @fs
      , pnChildren = nodesVal @children
      , pnDoneData = Nothing
      }

instance
  (KnownSymbol n, KnownNodes regions, KnownFeatures fs) =>
  KnownNode ('MkParallel n regions fs)
  where
  nodeVal =
    PNode
      { pnName = symText @n
      , pnKind = RParallel
      , pnFeatures = featuresVal @fs
      , pnChildren = nodesVal @regions
      , pnDoneData = Nothing
      }

instance (KnownSymbol n, KnownMaybeSymbol out) => KnownNode ('MkFinal n out) where
  nodeVal =
    PNode
      { pnName = symText @n
      , pnKind = RFinal
      , pnFeatures = []
      , pnChildren = []
      , pnDoneData = maybeSymbolVal @out
      }

instance
  (KnownSymbol n, KnownHistoryKind k, KnownMaybeSymbol def) =>
  KnownNode ('MkHistory n k def)
  where
  nodeVal =
    PNode
      { pnName = symText @n
      , pnKind = RHistory (historyKindVal @k) (maybeSymbolVal @def)
      , pnFeatures = []
      , pnChildren = []
      , pnDoneData = Nothing
      }

-- | Demote a node list, preserving declaration order.
class KnownNodes (ns :: [NodeSpec]) where
  nodesVal :: [PNode]

instance KnownNodes '[] where
  nodesVal = []

instance (KnownNode n, KnownNodes ns) => KnownNodes (n ': ns) where
  nodesVal = nodeVal @n : nodesVal @ns

-- | Demote the event declarations: names paired with rendered payload
-- type names ('Typeable'-based, for visualization and fingerprinting).
class KnownEvents (es :: [EventSpec]) where
  eventsVal :: [(Text, Text)]

instance KnownEvents '[] where
  eventsVal = []

instance (KnownSymbol n, Typeable p, KnownEvents es) => KnownEvents ('MkEvent n p ': es) where
  eventsVal = (symText @n, Text.pack (show (typeRep (Proxy @p)))) : eventsVal @es

{-------------------------------------------------------------------------------
  Assembly

  The pure pass over the intermediate tree: parents, preorder document
  positions (the synthetic root is implicitly 0), depths (top-level = 1),
  feature flattening, and transition indexing.
-------------------------------------------------------------------------------}

-- | Assemble the demoted pieces into the final 'RChart'.
assemble :: Text -> NodeName -> [(Text, Text)] -> [PNode] -> [PFeature] -> RChart
assemble name initial events top rootFeatures =
  RChart
    { rcName = name
    , rcInitial = initial
    , rcNodes = Map.fromList (snd (demoteForest rootName 1 1 top))
    , rcTopLevel = map pnName top
    , rcEvents = events
    , rcRootTransitions = flTransitions rootFlat
    , rcRootInvokes = flInvokes rootFlat
    , rcRootEntry = flEntry rootFlat
    }
  where
    -- Exit actions make no sense on the root (it is never exited); ignored.
    rootFlat = flattenFeatures rootFeatures

-- | Assemble sibling subtrees left to right, threading the preorder
-- counter; returns the next free position and the flattened node entries.
demoteForest :: NodeName -> Int -> Int -> [PNode] -> (Int, [(NodeName, RNode)])
demoteForest _ _ next [] = (next, [])
demoteForest parent depth next (n : ns) =
  let (next', entries) = demoteTree parent depth next n
      (next'', rest) = demoteForest parent depth next' ns
   in (next'', entries ++ rest)

-- | Assemble one node at the given preorder position, then its children.
demoteTree :: NodeName -> Int -> Int -> PNode -> (Int, [(NodeName, RNode)])
demoteTree parent depth order PNode{pnName = name, pnKind, pnFeatures, pnChildren, pnDoneData} =
  let Flattened{flTransitions, flInvokes, flEntry, flExit} = flattenFeatures pnFeatures
      node =
        RNode
          { rnName = name
          , rnKind = pnKind
          , rnParent = parent
          , rnChildren = map pnName pnChildren
          , rnTransitions = flTransitions
          , rnEntry = flEntry
          , rnExit = flExit
          , rnInvokes = flInvokes
          , rnDoneData = pnDoneData
          , rnOrder = order
          , rnDepth = depth
          }
      (next, kids) = demoteForest name (depth + 1) (order + 1) pnChildren
   in (next, (name, node) : kids)

-- | What a feature list contributes to its owning node, in feature-list
-- order; 'rtIndex' already assigned (0-based within this list).
data Flattened = Flattened
  { flTransitions :: [RTrans]
  , flInvokes :: [RInvoke]
  , flEntry :: [Text]
  , flExit :: [Text]
  }

{- | Flatten a feature list, preserving its order: an 'FInvoke' contributes
its onDone transitions then its onError transitions at the position where
it appears (placeholder triggers resolved to the invocation id), plus an
'RInvoke'; entry\/exit actions concatenate in order.
-}
flattenFeatures :: [PFeature] -> Flattened
flattenFeatures features =
  let (trans, invokes, entry, exit) = go features
   in Flattened
        { flTransitions = zipWith (\i t -> t{rtIndex = i}) [0 ..] trans
        , flInvokes = invokes
        , flEntry = entry
        , flExit = exit
        }
  where
    go [] = ([], [], [], [])
    go (f : fs) =
      let (ts, is, ens, exs) = go fs
       in case f of
            PFTrans t -> (finishTrans Nothing t : ts, is, ens, exs)
            PFEntry as -> (ts, is, as ++ ens, exs)
            PFExit as -> (ts, is, ens, as ++ exs)
            PFInvoke iid src onDone onError ->
              ( map (finishTrans (Just iid)) (onDone ++ onError) ++ ts
              , RInvoke{riId = iid, riSrc = src} : is
              , ens
              , exs
              )

-- | Finish a transition: resolve placeholder triggers against the
-- enclosing invocation id. 'rtIndex' is assigned by 'flattenFeatures'.
finishTrans :: Maybe Text -> PTrans -> RTrans
finishTrans mInvoke PTrans{ptTrigger, ptGuard, ptTargets, ptActions, ptInternal} =
  RTrans
    { rtTrigger = case ptTrigger of
        PResolved t -> t
        PInvokeDone -> TInvokeDone (enclosingInvoke "OnDone")
        PInvokeError -> TInvokeError (enclosingInvoke "OnError")
    , rtGuard = ptGuard
    , rtTargets = ptTargets
    , rtActions = ptActions
    , rtInternal = ptInternal
    , rtIndex = 0
    }
  where
    -- Unreachable for validated charts: OnDone/OnError only parse inside
    -- Invoke handler lists.
    enclosingInvoke lbl =
      case mInvoke of
        Just iid -> iid
        Nothing -> error ("StateMachine.Reify: " <> lbl <> " trigger outside an Invoke block")
