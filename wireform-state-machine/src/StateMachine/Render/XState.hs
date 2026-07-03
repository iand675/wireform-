{- | Render a chart as an XState v5 machine config.

The output is a plain JSON document in the shape @createMachine@ accepts,
importable into <https://stately.ai stately.ai> for visualization and
editing. The mapping from the runtime structure ("StateMachine.Runtime"):

* The chart becomes the root object: @id@ ('rcName'), @initial@
  ('rcInitial'), @states@, plus root-level @on@ \/ @after@ \/ @always@
  ('rcRootTransitions'), @invoke@ ('rcRootInvokes'), and @entry@
  ('rcRootEntry').

* Node kinds map onto XState state types: compound → @initial@ +
  @states@, parallel → @type: \"parallel\"@, final → @type: \"final\"@
  (with an @output@ marker @{\"$producer\": name}@ when the final state
  has done data — the producer is Haskell code, so only its registry name
  can travel), history → @type: \"history\"@ with @history:
  \"shallow\"|\"deep\"@ and a @target@ when a default exists.

* Transitions are grouped by trigger: named events and wildcards under
  @on@ (a 'TDone' trigger under @on[\"done.state.NAME\"]@), eventless
  transitions under @always@, delays under @after@ keyed by milliseconds.
  Invoke completion\/failure transitions fold back into the owning
  @invoke@ entry as @onDone@ \/ @onError@. When several transitions share
  a trigger the value is an array in document order — exactly the order
  XState (and "StateMachine.Step") evaluates candidates.

* Every state carries an @id@ equal to its (chart-unique) name, and
  transition targets are absolute @#name@ references — XState resolves
  bare names relative to the source state's parent, which breaks for
  cross-level targets (history rebinds, root-level handlers).

* Guards, actions, and services are referenced /by name/: the JSON names
  the registry entries ("StateMachine.Registry") but cannot carry their
  Haskell implementations. An imported machine therefore has the full
  structure and dangling references — precisely what Stately's editor
  expects of a config authored elsewhere.
-}
module StateMachine.Render.XState (
  -- * Rendering
  xstateConfig,
  xstateConfigText,
) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

import StateMachine.Runtime

{-------------------------------------------------------------------------------
  Entry points
-------------------------------------------------------------------------------}

-- | The chart as an XState v5 machine config.
xstateConfig :: RChart -> Value
xstateConfig c =
  object
    ( ["id" .= rcName c, "initial" .= rcInitial c, "states" .= statesValue c (rcTopLevel c)]
        ++ transitionPairs (rcRootTransitions c) (rcRootInvokes c)
        ++ actionListPair "entry" (rcRootEntry c)
    )

-- | 'xstateConfig' pretty-printed (2-space indent, keys in declaration
-- order: identity first, then behavior, then child @states@).
xstateConfigText :: RChart -> Text
xstateConfigText =
  TL.toStrict . TLB.toLazyText . Pretty.encodePrettyToTextBuilder' prettyConfig . xstateConfig

prettyConfig :: Pretty.Config
prettyConfig =
  Pretty.defConfig
    { Pretty.confIndent = Pretty.Spaces 2
    , Pretty.confCompare = Pretty.keyOrder canonicalKeys <> compare
    }
 where
  canonicalKeys =
    [ "id"
    , "initial"
    , "type"
    , "history"
    , "src"
    , "target"
    , "guard"
    , "actions"
    , "reenter"
    , "entry"
    , "exit"
    , "invoke"
    , "on"
    , "after"
    , "always"
    , "onDone"
    , "onError"
    , "states"
    , "output"
    ]

{-------------------------------------------------------------------------------
  Nodes
-------------------------------------------------------------------------------}

-- | The @states@ object for a list of sibling nodes.
statesValue :: RChart -> [NodeName] -> Value
statesValue c = object . map entry
 where
  entry n = Key.fromText n .= maybe (object []) (nodeValue c) (lookupNode c n)

{- | State names are chart-unique ("StateMachine.Validate"), so every
state carries an XState @id@ equal to its name and every target is the
absolute @#name@ — bare names would be resolved relative to the source
state's parent and break for cross-level targets (XState rejects e.g. a
root-level transition to a top-level state written without @#@\/@.@).
-}
nodeValue :: RChart -> RNode -> Value
nodeValue c n = object (("id" .= rnName n) : kindPairs ++ behaviorPairs)
 where
  kindPairs = case rnKind n of
    RAtomic -> []
    RCompound ini ->
      ["initial" .= ini, "states" .= statesValue c (rnChildren n)]
    RParallel ->
      ["type" .= t "parallel", "states" .= statesValue c (rnChildren n)]
    RFinal ->
      ("type" .= t "final") : outputPairs
    RHistory kind def ->
      ["type" .= t "history", "history" .= historyName kind]
        ++ maybe [] (\d -> ["target" .= idRef d]) def
  outputPairs = case rnDoneData n of
    Nothing -> []
    Just producer -> ["output" .= object ["$producer" .= producer]]
  behaviorPairs =
    transitionPairs (rnTransitions n) (rnInvokes n)
      ++ actionListPair "entry" (rnEntry n)
      ++ actionListPair "exit" (rnExit n)
  historyName = \case
    Shallow -> t "shallow"
    Deep -> t "deep"

actionListPair :: Key -> [Text] -> [Pair]
actionListPair _ [] = []
actionListPair key actions = [key .= actions]

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

{- | The @on@ \/ @always@ \/ @after@ \/ @invoke@ pairs shared by the root
object and every state: group a document-ordered transition list by
trigger, folding invoke completion transitions into their @invoke@ entry.
-}
transitionPairs :: [RTrans] -> [RInvoke] -> [Pair]
transitionPairs transitions invokes =
  concat [onPair, alwaysPair, afterPair, invokePair]
 where
  onPair = case groupOrdered (mapMaybe onKeyed transitions) of
    [] -> []
    groups -> ["on" .= object (map (\(e, trs) -> Key.fromText e .= candidates trs) groups)]
  onKeyed tr = case rtTrigger tr of
    TOn e -> Just (e, tr)
    TWildcard -> Just ("*", tr)
    TDone s -> Just ("done.state." <> s, tr)
    _ -> Nothing

  alwaysPair = case filter ((== TAlways) . rtTrigger) transitions of
    [] -> []
    trs -> ["always" .= candidates trs]

  afterPair = case groupOrdered (mapMaybe afterKeyed transitions) of
    [] -> []
    groups ->
      ["after" .= object (map (\(ms, trs) -> Key.fromString (show ms) .= candidates trs) groups)]
  afterKeyed tr = case rtTrigger tr of
    TAfter ms -> Just (ms, tr)
    _ -> Nothing

  invokePair = case invokes of
    [] -> []
    is -> ["invoke" .= map invokeValue is]
  invokeValue inv =
    object
      ( ["id" .= riId inv, "src" .= riSrc inv]
          ++ lifecyclePair "onDone" (byLifecycle (doneOf (riId inv)))
          ++ lifecyclePair "onError" (byLifecycle (errorOf (riId inv)))
      )
  lifecyclePair key = \case
    [] -> []
    trs -> [key .= candidates trs]
  byLifecycle match = filter (match . rtTrigger) transitions
  doneOf i = \case
    TInvokeDone i' -> i == i'
    _ -> False
  errorOf i = \case
    TInvokeError i' -> i == i'
    _ -> False

-- | One transition object, or an array when several candidates share a
-- trigger (kept in document order: XState evaluates them first-match).
candidates :: [RTrans] -> Value
candidates = \case
  [tr] -> transValue tr
  trs -> toJSON (map transValue trs)

transValue :: RTrans -> Value
transValue tr =
  object
    ( catMaybes
        [ targetPair
        , fmap ("guard" .=) (rtGuard tr)
        , actionsPair
        , reenterPair
        ]
    )
 where
  targetPair = case rtTargets tr of
    [] -> Nothing
    [target] -> Just ("target" .= idRef target)
    targets -> Just ("target" .= map idRef targets)
  actionsPair = case rtActions tr of
    [] -> Nothing
    actions -> Just ("actions" .= actions)
  -- XState v5 spells "do not exit and re-enter the source" as
  -- @reenter: false@; only targeted transitions carry the flag
  -- (targetless ones never exit anything).
  reenterPair
    | rtInternal tr && not (null (rtTargets tr)) = Just ("reenter" .= False)
    | otherwise = Nothing

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Group keyed values, preserving the original order of values within
-- each key (key order is irrelevant: the result feeds a JSON object).
groupOrdered :: Ord k => [(k, v)] -> [(k, [v])]
groupOrdered = Map.toList . Map.fromListWith (flip (++)) . map (\(k, v) -> (k, [v]))

-- | An absolute reference to the state with this (unique) name.
idRef :: NodeName -> Text
idRef name = "#" <> name

-- | Pin string literals to 'Text' in '(.=)' positions.
t :: Text -> Text
t = id
