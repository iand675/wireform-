-- Error/warning constructors deliberately carry named fields that differ
-- per constructor (same policy as hermes's Mailbox/Origin).
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | Snapshots: serializing machine state and restoring it later.

A 'Snapshot' is a plain JSON value: the chart's name and structural
fingerprint, the active configuration, the context, recorded history, and
the run status. Nothing in it is a closure — snapshots survive process
restarts, deployments, and (deliberately) /chart changes/.

Restoring is where charts meet their past selves. 'restore' validates
everything and refuses precisely: an unknown state name, an incoherent
configuration, a context that no longer parses — each a distinct
'RestoreError' carrying what a caller needs to react (including whether
the chart's fingerprint had changed, i.e. \"this snapshot came from an
older chart\"). 'restoreWith' adds 'Recovery' strategies so an
application can decide /per failure mode/ whether to restart fresh,
resume at a chosen state, or give up.

Two deliberate semantics:

* __Timers and invocations restart.__ A snapshot does not store elapsed
  delays or in-flight service calls; 'restoredEffects' re-arms them from
  zero. Document your delays accordingly.

* __History is sanitized, not trusted.__ Stored history referring to
  states that no longer exist is dropped (with a 'RestoreWarning'), never
  an error — history is an optimization, not truth.
-}
module StateMachine.Persist (
  -- * Snapshots
  Snapshot (..),
  snapshot,
  chartFingerprint,

  -- * Restoring
  Restored (..),
  RestoreWarning (..),
  RestoreError (..),
  restore,

  -- * Recovery strategies
  Recovery (..),
  RecoveryAction (..),
  RestoreOutcome (..),
  noRecovery,
  restartRecovery,
  restoreWith,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Numeric (showHex)

import StateMachine.Machine (Machine (..), Status (..))
import StateMachine.Registry (ChartImpl (..))
import StateMachine.Runtime
import StateMachine.Spec (ChartSpec, Ctx, Output)
import StateMachine.Step (Stepped, completionOf, initialize)

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

-- | A serialized machine state. The JSON encoding is stable; version 1.
data Snapshot = Snapshot
  { snapVersion :: Int
  , snapChart :: Text
  , snapFingerprint :: Text
  -- ^ 'chartFingerprint' of the chart that produced the snapshot.
  , snapConfig :: [NodeName]
  , snapContext :: Value
  , snapHistory :: Map NodeName [NodeName]
  , snapStatus :: Maybe Value
  -- ^ 'Nothing' = running; @'Just' output@ = finished with this output.
  }
  deriving stock (Show, Eq)

instance ToJSON Snapshot where
  toJSON s =
    object
      [ "version" .= snapVersion s
      , "chart" .= snapChart s
      , "fingerprint" .= snapFingerprint s
      , "configuration" .= snapConfig s
      , "context" .= snapContext s
      , "history" .= snapHistory s
      , "status" .= maybe (String "running") (\o -> object ["finished" .= o]) (snapStatus s)
      ]

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \o -> do
    v <- o .: "version"
    chart <- o .: "chart"
    fp <- o .: "fingerprint"
    cfg <- o .: "configuration"
    ctx <- o .: "context"
    hist <- fromMaybe Map.empty <$> o .:? "history"
    statusV <- o .: "status"
    st <- case statusV of
      String "running" -> pure Nothing
      other -> flip (withObject "status") other $ \so -> Just <$> so .: "finished"
    pure
      Snapshot
        { snapVersion = v
        , snapChart = chart
        , snapFingerprint = fp
        , snapConfig = cfg
        , snapContext = ctx
        , snapHistory = hist
        , snapStatus = st
        }

-- | Serialize a machine.
snapshot ::
  (ToJSON (Ctx spec), ToJSON (Output spec)) =>
  ChartImpl m spec ->
  Machine spec ->
  Snapshot
snapshot impl m =
  Snapshot
    { snapVersion = 1
    , snapChart = rcName chart
    , snapFingerprint = chartFingerprint chart
    , snapConfig = Set.toList (mConfig m)
    , snapContext = toJSON (mCtx m)
    , snapHistory = Map.map Set.toList (mHistory m)
    , snapStatus = case mStatus m of
        Running -> Nothing
        Finished o -> Just (toJSON o)
    }
 where
  chart = ciChart impl

{- | A structural hash of the chart (FNV-1a 64 over a canonical
rendering): states, kinds, hierarchy, transitions, events. Changes when
the chart's shape changes; stored in every snapshot so 'restore' can tell
\"stale snapshot\" apart from \"corrupt snapshot\".
-}
chartFingerprint :: RChart -> Text
chartFingerprint chart = hex64 (fnv1a64 (TE.encodeUtf8 canonical))
 where
  canonical = T.intercalate "\n" (chartLines chart)

chartLines :: RChart -> [Text]
chartLines chart =
  ("chart " <> rcName chart)
    : ("initial " <> rcInitial chart)
    : map eventLine (sortOn fst (rcEvents chart))
      ++ concatMap nodeLines (sortOn rnName (Map.elems (rcNodes chart)))
      ++ map (transLine rootName) (rcRootTransitions chart)
      ++ map invokeLine (rcRootInvokes chart)
      ++ map ("rootentry " <>) (rcRootEntry chart)
 where
  eventLine (n, ty) = "event " <> n <> " : " <> ty
  nodeLines n =
    ( "state "
        <> rnName n
        <> " kind="
        <> kindText (rnKind n)
        <> " parent="
        <> rnParent n
        <> " children="
        <> T.intercalate "," (rnChildren n)
        <> " entry="
        <> T.intercalate "," (rnEntry n)
        <> " exit="
        <> T.intercalate "," (rnExit n)
        <> " donedata="
        <> fromMaybe "-" (rnDoneData n)
    )
      : map (transLine (rnName n)) (rnTransitions n)
        ++ map invokeLine (rnInvokes n)
  kindText = \case
    RAtomic -> "atomic"
    RCompound i -> "compound:" <> i
    RParallel -> "parallel"
    RFinal -> "final"
    RHistory Shallow d -> "history:shallow:" <> fromMaybe "-" d
    RHistory Deep d -> "history:deep:" <> fromMaybe "-" d
  transLine src t =
    "trans "
      <> src
      <> " #"
      <> tshow (rtIndex t)
      <> " "
      <> triggerText (rtTrigger t)
      <> " guard="
      <> fromMaybe "-" (rtGuard t)
      <> " targets="
      <> T.intercalate "," (rtTargets t)
      <> " actions="
      <> T.intercalate "," (rtActions t)
      <> (if rtInternal t then " internal" else "")
  triggerText = \case
    TOn e -> "on:" <> e
    TWildcard -> "wildcard"
    TAlways -> "always"
    TAfter ms -> "after:" <> tshow ms
    TDone s -> "done:" <> s
    TInvokeDone i -> "invokedone:" <> i
    TInvokeError i -> "invokeerror:" <> i
  invokeLine iv = "invoke " <> riId iv <> " src=" <> riSrc iv
  tshow = T.pack . show

fnv1a64 :: BS.ByteString -> Word64
fnv1a64 = BS.foldl' go 14695981039346656037
 where
  go h b = (h `xor` fromIntegral b) * 1099511628211

hex64 :: Word64 -> Text
hex64 w = T.pack (pad (showHex w ""))
 where
  pad s = replicate (16 - length s) '0' <> s

{-------------------------------------------------------------------------------
  Restoring
-------------------------------------------------------------------------------}

-- | A successfully restored machine.
data Restored (spec :: ChartSpec) = Restored
  { restoredMachine :: Machine spec
  , restoredWarnings :: [RestoreWarning]
  , restoredEffects :: [EffectReq]
  -- ^ Re-arm requests for the restored configuration's timers and
  -- invocations (empty for finished machines). Hand these to the
  -- interpreter exactly as you would a step's effects.
  }

-- | Non-fatal observations made during restore.
data RestoreWarning
  = -- | The snapshot was taken by a chart with a different structure.
    -- Everything still validated; treat as informational (or reject, if
    -- your application demands exact-version snapshots).
    FingerprintChanged
      { rwStored :: Text
      , rwCurrent :: Text
      }
  | -- | A history entry referred to states that no longer exist and was
    -- dropped or filtered.
    DroppedHistory NodeName [NodeName]
  deriving stock (Show, Eq)

-- | Why a snapshot could not be restored. Every constructor says whether
-- the stored fingerprint matched the current chart — the difference
-- between \"stale snapshot after a deploy\" and \"corrupt data\".
data RestoreError
  = -- | The snapshot belongs to a different chart altogether.
    WrongChart {reExpected :: Text, reGot :: Text}
  | -- | Snapshot format version this library does not read.
    UnsupportedVersion Int
  | -- | Configuration members that are not states of the current chart.
    UnknownStates
      { reUnknown :: [NodeName]
      , reKnown :: [NodeName]
      , reFingerprintMatched :: Bool
      }
  | -- | The states all exist but do not form a legal configuration.
    IllegalConfiguration {reReason :: Text, reFingerprintMatched :: Bool}
  | -- | The stored context does not parse as the chart's context type.
    BadContext {reParseError :: String, reFingerprintMatched :: Bool}
  | -- | The stored final output does not parse as the chart's output type.
    BadOutput {reParseError :: String, reFingerprintMatched :: Bool}
  | -- | A 'Recovery' action itself failed.
    RecoveryFailed Text
  deriving stock (Show, Eq)

-- | Validate a snapshot against the current chart and rebuild the
-- machine. See the module header for the timer\/invocation semantics.
restore ::
  forall m spec.
  (FromJSON (Ctx spec), FromJSON (Output spec)) =>
  ChartImpl m spec ->
  Snapshot ->
  Either RestoreError (Restored spec)
restore impl snap = do
  checkVersion
  checkChart
  checkKnown
  checkCoherent
  ctxOrOut <- parseState
  let (hist, histWarnings) = sanitizeHistory chart (snapHistory snap)
      machine =
        Machine
          { mConfig = cfg
          , mCtx = fst ctxOrOut
          , mHistory = hist
          , mStatus = snd ctxOrOut
          }
      effects = case snd ctxOrOut of
        Finished _ -> []
        Running ->
          concatMap (armsFor chart) (Set.toList cfg)
            ++ map (\iv -> ReqStartInvoke (riId iv) (riSrc iv) rootName) (rcRootInvokes chart)
  pure
    Restored
      { restoredMachine = machine
      , restoredWarnings = fingerprintWarnings ++ histWarnings
      , restoredEffects = effects
      }
 where
  chart = ciChart impl
  cfg = Set.fromList (snapConfig snap)
  fpMatched = snapFingerprint snap == chartFingerprint chart
  fingerprintWarnings
    | fpMatched = []
    | otherwise = [FingerprintChanged (snapFingerprint snap) (chartFingerprint chart)]
  checkVersion
    | snapVersion snap == 1 = Right ()
    | otherwise = Left (UnsupportedVersion (snapVersion snap))
  checkChart
    | snapChart snap == rcName chart = Right ()
    | otherwise = Left (WrongChart (rcName chart) (snapChart snap))
  checkKnown =
    case filter (isNothing . lookupNode chart) (snapConfig snap) of
      [] -> Right ()
      unknown ->
        Left
          UnknownStates
            { reUnknown = unknown
            , reKnown = Map.keys (rcNodes chart)
            , reFingerprintMatched = fpMatched
            }
  checkCoherent =
    case configurationFault chart cfg of
      Nothing -> Right ()
      Just reason -> Left (IllegalConfiguration reason fpMatched)
  parseState = do
    ctx <- parseAs (`BadContext` fpMatched) (snapContext snap)
    st <- case snapStatus snap of
      Nothing -> Right Running
      Just v -> Finished <$> parseAs (`BadOutput` fpMatched) v
    pure (ctx, st)
  parseAs :: (FromJSON a) => (String -> RestoreError) -> Value -> Either RestoreError a
  parseAs mk v = either (Left . mk) Right (Aeson.parseEither parseJSON v)

-- | Why a set of (existing) states is not a legal configuration, if it
-- isn't.
configurationFault :: RChart -> Set NodeName -> Maybe Text
configurationFault chart cfg
  | Set.null cfg = Just "empty configuration"
  | otherwise =
      listToMaybe
        ( maybeToList checkTopLevel
            ++ ancestorsPresent
            ++ noHistoryMembers
            ++ compoundChildren
            ++ parallelRegions
        )
 where
  members = Set.toList cfg
  checkTopLevel =
    case filter (maybe False ((== rootName) . rnParent) . lookupNode chart) members of
      [_] -> Nothing
      tops -> Just ("expected exactly one active top-level state, got " <> tshow tops)
  ancestorsPresent =
    mapMaybe
      ( \n ->
          case filter (\a -> a /= rootName && not (Set.member a cfg)) (properAncestors chart n) of
            [] -> Nothing
            missing -> Just ("ancestors of " <> n <> " missing from configuration: " <> tshow missing)
      )
      members
  noHistoryMembers =
    mapMaybe
      (\n -> if isHistory chart n then Just ("history pseudo-state " <> n <> " cannot be active") else Nothing)
      members
  compoundChildren =
    mapMaybe
      ( \n ->
          if not (isCompound chart n)
            then Nothing
            else case filter (`Set.member` cfg) (childrenOf chart n) of
              [_] -> Nothing
              active -> Just ("compound " <> n <> " must have exactly one active child, got " <> tshow active)
      )
      members
  parallelRegions =
    mapMaybe
      ( \n ->
          if not (isParallel chart n)
            then Nothing
            else case filter (\r -> not (isHistory chart r) && not (Set.member r cfg)) (childrenOf chart n) of
              [] -> Nothing
              missing -> Just ("parallel " <> n <> " is missing active regions: " <> tshow missing)
      )
      members
  tshow :: (Show a) => a -> Text
  tshow = T.pack . show

-- | Keep only history entries whose key and members still exist; report
-- what was dropped.
sanitizeHistory ::
  RChart ->
  Map NodeName [NodeName] ->
  (Map NodeName (Set NodeName), [RestoreWarning])
sanitizeHistory chart = Map.foldrWithKey go (Map.empty, [])
 where
  go h stored (acc, warns)
    | not (isHistory chart h) = (acc, DroppedHistory h stored : warns)
    | otherwise =
        let (known, unknown) = span' (isNothing . lookupNode chart) stored
         in if null unknown
              then (Map.insert h (Set.fromList known) acc, warns)
              else
                ( if null known then acc else Map.insert h (Set.fromList known) acc
                , DroppedHistory h unknown : warns
                )
  span' p xs = (filter (not . p) xs, filter p xs)

{-------------------------------------------------------------------------------
  Recovery strategies
-------------------------------------------------------------------------------}

-- | What to do instead of failing.
data RecoveryAction (spec :: ChartSpec)
  = -- | Discard the snapshot and initialize the machine fresh with this
    -- context (root and initial entry actions run; invocations start).
    Restart (Ctx spec)
  | -- | Place the machine at the completion of the given states (initial
    -- children and parallel regions are filled in), with this context and
    -- no history. The states must exist in the current chart.
    ResumeAt [NodeName] (Ctx spec)

{- | Per-failure-mode recovery hooks. Each receives the failure's details
and the raw snapshot; 'Nothing' means \"do not recover — fail with the
original error\".
-}
data Recovery (spec :: ChartSpec) = Recovery
  { onWrongChart :: Text -> Snapshot -> Maybe (RecoveryAction spec)
  , onUnknownStates :: [NodeName] -> Snapshot -> Maybe (RecoveryAction spec)
  , onIllegalConfiguration :: Text -> Snapshot -> Maybe (RecoveryAction spec)
  , onBadContext :: String -> Snapshot -> Maybe (RecoveryAction spec)
  , onBadOutput :: String -> Snapshot -> Maybe (RecoveryAction spec)
  }

-- | Recover nothing; equivalent to plain 'restore'.
noRecovery :: Recovery spec
noRecovery =
  Recovery
    { onWrongChart = \_ _ -> Nothing
    , onUnknownStates = \_ _ -> Nothing
    , onIllegalConfiguration = \_ _ -> Nothing
    , onBadContext = \_ _ -> Nothing
    , onBadOutput = \_ _ -> Nothing
    }

-- | Recover every failure by restarting fresh with the given context —
-- the \"my old state is worthless, just boot\" policy.
restartRecovery :: Ctx spec -> Recovery spec
restartRecovery ctx =
  Recovery
    { onWrongChart = \_ _ -> Just (Restart ctx)
    , onUnknownStates = \_ _ -> Just (Restart ctx)
    , onIllegalConfiguration = \_ _ -> Just (Restart ctx)
    , onBadContext = \_ _ -> Just (Restart ctx)
    , onBadOutput = \_ _ -> Just (Restart ctx)
    }

-- | How a 'restoreWith' concluded.
data RestoreOutcome m (spec :: ChartSpec)
  = -- | The snapshot restored cleanly.
    Intact (Restored spec)
  | -- | Restore failed with the given error, and a 'Restart' recovery
    -- produced this freshly initialized machine.
    RecoveredByRestart (Stepped spec) RestoreError
  | -- | Restore failed with the given error, and a 'ResumeAt' recovery
    -- produced this machine.
    RecoveredByResume (Restored spec) RestoreError

-- | 'restore', but consult 'Recovery' hooks before failing.
restoreWith ::
  forall m spec.
  (Monad m, FromJSON (Ctx spec), FromJSON (Output spec)) =>
  ChartImpl m spec ->
  Recovery spec ->
  Snapshot ->
  m (Either RestoreError (RestoreOutcome m spec))
restoreWith impl recovery snap =
  case restore impl snap of
    Right ok -> pure (Right (Intact ok))
    Left err -> case hookFor err of
      Nothing -> pure (Left err)
      Just action -> runRecovery err action
 where
  chart = ciChart impl
  hookFor err = case err of
    WrongChart _ got -> onWrongChart recovery got snap
    UnknownStates unknown _ _ -> onUnknownStates recovery unknown snap
    IllegalConfiguration reason _ -> onIllegalConfiguration recovery reason snap
    BadContext e _ -> onBadContext recovery e snap
    BadOutput e _ -> onBadOutput recovery e snap
    UnsupportedVersion _ -> Nothing
    RecoveryFailed _ -> Nothing
  runRecovery err = \case
    Restart ctx -> do
      r <- initialize impl ctx
      pure $ case r of
        Left fault -> Left (RecoveryFailed (T.pack ("restart failed: " <> show fault)))
        Right stepped -> Right (RecoveredByRestart stepped err)
    ResumeAt targets ctx ->
      case filter (isNothing . lookupNode chart) targets of
        unknown@(_ : _) ->
          pure (Left (RecoveryFailed ("ResumeAt targets do not exist: " <> T.pack (show unknown))))
        [] -> do
          let cfg = completionOf chart Map.empty targets
              machine =
                Machine
                  { mConfig = cfg
                  , mCtx = ctx
                  , mHistory = Map.empty
                  , mStatus = Running
                  }
              effects =
                concatMap (armsFor chart) (Set.toList cfg)
                  ++ map (\iv -> ReqStartInvoke (riId iv) (riSrc iv) rootName) (rcRootInvokes chart)
          case configurationFault chart cfg of
            Just reason -> pure (Left (RecoveryFailed ("ResumeAt yields illegal configuration: " <> reason)))
            Nothing ->
              pure
                ( Right
                    ( RecoveredByResume
                        Restored
                          { restoredMachine = machine
                          , restoredWarnings = []
                          , restoredEffects = effects
                          }
                        err
                    )
                )
