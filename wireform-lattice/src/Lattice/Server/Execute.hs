{- | The per-slice Lattice executor: round-batched BFS traversal of a
compiled 'Plan' against a 'Backend', producing wire records, surrogate
keys, and the manifest fact set (spec §6.6, §8.1, §9).

Execution follows the pinned origin design:

* __Rounds.__ Traversal is breadth-first by depth. Each round loads every
  newly reached entity key with ONE 'beLoad' call per type and resolves
  every @has many@ edge occurrence with ONE 'beChildren' call per
  (parent type, edge field, bound window). N+1 is unrepresentable.
* __Slice-rank rules.__ With @rank(pub)=0 < rank(ctx)=1 < rank(priv)=2@:
  a root or edge is /traversed/ iff its membership level's rank is @<=@
  the response's rank; membership (manifest root map, page\/array fields)
  and scalar fields are /emitted/ only per the response's 'EmitMode' —
  @'EmitEq' s@ (query slices: a fact belongs to exactly one slice) or
  @'EmitAtMost' s@ (point fetches and mutation output selections: all
  facts at or below the caller's level).
* __Visibility at emission.__ A field or edge whose own policy is
  @RequiresClaims@ evaluates every predicate against the presented claims
  and the loaded row ('RhsField' compares by canonical JSON against the
  row of the entity declaring the policy — the field's own row, or the
  edge's parent row). A false field predicate suppresses the field; an
  entity all of whose candidate fields were suppressed this way emits
  @{"kind":"elided"}@ instead. A false edge predicate vanishes the edge
  (no field, no traversal). Root policies are checked against claims
  only ('RhsField' root predicates are treated as passing: row-level
  membership filtering is the backend's business through its loader
  arguments — documented simplification).
* __Dedup.__ An entity reached along several paths emits one record with
  the union of its emitted fields; rows are loaded at most once per
  response (first round to reach a key wins, which also gives
  within-response read stability).
* __@\@depth(n)@.__ A depth edge's 'peSelection' is one cyclically tied
  expansion level; the executor carries per-path fuel (field-keyed) and
  stops traversing (and emitting) the recursive edge when the fuel is
  spent, so the innermost level omits it. The tie is never forced deeper
  than the fuel allows.
* __Failures.__ A 'BackendFailure' from 'beLoad' is an Entity-scoped
  error; from 'beChildren' an Edge-scoped error; from a root loader a
  Root-scoped error. 'RowAbsent' reached via an edge emits nothing (the
  ref dangles) — except through a required @has one@ ('relOptional'
  'False'), where the dangle is a non-retryable Edge-scoped
  @lattice:cardinality@ error (§3.4); required targets are
  existence-probed even when the edge's selection is empty (point-fetch
  masks, mutation output). A bounded collection producing fewer than its
  declared @min@ items reports @lattice:collection-underflow@, the mirror
  of overflow: what exists still emits (§3.6). 'RowTombstone' emits a
  tombstone record. Exceeding 'maxRoundFanout' at runtime truncates that
  scope's children and reports a non-retryable Edge\/Root-scoped
  @lattice:internal@ error.
  An exception escaping mid-execution marks the result incomplete
  ('xrComplete' 'False'); the caller still renders what was produced.
* __Surrogate keys.__ @Type:key@ for every reached (loaded or attempted)
  entity; @{collection}:{grouping}@ for every scanned collection
  instantiated with the actual grouping values (edges: the grouping
  default is the link field, whose value is the parent's key component;
  a grouped-by override reads the parent row, then the bound arguments,
  then falls back to @\"\"@ — the helper is total). Members reached
  through a collection scan are recorded in 'xrCovered' for the caller's
  key-budget coarsening.

* __Derived fields (§3.7).__ A field with an @on read@ 'Derivation' never
  reads the row: after a round's jobs emit, every requested (type, field)
  resolves as one batch — @ViaEdge@ deps load through 'beLoad' (one call
  per target type, rows shared with the visible traversal), @ViaCollection@
  deps through 'beAggregate' (one call per (collection, aggregate)), and
  the assembled 'DepValues' feed one 'beDerive' call. Hidden loads count
  against the round fan-out budget; exhaustion degrades the response
  (unscoped @lattice:internal@, all derived fields elided). A failed dep
  load\/aggregate is a Field-scoped error on the owning entity's derived
  field; a key absent from 'beDerive''s result elides the field silently
  (parity with 'beComputed'). @maintained@ fields read from the row like
  any stored field.

  __Read-set keys:__ every attempted @ViaEdge@ dep contributes its entity
  key and every @ViaCollection@ dep its instantiated collection tag to the
  response's surrogate keys ('xrEntityKeys'\/'xrCollectionKeys').

  __Witness (§3.7 Validators), exact bytes:__ each @ViaEdge@ dep yields
  @[\"e\", \"Type:key\", ver]@ (tombstone version as-is, @\"\"@ for an
  absent row); each aggregate result yields
  @[\"a\", \"{collection}:{grouping}\", valueHash]@ where
  @valueHash = base64url(BLAKE3(canonicalJson(value))[0..11])@ unpadded.
  'witnessValue' is the canonical-JSON array of those triples sorted
  ascending as (tag, id\/key, ver\/hash) string triples. A point fetch
  touching derived fields uses
  @ETag: \"w:\" <> base64url(BLAKE3(utf8(ver) || 0x00 ||
  canonicalJson(witnessValue))[0..11])@ ('witnessEtag') instead of the
  bare @ver@; query manifests fold 'witnessValue' into the etag input
  (see 'Lattice.Server.manifestEtag').

Cursor problems abort the whole request ('AbortCursorRetired' → 410,
'AbortCursorMalformed' → 400) per spec §10.8.
-}
module Lattice.Server.Execute (
  -- * Environment and modes
  ExecEnv (..),
  EmitMode (..),
  sliceRank,
  levelRank,

  -- * Running
  ExecAbort (..),
  ExecResult (..),
  executeRoots,
  executeSeeds,

  -- * Derived-field witness (§3.7)
  WitnessEntry (..),
  witnessValue,
  witnessEtag,
  deriveGroupKey,

  -- * Selection builders and runtime binding helpers
  outputSelection,
  bindRuntimeArgs,
  argMapOf,
  runtimeKey,
  evalPredicates,
  claimOnlyPredicates,
  dedupOrd,
) where

import Control.Exception (
  Exception,
  SomeAsyncException (..),
  SomeException,
  fromException,
  handle,
  throwIO,
  try,
 )
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.Foldable (for_, toList, traverse_)
import Data.IORef
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Scientific qualified as Sci
import Control.Monad (when)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lattice.Backend
import Lattice.Canonical (canonicalFieldKey)
import Lattice.Hash (b64url, blake3)
import Lattice.Cursor (CursorError (..), decodeCursor)
import Lattice.Plan
import Lattice.Query.AST (fName, selectionFields)
import Lattice.Query.Validate (isNodesRootDef)
import Lattice.Schema
import Lattice.Types
import Lattice.Telemetry (
  LatticeTelemetry,
  errorEvent,
  intA,
  recordLoaderBatch,
  telemetryEnabled,
  txtA,
  withLatticeSpan,
 )
import Lattice.Telemetry qualified as Tel
import Lattice.Value (canonicalJson, qvalueToJson, renderScalarKey)
import Lattice.Wire
import Numeric.Natural (Natural)


-- ---------------------------------------------------------------------------
-- Environment
-- ---------------------------------------------------------------------------

data ExecEnv = ExecEnv
  { xSchema :: Schema
  , xBudgets :: Budgets
  , xBackend :: Backend
  , xClaims :: Claims
  -- ^ Presented claims (empty when none were presented).
  , xVars :: Map VarName A.Value
  -- ^ Bound query variables (absent optional variables are simply missing).
  , xMode :: EmitMode
  , xTelemetry :: LatticeTelemetry
  -- ^ §19 instrumentation; 'Lattice.Telemetry.noTelemetry' when off.
  , xProjections :: Map TypeName Projection
  -- ^ Per-type load projections ('Lattice.Plan.planProjections'): the
  -- static upper bound on the row fields this execution reads, handed to
  -- every 'beLoad'. Types absent from the map load 'ProjectAll' — pass
  -- 'Map.empty' where the whole visible entity renders by design (point
  -- fetches, mutation output).
  }


-- | The 'beLoad' projection for one type ('ProjectAll' when unmapped).
projectionOf :: ExecEnv -> TypeName -> Projection
projectionOf env ty = Map.findWithDefault ProjectAll ty (xProjections env)


{- | What emits. Traversal is always rank-bounded; emission is either
slice-exact (query data slices) or at-most (point fetches, mutation
output).
-}
data EmitMode
  = EmitEq SliceName
  | EmitAtMost SliceName
  deriving stock (Eq, Show)


sliceRank :: SliceName -> Int
sliceRank = \case
  SlicePub -> 0
  SliceCtx -> 1
  SlicePriv -> 2
  SlicePlan -> 0


levelRank :: Level -> Int
levelRank = sliceRank . sliceOfLevel


modeRank :: EmitMode -> Int
modeRank = \case
  EmitEq s -> sliceRank s
  EmitAtMost s -> sliceRank s


emitsAt :: EmitMode -> Level -> Bool
emitsAt m l = case m of
  EmitEq s -> sliceOfLevel l == s
  EmitAtMost s -> levelRank l <= sliceRank s


traversesAt :: EmitMode -> Level -> Bool
traversesAt m l = levelRank l <= modeRank m


-- ---------------------------------------------------------------------------
-- Results and aborts
-- ---------------------------------------------------------------------------

-- | Whole-request aborts discovered during execution (spec §10.8).
data ExecAbort
  = -- | A presented cursor's spec hash no longer matches: @410 lattice:cursor-retired@.
    AbortCursorRetired
  | -- | A presented cursor failed to decode: @400@.
    AbortCursorMalformed
  | -- | A request-time budget violation — a variable-bound @nodes@ @refs@
    -- list over 'maxRoundFanout' (§14.4): @400@, the ordinary budget
    -- rejection (the literal-list form rejects at compile time instead).
    AbortBudget Text
  deriving stock (Eq, Show)


newtype ExecAbortEx = ExecAbortEx ExecAbort
  deriving stock (Show)


instance Exception ExecAbortEx


data ExecResult = ExecResult
  { xrRecords :: [Record]
  -- ^ @entity@\/@tombstone@\/@elided@ records in first-reach order,
  -- followed by every scoped @error@ record. No manifest, no end record —
  -- the caller frames the stream.
  , xrRootMap :: Map Text [Ref]
  -- ^ Ordered per-root ref lists for roots whose membership emitted.
  , xrIdVers :: [(Text, Text)]
  -- ^ Sorted @(id, ver)@ pairs of every emitted record, tombstone versions
  -- included, elisions with an empty version — the manifest-etag fact set.
  , xrEntityKeys :: Set SurrogateKey
  -- ^ @Type:key@ for every reached entity, hidden derived-field deps
  -- included (§3.7 Invalidation).
  , xrCollectionKeys :: [SurrogateKey]
  -- ^ Scanned collections, instantiated, in scan order (deduplicated) —
  -- including derived aggregates' collection tags.
  , xrCovered :: Set SurrogateKey
  -- ^ Entity keys reached through a collection scan (coarsening candidates).
  , xrWitness :: Set WitnessEntry
  -- ^ The derived-value witness (§3.7 Validators); empty when the
  -- response touched no @on read@ derived field.
  , xrDegraded :: Bool
  -- ^ Any error record present.
  , xrComplete :: Bool
  -- ^ 'False' only when an exception truncated execution.
  }


{- | One witness entry (§3.7 Validators): an edge dep's identity or an
aggregate result's value hash. See the module haddock for the exact
rendered bytes.
-}
data WitnessEntry
  = -- | @ViaEdge@ dep: the dep entity and its version (tombstone version
    -- as stored; @\"\"@ for an absent row).
    WitnessEdge Ref Text
  | -- | @ViaCollection@ dep: the instantiated collection tag and the
    -- 12-byte-truncated BLAKE3 of the aggregate value's canonical JSON.
    WitnessAgg SurrogateKey Text
  deriving stock (Eq, Ord, Show)


-- | The canonical JSON rendering of a witness: sorted string triples.
witnessValue :: Set WitnessEntry -> A.Value
witnessValue w = A.toJSON (sort (map triple (Set.toList w)))
  where
    triple :: WitnessEntry -> [Text]
    triple = \case
      WitnessEdge ref ver -> ["e", renderRef ref, ver]
      WitnessAgg key h -> ["a", key, h]


{- | The point-fetch validator over a row version and a nonempty witness:
@\"w:\" <> base64url(BLAKE3(utf8(ver) || 0x00 ||
canonicalJson(witnessValue))[0..11])@.
-}
witnessEtag :: Text -> Set WitnessEntry -> Text
witnessEtag ver w =
  "w:"
    <> b64url
      (BS.take 12 (blake3 (TE.encodeUtf8 ver <> BS.singleton 0 <> canonicalJson (witnessValue w))))


-- | @valueHash@ of one aggregate result (module haddock).
aggValueHash :: A.Value -> Text
aggValueHash v = b64url (BS.take 12 (blake3 (canonicalJson v)))


-- ---------------------------------------------------------------------------
-- Engine state
-- ---------------------------------------------------------------------------

data EntAcc = EntAcc
  { eaVer :: Text
  , eaFields :: Map Text A.Value
  , eaSuppressed :: Bool
  }


data St = St
  { stRows :: Map Ref (Either BackendFailure LoadResult)
  , stOrder :: [Ref]
  -- ^ Reversed first-reach order of emission-relevant refs.
  , stEnts :: Map Ref EntAcc
  , stTombs :: Map Ref Text
  , stErrs :: [ErrorRecord]
  -- ^ Reversed.
  , stEntityKeys :: Set SurrogateKey
  , stCollKeys :: [SurrogateKey]
  -- ^ Reversed.
  , stCovered :: Set SurrogateKey
  , stPendingOne :: [(Ref, FieldName, Ref)]
  -- ^ Required to-one obligations, reversed: (parent, edge field, target).
  , stRootMap :: Map Text [Ref]
  , stWitness :: Set WitnessEntry
  , stIncomplete :: Bool
  , stPendingNodes :: [(Text, Bool, [(Ref, Level, Bool)])]
  -- ^ Implicit @nodes@ roots awaiting their post-round manifest fixup
  -- (§14.4): root name, whether the root-map key itself emits at this
  -- slice, and the admitted refs in request order with their per-type
  -- membership levels and whether their gate is row-comparing. The entry
  -- filters to refs that actually resolved and passed their row gate
  -- ('fixupNodesRoots') — absent entries are simply missing.
  , stNodesDenied :: Set Ref
  -- ^ @nodes@ refs whose loaded row failed the type's row-comparing
  -- @fetch by@ gate (§14.4): dropped from the manifest entry, and their
  -- tombstones scrubbed — indistinguishable from nonexistence.
  }


emptySt :: St
emptySt =
  St
    { stRows = Map.empty
    , stOrder = []
    , stEnts = Map.empty
    , stTombs = Map.empty
    , stErrs = []
    , stEntityKeys = Set.empty
    , stCollKeys = []
    , stCovered = Set.empty
    , stPendingOne = []
    , stRootMap = Map.empty
    , stWitness = Set.empty
    , stIncomplete = False
    , stPendingNodes = []
    , stNodesDenied = Set.empty
    }


-- | One unit of per-entity work: apply this node selection to this ref.
data Job = Job
  { jRef :: Ref
  , jNode :: NodeSelection
  , jFuel :: Map FieldName Int
  -- ^ Remaining @\@depth@ fuel along this path, keyed by edge field.
  , jGate :: Maybe Policy
  -- ^ A row-aware admission gate evaluated against the loaded row before
  -- anything of this job emits or traverses — the @nodes@ root's
  -- row-comparing @fetch by@ predicates (§14.4). Per job, not per ref: the
  -- same entity reached through an ordinary edge in the same response is
  -- governed by that path's own policies. 'Nothing' everywhere else.
  }


-- | One @has many@ edge occurrence grouped across a round's parents.
data EdgeTask = EdgeTask
  { etEdge :: PlanEdge
  , etCol :: CollectionDef
  , etKey :: Text
  , etWindow :: Window
  , etParents :: [(Ref, EntityRow, Map FieldName Int)]
  -- ^ Reversed accumulation order.
  }


addError :: IORef St -> ErrorRecord -> IO ()
addError stRef e = modifyIORef' stRef (\s -> s {stErrs = e : stErrs s})


scopedError :: Maybe Scope -> BackendFailure -> ErrorRecord
scopedError scope bf =
  ErrorRecord
    { errScope = scope
    , errCode = Just (bfCode bf)
    , errDomain = Nothing
    , errRetryable = bfRetryable bf
    , errMessage = bfMessage bf
    }


-- | Register a ref in first-reach order (idempotent).
reach :: St -> Ref -> St
reach s ref
  | Map.member ref (stEnts s) || Map.member ref (stTombs s) = s
  | otherwise = s {stOrder = ref : stOrder s}


addFieldSt :: Ref -> Text -> Text -> A.Value -> St -> St
addFieldSt ref ver key val s0 =
  let s = reach s0 ref
      acc = fromMaybe (EntAcc ver Map.empty False) (Map.lookup ref (stEnts s))
  in s {stEnts = Map.insert ref acc {eaFields = Map.insert key val (eaFields acc)} (stEnts s)}


suppressSt :: Ref -> Text -> St -> St
suppressSt ref ver s0 =
  let s = reach s0 ref
      acc = fromMaybe (EntAcc ver Map.empty False) (Map.lookup ref (stEnts s))
  in s {stEnts = Map.insert ref acc {eaSuppressed = True} (stEnts s)}


tombSt :: Ref -> Text -> St -> St
tombSt ref ver s0 =
  let s = reach s0 ref
  in s {stTombs = Map.insert ref ver (stTombs s)}


-- ---------------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------------

-- | Execute a compiled plan's roots (query data slices, refs projections).
executeRoots :: ExecEnv -> Map RootName PlanRoot -> IO (Either ExecAbort ExecResult)
executeRoots env roots = runEngine env $ \stRef counter ->
  concat <$> traverse (rootJobs env stRef counter) (Map.toAscList roots)


{- | Execute from seed entities with prebuilt node selections (point
fetches, mutation output rendering). Seeds whose selections carry empty
'peSelection's never traverse past the seed row.
-}
executeSeeds :: ExecEnv -> [(Ref, NodeSelection)] -> IO (Either ExecAbort ExecResult)
executeSeeds env seeds = runEngine env $ \_ _ ->
  pure (map (\(r, n) -> Job r n Map.empty Nothing) seeds)


{- | Run the engine inside a @lattice.execute@ span (§19.2: one per slice
execution) attributed with @lattice.slice@; each scoped error record also
becomes a @lattice.error@ span EVENT (scope @$tag@ + code, never the id).
-}
runEngine :: ExecEnv -> (IORef St -> IORef Int -> IO [Job]) -> IO (Either ExecAbort ExecResult)
runEngine env seed =
  withLatticeSpan (xTelemetry env) "lattice.execute" Tel.Internal [] execAttrs $ \msp -> do
    stRef <- newIORef emptySt
    outcome <- try $ handle (onCrash stRef) $ do
      counter <- newIORef (0 :: Int)
      jobs0 <- seed stRef counter
      rounds env stRef 0 (fromIntegral (maxDepth (xBudgets env)) + 2) jobs0
      checkCardinality stRef
      fixupNodesRoots env stRef
    case outcome of
      Left (ExecAbortEx a) -> pure (Left a)
      Right () -> do
        st <- readIORef stRef
        when (telemetryEnabled (xTelemetry env)) $
          for_ (stErrs st) $ \er ->
            errorEvent msp (scopeTag <$> errScope er) (errCode er)
        pure (Right (finalize st))
  where
    execAttrs =
      [("lattice.slice", txtA (renderSlice modeSlice))]
    modeSlice = case xMode env of
      EmitEq s -> s
      EmitAtMost s -> s
    scopeTag = \case
      ScopeEntity {} -> "Entity"
      ScopeField {} -> "Field"
      ScopeEdge {} -> "Edge"
      ScopeRoot {} -> "Root"
      ScopeItem {} -> "Item"
      ScopeUnknown t _ -> t
    onCrash :: IORef St -> SomeException -> IO ()
    onCrash stRef e
      | Just ExecAbortEx {} <- fromException e = throwIO e
      | Just SomeAsyncException {} <- fromException e = throwIO e
      | otherwise = do
          modifyIORef' stRef (\s -> s {stIncomplete = True})
          addError stRef $
            ErrorRecord
              { errScope = Nothing
              , errCode = Just "lattice:internal"
              , errDomain = Nothing
              , errRetryable = False
              , errMessage = Just "execution aborted by exception"
              }


finalize :: St -> ExecResult
finalize st =
  ExecResult
    { xrRecords = recs <> map RError (reverse (stErrs st))
    , xrRootMap = stRootMap st
    , xrIdVers = sort (mapMaybe idver recs)
    , xrEntityKeys = stEntityKeys st
    , xrCollectionKeys = dedupOrd (reverse (stCollKeys st))
    , xrCovered = stCovered st
    , xrWitness = stWitness st
    , xrDegraded = not (null (stErrs st))
    , xrComplete = not (stIncomplete st)
    }
  where
    recs = mapMaybe recOf (reverse (stOrder st))
    recOf ref
      | Just v <- Map.lookup ref (stTombs st) = Just (RTombstone ref v Nothing)
      | Just ea <- Map.lookup ref (stEnts st) =
          if Map.null (eaFields ea)
            then if eaSuppressed ea then Just (RElided ref) else Nothing
            else Just (REntity (EntityRecord ref (eaVer ea) (eaFields ea) Nothing Nothing))
      | otherwise = Nothing
    idver = \case
      RTombstone ref v _ -> Just (renderRef ref, v)
      RElided ref -> Just (renderRef ref, "")
      REntity er -> Just (renderRef (erId er), erVer er)
      _ -> Nothing


-- | Order-preserving deduplication.
dedupOrd :: (Ord a) => [a] -> [a]
dedupOrd = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs


-- ---------------------------------------------------------------------------
-- Roots
-- ---------------------------------------------------------------------------

rootJobs :: ExecEnv -> IORef St -> IORef Int -> (RootName, PlanRoot) -> IO [Job]
rootJobs env stRef counter (name, pr)
  | unRootName name == "nodes" && isNodesRootDef (prDef pr) = nodesRootJobs env stRef counter name pr
  | not (traversesAt (xMode env) (prLevel pr)) = pure []
  | not (claimOnlyPredicates (xClaims env) (rootPolicy (prDef pr))) = pure []
  | otherwise = do
      let bound = bindRuntimeArgs (xVars env) (prArgs pr)
          grouping = filter (\(n, _) -> n `notElem` paginationArgNames) bound
      refs <- case rootKind (prDef pr) of
        RootGet ->
          beGetRoot (xBackend env) name (argMapOf grouping) >>= \case
            Left bf -> do
              addError stRef (scopedError (Just (ScopeRoot name)) bf)
              pure []
            Right mref -> pure (maybeToList mref)
        RootList -> case rootCollection (prDef pr) of
          Nothing -> pure []
          Just col -> do
            win <- abortEither (windowFor col bound)
            beListRoot (xBackend env) name (argMapOf grouping) win >>= \case
              Left bf -> do
                addError stRef (scopedError (Just (ScopeRoot name)) bf)
                pure []
              Right page -> do
                _ <- overflowError stRef (Just (ScopeRoot name)) Nothing col page
                underflowError stRef (Just (ScopeRoot name)) col page
                modifyIORef' stRef $ \s ->
                  markCovered (pageRefs page) s {stCollKeys = rootCollectionKey col bound : stCollKeys s}
                pure (pageRefs page)
      when' (emitsAt (xMode env) (prLevel pr)) $
        modifyIORef' stRef (\s -> s {stRootMap = Map.insert (unRootName name) refs (stRootMap s)})
      enqueue env stRef counter (ScopeRoot name) $
        mapMaybe (\r -> (\n -> Job r n Map.empty Nothing) <$> nodeFor (prSelection pr) (refType r)) refs
  where
    when' b act = if b then act else pure ()


{- | The implicit @nodes@ root (§14.4): refs come from the bound @refs@
argument rather than a backend loader, gated per type by @fetch by@.

* A variable-bound list longer than 'maxRoundFanout' aborts the request
  ('AbortBudget'); the literal form was already rejected at compile time.
* Malformed refs, unknown types, forbidden types ('entityFetchBy' absent),
  and claim-value gate failures all EMIT NOTHING for that ref — no record,
  no error, no root-map entry: indistinguishable from nonexistence.
* A @fetch by@ whose predicates compare against row fields ('RhsField')
  cannot be decided before the load; the policy rides the job as 'jGate'
  and 'processJob' evaluates it against the loaded row — a failing row
  emits nothing (recorded in 'stNodesDenied' so the manifest entry drops
  too), and a tombstoned row, having no fields to compare, is treated as
  denied ('fixupNodesRoots' suppresses the tombstone record). This is
  stricter than declared roots' claims-only membership check, and
  deliberately so: for @nodes@ the row IS the membership fact.
* Admitted refs become ordinary jobs — 'loadRound' batches them one
  'beLoad' per type per round — with the per-type node selection of the
  interface-dispatch machinery; an admitted type the selection does not
  list still loads (existence is verified before the ref may enter the
  manifest) under an empty selection.
* The manifest root entry is deferred to 'fixupNodesRoots': request
  order, filtered to refs that resolved (found, tombstoned, or a scoped
  load failure — never 'RowAbsent'), each at its type's membership slice.
-}
nodesRootJobs :: ExecEnv -> IORef St -> IORef Int -> RootName -> PlanRoot -> IO [Job]
nodesRootJobs env stRef counter name pr = do
  let schema = xSchema env
      bound = bindRuntimeArgs (xVars env) (prArgs pr)
      raw = case lookup "refs" bound of
        Just (A.Array xs) -> [s | A.String s <- toList xs]
        _ -> []
      cap = fromIntegral (maxRoundFanout (xBudgets env)) :: Int
      gateOf r = do
        ent <- lookupEntity schema (refType r)
        pol <- entityFetchBy ent
        pure (pol, policyLevel pol)
      admitted =
        [ (r, lvl, pol)
        | r <- dedupOrd (mapMaybe parseRef raw)
        , Just (pol, lvl) <- [gateOf r]
        , claimOnlyPredicates (xClaims env) pol
        , traversesAt (xMode env) lvl
        ]
      listedLevels =
        mapMaybe
          (\t -> policyLevel <$> (entityFetchBy =<< lookupEntity schema t))
          (Map.keys (tsPerType (prSelection pr)))
      keyEmits = any (emitsAt (xMode env)) listedLevels
  when (length raw > cap) . throwIO . ExecAbortEx . AbortBudget $
    "nodes refs list length "
      <> T.pack (show (length raw))
      <> " exceeds the origin's maxRoundFanout budget "
      <> T.pack (show cap)
  modifyIORef' stRef $ \s ->
    s
      { stPendingNodes =
          (unRootName name, keyEmits, [(r, lvl, policyRowDependent pol) | (r, lvl, pol) <- admitted])
            : stPendingNodes s
      }
  enqueue env stRef counter (ScopeRoot name) $
    map
      ( \(r, _, pol) ->
          Job
            { jRef = r
            , jNode = fromMaybe (NodeSelection [] []) (nodeFor (prSelection pr) (refType r))
            , jFuel = Map.empty
            , jGate = if policyRowDependent pol then Just pol else Nothing
            }
      )
      admitted


-- | Does a policy carry row-comparing ('RhsField') predicates?
policyRowDependent :: Policy -> Bool
policyRowDependent = \case
  RequiresClaims preds -> any rowDep preds
  Public -> False
  Private -> False
  where
    rowDep p = case cpRhs p of
      RhsField _ -> True
      _ -> False


{- | Resolve the deferred @nodes@ manifest entries (§14.4): request order,
refs whose membership level emits at this slice and whose row resolved —
found (and not row-gate denied), tombstoned (row-independent gates only:
a row-comparing gate has no row to pass), or a load failure (the scoped
error explains it; dropping it would make an outage look like
nonexistence). 'RowAbsent' and denied entries are simply missing. A
row-gate-denied tombstone is also scrubbed from the record stream. The
root-map key appears whenever any listed type's membership lands in this
slice (an all-absent request keeps its empty entry), or when a ref
emitted.
-}
fixupNodesRoots :: ExecEnv -> IORef St -> IO ()
fixupNodesRoots env stRef = do
  st0 <- readIORef stRef
  for_ (stPendingNodes st0) $ \(_, _, refs) ->
    for_ refs $ \(r, _, rowGated) ->
      case Map.lookup r (stRows st0) of
        Just (Right (RowTombstone _))
          | rowGated ->
              modifyIORef' stRef $ \s ->
                s
                  { stTombs = Map.delete r (stTombs s)
                  , stNodesDenied = Set.insert r (stNodesDenied s)
                  }
        _ -> pure ()
  st <- readIORef stRef
  for_ (stPendingNodes st) $ \(name, keyEmits, refs) -> do
    let present r = case Map.lookup r (stRows st) of
          _ | Set.member r (stNodesDenied st) -> False
          Just (Right (RowFound _)) -> True
          Just (Right (RowTombstone _)) -> True
          Just (Left _) -> True
          _ -> False
        emitRefs = [r | (r, lvl, _) <- refs, emitsAt (xMode env) lvl, present r]
    when (keyEmits || not (null emitRefs)) $
      modifyIORef' stRef (\s -> s {stRootMap = Map.insert name emitRefs (stRootMap s)})


rootCollectionKey :: CollectionDef -> [(ArgName, A.Value)] -> SurrogateKey
rootCollectionKey col args = collectionKey (colName col) (map gv (NE.toList (colGrouping col)))
  where
    gv g = maybe "" renderScalarKey (lookup (ArgName (unFieldName g)) args)


markCovered :: [Ref] -> St -> St
markCovered refs s =
  s {stCovered = foldr (Set.insert . entityKeyOf) (stCovered s) refs}


-- ---------------------------------------------------------------------------
-- Rounds
-- ---------------------------------------------------------------------------

rounds :: ExecEnv -> IORef St -> Int -> Int -> [Job] -> IO ()
rounds _ _ _ _ [] = pure ()
rounds env stRef ix fuel jobs
  | fuel <= 0 = do
      modifyIORef' stRef (\s -> s {stIncomplete = True})
      addError stRef $
        ErrorRecord Nothing (Just "lattice:internal") Nothing False (Just "round budget exhausted")
  | otherwise = do
      -- §19.2: one @lattice.round[i]@ span per traversal round; the next
      -- round's span is a sibling, so the recursion sits outside the span.
      kids <-
        withLatticeSpan
          (xTelemetry env)
          ("lattice.round[" <> T.pack (show ix) <> "]")
          Tel.Internal
          []
          [("lattice.round.index", intA ix)]
          $ \_ -> do
            loadRound env stRef jobs
            counter <- newIORef (0 :: Int)
            deriveRef <- newIORef Map.empty
            (toOneKids, tasks) <- processJobs env stRef counter deriveRef jobs
            edgeKids <- concat <$> traverse (runEdgeTask env stRef counter) (Map.toAscList tasks)
            runDeriveTasks env stRef counter =<< readIORef deriveRef
            pure (toOneKids <> edgeKids)
      rounds env stRef (ix + 1) (fuel - 1) kids


{- | One @lattice.load@ span per loader invocation (§19.2) — loader name
and batch size as attributes, never in the span name — with the
@lattice.loader.batch_size@ histogram recorded alongside.
-}
loaderSpan :: ExecEnv -> Text -> Int -> IO a -> IO a
loaderSpan env loader batch act = do
  recordLoaderBatch (xTelemetry env) loader batch
  withLatticeSpan
    (xTelemetry env)
    "lattice.load"
    Tel.Internal
    []
    [ ("lattice.loader.name", txtA loader)
    , ("lattice.loader.batch_size", intA batch)
    ]
    (const act)


-- | Load every not-yet-loaded key, one 'beLoad' per type.
loadRound :: ExecEnv -> IORef St -> [Job] -> IO ()
loadRound env stRef jobs = do
  st0 <- readIORef stRef
  modifyIORef' stRef $ \s ->
    s {stEntityKeys = foldr (Set.insert . entityKeyOf . jRef) (stEntityKeys s) jobs}
  let needed =
        Map.fromListWith Set.union $
          mapMaybe
            ( \j ->
                if Map.member (jRef j) (stRows st0)
                  then Nothing
                  else Just (refType (jRef j), Set.singleton (refKey (jRef j)))
            )
            jobs
  for_ (Map.toAscList needed) $ \(ty, keys) -> do
    loaded <- loaderSpan env (unTypeName ty) (Set.size keys) $
      beLoad (xBackend env) ty (projectionOf env ty) (Set.toAscList keys)
    for_ (Set.toAscList keys) $ \k -> do
      let ref = Ref ty k
          res = fromMaybe (Right RowAbsent) (Map.lookup k loaded)
      modifyIORef' stRef (\s -> s {stRows = Map.insert ref res (stRows s)})
      case res of
        Left bf -> addError stRef (scopedError (Just (ScopeEntity ref)) bf)
        Right (RowTombstone v) -> modifyIORef' stRef (tombSt ref v)
        Right _ -> pure ()


type TaskKey = (TypeName, FieldName, Text)


processJobs ::
  ExecEnv ->
  IORef St ->
  IORef Int ->
  IORef (Map DKey DeriveTask) ->
  [Job] ->
  IO ([Job], Map TaskKey EdgeTask)
processJobs env stRef counter deriveRef jobs = go [] Map.empty jobs
  where
    go kids tasks [] = pure (reverse kids, tasks)
    go kids tasks (j : rest) = do
      (kids', tasks') <- processJob env stRef counter deriveRef tasks j
      go (reverse kids' <> kids) tasks' rest


processJob ::
  ExecEnv ->
  IORef St ->
  IORef Int ->
  IORef (Map DKey DeriveTask) ->
  Map TaskKey EdgeTask ->
  Job ->
  IO ([Job], Map TaskKey EdgeTask)
processJob env stRef counter deriveRef tasks j = do
  st <- readIORef stRef
  case (Map.lookup (jRef j) (stRows st), lookupEntity (xSchema env) (refType (jRef j))) of
    (Just (Right (RowFound row)), Just ent)
      -- §14.4: a nodes job whose row fails its fetch-by gate emits and
      -- traverses nothing; the denial also drops its manifest entry.
      | Just pol <- jGate j
      , not (rowPredicates (xClaims env) row pol) -> do
          modifyIORef' stRef (\s -> s {stNodesDenied = Set.insert (jRef j) (stNodesDenied s)})
          pure ([], tasks)
      | otherwise -> do
          traverse_ (emitField env stRef deriveRef ent (jRef j) row) (nsFields (jNode j))
          goEdges [] tasks (nsEdges (jNode j)) row
    _ -> pure ([], tasks)
  where
    goEdges kids ts [] _ = pure (reverse kids, ts)
    goEdges kids ts (pe : pes) row = do
      (kids', ts') <- edgeStep env stRef counter ts j row pe
      goEdges (reverse kids' <> kids) ts' pes row


emitField ::
  ExecEnv ->
  IORef St ->
  IORef (Map DKey DeriveTask) ->
  EntityDef ->
  Ref ->
  EntityRow ->
  PlanField ->
  IO ()
emitField env stRef deriveRef ent ref row pf
  | not (emitsAt (xMode env) (pfLevel pf)) = pure ()
  | otherwise = case lookupEntityField ent (pfName pf) of
      Nothing -> pure ()
      Just fd
        | not (rowPredicates (xClaims env) row (entityFieldPolicy ent fd)) ->
            modifyIORef' stRef (suppressSt ref (rowVer row))
        | Just d <- fieldDerivation fd
        , OnRead <- derivMaterialize d ->
            -- §3.7: on-read derived values never live on the row; defer to
            -- the round's batched derive pass ('runDeriveTasks').
            modifyIORef' deriveRef (addDeriveParent ent fd d (pfName pf) key ref row)
        | otherwise -> do
            mval <-
              if null (fieldArgs fd)
                then pure (Map.lookup (pfName pf) (rowFields row))
                else
                  beComputed
                    (xBackend env)
                    (refType ref)
                    (pfName pf)
                    (computedArgs fd (bindRuntimeArgs (xVars env) (pfArgs pf)))
                    row
            for_ mval $ \v -> do
              -- §3.5.2: an empty array where a nonempty list (@[t]+@)
              -- governs the row data is a Field-scoped integrity error;
              -- the value still emits.
              when' (violatesList1 (xSchema env) (fieldType fd) v) $
                addError stRef $
                  ErrorRecord
                    { errScope = Just (ScopeField ref (pfName pf))
                    , errCode = Just "lattice:integrity"
                    , errDomain = Nothing
                    , errRetryable = False
                    , errMessage = Just "row value violates its nonempty list type"
                    }
              modifyIORef' stRef (addFieldSt ref (rowVer row) key v)
  where
    key = runtimeKey (xVars env) (pfName pf) (pfArgs pf) (pfKey pf)
    when' b act = if b then act else pure ()


-- | Declared arguments of a computed field, bound values first, defaults filling gaps.
computedArgs :: FieldDef -> [(ArgName, A.Value)] -> Map ArgName A.Value
computedArgs fd bound = Map.fromList (mapMaybe one (fieldArgs fd))
  where
    one ad = case lookup (adName ad) bound of
      Just v -> Just (adName ad, v)
      Nothing -> (,) (adName ad) <$> (qvalueToJson =<< adDefault ad)


-- ---------------------------------------------------------------------------
-- Derived fields (§3.7)
-- ---------------------------------------------------------------------------

type DKey = (TypeName, FieldName)


-- | One round's accumulated requests for one @on read@ derived field.
data DeriveTask = DeriveTask
  { dtEnt :: EntityDef
  , dtFieldDef :: FieldDef
  , dtDeriv :: Derivation
  , dtKey :: Text
  -- ^ The wire field key (derived fields take no arguments, so the plan's
  -- static key is final).
  , dtParents :: Map Ref EntityRow
  }


addDeriveParent ::
  EntityDef ->
  FieldDef ->
  Derivation ->
  FieldName ->
  Text ->
  Ref ->
  EntityRow ->
  Map DKey DeriveTask ->
  Map DKey DeriveTask
addDeriveParent ent fd d fname key ref row = Map.alter step (refType ref, fname)
  where
    step = \case
      Nothing ->
        Just
          DeriveTask
            { dtEnt = ent
            , dtFieldDef = fd
            , dtDeriv = d
            , dtKey = key
            , dtParents = Map.singleton ref row
            }
      Just t -> Just t {dtParents = Map.insertWith (\_new old -> old) ref row (dtParents t)}


{- | Resolve a round's derived fields (module haddock, /Derived fields/):
hidden 'beLoad'\/'beAggregate' batches, 'DepValues' assembly, one
'beDerive' per (type, field), witness + read-set key recording.
-}
runDeriveTasks :: ExecEnv -> IORef St -> IORef Int -> Map DKey DeriveTask -> IO ()
runDeriveTasks env stRef counter tasks
  | Map.null tasks = pure ()
  | otherwise = do
      st0 <- readIORef stRef
      let schema = xSchema env
          taskList = Map.toAscList tasks

          -- Per (task, parent): resolved ViaEdge deps.
          edgeWants =
            concatMap
              ( \(k, t) ->
                  concatMap
                    ( \case
                        ViaEdge e frag
                          | Just rel@ToOne {} <- lookupEntityRel (dtEnt t) e ->
                              map
                                ( \(pref, prow) ->
                                    ((k, pref), (e, frag, linkTargetOf schema rel prow))
                                )
                                (Map.toAscList (dtParents t))
                        _ -> []
                    )
                    (NE.toList (derivReads (dtDeriv t)))
              )
              taskList

          -- Per (task, parent): ViaCollection deps with their group keys.
          aggWants =
            concatMap
              ( \(k, t) ->
                  concatMap
                    ( \case
                        ViaCollection r agg
                          | Just ToMany {relCollection = col} <- lookupEntityRel (dtEnt t) r ->
                              map
                                ( \(pref, prow) ->
                                    ((k, pref), (r, colName col, agg, deriveGroupKey col pref prow))
                                )
                                (Map.toAscList (dtParents t))
                        _ -> []
                    )
                    (NE.toList (derivReads (dtDeriv t)))
              )
              taskList

          allDepRefs = dedupOrd (mapMaybe (\(_, (_, _, mref)) -> mref) edgeWants)
          toLoad = filter (\r -> not (Map.member r (stRows st0))) allDepRefs
          aggCalls =
            Map.fromListWith
              Set.union
              (map (\(_, (_, cn, agg, gk)) -> ((cn, agg), Set.singleton gk)) aggWants)
          hiddenCount = length toLoad + sum (map Set.size (Map.elems aggCalls))
      ok <- reserveHidden env stRef counter hiddenCount
      when' ok $ do
        -- One 'beLoad' per dep target type across every task.
        let byType =
              Map.fromListWith
                Set.union
                (map (\r -> (refType r, Set.singleton (refKey r))) toLoad)
        loadedPairs <-
          traverse
            ( \(ty, keys) ->
                -- Hidden derived-field batch: batch_size only, no span
                -- (module haddock of "Lattice.Telemetry").
                recordLoaderBatch (xTelemetry env) (unTypeName ty) (Set.size keys)
                  *> ((,) ty <$> beLoad (xBackend env) ty (projectionOf env ty) (Set.toAscList keys))
            )
            (Map.toAscList byType)
        let loaded =
              Map.fromList
                ( concatMap
                    (\(ty, m) -> map (\(k, res) -> (Ref ty k, res)) (Map.toList m))
                    loadedPairs
                )
            depRow r = case Map.lookup r (stRows st0) of
              Just res -> res
              Nothing -> fromMaybe (Right RowAbsent) (Map.lookup r loaded)
        -- Cache successful hidden loads for later rounds (failures stay
        -- uncached so a visible reach re-loads and reports); record every
        -- attempted dep entity key (§3.7 Invalidation).
        modifyIORef' stRef $ \s ->
          s
            { stRows = Map.union (stRows s) (Map.filter isRight loaded)
            , stEntityKeys = foldr (Set.insert . entityKeyOf) (stEntityKeys s) allDepRefs
            }
        -- Witness the dep entities (§3.7 Validators).
        for_ allDepRefs $ \r -> case depRow r of
          Right (RowFound row) -> addWitness stRef (WitnessEdge r (rowVer row))
          Right (RowTombstone v) -> addWitness stRef (WitnessEdge r v)
          Right RowAbsent -> addWitness stRef (WitnessEdge r "")
          Left _ -> pure ()
        -- One 'beAggregate' per (collection, aggregate) across every task.
        aggPairs <-
          traverse
            ( \((cn, agg), gks) ->
                (,) (cn, agg) <$> beAggregate (xBackend env) cn agg (Set.toAscList gks)
            )
            (Map.toAscList aggCalls)
        let aggResults = Map.fromList aggPairs
        -- Collection tags + aggregate witness per instantiated group.
        for_ (dedupOrd (map (\(_, (_, cn, agg, gk)) -> (cn, agg, gk)) aggWants)) $
          \(cn, agg, gk) -> do
            modifyIORef' stRef (\s -> s {stCollKeys = collectionKey cn gk : stCollKeys s})
            case Map.lookup (cn, agg) aggResults of
              Just (Right vals)
                | Just v <- Map.lookup gk vals ->
                    addWitness stRef (WitnessAgg (collectionKey cn gk) (aggValueHash v))
              _ -> pure ()
        -- Assemble DepValues, derive, emit — one 'beDerive' per (type, field).
        let edgeByParent = Map.fromListWith (<>) (map (\(kp, w) -> (kp, [w])) edgeWants)
            aggByParent = Map.fromListWith (<>) (map (\(kp, w) -> (kp, [w])) aggWants)
        for_ taskList $ \(k@(ty, fname), t) -> do
          let ownNames =
                concatMap
                  ( \case
                      OwnFields fs -> NE.toList fs
                      _ -> []
                  )
                  (NE.toList (derivReads (dtDeriv t)))
              buildOne (pref, prow) = do
                let edges = Map.findWithDefault [] (k, pref) edgeByParent
                    aggs = Map.findWithDefault [] (k, pref) aggByParent
                    failures =
                      mapMaybe
                        ( \(_, _, mref) ->
                            mref >>= \r -> case depRow r of
                              Left bf -> Just bf
                              Right _ -> Nothing
                        )
                        edges
                        <> mapMaybe
                          ( \(_, cn, agg, _) -> case Map.lookup (cn, agg) aggResults of
                              Just (Left bf) -> Just bf
                              _ -> Nothing
                          )
                          aggs
                case failures of
                  bf : _ -> do
                    addError stRef (scopedError (Just (ScopeField pref fname)) bf)
                    pure Nothing
                  [] -> do
                    let dvE =
                          Map.fromList
                            ( mapMaybe
                                ( \(e, frag, mref) ->
                                    mref >>= \r -> case depRow r of
                                      Right (RowFound row) ->
                                        Just (e, (r, fragmentFieldsOf schema frag row))
                                      _ -> Nothing
                                )
                                edges
                            )
                        dvA =
                          Map.fromList
                            ( mapMaybe
                                ( \(rf, cn, agg, gk) -> case Map.lookup (cn, agg) aggResults of
                                    Just (Right vals) -> (,) rf <$> Map.lookup gk vals
                                    _ -> Nothing
                                )
                                aggs
                            )
                        dv =
                          DepValues
                            { dvOwn = Map.restrictKeys (rowFields prow) (Set.fromList ownNames)
                            , dvEdges = dvE
                            , dvAggregates = dvA
                            }
                    pure (Just (refKey pref, (pref, prow, dv)))
          inputs <- catMaybes <$> traverse buildOne (Map.toAscList (dtParents t))
          when' (not (null inputs)) $ do
            vals <-
              beDerive
                (xBackend env)
                ty
                fname
                (Map.fromList (map (\(kk, (_, _, dv)) -> (kk, dv)) inputs))
            for_ inputs $ \(kk, (pref, prow, _)) ->
              for_ (Map.lookup kk vals) $ \v -> do
                when' (violatesList1 schema (fieldType (dtFieldDef t)) v) $
                  addError stRef $
                    ErrorRecord
                      { errScope = Just (ScopeField pref fname)
                      , errCode = Just "lattice:integrity"
                      , errDomain = Nothing
                      , errRetryable = False
                      , errMessage = Just "derived value violates its nonempty list type"
                      }
                modifyIORef' stRef (addFieldSt pref (rowVer prow) (dtKey t) v)
  where
    when' b act = if b then act else pure ()


linkTargetOf :: Schema -> RelationshipDef -> EntityRow -> Maybe Ref
linkTargetOf schema rel prow =
  Map.lookup (relByField rel) (rowFields prow) >>= refFromValue schema (relTarget rel)


{- | A derived aggregate's grouping values at one owner (mirrors
'edgeCollectionKey' minus bound arguments): the link field holds the
owner's key component, other grouping fields read the owner row.
-}
deriveGroupKey :: CollectionDef -> Ref -> EntityRow -> GroupKey
deriveGroupKey col pref prow = map gv (NE.toList (colGrouping col))
  where
    gv g
      | g == colLink col = refKey pref
      | Just v <- Map.lookup g (rowFields prow) = renderScalarKey v
      | otherwise = ""


-- | The fragment's top-level plain fields read off a dep target row.
fragmentFieldsOf :: Schema -> FragmentName -> EntityRow -> Map FieldName A.Value
fragmentFieldsOf schema frag row = case Map.lookup frag (schemaFragments schema) of
  Nothing -> Map.empty
  Just fdef ->
    Map.restrictKeys
      (rowFields row)
      (Set.fromList (map fName (selectionFields (fragSelection fdef))))


addWitness :: IORef St -> WitnessEntry -> IO ()
addWitness stRef w = modifyIORef' stRef (\s -> s {stWitness = Set.insert w (stWitness s)})


{- | Reserve hidden derived-field loads against the round fan-out budget;
exhaustion degrades the response and skips the round's derived fields.
-}
reserveHidden :: ExecEnv -> IORef St -> IORef Int -> Int -> IO Bool
reserveHidden env stRef counter n = do
  used <- readIORef counter
  let cap = fromIntegral (maxRoundFanout (xBudgets env))
  if used + n <= cap
    then do
      writeIORef counter (used + n)
      pure True
    else do
      modifyIORef' stRef (\s -> s {stIncomplete = True})
      addError stRef $
        ErrorRecord
          Nothing
          (Just "lattice:internal")
          Nothing
          False
          (Just "round fan-out exhausted resolving derived fields")
      pure False


edgeStep ::
  ExecEnv ->
  IORef St ->
  IORef Int ->
  Map TaskKey EdgeTask ->
  Job ->
  EntityRow ->
  PlanEdge ->
  IO ([Job], Map TaskKey EdgeTask)
edgeStep env stRef counter tasks j row pe = case depthGate of
  Nothing -> pure ([], tasks)
  Just _
    | not (traversesAt (xMode env) (peLevel pe)) -> pure ([], tasks)
    | not (rowPredicates (xClaims env) row (fromMaybe Public (relPolicy (peRel pe)))) ->
        pure ([], tasks)
    | otherwise -> case peRel pe of
        ToOne {relTarget = tgt, relByField = byF, relOptional = opt} ->
          case Map.lookup byF (rowFields row) >>= refFromValue (xSchema env) tgt of
            Nothing -> do
              -- §3.4: the bare `has one` promises resolution; a link column
              -- holding no usable value is an immediate cardinality failure.
              when' (not opt) $
                addError stRef (cardinalityError (jRef j) (peField pe))
              pure ([], tasks)
            Just childRef -> do
              when' (emitsAt (xMode env) (peLevel pe)) $
                modifyIORef' stRef (addFieldSt (jRef j) (rowVer row) key (refValue childRef))
              when' (not opt) $
                modifyIORef' stRef $ \s ->
                  s {stPendingOne = (jRef j, peField pe, childRef) : stPendingOne s}
              kids <- case nodeFor (peSelection pe) (refType childRef) of
                Just childNode ->
                  enqueue env stRef counter (ScopeEdge (jRef j) (peField pe)) $
                    [Job childRef childNode (childFuelOf pe (jFuel j)) Nothing]
                Nothing
                  | opt -> pure []
                  | otherwise ->
                      -- Existence probe: a required to-one must witness its
                      -- target even when the selection stops at the ref
                      -- (point-fetch masks, mutation output); the empty
                      -- selection loads the row and traverses nothing.
                      enqueue env stRef counter (ScopeEdge (jRef j) (peField pe)) $
                        [Job childRef (NodeSelection [] []) (childFuelOf pe (jFuel j)) Nothing]
              pure (kids, tasks)
        ToMany {relCollection = col} -> do
          win <- abortEither (windowFor col (bindRuntimeArgs (xVars env) (peArgs pe)))
          let tkey = (refType (jRef j), peField pe, key)
              contrib = (jRef j, row, jFuel j)
              tasks' = Map.alter (addParent contrib win col) tkey tasks
          pure ([], tasks')
  where
    key = runtimeKey (xVars env) (peField pe) (peArgs pe) (peKey pe)
    when' b act = if b then act else pure ()
    depthGate = case peDepth pe of
      Nothing -> Just ()
      Just n ->
        if Map.findWithDefault n (peField pe) (jFuel j) <= 0
          then Nothing
          else Just ()
    addParent contrib win col = \case
      Nothing ->
        Just
          EdgeTask
            { etEdge = pe
            , etCol = col
            , etKey = key
            , etWindow = win
            , etParents = [contrib]
            }
      Just t -> Just t {etParents = contrib : etParents t}


childFuelOf :: PlanEdge -> Map FieldName Int -> Map FieldName Int
childFuelOf pe fuelMap = case peDepth pe of
  Nothing -> Map.empty
  Just n -> Map.insert (peField pe) (Map.findWithDefault n (peField pe) fuelMap - 1) fuelMap


refFromValue :: Schema -> Target -> A.Value -> Maybe Ref
refFromValue schema tgt v = case tgt of
  TargetEntity t -> case v of
    A.Null -> Nothing
    _ -> Just (Ref t (renderScalarKey v))
  _ -> case v of
    A.String s
      | Just r <- parseRef s
      , refType r `elem` targetTypes schema tgt ->
          Just r
    _ -> Nothing


nodeFor :: TypedSelection -> TypeName -> Maybe NodeSelection
nodeFor ts t = Map.lookup t (tsPerType ts)


runEdgeTask :: ExecEnv -> IORef St -> IORef Int -> (TaskKey, EdgeTask) -> IO [Job]
runEdgeTask env stRef counter ((pty, field, key), task) = do
  let parents = reverse (etParents task)
      pe = etEdge task
  results <-
    loaderSpan env (unTypeName pty <> "." <> unFieldName field) (length parents) $
      beChildren (xBackend env) pty field (map (\(r, row, _) -> (r, row)) parents) (etWindow task)
  fmap concat (traverse (perParent pe results) parents)
  where
    perParent pe results (pref, prow, fuelMap) =
      case fromMaybe missing (Map.lookup pref results) of
        Left bf -> do
          addError stRef (scopedError (Just (ScopeEdge pref field)) bf)
          pure []
        Right page -> do
          omit <- overflowError stRef (Just (ScopeEdge pref field)) (Just pref) (etCol task) page
          underflowError stRef (Just (ScopeEdge pref field)) (etCol task) page
          when' (emitsAt (xMode env) (peLevel pe) && not omit) $
            modifyIORef' stRef $
              addFieldSt pref (rowVer prow) key (pageValueOf (colWindow (etCol task)) page)
          modifyIORef' stRef $ \s ->
            markCovered
              (pageRefs page)
              s {stCollKeys = edgeCollectionKey env (etCol task) pref prow pe : stCollKeys s}
          enqueue env stRef counter (ScopeEdge pref field) $
            mapMaybe
              (\r -> (\n -> Job r n (childFuelOf pe fuelMap) Nothing) <$> nodeFor (peSelection pe) (refType r))
              (pageRefs page)
      where
        missing = Left (internalError (Just "backend returned no page for parent"))
    when' b act = if b then act else pure ()


{- | The wire value of one resolved collection window: a bounded collection
is a plain array of ref strings (spec §9.1); a paginated one is the
@{"$page":{…}}@ wrapper.
-}
pageValueOf :: Windowing -> Page -> A.Value
pageValueOf w page = case w of
  Bounded {} -> A.toJSON (map renderRef (pageRefs page))
  Paginated _ ->
    pageToJSON
      PageValue
        { pvItems = pageRefs page
        , pvNext = pageNext page
        , pvPrev = pagePrev page
        , pvTotal = pageTotal page
        }


{- | Report @lattice:collection-overflow@ when a bounded collection
overflowed. Returns 'True' when the field must be omitted (the
collection's policy is 'Overflow'); a 'Truncate' policy emits the capped
array anyway.
-}
overflowError :: IORef St -> Maybe Scope -> Maybe Ref -> CollectionDef -> Page -> IO Bool
overflowError stRef scope _ref col page
  | not (pageOverflow page) = pure False
  | otherwise = do
      addError stRef $
        ErrorRecord
          { errScope = scope
          , errCode = Just "lattice:collection-overflow"
          , errDomain = Nothing
          , errRetryable = False
          , errMessage = Nothing
          }
      pure $ case colWindow col of
        Bounded _ _ Overflow -> True
        _ -> False


{- | Report @lattice:collection-underflow@ when a bounded collection with a
declared floor produced fewer than @min@ items (spec §3.6). The mirror of
'overflowError': the items that exist still emit, the response degrades.
-}
underflowError :: IORef St -> Maybe Scope -> CollectionDef -> Page -> IO ()
underflowError stRef scope col page = case colWindow col of
  Bounded minN _ _
    | fromIntegral (length (pageRefs page)) < minN ->
        addError stRef $
          ErrorRecord
            { errScope = scope
            , errCode = Just "lattice:collection-underflow"
            , errDomain = Nothing
            , errRetryable = False
            , errMessage = Nothing
            }
  _ -> pure ()


{- | Resolve the round-deferred half of required to-one enforcement (§3.4):
every obligation recorded at 'edgeStep' whose target row loaded absent is
a dangling link — a non-retryable Edge-scoped @lattice:cardinality@ error.
A tombstoned target resolved (the deletion is on the wire); a backend
failure already reported Entity-scoped and stays a load failure, not an
integrity verdict.
-}
checkCardinality :: IORef St -> IO ()
checkCardinality stRef = do
  st <- readIORef stRef
  for_ (reverse (stPendingOne st)) $ \(pref, field, childRef) ->
    case Map.lookup childRef (stRows st) of
      Just (Right RowAbsent) -> addError stRef (cardinalityError pref field)
      _ -> pure ()


cardinalityError :: Ref -> FieldName -> ErrorRecord
cardinalityError pref field =
  ErrorRecord
    { errScope = Just (ScopeEdge pref field)
    , errCode = Just "lattice:cardinality"
    , errDomain = Nothing
    , errRetryable = False
    , errMessage = Nothing
    }


{- | The surrogate key of one scanned edge collection. Grouping values are
read parent-side: the link field's value is the parent's key component;
a grouped-by override reads the parent row, then the edge's bound
arguments, then falls back to @\"\"@ (total by design).
-}
edgeCollectionKey :: ExecEnv -> CollectionDef -> Ref -> EntityRow -> PlanEdge -> SurrogateKey
edgeCollectionKey env col pref prow pe =
  collectionKey (colName col) (map gv (NE.toList (colGrouping col)))
  where
    bound = bindRuntimeArgs (xVars env) (peArgs pe)
    gv g
      | g == colLink col = refKey pref
      | Just v <- Map.lookup g (rowFields prow) = renderScalarKey v
      | Just v <- lookup (ArgName (unFieldName g)) bound = renderScalarKey v
      | otherwise = ""


-- | Cap a round's child enqueue at 'maxRoundFanout' (runtime guard).
enqueue :: ExecEnv -> IORef St -> IORef Int -> Scope -> [Job] -> IO [Job]
enqueue env stRef counter scope kids = do
  used <- readIORef counter
  let cap = fromIntegral (maxRoundFanout (xBudgets env))
      room = max 0 (cap - used)
  if length kids <= room
    then do
      writeIORef counter (used + length kids)
      pure kids
    else do
      writeIORef counter cap
      addError stRef $
        ErrorRecord
          { errScope = Just scope
          , errCode = Just "lattice:internal"
          , errDomain = Nothing
          , errRetryable = False
          , errMessage = Just "maxRoundFanout exceeded; traversal truncated"
          }
      pure (take room kids)


-- ---------------------------------------------------------------------------
-- Windows and cursors
-- ---------------------------------------------------------------------------

abortEither :: Either ExecAbort a -> IO a
abortEither = either (throwIO . ExecAbortEx) pure


{- | Resolve a collection's window from its bound pagination arguments.
@around@ wins over @before@\/@last@ (backward) over the forward default;
page sizes clamp to the cursor spec's @maxPage@ and default to
@defaultPage@ (falling back to @maxPage@ when the collection declares no
default and the size argument is absent).
-}
windowFor :: CollectionDef -> [(ArgName, A.Value)] -> Either ExecAbort Window
windowFor col args = case colWindow col of
  Bounded _ n _ -> Right (WWhole n)
  Paginated cs -> do
    let num name = argNatural =<< lookup name args
        cur name = case lookup name args of
          Just (A.String t) -> Just t
          _ -> Nothing
        clamp n = min (csMaxPage cs) n
        defCount = fromMaybe (csMaxPage cs) (csDefaultPage cs)
        dec mtxt = case mtxt of
          Nothing -> Right Nothing
          Just t -> case decodeCursor cs t of
            Left CursorRetired -> Left AbortCursorRetired
            Left CursorMalformed -> Left AbortCursorMalformed
            Right c -> Right (Just c)
    mAfter <- dec (cur "after")
    mBefore <- dec (cur "before")
    mAround <- dec (cur "around")
    Right $ case (mAround, mBefore, num "last") of
      (Just a, _, _) ->
        WPage {wCount = clamp (fromMaybe defCount (num "first")), wDir = PageAround, wAnchor = Just a}
      (_, Just _, _) ->
        WPage {wCount = clamp (fromMaybe defCount (num "last")), wDir = PageBackward, wAnchor = mBefore}
      (_, _, Just n) ->
        WPage {wCount = clamp n, wDir = PageBackward, wAnchor = mBefore}
      _ ->
        WPage {wCount = clamp (fromMaybe defCount (num "first")), wDir = PageForward, wAnchor = mAfter}


argNatural :: A.Value -> Maybe Natural
argNatural = \case
  A.Number n -> case Sci.floatingOrInteger @Double n of
    Right i | i >= 0 -> Just (fromInteger i)
    _ -> Nothing
  _ -> Nothing


-- ---------------------------------------------------------------------------
-- Runtime argument binding and field keys
-- ---------------------------------------------------------------------------

{- | Bind plan arguments against the request's variables. An argument whose
variable is absent (an optional variable that was not supplied) is
omitted — omission is the only spelling of absence (§4.8 rule 6).
-}
bindRuntimeArgs :: Map VarName A.Value -> [(ArgName, BoundArg)] -> [(ArgName, A.Value)]
bindRuntimeArgs vars = mapMaybe one
  where
    one (n, ArgLit v) = Just (n, v)
    one (n, ArgVar v) = (,) n <$> Map.lookup v vars


argMapOf :: [(ArgName, A.Value)] -> Map ArgName A.Value
argMapOf = Map.fromList


{- | The runtime wire field key: the plan's precomputed key when every
argument is literal, else 'canonicalFieldKey' re-rendered over the bound
values (variable-bearing keys are completed at execution time).
-}
runtimeKey :: Map VarName A.Value -> FieldName -> [(ArgName, BoundArg)] -> Text -> Text
runtimeKey vars name args staticKey
  | any (isVar . snd) args = canonicalFieldKey name (bindRuntimeArgs vars args)
  | otherwise = staticKey
  where
    isVar = \case
      ArgVar _ -> True
      ArgLit _ -> False


-- ---------------------------------------------------------------------------
-- Visibility predicates
-- ---------------------------------------------------------------------------

{- | Evaluate @visible when@ predicates against presented claims and a
loaded row. A missing claim or row field fails the predicate; values
compare by canonical JSON.
-}
evalPredicates :: Claims -> Map FieldName A.Value -> [ClaimPredicate] -> Bool
evalPredicates claims row = all one
  where
    one (ClaimPredicate c rhs) = case Map.lookup c claims of
      Nothing -> False
      Just cv -> case rhs of
        RhsLiteral v -> eqJson cv v
        RhsOneOf vs -> any (eqJson cv) vs
        RhsField f -> maybe False (eqJson cv) (Map.lookup f row)
    eqJson a b = canonicalJson a == canonicalJson b


{- | Root-membership guard: evaluate only the row-independent predicates of
a policy against presented claims ('RhsField' passes — per-row membership
filtering belongs to the backend's loader arguments). 'Private' passes
(the transport layer has already admitted the principal).
-}
claimOnlyPredicates :: Claims -> Policy -> Bool
claimOnlyPredicates claims = \case
  Public -> True
  Private -> True
  RequiresClaims preds -> all one preds
  where
    one (ClaimPredicate c rhs) = case rhs of
      RhsField _ -> True
      RhsLiteral v -> maybe False (\cv -> canonicalJson cv == canonicalJson v) (Map.lookup c claims)
      RhsOneOf vs -> case Map.lookup c claims of
        Nothing -> False
        Just cv -> any (\v -> canonicalJson cv == canonicalJson v) vs


-- | Full row-aware policy check ('Public'\/'Private' pass unconditionally).
rowPredicates :: Claims -> EntityRow -> Policy -> Bool
rowPredicates claims row = \case
  Public -> True
  Private -> True
  RequiresClaims preds -> evalPredicates claims (rowFields row) preds


-- ---------------------------------------------------------------------------
-- Synthetic selections (mutation output, default point-fetch masks)
-- ---------------------------------------------------------------------------

{- | The whole visible field set of an entity type at (or below) a caller's
slice: every scalar field, every @has one@ edge, and every /bounded/
@has many@ edge whose joined level fits — paginated edges are excluded
(they need explicit window arguments). Edge selections are empty, so the
executor emits their values without traversing. @base@ joins into every
level (point fetches join the type's @fetch by@ policy; mutation output
passes 'LPublic').
-}
outputSelection :: Schema -> SliceName -> Level -> TypeName -> Maybe NodeSelection
outputSelection schema cap base t = do
  ent <- lookupEntity schema t
  let keep lvl = levelRank lvl <= sliceRank cap
      fieldOf (n, fd) =
        let lvl = joinLevel base (policyLevel (entityFieldPolicy ent fd))
        in if keep lvl
              then
                Just
                  PlanField
                    { pfName = n
                    , pfArgs = []
                    , pfKey = unFieldName n
                    , pfLevel = lvl
                    , pfDerivation = fieldDerivation fd
                    }
              else Nothing
      edgeOf (n, rel) = case rel of
        ToMany {relCollection = col}
          | Paginated _ <- colWindow col -> Nothing
        _ ->
          let lvl = joinLevel base (policyLevel (fromMaybe Public (relPolicy rel)))
          in if keep lvl
                then
                  Just
                    PlanEdge
                      { peField = n
                      , peRel = rel
                      , peArgs = []
                      , peKey = unFieldName n
                      , peLevel = lvl
                      , peDepth = Nothing
                      , peSelection = TypedSelection Map.empty
                      }
                else Nothing
  Just
    NodeSelection
      { nsFields = mapMaybe fieldOf (Map.toAscList (entityFields ent))
      , nsEdges = mapMaybe edgeOf (Map.toAscList (entityRels ent))
      }
