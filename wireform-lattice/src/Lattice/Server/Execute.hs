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
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Scientific qualified as Sci
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Lattice.Backend
import Lattice.Canonical (canonicalFieldKey)
import Lattice.Cursor (CursorError (..), decodeCursor)
import Lattice.Plan
import Lattice.Schema
import Lattice.Types
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
  }


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
  -- ^ @Type:key@ for every reached entity.
  , xrCollectionKeys :: [SurrogateKey]
  -- ^ Scanned collections, instantiated, in scan order (deduplicated).
  , xrCovered :: Set SurrogateKey
  -- ^ Entity keys reached through a collection scan (coarsening candidates).
  , xrDegraded :: Bool
  -- ^ Any error record present.
  , xrComplete :: Bool
  -- ^ 'False' only when an exception truncated execution.
  }


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
  , stIncomplete :: Bool
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
    , stIncomplete = False
    }


-- | One unit of per-entity work: apply this node selection to this ref.
data Job = Job
  { jRef :: Ref
  , jNode :: NodeSelection
  , jFuel :: Map FieldName Int
  -- ^ Remaining @\@depth@ fuel along this path, keyed by edge field.
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
  pure (map (\(r, n) -> Job r n Map.empty) seeds)


runEngine :: ExecEnv -> (IORef St -> IORef Int -> IO [Job]) -> IO (Either ExecAbort ExecResult)
runEngine env seed = do
  stRef <- newIORef emptySt
  outcome <- try $ handle (onCrash stRef) $ do
    counter <- newIORef (0 :: Int)
    jobs0 <- seed stRef counter
    rounds env stRef (fromIntegral (maxDepth (xBudgets env)) + 2) jobs0
    checkCardinality stRef
  case outcome of
    Left (ExecAbortEx a) -> pure (Left a)
    Right () -> Right . finalize <$> readIORef stRef
  where
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
        mapMaybe (\r -> (\n -> Job r n Map.empty) <$> nodeFor (prSelection pr) (refType r)) refs
  where
    when' b act = if b then act else pure ()


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

rounds :: ExecEnv -> IORef St -> Int -> [Job] -> IO ()
rounds _ _ _ [] = pure ()
rounds env stRef fuel jobs
  | fuel <= 0 = do
      modifyIORef' stRef (\s -> s {stIncomplete = True})
      addError stRef $
        ErrorRecord Nothing (Just "lattice:internal") Nothing False (Just "round budget exhausted")
  | otherwise = do
      loadRound env stRef jobs
      counter <- newIORef (0 :: Int)
      (toOneKids, tasks) <- processJobs env stRef counter jobs
      edgeKids <- concat <$> traverse (runEdgeTask env stRef counter) (Map.toAscList tasks)
      rounds env stRef (fuel - 1) (toOneKids <> edgeKids)


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
    loaded <- beLoad (xBackend env) ty (Set.toAscList keys)
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
  [Job] ->
  IO ([Job], Map TaskKey EdgeTask)
processJobs env stRef counter jobs = go [] Map.empty jobs
  where
    go kids tasks [] = pure (reverse kids, tasks)
    go kids tasks (j : rest) = do
      (kids', tasks') <- processJob env stRef counter tasks j
      go (reverse kids' <> kids) tasks' rest


processJob ::
  ExecEnv ->
  IORef St ->
  IORef Int ->
  Map TaskKey EdgeTask ->
  Job ->
  IO ([Job], Map TaskKey EdgeTask)
processJob env stRef counter tasks j = do
  st <- readIORef stRef
  case (Map.lookup (jRef j) (stRows st), lookupEntity (xSchema env) (refType (jRef j))) of
    (Just (Right (RowFound row)), Just ent) -> do
      traverse_ (emitField env stRef ent (jRef j) row) (nsFields (jNode j))
      goEdges [] tasks (nsEdges (jNode j)) row
    _ -> pure ([], tasks)
  where
    goEdges kids ts [] _ = pure (reverse kids, ts)
    goEdges kids ts (pe : pes) row = do
      (kids', ts') <- edgeStep env stRef counter ts j row pe
      goEdges (reverse kids' <> kids) ts' pes row


emitField :: ExecEnv -> IORef St -> EntityDef -> Ref -> EntityRow -> PlanField -> IO ()
emitField env stRef ent ref row pf
  | not (emitsAt (xMode env) (pfLevel pf)) = pure ()
  | otherwise = case lookupEntityField ent (pfName pf) of
      Nothing -> pure ()
      Just fd
        | not (rowPredicates (xClaims env) row (entityFieldPolicy ent fd)) ->
            modifyIORef' stRef (suppressSt ref (rowVer row))
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
                    [Job childRef childNode (childFuelOf pe (jFuel j))]
                Nothing
                  | opt -> pure []
                  | otherwise ->
                      -- Existence probe: a required to-one must witness its
                      -- target even when the selection stops at the ref
                      -- (point-fetch masks, mutation output); the empty
                      -- selection loads the row and traverses nothing.
                      enqueue env stRef counter (ScopeEdge (jRef j) (peField pe)) $
                        [Job childRef (NodeSelection [] []) (childFuelOf pe (jFuel j))]
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
              (\r -> (\n -> Job r n (childFuelOf pe fuelMap)) <$> nodeFor (peSelection pe) (refType r))
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
