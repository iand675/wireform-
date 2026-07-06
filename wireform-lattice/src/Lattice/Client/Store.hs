{- | The client-side normalized entity store (spec §9.1, §9.3).

The store is a version-keyed entity map patched by every response and
every mutation result uniformly. Merge semantics per applied record:

* @entity@ with a __new @ver@__: the carried fields /replace/ the stored
  entry — fields known at the old version are dropped, because a version
  change invalidates everything not restated (a partial field set has no
  version of its own to key a finer merge on, §3.5.1).
* @entity@ with the __same @ver@__: the carried fields /union/ into the
  stored entry (two slices or two queries observing one version each
  contribute their disjoint field sets).
* @tombstone@: the entity is evicted and remembered in the tombstone set
  (§9.3). A later @entity@ record for the same ref resurrects it: a fresh
  fact beats a remembered eviction.
* @unchanged@ (§10.4): the origin elided an entity it believes we hold.
  When the store holds that exact @(id, ver)@ the entry is kept and marked
  fresh (its entity key leaves the stale set — assembly treats it as
  present). When it does not — the digest's false positive — the ref is
  recorded as a __gap__ ('takeGaps') for the client to repair with a
  point fetch. @elided@ is a policy fact, not field data: a no-op.
* @invalidated@: the carried surrogate keys accumulate into the stale-key
  set ('staleKeys'), the minimal read-your-writes staleness signal.
* @manifest@, @error@, @end@, @plan@, @reauth@, and unknown kinds do not
  touch the store.

'denormalize' rebuilds the per-root JSON tree a caller expects from a
normalized snapshot by walking the query selection.
-}
module Lattice.Client.Store (
  -- * The store
  Store,
  newStore,
  StoredEntity,
  applyRecords,
  applyRecord,
  mergeEntityRecord,
  lookupEntity,
  entityVersions,
  isTombstoned,
  snapshotEntities,
  markStale,
  staleKeys,
  unchangedGaps,
  takeGaps,

  -- * Denormalization
  denormalize,
) where

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Lattice.Canonical (canonicalFieldKey)
import Lattice.Query.AST
import Lattice.Schema (
  FragmentDef (..),
  RootDef (..),
  RootKind (..),
  Schema (..),
  interfaceMembers,
 )
import Lattice.Types
import Lattice.Value (qvalueToJson)
import Lattice.Wire

-- | What the store knows about one entity contribution: a @ver@ and its
-- fields, keyed by canonical wire field key (e.g. @avatarUrl(size:48)@).
type StoredEntity = (Text, Map Text A.Value)


{- | An STM-shared normalized entity store.

Entries are keyed per @(id, src)@ (§18.1: a gateway emits an entity's
owner record and extension records separately, each versioned by its
contributing module — the store tracks @ver@ per source and unions
fields across sources on read). Records without a @src@ tag (every
monolithic origin) land in the 'Nothing' slot, preserving the exact
pre-federation semantics.
-}
data Store = Store
  { storeEntities :: TVar (Map Ref (Map (Maybe Text) StoredEntity))
  , storeTombstones :: TVar (Set Ref)
  , storeStale :: TVar (Set SurrogateKey)
  , storeGaps :: TVar (Set Ref)
  -- ^ Refs the origin elided as @unchanged@ that this store cannot
  -- satisfy (§10.4 false positives): repair by point fetch.
  }


newStore :: IO Store
newStore =
  Store
    <$> newTVarIO Map.empty
    <*> newTVarIO Set.empty
    <*> newTVarIO Set.empty
    <*> newTVarIO Set.empty


-- | Apply a response's records in stream order (§9.1 merge semantics,
-- module header).
applyRecords :: Store -> [Record] -> STM ()
applyRecords store = mapM_ (applyRecord store)


applyRecord :: Store -> Record -> STM ()
applyRecord Store {..} = \case
  REntity er -> do
    modifyTVar' storeTombstones (Set.delete (erId er))
    modifyTVar' storeEntities $ \m ->
      let srcs = Map.findWithDefault Map.empty (erId er) m
          slot = mergeEntityRecord er (Map.lookup (erSrc er) srcs)
      in Map.insert (erId er) (Map.insert (erSrc er) slot srcs) m
  -- A tombstone evicts the WHOLE entity: extension records cannot
  -- outlive the owner's row (§18.1).
  RTombstone ref _ver _item -> do
    modifyTVar' storeEntities (Map.delete ref)
    modifyTVar' storeTombstones (Set.insert ref)
  RInvalidated keys _item ->
    modifyTVar' storeStale (\s -> foldr Set.insert s keys)
  RManifest {} -> pure ()
  RElided {} -> pure ()
  RUnchanged ref ver -> do
    ents <- readTVar storeEntities
    case Map.lookup ref ents of
      Just srcs
        | any ((== ver) . fst) (Map.elems srcs) ->
            modifyTVar' storeStale (Set.delete (entityKeyOf ref))
      _ -> modifyTVar' storeGaps (Set.insert ref)
  RError {} -> pure ()
  REnd {} -> pure ()
  RPlan {} -> pure ()
  RReauth -> pure ()
  RUnknown {} -> pure ()


{- | The single-entity merge rule: same @ver@ unions carried fields into
the stored entry (carried values win a key collision); a different (or
first-seen) @ver@ replaces the entry with exactly the carried fields.
-}
mergeEntityRecord :: EntityRecord -> Maybe StoredEntity -> StoredEntity
mergeEntityRecord er = \case
  Just (oldVer, oldFields)
    | oldVer == erVer er -> (oldVer, Map.union (erFields er) oldFields)
  _ -> (erVer er, erFields er)


-- | The merged cross-source view: fields union across contributing
-- sources (disjoint by ownership); the representative @ver@ is the
-- untagged slot's when present (monolithic origins), else the first
-- source's in name order.
lookupEntity :: Store -> Ref -> STM (Maybe StoredEntity)
lookupEntity store ref =
  (fmap mergeSrcs . Map.lookup ref) <$> readTVar (storeEntities store)


-- | Per-source versions of one entity (§18.1: the store keys versions by
-- (entity, contributing module); 'Nothing' is the untagged slot).
entityVersions :: Store -> Ref -> STM (Map (Maybe Text) Text)
entityVersions store ref =
  maybe Map.empty (Map.map fst) . Map.lookup ref <$> readTVar (storeEntities store)


mergeSrcs :: Map (Maybe Text) StoredEntity -> StoredEntity
mergeSrcs srcs =
  ( maybe "" fst (headMaybe (Map.elems srcs))
  , Map.unions (map snd (Map.elems srcs))
  )
  where
    headMaybe = \case
      [] -> Nothing
      (x : _) -> Just x


isTombstoned :: Store -> Ref -> STM Bool
isTombstoned store ref = Set.member ref <$> readTVar (storeTombstones store)


-- | The full entity map (merged cross-source view), e.g. as input to
-- 'denormalize'.
snapshotEntities :: Store -> STM (Map Ref StoredEntity)
snapshotEntities store = Map.map mergeSrcs <$> readTVar (storeEntities store)


-- | Record surrogate keys as stale (mirrors an @invalidated@ record).
markStale :: Store -> [SurrogateKey] -> STM ()
markStale store keys = modifyTVar' (storeStale store) (\s -> foldr Set.insert s keys)


staleKeys :: Store -> STM (Set SurrogateKey)
staleKeys = readTVar . storeStale


-- | The accumulated @unchanged@ false-positive refs (§10.4), non-destructively.
unchangedGaps :: Store -> STM (Set Ref)
unchangedGaps = readTVar . storeGaps


-- | Drain the gap set: the caller takes responsibility for repairing
-- (point-fetching) the returned refs.
takeGaps :: Store -> STM (Set Ref)
takeGaps store = do
  gaps <- readTVar (storeGaps store)
  writeTVar (storeGaps store) Set.empty
  pure gaps


-- ---------------------------------------------------------------------------
-- Denormalization
-- ---------------------------------------------------------------------------

{- | Rebuild the per-root JSON tree a caller expects by walking the query
selection against a normalized snapshot.

For each root named in the manifest's root map, the corresponding
top-level query field's selection is walked over the root's refs:

* a stored to-one edge value @{\"$ref\":…}@ denormalizes to the nested
  object under the edge's subselection;
* a stored page value @{\"$page\":{items,next,prev,total}}@ denormalizes
  to an object with a denormalized @items@ array plus @next@\/@prev@\/
  @total@ carried through when present;
* a bounded edge's plain array of ref strings denormalizes to an array
  of nested objects;
* parameterized fields are looked up by canonical wire key
  ('canonicalFieldKey') with variables substituted from the bindings
  (query variable defaults fill unbound names);
* inline fragments dispatch on the ref's concrete type (and, when a
  schema is supplied and the name is an interface, on membership);
* @...SchemaFragment@ spreads expand against the supplied schema; when
  no schema is available they are __skipped__ (the fields they would
  have selected are simply absent from the tree);
* @\@depth(n)@ edges re-apply the enclosing selection @n@ levels deep,
  omitting the recursive edge at the innermost level;
* fields absent from the snapshot are omitted.

Every denormalized entity object carries @\"$ref\"@ (the rendered ref)
and @\"$ver\"@; a ref with no snapshot entry renders as a bare
@{\"$ref\":…}@ placeholder.

Root shape: with a schema, a 'RootGet' root yields the single object
(the root key is omitted when its membership is empty) and a 'RootList'
root yields an array. Without a schema every root yields an array,
since root kinds are a schema fact.
-}
denormalize ::
  Maybe Schema ->
  -- | The query (canonical or as-parsed; only its selection is walked).
  Document ->
  -- | Variable bindings.
  Map VarName A.Value ->
  Manifest ->
  -- | Snapshot, e.g. 'snapshotEntities'.
  Map Ref StoredEntity ->
  Map Text A.Value
denormalize mSchema doc vars manifest snapshot =
  Map.foldrWithKey addRoot Map.empty (mRoot manifest)
  where
    env0 :: Map VarName A.Value
    env0 = Map.union vars defaults

    defaults :: Map VarName A.Value
    defaults = foldr addDefault Map.empty (qVars (docQuery doc))
      where
        addDefault vd acc = case vdDefault vd >>= qvalueToJson of
          Just v -> Map.insert (vdName vd) v acc
          Nothing -> acc

    addRoot :: Text -> [Ref] -> Map Text A.Value -> Map Text A.Value
    addRoot name refs acc = case findRootField name of
      Nothing -> acc
      Just fld ->
        let sel = concat (fSelection fld)
            items = map (denormRef env0 Set.empty sel) refs
        in if rootIsSingle name
            then case items of
              [] -> acc
              (x : _) -> Map.insert name x acc
            else Map.insert name (A.Array (V.fromList items)) acc

    findRootField :: Text -> Maybe Field
    findRootField name = go (qSelection (docQuery doc))
      where
        go [] = Nothing
        go (SField f : rest)
          | unFieldName (fName f) == name = Just f
          | otherwise = go rest
        go (_ : rest) = go rest

    rootIsSingle :: Text -> Bool
    rootIsSingle name = case mSchema of
      Nothing -> False
      Just schema -> case Map.lookup (RootName name) (schemaRoots schema) of
        Just rd -> rootKind rd == RootGet
        Nothing -> False

    denormRef :: Map VarName A.Value -> Set FragmentName -> SelectionSet -> Ref -> A.Value
    denormRef env stack sel ref = case Map.lookup ref snapshot of
      Nothing -> A.toJSON (Map.singleton ("$ref" :: Text) (A.String (renderRef ref)))
      Just (ver, fields) ->
        let base =
              Map.fromList
                [ ("$ref", A.String (renderRef ref))
                , ("$ver", A.String ver)
                ]
        in A.toJSON (walk env stack sel ref fields base)

    walk ::
      Map VarName A.Value ->
      Set FragmentName ->
      SelectionSet ->
      Ref ->
      Map Text A.Value ->
      Map Text A.Value ->
      Map Text A.Value
    walk env stack sel ref fields = flip (foldl step) sel
      where
        step acc = \case
          SField f -> stepField env stack sel f fields acc
          SInline t sub
            | inlineApplies t ref -> walk env stack sub ref fields acc
            | otherwise -> acc
          SSpread n args -> stepSpread env stack n args ref fields acc

    stepField ::
      Map VarName A.Value ->
      Set FragmentName ->
      SelectionSet ->
      Field ->
      Map Text A.Value ->
      Map Text A.Value ->
      Map Text A.Value
    stepField env stack enclosing f@Field {..} fields acc =
      case traverse (resolveArg env) fArgs of
        Nothing -> acc
        Just argPairs ->
          let key = canonicalFieldKey fName argPairs
          in case Map.lookup key fields of
              Nothing -> acc
              Just v -> case edgeSelection enclosing f of
                Nothing -> Map.insert key v acc
                Just sub -> Map.insert key (edgeValue env stack sub v) acc

    -- The child selection an edge occurrence recurses with: its own
    -- subselection, or (for @depth) the enclosing selection with this
    -- edge's fuel decremented (dropped at zero).
    edgeSelection :: SelectionSet -> Field -> Maybe SelectionSet
    edgeSelection enclosing f = case (fSelection f, fDepth f) of
      (Just sub, _) -> Just sub
      (Nothing, Just n)
        | n >= 1 -> Just (concatMap (decrement n) enclosing)
        | otherwise -> Nothing
      (Nothing, Nothing) -> Nothing
      where
        decrement n = \case
          SField g
            | fName g == fName f
            , fArgs g == fArgs f
            , fDepth g == Just n ->
                if n <= 1 then [] else [SField g {fDepth = Just (n - 1)}]
          s -> [s]

    edgeValue :: Map VarName A.Value -> Set FragmentName -> SelectionSet -> A.Value -> A.Value
    edgeValue env stack sub v = case v of
      A.Object o
        | Just (A.String t) <- KM.lookup "$ref" o
        , Just r <- parseRef t ->
            denormRef env stack sub r
        | KM.member "$page" o
        , Just pv <- pageFromJSON v ->
            pageObject env stack sub pv
      A.Array elems -> A.Array (V.map (arrayItem env stack sub) elems)
      other -> other

    arrayItem :: Map VarName A.Value -> Set FragmentName -> SelectionSet -> A.Value -> A.Value
    arrayItem env stack sub = \case
      A.String t | Just r <- parseRef t -> denormRef env stack sub r
      other -> other

    pageObject :: Map VarName A.Value -> Set FragmentName -> SelectionSet -> PageValue -> A.Value
    pageObject env stack sub PageValue {..} =
      A.toJSON pairs
      where
        pairs :: Map Text A.Value
        pairs =
          Map.fromList
            ( ("items", A.Array (V.fromList (map (denormRef env stack sub) pvItems)))
                : concat
                  [ maybe [] (\c -> [("next", A.String c)]) pvNext
                  , maybe [] (\c -> [("prev", A.String c)]) pvPrev
                  , maybe [] (\n -> [("total", A.toJSON n)]) pvTotal
                  ]
            )

    inlineApplies :: TypeName -> Ref -> Bool
    inlineApplies t ref =
      t == refType ref || case mSchema of
        Nothing -> False
        Just schema ->
          Set.member
            (refType ref)
            (interfaceMembers schema (InterfaceName (unTypeName t)))

    stepSpread ::
      Map VarName A.Value ->
      Set FragmentName ->
      FragmentName ->
      [Argument] ->
      Ref ->
      Map Text A.Value ->
      Map Text A.Value ->
      Map Text A.Value
    stepSpread env stack n args ref fields acc = case mSchema of
      Nothing -> acc
      Just schema
        | Set.member n stack -> acc
        | Just fd <- Map.lookup n (schemaFragments schema)
        , fragApplies schema fd ref
        , Just env' <- fragEnv env fd args ->
            walk env' (Set.insert n stack) (fragSelection fd) ref fields acc
        | otherwise -> acc

    fragApplies :: Schema -> FragmentDef -> Ref -> Bool
    fragApplies schema fd ref =
      fragOn fd == unTypeName (refType ref)
        || Set.member (refType ref) (interfaceMembers schema (InterfaceName (fragOn fd)))

    -- Schema-fragment parameters are the only variables in scope inside a
    -- fragment selection (they are late-bound schema declarations); each
    -- binds from the spread's argument (resolved against the caller's
    -- environment) or the parameter's declared default.
    fragEnv :: Map VarName A.Value -> FragmentDef -> [Argument] -> Maybe (Map VarName A.Value)
    fragEnv outer fd args = foldr bindParam (Just Map.empty) (fragParams fd)
      where
        bindParam _ Nothing = Nothing
        bindParam vd (Just acc) =
          case lookupArg (vdName vd) >>= resolveQValue outer of
            Just v -> Just (Map.insert (vdName vd) v acc)
            Nothing -> case vdDefault vd >>= qvalueToJson of
              Just v -> Just (Map.insert (vdName vd) v acc)
              Nothing -> Nothing
        lookupArg (VarName name) = go args
          where
            go [] = Nothing
            go (Argument (ArgName an) qv : rest)
              | an == name = Just qv
              | otherwise = go rest

    resolveArg :: Map VarName A.Value -> Argument -> Maybe (ArgName, A.Value)
    resolveArg env (Argument n qv) = (,) n <$> resolveQValue env qv

    resolveQValue :: Map VarName A.Value -> QValue -> Maybe A.Value
    resolveQValue env = \case
      QVar v -> Map.lookup v env
      QList qs -> A.Array . V.fromList <$> traverse (resolveQValue env) qs
      q -> qvalueToJson q
