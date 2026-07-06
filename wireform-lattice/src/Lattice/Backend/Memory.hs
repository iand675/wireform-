{- | A schema-driven in-memory 'Backend' for demos and tests.

'MemoryDb' is a set of per-entity-type tables ('TVar'-backed, so writers
compose in STM) with automatic version tokens: a fresh row is @e1@, every
update bumps the counter (@e2@, @e3@, …), and 'deleteRow' leaves a
tombstone whose version renders as @t:\<n\>@. A snapshot counter advances
once per write batch — a batch being every write between two observations
of 'snapshotToken' — so the token changes exactly when data changed.

'memoryBackend' interprets a 'Lattice.Schema.Schema' over the tables:

* 'beLoad' is a table lookup (absent → 'RowAbsent', deleted →
  'RowTombstone').
* 'beListRoot' generically serves field-backed list roots by scanning the
  target tables for rows whose grouping fields equal the grouping
  arguments, sorting by the collection keyset and paginating with
  "Lattice.Cursor" cursors. Param-backed roots (whose parameters are not
  target fields) need an 'mhListOverrides' hook.
* 'beChildren' generically serves @has many@ edges in one scan pass:
  target rows group by their link-field value, each parent's group is the
  set whose link value equals the parent's key (or, for a grouped-by
  override, the grouping field read off the parent row).
* 'beGetRoot' and 'beMutate' have no generic interpretation and always
  come from hooks.

Keyset comparison is typed: numbers compare numerically, strings
lexicographically (timestamps compare as their RFC 3339 renderings, which
is chronological order for a fixed UTC offset), and enum-typed columns
compare by constructor declaration order per the schema. Keysets are
expected to be unique per row (the usual keyset-pagination contract);
rows tying on the whole keyset sort by ref key for determinism but are
skipped together by an @after@\/@before@ anchor.

'mhFailures' is a fault-injection hook consulted once per loader call
('beLoad', 'beGetRoot', 'beListRoot', 'beChildren'); returning @Just@
fails that whole call, which testers use to exercise scoped error
records.

Entity keys are the canonical wire form of the key field(s) —
'renderScalarKey', comma-joined for composite keys; 'entityRowKey'
computes them from a row's fields.
-}
module Lattice.Backend.Memory (
  -- * Database
  MemoryDb,
  newMemoryDb,
  putRow,
  deleteRow,
  readRow,
  tableRows,
  snapshotToken,
  entityRowKey,

  -- * Hooks
  MemoryHooks (..),
  defaultHooks,

  -- * Backend
  memoryBackend,

  -- * Pagination machinery
  pageFromRows,
) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Aeson qualified as A
import Data.List (elemIndex, sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Backend
import Lattice.Cursor (Cursor (..), encodeCursor)
import Lattice.Schema
import Lattice.Types
import Lattice.Value (canonicalJson, renderScalarKey)
import Numeric.Natural (Natural)


-- ---------------------------------------------------------------------------
-- Storage
-- ---------------------------------------------------------------------------

-- | A stored row: its edit counter and its fields ('Nothing' = tombstone).
data StoredRow = StoredRow
  { srSeq :: Natural
  , srLive :: Maybe (Map FieldName A.Value)
  }


data MemoryDb = MemoryDb
  { dbTables :: TVar (Map TypeName (Map Text StoredRow))
  , dbSnapshotCounter :: TVar Natural
  , dbDirty :: TVar Bool
  }


newMemoryDb :: IO MemoryDb
newMemoryDb = MemoryDb <$> newTVarIO Map.empty <*> newTVarIO 1 <*> newTVarIO False


{- | Insert or update a row. Fresh rows get version @e1@; updating (or
reviving a tombstoned key) bumps the counter.
-}
putRow :: MemoryDb -> TypeName -> Text -> Map FieldName A.Value -> STM ()
putRow db ty key fields = do
  tables <- readTVar (dbTables db)
  let table = fromMaybe Map.empty (Map.lookup ty tables)
      nextSeq = maybe 1 ((+ 1) . srSeq) (Map.lookup key table)
      table' = Map.insert key (StoredRow nextSeq (Just fields)) table
  writeTVar (dbTables db) (Map.insert ty table' tables)
  writeTVar (dbDirty db) True


{- | Delete a row, leaving a tombstone whose version renders as @t:\<n\>@.
Deleting an already-tombstoned key is a no-op; deleting an absent key
still records a tombstone (at @t:1@).
-}
deleteRow :: MemoryDb -> TypeName -> Text -> STM ()
deleteRow db ty key = do
  tables <- readTVar (dbTables db)
  let table = fromMaybe Map.empty (Map.lookup ty tables)
  case Map.lookup key table of
    Just (StoredRow _ Nothing) -> pure ()
    existing -> do
      let nextSeq = maybe 1 ((+ 1) . srSeq) existing
          table' = Map.insert key (StoredRow nextSeq Nothing) table
      writeTVar (dbTables db) (Map.insert ty table' tables)
      writeTVar (dbDirty db) True


{- | The current snapshot token. Reading it seals the current write batch:
if anything was written since the last read, the counter bumps once.
-}
snapshotToken :: MemoryDb -> STM SnapshotToken
snapshotToken db = do
  dirty <- readTVar (dbDirty db)
  when dirty $ do
    modifyTVar' (dbSnapshotCounter db) (+ 1)
    writeTVar (dbDirty db) False
  n <- readTVar (dbSnapshotCounter db)
  pure ("mem:" <> tshow n)


-- | Load one key ('beLoad' semantics, single row).
readRow :: MemoryDb -> TypeName -> Text -> STM LoadResult
readRow db ty key = do
  tables <- readTVar (dbTables db)
  pure $ case Map.lookup ty tables >>= Map.lookup key of
    Nothing -> RowAbsent
    Just (StoredRow n Nothing) -> RowTombstone (tombVer n)
    Just (StoredRow n (Just fields)) -> RowFound (EntityRow (liveVer n) fields)


-- | Every live row of a table (tombstones excluded), keyed by entity key.
tableRows :: MemoryDb -> TypeName -> STM (Map Text EntityRow)
tableRows db ty = do
  tables <- readTVar (dbTables db)
  pure (Map.mapMaybe liveRow (fromMaybe Map.empty (Map.lookup ty tables)))
  where
    liveRow sr = EntityRow (liveVer (srSeq sr)) <$> srLive sr


{- | The table key of a row per the protocol's key convention:
'renderScalarKey' of the entity's key field(s), comma-joined for
composite keys. 'Nothing' when the type is unknown or a key field is
missing from the row.
-}
entityRowKey :: Schema -> TypeName -> Map FieldName A.Value -> Maybe Text
entityRowKey schema ty fields = do
  entity <- lookupEntity schema ty
  parts <- traverse keyPart (NE.toList (entityKey entity))
  pure (T.intercalate "," parts)
  where
    keyPart f = renderScalarKey <$> Map.lookup f fields


liveVer :: Natural -> Text
liveVer n = "e" <> tshow n


tombVer :: Natural -> Text
tombVer n = "t:" <> tshow n


tshow :: (Show a) => a -> Text
tshow = T.pack . show


-- ---------------------------------------------------------------------------
-- Hooks
-- ---------------------------------------------------------------------------

-- | Deployment-shaped behavior the schema alone cannot supply.
data MemoryHooks = MemoryHooks
  { mhComputed :: Map (TypeName, FieldName) (Map ArgName A.Value -> EntityRow -> IO (Maybe A.Value))
  -- ^ Argument-taking field evaluators ('beComputed').
  , mhGetRoots :: Map RootName (MemoryDb -> Map ArgName A.Value -> IO (Maybe Ref))
  -- ^ @get@ root resolvers; required per @get@ root (no generic reading).
  , mhListOverrides :: Map RootName (MemoryDb -> Map ArgName A.Value -> Window -> IO (Either BackendFailure Page))
  -- ^ @list@ roots that the generic field-backed scan cannot serve
  -- (param-backed roots like full-text search).
  , mhChildrenOverrides :: Map (TypeName, FieldName) (MemoryDb -> [(Ref, EntityRow)] -> Window -> IO (Map Ref (Either BackendFailure Page)))
  -- ^ @has many@ edges resolved from parent-side state instead of a
  -- child-table scan.
  , mhMutations :: Map MutationName (MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome)
  -- ^ Mutation effects ('beMutate').
  , mhFailures :: IO (Maybe BackendFailure)
  -- ^ Fault injection: consulted once per loader call; @Just@ fails that
  -- call. Defaults to @pure Nothing@.
  }


defaultHooks :: MemoryHooks
defaultHooks =
  MemoryHooks
    { mhComputed = Map.empty
    , mhGetRoots = Map.empty
    , mhListOverrides = Map.empty
    , mhChildrenOverrides = Map.empty
    , mhMutations = Map.empty
    , mhFailures = pure Nothing
    }


-- ---------------------------------------------------------------------------
-- Backend
-- ---------------------------------------------------------------------------

memoryBackend :: Schema -> MemoryDb -> MemoryHooks -> Backend
memoryBackend schema db hooks =
  Backend
    { beSnapshot = atomically (snapshotToken db)
    , beGetRoot = getRoot
    , beListRoot = listRoot
    , beChildren = children
    , beLoad = load
    , beComputed = computed
    , beMutate = mutate
    }
  where
    withFault :: (BackendFailure -> a) -> IO a -> IO a
    withFault onFail act =
      mhFailures hooks >>= \case
        Just f -> pure (onFail f)
        Nothing -> act

    load ty keys =
      withFault (\f -> Map.fromList (map (\k -> (k, Left f)) keys)) $
        atomically (Map.fromList <$> traverse (\k -> (\r -> (k, Right r)) <$> readRow db ty k) keys)

    getRoot name args = withFault Left $
      case Map.lookup name (mhGetRoots hooks) of
        Nothing -> pure (Left (internalError (Just ("no get-root hook for " <> unRootName name))))
        Just hook -> Right <$> hook db args

    listRoot name args window = withFault Left $
      case Map.lookup name (mhListOverrides hooks) of
        Just hook -> hook db args window
        Nothing -> genericListRoot schema db name args window

    children ty field parents window =
      withFault (\f -> Map.fromList (map (\(r, _) -> (r, Left f)) parents)) $
        case Map.lookup (ty, field) (mhChildrenOverrides hooks) of
          Just hook -> hook db parents window
          Nothing -> genericChildren schema db ty field parents window

    computed ty field args row =
      case Map.lookup (ty, field) (mhComputed hooks) of
        Nothing -> pure Nothing
        Just hook -> hook args row

    mutate name claims args =
      case Map.lookup name (mhMutations hooks) of
        Nothing -> pure (MutationFailed (internalError (Just ("no mutation hook for " <> unMutationName name))))
        Just hook -> hook db claims args


-- ---------------------------------------------------------------------------
-- Generic loaders
-- ---------------------------------------------------------------------------

{- | Field-backed @list@ root: scan the target tables for rows whose
grouping fields equal the grouping arguments (grouping fields with no
matching argument don't filter), then sort and window by the collection
keyset.
-}
genericListRoot ::
  Schema ->
  MemoryDb ->
  RootName ->
  Map ArgName A.Value ->
  Window ->
  IO (Either BackendFailure Page)
genericListRoot schema db name args window =
  case Map.lookup name (schemaRoots schema) of
    Just root
      | RootList <- rootKind root
      , Just col <- rootCollection root -> do
          let targets = targetTypes schema (rootTarget root)
          rows <- atomically (liveRowsOf db targets)
          let matching = filter (rowMatchesGrouping args col . snd) rows
          pure (Right (pageFromRows schema targets (colWindow col) window matching))
    _ -> pure (Left (internalError (Just ("no generic reading of list root " <> unRootName name))))


{- | @has many@ edge: one scan pass over the target tables, grouped by the
link-field value; each parent's page is the group matching its key (or
its grouped-by field value).
-}
genericChildren ::
  Schema ->
  MemoryDb ->
  TypeName ->
  FieldName ->
  [(Ref, EntityRow)] ->
  Window ->
  IO (Map Ref (Either BackendFailure Page))
genericChildren schema db ty field parents window =
  case lookupEntity schema ty >>= (`lookupEntityRel` field) of
    Just ToMany {relTarget, relCollection = col} -> do
      let targets = targetTypes schema relTarget
      rows <- atomically (liveRowsOf db targets)
      let grouped = foldr insertChild Map.empty rows
          insertChild (ref, fields) acc = case Map.lookup (colLink col) fields of
            Nothing -> acc
            Just v -> Map.insertWith (<>) (renderScalarKey v) [(ref, fields)] acc
          pageFor parent =
            pageFromRows schema targets (colWindow col) window $
              fromMaybe [] (Map.lookup (parentGroupValue col parent) grouped)
      pure (Map.fromList (map (\p -> (fst p, Right (pageFor p))) parents))
    _ ->
      pure . Map.fromList $
        map
          (\(r, _) -> (r, Left (internalError (Just ("no has-many edge " <> unFieldName field <> " on " <> unTypeName ty)))))
          parents


-- | Every live row of the given tables as @(ref, fields)@.
liveRowsOf :: MemoryDb -> [TypeName] -> STM [(Ref, Map FieldName A.Value)]
liveRowsOf db targets = concat <$> traverse one targets
  where
    one ty = do
      rows <- tableRows db ty
      pure (map (\(k, row) -> (Ref ty k, rowFields row)) (Map.toList rows))


-- | Does a candidate row belong to the collection instance named by @args@?
rowMatchesGrouping :: Map ArgName A.Value -> CollectionDef -> Map FieldName A.Value -> Bool
rowMatchesGrouping args col fields = all matches (NE.toList (colGrouping col))
  where
    matches gf = case Map.lookup (ArgName (unFieldName gf)) args of
      Nothing -> True
      Just want -> case Map.lookup gf fields of
        Nothing -> False
        Just got -> canonicalJson got == canonicalJson want


{- | The grouping value a parent contributes to a child scan: the default
grouping (the link field itself) holds the parent's key; a grouped-by
override reads the named field off the parent row, falling back to the
parent key when the parent carries no such field.
-}
parentGroupValue :: CollectionDef -> (Ref, EntityRow) -> Text
parentGroupValue col (ref, row) = case NE.toList (colGrouping col) of
  [g]
    | g /= colLink col
    , Just v <- Map.lookup g (rowFields row) ->
        renderScalarKey v
  _ -> refKey ref


-- ---------------------------------------------------------------------------
-- Sorting and pagination
-- ---------------------------------------------------------------------------

data KeyedRow = KeyedRow
  { krRef :: Ref
  , krKeys :: [A.Value]
  }


{- | Sort and window a set of matching rows per the collection's declared
'Windowing'. Bounded collections sort by ref key and cap at the window's
size, flagging 'pageOverflow' when the set exceeds the cap under the
'Overflow' policy. Paginated collections sort by the keyset (each
column's declared 'Direction' applied, ref key as final tiebreaker) and
resolve the window's anchor cursor against the keyset values, minting
@next@\/@prev@ cursors from the boundary rows.

Exported so hook overrides (e.g. a parent-side @friends@ edge) can reuse
the exact pagination semantics on rows they synthesize.
-}
pageFromRows ::
  Schema ->
  -- | Target types, for keyset column typing.
  [TypeName] ->
  -- | The collection's declared windowing.
  Windowing ->
  -- | The requested window.
  Window ->
  [(Ref, Map FieldName A.Value)] ->
  Page
pageFromRows schema targets windowing window rows = case windowing of
  Bounded _ _ policy ->
    let cap = fromIntegral $ case window of
          WWhole n -> n
          WPage {wCount} -> wCount
        sorted = sortBy (\a b -> compare (refKey (fst a)) (refKey (fst b))) rows
        total = length sorted
    in Page
        { pageRefs = map fst (take cap sorted)
        , pageNext = Nothing
        , pagePrev = Nothing
        , pageTotal = Nothing
        , pageOverflow = policy == Overflow && total > cap
        }
  Paginated spec -> paginateKeyset schema targets spec window rows


paginateKeyset ::
  Schema ->
  [TypeName] ->
  CursorSpec ->
  Window ->
  [(Ref, Map FieldName A.Value)] ->
  Page
paginateKeyset schema targets spec window rows =
  Page
    { pageRefs = map krRef items
    , pageNext = next
    , pagePrev = prev
    , pageTotal = totalOut
    , pageOverflow = False
    }
  where
    keysetFields = NE.toList (csKeyset spec)
    cols = map (\(f, d) -> (columnComparator schema targets f, d)) keysetFields
    keyed =
      map
        (\(ref, fields) -> KeyedRow ref (map (\(f, _) -> fromMaybe A.Null (Map.lookup f fields)) keysetFields))
        rows
    cmpAnchor r anchorKeys = cmpKeys cols (krKeys r) anchorKeys
    cmpRow a b = cmpKeys cols (krKeys a) (krKeys b) <> compare (refKey (krRef a)) (refKey (krRef b))
    sorted = sortBy cmpRow keyed
    total = length sorted

    (start, items) = case window of
      WWhole n -> (0, take (fromIntegral n) sorted)
      WPage count dir anchor ->
        let n = fromIntegral count
            anchorKeys = curValues <$> anchor
        in case dir of
            PageForward -> case anchorKeys of
              Nothing -> (0, take n sorted)
              Just ks ->
                let skipped = length (takeWhile (\r -> cmpAnchor r ks /= GT) sorted)
                in (skipped, take n (drop skipped sorted))
            PageBackward ->
              let pool = case anchorKeys of
                    Nothing -> sorted
                    Just ks -> takeWhile (\r -> cmpAnchor r ks == LT) sorted
                  s = max 0 (length pool - n)
              in (s, drop s pool)
            PageAround -> case anchorKeys of
              Nothing -> (0, take n sorted)
              Just ks ->
                let idx = length (takeWhile (\r -> cmpAnchor r ks == LT) sorted)
                    s = max 0 (min (idx - (n `div` 2)) (total - n))
                in (s, take n (drop s sorted))

    (next, prev) = case (items, reverse items) of
      (firstItem : _, lastItem : _) ->
        ( if start + length items < total
            then Just (encodeCursor spec (krKeys lastItem))
            else Nothing
        , if start > 0
            then Just (encodeCursor spec (krKeys firstItem))
            else Nothing
        )
      _ -> (Nothing, Nothing)

    totalOut = case csTotal spec of
      CountNone -> Nothing
      _ -> Just total


-- | Compose per-column comparisons, applying each column's 'Direction'.
cmpKeys :: [(A.Value -> A.Value -> Ordering, Direction)] -> [A.Value] -> [A.Value] -> Ordering
cmpKeys cols as bs = mconcat (zipWith3 apply cols as bs)
  where
    apply (cmp, dir) a b = case dir of
      Asc -> cmp a b
      Desc -> cmp b a


{- | Typed keyset-column comparison. Enum-typed columns (per the first
target type declaring the field) compare by constructor declaration
order; everything else falls through to 'compareCanonical'.
-}
columnComparator :: Schema -> [TypeName] -> FieldName -> (A.Value -> A.Value -> Ordering)
columnComparator schema targets field = case enumCtors of
  Just ctors -> compareEnum ctors
  Nothing -> compareCanonical
  where
    fieldTypes =
      mapMaybe
        (\ty -> fieldType <$> (lookupEntity schema ty >>= (`lookupEntityField` field)))
        targets
    enumCtors = case fieldTypes of
      (t : _)
        | TNamed n <- stripOptional t
        , Just (DeclEnum _ ctors) <- Map.lookup n (schemaTypes schema) ->
            Just (NE.toList ctors)
      _ -> Nothing
    stripOptional = \case
      TOptional t -> stripOptional t
      t -> t


-- | Declaration-order comparison; unknown constructors sort last, by text.
compareEnum :: [Text] -> A.Value -> A.Value -> Ordering
compareEnum ctors a b = case (a, b) of
  (A.String x, A.String y) -> compare (idx x, x) (idx y, y)
  _ -> compareCanonical a b
  where
    idx x = fromMaybe (length ctors) (elemIndex x ctors)


{- | Numbers numerically, strings lexicographically, 'A.Null' (and absent
fields) first; mixed shapes order by constructor rank, composite values
by canonical JSON bytes.
-}
compareCanonical :: A.Value -> A.Value -> Ordering
compareCanonical a b = case (a, b) of
  (A.Number x, A.Number y) -> compare x y
  (A.String x, A.String y) -> compare x y
  (A.Bool x, A.Bool y) -> compare x y
  (A.Null, A.Null) -> EQ
  _
    | rank a /= rank b -> compare (rank a) (rank b)
    | otherwise -> compare (canonicalJson a) (canonicalJson b)
  where
    rank :: A.Value -> Int
    rank = \case
      A.Null -> 0
      A.Bool _ -> 1
      A.Number _ -> 2
      A.String _ -> 3
      A.Array _ -> 4
      A.Object _ -> 5
