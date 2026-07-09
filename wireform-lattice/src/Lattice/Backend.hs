{- | The origin backend contract: what a deployment supplies to
"Lattice.Server" to serve a schema.

The shape enforces the protocol's central execution constraint (§3.1):
loaders are set-in, map-out. There is no per-parent resolution type; the
executor always hands the backend the full key set of a round, and N+1 is
inexpressible.

Loads are policy-free: backends fetch rows by key and know nothing about
callers. Visibility is applied at emission by the server, per response,
against the response's slice and claims (§6.9).

== Projections: load only what the plan reads

Every 'beLoad' call carries a 'Projection': the set of stored fields the
executor can possibly read off the returned rows for that entity type,
derived statically from the compiled plan ('Lattice.Plan.planProjections').
A backend MAY use it to fetch less — the canonical example is a SQL
backend rendering @SELECT@ column lists:

@
load ty proj keys = case proj of
  ProjectAll      -> ... "SELECT * FROM t WHERE key IN (...)"
  ProjectFields fs -> ... "SELECT ver, k1, k2, " <> columnsFor fs <> " ..."
@

The contract is one-directional: a returned row MUST include every
projected field the stored row actually has (an absent projected field
reads as an absent field — silent data loss, not an error), and MAY
include more; ignoring the projection and returning full rows is always
correct. 'rowVer' is required regardless. Rows are dynamic
(@Map FieldName Value@), so the projection is a plain value — no
higher-kinded row machinery is needed at this seam; a typed-row backend
adapter (e.g. an HKD record @User f@ with @f ~ Maybe@ per column) can be
layered on top by interpreting the same 'Projection' against its
field-to-column table.

Parent rows handed to 'beChildren' (and the owner rows a derived-field
compute reads through 'DepValues') come from projected loads: they carry
the link/grouping fields plus whatever the plan selected, not the whole
row. A children resolver that wants other parent-side state must read its
own storage — it /is/ the storage.

== Derived fields (§3.7)

Three seams serve derived fields, all batched like every other loader:

* 'beAggregate' resolves one collection aggregate for a set of grouping
  keys at once (set-in, map-out). A 'GroupKey' is the collection's
  grouping values in 'Lattice.Schema.colGrouping' order, each the
  canonical wire text of the grouping field's value on the member rows
  (for the link field: the parent's key component).
* 'beDerive' is the registered compute of one derived field: a BATCHED
  PURE function from dep payloads to values, keyed by the owning
  entities' keys. A key absent from the result elides the field for that
  entity (mirroring 'beComputed'). The executor assembles 'DepValues'
  from the declared read set; the compute never does its own I\/O.
* 'beStoreDerived' is the write half of @maintained@ materialization: a
  bracketed backend write of recomputed values onto the owning rows
  (write scope: exactly those entities), bumping each row's @ver@
  ordinarily. The result maps each written key to its NEW version; keys
  absent from the result (vanished\/tombstoned rows) are skipped by the
  relay. Only the origin's maintained-derivation relay calls this.
-}
module Lattice.Backend (
  Backend (..),
  Projection (..),
  projectsField,
  projectRow,
  EntityRow (..),
  LoadResult (..),
  Page (..),
  Window (..),
  PageDir (..),
  BackendFailure (..),
  loaderTimeout,
  upstreamUnavailable,
  internalError,
  MutationOutcome (..),
  CommitResult (..),
  WriteFact (..),
  MutatePrecondition (..),
  PreCheck (..),
  GroupKey,
  DepValues (..),
  emptyDepValues,
) where

import Data.Aeson qualified as A
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Set (Set)
import Data.Set qualified as Set
import Lattice.Cursor (Cursor)
import Lattice.Schema (Aggregate)
import Lattice.Types
import Numeric.Natural (Natural)


-- | One entity's stored state: a version token and its field values.
data EntityRow = EntityRow
  { rowVer :: Text
  , rowFields :: Map FieldName A.Value
  }
  deriving stock (Eq, Show)


{- | The stored fields a 'beLoad' caller can possibly read off the returned
rows for one entity type, derived from the compiled plan
('Lattice.Plan.planProjections'). See the module haddock (/Projections/)
for the contract; '<>' is union with 'ProjectAll' absorbing.
-}
data Projection
  = ProjectAll
  | ProjectFields (Set FieldName)
  deriving stock (Eq, Show)


instance Semigroup Projection where
  ProjectAll <> _ = ProjectAll
  _ <> ProjectAll = ProjectAll
  ProjectFields a <> ProjectFields b = ProjectFields (Set.union a b)


-- | @'ProjectFields' 'Set.empty'@: nothing beyond @ver@ is read (existence
-- probes, empty nodes selections).
instance Monoid Projection where
  mempty = ProjectFields Set.empty


projectsField :: Projection -> FieldName -> Bool
projectsField ProjectAll _ = True
projectsField (ProjectFields fs) f = Set.member f fs


-- | Restrict a row to the projected fields ('ProjectAll' is identity).
-- Backends that fetch full rows anyway (in-memory tables) apply this to
-- honor the projection strictly, which keeps callers honest about what
-- they declared.
projectRow :: Projection -> EntityRow -> EntityRow
projectRow ProjectAll row = row
projectRow (ProjectFields fs) row = row {rowFields = Map.restrictKeys (rowFields row) fs}


data LoadResult
  = RowFound EntityRow
  | RowAbsent
  | -- | The entity existed and was deleted; the tombstone version is emitted.
    RowTombstone Text
  deriving stock (Eq, Show)


-- | One resolved collection window.
data Page = Page
  { pageRefs :: [Ref]
  , pageNext :: Maybe Text
  -- ^ Boundary cursors, already encoded (null-terminated, §3.6).
  , pagePrev :: Maybe Text
  , pageTotal :: Maybe Int
  -- ^ Populated only when the collection's 'Lattice.Schema.CountPolicy' says so.
  , pageOverflow :: Bool
  -- ^ A bounded collection exceeded its declared @max@ (with 'Lattice.Schema.Overflow'
  -- policy): the server reports @lattice:collection-overflow@, Edge-scoped.
  }
  deriving stock (Eq, Show)


-- | The window a collection is scanned at.
data Window
  = -- | Bounded collection: return the whole set, capped.
    WWhole Natural
  | -- | Paginated collection.
    WPage
      { wCount :: Natural
      , wDir :: PageDir
      , wAnchor :: Maybe Cursor
      -- ^ Decoded @after@\/@before@\/@around@ cursor.
      }
  deriving stock (Eq, Show)


data PageDir = PageForward | PageBackward | PageAround
  deriving stock (Eq, Show)


-- | An infrastructure failure, reported as a scoped @error@ record (§9.4.2).
data BackendFailure = BackendFailure
  { bfCode :: Text
  -- ^ Protocol vocabulary: @lattice:loader-timeout@, @lattice:upstream-unavailable@, @lattice:internal@.
  , bfRetryable :: Bool
  , bfMessage :: Maybe Text
  }
  deriving stock (Eq, Show)


loaderTimeout :: BackendFailure
loaderTimeout = BackendFailure "lattice:loader-timeout" True Nothing


upstreamUnavailable :: BackendFailure
upstreamUnavailable = BackendFailure "lattice:upstream-unavailable" True Nothing


internalError :: Maybe Text -> BackendFailure
internalError = BackendFailure "lattice:internal" False


data MutationOutcome
  = MutationCommitted CommitResult
  | -- | A declared domain error (§9.4.2): the effect did not commit.
    MutationDomainError A.Value
  | -- | The @allow when@ guard failed.
    MutationDenied
  | -- | The request's 'MutatePrecondition' evaluated false against the
    -- target row's current state; nothing committed. The server renders
    -- the @412@ whose body carries current state (§11.7).
    MutationPreconditionFailed
  | -- | Infrastructure failure; nothing committed.
    MutationFailed BackendFailure
  deriving stock (Show)


{- | A conditional request on a verb-bound mutation (§11.7): the check the
effect bracket must evaluate against the target row's __current__ version
before applying the effect — inside the same transaction, never against a
cache. When the check fails the backend returns
'MutationPreconditionFailed' and commits nothing.
-}
data MutatePrecondition = MutatePrecondition
  { mpTarget :: Ref
  -- ^ The row named by the bound entity URL.
  , mpCheck :: PreCheck
  }
  deriving stock (Eq, Show)


-- | The precondition proper.
data PreCheck
  = -- | @If-Match: \"ver\"@ — proceed only when the row exists at exactly
    -- this version (strong comparison; a tombstoned or absent row fails).
    PreIfMatch Text
  | -- | @If-None-Match: *@ on PUT — create-if-absent: proceed only when
    -- the row has no current representation (absent or tombstoned).
    PreIfAbsent
  deriving stock (Eq, Show)


data CommitResult = CommitResult
  { crResult :: [Ref]
  -- ^ The refs the output selection renders from (usually one).
  , crWrites :: [WriteFact]
  -- ^ Everything the effect touched; checked against the declared write
  -- set and compiled into the invalidation key set.
  , crSnapshot :: SnapshotToken
  }
  deriving stock (Show)


{- | A fact about what a committed effect wrote. The server enforces the
declared write set over these (a fact outside the declaration is
@500 lattice:write-scope@, §11.4) and derives the @invalidated@ record
and CDN purge keys from them.
-}
data WriteFact
  = -- | Entity created or updated (fresh @ver@ visible via 'beLoad').
    WroteEntity Ref
  | -- | Entity deleted: a @tombstone@ record is emitted with this version.
    DeletedEntity Ref Text
  | -- | A collection's membership changed at the given grouping values
    -- (canonical text, one per grouping field).
    WroteCollection CollectionName [Text]
  deriving stock (Eq, Show)


{- | One collection-aggregate grouping instance: the grouping values in
'Lattice.Schema.colGrouping' order, canonical wire text each (§10.5) —
the same values that instantiate the collection's surrogate key.
-}
type GroupKey = [Text]


{- | The dep payloads handed to 'beDerive' for one owning entity,
assembled by the executor from the field's declared read set (§3.7):

* 'dvOwn' — @own(…)@ deps: the named stored fields off the owning row
  (absent row fields are absent here too).
* 'dvEdges' — @\<edge\> ...Fragment@ deps, keyed by edge field name: the
  resolved target ref and the fragment's top-level scalar fields read off
  the target row. An unresolved link or an absent\/tombstoned target row
  leaves the key absent.
* 'dvAggregates' — collection aggregate deps, keyed by the @has many@
  relationship's field name: the aggregate value at this owner's
  grouping key.
-}
data DepValues = DepValues
  { dvOwn :: Map FieldName A.Value
  , dvEdges :: Map FieldName (Ref, Map FieldName A.Value)
  , dvAggregates :: Map FieldName A.Value
  }
  deriving stock (Eq, Show)


emptyDepValues :: DepValues
emptyDepValues = DepValues Map.empty Map.empty Map.empty


{- | The origin backend. All loaders are batched: one call per (type, round),
never per row.
-}
data Backend = Backend
  { beSnapshot :: IO SnapshotToken
  -- ^ The storage snapshot token for the current read (§13.1).
  , beGetRoot :: RootName -> Map ArgName A.Value -> IO (Either BackendFailure (Maybe Ref))
  -- ^ Resolve a @get@ root to at most one entity.
  , beListRoot :: RootName -> Map ArgName A.Value -> Window -> IO (Either BackendFailure Page)
  -- ^ Scan a @list@ root's collection at the given grouping-key arguments.
  , beChildren ::
      TypeName ->
      FieldName ->
      [(Ref, EntityRow)] ->
      Window ->
      IO (Map Ref (Either BackendFailure Page))
  -- ^ Resolve a @has many@ edge for every parent in the round at once
  -- (parent type, edge field, loaded parents). Set-in, map-out. Parent
  -- rows are projected (module haddock, /Projections/): they carry the
  -- link\/grouping fields and the plan's selections; a resolver needing
  -- other parent-side state reads its own storage.
  , beLoad :: TypeName -> Projection -> [Text] -> IO (Map Text (Either BackendFailure LoadResult))
  -- ^ Load entity rows by key, batched per type per round. The
  -- 'Projection' is the static upper bound on the fields the caller will
  -- read (module haddock, /Projections/); returning fuller rows is
  -- always correct.
  , beComputed ::
      TypeName ->
      FieldName ->
      Map ArgName A.Value ->
      EntityRow ->
      IO (Maybe A.Value)
  -- ^ Evaluate an argument-taking field (e.g. @avatarUrl(size: 96)@)
  -- against a loaded row. 'Nothing' elides the field.
  , beMutate ::
      MutationName ->
      Claims ->
      Map ArgName A.Value ->
      Maybe MutatePrecondition ->
      IO MutationOutcome
  -- ^ Run one mutation effect. The server has already checked the guard
  -- against the claims; backends may re-check. Write-set enforcement
  -- happens on the returned 'WriteFact's. A 'MutatePrecondition' (verb
  -- bindings, §11.7) MUST be evaluated inside the effect bracket against
  -- the target row's current version; on failure return
  -- 'MutationPreconditionFailed' without committing.
  , beAggregate ::
      CollectionName ->
      Aggregate ->
      [GroupKey] ->
      IO (Either BackendFailure (Map GroupKey A.Value))
  -- ^ Resolve one collection aggregate for every grouping key of a round
  -- at once (§3.7 Planning). Set-in, map-out; a key absent from the
  -- result elides the dependent derived field for that owner. A 'Left'
  -- fails every owner in the call (Field-scoped errors).
  , beDerive ::
      TypeName ->
      FieldName ->
      Map Text DepValues ->
      IO (Map Text A.Value)
  -- ^ The batched pure compute of one derived field (§3.7), keyed by
  -- owning entity key. Absent keys elide the field (like 'beComputed').
  , beStoreDerived ::
      TypeName ->
      FieldName ->
      Map Text A.Value ->
      IO (Map Text Text)
  -- ^ @maintained@ write-back (§3.7 Materialization): store each value on
  -- its owning row in a bracketed write scoped to exactly that entity,
  -- bumping @ver@ ordinarily. Returns the NEW version per written key;
  -- absent keys (vanished rows) are skipped by the relay.
  }
