{- | The origin backend contract: what a deployment supplies to
"Lattice.Server" to serve a schema.

The shape enforces the protocol's central execution constraint (§3.1):
loaders are set-in, map-out. There is no per-parent resolution type; the
executor always hands the backend the full key set of a round, and N+1 is
inexpressible.

Loads are policy-free: backends fetch rows by key and know nothing about
callers. Visibility is applied at emission by the server, per response,
against the response's slice and claims (§6.9).
-}
module Lattice.Backend (
  Backend (..),
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
) where

import Data.Aeson qualified as A
import Data.Map.Strict (Map)
import Data.Text (Text)
import Lattice.Cursor (Cursor)
import Lattice.Types
import Numeric.Natural (Natural)


-- | One entity's stored state: a version token and its field values.
data EntityRow = EntityRow
  { rowVer :: Text
  , rowFields :: Map FieldName A.Value
  }
  deriving stock (Eq, Show)


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
  | -- | Infrastructure failure; nothing committed.
    MutationFailed BackendFailure
  deriving stock (Show)


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
  -- (parent type, edge field, loaded parents). Set-in, map-out.
  , beLoad :: TypeName -> [Text] -> IO (Map Text (Either BackendFailure LoadResult))
  -- ^ Load entity rows by key, batched per type per round.
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
      IO MutationOutcome
  -- ^ Run one mutation effect. The server has already checked the guard
  -- against the claims; backends may re-check. Write-set enforcement
  -- happens on the returned 'WriteFact's.
  }
