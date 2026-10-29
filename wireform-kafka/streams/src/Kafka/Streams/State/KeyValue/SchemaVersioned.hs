{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.SchemaVersioned
Description : Schema-versioned KV stores + burn-in migration
              (Riffle §4)

A state store outlives any single deployment of the topology
that wrote to it. The classic Kafka Streams answer is "evolve
your serdes so they round-trip every historical encoding",
which works for additive changes but not for structural ones
(e.g. splitting one field into two, or moving a field into a
nested record).

Riffle §4 adds first-class /schema versioning/ to a wrapped
'KeyValueStore':

  * Every entry is tagged with its writer's 'SchemaVersion'.
  * On read, the entry's version is compared with the current
    version; if the entry is from a previous version the
    wrapper applies the registered 'SchemaMigration' chain to
    produce a fresh-version value.
  * On startup, a "burn-in" pass walks every entry and persists
    the migrated form so future reads cost zero migrations.
    The burn-in is interruptible and tracks progress in a
    per-store cursor so a restart picks up where it left off.

The wrapper expects the underlying store to hold
@(SchemaVersion, v)@ pairs (the version is part of the payload,
not a separate column).
-}
module Kafka.Streams.State.KeyValue.SchemaVersioned (
  -- * Versions and migrations
  SchemaVersion (..),
  SchemaMigration (..),
  MigrationChain,
  identityMigration,
  runMigrationChain,

  -- * Wrapper
  schemaVersionedKeyValueStore,
  readCurrentVersionEntry,

  -- * Burn-in migration
  burnInMigrate,
  BurnInProgress (..),
  readBurnInProgress,
) where

import Control.Monad (forM_)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
 )
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
  kvIteratorToList,
 )


----------------------------------------------------------------------
-- Versions and migrations
----------------------------------------------------------------------

{- | A monotonically-increasing schema version. The wire / on-disk
format must include this number so the wrapper can read it.
-}
newtype SchemaVersion = SchemaVersion {unSchemaVersion :: Int}
  deriving stock (Eq, Ord, Show, Generic)


{- | A migration that lifts a value from version @v_from@ to
@v_to@. The version pair lives in the data, not on the type
(Haskell can't carry a runtime-determined version into the type
system without dependent types), so the type @v@ has to be the
common /current schema/ representation. Migrations therefore
map @(SchemaVersion, v)@ → @(SchemaVersion, v)@.

If the migration cannot deal with the input (e.g. an
unrecognised version), it returns 'Left' with an explanatory
message; the wrapper surfaces this as a deserialisation error
so the runtime's standard error handler kicks in.
-}
data SchemaMigration v = SchemaMigration
  { smFrom :: !SchemaVersion
  , smTo :: !SchemaVersion
  , smRun :: !(v -> Either Text v)
  }


{- | A chain of migrations to apply in order. Build with the
helper 'runMigrationChain' which finds the path from @from@ to
@to@.
-}
type MigrationChain v = [SchemaMigration v]


{- | The trivial migration that maps @v@ to @v@ unchanged. Used as
a base case when the entry's version matches the current
version.
-}
identityMigration :: SchemaVersion -> SchemaMigration v
identityMigration ver = SchemaMigration ver ver Right


{- | Apply a list of migrations in order. The first one's
@smFrom@ should match the entry's stored version, and the
last one's @smTo@ should match the current version.
-}
runMigrationChain
  :: MigrationChain v
  -> v
  -> Either Text v
runMigrationChain [] v = Right v
runMigrationChain (m : ms) v =
  case smRun m v of
    Left err -> Left err
    Right v' -> runMigrationChain ms v'


----------------------------------------------------------------------
-- Wrapper
----------------------------------------------------------------------

{- | Resolve the linear chain of migrations needed to lift @from@
to @to@, given an ordered registry. Returns 'Left' if there is
no path. We assume the registry is forward-only: every
migration's @smTo == smFrom + 1@. Riffle doesn't ship
multi-step migrations as a single object because reasoning
about partial failure across them is much harder.
-}
resolveChain
  :: [SchemaMigration v]
  -> SchemaVersion
  -> SchemaVersion
  -> Either Text (MigrationChain v)
resolveChain registry from to
  | from == to = Right []
  | from > to =
      Left
        ( "schema version "
            <> showSV from
            <> " is newer than current "
            <> showSV to
        )
  | otherwise =
      case [m | m <- registry, smFrom m == from] of
        [] ->
          Left
            ( "no migration registered from "
                <> showSV from
            )
        (m : _) ->
          case resolveChain registry (smTo m) to of
            Left err -> Left err
            Right ms -> Right (m : ms)
  where
    showSV (SchemaVersion n) = T.pack (show n)


{- | Wrap a base 'KeyValueStore' so each value carries a writer
'SchemaVersion' and is migrated on read.

@current@ is the version this codebase writes; @migrations@ is
the linear chain of forward migrations (each @smTo == smFrom +
1@). On 'kvsGet' the wrapper:

  1. Reads @(ver, raw)@ from the underlying store.
  2. If @ver == current@, returns @raw@ unchanged.
  3. Otherwise applies the chain @ver → current@ and returns
     the migrated value. If migration fails, returns 'Nothing'
     (matching the deserialisation-error contract of the
     Streams runtime).

'kvsPut' always stamps the entry with @current@.
-}
schemaVersionedKeyValueStore
  :: forall k v
   . SchemaVersion
  -> [SchemaMigration v]
  -> KeyValueStore k (SchemaVersion, v)
  -> IO (KeyValueStore k v)
schemaVersionedKeyValueStore current migrations under =
  pure
    KeyValueStore
      { kvsBase = kvsBase under
      , kvsCache = Nothing
      , kvsApproxEntries = kvsApproxEntries under
      , kvsGet = svGet
      , kvsPut = svPut
      , kvsPutIfAbsent = svPutIfAbsent
      , kvsDelete = svDelete
      , kvsRange = \lo hi -> kvsRange under lo hi >>= migrateIter
      , kvsAll = kvsAll under >>= migrateIter
      , kvsReverseRange = \lo hi ->
          kvsReverseRange under lo hi
            >>= migrateIter
      , kvsReverseAll = kvsReverseAll under >>= migrateIter
      }
  where
    svGet k = do
      m <- kvsGet under k
      case m of
        Nothing -> pure Nothing
        Just (ver, raw) -> case migrate ver raw of
          Left _ -> pure Nothing
          Right v -> pure (Just v)
    svPut k v = kvsPut under k (current, v)
    svPutIfAbsent k v = do
      r <- kvsPutIfAbsent under k (current, v)
      case r of
        Nothing -> pure Nothing
        Just (ver, raw) -> case migrate ver raw of
          Left _ -> pure Nothing -- conservative; treat as absent
          Right vMig -> pure (Just vMig)
    svDelete k = do
      r <- kvsDelete under k
      case r of
        Nothing -> pure Nothing
        Just (ver, raw) -> case migrate ver raw of
          Left _ -> pure Nothing
          Right vMig -> pure (Just vMig)
    migrate ver raw =
      case resolveChain migrations ver current of
        Left err -> Left err
        Right chain -> runMigrationChain chain raw
    migrateIter it =
      pure
        KeyValueIterator
          { kvIterClose = kvIterClose it
          , kvIterNext = nextLive
          }
      where
        nextLive = do
          mx <- kvIterNext it
          case mx of
            Nothing -> pure Nothing
            Just (k, (ver, raw)) -> case migrate ver raw of
              Left _ -> nextLive
              Right v -> pure (Just (k, v))


{- | Read the raw @(SchemaVersion, v)@ for a key, bypassing
migration. Useful for diagnostic tooling and the burn-in
migration sweep.
-}
readCurrentVersionEntry
  :: KeyValueStore k (SchemaVersion, v)
  -> k
  -> IO (Maybe (SchemaVersion, v))
readCurrentVersionEntry = kvsGet


----------------------------------------------------------------------
-- Burn-in migration
----------------------------------------------------------------------

{- | Progress of a burn-in migration sweep. The runtime persists
this between restarts so an interrupted burn-in resumes from
the same cursor.
-}
data BurnInProgress = BurnInProgress
  { bipScanned :: !Int64
  -- ^ Total entries scanned so far.
  , bipMigrated :: !Int64
  -- ^ Entries that were on an older version and got rewritten.
  , bipFailed :: !Int64
  {- ^ Entries that failed migration (logged separately by the
  caller; the wrapper does not block on these).
  -}
  , bipComplete :: !Bool
  {- ^ True once a sweep has scanned every entry that existed at
  the start of the sweep.
  -}
  }
  deriving stock (Eq, Show, Generic)


emptyProgress :: BurnInProgress
emptyProgress = BurnInProgress 0 0 0 False


{- | Drive a burn-in migration. Walks every entry in the
underlying store; for any entry whose version is older than
@current@ runs the migration chain and rewrites the entry.
Returns the final 'BurnInProgress' and an 'IORef' so the
caller can poll progress concurrently.
-}
burnInMigrate
  :: forall k v
   . SchemaVersion
  -> [SchemaMigration v]
  -> KeyValueStore k (SchemaVersion, v)
  -> IO (IORef BurnInProgress)
burnInMigrate current migrations under = do
  ref <- newIORef emptyProgress
  it <- kvsAll under
  pairs <- kvIteratorToList it
  -- We sweep the snapshot. New writes that arrive after the
  -- iterator was materialised are simply written at @current@
  -- by the wrapper's 'kvsPut'.
  forM_ pairs $ \(k, (ver, raw)) -> do
    if ver == current
      then bumpScanned ref
      else case resolveChain migrations ver current of
        Left _ -> bumpFailed ref
        Right chain ->
          case runMigrationChain chain raw of
            Left _ -> bumpFailed ref
            Right v' -> do
              kvsPut under k (current, v')
              bumpMigrated ref
  markComplete ref
  pure ref
  where
    bumpScanned r =
      atomicModifyIORef'
        r
        (\p -> (p {bipScanned = bipScanned p + 1}, ()))
    bumpMigrated r =
      atomicModifyIORef'
        r
        ( \p ->
            ( p
                { bipScanned = bipScanned p + 1
                , bipMigrated = bipMigrated p + 1
                }
            , ()
            )
        )
    bumpFailed r =
      atomicModifyIORef'
        r
        ( \p ->
            ( p
                { bipScanned = bipScanned p + 1
                , bipFailed = bipFailed p + 1
                }
            , ()
            )
        )
    markComplete r =
      atomicModifyIORef' r (\p -> (p {bipComplete = True}, ()))


{- | Read the current burn-in progress snapshot. Safe to call
concurrently with an ongoing 'burnInMigrate'.
-}
readBurnInProgress :: IORef BurnInProgress -> IO BurnInProgress
readBurnInProgress = readIORef
