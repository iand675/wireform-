{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.Remote
Description : Remote-KV state store backend (Riffle §8)

Kafka Streams' classic KV stores are co-resident with the
worker thread: stores live in-process, in-memory or backed by
a local RocksDB instance. That gives the engine excellent
read latency at the cost of pinning every worker's state to
the host it runs on. Riffle §8 adds an alternative backend
where the store data lives in a /remote distributed KV/
(FoundationDB, TiKV, DynamoDB) — the worker holds only a
read-through cache and the actual rows are persisted by the
remote system.

Why this matters:

  * Restoration is bounded by network round-trips, not state
    size. A 100 GB store comes back online in seconds rather
    than minutes.
  * Active / standby switchover is purely a routing change;
    the new owner just starts talking to the same key range
    on the same remote.
  * Many Kafka Streams use-cases that today push state into
    an external DB and join it back via a global-table pattern
    can collapse into a single state store with stronger
    consistency.

This module defines the contract a remote KV must satisfy and
ships an in-process mock so the chaos suite + downstream
topology tests can drive it without standing up a real
distributed KV. The real adapters (FoundationDB / TiKV /
DynamoDB) live in separate packages because each pulls in its
own driver dependency.
-}
module Kafka.Streams.State.KeyValue.Remote (
  -- * Remote contract
  RemoteKVClient (..),
  RemoteCall (..),
  RemoteError (..),
  RemoteOutcome,

  -- * In-process mock
  inMemoryRemoteKVClient,
  RemoteFaultPolicy (..),
  noRemoteFaults,
  setRemoteFault,
  clearRemoteFault,

  -- * Wrapper
  remoteKeyValueStore,
) where

import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
  StateStore (..),
  StoreName,
 )


----------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------

{- | Identifies the call site for failure injection / metrics /
tracing. Every operation on the remote is one of these.
-}
data RemoteCall
  = RcGet
  | RcPut
  | RcPutIfAbsent
  | RcDelete
  | RcRange
  | RcAll
  | RcApproxEntries
  deriving stock (Eq, Ord, Show, Generic)


{- | Failure mode reported by the remote client. The wrapper
propagates 'Retryable' to the caller via a runtime exception
(the topology runtime handles it the same way it would a
producer fault); 'Fatal' is unrecoverable.
-}
data RemoteError
  = RemoteRetryable !Text
  | RemoteFatal !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)


type RemoteOutcome a = Either RemoteError a


{- | The remote-KV contract. Each method maps to one round-trip
against the underlying distributed KV. The mock implements
this directly; real adapters implement it via their own
client SDK.
-}
data RemoteKVClient k v = RemoteKVClient
  { rkName :: !Text
  , rkGet :: !(k -> IO (RemoteOutcome (Maybe v)))
  , rkPut :: !(k -> v -> IO (RemoteOutcome ()))
  , rkPutIfAbsent :: !(k -> v -> IO (RemoteOutcome (Maybe v)))
  , rkDelete :: !(k -> IO (RemoteOutcome (Maybe v)))
  , rkRange :: !(k -> k -> IO (RemoteOutcome [(k, v)]))
  , rkAll :: !(IO (RemoteOutcome [(k, v)]))
  , rkApproxEntries :: !(IO (RemoteOutcome Int))
  }


----------------------------------------------------------------------
-- In-process mock
----------------------------------------------------------------------

{- | A per-call fault policy. Each 'RemoteCall' can be configured
with a queue of injected errors (FIFO: the first call to that
method gets the first queued error). Empty queue means "no
injection — call succeeds normally".
-}
data RemoteFaultPolicy = RemoteFaultPolicy
  { rfpFaults :: !(TVar (Map RemoteCall [RemoteError]))
  }


noRemoteFaults :: IO RemoteFaultPolicy
noRemoteFaults = RemoteFaultPolicy <$> newTVarIO Map.empty


setRemoteFault :: RemoteFaultPolicy -> RemoteCall -> RemoteError -> IO ()
setRemoteFault p c e = atomically $
  modifyTVar' (rfpFaults p) $ \m ->
    Map.insertWith (\new old -> old <> new) c [e] m


clearRemoteFault :: RemoteFaultPolicy -> RemoteCall -> IO ()
clearRemoteFault p c =
  atomically $
    modifyTVar' (rfpFaults p) (Map.delete c)


{- | Pop the next injected fault for a call, if any. The wrapper
code paths consult this first; an empty list means the call
runs as normal.
-}
popFault :: RemoteFaultPolicy -> RemoteCall -> IO (Maybe RemoteError)
popFault p c = atomically $ do
  m <- readTVar (rfpFaults p)
  case Map.lookup c m of
    Just (e : rest) -> do
      writeTVar
        (rfpFaults p)
        (if null rest then Map.delete c m else Map.insert c rest m)
      pure (Just e)
    _ -> pure Nothing


{- | An in-process 'RemoteKVClient' backed by a 'Data.Map.Strict.Map'
plus a 'RemoteFaultPolicy'. The map is keyed by @k@ so it
requires 'Ord k'.
-}
inMemoryRemoteKVClient
  :: forall k v
   . Ord k
  => Text
  -> RemoteFaultPolicy
  -> IO (RemoteKVClient k v)
inMemoryRemoteKVClient nm policy = do
  ref <- newIORef (Map.empty :: Map k v)
  let withFault call body = do
        mf <- popFault policy call
        case mf of
          Just e -> pure (Left e)
          Nothing -> Right <$> body
  pure
    RemoteKVClient
      { rkName = nm
      , rkGet = \k ->
          withFault RcGet $
            Map.lookup k <$> readIORef ref
      , rkPut = \k v ->
          withFault RcPut $
            atomicModifyIORef' ref (\m -> (Map.insert k v m, ()))
      , rkPutIfAbsent = \k v -> withFault RcPutIfAbsent $ do
          atomicModifyIORef' ref $ \m ->
            case Map.lookup k m of
              Just old -> (m, Just old)
              Nothing -> (Map.insert k v m, Nothing)
      , rkDelete = \k -> withFault RcDelete $
          atomicModifyIORef' ref $ \m ->
            case Map.lookup k m of
              Nothing -> (m, Nothing)
              Just v -> (Map.delete k m, Just v)
      , rkRange = \lo hi -> withFault RcRange $ do
          m <- readIORef ref
          pure
            [ (k, v)
            | (k, v) <- Map.toAscList m
            , k >= lo
            , k <= hi
            ]
      , rkAll =
          withFault RcAll $
            Map.toAscList <$> readIORef ref
      , rkApproxEntries =
          withFault RcApproxEntries $
            Map.size <$> readIORef ref
      }


----------------------------------------------------------------------
-- Wrapper
----------------------------------------------------------------------

{- | Wrap a 'RemoteKVClient' as a 'KeyValueStore'. Synchronous
calls throw 'RemoteError' if the remote returns a fault; the
runtime's exception handler decides whether to retry the
topology, route to the DLQ, or shut down. This mirrors how
the Producer's send failures are handled today.

The wrapper does /not/ cache reads — that's a follow-up. A
read-through cache layered on top would be a Phase 3 item.
-}
remoteKeyValueStore
  :: forall k v
   . StoreName
  -> RemoteKVClient k v
  -> IO (KeyValueStore k v)
remoteKeyValueStore name client = do
  pure
    KeyValueStore
      { kvsCache = Nothing
      , kvsBase =
          StateStore
            { storeStoreName = name
            , storePersistent = True -- the remote durably holds the data
            , storeFlush = pure ()
            , storeClose = pure ()
            }
      , kvsApproxEntries = do
          n <- unwrapM RcApproxEntries (rkApproxEntries client)
          pure (fromIntegral n)
      , kvsGet = \k -> unwrapM RcGet (rkGet client k)
      , kvsPut = \k v -> unwrap_ RcPut (rkPut client k v)
      , kvsPutIfAbsent = \k v ->
          unwrapM
            RcPutIfAbsent
            (rkPutIfAbsent client k v)
      , kvsDelete = \k -> unwrapM RcDelete (rkDelete client k)
      , kvsRange = \lo hi -> do
          rs <- unwrapM RcRange (rkRange client lo hi)
          iterFromList rs
      , kvsAll = do
          rs <- unwrapM RcAll (rkAll client)
          iterFromList rs
      , kvsReverseRange = \lo hi -> do
          rs <- unwrapM RcRange (rkRange client lo hi)
          iterFromList (reverse rs)
      , kvsReverseAll = do
          rs <- unwrapM RcAll (rkAll client)
          iterFromList (reverse rs)
      }
  where
    unwrapM call act = do
      r <- act
      case r of
        Right a -> pure a
        Left e -> failed call e
    unwrap_ call act = do
      r <- act
      case r of
        Right () -> pure ()
        Left e -> failed call e
    failed call err = do
      -- A fatal goes straight up; a retryable also throws and
      -- the runtime's standard handler decides what to do
      -- (e.g. abort the commit cycle, mark the task failed).
      when False (print (call, rkName client)) -- placeholder for tracing
      throwIO err


iterFromList :: [(k, v)] -> IO (KeyValueIterator k v)
iterFromList xs0 = do
  ref <- newIORef xs0
  pure
    KeyValueIterator
      { kvIterNext = atomicModifyIORef' ref $ \xs -> case xs of
          [] -> ([], Nothing)
          (h : tl) -> (tl, Just h)
      , kvIterClose = atomicModifyIORef' ref (\_ -> ([], ()))
      }
