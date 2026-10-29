{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.Session.InMemory
Description : In-memory session store

Sessions are stored as @Map (sessionEnd, key, sessionStart)
value@. The @sessionEnd@ as primary axis lets 'ssFindSessions'
(which is bounded by @[earliestSessionEndTime, latestSessionStartTime]@)
short-circuit cheaply.
-}
module Kafka.Streams.State.Session.InMemory (
  inMemorySessionStore,
  inMemorySessionStoreBuilder,
) where

import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Kafka.Streams.State.Store (
  SessionKey (..),
  SessionStore (..),
  StateStore (..),
  StoreBuilderS (..),
  StoreName,
  defaultLoggingConfig,
  kvIteratorFromList,
 )
import Kafka.Streams.Time (Timestamp (..))


inMemorySessionStore
  :: forall k v
   . Ord k
  => StoreName
  -> Int64 -- retention in ms
  -> IO (SessionStore k v)
inMemorySessionStore nm retention = do
  ref <- newIORef (Map.empty :: Map (Timestamp, k, Timestamp) v)
  pure (mkStore nm retention ref)


mkStore
  :: forall k v
   . Ord k
  => StoreName
  -> Int64
  -> IORef (Map (Timestamp, k, Timestamp) v)
  -> SessionStore k v
mkStore nm retention ref =
  SessionStore
    { ssBase =
        StateStore
          { storeStoreName = nm
          , storePersistent = False
          , storeFlush = pure ()
          , storeClose = writeIORef ref Map.empty
          }
    , ssRetention = retention
    , ssPut = \(SessionKey k start end) v ->
        atomicModifyIORef' ref $ \m ->
          let !m' = Map.insert (end, k, start) v m
              !m'' = expire retention end m'
          in (m'', ())
    , ssRemove = \(SessionKey k start end) ->
        atomicModifyIORef' ref $ \m ->
          let !m' = Map.delete (end, k, start) m in (m', ())
    , ssFetchSession = \(SessionKey k start end) -> do
        m <- readIORef ref
        pure (Map.lookup (end, k, start) m)
    , ssFindSessions = \k earliestEnd latestStart -> do
        m <- readIORef ref
        let !sub =
              Map.takeWhileAntitone
                (\(e, _, _) -> e <= maxBoundT)
                $ Map.dropWhileAntitone
                  (\(e, _, _) -> e < earliestEnd)
                  m
            !out =
              [ (SessionKey k' s e, v)
              | ((e, k', s), v) <- Map.toAscList sub
              , k' == k
              , s <= latestStart
              ]
        kvIteratorFromList out
    , ssFindAllSessions = \earliestEnd latestStart -> do
        m <- readIORef ref
        let !sub =
              Map.takeWhileAntitone
                (\(_, _, _) -> True)
                $ Map.dropWhileAntitone
                  (\(e, _, _) -> e < earliestEnd)
                  m
            !out =
              [ (SessionKey k s e, v)
              | ((e, k, s), v) <- Map.toAscList sub
              , s <= latestStart
              ]
        kvIteratorFromList out
    }
  where
    maxBoundT = Timestamp maxBound


expire
  :: Int64
  -> Timestamp
  -> Map (Timestamp, k, Timestamp) v
  -> Map (Timestamp, k, Timestamp) v
expire retention (Timestamp now) m =
  let !cutoff = Timestamp (now - retention)
  in Map.dropWhileAntitone (\(e, _, _) -> e < cutoff) m


inMemorySessionStoreBuilder
  :: Ord k
  => StoreName
  -> Int64
  -> StoreBuilderS k v
inMemorySessionStoreBuilder nm retention =
  StoreBuilderS
    { sbSName = nm
    , sbSLogging = defaultLoggingConfig
    , sbSSerdes = Nothing
    , sbSBuild = \_ -> inMemorySessionStore nm retention
    }
