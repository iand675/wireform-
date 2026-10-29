{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.State.KeyValue.Caching
Description : The record cache in front of a table's key-value store

The JVM's record cache (@statestore.cache.max.bytes@): writes to a
cached store are buffered per key, and only the latest one per key
reaches the store, its changelog and the operators downstream — at
the next commit, or earlier when the task's caches outgrow their
share of the budget. A count over a hot key then emits one update per
commit instead of one per record.

  * Reads see the buffered writes: 'kvsGet', ranges and interactive
    queries read through the cache.
  * A flush writes each dirty key to the store underneath, logs it
    (from the bytes serialised when it was written), and hands
    @(key, new, old)@ to the processor's 'FlushListener', which
    forwards the change. @old@ is what the store held before, so a
    run of updates between flushes becomes one change. A key that was
    added and deleted between two flushes is neither written nor
    forwarded.
  * Keys flush in the order of their last write (as the JVM's
    @NamedCache@ does).
  * Closing discards buffered writes: the runtime commits (and so
    flushes) before a clean close, and restores from the changelog
    after anything else.

Window stores ('cachedWindowStore') cache the same way, per key and
window start: a flush writes each dirty window and hands it to the
aggregation processor, which forwards it under its window.

Sizes are the serialised key and value of a logged store's entries,
plus 'entryOverhead'; an unlogged store's entries are counted at
'unloggedEntrySize', since nothing else serialises them.
-}
module Kafka.Streams.State.KeyValue.Caching (
  CacheBudget,
  newCacheBudget,
  cacheBytesUsed,
  CachedStore (..),
  cachedKeyValueStore,
  CachedWindowStore (..),
  cachedWindowStore,
  entryOverhead,
  unloggedEntrySize,
) where

import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Kafka.Streams.Internal.Serdes (int64BE)
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.State.Store (
  FlushListener,
  KeyValueIterator (..),
  KeyValueStore (..),
  StateStore (..),
  StoreCache (..),
  StoreSerdes (..),
  WindowCache (..),
  WindowStore (..),
  WindowedKey (..),
  kvIteratorFromList,
  kvIteratorToList,
 )
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (Headers, emptyHeaders)


{- | What the caches of one task may hold (in bytes, read at every
write: the runtime splits @statestore.cache.max.bytes@ over the
instance's tasks), and what they hold now.
-}
data CacheBudget = CacheBudget
  { cbLimit :: !(IO Int)
  , cbUsed :: !(IORef Int)
  }


newCacheBudget :: IO Int -> IO CacheBudget
newCacheBudget limit = CacheBudget limit <$> newIORef 0


cacheBytesUsed :: CacheBudget -> IO Int
cacheBytesUsed = readIORef . cbUsed


-- | Bytes counted per entry on top of its serialised key and value.
entryOverhead :: Int
entryOverhead = 48


-- | Bytes counted per entry of an unlogged store.
unloggedEntrySize :: Int
unloggedEntrySize = 64


data Entry v = Entry
  { eValue :: !(Maybe v)
  -- ^ 'Nothing': deleted
  , eKey :: !ByteString
  -- ^ The serialised key (logged stores)
  , eBytes :: !(Maybe ByteString)
  -- ^ The serialised value (logged stores; 'Nothing' for a delete)
  , eSize :: {-# UNPACK #-} !Int
  , eTs :: !Timestamp
  , eHeaders :: !Headers
  , eSeq :: {-# UNPACK #-} !Int
  }


data Cache k v = Cache
  { cEntries :: !(Map k (Entry v))
  , cOrder :: !(IntMap k)
  -- ^ Dirty keys by the sequence number of their last write
  , cNext :: {-# UNPACK #-} !Int
  }


emptyCache :: Cache k v
emptyCache = Cache Map.empty IntMap.empty 0


-- | A cached store and what the engine drives it with.
data CachedStore k v = CachedStore
  { csStore :: !(KeyValueStore k v)
  -- ^ The store as processors and queries see it ('kvsCache' set)
  , csFlush :: !(IO Bool)
  -- ^ Flush every dirty entry; whether there was any
  }


{- | Put a cache in front of @inner@, an unlogged store. With
@logged@, flushed entries are also written to the changelog sink
(key bytes, value bytes or 'Nothing' for a delete).
-}
cachedKeyValueStore
  :: forall k v
   . Ord k
  => CacheBudget
  -> Maybe (StoreSerdes k v, ByteString -> Maybe ByteString -> IO ())
  -> KeyValueStore k v
  -> IO (CachedStore k v)
cachedKeyValueStore budget logged inner = do
  ref <- newIORef (emptyCache :: Cache k v)
  listenerRef <- newIORef (\_ _ _ _ _ -> pure () :: IO ())
  let
    write :: k -> Maybe v -> Timestamp -> Headers -> IO ()
    write k mv ts hs = do
      c0 <- readIORef ref
      let prev = Map.lookup k (cEntries c0)
          (!kb, !vb, !size) = case logged of
            Nothing -> (BS.empty, Nothing, unloggedEntrySize)
            Just (sd, _) ->
              let !kb' = maybe (serialize (storeKeySerde sd) k) eKey prev
                  !vb' = serialize (storeValueSerde sd) <$> mv
              in (kb', vb', BS.length kb' + maybe 0 BS.length vb' + entryOverhead)
      delta <- atomicModifyIORef' ref $ \c ->
        let old = Map.lookup k (cEntries c)
            !sq = cNext c
            !e = Entry mv kb vb size ts hs sq
            !c' =
              Cache
                { cEntries = Map.insert k e (cEntries c)
                , cOrder = IntMap.insert sq k (maybe id (IntMap.delete . eSeq) old (cOrder c))
                , cNext = sq + 1
                }
        in (c', size - maybe 0 eSize old)
      used <- atomicModifyIORef' (cbUsed budget) (\u -> let !u' = u + delta in (u', u'))
      limit <- cbLimit budget
      -- over budget: this store's dirty entries go (the JVM evicts
      -- from the cache being written, which flushes all of its dirty
      -- entries)
      when (used > limit) (void flush)

    flush :: IO Bool
    flush = do
      c <- readIORef ref
      if Map.null (cEntries c)
        then pure False
        else do
          listener <- readIORef listenerRef
          forM_ (IntMap.elems (cOrder c)) $ \k ->
            forM_ (Map.lookup k (cEntries c)) $ \e -> do
              old <- kvsGet inner k
              case (eValue e, old) of
                (Nothing, Nothing) -> pure ()
                (new, _) -> do
                  case new of
                    Just v -> kvsPut inner k v
                    Nothing -> void (kvsDelete inner k)
                  forM_ logged $ \(_, emit) -> emit (eKey e) (eBytes e)
                  listener k new old (eTs e) (eHeaders e)
          -- drop what was flushed; a write made meanwhile stays dirty
          freed <- atomicModifyIORef' ref $ \c' ->
            let (gone, kept) = IntMap.spanAntitone (< cNext c) (cOrder c')
                goneKeys = IntMap.elems gone
                !bytes = sum (map (\k -> maybe 0 eSize (Map.lookup k (cEntries c'))) goneKeys)
                !entries = foldr Map.delete (cEntries c') goneKeys
            in (c' {cEntries = entries, cOrder = kept}, bytes)
          atomicModifyIORef' (cbUsed budget) (\u -> (u - freed, ()))
          pure True

    get :: k -> IO (Maybe v)
    get k = do
      c <- readIORef ref
      case Map.lookup k (cEntries c) of
        Just e -> pure (eValue e)
        Nothing -> kvsGet inner k

    -- the store's rows in [lo, hi] (or all of them) with the cache applied
    merged :: Maybe (k, k) -> IO [(k, v)]
    merged bounds = do
      c <- readIORef ref
      base <- maybe (kvsAll inner) (uncurry (kvsRange inner)) bounds >>= kvIteratorToList
      let slice = case bounds of
            Nothing -> cEntries c
            Just (lo, hi) -> Map.takeWhileAntitone (<= hi) (Map.dropWhileAntitone (< lo) (cEntries c))
          -- a cached delete hides the row, a cached value replaces or adds it
          applied =
            Map.mergeWithKey
              (\_ _ e -> eValue e)
              id
              (Map.mapMaybe eValue)
              (Map.fromDistinctAscList base)
              slice
      pure (Map.toAscList applied)

    plain :: k -> Maybe v -> IO ()
    plain k mv = write k mv (Timestamp 0) emptyHeaders

    base = kvsBase inner

  pure
    CachedStore
      { csFlush = flush
      , csStore =
          inner
            { kvsBase =
                base
                  { storeFlush = flush >> storeFlush base
                  }
            , kvsGet = get
            , kvsPut = \k v -> plain k (Just v)
            , kvsPutIfAbsent = \k v -> do
                prev <- get k
                case prev of
                  Nothing -> Nothing <$ plain k (Just v)
                  Just _ -> pure prev
            , kvsDelete = \k -> do
                prev <- get k
                plain k Nothing
                pure prev
            , kvsRange = \lo hi -> merged (Just (lo, hi)) >>= kvIteratorFromList
            , kvsAll = merged Nothing >>= kvIteratorFromList
            , kvsReverseRange = \lo hi -> merged (Just (lo, hi)) >>= kvIteratorFromList . reverse
            , kvsReverseAll = merged Nothing >>= kvIteratorFromList . reverse
            , kvsApproxEntries = do
                n <- kvsApproxEntries inner
                c <- readIORef ref
                pure (n + fromIntegral (Map.size (Map.filter (not . null . eValue) (cEntries c))))
            , kvsCache =
                Just
                  StoreCache
                    { scWrite = write
                    , scOnFlush = writeIORef listenerRef
                    }
            }
      }


data WEntry v = WEntry
  { weValue :: !v
  , weKey :: !ByteString
  , weBytes :: !ByteString
  , weSize :: {-# UNPACK #-} !Int
  , weTs :: !Timestamp
  , weHeaders :: !Headers
  , weSeq :: {-# UNPACK #-} !Int
  }


data WCache k v = WCache
  { wcEntries :: !(Map (k, Timestamp) (WEntry v))
  , wcOrder :: !(IntMap (k, Timestamp))
  , wcNext :: {-# UNPACK #-} !Int
  }


-- | A cached window store and what the engine drives it with.
data CachedWindowStore k v = CachedWindowStore
  { cwStore :: !(WindowStore k v)
  -- ^ The store as processors and queries see it ('wsCache' set)
  , cwFlush :: !(IO Bool)
  }


{- | Put a cache in front of @inner@, an unlogged window store; with
@logged@, flushed windows are also logged (key bytes then the window
start, as the changelog of an uncached window store).
-}
cachedWindowStore
  :: forall k v
   . Ord k
  => CacheBudget
  -> Maybe (StoreSerdes k v, ByteString -> Maybe ByteString -> IO ())
  -> WindowStore k v
  -> IO (CachedWindowStore k v)
cachedWindowStore budget logged inner = do
  ref <- newIORef (WCache Map.empty IntMap.empty 0 :: WCache k v)
  listenerRef <- newIORef (\_ _ _ _ _ -> pure () :: IO ())
  let
    write :: k -> v -> Timestamp -> Timestamp -> Headers -> IO ()
    write k v start@(Timestamp st) ts hs = do
      c0 <- readIORef ref
      let key = (k, start)
          prev = Map.lookup key (wcEntries c0)
          (!kb, !vb, !size) = case logged of
            Nothing -> (BS.empty, BS.empty, unloggedEntrySize)
            Just (sd, _) ->
              let !kb' = maybe (serialize (storeKeySerde sd) k <> int64BE st) weKey prev
                  !vb' = serialize (storeValueSerde sd) v
              in (kb', vb', BS.length kb' + BS.length vb' + entryOverhead)
      delta <- atomicModifyIORef' ref $ \c ->
        let old = Map.lookup key (wcEntries c)
            !sq = wcNext c
            !e = WEntry v kb vb size ts hs sq
            !c' =
              WCache
                { wcEntries = Map.insert key e (wcEntries c)
                , wcOrder = IntMap.insert sq key (maybe id (IntMap.delete . weSeq) old (wcOrder c))
                , wcNext = sq + 1
                }
        in (c', size - maybe 0 weSize old)
      used <- atomicModifyIORef' (cbUsed budget) (\u -> let !u' = u + delta in (u', u'))
      limit <- cbLimit budget
      when (used > limit) (void flush)

    flush :: IO Bool
    flush = do
      c <- readIORef ref
      if Map.null (wcEntries c)
        then pure False
        else do
          listener <- readIORef listenerRef
          forM_ (IntMap.elems (wcOrder c)) $ \key@(k, start) ->
            forM_ (Map.lookup key (wcEntries c)) $ \e -> do
              wsPut inner k (weValue e) start
              forM_ logged $ \(_, emit) -> emit (weKey e) (Just (weBytes e))
              listener k start (weValue e) (weTs e) (weHeaders e)
          freed <- atomicModifyIORef' ref $ \c' ->
            let (gone, kept) = IntMap.spanAntitone (< wcNext c) (wcOrder c')
                goneKeys = IntMap.elems gone
                !bytes = sum (map (\key -> maybe 0 weSize (Map.lookup key (wcEntries c'))) goneKeys)
            in (c' {wcEntries = foldr Map.delete (wcEntries c') goneKeys, wcOrder = kept}, bytes)
          atomicModifyIORef' (cbUsed budget) (\u -> (u - freed, ()))
          pure True

    cachedOf :: WCache k v -> Map (Timestamp, k) v
    cachedOf c = Map.fromList (map (\((k, t), e) -> ((t, k), weValue e)) (Map.toList (wcEntries c)))

    -- every window in [from, to] across keys, the cache applied, by start then key
    allIn :: Maybe (Timestamp, Timestamp) -> IO [(WindowedKey k, v)]
    allIn bounds = do
      c <- readIORef ref
      base <- maybe (wsAll inner) (uncurry (wsFetchAllRange inner)) bounds >>= kvIteratorToList
      let inRange t = maybe True (\(lo, hi) -> t >= lo && t <= hi) bounds
          merged =
            Map.union
              (Map.filterWithKey (\(t, _) _ -> inRange t) (cachedOf c))
              (Map.fromList (map (\(WindowedKey k t, v) -> ((t, k), v)) base))
      pure (map (\((t, k), v) -> (WindowedKey k t, v)) (Map.toAscList merged))

    base_ = wsBase inner

  pure
    CachedWindowStore
      { cwFlush = flush
      , cwStore =
          inner
            { wsBase = base_ {storeFlush = flush >> storeFlush base_}
            , wsPut = \k v t -> write k v t (Timestamp 0) emptyHeaders
            , wsFetch = \k t -> do
                c <- readIORef ref
                case Map.lookup (k, t) (wcEntries c) of
                  Just e -> pure (Just (weValue e))
                  Nothing -> wsFetch inner k t
            , wsFetchRange = \k from to -> do
                c <- readIORef ref
                base <- wsFetchRange inner k from to >>= kvIteratorToList
                let cached =
                      Map.fromList
                        ( map
                            (\((_, t), e) -> (t, weValue e))
                            -- the key's windows in [from, to]: one slice of the (key, start) order
                            (Map.toList (Map.takeWhileAntitone (<= (k, to)) (Map.dropWhileAntitone (< (k, from)) (wcEntries c))))
                        )
                kvIteratorFromList (Map.toAscList (Map.union cached (Map.fromList base)))
            , wsFetchAllRange = \from to -> allIn (Just (from, to)) >>= kvIteratorFromList
            , wsAll = allIn Nothing >>= kvIteratorFromList
            , wsCache =
                Just
                  WindowCache
                    { wcWrite = write
                    , wcOnFlush = writeIORef listenerRef
                    }
            }
      }
