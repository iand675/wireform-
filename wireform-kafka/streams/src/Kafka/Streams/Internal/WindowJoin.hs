{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.Internal.WindowJoin
Description : The processors of a KStream-KStream window join

A window join keeps each input's recent records in a window store
(a list per key and timestamp, so records with the same key and
timestamp are all kept), and joins every record against the other
input's store:

  * a left record at @t@ joins right records in
    @[t - before, t + after]@; a right record at @t@ joins left records
    in the mirror range @[t - after, t + before]@;
  * a joined record carries the later of the two timestamps;
  * both sides share one stream time per task (the latest timestamp
    either has seen); a record whose window closed more than the grace
    period ago is dropped.

Left and outer joins hold a record that found no match in a shared
"outer" store until its window closes, and emit it with the other
side missing only then, and only if nothing matched it meanwhile (the
JVM's behaviour since KIP-633; before it such records were emitted at
once, followed by the real match). A record that arrives after its
window already closed is emitted at once. The held records are
emitted in timestamp order as stream time passes their window's end.
-}
module Kafka.Streams.Internal.WindowJoin (
  Side (..),
  OuterKey (..),
  outerKeySerde,
  TimeTrackers,
  newTimeTrackers,
  SideConfig (..),
  sideProcessor,
  SelfJoinConfig (..),
  selfJoinProcessor,
  appendAt,
) where

import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified
import Kafka.Streams.Internal.Serdes (readInt64BE)
import Kafka.Streams.Processor (
  Processor (..),
  ProcessorContext (..),
  TaskId,
  forwardRecord,
  getStateStore,
  processorName,
 )
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueIterator (..),
  KeyValueStore (..),
  StoreName,
  WindowStore (..),
  kvIteratorToList,
 )
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (Headers, Record (..))
import Unsafe.Coerce (unsafeCoerce)


-- | Which input of the join a record came in on.
data Side = RightSide | LeftSide
  -- ^ In this order, as the JVM's outer store orders them at one timestamp
  deriving stock (Eq, Ord, Show)


-- | A held, so far unmatched, record: its timestamp, side and key.
data OuterKey k = OuterKey
  { okTs :: !Int64
  , okSide :: !Side
  , okKey :: !k
  }
  deriving stock (Eq, Ord, Show)


-- | Timestamp (8 bytes big-endian), side (1 byte: 1 left), key.
outerKeySerde :: Serde k -> Serde (OuterKey k)
outerKeySerde ks =
  Serde
    { serialize = \(OuterKey ts side k) ->
        BSL.toStrict
          ( BB.toLazyByteString
              (BB.int64BE ts <> BB.word8 (if side == LeftSide then 1 else 0) <> BB.byteString (serialize ks k))
          )
    , deserialize = \bs -> case (readInt64BE bs, BS.indexMaybe bs 8) of
        (Just ts, Just tag) -> OuterKey ts (if tag == 1 then LeftSide else RightSide) <$> deserialize ks (BS.drop 9 bs)
        _ -> Left "outer join key: truncated"
    , serializeHeaders = const mempty
    }


{- | One stream time per task, shared by a join's two sides (the JVM's
@TimeTrackerSupplier@). A side takes its task's at init and drops it
at close, so a rebuilt task starts afresh.
-}
newtype TimeTrackers = TimeTrackers (IORef (Map TaskId (IORef Int64)))


newTimeTrackers :: IO TimeTrackers
newTimeTrackers = TimeTrackers <$> newIORef Map.empty


trackerFor :: TimeTrackers -> TaskId -> IO (IORef Int64)
trackerFor (TimeTrackers ref) tid = do
  fresh <- newIORef minBound
  atomicModifyIORef' ref $ \m -> case Map.lookup tid m of
    Just t -> (m, t)
    Nothing -> (Map.insert tid fresh m, fresh)


dropTracker :: TimeTrackers -> TaskId -> IO ()
dropTracker (TimeTrackers ref) tid = atomicModifyIORef' ref (\m -> (Map.delete tid m, ()))


-- | Advance the stream time to @ts@; the stream time after.
advance :: IORef Int64 -> Int64 -> IO Int64
advance ref ts = atomicModifyIORef' ref (\cur -> let !n = max cur ts in (n, n))


-- | Add @v@ to the values kept for @k@ at @ts@.
appendAt :: WindowStore k [v] -> k -> v -> Int64 -> IO ()
appendAt ws k v ts = do
  prev <- wsFetch ws k (Timestamp ts)
  wsPut ws k (maybe [v] (<> [v]) prev) (Timestamp ts)


-- | Everything kept for @k@ in @[lo, hi]@, oldest first.
fetchAll :: WindowStore k [v] -> k -> Int64 -> Int64 -> IO [(Int64, v)]
fetchAll ws k lo hi = do
  rows <- wsFetchRange ws k (Timestamp lo) (Timestamp hi) >>= kvIteratorToList
  pure (concatMap (\(Timestamp t, vs) -> map ((,) t) vs) rows)


-- | One side of a window join.
data SideConfig k vSelf vOther v1 v2 vo = SideConfig
  { scSide :: !Side
  , scBack :: !Int64
  -- ^ How far before a record of this side its matches may be (left: @before@, right: @after@)
  , scAhead :: !Int64
  -- ^ How far after (left: @after@, right: @before@)
  , scBefore :: !Int64
  -- ^ The windows' @before@ and @after@, for when held records' windows close
  , scAfter :: !Int64
  , scGrace :: !Int64
  , scJoin :: !(vSelf -> vOther -> vo)
  -- ^ Join a record of this side with one of the other
  , scUnmatched :: !(Maybe (vSelf -> vo))
  -- ^ Left / outer joins: what this side emits for a record nothing matched
  , scHold :: !(vSelf -> Either v1 v2)
  , scRender :: !(Either v1 v2 -> Maybe vo)
  -- ^ What a held record of either side emits once its window closes
  , scSelfStore :: !StoreName
  , scOtherStore :: !StoreName
  , scOuterStore :: !(Maybe StoreName)
  , scTimes :: !TimeTrackers
  , scName :: !Data.Text.Text
  }


bind :: ProcessorContext -> StoreName -> IO AnyStateStore
bind ctx sn =
  getStateStore ctx sn >>= \case
    Just st -> pure st
    Nothing -> error ("window join: store missing: " <> show sn)


asWindow :: AnyStateStore -> WindowStore k v
asWindow = \case
  AnyWindowStore ws -> unsafeCoerce ws
  _ -> error "window join: expected a window store"


asKV :: AnyStateStore -> KeyValueStore k v
asKV = \case
  AnyKeyValueStore kvs -> unsafeCoerce kvs
  _ -> error "window join: expected a key-value store"


data Bound k vSelf vOther v1 v2 = Bound
  { bCtx :: !ProcessorContext
  , bSelf :: !(WindowStore k [vSelf])
  , bOther :: !(WindowStore k [vOther])
  , bOuter :: !(Maybe (KeyValueStore (OuterKey k) [Either v1 v2]))
  , bTime :: !(IORef Int64)
  }


sideProcessor :: forall k vSelf vOther v1 v2 vo. SideConfig k vSelf vOther v1 v2 vo -> IO (Processor k vSelf)
sideProcessor cfg = do
  ref <- newIORef Nothing
  pure
    Processor
      { procName = processorName cfg.scName
      , procInit = \ctx -> do
          self_ <- asWindow <$> bind ctx cfg.scSelfStore
          other <- asWindow <$> bind ctx cfg.scOtherStore
          outer <- traverse (fmap asKV . bind ctx) cfg.scOuterStore
          time <- trackerFor cfg.scTimes (ctxTaskId ctx)
          writeIORef ref (Just (Bound ctx self_ other outer time :: Bound k vSelf vOther v1 v2))
      , procClose = readIORef ref >>= mapM_ (\b -> dropTracker cfg.scTimes (ctxTaskId b.bCtx))
      , procProcess = \r -> do
          mb <- readIORef ref
          case (mb, recordKey r) of
            (Just b, Just k) -> process b k r
            _ -> pure ()
      }
  where
    process :: Bound k vSelf vOther v1 v2 -> k -> Record k vSelf -> IO ()
    process b k r = do
      let Timestamp ts = recordTimestamp r
          v = recordValue r
          ctx = b.bCtx
          hs = recordHeaders r
      appendAt b.bSelf k v ts
      st <- advance b.bTime ts
      -- dropped: its window closed more than the grace period ago
      unless (ts + cfg.scAhead + cfg.scGrace < st) $ do
        when (ts == st) $ mapM_ (\o -> emitClosed ctx o st hs) b.bOuter
        matches <- fetchAll b.bOther k (max 0 (ts - cfg.scBack)) (max 0 (ts + cfg.scAhead))
        forM_ matches $ \(ots, ov) -> do
          -- a held record of the other side has found its match
          forM_ b.bOuter $ \o -> kvsDelete o (OuterKey ots (opposite cfg.scSide) k)
          forwardRecord ctx (Record (Just k) (cfg.scJoin v ov) (Timestamp (max ts ots)) hs)
        case cfg.scUnmatched of
          Just unmatched | null matches -> case b.bOuter of
            Just o | ts + cfg.scAhead >= st -> do
              let key = OuterKey ts cfg.scSide k
              prev <- kvsGet o key
              kvsPut o key (maybe [cfg.scHold v] (<> [cfg.scHold v]) prev)
            _ -> forwardRecord ctx (Record (Just k) (unmatched v) (Timestamp ts) hs)
          _ -> pure ()

    -- emit, oldest first, the held records whose window has closed
    emitClosed :: ProcessorContext -> KeyValueStore (OuterKey k) [Either v1 v2] -> Int64 -> Headers -> IO ()
    emitClosed ctx o st hs = do
      it <- kvsAll o
      let go leftOpen rightOpen done
            | leftOpen && rightOpen = pure done
            | otherwise =
                kvIterNext it >>= \case
                  Nothing -> pure done
                  Just (key@(OuterKey ots side k), vals)
                    | ots + (if side == LeftSide then cfg.scAfter else cfg.scBefore) + cfg.scGrace >= st ->
                        go (leftOpen || side == LeftSide) (rightOpen || side == RightSide) done
                    | otherwise -> do
                        forM_ vals $ \hv ->
                          forM_ (cfg.scRender hv) $ \out -> forwardRecord ctx (Record (Just k) out (Timestamp ots) hs)
                        go leftOpen rightOpen (key : done)
      closed <- go False False []
      kvIterClose it
      mapM_ (kvsDelete o) closed


opposite :: Side -> Side
opposite = \case
  LeftSide -> RightSide
  RightSide -> LeftSide


-- | An inner join of a stream with itself, over one store (KIP-862).
data SelfJoinConfig v vo = SelfJoinConfig
  { sjBefore :: !Int64
  , sjAfter :: !Int64
  , sjGrace :: !Int64
  , sjJoin :: !(v -> v -> vo)
  , sjStore :: !StoreName
  , sjTimes :: !TimeTrackers
  , sjName :: !Data.Text.Text
  }


{- | The self-join's one processor. It emits exactly what the two-sided
join of the stream with itself would, in the same order: for a record
@b@ at @t@, first @join b x@ for every earlier record @x@ the left side
would match, then @join y b@ for every @y@ the right side would match,
with @join b b@ where @b@ itself falls among them (after the records
with its timestamp).
-}
selfJoinProcessor :: forall k v vo. SelfJoinConfig v vo -> IO (Processor k v)
selfJoinProcessor cfg = do
  ref <- newIORef Nothing
  pure
    Processor
      { procName = processorName cfg.sjName
      , procInit = \ctx -> do
          ws <- asWindow <$> bind ctx cfg.sjStore
          time <- trackerFor cfg.sjTimes (ctxTaskId ctx)
          writeIORef ref (Just (ctx, ws :: WindowStore k [v], time))
      , procClose = readIORef ref >>= mapM_ (\(ctx, _, _) -> dropTracker cfg.sjTimes (ctxTaskId ctx))
      , procProcess = \r -> do
          m <- readIORef ref
          case (m, recordKey r) of
            (Just (ctx, ws, time), Just k) -> do
              let Timestamp ts = recordTimestamp r
                  v = recordValue r
                  hs = recordHeaders r
                  emit out t = forwardRecord ctx (Record (Just k) out (Timestamp t) hs)
              -- the left side's turn: it sees the earlier records only
              asLeft <- fetchAll ws k (max 0 (ts - cfg.sjBefore)) (max 0 (ts + cfg.sjAfter))
              appendAt ws k v ts
              st <- advance time ts
              -- the two sides drop a late record alike when the window is symmetric;
              -- each checks its own reach otherwise
              let leftLate = ts + cfg.sjAfter + cfg.sjGrace < st
                  rightLate = ts + cfg.sjBefore + cfg.sjGrace < st
              unless leftLate $ forM_ asLeft $ \(ots, ov) -> emit (cfg.sjJoin v ov) (max ts ots)
              unless rightLate $ do
                -- the right side's turn: it sees this record too
                asRight <- fetchAll ws k (max 0 (ts - cfg.sjAfter)) (max 0 (ts + cfg.sjBefore))
                forM_ asRight $ \(ots, ov) -> emit (cfg.sjJoin ov v) (max ts ots)
            _ -> pure ()
      }
