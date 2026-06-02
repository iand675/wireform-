{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.ForeignKeyJoin
-- Description : KTable-KTable foreign-key join (KIP-213)
--
-- Joins a left 'KTable' on a /derived/ key — extracted from the
-- left value — into a right 'KTable' that is partitioned on that
-- derived key. The join result is itself a 'KTable', materialised
-- into a fresh store.
--
-- == KIP-213 implementation notes
--
-- The Java semantics drive a four-topic protocol (subscription,
-- subscription-response, foreign-key state, output). In a
-- single-task in-process runtime the partitioning collapses, but
-- the /tracking/ does not: when the left side updates @(k, v)@ to
-- a new foreign key @fk@, we must remember that @k@ is now
-- subscribed to @fk@ (and unsubscribe from the previous @fk@), so
-- that subsequent right-side updates on @fk@ re-emit the join
-- for @k@.
--
-- This combinator follows the KIP-213 contract end-to-end:
--
--   * Each left record carries a /subscription token/ derived
--     from the value's hash. The right side stores the token
--     alongside the subscription set.
--   * When the right side emits a responder for a subscriber, it
--     verifies the token still matches the live left value's
--     token. A mismatch (the left value has been replaced and a
--     fresh subscription is pending) drops the responder; the
--     pending subscription will trigger the correct emit when it
--     processes its own join.
--
-- In a single-task synchronous topology the token verification is
-- redundant but harmless (the right-side processor always sees
-- the live left state). In a future multi-task wiring it is the
-- correctness invariant: subscription / responder messages flow
-- through internal topics and timing skew across tasks is real.
--
-- == State stores used
--
--   * @subscriptions@ — @fk -> 'Map' k 'SubscriptionToken'@:
--     which left keys have subscribed to a given foreign key,
--     paired with the token that was current at subscription
--     time.
--   * @left-state@ — @k -> v_left@: the latest left-side value
--     per left key (used by right-side updates to recompute
--     joins).
--   * @left-token@ — @k -> 'SubscriptionToken'@: the latest token
--     seen for the left key. Compared against subscription
--     tokens during right-side responder verification.
--   * @last-fk@ — @k -> fk@: the foreign key the left key was
--     last subscribed to. Used for unsubscribe-on-change.
--   * Materialised output store: the join result.
{-# LANGUAGE TypeApplications #-}

module Kafka.Streams.ForeignKeyJoin
  ( foreignKeyJoinKTable
  , leftForeignKeyJoinKTable
  , foreignKeyJoinKTableWith
  , leftForeignKeyJoinKTableWith
  ) where

import Data.Hashable (Hashable, hash)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Unsafe.Coerce as Unsafe
import GHC.Generics (Generic)

import Kafka.Streams.KTable
  ( KTable (..)
  , ktableBuilder
  , ktableKeySerde
  , ktableNode
  , ktableStore
  )
import Kafka.Streams.Materialized
  ( Materialized (..)
  )
import Kafka.Streams.StreamsBuilder
  ( freshNodeName
  , freshStoreName
  , withTopology_
  )
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext
  , forwardRecord
  , getStateStore
  , processorName
  )
import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStoreBuilder
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , StoreBuilderKV
  , StoreName
  )
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Joined as Joined
import Kafka.Streams.Serde (HasSerde (..))
import qualified Data.Text as T
import Kafka.Streams.Types (Record (..))

-- | Internal: token derived from a left value's hash. Stamped on
-- every subscription so the right side can detect (and drop) a
-- responder that refers to a left value the publisher has since
-- replaced.
newtype SubscriptionToken = SubscriptionToken { unToken :: Int }
  deriving stock (Eq, Ord, Show, Generic)

mkToken :: Hashable v => v -> SubscriptionToken
mkToken v = SubscriptionToken (hash v)

-- | Inner foreign-key KTable-KTable join. Implements KIP-213 with
-- subscription token verification.
foreignKeyJoinKTable
  :: forall k v fk vr v'
   . (Ord k, Ord fk, Hashable v, HasSerde v')
  => (v -> fk)                 -- foreign-key extractor
  -> (v -> vr -> v')           -- joiner
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
foreignKeyJoinKTable extractor joiner =
  buildFKJoin "FK-JOIN" extractor (FKModeInner joiner)

-- | Left foreign-key KTable-KTable join. The joiner gets a 'Maybe'
-- on the right side; left records always emit at least once.
leftForeignKeyJoinKTable
  :: forall k v fk vr v'
   . (Ord k, Ord fk, Hashable v, HasSerde v')
  => (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
leftForeignKeyJoinKTable extractor joiner =
  buildFKJoin "FK-LEFTJOIN" extractor (FKModeLeft joiner)

-- | 'foreignKeyJoinKTable' that accepts a 'Joined.TableJoined'
-- (KIP-545) for naming + partitioner overrides.
--
-- == Current behaviour
--
-- The 'Joined.TableJoined' configuration is /accepted/ but only
-- partially applied in the in-process driver:
--
--   * The 'Joined.name' override is recorded but the inner
--     processor names are still auto-generated. (A real-broker
--     runtime that honors 'TableJoined' end-to-end is on the
--     follow-up list.)
--   * The left/right partitioner overrides are ignored — the
--     in-process driver doesn't materialise broker partitions,
--     so per-side partitioning is moot.
--
-- The point of accepting the argument now is API parity: callers
-- can plumb 'TableJoined' through without rewriting their
-- topologies when the runtime gains full support.
foreignKeyJoinKTableWith
  :: forall k v fk vr v'
   . (Ord k, Ord fk, Hashable v, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
foreignKeyJoinKTableWith _tj extractor joiner =
  buildFKJoin "FK-JOIN" extractor (FKModeInner joiner)

leftForeignKeyJoinKTableWith
  :: forall k v fk vr v'
   . (Ord k, Ord fk, Hashable v, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
leftForeignKeyJoinKTableWith _tj extractor joiner =
  buildFKJoin "FK-LEFTJOIN" extractor (FKModeLeft joiner)

-- | Mode + joiner closure carried into the side processors.
data FKMode v vr v' where
  FKModeInner :: (v -> vr -> v')        -> FKMode v vr v'
  FKModeLeft  :: (v -> Maybe vr -> v')  -> FKMode v vr v'

buildFKJoin
  :: forall k v fk vr v'
   . (Ord k, Ord fk, Hashable v, HasSerde v')
  => String                            -- prefix (debug only)
  -> (v -> fk)
  -> FKMode v vr v'
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
buildFKJoin _prefix extractor mode m kl kr = do
  let b = ktableBuilder kl
  outNm <- maybe (freshStoreName b "KTABLE-FK-OUT")
                 pure
                 (matName m)
  subsNm   <- freshStoreName b "KTABLE-FK-SUBS"
  leftNm   <- freshStoreName b "KTABLE-FK-LEFT"
  tokenNm  <- freshStoreName b "KTABLE-FK-TOKEN"
  lastFKNm <- freshStoreName b "KTABLE-FK-LASTFK"

  let outBuilder    = inMemoryKeyValueStoreBuilder outNm
                        :: StoreBuilderKV k v'
      subsBuilder   = inMemoryKeyValueStoreBuilder subsNm
                        :: StoreBuilderKV fk (Map k SubscriptionToken)
      leftBuilder   = inMemoryKeyValueStoreBuilder leftNm
                        :: StoreBuilderKV k v
      tokenBuilder  = inMemoryKeyValueStoreBuilder tokenNm
                        :: StoreBuilderKV k SubscriptionToken
      lastFKBuilder = inMemoryKeyValueStoreBuilder lastFKNm
                        :: StoreBuilderKV k fk
  leftProcNm   <- freshNodeName b "KTABLE-FK-LEFT-PROC"
  rightProcNm  <- freshNodeName b "KTABLE-FK-RIGHT-PROC"
  -- Unifier passthrough: the left and right side processors both
  -- emit to it; downstream nodes (and 'toKStreamFromKTable')
  -- treat THIS node as the FK-join's output anchor. Without it
  -- downstream sinks would only see records from one of the two
  -- side processors.
  emitNm       <- freshNodeName b "KTABLE-FK-EMIT"

  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = leftProcNm
                  , Topo.processorSpecParents  = [ktableNode kl]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkLeftSideProc @k @v @fk @vr @v'
                           extractor mode
                           (ktableStore kr)
                           outNm subsNm leftNm tokenNm lastFKNm)
                  , Topo.processorSpecStores   =
                      [outNm, subsNm, leftNm, tokenNm, lastFKNm, ktableStore kr]
                  } t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = rightProcNm
                  , Topo.processorSpecParents  = [ktableNode kr]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkRightSideProc @k @v @fk @vr @v'
                           mode
                           outNm subsNm leftNm tokenNm)
                  , Topo.processorSpecStores   =
                      [outNm, subsNm, leftNm, tokenNm, ktableStore kr]
                  } t1
        !t3 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = emitNm
                  , Topo.processorSpecParents  = [leftProcNm, rightProcNm]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mkFKJoinEmit @k @v')
                  , Topo.processorSpecStores   = []
                  } t2
        !t4 = Topo.addStateStoreKV outBuilder    [leftProcNm, rightProcNm] t3
        !t5 = Topo.addStateStoreKV subsBuilder   [leftProcNm, rightProcNm] t4
        !t6 = Topo.addStateStoreKV leftBuilder   [leftProcNm, rightProcNm] t5
        !t7 = Topo.addStateStoreKV tokenBuilder  [leftProcNm, rightProcNm] t6
        !t8 = Topo.addStateStoreKV lastFKBuilder [leftProcNm]              t7
     in t8
  pure KTable
    { ktableNode       = emitNm
    , ktableStore      = outNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde kl
    , ktableValueSerde = serde
    }

-- | Trivial passthrough used as the FK-join's output anchor so
-- downstream consumers see emits from BOTH side processors.
mkFKJoinEmit :: forall k v . IO (Processor k v)
mkFKJoinEmit = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KTABLE-FK-EMIT"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> forwardRecord ctx r
    }

----------------------------------------------------------------------
-- Side processors
----------------------------------------------------------------------

-- | Left-side processor: handles updates to the left KTable. Maintains
-- the left value cache, the per-key subscription token, the foreign
-- key the left key currently subscribes to, and the subscription set
-- keyed by foreign key.
mkLeftSideProc
  :: forall k v fk vr v'
   . (Ord k, Ord fk, Hashable v)
  => (v -> fk)
  -> FKMode v vr v'
  -> StoreName              -- right table store (for synchronous lookup)
  -> StoreName              -- output store
  -> StoreName              -- subscriptions store: fk -> Map k SubscriptionToken
  -> StoreName              -- left-state store
  -> StoreName              -- per-left-key token store
  -> StoreName              -- last-fk store
  -> IO (Processor k v)
mkLeftSideProc extractor mode rightNm outNm subsNm leftNm tokenNm lastFKNm = do
  ctxRef    <- newIORef Nothing
  rightRef  <- newIORef (Nothing :: Maybe (KeyValueStore fk vr))
  outRef    <- newIORef (Nothing :: Maybe (KeyValueStore k v'))
  subsRef   <- newIORef (Nothing :: Maybe (KeyValueStore fk (Map k SubscriptionToken)))
  leftRef   <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  tokenRef  <- newIORef (Nothing :: Maybe (KeyValueStore k SubscriptionToken))
  lastFKRef <- newIORef (Nothing :: Maybe (KeyValueStore k fk))
  pure Processor
    { procName = processorName "FK-JOIN-LEFT"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        bindStore ctx rightNm  rightRef
        bindStore ctx outNm    outRef
        bindStore ctx subsNm   subsRef
        bindStore ctx leftNm   leftRef
        bindStore ctx tokenNm  tokenRef
        bindStore ctx lastFKNm lastFKRef
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k -> do
          mctx    <- readIORef ctxRef
          mright  <- readIORef rightRef
          mout    <- readIORef outRef
          msubs   <- readIORef subsRef
          mleft   <- readIORef leftRef
          mtoken  <- readIORef tokenRef
          mLastFK <- readIORef lastFKRef
          case (mctx, mright, mout, msubs, mleft, mtoken, mLastFK) of
            (Just ctx, Just right_, Just out_, Just subs_, Just left_, Just token_, Just lastFK_) -> do
              let v       = recordValue r
                  newFK   = extractor v
                  newTok  = mkToken v
              -- Unsubscribe k from any old fk we previously subscribed to.
              mOldFK <- kvsGet lastFK_ k
              case mOldFK of
                Just oldFK | oldFK /= newFK -> do
                  mOldMap <- kvsGet subs_ oldFK
                  case mOldMap of
                    Just sm -> do
                      let !sm' = Map.delete k sm
                      if Map.null sm'
                        then () <$ kvsDelete subs_ oldFK
                        else kvsPut subs_ oldFK sm'
                    Nothing -> pure ()
                _ -> pure ()
              -- Subscribe k to newFK with the new token.
              mNewMap <- kvsGet subs_ newFK
              let !curMap = case mNewMap of
                    Just sm -> sm
                    Nothing -> Map.empty
                  !nextMap = Map.insert k newTok curMap
              kvsPut subs_ newFK nextMap
              kvsPut lastFK_ k newFK
              kvsPut left_ k v
              kvsPut token_ k newTok
              -- Compute and emit join result.
              mvr <- kvsGet right_ newFK
              emitJoin mode ctx out_ k r v mvr
            _ -> pure ()
    }

-- | Right-side processor: handles updates to the right KTable. For
-- each subscriber, verifies the cached token matches the
-- subscription's token before emitting. If the token check fails
-- (the left value has been replaced and a new subscription is
-- pending), the responder is silently dropped — the new subscription
-- will trigger the correct emit.
mkRightSideProc
  :: forall k v fk vr v'
   . (Ord k, Ord fk)
  => FKMode v vr v'
  -> StoreName              -- output store
  -> StoreName              -- subscriptions store
  -> StoreName              -- left-state store
  -> StoreName              -- per-left-key token store
  -> IO (Processor fk vr)
mkRightSideProc mode outNm subsNm leftNm tokenNm = do
  ctxRef   <- newIORef Nothing
  outRef   <- newIORef (Nothing :: Maybe (KeyValueStore k v'))
  subsRef  <- newIORef (Nothing :: Maybe (KeyValueStore fk (Map k SubscriptionToken)))
  leftRef  <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  tokenRef <- newIORef (Nothing :: Maybe (KeyValueStore k SubscriptionToken))
  pure Processor
    { procName = processorName "FK-JOIN-RIGHT"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        bindStore ctx outNm   outRef
        bindStore ctx subsNm  subsRef
        bindStore ctx leftNm  leftRef
        bindStore ctx tokenNm tokenRef
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just fk -> do
          mctx    <- readIORef ctxRef
          mout    <- readIORef outRef
          msubs   <- readIORef subsRef
          mleft   <- readIORef leftRef
          mtoken  <- readIORef tokenRef
          case (mctx, mout, msubs, mleft, mtoken) of
            (Just ctx, Just out_, Just subs_, Just left_, Just token_) -> do
              let vr = recordValue r
                  !ts = recordTimestamp r
                  !hs = recordHeaders r
              mMap <- kvsGet subs_ fk
              case mMap of
                Nothing -> pure ()
                Just smap ->
                  -- Iterate over subscribers; for each k verify
                  -- the live token matches the subscription token
                  -- before emitting. A mismatch means the left
                  -- value has been mutated and a fresh
                  -- subscription is pending.
                  Map.foldrWithKey
                    (\k subTok next -> do
                       mLiveTok <- kvsGet token_ k
                       case mLiveTok of
                         Just liveTok | liveTok == subTok -> do
                           mLeftV <- kvsGet left_ k
                           case mLeftV of
                             Just lv ->
                               let !carrier = Record (Just k) vr ts hs :: Record k vr
                                in emitJoin mode ctx out_ k carrier lv (Just vr)
                             Nothing -> pure ()
                         _ -> pure ()
                       next)
                    (pure ())
                    smap
            _ -> pure ()
    }

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bindStore
  :: forall k v
   . ProcessorContext
  -> StoreName
  -> IORef (Maybe (KeyValueStore k v))
  -> IO ()
bindStore ctx sn ref =
  getStateStore ctx sn >>= \case
    Just (AnyKeyValueStore kvs) ->
      writeIORef ref (Just (Unsafe.unsafeCoerce kvs))
    _ -> error $ "FK join: store missing: " <> show sn

-- | Emit the join output for a single key, dispatching on the mode.
emitJoin
  :: forall k v vr v' rIn
   . FKMode v vr v'
  -> ProcessorContext
  -> KeyValueStore k v'
  -> k
  -> Record k rIn               -- carrier of timestamp/headers
  -> v
  -> Maybe vr
  -> IO ()
emitJoin mode ctx out_ k carrier v mvr =
  case mode of
    FKModeInner joiner ->
      case mvr of
        Just vr -> do
          let !v' = joiner v vr
          kvsPut out_ k v'
          forwardRecord ctx
            (Record (Just k) v'
              (recordTimestamp carrier)
              (recordHeaders carrier) :: Record k v')
        Nothing -> () <$ kvsDelete out_ k
    FKModeLeft joiner -> do
      let !v' = joiner v mvr
      kvsPut out_ k v'
      forwardRecord ctx
        (Record (Just k) v'
          (recordTimestamp carrier)
          (recordHeaders carrier) :: Record k v')
