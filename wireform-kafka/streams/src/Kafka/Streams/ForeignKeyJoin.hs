{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.ForeignKeyJoin
Description : KTable-KTable foreign-key join (KIP-213)

Joins each row of a left 'KTable' (keyed by @k@) with the row of a
right 'KTable' (keyed by @fk@) that a function of the left value
names. The two tables are partitioned differently (by @k@ and by
@fk@), so the join is the KIP-213 protocol over two internal topics:

  1. /Subscription/ (@\<name\>-subscription-registration@, keyed by
     @fk@, co-partitioned with the right table): a left row
     subscribes its key to its foreign key, carrying a hash of the
     left value; when the row's foreign key changes or the row is
     deleted, it unsubscribes from the old one.
  2. The right side keeps, per foreign key, the subscribed left keys
     and their hashes. A subscription is answered with the right row
     as it is now; a right-row change is sent to every subscriber.
  3. /Response/ (@\<name\>-subscription-response@, keyed by @k@,
     co-partitioned with the left table): the left side joins the
     response with its current left row, unless the response carries
     the hash of a left value that has since been replaced (that
     value's own subscription answers instead).

A deleted left row deletes its join row at once. An inner join has no
row while the right side has none; a left join has one for every left
row.
-}
module Kafka.Streams.ForeignKeyJoin (
  foreignKeyJoinKTable,
  leftForeignKeyJoinKTable,
  foreignKeyJoinKTableWith,
  leftForeignKeyJoinKTableWith,
) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Hashable (hashWithSalt)
import Data.IORef
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Kafka.Client.Internal.Murmur2 qualified as Murmur2
import Kafka.Streams.Consumed (consumed, consumedExtractor)
import Kafka.Streams.Internal.Serdes (int64BE, mapSerde, readInt64BE)
import Kafka.Streams.Joined qualified as Joined
import Kafka.Streams.KTable (
  Change (..),
  KTable (..),
 )
import Kafka.Streams.Materialized (
  Materialized (..),
  materializedKV,
 )
import Kafka.Streams.Processor (
  Processor (..),
  ProcessorContext,
  forwardRecord,
  getStateStore,
  processorName,
 )
import Kafka.Streams.Produced (StreamPartitioner (..))
import Kafka.Streams.Serde (HasSerde (..), Serde (..))
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueStore (..),
  StoreBuilderKV,
  StoreName,
  withStoreSerdesKV,
 )
import Kafka.Streams.StreamsBuilder (
  freshNodeName,
  freshStoreName,
  withTopology_,
 )
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Record (..), TopicName, topicName)
import Unsafe.Coerce qualified as Unsafe


-- | Inner foreign-key KTable-KTable join.
foreignKeyJoinKTable
  :: forall k v fk vr v'
   . (Ord k, Ord fk, HasSerde v')
  => (v -> fk)
  -- ^ foreign-key extractor
  -> (v -> vr -> v')
  -- ^ joiner
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
foreignKeyJoinKTable extractor joiner =
  buildFKJoin Joined.tableJoined extractor (\v mr -> joiner v <$> mr)


-- | Left foreign-key join: a row for every left row (the right side as a 'Maybe').
leftForeignKeyJoinKTable
  :: forall k v fk vr v'
   . (Ord k, Ord fk, HasSerde v')
  => (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
leftForeignKeyJoinKTable extractor joiner =
  buildFKJoin Joined.tableJoined extractor (\v mr -> Just (joiner v mr))


{- | 'foreignKeyJoinKTable' with a 'Joined.TableJoined' (KIP-545): its
name names the internal topics, and its partitioners say how the left
and right tables are partitioned, so the response and subscription
topics are written the same way.
-}
foreignKeyJoinKTableWith
  :: forall k v fk vr v'
   . (Ord k, Ord fk, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
foreignKeyJoinKTableWith tj extractor joiner =
  buildFKJoin tj extractor (\v mr -> joiner v <$> mr)


leftForeignKeyJoinKTableWith
  :: forall k v fk vr v'
   . (Ord k, Ord fk, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
leftForeignKeyJoinKTableWith tj extractor joiner =
  buildFKJoin tj extractor (\v mr -> Just (joiner v mr))


----------------------------------------------------------------------
-- Messages
----------------------------------------------------------------------

data Subscription k = Subscription
  { subSubscribe :: !Bool
  -- ^ 'False': unsubscribe
  , subHash :: !Int64
  , subKey :: !k
  }


-- | op byte, 8-byte hash, then the left key's bytes.
subscriptionSerde :: Serde k -> Serde (Subscription k)
subscriptionSerde ks =
  Serde
    { serialize = \s -> BS.cons (if subSubscribe s then 1 else 0) (int64BE (subHash s) <> serialize ks (subKey s))
    , deserialize = \bs -> case BS.uncons bs of
        Just (op, rest) | Just h <- readInt64BE rest -> Subscription (op == 1) h <$> deserialize ks (BS.drop 8 rest)
        _ -> Left "truncated foreign-key subscription"
    , serializeHeaders = const mempty
    }


data Response vr = Response
  { respHash :: !Int64
  , respRight :: !(Maybe vr)
  }


-- | 8-byte hash, a presence byte, then the right value's bytes.
responseSerde :: Serde vr -> Serde (Response vr)
responseSerde vs =
  Serde
    { serialize = \r ->
        int64BE (respHash r) <> maybe (BS.singleton 0) (BS.cons 1 . serialize vs) (respRight r)
    , deserialize = \bs -> case (readInt64BE bs, BS.uncons (BS.drop 8 bs)) of
        (Just h, Just (0, _)) -> Right (Response h Nothing)
        (Just h, Just (_, rest)) -> Response h . Just <$> deserialize vs rest
        _ -> Left "truncated foreign-key response"
    , serializeHeaders = const mempty
    }


hashSerde :: Serde Int64
hashSerde =
  Serde
    { serialize = int64BE
    , deserialize = maybe (Left "truncated hash") Right . readInt64BE
    , serializeHeaders = const mempty
    }


-- | Per foreign key: the subscribed left keys and their values' hashes.
type Subscribers k = Map k Int64


-- | A 64-bit hash of a left value's bytes (two independent 32-bit halves).
valueHash :: ByteString -> Int64
valueHash bs =
  (fromIntegral (Murmur2.murmur2 bs) `shiftL` 32)
    .|. (fromIntegral (hashWithSalt 0x5bd1e995 bs) .&. 0xffffffff)


----------------------------------------------------------------------
-- Topology
----------------------------------------------------------------------

-- | A sink partitioned like the table it is co-partitioned with ('Joined.TableJoined').
partitionedSink
  :: Maybe (Text -> Maybe key -> Int -> Int)
  -> Topo.NodeName
  -> TopicName
  -> Serde key
  -> Serde val
  -> [Topo.NodeName]
  -> Topo.Topology
  -> Topo.Topology
partitionedSink mp nm tp ks vs parents = case mp of
  Nothing -> Topo.addSink nm tp ks vs parents
  Just f -> Topo.addSinkPartitioned nm tp ks vs (StreamPartitioner (\t mk _ n -> pure (Just (f t mk n)))) parents


addProc :: Topo.NodeName -> [Topo.NodeName] -> [StoreName] -> IO (Processor k v) -> Topo.Topology -> Topo.Topology
addProc nm parents stores p =
  Topo.addProcessorWith
    Topo.ProcessorSpec
      { Topo.processorSpecName = nm
      , Topo.processorSpecParents = parents
      , Topo.processorSpecSupplier = Topo.AnyProcessor p
      , Topo.processorSpecStores = stores
      }


buildFKJoin
  :: forall k v fk vr v'
   . (Ord k, Ord fk, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> Maybe vr -> Maybe v')
  -- ^ the join row for a left value and the right row
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
buildFKJoin tj extractor row m kl kr = do
  let b = ktableBuilder kl
      lk = ktableKeySerde kl
      rk = ktableKeySerde kr
      hashOf v = valueHash (serialize (ktableValueSerde kl) v)
      outSerde = fromMaybe serde (matValueSerde m)
  joinNm <- freshNodeName b "KTABLE-FK-JOIN"
  let base = fromMaybe (Topo.unNodeName joinNm) tj.name
  outNm <- maybe (freshStoreName b "KTABLE-FK-JOIN-STORE") pure (matName m)
  subsNm <- freshStoreName b "KTABLE-FK-JOIN-SUBSCRIPTION-STORE"
  let subTopic = topicName (base <> "-subscription-registration")
      respTopic = topicName (base <> "-subscription-response")
      subSerde = subscriptionSerde lk
      respSerde = responseSerde (ktableValueSerde kr)
      outBuilder = materializedKV m outNm lk outSerde :: StoreBuilderKV k v'
      subsBuilder =
        withStoreSerdesKV rk (mapSerde lk hashSerde) (inMemoryKeyValueStoreBuilder subsNm)
          :: StoreBuilderKV fk (Subscribers k)
      node suffix = freshNodeName b ("KTABLE-FK-JOIN-" <> suffix)
  subscribeNm <- node "SUBSCRIBE"
  subSinkNm <- node "SUBSCRIPTION-SINK"
  subSourceNm <- node "SUBSCRIPTION-SOURCE"
  registerNm <- node "SUBSCRIPTION-PROCESSOR"
  rightNm <- node "RIGHT"
  respMergeNm <- node "RESPONSE-MERGE"
  respSinkNm <- node "RESPONSE-SINK"
  respSourceNm <- node "RESPONSE-SOURCE"
  respondNm <- node "RESPOND"
  deleteNm <- node "DELETE"
  outMergeNm <- node "OUTPUT"
  withTopology_ b $ \t0 ->
    let internal tp = Topo.addInternalTopic tp Topo.InternalTopicSpec {Topo.itsPartitions = Nothing}
        -- left side: subscribe / unsubscribe, and deletes
        t1 = addProc subscribeNm [ktableNode kl] [] (subscribeProcessor @k @v @fk extractor hashOf) t0
        t2 = internal subTopic t1
        t3 = partitionedSink tj.otherPartitioner subSinkNm subTopic rk subSerde [subscribeNm] t2
        t4 = addProc deleteNm [ktableNode kl] [outNm] (deleteProcessor @k @v @v' outNm) t3
        -- right side: the subscription store, and right-row changes
        t5 = Topo.addSource subSourceNm [subTopic] rk subSerde (consumedExtractor (consumed rk subSerde)) t4
        t6 = addProc registerNm [subSourceNm] [subsNm, ktableStore kr] (registerProcessor @k @fk @vr subsNm (ktableStore kr)) t5
        t7 = addProc rightNm [ktableNode kr] [subsNm] (rightProcessor @k @fk @vr subsNm) t6
        t8 = addProc respMergeNm [registerNm, rightNm] [] (passThrough @k @(Response vr)) t7
        t9 = internal respTopic t8
        t10 = partitionedSink tj.leftPartitioner respSinkNm respTopic lk respSerde [respMergeNm] t9
        -- back on the left: join responses with the current left row
        t11 = Topo.addSource respSourceNm [respTopic] lk respSerde (consumedExtractor (consumed lk respSerde)) t10
        t12 = addProc respondNm [respSourceNm] [ktableStore kl, outNm] (respondProcessor @k @v @vr @v' (ktableStore kl) outNm hashOf row) t11
        t13 = addProc outMergeNm [deleteNm, respondNm] [] (passThrough @k @(Change v')) t12
        t14 = Topo.addStateStoreKV outBuilder [deleteNm, respondNm] t13
    in Topo.addStateStoreKV subsBuilder [registerNm, rightNm] t14
  pure
    KTable
      { ktableNode = outMergeNm
      , ktableStore = outNm
      , ktableBuilder = b
      , ktableKeySerde = lk
      , ktableValueSerde = outSerde
      }


----------------------------------------------------------------------
-- Processors
----------------------------------------------------------------------

withContext :: Text -> (ProcessorContext -> IO (Record k x -> IO ())) -> IO (Processor k x)
withContext nm setUp = do
  ref <- newIORef Nothing
  pure
    Processor
      { procName = processorName nm
      , procInit = \ctx -> setUp ctx >>= writeIORef ref . Just
      , procClose = pure ()
      , procProcess = \r -> readIORef ref >>= mapM_ ($ r)
      }


bindKV :: ProcessorContext -> StoreName -> IO (KeyValueStore a b)
bindKV ctx sn =
  getStateStore ctx sn >>= \case
    Just (AnyKeyValueStore kvs) -> pure (Unsafe.unsafeCoerce kvs)
    _ -> error ("foreign-key join: store missing: " <> show sn)


passThrough :: forall k v. IO (Processor k v)
passThrough = withContext "KTABLE-FK-JOIN-MERGE" (\ctx -> pure (forwardRecord ctx))


-- | Left row changed: unsubscribe from a foreign key it left, subscribe to its current one.
subscribeProcessor :: forall k v fk. Eq fk => (v -> fk) -> (v -> Int64) -> IO (Processor k (Change v))
subscribeProcessor extractor hashOf =
  withContext "KTABLE-FK-JOIN-SUBSCRIBE" $ \ctx -> pure $ \r -> case recordKey r of
    Nothing -> pure ()
    Just k -> do
      let Change new old = recordValue r
          newFk = extractor <$> new
          send fk s = forwardRecord ctx r {recordKey = Just fk, recordValue = s}
      case extractor <$> old of
        Just ofk | Just ofk /= newFk -> send ofk (Subscription False 0 k)
        _ -> pure ()
      mapM_ (\v -> send (extractor v) (Subscription True (hashOf v) k)) new


-- | A deleted left row deletes its join row at once.
deleteProcessor :: forall k v (v' :: Type). StoreName -> IO (Processor k (Change v))
deleteProcessor outNm =
  withContext "KTABLE-FK-JOIN-DELETE" $ \ctx -> do
    out <- bindKV ctx outNm :: IO (KeyValueStore k v')
    pure $ \r -> case (recordKey r, changeNew (recordValue r)) of
      (Just k, Nothing) -> do
        prev <- kvsDelete out k
        mapM_ (\p -> forwardRecord ctx r {recordValue = Change Nothing (Just p) :: Change v'}) prev
      _ -> pure ()


-- | Keep the subscription store; answer a subscription with the right row as it is now.
registerProcessor :: forall k fk (vr :: Type). Ord k => StoreName -> StoreName -> IO (Processor fk (Subscription k))
registerProcessor subsNm rightNm =
  withContext "KTABLE-FK-JOIN-SUBSCRIPTION-PROCESSOR" $ \ctx -> do
    subs <- bindKV ctx subsNm :: IO (KeyValueStore fk (Subscribers k))
    right <- bindKV ctx rightNm :: IO (KeyValueStore fk vr)
    pure $ \r -> case recordKey r of
      Nothing -> pure ()
      Just fk -> do
        let s = recordValue r
        cur <- fromMaybe Map.empty <$> kvsGet subs fk
        if subSubscribe s
          then do
            kvsPut subs fk (Map.insert (subKey s) (subHash s) cur)
            rv <- kvsGet right fk
            forwardRecord ctx r {recordKey = Just (subKey s), recordValue = Response (subHash s) rv}
          else do
            let rest = Map.delete (subKey s) cur
            if Map.null rest then () <$ kvsDelete subs fk else kvsPut subs fk rest


-- | A right row changed: tell every subscriber.
rightProcessor :: forall (k :: Type) fk vr. StoreName -> IO (Processor fk (Change vr))
rightProcessor subsNm =
  withContext "KTABLE-FK-JOIN-RIGHT" $ \ctx -> do
    subs <- bindKV ctx subsNm :: IO (KeyValueStore fk (Subscribers k))
    pure $ \r -> case recordKey r of
      Nothing -> pure ()
      Just fk -> do
        cur <- fromMaybe Map.empty <$> kvsGet subs fk
        let new = changeNew (recordValue r)
        mapM_
          (\(k, h) -> forwardRecord ctx r {recordKey = Just k, recordValue = Response h new})
          (Map.toList cur)


-- | Join a response with the current left row, unless it answers a replaced left value.
respondProcessor
  :: forall k v vr v'
   . StoreName
  -> StoreName
  -> (v -> Int64)
  -> (v -> Maybe vr -> Maybe v')
  -> IO (Processor k (Response vr))
respondProcessor leftNm outNm hashOf row =
  withContext "KTABLE-FK-JOIN-RESPOND" $ \ctx -> do
    left <- bindKV ctx leftNm :: IO (KeyValueStore k v)
    out <- bindKV ctx outNm :: IO (KeyValueStore k v')
    pure $ \r -> case recordKey r of
      Nothing -> pure ()
      Just k -> do
        lv <- kvsGet left k
        case lv of
          Just v
            | hashOf v == respHash (recordValue r) -> do
                let new = row v (respRight (recordValue r))
                prev <- kvsGet out k
                case new of
                  Just n -> kvsPut out k n
                  Nothing -> () <$ kvsDelete out k
                case (new, prev) of
                  (Nothing, Nothing) -> pure ()
                  _ -> forwardRecord ctx r {recordValue = Change new prev}
          -- a deleted or since-replaced left row: drop the stale answer
          _ -> pure ()
