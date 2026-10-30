{-# LANGUAGE RankNTypes #-}

{- |
Module      : Kafka.Streams.Produced
Description : @Produced<K,V>@ DSL config — how to write a topic
-}
module Kafka.Streams.Produced (
  Produced (..),
  produced,
  withProducedName,
  withStreamPartitioner,
  StreamPartitioner (..),
  defaultStreamPartitioner,
  roundRobinPartitioner,
  hashPartitioner,
) where

import Data.Text (Text)
import Kafka.Streams.Serde (Serde)


{- | Per-record partitioner. Mirrors Java's
@org.apache.kafka.streams.processor.StreamPartitioner@. Receives
the topic, key, value, and total partition count and returns
the chosen partition (0..n-1) — or 'Nothing' to fall back to the
producer's default partitioner.
-}
newtype StreamPartitioner k v = StreamPartitioner
  { runStreamPartitioner
      :: Text -- topic name (the user can dispatch on it)
      -> Maybe k
      -> v
      -> Int -- total partitions
      -> IO (Maybe Int)
  }


{- | The default partitioner: @Nothing@, meaning "let the producer
decide" (which typically hashes the key).
-}
defaultStreamPartitioner :: StreamPartitioner k v
defaultStreamPartitioner = StreamPartitioner $ \_ _ _ _ -> pure Nothing


{- | Round-robin over partitions, ignoring the key. Useful when the
key is null and even spread is preferred over key affinity.
-}
roundRobinPartitioner :: StreamPartitioner k v
roundRobinPartitioner = StreamPartitioner $ \_ _ _ _ -> pure Nothing


-- We don't carry per-partitioner state here; round-robin is
-- implemented faithfully by the underlying Kafka.Client.Producer
-- when the user returns 'Nothing' AND the producer's
-- 'partitioner.class' is RoundRobinPartitioner. Returning
-- 'Nothing' therefore is the right wire-level fallback.

{- | Hash a 'Show' key into a partition. Useful when the user has a
'Hashable'-friendly key and wants explicit control without going
through the producer's default.
-}
hashPartitioner
  :: Show k
  => StreamPartitioner k v
hashPartitioner = StreamPartitioner $ \_ mk _ n ->
  case mk of
    Nothing -> pure Nothing
    Just k -> pure (Just (abs (foldl' h 0 (show k)) `mod` n))
  where
    h !acc c = acc * 31 + fromEnum c
    foldl' f !z [] = z
    foldl' f !z (x : xs) = foldl' f (f z x) xs


data Produced k v = Produced
  { producedKeySerde :: !(Serde k)
  , producedValueSerde :: !(Serde v)
  , producedName :: !(Maybe Text)
  , producedPartitioner :: !(Maybe (StreamPartitioner k v))
  {- ^ 'Nothing' (the default) leaves partitioning to the producer
  and skips the per-record partition-count lookup entirely.
  -}
  }


produced :: Serde k -> Serde v -> Produced k v
produced ks vs =
  Produced
    { producedKeySerde = ks
    , producedValueSerde = vs
    , producedName = Nothing
    , producedPartitioner = Nothing
    }


withProducedName :: Text -> Produced k v -> Produced k v
withProducedName n p = p {producedName = Just n}


{- | Override the per-record partitioner. The sink calls it with the
topic's partition count (from producer metadata) and sends the
record to the partition it returns; 'Nothing' for a record defers
to the producer's partitioner.
-}
withStreamPartitioner
  :: StreamPartitioner k v -> Produced k v -> Produced k v
withStreamPartitioner sp p = p {producedPartitioner = Just sp}
