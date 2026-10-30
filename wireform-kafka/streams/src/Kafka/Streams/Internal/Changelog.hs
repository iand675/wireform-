{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.Internal.Changelog
Description : Change-logging store wrappers and changelog restore

A logged store is the JVM's @ChangeLogging*Store@: every write to the
store is also sent, as bytes, to the store's changelog partition, so
the store can be rebuilt on any instance by replaying that partition.
Here that is a wrapper around the typed store, built from the store's
'StoreSerdes', plus the inverse: applying a changelog record back onto
the /unwrapped/ store during restore (which must not re-log it).

Changelog record layout (keys are self-describing so restore needs
nothing but the serdes):

  * key-value: key = the key's bytes; value = the value's bytes, or
    null for a delete (a tombstone, which compaction drops)
  * window: key = key bytes ++ 8-byte big-endian window start
  * session: key = key bytes ++ 8-byte end ++ 8-byte start; a
    removed session is a tombstone
-}
module Kafka.Streams.Internal.Changelog (
  ChangelogSink,
  logKeyValueStore,
  logWindowStore,
  logSessionStore,
  restoreKeyValue,
  restoreWindow,
  restoreSession,
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isNothing)
import Data.Text (Text)
import Kafka.Streams.Internal.Serdes (int64BE, readInt64BE)
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  SessionKey (..),
  SessionStore (..),
  StoreSerdes (..),
  WindowStore (..),
 )
import Kafka.Streams.Time (Timestamp (..))


-- | Sends one changelog record: key bytes and (null for a delete) value bytes.
type ChangelogSink = ByteString -> Maybe ByteString -> IO ()


-- | Log every write to a key-value store.
logKeyValueStore :: StoreSerdes k v -> ChangelogSink -> KeyValueStore k v -> KeyValueStore k v
logKeyValueStore sd emit inner =
  let ks = storeKeySerde sd
      vs = storeValueSerde sd
      put k v = do
        kvsPut inner k v
        emit (serialize ks k) (Just (serialize vs v))
  in inner
       { kvsPut = put
       , kvsPutIfAbsent = \k v -> do
           r <- kvsPutIfAbsent inner k v
           when (isNothing r) (emit (serialize ks k) (Just (serialize vs v)))
           pure r
       , kvsDelete = \k -> do
           r <- kvsDelete inner k
           emit (serialize ks k) Nothing
           pure r
       }


-- | Log every write to a window store.
logWindowStore :: StoreSerdes k v -> ChangelogSink -> WindowStore k v -> WindowStore k v
logWindowStore sd emit inner =
  inner
    { wsPut = \k v ts@(Timestamp t) -> do
        wsPut inner k v ts
        emit (serialize (storeKeySerde sd) k <> int64BE t) (Just (serialize (storeValueSerde sd) v))
    }


-- | Log every write to a session store.
logSessionStore :: StoreSerdes k v -> ChangelogSink -> SessionStore k v -> SessionStore k v
logSessionStore sd emit inner =
  let sessionKeyBytes sk =
        serialize (storeKeySerde sd) (skKey sk)
          <> int64BE (unTimestamp (skEnd sk))
          <> int64BE (unTimestamp (skStart sk))
  in inner
       { ssPut = \sk v -> do
           ssPut inner sk v
           emit (sessionKeyBytes sk) (Just (serialize (storeValueSerde sd) v))
       , ssRemove = \sk -> do
           ssRemove inner sk
           emit (sessionKeyBytes sk) Nothing
       }


-- | Apply one changelog record to an (unwrapped) key-value store.
restoreKeyValue :: StoreSerdes k v -> KeyValueStore k v -> ByteString -> Maybe ByteString -> IO (Either Text ())
restoreKeyValue sd inner kb mvb =
  case deserialize (storeKeySerde sd) kb of
    Left e -> pure (Left e)
    Right k -> case mvb of
      Nothing -> Right () <$ kvsDelete inner k
      Just vb -> case deserialize (storeValueSerde sd) vb of
        Left e -> pure (Left e)
        Right v -> Right <$> kvsPut inner k v


-- | Apply one changelog record to an (unwrapped) window store.
restoreWindow :: StoreSerdes k v -> WindowStore k v -> ByteString -> Maybe ByteString -> IO (Either Text ())
restoreWindow sd inner kb mvb =
  let !n = BS.length kb - 8
  in if n < 0
       then pure (Left "window changelog key shorter than its timestamp")
       else case (deserialize (storeKeySerde sd) (BS.take n kb), readInt64BE (BS.drop n kb), mvb) of
         (Left e, _, _) -> pure (Left e)
         (_, Nothing, _) -> pure (Left "window changelog key without a timestamp")
         -- Window stores expire by retention, not by delete; a null
         -- value only appears if another writer sent one, and there
         -- is nothing to undo.
         (Right _, Just _, Nothing) -> pure (Right ())
         (Right k, Just t, Just vb) -> case deserialize (storeValueSerde sd) vb of
           Left e -> pure (Left e)
           Right v -> Right <$> wsPut inner k v (Timestamp t)


-- | Apply one changelog record to an (unwrapped) session store.
restoreSession :: StoreSerdes k v -> SessionStore k v -> ByteString -> Maybe ByteString -> IO (Either Text ())
restoreSession sd inner kb mvb =
  let !n = BS.length kb - 16
  in if n < 0
       then pure (Left "session changelog key shorter than its window")
       else
         let rest = BS.drop n kb
         in case (deserialize (storeKeySerde sd) (BS.take n kb), readInt64BE rest, readInt64BE (BS.drop 8 rest)) of
              (Left e, _, _) -> pure (Left e)
              (Right k, Just end, Just start) -> do
                let sk = SessionKey {skKey = k, skStart = Timestamp start, skEnd = Timestamp end}
                case mvb of
                  Nothing -> Right <$> ssRemove inner sk
                  Just vb -> case deserialize (storeValueSerde sd) vb of
                    Left e -> pure (Left e)
                    Right v -> Right <$> ssPut inner sk v
              _ -> pure (Left "session changelog key without its window")
