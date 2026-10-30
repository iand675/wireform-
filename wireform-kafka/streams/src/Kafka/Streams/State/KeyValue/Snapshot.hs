{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.Snapshot
Description : Snapshot-aware KV store wrapper (Riffle \xc2\xa71)

Wraps an underlying 'KeyValueStore' with periodic snapshots
to an 'ObjectStoreClient' and a recovery path that restores
from the latest snapshot manifest. The point: bound recovery
time by /time since the last snapshot/ rather than /state
size/, which is the fundamental ceiling of changelog-only
recovery today.

== Contract

  * Writes go through to the underlying store unchanged.
  * On a 'snapshotStore' invocation, the wrapper takes a
    consistent snapshot (scans the underlying store) and
    publishes it as a single object under
    @<storeName>/snapshots/<snapshotId>@. A manifest under
    @<storeName>/manifest@ records the latest snapshot id.
  * On 'restoreFromSnapshot', the wrapper reads the manifest,
    downloads the snapshot blob, and replays it into the
    underlying store. The caller is expected to follow up by
    replaying the changelog from
    'snapshotAdvancedTo' onward.

The publish + restore loop is driven by
'Kafka.Streams.Runtime.Snapshot' (a separate module that
owns the lifecycle thread).
This module is the data-plane piece.
-}
module Kafka.Streams.State.KeyValue.Snapshot (
  -- * Contract
  SnapshotId (..),
  SnapshotManifest (..),
  SnapshotPlan (..),

  -- * Operations
  snapshotStore,
  restoreFromSnapshot,
  readLatestManifest,
  listSnapshots,

  -- * Helpers
  storeSnapshotKey,
  storeManifestKey,
) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Kafka.Streams.Runtime.ObjectStore (
  ObjectKey (..),
  ObjectStoreClient (..),
 )
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  StoreName,
  kvIteratorToList,
  unStoreName,
 )
import Kafka.Streams.Time (Duration)


----------------------------------------------------------------------
-- Identity / manifest
----------------------------------------------------------------------

{- | A monotonic snapshot identifier. Typically derived from the
changelog offset the snapshot was taken at, but the wrapper
treats it as opaque.
-}
newtype SnapshotId = SnapshotId {unSnapshotId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)


{- | Manifest pointing at the latest snapshot for a store. The
wire format is intentionally trivial: two ASCII lines (id +
advancedTo).
-}
data SnapshotManifest = SnapshotManifest
  { manifestSnapshotId :: !SnapshotId
  , manifestAdvancedTo :: !Int64
  {- ^ Changelog offset the snapshot was taken at. Recovery
  replays @[manifestAdvancedTo, end-of-changelog)@.
  -}
  }
  deriving stock (Eq, Show, Generic)


{- | Operational policy for how often to publish a snapshot.
The runtime's snapshot thread reads this every commit cycle.
-}
data SnapshotPlan = SnapshotPlan
  { spInterval :: !Duration
  -- ^ Wall-clock cadence between snapshots.
  , spMaxRecordsBetween :: !(Maybe Int64)
  {- ^ Alternative trigger: snapshot once @N@ records have
  accumulated since the last snapshot.
  -}
  , spRetention :: !Int
  -- ^ How many historical snapshots to keep before pruning.
  }


----------------------------------------------------------------------
-- Key helpers
----------------------------------------------------------------------

{- | Where in the object store the snapshot blob lives:
@<storeName>\/snapshots\/<snapshotId>@.
-}
storeSnapshotKey :: StoreName -> SnapshotId -> ObjectKey
storeSnapshotKey sn (SnapshotId sid) =
  ObjectKey (unStoreName sn <> "/snapshots/" <> T.pack (show sid))


{- | Where the manifest pointer lives:
@<storeName>\/manifest@.
-}
storeManifestKey :: StoreName -> ObjectKey
storeManifestKey sn = ObjectKey (unStoreName sn <> "/manifest")


----------------------------------------------------------------------
-- Snapshot the store
----------------------------------------------------------------------

{- | Snapshot the underlying store to the object store. Walks
every entry via 'kvsAll', encodes the @(k, v)@ pairs through
the caller-supplied byte encoders, writes them as a single
object, and publishes a manifest pointing at it.

The encoders take a @k@ \/ @v@ and produce a 'ByteString'.
Real implementations use the store's serdes; tests pass
direct binary encoders.
-}
snapshotStore
  :: forall k v
   . ObjectStoreClient
  -> StoreName
  -> SnapshotId
  -> Int64
  -- ^ advancedTo
  -> (k -> ByteString)
  -> (v -> ByteString)
  -> KeyValueStore k v
  -> IO (Either Text ())
snapshotStore os sn sid advancedTo encK encV kvs = do
  it <- kvsAll kvs
  pairs <- kvIteratorToList it
  let !blob = encodeBlob encK encV pairs
  putR <- osPut os (storeSnapshotKey sn sid) blob
  case putR of
    Left e -> pure (Left ("snapshot blob put: " <> T.pack (show e)))
    Right () -> do
      let mfBytes = encodeManifest (SnapshotManifest sid advancedTo)
      manR <- osPut os (storeManifestKey sn) mfBytes
      case manR of
        Left e -> pure (Left ("manifest put: " <> T.pack (show e)))
        Right () -> pure (Right ())


{- | Restore the underlying store from the latest snapshot.
Reads the manifest, fetches the snapshot blob, decodes it,
and bulk-loads via 'kvsPut'. Returns the recovered
'SnapshotManifest' so the caller knows where to start the
changelog tail replay.
-}
restoreFromSnapshot
  :: forall k v
   . ObjectStoreClient
  -> StoreName
  -> (ByteString -> Either Text k)
  -> (ByteString -> Either Text v)
  -> KeyValueStore k v
  -> IO (Either Text (Maybe SnapshotManifest))
restoreFromSnapshot os sn decK decV kvs = do
  mfR <- readLatestManifest os sn
  case mfR of
    Left e -> pure (Left e)
    Right Nothing -> pure (Right Nothing) -- no snapshot yet
    Right (Just mf) -> do
      blobR <- osGet os (storeSnapshotKey sn (manifestSnapshotId mf))
      case blobR of
        Left e ->
          pure
            ( Left
                ( "snapshot blob get: "
                    <> T.pack (show e)
                )
            )
        Right Nothing ->
          pure
            ( Left
                ( "snapshot blob missing for "
                    <> T.pack (show mf)
                )
            )
        Right (Just blob) ->
          case decodeBlob decK decV blob of
            Left e -> pure (Left ("snapshot blob decode: " <> e))
            Right pairs -> do
              forM_ pairs (\(k, v) -> kvsPut kvs k v)
              pure (Right (Just mf))


{- | Read the manifest for a store, or 'Nothing' if no snapshot
has ever been published.
-}
readLatestManifest
  :: ObjectStoreClient
  -> StoreName
  -> IO (Either Text (Maybe SnapshotManifest))
readLatestManifest os sn = do
  r <- osGet os (storeManifestKey sn)
  case r of
    Left e -> pure (Left (T.pack (show e)))
    Right Nothing -> pure (Right Nothing)
    Right (Just bs) -> case decodeManifest bs of
      Left e -> pure (Left e)
      Right mf -> pure (Right (Just mf))


{- | List every snapshot id currently published for a store.
Used by retention pruning.
-}
listSnapshots
  :: ObjectStoreClient
  -> StoreName
  -> IO (Either Text [SnapshotId])
listSnapshots os sn = do
  r <- osList os (unStoreName sn <> "/snapshots/")
  case r of
    Left e -> pure (Left (T.pack (show e)))
    Right ks -> pure (Right (extractIds ks))
  where
    prefix = unStoreName sn <> "/snapshots/"
    extractIds = concatMap go
    go (ObjectKey k) =
      case T.stripPrefix prefix k of
        Just rest -> case reads (T.unpack rest) of
          [(n, "")] -> [SnapshotId n]
          _ -> []
        Nothing -> []


----------------------------------------------------------------------
-- Wire format
----------------------------------------------------------------------

{- | Encode @(k, v)@ pairs as:

@
count :: Int64 BE
(klen :: Int64 BE, kbytes, vlen :: Int64 BE, vbytes) * count
@

Trivial length-prefix framing. The runtime's real
implementation can swap in a compressed columnar format
later; the contract is "round-trip every entry".
-}
encodeBlob
  :: (k -> ByteString) -> (v -> ByteString) -> [(k, v)] -> ByteString
encodeBlob encK encV pairs =
  BS.concat $
    encInt64 (fromIntegral (length pairs))
      : concatMap encPair pairs
  where
    encPair (k, v) =
      let !kb = encK k
          !vb = encV v
      in [ encInt64 (fromIntegral (BS.length kb))
         , kb
         , encInt64 (fromIntegral (BS.length vb))
         , vb
         ]


decodeBlob
  :: (ByteString -> Either Text k)
  -> (ByteString -> Either Text v)
  -> ByteString
  -> Either Text [(k, v)]
decodeBlob decK decV bs0 = do
  (count, rest0) <- takeInt64 bs0
  go (fromIntegral count) rest0 []
  where
    go 0 _ acc = Right (List.reverse acc)
    go n bs acc = do
      (klen, r1) <- takeInt64 bs
      (kb, r2) <- takeN (fromIntegral klen) r1
      (vlen, r3) <- takeInt64 r2
      (vb, r4) <- takeN (fromIntegral vlen) r3
      k <- decK kb
      v <- decV vb
      go (n - 1 :: Int) r4 ((k, v) : acc)
    takeN i b
      | BS.length b < i = Left "snapshot blob: truncated"
      | otherwise = Right (BS.take i b, BS.drop i b)


encodeManifest :: SnapshotManifest -> ByteString
encodeManifest m =
  TE.encodeUtf8 $
    "id="
      <> T.pack (show (unSnapshotId (manifestSnapshotId m)))
      <> "\nadvancedTo="
      <> T.pack (show (manifestAdvancedTo m))
      <> "\n"


decodeManifest :: ByteString -> Either Text SnapshotManifest
decodeManifest bs = do
  txt <- case TE.decodeUtf8' bs of
    Left e -> Left (T.pack (show e))
    Right t -> Right t
  let ls = T.splitOn "\n" txt
      mLookup k =
        case [ T.drop (T.length (k <> "=")) l
             | l <- ls
             , (k <> "=") `T.isPrefixOf` l
             ] of
          (v : _) -> Just v
          [] -> Nothing
  sidRaw <- maybe (Left "manifest: missing id") Right (mLookup "id")
  advRaw <-
    maybe
      (Left "manifest: missing advancedTo")
      Right
      (mLookup "advancedTo")
  sid <- case reads (T.unpack sidRaw) :: [(Int64, String)] of
    [(n, "")] -> Right (SnapshotId n)
    _ -> Left "manifest: malformed id"
  adv <- case reads (T.unpack advRaw) :: [(Int64, String)] of
    [(n, "")] -> Right n
    _ -> Left "manifest: malformed advancedTo"
  pure
    SnapshotManifest
      { manifestSnapshotId = sid
      , manifestAdvancedTo = adv
      }


----------------------------------------------------------------------
-- 8-byte big-endian Int64
----------------------------------------------------------------------

encInt64 :: Int64 -> ByteString
encInt64 n =
  BS.pack
    [ fromIntegral (n `unsafeShiftR` shiftBy)
    | shiftBy <- [56, 48, 40, 32, 24, 16, 8, 0]
    ]
  where
    unsafeShiftR :: Int64 -> Int -> Int64
    unsafeShiftR x i =
      fromIntegral
        ((fromIntegral x :: Integer) `Prelude.div` (2 ^ i))


takeInt64 :: ByteString -> Either Text (Int64, ByteString)
takeInt64 bs
  | BS.length bs < 8 = Left "snapshot blob: truncated int64"
  | otherwise =
      let !hi = BS.take 8 bs
          !lo = BS.drop 8 bs
          !n =
            foldl
              (\acc b -> acc * 256 + fromIntegral b)
              (0 :: Integer)
              (BS.unpack hi)
      in Right (fromInteger n, lo)
