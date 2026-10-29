{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.Discovery
Description : KIP-535 cross-instance interactive-query discovery

Mirrors @org.apache.kafka.streams.StreamsMetadata@ and
@KeyQueryMetadata@:

  * 'StreamsMetadata' — what every instance in the group
    looks like to its peers: host:port, owned partitions per
    store, standby partitions per store.
  * 'KeyQueryMetadata' — for a specific @(store, key)@ which
    instance owns the active and which owns the standby.

An instance with 'Kafka.Streams.Config.applicationServer' set
advertises it in its group subscription, and every assignment tells
every instance which host runs (and which holds a standby of) each
task: 'Kafka.Streams.Runtime.streamsMetadata' and
'Kafka.Streams.Runtime.queryMetadataForKey' read it. This module is
the data model, plus 'makeKeyQueryMetadata' for callers that hold a
list of 'StreamsMetadata' from elsewhere.
-}
module Kafka.Streams.Discovery (
  -- * Metadata
  StreamsMetadata (..),
  HostInfo (..),
  parseHostInfo,

  -- * KeyQueryMetadata
  KeyQueryMetadata (..),
  makeKeyQueryMetadata,
) where

import Data.Int (Int32)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Generics (Generic)
import Kafka.Client.Consumer qualified as KC


{- | host:port pair an instance advertises in JoinGroup's
subscription metadata. Mirrors Java's
@org.apache.kafka.streams.state.HostInfo@.
-}
data HostInfo = HostInfo
  { host :: !Text
  , port :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)


{- | Parse a @"host:port"@ string into a 'HostInfo'. Returns
'Left' with a reason if the format is wrong.
-}
parseHostInfo :: Text -> Either String HostInfo
parseHostInfo t = case T.splitOn ":" t of
  [h, pTxt] -> case T.decimal pTxt of
    Right (p, rest) | T.null rest -> Right (HostInfo h p)
    _ -> Left ("application.server: bad port: " <> T.unpack pTxt)
  _ -> Left ("application.server: expected host:port, got " <> T.unpack t)


{- | What one instance of the streams app looks like to its
peers. Mirrors Java's 'StreamsMetadata'.
-}
data StreamsMetadata = StreamsMetadata
  { host :: !HostInfo
  , stateStores :: !(Set Text)
  -- ^ Every state-store name this instance has materialised.
  , topicPartitions :: !(Set KC.TopicPartition)
  -- ^ Active partitions this instance owns.
  , standbyPartitions :: !(Set KC.TopicPartition)
  -- ^ Partitions held in standby.
  }
  deriving stock (Eq, Show, Generic)


{- | KIP-535 routing record: for a given @(store, key)@ which
instance owns the active and which (if any) owns the
standby. Used by external IQ proxies to decide where to
send the user's query.
-}
data KeyQueryMetadata = KeyQueryMetadata
  { activeHost :: !HostInfo
  , standbyHosts :: ![HostInfo]
  , partition :: !Int32
  }
  deriving stock (Eq, Show, Generic)


{- | Build a 'KeyQueryMetadata' from the metadata of every peer
in the group and the partition that owns @key@.
-}
makeKeyQueryMetadata
  :: [StreamsMetadata]
  -- ^ all peers' metadata
  -> Text
  -- ^ topic
  -> Int32
  -- ^ partition the key lands on
  -> Maybe KeyQueryMetadata
makeKeyQueryMetadata peers topic part =
  let tp = KC.TopicPartition topic part
      activeOf m = Set.member tp m.topicPartitions
      standbyOf m = Set.member tp m.standbyPartitions
      activePeers = filter activeOf peers
      standbyPeers = filter standbyOf peers
  in case activePeers of
       (a : _) ->
         Just
           KeyQueryMetadata
             { activeHost = a.host
             , standbyHosts = map (\m -> m.host) standbyPeers
             , partition = part
             }
       [] -> Nothing
