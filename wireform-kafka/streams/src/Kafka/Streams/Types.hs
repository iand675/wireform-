{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Kafka.Streams.Types
-- Description : Record / Headers / metadata types used by Streams
module Kafka.Streams.Types
  ( -- * Records
    Record (..)
  , mkRecord
  , mapKey
  , mapValue
  , mapKV
    -- * Headers
  , Header (..)
  , Headers
  , emptyHeaders
  , headersFromList
  , headersToList
  , addHeader
  , removeHeader
  , lastHeader
  , allHeaders
    -- * Topic-Partition
  , TopicPartition (..)
  , TopicName
  , topicName
  , unTopicName
    -- * Node names (shared between Topology + Processor)
  , NodeName (..)
  , nodeName
    -- * Source metadata
  , RecordMetadata (..)
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.List (foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Kafka.Streams.Time (Timestamp)

-- | Topic name; thin newtype to avoid mixing topic names with other
-- 'Text' values.
newtype TopicName = TopicName { unTopicName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | String literals desugar straight to 'TopicName' under
-- @OverloadedStrings@, which mirrors how Kafka topic names appear in
-- config files and tests. The constructor itself stays unexported so
-- everything still goes through the 'Text' boundary.
instance IsString TopicName where
  fromString = TopicName . Text.pack

topicName :: Text -> TopicName
topicName = TopicName

-- | Topic-partition pair. Identical to the one in
-- "Kafka.Client.Consumer" but defined here to keep the streams
-- package self-contained.
data TopicPartition = TopicPartition
  { tpTopic     :: !TopicName
  , tpPartition :: !Int32
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | A single header. Kafka allows duplicate keys so we keep header
-- order through 'Headers'.
data Header = Header
  { headerKey   :: !Text
  , headerValue :: !ByteString
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | Ordered, possibly-duplicating header collection (matches the Java
-- @org.apache.kafka.common.header.Headers@ semantics).
newtype Headers = Headers { headerEntries :: Seq Header }
  deriving stock (Eq, Show, Generic)

-- | Concatenation, preserving order and duplicate keys (Kafka
-- permits duplicates). Lets two header sets be merged with '<>'.
instance Semigroup Headers where
  Headers a <> Headers b = Headers (a <> b)

-- | 'mempty' is the empty header set.
instance Monoid Headers where
  mempty = emptyHeaders

emptyHeaders :: Headers
emptyHeaders = Headers Seq.empty

headersFromList :: [Header] -> Headers
headersFromList = Headers . Seq.fromList

headersToList :: Headers -> [Header]
headersToList (Headers s) = foldr (:) [] s

addHeader :: Header -> Headers -> Headers
addHeader h (Headers s) = Headers (s Seq.|> h)

removeHeader :: Text -> Headers -> Headers
removeHeader k (Headers s) =
  Headers (Seq.filter ((/= k) . headerKey) s)

-- | Last header with a given key (mirrors @Headers#lastHeader@).
lastHeader :: Text -> Headers -> Maybe Header
lastHeader k (Headers s) =
  case Seq.viewr (Seq.filter ((== k) . headerKey) s) of
    Seq.EmptyR -> Nothing
    _ Seq.:> h -> Just h

-- | All headers with the given key, in insertion order.
allHeaders :: Text -> Headers -> [Header]
allHeaders k (Headers s) =
  foldl' (\acc h -> if headerKey h == k then h : acc else acc) []
    (Seq.reverse s)

-- | A record flowing through the topology. Mirrors the Java
-- @org.apache.kafka.streams.processor.api.Record@ shape (key, value,
-- timestamp, headers) plus the source 'RecordMetadata' (which is the
-- Java @ProcessorContext#recordMetadata@).
data Record k v = Record
  { recordKey       :: !(Maybe k)
  , recordValue     :: !v
  , recordTimestamp :: !Timestamp
  , recordHeaders   :: !Headers
  }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | A record is a 'Bifunctor' over its (optional) key and its
-- value: 'first' maps the key (inside the 'Maybe'), 'second'
-- maps the value, and 'bimap' does both at once. This is the
-- typeclass-level companion to the explicit 'mapKey' \/
-- 'mapValue' \/ 'mapKV' helpers — note 'first' lifts a
-- @k -> k'@ through the 'Maybe', whereas 'mapKey' operates on
-- the @'Maybe' k@ directly. The derived 'Functor' \/ 'Foldable'
-- \/ 'Traversable' instances all range over the value, so
-- @'second' = 'fmap'@.
instance Bifunctor Record where
  bimap f g r = r
    { recordKey   = fmap f (recordKey r)
    , recordValue = g (recordValue r)
    }
  first f r  = r { recordKey   = fmap f (recordKey r) }
  second g r = r { recordValue = g (recordValue r) }

mkRecord :: Maybe k -> v -> Timestamp -> Record k v
mkRecord k v t = Record k v t emptyHeaders

mapKey :: (Maybe k -> Maybe k') -> Record k v -> Record k' v
mapKey f r = r { recordKey = f (recordKey r) }

mapValue :: (v -> v') -> Record k v -> Record k v'
mapValue f r = r { recordValue = f (recordValue r) }

mapKV :: (Maybe k -> v -> (Maybe k', v')) -> Record k v -> Record k' v'
mapKV f r =
  let (k', v') = f (recordKey r) (recordValue r)
   in r { recordKey = k', recordValue = v' }

-- | Source-side metadata (broker offset, partition, original topic).
-- Available inside processors via 'Kafka.Streams.Processor.recordMetadata'.
data RecordMetadata = RecordMetadata
  { rmTopic     :: !TopicName
  , rmPartition :: !Int32
  , rmOffset    :: !Int64
  }
  deriving stock (Eq, Show, Generic)

-- | Node identifier used by both the topology graph and processor
-- forwarding. Every source / processor / sink in the topology has a
-- unique 'NodeName'.
newtype NodeName = NodeName { unNodeName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | String literals desugar to 'NodeName' under
-- @OverloadedStrings@, matching the existing 'TopicName'
-- treatment so node names can be written inline.
instance IsString NodeName where
  fromString = NodeName . Text.pack

nodeName :: Text -> NodeName
nodeName = NodeName
