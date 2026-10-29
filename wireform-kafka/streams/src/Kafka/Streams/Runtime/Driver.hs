{- |
Module      : Kafka.Streams.Runtime.Driver
Description : Everything the streams runtime needs from a Kafka cluster

The runtime never talks to a client library directly; it talks to a
'StreamDriver', a record of the operations a Kafka Streams instance
performs against the cluster:

  * the /main consumer/: a member of the application's consumer group
    (@group.id = application.id@), subscribed to every source and
    repartition topic, whose assignor is the application's (a
    'GroupAssignor': "Kafka.Streams.Runtime.GroupAssignor" deals the
    tasks). Its rebalance callbacks run synchronously
    inside 'sdPoll' (so the runtime can commit and hand tasks over at
    exactly the right moment), and it commits explicit offsets.
  * /readers/: group-less consumers assigned to explicit partitions,
    used to restore stores from their changelogs and to keep global
    stores current.
  * the /producer/: sends asynchronously and, under exactly-once,
    runs the transactions that commit output, changelog writes and
    consumed offsets atomically.
  * /admin/: creates the internal (repartition and changelog) topics.

"Kafka.Streams.Runtime.NativeDriver" builds one over the real
clients; "Kafka.Streams.Runtime.MockBroker" over an in-memory broker
for tests.
-}
module Kafka.Streams.Runtime.Driver (
  StreamDriver (..),
  RebalanceCallbacks (..),
  noRebalanceCallbacks,
  GroupAssignor (..),
  Reader (..),
  NewInternalTopic (..),
  DriverRecord (..),
) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Kafka.Client.Assignor (CustomAssignor)
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP


{- | Called by the main consumer, on the thread that called 'sdPoll',
around a group rebalance.
-}
data RebalanceCallbacks = RebalanceCallbacks
  { rcBeforeRejoin :: !(IO ())
  {- ^ Still a member of the old generation: the last chance to commit
  what was processed for partitions that may move. Not called when
  the member was fenced (its commits would be rejected).
  -}
  , rcRevoked :: !([KC.TopicPartition] -> IO ())
  -- ^ Partitions no longer assigned after an orderly rebalance
  , rcAssigned :: !([KC.TopicPartition] -> IO ())
  -- ^ Partitions newly assigned; their positions are the committed offsets
  , rcLost :: !([KC.TopicPartition] -> IO ())
  -- ^ Partitions taken away while fenced: nothing may be committed for them
  , rcAssignment :: !([KC.TopicPartition] -> ByteString -> IO ())
  {- ^ After every completed rebalance (after the callbacks above, also
  when the partitions didn't change): the whole assignment and the
  user data the leader's assignor attached for this member
  -}
  }


-- | How the group's leader deals partitions, and what each member tells it.
data GroupAssignor = GroupAssignor
  { gaAssignor :: !CustomAssignor
  , gaSubscriptionUserData :: !(IO ByteString)
  -- ^ Read at every join
  }


noRebalanceCallbacks :: RebalanceCallbacks
noRebalanceCallbacks =
  RebalanceCallbacks
    { rcBeforeRejoin = pure ()
    , rcRevoked = \_ -> pure ()
    , rcAssigned = \_ -> pure ()
    , rcLost = \_ -> pure ()
    , rcAssignment = \_ _ -> pure ()
    }


-- | A record as a consumer hands it over.
data DriverRecord = DriverRecord
  { drTopic :: !Text
  , drPartition :: !Int32
  , drOffset :: !Int64
  , drTimestamp :: !Int64
  , drKey :: !(Maybe ByteString)
  , drValue :: !(Maybe ByteString)
  -- ^ 'Nothing' is a null value (a tombstone)
  , drHeaders :: ![(Text, ByteString)]
  }
  deriving stock (Eq, Show)


{- | A group-less consumer over explicitly assigned partitions. Reads
with the driver's isolation level, so under exactly-once it never
returns records of open or aborted transactions, and 'rdEndOffsets'
is the last stable offset.
-}
data Reader = Reader
  { rdAssign :: !([(KC.TopicPartition, Int64)] -> IO (Either String ()))
  {- ^ Replace the assignment, each partition positioned at the given
  offset (@-2@: the log start).
  -}
  , rdPoll :: !(Int -> IO (Either String [DriverRecord]))
  , rdPosition :: !(KC.TopicPartition -> IO (Either String Int64))
  {- ^ The next offset to read. Can be past the last returned record
  when the log has transaction markers or compacted gaps.
  -}
  , rdEndOffsets :: !([KC.TopicPartition] -> IO (Either String (Map KC.TopicPartition Int64)))
  , rdClose :: !(IO ())
  }


-- | An internal topic the runtime needs to exist.
data NewInternalTopic = NewInternalTopic
  { nitName :: !Text
  , nitPartitions :: !Int
  , nitReplicationFactor :: !Int
  -- ^ @-1@: the broker default
  , nitConfigs :: ![(Text, Text)]
  }
  deriving stock (Eq, Show)


data StreamDriver = StreamDriver
  { -- Main consumer ------------------------------------------------------
    sdSubscribe :: !([Text] -> GroupAssignor -> RebalanceCallbacks -> IO (Either String ()))
  -- ^ Join the group, subscribed to these topics.
  , sdPoll :: !(Int -> IO (Either String [DriverRecord]))
  {- ^ Fetch records, waiting up to the given milliseconds. Rebalances
  (and their callbacks) happen inside this call.
  -}
  , sdCommitOffsets :: !([(KC.TopicPartition, Int64)] -> IO (Either String ()))
  -- ^ Commit these next-offsets-to-read for the group (at-least-once).
  , sdCommitted :: !([KC.TopicPartition] -> IO (Either String (Map KC.TopicPartition Int64)))
  -- ^ The group's committed offsets; partitions without one are absent.
  , sdSeek :: !(KC.TopicPartition -> Int64 -> IO (Either String ()))
  -- ^ Reposition the main consumer (@-2@: log start, @-1@: log end).
  , sdPause :: !([KC.TopicPartition] -> IO ())
  , sdResume :: !([KC.TopicPartition] -> IO ())
  , sdRequestRejoin :: !(IO ())
  -- ^ Ask for a rebalance at the next poll.
  , sdCloseConsumer :: !(Bool -> Int -> IO ())
  -- ^ Leave the group (if the flag is set), waiting at most this many ms.
  , -- Readers -----------------------------------------------------------
    sdNewReader :: !(IO (Either String Reader))
  , -- Producer ----------------------------------------------------------
    sdSend :: !(KP.ProducerRecord -> (Either String KP.RecordMetadata -> IO ()) -> IO (Either String ()))
  {- ^ Enqueue a record; the callback runs once it is acknowledged or
  has failed. A 'Left' result means it was never enqueued.
  -}
  , sdFlush :: !(IO (Either String ()))
  -- ^ Wait until every enqueued record is acknowledged or failed.
  , sdPartitionsFor :: !(Text -> IO (Either String Int))
  , sdCloseProducer :: !(IO ())
  , -- Transactions (exactly-once) ---------------------------------------
    sdInitTransactions :: !(IO (Either String ()))
  -- ^ Fence older producers with this transactional id; once, before any send.
  , sdBeginTransaction :: !(IO (Either String ()))
  , sdSendOffsetsToTransaction :: !([(KC.TopicPartition, Int64)] -> IO (Either String ()))
  {- ^ Commit these consumed offsets with the open transaction, fenced
  by the main consumer's current group generation (KIP-447).
  -}
  , sdCommitTransaction :: !(IO (Either String ()))
  , sdAbortTransaction :: !(IO (Either String ()))
  , -- Admin -------------------------------------------------------------
    sdCreateTopics :: !([NewInternalTopic] -> IO (Either String ()))
  -- ^ Create the topics that don't exist; existing ones are left alone.
  , sdDescribeTopics :: !([Text] -> IO (Either String (Map Text Int)))
  -- ^ Partition counts of the topics that exist.
  , sdListTopics :: !(IO (Either String [Text]))
  -- ^ Every topic in the cluster but Kafka's internal ones (for pattern sources).
  , sdPartitionRacks :: !([KC.TopicPartition] -> IO (Either String (Map KC.TopicPartition (Set Text))))
  {- ^ The racks of the brokers holding each partition's replicas (for
  rack-aware assignment); a partition with no rack-tagged replica maps
  to the empty set
  -}
  -- ^ Every topic in the cluster but Kafka's internal ones (for pattern sources).
  , sdDeleteRecords :: !([(KC.TopicPartition, Int64)] -> IO (Either String ()))
  -- ^ Drop everything before these offsets (purging consumed repartition records).
  , sdClose :: !(IO ())
  -- ^ Release whatever the driver itself holds (admin client, ...).
  }
