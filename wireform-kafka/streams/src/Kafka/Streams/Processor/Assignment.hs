{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Processor.Assignment
Description : User-pluggable task assignor (KIP-924)

The streams runtime deals tasks with its own lag-aware assignor
("Kafka.Streams.Runtime.GroupAssignor"). Java 4.0 exposes
@org.apache.kafka.streams.processor.assignment.TaskAssignor@ as
a /pluggable/ interface so applications can supply their own
placement policy.

This module is the Haskell mirror of that pluggable surface:

  * 'ApplicationState'             — read-only metadata about the
    rebalance the runtime asks the assignor to make a decision
    against.
  * 'KafkaStreamsState'            — per-client metadata: which
    consumer ids belong to it, what tasks it currently owns,
    rack id.
  * 'TaskInfo' / 'TaskTopicPartition' — per-task metadata.
  * 'TaskAssignor'                 — record-of-functions the user
    plugs into 'Kafka.Streams.Config.StreamsConfig.taskAssignor'.
  * 'TaskAssignment'               — the assignor's output.
  * 'AssignmentError' / 'AssignmentConfigs' — surface error codes
    and tunables matching Java's enum / record.

Set 'Kafka.Streams.Config.taskAssignor' and the group leader calls
'taAssign' at every rebalance instead of the built-in dealing. The
result is validated ('AssignmentError'; every task active exactly
once, no standby next to its active, only known processes and tasks)
and reported through 'taOnAssignmentComputed'; an invalid result, or
an exception, falls back to the built-in assignment.
-}
module Kafka.Streams.Processor.Assignment (
  -- * Input metadata
  ApplicationState (..),
  KafkaStreamsState (..),
  ProcessId (..),
  TaskInfo (..),
  TaskTopicPartition (..),

  -- * Output
  TaskAssignment (..),
  KafkaStreamsAssignment (..),
  AssignmentError (..),

  -- * The assignor
  TaskAssignor (..),
  defaultTaskAssignor,

  -- * Tunables (KIP-924 'AssignmentConfigs')
  AssignmentConfigs (..),
  defaultAssignmentConfigs,

  -- * Rack-aware sub-tunables
  RackAwareAssignmentConfigs (..),
  defaultRackAwareAssignmentConfigs,
) where

import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Kafka.Streams.Processor (TaskId)


----------------------------------------------------------------------
-- Identity types
----------------------------------------------------------------------

{- | Stable Kafka-streams-client identifier. Mirrors
@org.apache.kafka.streams.processor.assignment.ProcessId@.
-}
newtype ProcessId = ProcessId {unProcessId :: UUID}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


----------------------------------------------------------------------
-- Per-task metadata
----------------------------------------------------------------------

{- | A topic-partition a task subscribes to. Mirrors
@TaskTopicPartition@.
-}
data TaskTopicPartition = TaskTopicPartition
  { ttpTopic :: !Text
  , ttpPartition :: !Int32
  , ttpIsChangelog :: !Bool
  {- ^ 'True' for changelog source topics; 'False' for
  ordinary source topics.
  -}
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


{- | Per-task metadata. Mirrors
@org.apache.kafka.streams.processor.assignment.TaskInfo@.
-}
data TaskInfo = TaskInfo
  { tiTaskId :: !TaskId
  , tiTopicPartitions :: !(Set TaskTopicPartition)
  , tiIsStateful :: !Bool
  , tiStores :: !(Set Text)
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- Per-client metadata
----------------------------------------------------------------------

{- | A read-only metadata view of a single Kafka-streams client
participating in this rebalance. Mirrors
@KafkaStreamsState@.
-}
data KafkaStreamsState = KafkaStreamsState
  { kssProcessId :: !ProcessId
  , kssNumProcessingThreads :: !Int
  , kssClientTags :: !(Map Text Text)
  , kssPreviousActiveTasks :: !(Set TaskId)
  , kssPreviousStandbyTasks :: !(Set TaskId)
  , kssRackId :: !(Maybe Text)
  , kssTaskLags :: !(Map TaskId Int64)
  {- ^ Per stateful task: how many changelog records this client's
  state is behind (0 for the running owner; the whole changelog
  without local state). JVM: @KafkaStreamsState.lagFor@
  -}
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- Application-wide input
----------------------------------------------------------------------

{- | Read-only metadata for the rebalance the assignor is being
asked to make a decision against. Mirrors
@ApplicationState@.
-}
data ApplicationState = ApplicationState
  { asAllTasks :: !(Map TaskId TaskInfo)
  , asKafkaStreamsStates :: !(Map ProcessId KafkaStreamsState)
  , asAssignmentConfigs :: !AssignmentConfigs
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- Assignor output
----------------------------------------------------------------------

{- | What gets dealt to a single 'KafkaStreamsState'. Mirrors
@KafkaStreamsAssignment@.
-}
data KafkaStreamsAssignment = KafkaStreamsAssignment
  { ksaActiveTasks :: !(Set TaskId)
  , ksaStandbyTasks :: !(Set TaskId)
  , ksaFollowupRebalanceDeadlineMs :: !(Maybe Int)
  {- ^ If 'Just', the assignor wants the broker to schedule
  another rebalance no later than this many ms from now.
  -}
  }
  deriving stock (Eq, Show, Generic)


{- | The final assignment a 'TaskAssignor' returns. Mirrors the
nested @TaskAssignor.TaskAssignment@ wrapper class.
-}
data TaskAssignment = TaskAssignment
  { taAssignments :: !(Map ProcessId KafkaStreamsAssignment)
  }
  deriving stock (Eq, Show, Generic)


{- | Error codes the runtime can flag against a returned
'TaskAssignment'. Mirrors @TaskAssignor.AssignmentError@.
-}
data AssignmentError
  = AssignmentErrorNone
  | ActiveTaskAssignedMultipleTimes
  | InvalidStandbyTask
  | MissingProcessId
  -- ^ A client of the group got no entry
  | MissingActiveTask
  -- ^ A task has no active anywhere
  | UnknownProcessId
  | UnknownTaskId
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- The plug point
----------------------------------------------------------------------

{- | A user-supplied task assignor. Mirrors
@org.apache.kafka.streams.processor.assignment.TaskAssignor@:

  * 'taAssign'             — produce a 'TaskAssignment' from
    the 'ApplicationState'. May throw / signal a
    'TaskAssignmentException' to keep the previous
    assignment and schedule an immediate followup rebalance.
  * 'taOnAssignmentComputed' — fires after the runtime has
    validated the 'TaskAssignment'. Receives the validation
    error (or 'AssignmentErrorNone'). Useful for logging /
    metrics; not allowed to alter the assignment.
  * 'taConfigure'          — passed the streams configuration
    'Map' so a single 'TaskAssignor' instance can be re-used
    across applications with different tunables.
-}
data TaskAssignor = TaskAssignor
  { taAssign :: ApplicationState -> IO TaskAssignment
  , taOnAssignmentComputed :: TaskAssignment -> AssignmentError -> IO ()
  , taConfigure :: Map Text Text -> IO ()
  }


{- | The default assignor: hands every task to the first
registered client. Useful as a starting point for tests; not
a sensible production policy.
-}
defaultTaskAssignor :: TaskAssignor
defaultTaskAssignor =
  TaskAssignor
    { taAssign = \app -> do
        let !pids = Map.keys (asKafkaStreamsStates app)
            !tids = Map.keysSet (asAllTasks app)
        case pids of
          [] -> pure (TaskAssignment Map.empty)
          (p : _) ->
            pure
              ( TaskAssignment
                  ( Map.singleton
                      p
                      KafkaStreamsAssignment
                        { ksaActiveTasks = tids
                        , ksaStandbyTasks = Set.empty
                        , ksaFollowupRebalanceDeadlineMs = Nothing
                        }
                  )
              )
    , taOnAssignmentComputed = \_ _ -> pure ()
    , taConfigure = \_ -> pure ()
    }


----------------------------------------------------------------------
-- Tunables (KIP-924 AssignmentConfigs)
----------------------------------------------------------------------

{- | Tunables for the high-level assignor. Mirrors
@AssignmentConfigs@.
-}
data AssignmentConfigs = AssignmentConfigs
  { acNumStandbyReplicas :: !Int
  , acAcceptableRecoveryLag :: !Int
  , acMaxWarmupReplicas :: !Int
  , acProbingRebalanceIntervalMs :: !Int
  , acRackAware :: !RackAwareAssignmentConfigs
  }
  deriving stock (Eq, Show, Generic)


defaultAssignmentConfigs :: AssignmentConfigs
defaultAssignmentConfigs =
  AssignmentConfigs
    { acNumStandbyReplicas = 0
    , acAcceptableRecoveryLag = 10_000
    , acMaxWarmupReplicas = 2
    , acProbingRebalanceIntervalMs = 10 * 60 * 1000 -- 10 min
    , acRackAware = defaultRackAwareAssignmentConfigs
    }


{- | Mirrors @RackAwareAssignmentConfigs@. The cost knobs are
used by the assignor's optimiser to weight rack locality
against load balance.
-}
data RackAwareAssignmentConfigs = RackAwareAssignmentConfigs
  { raStrategy :: !Text
  -- ^ One of @\"none\"@, @\"min_traffic\"@, @\"balance_subtopology\"@.
  , raTrafficCost :: !Int
  , raNonOverlapCost :: !Int
  , raAssignmentTags :: ![Text]
  }
  deriving stock (Eq, Show, Generic)


defaultRackAwareAssignmentConfigs :: RackAwareAssignmentConfigs
defaultRackAwareAssignmentConfigs =
  RackAwareAssignmentConfigs
    { raStrategy = "none"
    , raTrafficCost = 10
    , raNonOverlapCost = 1
    , raAssignmentTags = []
    }
