{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.Topology.Subtopology
Description : Split a topology into independently partitioned subtopologies

The JVM runtime never runs a topology as one unit. It cuts the graph
at topics (a repartition topic's sink and source land in different
pieces), keeps nodes that share a state store together, and runs one
/task/ per (subtopology, partition): its own processors, its own
stores, its own changelog partition. A record from partition @p@ of
any source topic of subtopology @s@ goes to task @(s, p)@, which is
what makes co-partitioned joins and per-key state correct.

The global part (nodes feeding a global store) is separate: one
instance of it per process, fed every partition of its topics.

This module computes that split and the pieces the runtime needs to
build a task: the subtopology restricted to its own nodes, and the
internal topic names prefixed with the application id.
-}
module Kafka.Streams.Topology.Subtopology (
  Subtopology (..),
  SubtopologyPlan (..),
  planSubtopologies,
  restrictTopology,
  prefixInternalTopics,
  internalTopicName,
  changelogTopicFor,
  ChangelogTopic (..),
) where

import Data.Foldable (toList)
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Kafka.Streams.State.Store (StoreName, unStoreName)
import Kafka.Streams.Topology (
  NodeName,
  ProcessorSpec (..),
  SinkSpec (..),
  SourceSpec (..),
  Topology,
  effectiveChangelogReuse,
  parentsOf,
  topoChangelogPlan,
  topoChildrenIndex,
  topoGlobalStores,
  topoInternalTopics,
  topoOrder,
  topoProcessors,
  topoSinks,
  topoSourceOrder,
  topoSources,
  topoStoreOwners,
  topoStores,
 )
import Kafka.Streams.Types (TopicName, topicName, unTopicName)


-- | One independently partitioned piece of the topology.
data Subtopology = Subtopology
  { subId :: !Int
  -- ^ Stable index, by the first node's insertion order
  , subNodes :: !(Set NodeName)
  , subSourceTopics :: ![TopicName]
  -- ^ Every topic a source node of this subtopology reads
  , subSinkTopics :: ![TopicName]
  , subStores :: ![StoreName]
  }
  deriving stock (Eq, Show)


data SubtopologyPlan = SubtopologyPlan
  { spSubtopologies :: ![Subtopology]
  , spGlobalNodes :: !(Set NodeName)
  -- ^ Nodes feeding global stores (and those stores' sources)
  , spGlobalTopics :: ![TopicName]
  , spGlobalStores :: ![StoreName]
  }
  deriving stock (Eq, Show)


{- | Split @t@: nodes linked by a parent edge or by a shared store
form one subtopology; nodes upstream of a global store form the
global part.
-}
planSubtopologies :: Topology -> SubtopologyPlan
planSubtopologies t =
  let !order = toList (topoOrder t)
      !rank = Map.fromList (zip order [0 :: Int ..])
      !globalStores = Set.toList (topoGlobalStores t)
      ownersOf sn = Map.findWithDefault [] sn (topoStoreOwners t)
      usersOf = storeUsers t
      -- The global part: owners of global stores and everything
      -- upstream of them.
      ancestors = go Set.empty
        where
          go seen [] = seen
          go seen (n : ns)
            | Set.member n seen = go seen ns
            | otherwise = go (Set.insert n seen) (parentsOf t n <> ns)
      !globalNodes = ancestors (concatMap ownersOf globalStores)
      regular = filter (`Set.notMember` globalNodes) order
      -- Union-find over node names (a plain map of representatives;
      -- topologies are small).
      edges =
        concatMap (\n -> map (\p -> (n, p)) (parentsOf t n)) regular
          <> concatMap
            ( \sn -> case filter (`Set.notMember` globalNodes) (usersOf sn) of
                (o : os) -> map (\o' -> (o, o')) os
                [] -> []
            )
            (filter (`notElem` globalStores) (Map.keys (topoStores t)))
      reps0 = Map.fromList (map (\n -> (n, n)) regular)
      find reps n = case Map.lookup n reps of
        Just p | p /= n -> find reps p
        _ -> n
      union reps (a, b)
        | Map.member a reps && Map.member b reps =
            let ra = find reps a
                rb = find reps b
            in if ra == rb
                 then reps
                 else
                   -- keep the earlier-inserted node as representative
                   if Map.findWithDefault 0 ra rank <= Map.findWithDefault 0 rb rank
                     then Map.insert rb ra reps
                     else Map.insert ra rb reps
        | otherwise = reps
      reps = foldl' union reps0 edges
      groups =
        Map.elems
          ( Map.fromListWith
              (flip (<>))
              (map (\n -> (find reps n, [n])) regular)
          )
      ordered = sortOn (\ns -> minimum (map (\n -> Map.findWithDefault 0 n rank) ns)) groups
      mkSub i ns =
        let !nodeSet = Set.fromList ns
        in Subtopology
             { subId = i
             , subNodes = nodeSet
             , subSourceTopics =
                 dedup
                   ( concatMap
                       sourceTopics
                       (Map.elems (Map.restrictKeys (topoSources t) nodeSet))
                   )
             , subSinkTopics =
                 dedup
                   (map sinkTopic (Map.elems (Map.restrictKeys (topoSinks t) nodeSet)))
             , subStores =
                 filter
                   (\sn -> notElem sn globalStores && any (`Set.member` nodeSet) (usersOf sn))
                   (Map.keys (topoStores t))
             }
  in SubtopologyPlan
       { spSubtopologies = zipWith mkSub [0 ..] ordered
       , spGlobalNodes = globalNodes
       , spGlobalTopics =
           dedup
             ( concatMap
                 sourceTopics
                 (Map.elems (Map.restrictKeys (topoSources t) globalNodes))
             )
       , spGlobalStores = globalStores
       }


{- | The nodes that touch a store: its owners and every processor that
declares it ('processorSpecStores'), all of which must share a task.
-}
storeUsers :: Topology -> StoreName -> [NodeName]
storeUsers t sn =
  Map.findWithDefault [] sn (topoStoreOwners t)
    <> Map.keys (Map.filter (elem sn . processorSpecStores) (topoProcessors t))


dedup :: Ord a => [a] -> [a]
dedup = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs


{- | The topology with only @keep@'s nodes and the stores they own;
the shape one task's engine is built from.
-}
restrictTopology :: Set NodeName -> Topology -> Topology
restrictTopology keep t =
  let -- a global store belongs with its owners (the global part); a
      -- task processor reading it sees the shared copy
      keptStore sn
        | Set.member sn (topoGlobalStores t) = any (`Set.member` keep) (Map.findWithDefault [] sn (topoStoreOwners t))
        | otherwise = any (`Set.member` keep) (storeUsers t sn)
      !stores = Map.filterWithKey (\sn _ -> keptStore sn) (topoStores t)
  in t
       { topoSources = Map.restrictKeys (topoSources t) keep
       , topoProcessors =
           Map.map
             (\p -> p {processorSpecParents = filter (`Set.member` keep) (processorSpecParents p)})
             (Map.restrictKeys (topoProcessors t) keep)
       , topoSinks = Map.restrictKeys (topoSinks t) keep
       , topoStores = stores
       , topoStoreOwners =
           Map.map (filter (`Set.member` keep)) (Map.restrictKeys (topoStoreOwners t) (Map.keysSet stores))
       , topoOrder = Seq.filter (`Set.member` keep) (topoOrder t)
       , topoSourceOrder = Seq.filter (`Set.member` keep) (topoSourceOrder t)
       , topoChildrenIndex =
           Map.map (filter (`Set.member` keep)) (Map.restrictKeys (topoChildrenIndex t) keep)
       , topoGlobalStores = Set.filter (`Map.member` stores) (topoGlobalStores t)
       , topoChangelogPlan = Map.restrictKeys (topoChangelogPlan t) (Map.keysSet stores)
       }


-- | @\<application.id\>-\<name\>@, the broker name of an internal topic.
internalTopicName :: Text -> TopicName -> TopicName
internalTopicName appId tp = topicName (appId <> "-" <> unTopicName tp)


{- | Rename every internal (repartition) topic to its broker name in
sources, sinks and the internal-topic table.
-}
prefixInternalTopics :: Text -> Topology -> Topology
prefixInternalTopics appId t =
  let !internal = topoInternalTopics t
      rename tp = if Map.member tp internal then internalTopicName appId tp else tp
  in t
       { topoSources = Map.map (\s -> s {sourceTopics = map rename (sourceTopics s)}) (topoSources t)
       , topoSinks = Map.map (\s -> s {sinkTopic = rename (sinkTopic s)}) (topoSinks t)
       , topoInternalTopics = Map.mapKeys (internalTopicName appId) internal
       , topoChangelogPlan = Map.map rename (topoChangelogPlan t)
       }


{- | Where a store's changelog lives: the source topic it reuses
(KIP-295, 'effectiveChangelogReuse'), or the internal
@\<application.id\>-\<store\>-changelog@ topic.
-}
changelogTopicFor :: Text -> Topology -> StoreName -> ChangelogTopic
changelogTopicFor appId t sn = case effectiveChangelogReuse t sn of
  Just src -> ChangelogTopic {clTopic = src, clInternal = False}
  Nothing ->
    ChangelogTopic
      { clTopic = topicName (appId <> "-" <> unStoreName sn <> "-changelog")
      , clInternal = True
      }


data ChangelogTopic = ChangelogTopic
  { clTopic :: !TopicName
  , clInternal :: !Bool
  {- ^ 'True' for a dedicated changelog the runtime creates and
  writes; 'False' when the store reuses a source topic (read on
  restore, never written).
  -}
  }
  deriving stock (Eq, Show)

