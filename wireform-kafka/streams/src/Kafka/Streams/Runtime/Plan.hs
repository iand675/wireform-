{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.Plan
Description : From a topology to the tasks, topics and stores the runtime runs

What the JVM's @InternalTopologyBuilder@ + @RepartitionTopics@ +
@ChangelogTopics@ work out at startup:

  * the subtopologies ("Kafka.Streams.Topology.Subtopology"), each
    restricted to its own nodes and with internal topics renamed to
    @\<application.id\>-\<name\>@;
  * how many tasks each one has: the most partitions of any of its
    source topics. A repartition topic without an explicit count gets
    the most partitions of the topics feeding the subtopology that
    writes it;
  * which stores are change-logged, and where;
  * the global part, if any.

It also creates the internal topics and refuses to start when the
cluster cannot run the topology correctly: missing source topics,
stateful subtopologies over topics with different partition counts
(a join or aggregation would see keys split across tasks), an
existing internal topic with the wrong partition count, or a logged
store without serdes.
-}
module Kafka.Streams.Runtime.Plan (
  RuntimePlan (..),
  SubPlan (..),
  GlobalPlan (..),
  buildRuntimePlan,
  taskDirName,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Consumed (AutoOffsetReset)
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Driver (NewInternalTopic (..), StreamDriver (..))
import Kafka.Streams.State.Store (LoggingConfig (..), StoreName, unStoreName)
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Subtopology
import Kafka.Streams.Types (unTopicName)


-- | One subtopology, ready to instantiate as tasks.
data SubPlan = SubPlan
  { subPlanId :: !Int
  , subPlanTopology :: !Topo.TopologyValid
  -- ^ Restricted to this subtopology's nodes and stores
  , subPlanSourceTopics :: ![Text]
  , subPlanTasks :: !Int
  -- ^ Number of tasks (partitions)
  , subPlanChangelogs :: !(Map StoreName ChangelogTopic)
  -- ^ The logged stores and their changelogs
  , subPlanOffsetReset :: !(Map Text AutoOffsetReset)
  -- ^ Per source topic: where to start without a committed offset
  }


data GlobalPlan = GlobalPlan
  { globalPlanTopology :: !Topo.TopologyValid
  , globalPlanTopics :: ![Text]
  , globalPlanStores :: ![StoreName]
  }


data RuntimePlan = RuntimePlan
  { rpSubtopologies :: !(Map Int SubPlan)
  , rpTopicSubtopologies :: !(Map Text [Int])
  -- ^ Which subtopologies read each subscribed topic
  , rpSubscription :: ![Text]
  -- ^ Every topic the main consumer subscribes to (sources + repartitions)
  , rpRepartitionTopics :: ![Text]
  , rpPartitionCounts :: !(Map Text Int)
  -- ^ Every source, repartition and global topic's partition count
  , rpGlobal :: !(Maybe GlobalPlan)
  }


-- | A task's directory name under @\<state.dir\>/\<application.id\>@.
taskDirName :: TaskId -> FilePath
taskDirName (TaskId s p) = show s <> "_" <> show p


{- | Work out the plan for @valid@ against the cluster behind @drv@,
creating the internal topics (with @replication@ replicas, @-1@ for
the broker default).
-}
buildRuntimePlan :: Text -> Int -> StreamDriver -> Topo.TopologyValid -> IO (Either Text RuntimePlan)
buildRuntimePlan appId replication drv valid = do
  let topo = prefixInternalTopics appId (Topo.topologyValidGraph valid)
      plan = planSubtopologies topo
      subs = spSubtopologies plan
      internal = topoInternalTopicsText topo
      txt = unTopicName
      allSources = concatMap (map txt . subSourceTopics) subs <> map txt (spGlobalTopics plan)
      external = dedup (filter (`Map.notMember` internal) allSources)
      patterned = mapMaybe Topo.sourcePattern (Map.elems (Topo.topoSources topo))
  if not (null patterned)
    then pure (Left ("pattern subscriptions are not supported: " <> T.intercalate ", " patterned))
    else do
      eDesc <- sdDescribeTopics drv external
      case eDesc of
        Left e -> pure (Left ("describe source topics: " <> T.pack e))
        Right known -> do
          let missing = filter (`Map.notMember` known) external
          if not (null missing)
            then pure (Left ("missing source topics: " <> T.intercalate ", " missing))
            else case resolveCounts subs internal known of
              Left e -> pure (Left e)
              Right counts -> finish topo plan internal counts
  where
    finish topo plan internal counts = do
      let subs = spSubtopologies plan
          txt = unTopicName
          taskCount sub = maximum (1 : map (\t -> Map.findWithDefault 1 (txt t) counts) (subSourceTopics sub))
          builderOf sn = Map.lookup sn (Topo.topoStores topo)
          isLogged sn = maybe False loggingEnabled (builderOf sn >>= Topo.storeBuilderLogging)
          logged sub =
            Map.fromList
              (map (\sn -> (sn, changelogTopicFor appId topo sn)) (filter isLogged (subStores sub)))
          loggedOf sub = Map.toList (logged sub)
          noSerdes =
            map unStoreName $
              filter
                (\sn -> maybe False (not . Topo.storeBuilderHasSerdes) (builderOf sn))
                (concatMap (Map.keys . logged) subs)
          sourceCounts sub = map (\t -> Map.findWithDefault 1 (txt t) counts) (subSourceTopics sub)
          notCopartitioned =
            map subId $
              filter
                (\sub -> not (null (subStores sub)) && Set.size (Set.fromList (sourceCounts sub)) > 1)
                subs
          changelogTopics =
            concatMap
              ( \sub ->
                  map
                    ( \(sn, cl) ->
                        NewInternalTopic
                          { nitName = txt (clTopic cl)
                          , nitPartitions = taskCount sub
                          , nitReplicationFactor = replication
                          , nitConfigs = maybe [] loggingTopicCfg (builderOf sn >>= Topo.storeBuilderLogging)
                          }
                    )
                    (filter (clInternal . snd) (loggedOf sub))
              )
              subs
          repartitionTopics =
            map
              ( \t ->
                  NewInternalTopic
                    { nitName = t
                    , nitPartitions = Map.findWithDefault 1 t counts
                    , nitReplicationFactor = replication
                    , nitConfigs = [("cleanup.policy", "delete"), ("retention.ms", "-1")]
                    }
              )
              (Map.keys internal)
          -- A store that reuses its source topic as changelog needs the
          -- topic's partitions to line up with its tasks.
          reuseMismatch =
            concatMap
              ( \sub ->
                  map
                    (unStoreName . fst)
                    ( filter
                        (\(_, cl) -> not (clInternal cl) && Map.findWithDefault 1 (txt (clTopic cl)) counts /= taskCount sub)
                        (loggedOf sub)
                    )
              )
              subs
      if
        | not (null noSerdes) ->
            pure . Left $
              "logged stores without serdes (give them serdes or disable logging): "
                <> T.intercalate ", " noSerdes
        | not (null notCopartitioned) ->
            pure . Left $
              "stateful subtopologies over source topics with different partition counts (not co-partitioned): "
                <> T.intercalate ", " (map (T.pack . show) notCopartitioned)
        | not (null reuseMismatch) ->
            pure (Left ("stores whose source-topic changelog doesn't match their task count: " <> T.intercalate ", " reuseMismatch))
        | otherwise -> do
            let wanted = repartitionTopics <> changelogTopics
            eCreate <- sdCreateTopics drv wanted
            eDesc <- sdDescribeTopics drv (map nitName wanted)
            case (eCreate, eDesc) of
              (Left e, _) -> pure (Left ("create internal topics: " <> T.pack e))
              (_, Left e) -> pure (Left ("describe internal topics: " <> T.pack e))
              (Right (), Right got) -> do
                let wrong =
                      mapMaybe
                        ( \nt -> case Map.lookup (nitName nt) got of
                            Just have
                              | have /= nitPartitions nt ->
                                  Just (nitName nt <> " has " <> T.pack (show have) <> " partitions, needs " <> T.pack (show (nitPartitions nt)))
                            _ -> Nothing
                        )
                        wanted
                    absent = map nitName (filter (\nt -> Map.notMember (nitName nt) got) wanted)
                if
                  | not (null absent) -> pure (Left ("internal topics were not created: " <> T.intercalate ", " absent))
                  | not (null wrong) -> pure (Left ("internal topics with the wrong partition count: " <> T.intercalate "; " wrong))
                  | otherwise -> pure (Right (assemble topo plan internal counts taskCount logged))

    assemble topo plan internal counts taskCount logged =
      let subs = spSubtopologies plan
          txt = unTopicName
          resetFor sub =
            Map.fromList
              ( concatMap
                  (\spec -> map (\t -> (txt t, Topo.sourceOffsetReset spec)) (Topo.sourceTopics spec))
                  (Map.elems (Map.restrictKeys (Topo.topoSources topo) (subNodes sub)))
              )
          subPlans =
            Map.fromList
              ( map
                  ( \sub ->
                      ( subId sub
                      , SubPlan
                          { subPlanId = subId sub
                          , subPlanTopology = Topo.unsafeAssumeValid (restrictTopology (subNodes sub) topo)
                          , subPlanSourceTopics = map txt (subSourceTopics sub)
                          , subPlanTasks = taskCount sub
                          , subPlanChangelogs = logged sub
                          , subPlanOffsetReset = resetFor sub
                          }
                      )
                  )
                  subs
              )
          global
            | Set.null (spGlobalNodes plan) = Nothing
            | otherwise =
                Just
                  GlobalPlan
                    { globalPlanTopology = Topo.unsafeAssumeValid (restrictTopology (spGlobalNodes plan) topo)
                    , globalPlanTopics = map txt (spGlobalTopics plan)
                    , globalPlanStores = spGlobalStores plan
                    }
      in RuntimePlan
           { rpSubtopologies = subPlans
           , rpTopicSubtopologies =
               Map.fromListWith
                 (flip (<>))
                 (concatMap (\sub -> map (\t -> (txt t, [subId sub])) (subSourceTopics sub)) subs)
           , rpSubscription = dedup (concatMap (map txt . subSourceTopics) subs)
           , rpRepartitionTopics = Map.keys internal
           , rpPartitionCounts = counts
           , rpGlobal = global
           }


topoInternalTopicsText :: Topo.Topology -> Map Text Topo.InternalTopicSpec
topoInternalTopicsText = Map.mapKeys unTopicName . Topo.topoInternalTopics


{- | Partition counts of every topic: the external ones as found; a
repartition topic its declared count, or, when a stateful
subtopology reads it alongside topics whose counts are known, the
most partitions of those (it must be co-partitioned with them, as the
JVM's @CopartitionedTopicsEnforcer@ requires); otherwise the most
partitions of the topics feeding the subtopology that writes it.
Resolved until nothing changes.
-}
resolveCounts :: [Subtopology] -> Map Text Topo.InternalTopicSpec -> Map Text Int -> Either Text (Map Text Int)
resolveCounts subs internal = go (2 * Map.size internal + 1)
  where
    txt = unTopicName
    writers r = filter (\sub -> r `elem` map txt (subSinkTopics sub)) subs
    statefulReaders r = filter (\sub -> not (null (subStores sub)) && r `elem` map txt (subSourceTopics sub)) subs
    go :: Int -> Map Text Int -> Either Text (Map Text Int)
    go fuel counts
      | all (`Map.member` counts) (Map.keys internal) = Right counts
      | fuel <= 0 =
          Left ("cannot work out the partition count of " <> T.intercalate ", " (filter (`Map.notMember` counts) (Map.keys internal)))
      | otherwise =
          let step acc (r, spec)
                | Map.member r acc = acc
                | Just n <- Topo.itsPartitions spec = Map.insert r n acc
                | not (null copartitioned) = Map.insert r (maximum copartitioned) acc
                | not (null feeding) && all (`Map.member` acc) feeding = Map.insert r (maximum (map (acc Map.!) feeding)) acc
                | otherwise = acc
                where
                  copartitioned =
                    mapMaybe
                      (`Map.lookup` acc)
                      (filter (/= r) (concatMap (map txt . subSourceTopics) (statefulReaders r)))
                  feeding = concatMap (map txt . subSourceTopics) (writers r)
          in go (fuel - 1) (foldl step counts (Map.toList internal))


dedup :: Ord a => [a] -> [a]
dedup = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs
