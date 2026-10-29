{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Runtime.GroupAssignor
Description : The streams group assignor: tasks, standbys, warmups

Instances of one application share its tasks through a consumer
group whose protocol is this assignor (@"streams"@). Every member's
JoinGroup subscription carries a 'StreamsSubscription': its process
id, its @application.server@, its thread count, and for every task it
holds local state for, the sum of the changelog offsets that state
covers ('runningActive' for a task it is running). The leader
measures that against the changelogs' end offsets ('taskLag') and
deals the tasks ('assignTasks'), much like the JVM's
@HighAvailabilityTaskAssignor@:

  * a stateful task stays with its owner, or goes where the load is
    balanced, as long as that instance is /caught up/ (within
    @acceptable.recovery.lag@); otherwise it goes to a caught-up
    instance, and the instance that should own it gets a /warmup/
    replica: a standby that restores in the background. A probing
    rebalance after @probing.rebalance.interval.ms@ moves the task
    once the warmup has caught up. With nobody caught up the task goes
    where the balance says (it has to restore either way);
  * @num.standby.replicas@ standbys per stateful task, on other
    instances, preferring the ones with the most state;
  * stateless tasks are balanced, sticky where the balance allows.

Load is balanced in proportion to each instance's threads. A
user 'PA.TaskAssignor' (KIP-924, 'Kafka.Streams.Config.taskAssignor')
replaces the dealing; its result is validated and the built-in
assignment is used when it is invalid.

Each member's assignment carries a 'StreamsAssignment': its active
and standby tasks, every instance's tasks by host (for interactive-
query discovery), and whether to probe.
-}
module Kafka.Streams.Runtime.GroupAssignor (
  -- * Wire format
  StreamsSubscription (..),
  encodeSubscription,
  decodeSubscription,
  StreamsAssignment (..),
  HostTasks (..),
  encodeAssignment,
  decodeAssignment,
  runningActive,

  -- * Dealing tasks
  AssignConfig (..),
  ClientInfo (..),
  TaskSpec (..),
  ClientTasks (..),
  assignTasks,
  taskLag,

  -- * The group assignor
  LeaderContext (..),
  streamsAssignor,
  streamsProtocolName,
) where

import Control.Exception (SomeException, try)
import Control.Monad (replicateM, unless, when)
import Data.Binary.Get qualified as G
import Data.Binary.Put qualified as P
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Data.Word (Word8)
import Kafka.Client.Assignor (CustomAssignor (..), MemberAssignment (..), MemberSubscription (..))
import Kafka.Client.Consumer qualified as KC
import Kafka.Streams.Discovery (HostInfo (..))
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Processor.Assignment qualified as PA


----------------------------------------------------------------------
-- Wire format
----------------------------------------------------------------------

-- | The offset sum a member reports for a task it is running.
runningActive :: Int64
runningActive = -2


-- | What a member tells the leader when it joins.
data StreamsSubscription = StreamsSubscription
  { ssProcessId :: !Text
  -- ^ Stable across restarts (kept in the state directory)
  , ssHost :: !(Maybe HostInfo)
  -- ^ @application.server@
  , ssCapacity :: !Int
  -- ^ Stream threads
  , ssOffsetSums :: !(Map TaskId Int64)
  {- ^ Per task with local state, the summed changelog offsets the state
  covers, or 'runningActive'
  -}
  , ssStandbys :: !(Set TaskId)
  -- ^ The standbys it holds
  }
  deriving stock (Eq, Show)


-- | One instance's share, as every member learns it.
data HostTasks = HostTasks
  { htActive :: !(Set TaskId)
  , htStandby :: !(Set TaskId)
  }
  deriving stock (Eq, Show)


-- | What the leader tells a member.
data StreamsAssignment = StreamsAssignment
  { saActive :: !(Set TaskId)
  , saStandby :: !(Set TaskId)
  -- ^ Standbys and warmups
  , saHosts :: !(Map HostInfo HostTasks)
  -- ^ Every instance that advertises an @application.server@
  , saProbe :: !Bool
  -- ^ Warmups are restoring: rebalance again after the probing interval
  }
  deriving stock (Eq, Show)


wireVersion :: Word8
wireVersion = 1


putText :: Text -> P.Put
putText t = do
  let b = TE.encodeUtf8 t
  P.putInt32be (fromIntegral (BS.length b))
  P.putByteString b


getText :: G.Get Text
getText = do
  n <- G.getInt32be
  when (n < 0) (fail "negative string length")
  b <- G.getByteString (fromIntegral n)
  either (fail . show) pure (TE.decodeUtf8' b)


putTaskId :: TaskId -> P.Put
putTaskId (TaskId s p) = P.putInt32be (fromIntegral s) >> P.putInt32be p


getTaskId :: G.Get TaskId
getTaskId = TaskId . fromIntegral <$> G.getInt32be <*> G.getInt32be


putList :: (a -> P.Put) -> [a] -> P.Put
putList f xs = P.putInt32be (fromIntegral (length xs)) >> mapM_ f xs


getList :: G.Get a -> G.Get [a]
getList g = do
  n <- G.getInt32be
  when (n < 0) (fail "negative count")
  replicateM (fromIntegral n) g


putHost :: HostInfo -> P.Put
putHost h = putText h.host >> P.putInt32be (fromIntegral h.port)


getHost :: G.Get HostInfo
getHost = HostInfo <$> getText <*> (fromIntegral <$> G.getInt32be)


runGetStrict :: G.Get a -> ByteString -> Either String a
runGetStrict g bs = case G.runGetOrFail g (BL.fromStrict bs) of
  Left (_, _, e) -> Left e
  Right (rest, _, a)
    | BL.null rest -> Right a
    | otherwise -> Left "trailing bytes"


checkVersion :: G.Get ()
checkVersion = do
  v <- G.getWord8
  unless (v == wireVersion) (fail ("unsupported streams protocol version " <> show v))


encodeSubscription :: StreamsSubscription -> ByteString
encodeSubscription s = BL.toStrict . P.runPut $ do
  P.putWord8 wireVersion
  putText s.ssProcessId
  maybe (P.putWord8 0) (\h -> P.putWord8 1 >> putHost h) s.ssHost
  P.putInt32be (fromIntegral s.ssCapacity)
  putList (\(t, o) -> putTaskId t >> P.putInt64be o) (Map.toList s.ssOffsetSums)
  putList putTaskId (Set.toList s.ssStandbys)


decodeSubscription :: ByteString -> Either String StreamsSubscription
decodeSubscription = runGetStrict $ do
  checkVersion
  pid <- getText
  hasHost <- G.getWord8
  h <- if hasHost == 0 then pure Nothing else Just <$> getHost
  cap <- fromIntegral <$> G.getInt32be
  sums <- Map.fromList <$> getList ((,) <$> getTaskId <*> G.getInt64be)
  sbs <- Set.fromList <$> getList getTaskId
  pure (StreamsSubscription pid h cap sums sbs)


encodeAssignment :: StreamsAssignment -> ByteString
encodeAssignment a = BL.toStrict . P.runPut $ do
  P.putWord8 wireVersion
  putList putTaskId (Set.toList a.saActive)
  putList putTaskId (Set.toList a.saStandby)
  putList
    (\(h, ht) -> putHost h >> putList putTaskId (Set.toList ht.htActive) >> putList putTaskId (Set.toList ht.htStandby))
    (Map.toList a.saHosts)
  P.putWord8 (if a.saProbe then 1 else 0)


decodeAssignment :: ByteString -> Either String StreamsAssignment
decodeAssignment = runGetStrict $ do
  checkVersion
  act <- Set.fromList <$> getList getTaskId
  sb <- Set.fromList <$> getList getTaskId
  hosts <-
    Map.fromList
      <$> getList ((,) <$> getHost <*> (HostTasks <$> (Set.fromList <$> getList getTaskId) <*> (Set.fromList <$> getList getTaskId)))
  probe <- (/= 0) <$> G.getWord8
  pure (StreamsAssignment act sb hosts probe)


----------------------------------------------------------------------
-- Dealing tasks
----------------------------------------------------------------------

data AssignConfig = AssignConfig
  { acStandbys :: !Int
  , acAcceptableLag :: !Int64
  , acMaxWarmups :: !Int
  }
  deriving stock (Eq, Show)


data ClientInfo = ClientInfo
  { ciId :: !Text
  , ciCapacity :: !Int
  , ciOffsetSums :: !(Map TaskId Int64)
  }
  deriving stock (Eq, Show)


data TaskSpec = TaskSpec
  { tsStateful :: !Bool
  , tsEndSum :: !Int64
  -- ^ Summed end offsets of the task's changelog partitions
  }
  deriving stock (Eq, Show)


data ClientTasks = ClientTasks
  { ctActive :: !(Set TaskId)
  , ctStandby :: !(Set TaskId)
  -- ^ Standbys, including the warmups
  , ctWarmup :: !(Set TaskId)
  }
  deriving stock (Eq, Show)


noTasks :: ClientTasks
noTasks = ClientTasks Set.empty Set.empty Set.empty


{- | How far a client's state for a task is behind its changelogs: 0
for the running owner, the whole changelog without state.
-}
taskLag :: TaskSpec -> Maybe Int64 -> Int64
taskLag spec = \case
  _ | not (tsStateful spec) -> 0
  Just s | s == runningActive -> 0
  Just s -> max 0 (tsEndSum spec - s)
  Nothing -> tsEndSum spec


{- | Deal @tasks@ over @clients@ (see the module header). Every task is
active on exactly one client; a client never holds a standby of a
task it runs; ties go to the lower client id.
-}
assignTasks :: AssignConfig -> Map TaskId TaskSpec -> [ClientInfo] -> Map Text ClientTasks
assignTasks _ _ [] = Map.empty
assignTasks cfg tasks clients =
  let ids = map ciId clients
      capOf = Map.fromList (map (\c -> (ciId c, max 1 (ciCapacity c))) clients)
      cap c = Map.findWithDefault 1 c capOf
      totalCap = sum (Map.elems capOf)
      sumsOf = Map.fromList (map (\c -> (ciId c, ciOffsetSums c)) clients)
      sumFor c t = Map.lookup c sumsOf >>= Map.lookup t
      lagOf c t spec = taskLag spec (sumFor c t)
      quota n c = (n * cap c + totalCap - 1) `quot` totalCap
      -- the client where one more task weighs least
      lightest load = minimumOn (\c -> ((Map.findWithDefault 0 c load + 1) % cap c, c))
      prevOwner t = filter (\c -> sumFor c t == Just runningActive) ids

      (stateful, stateless) = Map.partition tsStateful tasks
      nStateful = Map.size stateful
      nAll = Map.size tasks

      -- actives for the stateful tasks, and the warmups they call for
      placeStateful (act, load, warm) (t, spec) =
        let target = case prevOwner t of
              (p : _) | Map.findWithDefault 0 p load < quota nStateful p -> p
              _ -> lightest load ids
            caught = filter (\c -> lagOf c t spec <= acAcceptableLag cfg) ids
            (owner, mWarm)
              | lagOf target t spec <= acAcceptableLag cfg = (target, Nothing)
              | null caught = (target, Nothing)
              | otherwise = case filter (`elem` caught) (prevOwner t) of
                  (p : _) -> (p, Just target)
                  [] -> (lightest load caught, Just target)
        in ( Map.insert t owner act
           , Map.insertWith (+) owner 1 load
           , maybe warm (\w -> (t, w) : warm) mWarm
           )
      (act1, loadS, warmWanted) = foldl' placeStateful (Map.empty, Map.empty, []) (Map.toList stateful)

      placeStateless (act, load) t =
        let owner = case prevOwner t of
              (p : _) | Map.findWithDefault 0 p load < quota nAll p -> p
              _ -> lightest load ids
        in (Map.insert t owner act, Map.insertWith (+) owner 1 load)
      (actives, _) = foldl' placeStateless (act1, loadS) (Map.keys stateless)

      warmups = take (max 0 (acMaxWarmups cfg)) (reverse warmWanted)

      -- standbys: the clients with the most state first, then the fewest standbys
      placeStandbys sb (t, spec) =
        let owner = Map.lookup t actives
            warm = map snd (filter ((== t) . fst) warmups)
            have = length warm
            pick acc _ =
              let taken = Map.findWithDefault Set.empty t acc
                  candidates = filter (\c -> Just c /= owner && Set.notMember c taken) ids
                  sbLoad = Map.fromListWith (+) (concatMap (\(_, cs) -> map (\c -> (c, 1 :: Int)) (Set.toList cs)) (Map.toList acc))
              in case candidates of
                   [] -> acc
                   _ ->
                     let c = minimumOn (\x -> (lagOf x t spec, (Map.findWithDefault 0 x sbLoad + 1) % cap x, x)) candidates
                     in Map.insertWith Set.union t (Set.singleton c) acc
            withWarm = Map.insertWith Set.union t (Set.fromList warm) sb
        in foldl' pick withWarm (replicate (max 0 (acStandbys cfg - have)) ())
      standbys = foldl' placeStandbys Map.empty (Map.toList stateful)

      base = Map.fromList (map (\c -> (c, noTasks)) ids)
      withActive = Map.foldrWithKey (\t c -> Map.adjust (\ct -> ct {ctActive = Set.insert t (ctActive ct)}) c) base actives
      withStandby =
        Map.foldrWithKey
          (\t cs m -> foldr (\c -> Map.adjust (\ct -> ct {ctStandby = Set.insert t (ctStandby ct)}) c) m (Set.toList cs))
          withActive
          standbys
  in foldr (\(t, c) -> Map.adjust (\ct -> ct {ctWarmup = Set.insert t (ctWarmup ct)}) c) withStandby warmups


minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)


----------------------------------------------------------------------
-- The group assignor
----------------------------------------------------------------------

streamsProtocolName :: Text
streamsProtocolName = "streams"


-- | What the leader needs to deal the application's tasks.
data LeaderContext = LeaderContext
  { lcTasks :: ![TaskId]
  , lcTaskPartitions :: !(TaskId -> [KC.TopicPartition])
  , lcTaskChangelogs :: !(Map TaskId ([KC.TopicPartition], [KC.TopicPartition]))
  -- ^ Per stateful task: its internal changelog partitions, and the source partitions its stores reuse
  , lcTaskStores :: !(TaskId -> Set Text)
  , lcEndOffsets :: !([KC.TopicPartition] -> IO (Either String (Map KC.TopicPartition Int64)))
  , lcCommitted :: !([KC.TopicPartition] -> IO (Either String (Map KC.TopicPartition Int64)))
  , lcConfig :: !AssignConfig
  , lcAssignmentConfigs :: !PA.AssignmentConfigs
  , lcUserAssignor :: !(Maybe PA.TaskAssignor)
  , lcLog :: !(String -> IO ())
  }


-- | The consumer-group assignor every instance of the application advertises.
streamsAssignor :: LeaderContext -> CustomAssignor
streamsAssignor lc =
  CustomAssignor
    { caName = streamsProtocolName
    , caAssign = \_counts members -> leaderAssign lc members
    }


leaderAssign :: LeaderContext -> [MemberSubscription] -> IO (Either String (Map Text MemberAssignment))
leaderAssign lc members =
  case traverse (\m -> (,) (msMemberId m) <$> decodeSubscription (msUserData m)) members of
    Left e -> pure (Left ("a member's streams subscription does not decode: " <> e))
    Right subs -> do
      specs <- taskSpecs lc (map snd subs)
      let clients = map (\(mid, s) -> ClientInfo mid (ssCapacity s) (ssOffsetSums s)) subs
          builtIn = assignTasks (lcConfig lc) specs clients
      dealt <- case lcUserAssignor lc of
        Nothing -> pure builtIn
        Just ua -> userAssign lc ua specs subs builtIn
      let probe = any (not . Set.null . ctWarmup) (Map.elems dealt)
          hosts =
            Map.fromList
              ( mapMaybe
                  ( \(mid, s) -> do
                      h <- ssHost s
                      let ct = Map.findWithDefault noTasks mid dealt
                      pure (h, HostTasks (ctActive ct) (ctStandby ct))
                  )
                  subs
              )
          forMember mid =
            let ct = Map.findWithDefault noTasks mid dealt
                parts = concatMap (lcTaskPartitions lc) (Set.toList (ctActive ct))
                byTopic = Map.toList (Map.fromListWith (<>) (map (\tp -> (tp.topic, [tp.partition])) parts))
            in MemberAssignment
                 { maPartitions = map (fmap (Set.toList . Set.fromList)) byTopic
                 , maUserData = encodeAssignment (StreamsAssignment (ctActive ct) (ctStandby ct) hosts probe)
                 }
      pure (Right (Map.fromList (map (\(mid, _) -> (mid, forMember mid)) subs)))


{- | Every task's spec. A changelog whose end can't be read counts the
most advanced member as caught up.
-}
taskSpecs :: LeaderContext -> [StreamsSubscription] -> IO (Map TaskId TaskSpec)
taskSpecs lc subs = do
  let cls = lcTaskChangelogs lc
      internal = concatMap fst (Map.elems cls)
      reused = concatMap snd (Map.elems cls)
  eEnds <- if null internal then pure (Right Map.empty) else lcEndOffsets lc internal
  eCommitted <- if null reused then pure (Right Map.empty) else lcCommitted lc reused
  either (\e -> lcLog lc ("assignor: changelog end offsets: " <> e)) (const (pure ())) eEnds
  either (\e -> lcLog lc ("assignor: committed offsets: " <> e)) (const (pure ())) eCommitted
  let known = either (const Nothing) Just
      bestReported t = maximum (0 : filter (>= 0) (mapMaybe (Map.lookup t . ssOffsetSums) subs))
      spec t = case Map.lookup t cls of
        Nothing -> TaskSpec False 0
        Just (ins, rs) ->
          let end = case (known eEnds, known eCommitted) of
                (Just ends, Just comm) ->
                  sum (map (\tp -> Map.findWithDefault 0 tp ends) ins) + sum (map (\tp -> Map.findWithDefault 0 tp comm) rs)
                _ -> bestReported t
          in TaskSpec True end
  pure (Map.fromList (map (\t -> (t, spec t)) (lcTasks lc)))


{- | Run the user's KIP-924 assignor; an invalid result (or one that
throws) falls back to the built-in assignment.
-}
userAssign
  :: LeaderContext
  -> PA.TaskAssignor
  -> Map TaskId TaskSpec
  -> [(Text, StreamsSubscription)]
  -> Map Text ClientTasks
  -> IO (Map Text ClientTasks)
userAssign lc ua specs subs builtIn = do
  let pidOf s = maybe (PA.ProcessId UUID.nil) PA.ProcessId (UUID.fromText (ssProcessId s))
      byPid = Map.fromList (map (\(mid, s) -> (pidOf s, mid)) subs)
      states =
        Map.fromList
          ( map
              ( \(_, s) ->
                  ( pidOf s
                  , PA.KafkaStreamsState
                      { PA.kssProcessId = pidOf s
                      , PA.kssNumProcessingThreads = ssCapacity s
                      , PA.kssClientTags = Map.empty
                      , PA.kssPreviousActiveTasks = Map.keysSet (Map.filter (== runningActive) (ssOffsetSums s))
                      , PA.kssPreviousStandbyTasks = ssStandbys s
                      , PA.kssRackId = Nothing
                      , PA.kssTaskLags =
                          Map.mapWithKey (\t spec -> taskLag spec (Map.lookup t (ssOffsetSums s))) (Map.filter tsStateful specs)
                      }
                  )
              )
              subs
          )
      info t spec =
        PA.TaskInfo
          { PA.tiTaskId = t
          , PA.tiTopicPartitions =
              Set.fromList
                ( map (\tp -> PA.TaskTopicPartition tp.topic tp.partition False) (lcTaskPartitions lc t)
                    <> map (\tp -> PA.TaskTopicPartition tp.topic tp.partition True) (maybe [] fst (Map.lookup t (lcTaskChangelogs lc)))
                )
          , PA.tiIsStateful = tsStateful spec
          , PA.tiStores = lcTaskStores lc t
          }
      app =
        PA.ApplicationState
          { PA.asAllTasks = Map.mapWithKey info specs
          , PA.asKafkaStreamsStates = states
          , PA.asAssignmentConfigs = lcAssignmentConfigs lc
          }
  r <- try (PA.taAssign ua app) :: IO (Either SomeException PA.TaskAssignment)
  case r of
    Left e -> do
      lcLog lc ("task assignor failed, using the built-in assignment: " <> show e)
      pure builtIn
    Right ta -> do
      let err = validate ta
      _ <- try (PA.taOnAssignmentComputed ua ta err) :: IO (Either SomeException ())
      if err /= PA.AssignmentErrorNone
        then do
          lcLog lc ("task assignor returned an invalid assignment (" <> show err <> "), using the built-in assignment")
          pure builtIn
        else
          pure $
            Map.fromList
              ( map
                  ( \(pid, ksa) ->
                      ( Map.findWithDefault "" pid byPid
                      , ClientTasks (PA.ksaActiveTasks ksa) (PA.ksaStandbyTasks ksa) Set.empty
                      )
                  )
                  (Map.toList (PA.taAssignments ta))
              )
  where
    validate ta =
      let asg = Map.toList (PA.taAssignments ta)
          pids = Set.fromList (map (\(_, s) -> maybe (PA.ProcessId UUID.nil) PA.ProcessId (UUID.fromText (ssProcessId s))) subs)
          actives = concatMap (Set.toList . PA.ksaActiveTasks . snd) asg
          allTasks = Map.keysSet specs
          everyTask = concatMap (\(_, k) -> Set.toList (PA.ksaActiveTasks k) <> Set.toList (PA.ksaStandbyTasks k)) asg
          -- the first that applies
          checks =
            [ (any (\(p, _) -> Set.notMember p pids) asg, PA.UnknownProcessId)
            , (Set.size pids /= length asg, PA.MissingProcessId)
            , (any (`Set.notMember` allTasks) everyTask, PA.UnknownTaskId)
            , (length actives /= Set.size (Set.fromList actives), PA.ActiveTaskAssignedMultipleTimes)
            , (Set.fromList actives /= allTasks, PA.MissingActiveTask)
            , (any (\(_, k) -> not (Set.null (Set.intersection (PA.ksaActiveTasks k) (PA.ksaStandbyTasks k)))) asg, PA.InvalidStandbyTask)
            ]
      in maybe PA.AssignmentErrorNone snd (find fst checks)

