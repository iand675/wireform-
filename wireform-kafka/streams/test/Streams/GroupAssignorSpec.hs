{-# LANGUAGE OverloadedStrings #-}

{- | The streams group assignor's dealing ('assignTasks') and its wire
formats, as properties over random groups: who has state for which
task (none, some offsets, running it) and how far each changelog
runs.
-}
module Streams.GroupAssignorSpec (tests) where

import Control.Monad (forM, forM_, when)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Discovery (HostInfo (..))
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.GroupAssignor
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Group assignor" $ do
    it "every task is active exactly once" $ H.withTests 300 prop_active_once
    it "standbys: never next to the active, as many as asked (and the clients allow)" $ H.withTests 300 prop_standbys
    it "an active goes to a caught-up client whenever one exists" $ H.withTests 300 prop_caught_up
    it "warmups: on clients without the active, within max.warmup.replicas" $ H.withTests 300 prop_warmups
    it "without state anywhere, actives are balanced in proportion to threads" $ H.withTests 300 prop_balanced
    it "running owners keep their tasks when the balance allows" $ H.withTests 200 prop_sticky
    it "a lagging owner hands over to a caught-up client and warms up" unit_warmup_scenario
    it "subscription and assignment round-trip their wire format" $ H.withTests 200 prop_codecs
    it "min_traffic moves actives to their partitions' racks" unit_rack_local
    it "rack-aware: same counts, no cold moves, never more cross-rack reads" $ H.withTests 200 prop_rack_aware
    it "balance_subtopology spreads each subtopology's tasks" unit_balance_subtopology
    it "standbys go to clients with tag values the other replicas lack (KIP-708)" unit_standby_tags


data Group = Group
  { gConfig :: AssignConfig
  , gTasks :: Map TaskId TaskSpec
  , gClients :: [ClientInfo]
  }
  deriving stock (Show)


genGroup :: H.Gen Group
genGroup = do
  nSubs <- Gen.int (Range.linear 1 3)
  tasks <- fmap (Map.fromList . concat) . forM [0 .. nSubs - 1] $ \s -> do
    parts <- Gen.int (Range.linear 1 8)
    stateful <- Gen.bool
    forM [0 .. parts - 1] $ \p -> do
      end <- if stateful then Gen.int64 (Range.linear 0 5_000) else pure 0
      pure (TaskId s (fromIntegral p), TaskSpec stateful end)
  nClients <- Gen.int (Range.linear 1 5)
  owners <- forM (Map.keys tasks) $ \t -> (,) t <$> Gen.maybe (Gen.int (Range.linear 0 (nClients - 1)))
  clients <- forM [0 .. nClients - 1] $ \c -> do
    cap <- Gen.int (Range.linear 1 3)
    sums <- fmap (Map.fromList . mapMaybe id) . forM (Map.toList tasks) $ \(t, spec) ->
      if lookup t owners == Just (Just c)
        then pure (Just (t, runningActive))
        else
          if tsStateful spec
            then Gen.frequency [(3, pure Nothing), (2, Just . (,) t <$> Gen.int64 (Range.linear 0 (tsEndSum spec)))]
            else pure Nothing
    pure (ClientInfo ("c" <> T.pack (show c)) cap sums)
  cfg <- AssignConfig <$> Gen.int (Range.linear 0 2) <*> Gen.int64 (Range.linear 0 2_000) <*> Gen.int (Range.linear 0 3)
  pure (Group cfg tasks clients)


deal :: Group -> Map Text ClientTasks
deal g = assignTasks (gConfig g) (gTasks g) (gClients g)


lagOn :: Group -> Text -> TaskId -> Int64
lagOn g c t =
  let spec = gTasks g Map.! t
      sums = maybe Map.empty ciOffsetSums (lookupClient g c)
  in taskLag spec (Map.lookup t sums)


lookupClient :: Group -> Text -> Maybe ClientInfo
lookupClient g c = case filter ((== c) . ciId) (gClients g) of
  (ci : _) -> Just ci
  [] -> Nothing


activeOf :: Map Text ClientTasks -> TaskId -> [Text]
activeOf d t = Map.keys (Map.filter (Set.member t . ctActive) d)


prop_active_once :: H.Property
prop_active_once = H.property $ do
  g <- H.forAll genGroup
  let d = deal g
  Map.keysSet d H.=== Set.fromList (map ciId (gClients g))
  forM_ (Map.keys (gTasks g)) $ \t -> length (activeOf d t) H.=== 1
  -- and nothing that isn't a task
  Set.unions (map ctActive (Map.elems d)) H.=== Map.keysSet (gTasks g)


prop_standbys :: H.Property
prop_standbys = H.property $ do
  g <- H.forAll genGroup
  let d = deal g
      nClients = length (gClients g)
  forM_ (Map.toList (gTasks g)) $ \(t, spec) -> do
    let holders = Map.keys (Map.filter (Set.member t . ctStandby) d)
    forM_ (activeOf d t) $ \a -> H.assert (a `notElem` holders)
    if tsStateful spec
      then length holders H.=== min (max (acStandbys (gConfig g)) (length (warmupHolders d t))) (nClients - 1)
      else holders H.=== []
  where
    warmupHolders d t = Map.keys (Map.filter (Set.member t . ctWarmup) d)


prop_caught_up :: H.Property
prop_caught_up = H.property $ do
  g <- H.forAll genGroup
  let d = deal g
      ok c t = lagOn g c t <= acAcceptableLag (gConfig g)
  forM_ (Map.keys (gTasks g)) $ \t -> do
    let anyCaught = any (\c -> ok (ciId c) t) (gClients g)
    forM_ (activeOf d t) $ \a -> when anyCaught (H.assert (ok a t))


prop_warmups :: H.Property
prop_warmups = H.property $ do
  g <- H.forAll genGroup
  let d = deal g
      warm = concatMap (\(c, ct) -> map (\t -> (c, t)) (Set.toList (ctWarmup ct))) (Map.toList d)
  H.assert (length warm <= max 0 (acMaxWarmups (gConfig g)))
  forM_ warm $ \(c, t) -> do
    H.assert (c `notElem` activeOf d t)
    -- a warmup is also a standby, and only for a stateful task
    H.assert (Set.member t (ctStandby (d Map.! c)))
    H.assert (tsStateful (gTasks g Map.! t))


prop_balanced :: H.Property
prop_balanced = H.property $ do
  g0 <- H.forAll genGroup
  let g = g0 {gClients = map (\c -> c {ciOffsetSums = Map.empty}) (gClients g0)}
      d = deal g
      total = Map.size (gTasks g)
      totalCap = sum (map (max 1 . ciCapacity) (gClients g))
  forM_ (gClients g) $ \c -> do
    let n = Set.size (ctActive (d Map.! ciId c))
        quota = (total * max 1 (ciCapacity c) + totalCap - 1) `quot` totalCap
    H.annotateShow (ciId c, n, quota)
    -- the stateful and stateless rounds each stay within their share
    H.assert (n <= quota + 1)


prop_sticky :: H.Property
prop_sticky = H.property $ do
  g0 <- H.forAll genGroup
  -- stateless tasks, equal threads, every task owned by someone and
  -- the owners already balanced: nothing moves
  let clients = gClients g0
      nC = length clients
      tasks = Map.map (const (TaskSpec False 0)) (gTasks g0)
      owned = zip (Map.keys tasks) (cycle [0 .. nC - 1])
      clients' =
        zipWith
          ( \i c ->
              c
                { ciCapacity = 1
                , ciOffsetSums = Map.fromList (map (\(t, _) -> (t, runningActive)) (filter ((== i) . snd) owned))
                }
          )
          [0 :: Int ..]
          clients
      g = g0 {gTasks = tasks, gClients = clients'}
      d = deal g
  forM_ owned $ \(t, i) -> activeOf d t H.=== [ciId (clients' !! i)]


unit_warmup_scenario :: IO ()
unit_warmup_scenario = do
  -- c0 runs both tasks; c1 has nothing: the balance wants one task on
  -- c1, but c1 is 1000 records behind, so c0 keeps it and c1 warms up
  let t0 = TaskId 0 0
      t1 = TaskId 0 1
      tasks = Map.fromList [(t0, TaskSpec True 1000), (t1, TaskSpec True 1000)]
      c0 = ClientInfo "c0" 1 (Map.fromList [(t0, runningActive), (t1, runningActive)])
      c1 = ClientInfo "c1" 1 Map.empty
      cfg = AssignConfig {acStandbys = 0, acAcceptableLag = 10, acMaxWarmups = 2}
      d = assignTasks cfg tasks [c0, c1]
  ctActive (d Map.! "c0") `shouldBe` Set.fromList [t0, t1]
  Set.size (ctWarmup (d Map.! "c1")) `shouldBe` 1
  -- the warmup caught up: the next assignment moves that task
  let warmed = Set.findMin (ctWarmup (d Map.! "c1"))
      c1' = ClientInfo "c1" 1 (Map.singleton warmed 995)
      d' = assignTasks cfg tasks [c0, c1']
  ctActive (d' Map.! "c1") `shouldBe` Set.singleton warmed
  ctWarmup (d' Map.! "c1") `shouldBe` Set.empty
  -- with nobody caught up the balance decides
  let d'' = assignTasks cfg tasks [ClientInfo "c0" 1 Map.empty, ClientInfo "c1" 1 Map.empty]
  map (Set.size . ctActive) (Map.elems d'') `shouldBe` [1, 1]


prop_codecs :: H.Property
prop_codecs = H.property $ do
  let genTask = TaskId <$> Gen.int (Range.linear 0 50) <*> Gen.int32 (Range.linear 0 500)
      genHost = HostInfo <$> Gen.text (Range.linear 0 20) Gen.unicode <*> Gen.int (Range.linear 0 65535)
      genTasks = Set.fromList <$> Gen.list (Range.linear 0 20) genTask
  sub <-
    H.forAll $
      StreamsSubscription
        <$> Gen.text (Range.linear 0 40) Gen.alphaNum
        <*> Gen.maybe genHost
        <*> Gen.int (Range.linear 1 64)
        <*> (Map.fromList <$> Gen.list (Range.linear 0 20) ((,) <$> genTask <*> Gen.int64 (Range.linear (-2) 1_000_000)))
        <*> genTasks
        <*> Gen.maybe (Gen.text (Range.linear 0 10) Gen.unicode)
        <*> (Map.fromList <$> Gen.list (Range.linear 0 4) ((,) <$> Gen.text (Range.linear 0 8) Gen.alphaNum <*> Gen.text (Range.linear 0 8) Gen.unicode))
  decodeSubscription (encodeSubscription sub) H.=== Right sub
  asg <-
    H.forAll $
      StreamsAssignment
        <$> genTasks
        <*> genTasks
        <*> (Map.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> genHost <*> (HostTasks <$> genTasks <*> genTasks)))
        <*> Gen.bool
  decodeAssignment (encodeAssignment asg) H.=== Right asg


-- | The racks of a two-rack cluster, per task partition.
rackA, rackB :: Set.Set Text
rackA = Set.singleton "a"
rackB = Set.singleton "b"


unit_rack_local :: IO ()
unit_rack_local = do
  let ts = map (TaskId 0) [0 .. 3]
      tasks = Map.fromList (map (\t -> (t, TaskSpec False 0)) ts)
      -- tasks 0 and 1 read partitions on rack b, 2 and 3 on rack a
      racks = Map.fromList (zip ts [[rackB], [rackB], [rackA], [rackA]])
      clients = [ClientInfo "c0" 1 Map.empty, ClientInfo "c1" 1 Map.empty]
      cfg = AssignConfig 0 0 0
      rp = noRackPlacement {rpStrategy = RackMinTraffic, rpClientRacks = Map.fromList [("c0", "a"), ("c1", "b")], rpTaskRacks = racks}
      d = assignTasksWith rp cfg tasks clients
  Map.map ctActive d `shouldBe` Map.fromList [("c0", Set.fromList (drop 2 ts)), ("c1", Set.fromList (take 2 ts))]
  -- without racks the plain deal is kept
  assignTasksWith rp {rpClientRacks = Map.singleton "c0" "a"} cfg tasks clients `shouldBe` assignTasks cfg tasks clients


crossTotal :: RackPlacement -> Map Text ClientTasks -> Int
crossTotal rp d =
  sum
    ( concatMap
        ( \(c, ct) ->
            map (\t -> crossRackCost (Map.findWithDefault [] t (rpTaskRacks rp)) (Map.findWithDefault "" c (rpClientRacks rp))) (Set.toList (ctActive ct))
        )
        (Map.toList d)
    )


prop_rack_aware :: H.Property
prop_rack_aware = H.property $ do
  g <- H.forAll genGroup
  strategy <- H.forAll (Gen.element [RackMinTraffic, RackBalanceSubtopology])
  let racks = ["a", "b", "c"]
  clientRacks <- H.forAll (Map.fromList <$> traverse (\c -> (,) (ciId c) <$> Gen.element racks) (gClients g))
  taskRacks <-
    H.forAll $
      Map.fromList
        <$> traverse
          (\t -> (,) t <$> Gen.list (Range.linear 1 3) (Set.fromList <$> Gen.subsequence racks))
          (Map.keys (gTasks g))
  let rp = noRackPlacement {rpStrategy = strategy, rpClientRacks = clientRacks, rpTaskRacks = taskRacks}
      base = deal g
      d = assignTasksWith rp (gConfig g) (gTasks g) (gClients g)
  -- every task active once, on as many tasks per client as the plain deal
  Set.unions (map ctActive (Map.elems d)) H.=== Map.keysSet (gTasks g)
  Map.map (Set.size . ctActive) d H.=== Map.map (Set.size . ctActive) base
  forM_ (Map.keys (gTasks g)) $ \t -> length (activeOf d t) H.=== 1
  -- a task only moves to a client that need not restore it
  forM_ (Map.toList (gTasks g)) $ \(t, spec) -> do
    let [now] = activeOf d t
        [was] = activeOf base t
    when (now /= was && tsStateful spec) $ H.assert (lagOn g now t <= acAcceptableLag (gConfig g))
  -- min_traffic never reads more across racks than the plain deal
  when (strategy == RackMinTraffic) $ H.assert (crossTotal rp d <= crossTotal rp base)
  -- standbys never sit next to their active
  forM_ (Map.keys (gTasks g)) $ \t ->
    forM_ (activeOf d t) $ \a -> H.assert (Set.notMember t (ctStandby (d Map.! a)))


unit_balance_subtopology :: IO ()
unit_balance_subtopology = do
  -- subtopology 0's two tasks both read rack a; balance puts one on each client
  let ts0 = map (TaskId 0) [0, 1]
      ts1 = map (TaskId 1) [0, 1]
      tasks = Map.fromList (map (\t -> (t, TaskSpec False 0)) (ts0 <> ts1))
      racks = Map.fromList (zip (ts0 <> ts1) [[rackA], [rackA], [rackB], [rackB]])
      clients = [ClientInfo "c0" 1 Map.empty, ClientInfo "c1" 1 Map.empty]
      cfg = AssignConfig 0 0 0
      rp s = noRackPlacement {rpStrategy = s, rpClientRacks = Map.fromList [("c0", "a"), ("c1", "b")], rpTaskRacks = racks}
      local = assignTasksWith (rp RackMinTraffic) cfg tasks clients
      spread = assignTasksWith (rp RackBalanceSubtopology) cfg tasks clients
  Map.map ctActive local `shouldBe` Map.fromList [("c0", Set.fromList ts0), ("c1", Set.fromList ts1)]
  forM_ (Map.elems spread) $ \ct -> do
    Set.size (Set.filter (\(TaskId s _) -> s == 0) (ctActive ct)) `shouldBe` 1
    Set.size (Set.filter (\(TaskId s _) -> s == 1) (ctActive ct)) `shouldBe` 1


unit_standby_tags :: IO ()
unit_standby_tags = do
  let t = TaskId 0 0
      tasks = Map.singleton t (TaskSpec True 100)
      -- c0 runs it; c1 has nearly all of its state but shares c0's zone
      clients =
        [ ClientInfo "c0" 1 (Map.singleton t runningActive)
        , ClientInfo "c1" 1 (Map.singleton t 99)
        , ClientInfo "c2" 1 Map.empty
        ]
      cfg = AssignConfig 1 0 0
      tags = Map.fromList [("c0", Map.singleton "zone" "z1"), ("c1", Map.singleton "zone" "z1"), ("c2", Map.singleton "zone" "z2")]
      withTags = assignTasksWith noRackPlacement {rpTagKeys = ["zone"], rpClientTags = tags} cfg tasks clients
      without = assignTasks cfg tasks clients
  Map.keys (Map.filter (Set.member t . ctStandby) withTags) `shouldBe` ["c2"]
  Map.keys (Map.filter (Set.member t . ctStandby) without) `shouldBe` ["c1"]
