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
  decodeSubscription (encodeSubscription sub) H.=== Right sub
  asg <-
    H.forAll $
      StreamsAssignment
        <$> genTasks
        <*> genTasks
        <*> (Map.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> genHost <*> (HostTasks <$> genTasks <*> genTasks)))
        <*> Gen.bool
  decodeAssignment (encodeAssignment asg) H.=== Right asg
