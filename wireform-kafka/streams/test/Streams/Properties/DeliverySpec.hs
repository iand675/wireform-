{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The delivery guarantees, as properties of the real runtime on the
in-memory broker: random batches over random partitions, with random
faults between them (failed sends, failed transaction commits, failed
offset commits, an instance leaving and coming back, one joining).

  * at least once: every input reaches the output, and nothing else
    does (duplicates allowed);
  * exactly once: a @read_committed@ consumer sees every input exactly
    once, and a count over the same input is exact.
-}
module Streams.Properties.DeliverySpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Int (Int64)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import Streams.RuntimeHarness
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Delivery under faults (runtime)" $ do
    it "at least once: no record lost, none made up" $ H.withTests 25 (prop_delivery AtLeastOnceP)
    it "exactly once: every record once, counts exact" $ H.withTests 25 (prop_delivery ExactlyOnceV2)


data Fault
  = NoFault
  | FailSends !Int
  | FailTxnCommits !Int
  | FailOffsetCommits !Int
  | Bounce
  -- ^ An instance leaves and a new one starts on its state directory
  | Join
  -- ^ Another instance joins
  deriving stock (Show)


data Workload = Workload
  { wPartitions :: !Int
  , wSteps :: ![([Text], Fault)]
  -- ^ A batch of values (keyed by themselves), then a fault
  }
  deriving stock (Show)


genWorkload :: ProcessingGuarantee -> H.Gen Workload
genWorkload g = do
  parts <- Gen.int (Range.linear 1 4)
  nSteps <- Gen.int (Range.linear 1 4)
  steps <- traverse (step parts) [1 .. nSteps]
  pure (Workload parts steps)
  where
    step _ i = do
      n <- Gen.int (Range.linear 1 15)
      -- a few repeated keys, so counts are more than 1
      let vals = map (\j -> "k" <> T.pack (show (j `mod` 7)) <> "-" <> T.pack (show i) <> "-" <> T.pack (show j)) [1 .. n]
      f <-
        Gen.frequency
          ( [ (3, pure NoFault)
            , (2, FailSends <$> Gen.int (Range.linear 1 2))
            , (2, FailOffsetCommits <$> Gen.int (Range.linear 1 2))
            , (2, pure Bounce)
            , (1, pure Join)
            ]
              <> (if g == ExactlyOnceV2 then [(3, FailTxnCommits <$> Gen.int (Range.linear 1 2))] else [])
          )
      pure (vals, f)


keyOf :: Text -> Text
keyOf = T.takeWhile (/= '-')


-- | @in -> out@ unchanged, and a count per key to @counts@.
topology :: IO TopologyValid
topology = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  rekeyed <- selectKey (\r -> keyOf (recordValue r)) s
  counts <- countStream (materializedAs (storeName "per-key")) (groupByKey (grouped textSerde textSerde) rekeyed)
  out <- toKStreamFromKTable counts
  toTopic (topicName "counts") (produced textSerde int64Serde) out
  buildTopology b >>= either (error . show) pure . validateTopology


prop_delivery :: ProcessingGuarantee -> H.Property
prop_delivery g = H.property $ do
  w <- H.forAll (genWorkload g)
  (inputs, outputs, counts) <- liftIO (runWorkload g w)
  let expectedCounts = Map.fromListWith (+) (map (\v -> (keyOf v, 1 :: Int64)) inputs)
  H.annotateShow (length inputs, length outputs)
  -- nothing made up, nothing lost
  H.assert (all (`Set.member` Set.fromList inputs) outputs)
  Set.fromList outputs H.=== Set.fromList inputs
  case g of
    ExactlyOnceV2 -> do
      sort outputs H.=== sort inputs
      counts H.=== expectedCounts
    AtLeastOnceP ->
      -- at least as many as were sent, per key
      H.assert (and (Map.intersectionWith (<=) expectedCounts counts) && Map.keysSet counts == Map.keysSet expectedCounts)


runWorkload :: ProcessingGuarantee -> Workload -> IO ([Text], [Text], Map.Map Text Int64)
runWorkload g w = withStateDirs 3 $ \(d0 : moreDirs) -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" (wPartitions w)
  topo <- topology
  let cfg d = (testConfig "dlv" d) {processingGuarantee = g}
  first <- startOn broker (cfg d0) topo
  live <- newIORef [(d0, first)]
  spare <- newIORef moreDirs
  sent <- newIORef []
  let running = map snd <$> readIORef live
      settle = running >>= \ks -> eventually ks "assigned" (and <$> mapM (fmap (not . null) . ownedPartitions) (take 1 ks))
  ( do
      settle
      forM_ (wSteps w) $ \(vals, fault) -> do
        forM_ vals $ \v -> mockProduce broker "in" (Just (textBytes (keyOf v))) (Just (textBytes v)) 0
        modifyIORef' sent (<> vals)
        case fault of
          NoFault -> pure ()
          FailSends n -> mockFailSends broker "out" n
          FailTxnCommits n -> mockFailTransactionCommits broker n
          FailOffsetCommits n -> mockFailOffsetCommits broker n
          Bounce -> do
            (d, ks) : rest <- readIORef live
            closeKafkaStreams ks >> awaitState ks StreamsClosed
            ks' <- startOn broker (cfg d) topo
            writeIORef live (rest <> [(d, ks')])
          Join ->
            readIORef spare >>= \case
              d : more -> do
                ks <- startOn broker (cfg d) topo
                writeIORef spare more
                modifyIORef' live (<> [(d, ks)])
              [] -> pure ()
      inputs <- readIORef sent
      let want = Set.fromList inputs
          outputs = mapMaybe (fmap decodeText . (.drValue)) <$> visible broker "out"
      ks <- running
      eventually ks "every input delivered" (Set.isSubsetOf want . Set.fromList <$> outputs)
      let latest = Map.mapKeys decodeText . Map.mapMaybe (fmap decodeI64) . latestBy <$> visible broker "counts"
          expected = Map.fromListWith (+) (map (\v -> (keyOf v, 1 :: Int64)) inputs)
      eventually ks "every key counted" (and . Map.intersectionWith (<=) expected <$> latest)
      -- let anything still in flight land before reading the result
      mapM_ (\k -> awaitTicks k 20) ks
      (,,) inputs <$> outputs <*> latest
    )
    `finally` (readIORef live >>= closeAll . map snd)
  where
    decodeText = either (error . T.unpack) id . deserialize textSerde
