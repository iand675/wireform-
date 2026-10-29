{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.Runner
Description : Run a demo against the in-process driver or a real Kafka broker

The DSL example demos in this executable used to be hard-wired to
the in-process 'Kafka.Streams.Driver.TopologyTestDriver': they
build a topology, instantiate the test driver, push records in
with 'pipeInput', drain results with 'readOutput'. That's great
for fast deterministic feedback but doesn't prove the same
topology runs end-to-end against a real broker.

This module introduces a tiny driver adapter that wraps either
the test driver ('InMemory' mode) or a live 'KafkaStreams'
instance against a broker ('Broker' mode), so a demo can be
written once and run in either mode. Activate broker mode by
passing @--broker host:port@ or setting the
@WIREFORM_KAFKA_BROKER@ env var.

The adapter only handles the lowest-common-denominator subset
of the test-driver API:

  * Send a record to an input topic (key + value + optional
    timestamp / partition — see caveats below).
  * Drain records from an output topic until quiescent.
  * Wait for stream-time progress (test driver:
    'advanceDriverStreamTime'; broker: 'awaitTicks').
  * Read a key-value store via the 'ReadOnlyKeyValueStore'
    shape exposed by 'queryKVStore' / 'queryEngineStore'.

The broker-mode 'ddSend' implementation now routes through
'Kafka.Client.Producer.sendRecord', so explicit per-record
timestamps and partitions are honoured end-to-end (the wire
batch carries @timestamp@ as its base when the record opens
a fresh batch, or as a signed delta otherwise). Windowed
demos can therefore in principle run against a broker, but
they additionally rely on 'advanceDriverStreamTime' to push
stream time forward with no input record — which has no
direct broker analogue. They stay flagged 'InMemoryOnly' in
'Main.hs' until each demo is updated to push a real sentinel
record on the broker path instead.
-}
module Kafka.Streams.Examples.Runner (
  -- * Mode
  RunMode (..),
  brokerEndpoint,
  parseRunMode,
  runModeFromEnv,

  -- * Adapter
  DemoTopic (..),
  DemoDriver (..),
  withDemoDriver,

  -- * Convenience for demos that are in-memory-only
  brokerOnlyWarning,
  runInMemoryWith,
) where

import Control.Exception (finally)
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.AdminClient qualified as Adm
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP
import Kafka.Streams.Imperative
import Kafka.Streams.InteractiveQueries qualified as IQ
import Kafka.Streams.Runtime (
  KafkaStreams,
  StreamsStatus (..),
  awaitState,
  awaitTicks,
  closeKafkaStreams,
  newKafkaStreams,
  startKafkaStreams,
 )
import System.Environment qualified as Env
import System.IO (hPutStrLn, stderr)


----------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------

{- | Where to run a demo. @Broker host:port@ uses the real client;
'InMemory' uses 'TopologyTestDriver'.
-}
data RunMode
  = InMemory
  | Broker !Text
  deriving (Eq, Show)


brokerEndpoint :: RunMode -> Maybe Text
brokerEndpoint = \case
  InMemory -> Nothing
  Broker b -> Just b


{- | Parse argv. Returns @(mode, rest)@. Recognises a single
@--broker host:port@ flag (anywhere in argv); everything else
is left for the caller. Falls back to 'runModeFromEnv' when no
flag is given.
-}
parseRunMode :: [String] -> IO (RunMode, [String])
parseRunMode argv = case extract argv of
  (Just b, rest) -> pure (Broker (T.pack b), rest)
  (Nothing, rest) -> do
    mEnv <- runModeFromEnv
    case mEnv of
      Just b -> pure (Broker b, rest)
      Nothing -> pure (InMemory, rest)
  where
    extract :: [String] -> (Maybe String, [String])
    extract = go []
      where
        go acc [] = (Nothing, reverse acc)
        go acc ("--broker" : b : xs) = (Just b, reverse acc ++ xs)
        go acc (x : xs) = go (x : acc) xs


{- | @WIREFORM_KAFKA_BROKER@ env var, matching the convention used
by the integration test suites under @test-integration/@.
-}
runModeFromEnv :: IO (Maybe Text)
runModeFromEnv = do
  m <- Env.lookupEnv "WIREFORM_KAFKA_BROKER"
  pure $ case m of
    Just v | not (null v) -> Just (T.pack v)
    _ -> Nothing


----------------------------------------------------------------------
-- Adapter surface
----------------------------------------------------------------------

data DemoTopic = DemoTopic
  { dtName :: !TopicName
  , dtPartitions :: !Int
  }
  deriving (Eq, Show)


data DemoDriver = DemoDriver
  { ddSend
      :: TopicName
      -> Maybe ByteString
      -> ByteString
      -> Timestamp
      -> Int32
      -> IO ()
  {- ^ Send a record to an input topic. In broker mode the
  timestamp / partition arguments are now honoured by
  routing through 'Kafka.Client.Producer.sendRecord' (the
  producer translates the absolute timestamp to the
  'recordTimestampDelta' the batch accumulator expects).
  -}
  , ddRead :: TopicName -> IO [CollectedRecord]
  {- ^ Drain records currently available on an output topic.
  Block briefly to give the topology time to flush; safe to
  call multiple times (each call returns only the records
  that arrived since the previous call).
  -}
  , ddAdvance :: Timestamp -> IO ()
  {- ^ In-memory: 'advanceDriverStreamTime'. Broker: 'awaitTicks'
  (best-effort — the broker has no notion of stream time we
  can directly poke).
  -}
  , ddGetKV
      :: forall k v
       . StoreName
      -> IO (Maybe (IQ.ReadOnlyKeyValueStore k v))
  {- ^ Read-only handle onto a state store. In-memory:
  'Kafka.Streams.InteractiveQueries.queryEngineStore'. Broker:
  'Kafka.Streams.InteractiveQueries.queryKVStore'.
  -}
  , ddIsBroker :: !Bool
  }


{- | Run a demo body with a driver adapter wired to @mode@. The
caller declares the topics it will read from / write to so we
can ensure they exist in broker mode and pre-subscribe the
consumer.

Currently the in-memory driver auto-creates topics on demand
and ignores the partition count in 'DemoTopic'; the broker
driver actually consults it.
-}
withDemoDriver
  :: RunMode
  -> Text
  -- ^ application id
  -> IO Topology
  -- ^ topology builder
  -> [DemoTopic]
  -- ^ input topics (consumed by topology)
  -> [DemoTopic]
  -- ^ output topics (sinks the demo will drain)
  -> (DemoDriver -> IO ())
  -> IO ()
withDemoDriver mode appId mkTopo inTopics outTopics action = case mode of
  InMemory -> withInMemoryDriver mkTopo appId action
  Broker bs -> withBrokerDriver bs mkTopo appId inTopics outTopics action


----------------------------------------------------------------------
-- In-memory adapter
----------------------------------------------------------------------

withInMemoryDriver
  :: IO Topology
  -> Text
  -> (DemoDriver -> IO ())
  -> IO ()
withInMemoryDriver mkTopo appId action = do
  topo <- mkTopo
  driver <- newDriver topo appId
  let dd =
        DemoDriver
          { ddSend = \topic key val ts part ->
              pipeInput driver topic key val ts (fromIntegral part)
          , ddRead = readOutput driver
          , ddAdvance = advanceDriverStreamTime driver
          , ddGetKV = \sn ->
              IQ.queryEngineStore (driverEngine driver) sn
          , ddIsBroker = False
          }
  action dd `finally` closeDriver driver


----------------------------------------------------------------------
-- Broker adapter
----------------------------------------------------------------------

withBrokerDriver
  :: Text
  -> IO Topology
  -> Text
  -> [DemoTopic]
  -> [DemoTopic]
  -> (DemoDriver -> IO ())
  -> IO ()
withBrokerDriver brokers mkTopo appId inTopics outTopics action = do
  -- 1. Ensure all topics exist.
  ensureAllTopics brokers (inTopics ++ outTopics)

  -- 2. Build + validate topology.
  topo <- mkTopo
  validated <- case validateTopology topo of
    Left err -> fail ("withDemoDriver(broker): topology invalid: " <> show err)
    Right v -> pure v

  -- 3. Streams instance.
  let cfg =
        defaultStreamsConfig
          { applicationId = appId
          , bootstrapServers = [brokers]
          , clientId = appId <> "-client"
          , pollMs = 100
          }
  ks <- newKafkaStreams cfg validated

  -- 4. Producer + consumer for demo IO.
  prodResult <- KP.createProducer [brokers] KP.defaultProducerConfig
  prod <- case prodResult of
    Left e -> fail ("createProducer: " <> e)
    Right p -> pure p

  let consGroup = appId <> "-demo-consumer"
  consResult <- KC.createConsumer [brokers] consGroup KC.defaultConsumerConfig
  cons <- case consResult of
    Left e -> KP.closeProducer prod >> fail ("createConsumer: " <> e)
    Right c -> pure c

  -- Subscribe to output topics and seek to current end so we only
  -- see records this demo produces.
  let subscriptions =
        [ KC.TopicPartition (unTopicName (dtName dt)) (fromIntegral p)
        | dt <- outTopics
        , p <- [0 .. dtPartitions dt - 1]
        ]
  _ <- KC.assign cons subscriptions
  _ <- KC.seekToEnd cons subscriptions

  -- Per-topic buffer of records observed since last drain.
  buf <- newIORef (Map.empty :: Map.Map TopicName [CollectedRecord])

  -- 5. Start streams; wait for it to come up.
  startKafkaStreams ks
  awaitState ks StreamsRunning

  let dd =
        DemoDriver
          { ddSend = \topic key val (Timestamp ts) part -> do
              let pr =
                    KP.ProducerRecord
                      { KP.topic = unTopicName topic
                      , KP.key = key
                      , KP.value = Just val
                      , KP.headers = []
                      , KP.partition = Just part
                      , KP.timestamp = Just ts
                      }
              r <- KP.sendRecord prod pr
              case r of
                Left e -> fail ("sendRecord: " <> e)
                Right _ -> pure ()
          , ddRead = \topic -> do
              -- Drain all records currently available for this
              -- topic, polling until we see one empty cycle. Cap
              -- the wall-clock spend so a misconfigured demo can't
              -- hang the executable.
              drainTopic cons buf topic 3
          , ddAdvance = \_ts -> do
              -- Best-effort: give the runtime room to consume +
              -- process anything we just produced. Wait for 5
              -- engine ticks to elapse or 2 seconds, whichever
              -- comes first.
              _ <- awaitTicks ks 5
              pure ()
          , ddGetKV = \sn -> IQ.queryKVStore ks sn
          , ddIsBroker = True
          }

  let cleanup = do
        KP.flushProducer prod
        KP.closeProducer prod
        KC.closeConsumer cons
        closeKafkaStreams ks

  action dd `finally` cleanup


ensureAllTopics :: Text -> [DemoTopic] -> IO ()
ensureAllTopics brokers ts = do
  r <- Adm.createAdminClient [brokers] Adm.defaultAdminClientConfig
  case r of
    Left e -> fail ("createAdminClient: " <> e)
    Right adm -> do
      forM_ ts $ \dt -> do
        let nt =
              Adm.NewTopic
                { Adm.ntName = unTopicName (dt.dtName)
                , Adm.ntNumPartitions = fromIntegral (dt.dtPartitions)
                , Adm.ntReplicationFactor = 1
                , Adm.ntConfigs = []
                }
        _ <- Adm.ensureTopic adm nt
        pure ()
      Adm.closeAdminClient adm


{- | Drain records currently available for @topic@: keep polling
until @maxEmptyCycles@ consecutive polls come back empty, then
return whatever we accumulated for @topic@. Other topics'
records stay buffered for the next 'ddRead' that asks for
them.
-}
drainTopic
  :: KC.Consumer
  -> IORef (Map.Map TopicName [CollectedRecord])
  -> TopicName
  -> Int -- consecutive empty polls before giving up
  -> IO [CollectedRecord]
drainTopic cons buf topic maxEmptyCycles = do
  let pollOne = do
        r <- KC.poll cons 200
        case r of
          Left _ -> pure []
          Right rs -> pure rs
      loop !emptyStreak
        | emptyStreak >= maxEmptyCycles = pure ()
        | otherwise = do
            rs <- pollOne
            if null rs
              then loop (emptyStreak + 1)
              else do
                stash rs
                loop 0
      stash rs =
        atomicModifyIORef' buf $ \m ->
          (List.foldl' insertOne m rs, ())
      insertOne m cr =
        let tname = topicName (crTopicText cr)
            existing = Map.findWithDefault [] tname m
        in Map.insert tname (existing ++ [crFromConsumer cr]) m
  loop 0
  taken <- atomicModifyIORef' buf $ \m ->
    let bs = Map.findWithDefault [] topic m
    in (Map.insert topic [] m, bs)
  pure taken


{- | Convert a 'KC.ConsumerRecord' into the 'CollectedRecord'
shape the demos already use for their pretty-printers.
-}
crFromConsumer :: KC.ConsumerRecord -> CollectedRecord
crFromConsumer cr =
  CollectedRecord
    { crTopic = topicName cr.topic
    , crKey = cr.key
    , crValue = cr.value
    , crTimestamp = Timestamp cr.timestamp
    , crHeaders =
        headersFromList
          [Header k v | (k, v) <- cr.headers]
    , crPartition = Just (fromIntegral cr.partition)
    }


-- The 'KC.topic' record field name is shared by
-- 'KC.ConsumerRecord' and 'KC.TopicPartition'. Use a tiny
-- selector that pins it to 'KC.ConsumerRecord' so the import
-- isn't ambiguous.
crTopicText :: KC.ConsumerRecord -> Text
crTopicText cr = cr.topic


----------------------------------------------------------------------
-- Convenience for demos that aren't broker-compatible
----------------------------------------------------------------------

{- | Print a clear message to stderr explaining that a demo will
run in-memory regardless of the requested mode. Used by demos
whose contract depends on test-driver-only knobs (explicit
record timestamps, 'advanceDriverStreamTime', Punctuator
coordination) that the broker producer/consumer API can't
reproduce yet.
-}
brokerOnlyWarning :: String -> RunMode -> IO ()
brokerOnlyWarning name = \case
  InMemory -> pure ()
  Broker _ ->
    hPutStrLn stderr $
      "wireform-kafka-streams-examples: '"
        <> name
        <> "' depends on the in-process test driver "
        <> "(explicit timestamps / stream-time advance / store mutation); "
        <> "ignoring --broker and running in-memory."


{- | Convenience: warn + run the existing in-memory body. Used by
the @ops-*@ demos which are wired to 'MockSet' / 'WorkerPool'
and have no broker analogue.
-}
runInMemoryWith :: String -> RunMode -> IO () -> IO ()
runInMemoryWith name mode body = brokerOnlyWarning name mode >> body
