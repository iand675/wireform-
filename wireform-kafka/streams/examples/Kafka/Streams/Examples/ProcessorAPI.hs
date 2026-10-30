{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.ProcessorAPI
Description : Low-level Processor API + Punctuator

The DSL covers the 80% case; for the rest, the Processor API
gives you direct access to 'ProcessorContext' (forward,
schedule, getStateStore). This example mirrors the JVM
low-level demo: build a processor that maintains a
per-key event count and, on a wall-clock punctuator, emits a
summary record.

Java (paraphrased):

@
Topology t = new Topology();
t.addSource("src", "events");
t.addProcessor("counter", CountingProcessor::new, "src");
t.addStateStore(Stores.keyValueStoreBuilder(...), "counter");
t.addSink("snk", "summary", "counter");
@

Haskell (free-arrow). 'F.processValuesStream' emits a typed
@KStream k v'@; an 'F.liftIO_' fragment attaches the state
store to the processor's auto-generated node (the upstream
@KStream@ value carries its 'KS.kstreamParent' so the wiring
is local); a downstream 'F.sink' publishes the running counts.
-}
module Kafka.Streams.Examples.ProcessorAPI (
  runDemo,
  processorAPITopology,
  buildProcessorAPITopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.KStream qualified as KS
import Kafka.Streams.StreamsBuilder qualified as SB
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F
import Unsafe.Coerce qualified as Unsafe


storeNm :: StoreName
storeNm = storeName "counter-store"


{- | The custom processor: keeps a per-key Int64 count in a state
store; on every record forwards the running count downstream.
-}
countingProcessor :: IO (Processor Text Text)
countingProcessor = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore Text Int64))
  pure
    Processor
      { procName = processorName "CountingProcessor"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx storeNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef
                storeRef
                (Just (Unsafe.unsafeCoerce kvs))
            _ -> error "counter store missing"
          _ <-
            schedule
              ctx
              1000
              WallClockTimePunctuation
              (Punctuator (\_t -> pure ()))
          pure ()
      , procClose = pure ()
      , procProcess = \r -> case recordKey r of
          Nothing -> pure ()
          Just k -> do
            mctx <- readIORef ctxRef
            mStore <- readIORef storeRef
            case (mctx, mStore) of
              (Just ctx, Just kvs) -> do
                cur <- kvsGet kvs k
                let !next = maybe 1 (+ 1) cur
                kvsPut kvs k next
                forwardRecord
                  ctx
                  ( Record
                      (Just k)
                      next
                      (recordTimestamp r)
                      (recordHeaders r)
                      :: Record Text Int64
                  )
              _ -> pure ()
      }


processorAPITopology :: F.Topology Void ()
processorAPITopology =
  F.source "events"
    >>> F.processValuesStream
      "Counter"
      [storeNm]
      countingProcessor
      int64Serde
    -- Register the counter store, owned by the processor we just
    -- attached. The 'F.liftIO_' fragment receives the upstream
    -- 'KStream' so we can recover the processor's auto-generated
    -- node name via 'KS.kstreamParent' and grant it write access
    -- to the store.
    >>> F.liftIO_
      "attach-counter-store"
      ( \b s -> do
          SB.withTopology_ b $ \t ->
            Topo.addStateStoreKV
              ( inMemoryKeyValueStoreBuilder storeNm
                  :: StoreBuilderKV Text Int64
              )
              [KS.kstreamParent s]
              t
          pure s
      )
    >>> F.sink "counts-stream"


buildProcessorAPITopology :: IO Topo.Topology
buildProcessorAPITopology = F.buildTopologyFrom processorAPITopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== ProcessorAPIDemo ==="
  topo <- buildProcessorAPITopology
  driver <- newDriver topo "processor-api-app"

  let ev k tsMs =
        pipeInput
          driver
          (topicName "events")
          (Just (BSC.pack (T.unpack k)))
          (BSC.pack "hit")
          (Timestamp tsMs)
          0
  mapM_
    (\(k, ts) -> ev k ts)
    [ ("alice", 0)
    , ("bob", 1)
    , ("alice", 2)
    , ("alice", 3)
    , ("carol", 4)
    , ("bob", 5)
    ]
  out <- readOutput driver (topicName "counts-stream")
  putStrLn ("Counts emitted (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
          v = case deserialize int64Serde (crValueBytes cr) :: Either Text Int64 of
            Right n -> show n
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> k <> " -> " <> v)
