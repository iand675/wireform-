{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.SideEffects
Description : Every flavour of side effect the streams DSL supports

The DSL has four seams for running 'IO' inside a topology:

  * 'F.peek'           — observe records non-destructively
                         (mirrors @KStream.peek@)
  * 'F.mapValuesM'     — IO-flavoured @KStream.mapValues@; the JVM
                         version is nominally pure but in practice
                         people sneak side effects into it. Here it
                         is explicit.
  * 'F.foreach'        — terminal IO sink, no downstream node
                         (mirrors @KStream.foreach@)
  * The Processor API + a 'Punctuator' scheduled by
    'schedule', for wall-clock or stream-time triggered work
    (mirrors @ProcessorContext.schedule@).

This demo wires all four into a single 'F.Topology' value that
emulates a mini order-processing pipeline:

  1. Trace every incoming event with 'F.peek' (logger seam).
  2. Enrich each order via a simulated /external/ customer
     lookup with 'F.mapValuesM' (DB / HTTP seam).
  3. Tap a metrics counter with another 'F.peek' (metrics seam).
  4. A Processor + stream-time 'Punctuator' summarises the
     batch every 30 seconds (background-flush seam). It runs
     on the /same/ source via the 'Semigroup' instance on
     @F.Topology Void ()@ — both halves of the @<>@ share the
     same upstream lineage so they commit atomically under EOS.

In a real deployment you would replace the IORefs with whatever
side-effecting clients your service uses.

== Caveats vs. the JVM

* Effects in 'peek' / 'foreach' / 'mapValuesM' / a 'Punctuator'
  are NOT part of an EOS-V2 transaction. A topology rewind on
  rebalance will replay them. If exactly-once is required for
  the side effect, gate it on an idempotency-token state store
  the way the JVM advice does.
* On a single task, effects fire in topology order. With
  @numStreamThreads > 1@ different keys may be processed
  concurrently across tasks; if your effect needs a global
  ordering, route through @F.repartition@ to one partition or
  sink to a topic and consume separately.
-}
module Kafka.Streams.Examples.SideEffects (
  runDemo,
  buildSideEffectsTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F
import Unsafe.Coerce qualified as Unsafe


{- | A bag of "external systems" the topology talks to. We pass
it in so the runDemo function can also inspect the resulting
effect log after running. In a production app these would be
handles to a logger, a DB connection pool, a metrics client,
and an outbox.
-}
data Externals = Externals
  { extLog :: IORef [Text]
  , extMetrics :: IORef (Map.Map Text Int64)
  , extLookups :: IORef Int64
  , extProfileDB :: IORef (Map.Map Text Text)
  , extBatchOut :: IORef [(Timestamp, Text)]
  }


newExternals :: IO Externals
newExternals = do
  l <- newIORef []
  m <- newIORef Map.empty
  lu <- newIORef 0
  db <- newIORef Map.empty
  bs <- newIORef []
  pure (Externals l m lu db bs)


----------------------------------------------------------------------
-- Topology
----------------------------------------------------------------------

{- | Topology shape:

@
   orders [src]
     \>>> peek (log)
     \>>> mapValuesM (enrich via fake DB)
     \>>> peek (metrics tap)
     \>>> sink "enriched-orders"
@

combined via @\<\>@ with the parallel
background-flush sub-pipeline:

@
   orders [src]
     \>>> processWithStateStoreKV "BatchSummary" buf (procPunctuator)
@

Because both halves of @\<\>@ root in the same 'F.source', the
Kafka task assigner groups them under one task; EOS atomicity
extends across both branches.
-}
sideEffectsTopology :: Externals -> F.Topology Void ()
sideEffectsTopology ext =
  enrichLeg <> batchSummaryLeg
  where
    src :: F.Topology Void (KStream Text Text)
    src = F.source "orders"

    enrichLeg :: F.Topology Void ()
    enrichLeg =
      src
        >>> F.peek
          ( \r ->
              modifyIORef'
                (extLog ext)
                (\xs -> xs ++ [trace r])
          )
        >>> F.mapValuesM (lookupProfile ext)
        >>> F.peek (bumpMetrics ext)
        >>> F.sink "enriched-orders"

    batchSummaryLeg :: F.Topology Void ()
    batchSummaryLeg =
      src
        >>> F.processWithStateStoreKV
          "BatchSummary"
          bufBuilder
          (batchSummaryProc ext (storeName "batch-buffer"))

    bufBuilder :: StoreBuilderKV Text Int64
    bufBuilder = inMemoryKeyValueStoreBuilder (storeName "batch-buffer")

    trace r =
      "trace key="
        <> maybe "<no-key>" (T.pack . show) (recordKey r)
        <> " value="
        <> recordValue r


-- | Build the imperative 'Topology' graph from the AST.
buildSideEffectsTopology :: Externals -> IO Topo.Topology
buildSideEffectsTopology = F.buildTopologyFrom . sideEffectsTopology


----------------------------------------------------------------------
-- Side-effect handlers
----------------------------------------------------------------------

lookupProfile :: Externals -> Text -> IO Text
lookupProfile ext order = do
  modifyIORef' (extLookups ext) (+ 1)
  db <- readIORef (extProfileDB ext)
  let !customerId = T.takeWhile (/= '|') order
      !rest = T.drop 1 (T.dropWhile (/= '|') order)
      !customer = Map.findWithDefault "<unknown>" customerId db
  pure (customer <> "|" <> rest)


bumpMetrics :: Externals -> Record Text Text -> IO ()
bumpMetrics ext r = do
  modifyIORef'
    (extMetrics ext)
    (Map.insertWith (+) "events.observed" 1)
  let v = recordValue r
  if "<unknown>" `T.isInfixOf` v
    then
      modifyIORef'
        (extMetrics ext)
        (Map.insertWith (+) "events.unknown_customer" 1)
    else pure ()


----------------------------------------------------------------------
-- Background-flush processor
----------------------------------------------------------------------

batchSummaryProc
  :: Externals
  -> StoreName
  -> IO (Processor Text Text)
batchSummaryProc ext bufNm = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore Text Int64))
  pure
    Processor
      { procName = processorName "BatchSummary"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          getStateStore ctx bufNm >>= \case
            Just (AnyKeyValueStore kvs) ->
              writeIORef
                storeRef
                (Just (Unsafe.unsafeCoerce kvs))
            _ -> error "batchSummaryProc: store missing"
          _ <-
            schedule
              ctx
              30_000
              StreamTimePunctuation
              (Punctuator (\t -> flushBatch ext storeRef t))
          pure ()
      , procClose = pure ()
      , procProcess = \r -> do
          mst <- readIORef storeRef
          case (mst, recordKey r) of
            (Just kvs, Just k) -> do
              cur <- kvsGet kvs k
              kvsPut kvs k (maybe 1 (+ 1) cur)
            _ -> pure ()
      }


flushBatch
  :: Externals
  -> IORef (Maybe (KeyValueStore Text Int64))
  -> Timestamp
  -> IO ()
flushBatch ext storeRef now = do
  mst <- readIORef storeRef
  case mst of
    Nothing -> pure ()
    Just kvs -> do
      it <- kvsAll kvs
      pairs <- kvIteratorToList it
      mapM_
        ( \(k, n) -> do
            modifyIORef'
              (extBatchOut ext)
              (\xs -> xs ++ [(now, k <> ":" <> T.pack (show n))])
            kvsPut kvs k 0
        )
        pairs


----------------------------------------------------------------------
-- Demo
----------------------------------------------------------------------

runDemo :: IO ()
runDemo = do
  putStrLn "=== SideEffectsDemo ==="
  ext <- newExternals
  writeIORef (extProfileDB ext) $
    Map.fromList
      [("c1", "alice"), ("c2", "bob"), ("c3", "carol")]

  topo <- buildSideEffectsTopology ext
  driver <- newDriver topo "side-effects-app"

  let order ts c v =
        pipeInput
          driver
          (topicName "orders")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack (c <> "|" <> v)))
          (Timestamp ts)
          0
      sec :: Int64 -> Int64
      sec n = n * 1000

  order (sec 0) "c1" "o-100|199.95"
  order (sec 5) "c2" "o-101|49.50"
  order (sec 12) "c1" "o-102|14.99"
  order (sec 25) "c4" "o-103|9.00"
  order (sec 35) "c3" "o-104|79.00"
  order (sec 60) "c1" "o-105|10.00"
  order (sec 75) "c2" "o-106|22.50"

  advanceDriverStreamTime driver (Timestamp (sec 90))

  enriched <- readOutput driver (topicName "enriched-orders")
  putStrLn ("Enriched orders (sink): " <> show (length enriched))
  mapM_ (\cr -> putStrLn ("  -> " <> BSC.unpack (crValueBytes cr))) enriched

  putStrLn ""
  logLines <- readIORef (extLog ext)
  putStrLn ("peek log lines: " <> show (length logLines))
  mapM_ (\l -> putStrLn ("  " <> T.unpack l)) (take 3 logLines)
  putStrLn
    ( if length logLines > 3
        then "  ... " <> show (length logLines - 3) <> " more"
        else ""
    )

  putStrLn ""
  metrics <- readIORef (extMetrics ext)
  putStrLn "metrics counters (foreach tap):"
  mapM_
    (\(k, v) -> putStrLn ("  " <> T.unpack k <> " = " <> show v))
    (Map.toAscList metrics)

  putStrLn ""
  lookups <- readIORef (extLookups ext)
  putStrLn ("mapValuesM external lookups: " <> show lookups)

  putStrLn ""
  batches <- readIORef (extBatchOut ext)
  putStrLn
    ( "batch summaries flushed by Punctuator: "
        <> show (length batches)
    )
  mapM_
    ( \(Timestamp t, line) ->
        putStrLn ("  t=" <> show t <> " " <> T.unpack line)
    )
    batches

  closeDriver driver
