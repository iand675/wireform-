{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.Cogroup
Description : KIP-150 cogroup — combine multiple grouped streams
              with distinct value types into one aggregate

'F.cogroup' lets you merge two (or more) grouped streams of
different value types into a shared aggregator. The classic
demo: one stream of "deposit" amounts and another of
"withdrawal" amounts, both keyed by accountId, contribute to a
running balance.

Java:

@
KStream<String, Long> deposits   = builder.stream("deposits");
KStream<String, Long> withdrawals = builder.stream("withdrawals");
KGroupedStream<String, Long> dgrp = deposits.groupByKey();
KGroupedStream<String, Long> wgrp = withdrawals.groupByKey();
KTable<String, Long> balance = dgrp
  .cogroup((k, v, agg) -> agg + v)
  .cogroup(wgrp, (k, v, agg) -> agg - v)
  .aggregate(() -> 0L, Materialized.as("balances"));
balance.toStream().to("balances-out");
@

Haskell (free-arrow). The cogroup is built up incrementally
inside a small 'F.liftIO_' block — it's the cleanest way to
combine the two source-rooted 'KGroupedStream' fragments into
the @(CogroupedStream, KGroupedStream)@ tuple that
'F.addCogrouped' expects without contorting the source legs.
-}
module Kafka.Streams.Examples.Cogroup (
  runDemo,
  cogroupTopology,
  buildCogroupTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Cogroup qualified as Cog
import Kafka.Streams.Materialized qualified as Mat
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


cogroupTopology :: F.Topology Void ()
cogroupTopology =
  -- Build a CogroupedStream by compiling each source-rooted
  -- 'F.Topology' fragment into the shared 'StreamsBuilder' and
  -- chaining 'cogroup' / 'addCogrouped' against the resulting
  -- 'KGroupedStream' handles.
  F.liftIO_
    "build-balance-cogroup"
    ( \b _ -> do
        dgrp <- F.compileInBuilder b deposits
        wgrp <- F.compileInBuilder b withdrawals
        let cog0 = Cog.cogroup dgrp (\_k v acc -> acc + v)
            cog = Cog.addCogrouped cog0 wgrp (\_k v acc -> acc - v)
        pure cog
    )
    >>> F.aggregateCogrouped (pure (0 :: Int64)) balancesMat
    >>> F.toStream
    >>> F.sink "balances-out"
  where
    deposits :: F.Topology Void (KGroupedStream Text Int64)
    deposits =
      F.source @Text @Int64 "deposits"
        >>> F.groupByKey
    withdrawals :: F.Topology Void (KGroupedStream Text Int64)
    withdrawals =
      F.source @Text @Int64 "withdrawals"
        >>> F.groupByKey
    balancesMat :: Materialized Text Int64
    balancesMat =
      Mat.withValueSerde int64Serde $
        Mat.withKeySerde textSerde $
          Mat.materializedAs (storeName "balances-store")


buildCogroupTopology :: IO Topo.Topology
buildCogroupTopology = F.buildTopologyFrom cogroupTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== CogroupDemo ==="
  topo <- buildCogroupTopology
  driver <- newDriver topo "cogroup-app"

  let dep acct n =
        pipeInput
          driver
          (topicName "deposits")
          (Just (BSC.pack (T.unpack acct)))
          (serialize int64Serde n)
          (Timestamp 0)
          0
      wdr acct n =
        pipeInput
          driver
          (topicName "withdrawals")
          (Just (BSC.pack (T.unpack acct)))
          (serialize int64Serde n)
          (Timestamp 0)
          0

  dep "alice" 1000
  dep "bob" 500
  wdr "alice" 200
  dep "alice" 50
  wdr "bob" 300
  dep "carol" 750

  out <- readOutput driver (topicName "balances-out")
  putStrLn ("Balance updates (" <> show (length out) <> "):")
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
      in putStrLn ("  " <> k <> " = " <> v)
