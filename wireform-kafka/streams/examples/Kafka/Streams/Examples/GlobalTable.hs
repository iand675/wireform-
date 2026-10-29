{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.GlobalTable
Description : KStream-GlobalKTable join (cluster-wide replicated lookup)

A 'GlobalKTable' is replicated to every instance, so a join
against it does not require co-partitioning. Useful for small
reference tables (currency rates, country codes, sku metadata)
that you want available on every node without rebalancing.

Java:

@
GlobalKTable<String, String> rates =
  builder.globalTable("rates");
KStream<String, Order> orders = builder.stream("orders");
KStream<String, Order> withRate = orders.join(
  rates,
  (orderId, order) -> order.currency,
  (order, rate) -> order.applyRate(rate)
);
withRate.to("orders-with-rate");
@

Haskell (free-arrow): the order 'KStream' and the global
lookup table are paired with '&&&' and routed through
'F.streamGlobalTableJoin'.
-}
module Kafka.Streams.Examples.GlobalTable (
  runDemo,
  globalTableTopology,
  buildGlobalTableTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


globalTableTopology :: F.Topology Void ()
globalTableTopology =
  F.joinStreamGlobalTable
    orders
    rates
    (\_orderId v -> T.takeWhile (/= '|') v)
    (\order rate -> order <> "|rate=" <> rate)
    >>> F.sink "orders-with-rate"
  where
    orders :: F.Topology Void (KStream Text Text)
    orders = F.source "orders"

    rates :: F.Topology Void (GlobalKTable Text Text)
    rates = F.globalTableSource "rates" textSerde textSerde


buildGlobalTableTopology :: IO Topo.Topology
buildGlobalTableTopology = F.buildTopologyFrom globalTableTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== GlobalTableDemo ==="
  topo <- buildGlobalTableTopology
  driver <- newDriver topo "global-table-app"

  let rate c v =
        pipeInput
          driver
          (topicName "rates")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack v))
          (Timestamp 0)
          0
      ord o v =
        pipeInput
          driver
          (topicName "orders")
          (Just (BSC.pack (T.unpack o)))
          (BSC.pack (T.unpack v))
          (Timestamp 0)
          0

  rate "USD" "1.00"
  rate "EUR" "1.08"
  rate "GBP" "1.27"

  ord "o-1" "USD|199.95"
  ord "o-2" "EUR|49.50"
  ord "o-3" "GBP|14.99"

  out <- readOutput driver (topicName "orders-with-rate")
  putStrLn ("Joined orders (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
      in putStrLn ("  " <> k <> " -> " <> BSC.unpack (crValueBytes cr))
