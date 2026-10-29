{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.OrdersEnrichment
Description : Multi-stage enrichment — KStream-KTable join

Inspired by the @kafka-streams-examples@ microservices /Orders/
topology: an order event stream is enriched with the customer's
profile via a KStream-KTable join.

Java (paraphrased):

@
KStream<String, Order>     orders     = builder.stream("orders");
KTable<String, Customer>   customers  = builder.table("customers");
KStream<String, Enriched>  enriched   = orders.join(customers, ...);
enriched.to("enriched-orders");
@

Haskell (free-arrow): the customer 'KTable' and the order
'KStream' are paired with '&&&' and fed to 'F.streamTableJoin'.
-}
module Kafka.Streams.Examples.OrdersEnrichment (
  runDemo,
  ordersEnrichmentTopology,
  buildOrdersEnrichmentTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


ordersEnrichmentTopology :: F.Topology Void ()
ordersEnrichmentTopology =
  F.joinStreamTable
    orders
    customers
    (\order profile -> profile <> "|" <> order)
    (joined textSerde textSerde textSerde)
    >>> F.sink "enriched-orders"
  where
    orders :: F.Topology Void (KStream Text Text)
    orders = F.source "orders"

    customers :: F.Topology Void (KTable Text Text)
    customers = F.tableSource "customers"


buildOrdersEnrichmentTopology :: IO Topo.Topology
buildOrdersEnrichmentTopology = F.buildTopologyFrom ordersEnrichmentTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== OrdersEnrichmentDemo ==="
  topo <- buildOrdersEnrichmentTopology
  driver <- newDriver topo "orders-app"

  let cust c v ts =
        pipeInput
          driver
          (topicName "customers")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack v))
          (Timestamp ts)
          0
      ord c v ts =
        pipeInput
          driver
          (topicName "orders")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack v))
          (Timestamp ts)
          0

  cust "c1" "alice|us-east" 0
  cust "c2" "bob|eu-west" 0
  ord "c1" "o-100|199.95" 10
  ord "c2" "o-101|49.50" 11
  cust "c1" "alice|us-west" 12
  ord "c1" "o-102|14.99" 13

  out <- readOutput driver (topicName "enriched-orders")
  putStrLn ("Enriched orders (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
      in putStrLn ("  " <> k <> " -> " <> BSC.unpack (crValueBytes cr))
