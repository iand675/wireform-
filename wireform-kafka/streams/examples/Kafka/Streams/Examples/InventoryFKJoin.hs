{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.InventoryFKJoin
Description : KIP-213 KTable-KTable foreign-key join

Demonstrates 'F.foreignKeyJoin': product catalog @KTable@
joined into a per-warehouse inventory @KTable@ on a foreign
key extracted from the inventory value.

Java (paraphrased):

@
KTable<String, Inventory> inventory = builder.table("inventory");
KTable<String, Product>   products  = builder.table("products");
KTable<String, Stocked>   stocked   = inventory.join(
  products,
  (Inventory inv) -> inv.productId,         // FK extractor
  (Inventory inv, Product p) -> new Stocked(inv, p)
);
stocked.toStream().to("stocked");
@

Haskell (free-arrow): both sides are materialised as tables,
paired with '&&&', and run through 'F.foreignKeyJoin' before a
'F.toStream' + 'F.sink' tail.
-}
module Kafka.Streams.Examples.InventoryFKJoin (
  runDemo,
  fkJoinTopology,
  buildFKJoinTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Materialized qualified as Mat
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


fkJoinTopology :: F.Topology Void ()
fkJoinTopology =
  F.joinForeignKey
    inventory
    products
    (\v -> T.takeWhile (/= '|') v)
    ( \inv prod ->
        prod <> "|" <> T.drop 1 (T.dropWhile (/= '|') inv)
    )
    stockedMat
    >>> F.toStream
    >>> F.sink "stocked"
  where
    inventory :: F.Topology Void (KTable Text Text)
    inventory = F.tableSource "inventory"

    products :: F.Topology Void (KTable Text Text)
    products = F.tableSource "products"

    stockedMat :: Materialized Text Text
    stockedMat =
      Mat.withValueSerde textSerde $
        Mat.withKeySerde textSerde $
          Mat.materializedAs (storeName "stocked-store")


buildFKJoinTopology :: IO Topo.Topology
buildFKJoinTopology = F.buildTopologyFrom fkJoinTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== InventoryFKJoinDemo ==="
  topo <- buildFKJoinTopology
  driver <- newDriver topo "fk-join-app"

  let inv w v =
        pipeInput
          driver
          (topicName "inventory")
          (Just (BSC.pack (T.unpack w)))
          (BSC.pack (T.unpack v))
          (Timestamp 0)
          0
      prod p v =
        pipeInput
          driver
          (topicName "products")
          (Just (BSC.pack (T.unpack p)))
          (BSC.pack (T.unpack v))
          (Timestamp 0)
          0

  prod "p-1" "Coffee Beans|grocery"
  prod "p-2" "USB Cable|electronics"
  inv "w-east" "p-1|120"
  inv "w-east" "p-2|45"
  inv "w-west" "p-1|80"

  prod "p-1" "Single-Origin Coffee|grocery"

  inv "w-east" "p-3|10"
  prod "p-3" "Stovetop Kettle|kitchenware"

  out <- readOutput driver (topicName "stocked")
  putStrLn ("Stocked records (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
      in putStrLn ("  " <> k <> " -> " <> BSC.unpack (crValueBytes cr))
