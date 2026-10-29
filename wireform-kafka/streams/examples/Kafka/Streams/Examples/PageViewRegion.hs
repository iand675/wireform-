{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.PageViewRegion
Description : KStream-KTable join — enrich page views with user region

Mirror of @org.apache.kafka.streams.examples.pageview.PageViewTypedDemo@.
The KTable holds @user -> region@; the KStream holds page views keyed
by user. Each view is enriched with the user's current region.

Java (paraphrased):

@
KStream<String,PageView>  views   = builder.stream("PageViews");
KTable <String,UserProfile> users = builder.table("UserProfiles");
KStream<String,String> enriched =
    views.join(users, (pv, up) -> pv.page + "," + up.region);
enriched.to("EnrichedPageViews");
@

Haskell (free-arrow): two source legs are brought together with
'&&&', the join consumes the resulting @(KStream, KTable)@ tuple,
and the result is sunk back to a topic.
-}
module Kafka.Streams.Examples.PageViewRegion (
  runDemo,
  pageViewRegionTopology,
  buildPageViewRegionTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


pageViewRegionTopology :: F.Topology Void ()
pageViewRegionTopology =
  F.joinStreamTable
    views
    users
    (\page region -> page <> "," <> region)
    (joined textSerde textSerde textSerde)
    >>> F.sink "EnrichedPageViews"
  where
    views :: F.Topology Void (KStream Text Text)
    views = F.source "PageViews"

    users :: F.Topology Void (KTable Text Text)
    users = F.tableSource "UserProfiles"


buildPageViewRegionTopology :: IO Topo.Topology
buildPageViewRegionTopology = F.buildTopologyFrom pageViewRegionTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== PageViewRegionDemo ==="
  topo <- buildPageViewRegionTopology
  driver <- newDriver topo "page-view-region-app"

  let user u r =
        pipeInput
          driver
          (topicName "UserProfiles")
          (Just (BSC.pack (T.unpack u)))
          (BSC.pack (T.unpack r))
          (Timestamp 0)
          0
  user "alice" "us-east"
  user "bob" "eu-west"
  user "carol" "ap-south"

  let view u page ts =
        pipeInput
          driver
          (topicName "PageViews")
          (Just (BSC.pack (T.unpack u)))
          (BSC.pack (T.unpack page))
          (Timestamp ts)
          0
  view "alice" "/home" 1
  view "bob" "/products" 2
  view "carol" "/checkout" 3
  view "dave" "/home" 4 -- not in KTable; dropped by inner join
  out <- readOutput driver (topicName "EnrichedPageViews")
  putStrLn ("Enriched page views (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let u = case crKey cr of
            Just k -> BSC.unpack k
            Nothing -> "<no-user>"
      in putStrLn ("  " <> u <> " -> " <> BSC.unpack (crValueBytes cr))
