{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.Examples.Group
Description : Recommended high-level consumer — runConsumer

The simplest production-shape Kafka reader: 'Kafka.runConsumer'
joins a consumer group, drives the poll loop, calls the handler
once per record, commits after each successful return, and leaves
the group cleanly on a normal exit or an exception.

> cabal run wireform-kafka-client-examples group
-}
module Kafka.Client.Examples.Group (runDemo) where

import Data.ByteString.Char8 qualified as BS8
import Kafka qualified


runDemo :: IO ()
runDemo =
  Kafka.runConsumer
    Kafka.defaultGroupConfig
      { Kafka.bootstrapBrokers = ["localhost:9092"]
      , Kafka.groupId = "demo-group"
      , Kafka.topics = ["events"]
      }
    $ \rec ->
      putStrLn $ "got " <> show rec.key <> " -> " <> maybe "<null>" BS8.unpack rec.value
