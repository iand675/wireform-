{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.Examples.Consume
Description : The smallest possible consumer — bracket + poll loop.

Uses 'Kafka.Client.Consumer.withConsumer' (the low-level bracket
that keeps the application driving the poll loop). Polls once,
commits, prints what came back. For a "call this handler once per
record" loop see 'Group'.

> cabal run wireform-kafka-client-examples consume
-}
module Kafka.Client.Examples.Consume (runDemo) where

import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS8
import Kafka.Client.Consumer qualified as Consumer


runDemo :: IO ()
runDemo =
  Consumer.withConsumer
    ["localhost:9092"]
    "demo-consume"
    Consumer.defaultConsumerConfig
    ["events"]
    $ \c -> do
      Right recs <- Consumer.poll c 5000
      forM_ recs $ \rec ->
        putStrLn $ "  " <> show rec.offset <> " " <> maybe "<null>" BS8.unpack rec.value
      _ <- Consumer.commitSync c
      pure ()
