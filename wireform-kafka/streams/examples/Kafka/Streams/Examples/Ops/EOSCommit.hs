{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.EOSCommit
Description : Exactly-once-v2 commits: what a read_committed consumer sees

The same passthrough topology, once at least once and once exactly once
v2, on the in-memory broker. Both write the same output; under
exactly-once it only becomes visible to a @read_committed@ consumer
when its transaction commits.

Then a transaction commit fails: the runtime aborts the transaction,
rewinds the task to the last commit and processes the record again in
a new one. The aborted copy stays in the log (a @read_uncommitted@
peek sees it), but a @read_committed@ consumer sees the record exactly
once.
-}
module Kafka.Streams.Examples.Ops.EOSCommit (
  runDemo,
) where

import Data.Maybe (mapMaybe)
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker


runDemo :: IO ()
runDemo = withDemoDirs 2 $ \[d1, d2] -> do
  section "EOSCommitDemo"
  topo <- passthroughTopo

  bullet "Mode 1: at-least-once"
  b1 <- newMockBroker (Just 1)
  mockCreateTopic b1 "in" 1
  alo <- startInstance b1 (demoConfig "ops-alo" d1) topo
  mapM_ (sendTo b1 "in" 0) ["alpha", "bravo"]
  _ <- waitUntil [alo] ((== 2) . length <$> outputValues b1 "out")
  outputValues b1 "out" >>= \vs -> bullet ("    read-committed observer sees: " <> show vs)
  stopInstance alo

  bullet "Mode 2: exactly-once-v2"
  b2 <- newMockBroker (Just 1)
  mockCreateTopic b2 "in" 1
  eos <- startInstance b2 ((demoConfig "ops-eos" d2) {processingGuarantee = ExactlyOnceV2}) topo
  mapM_ (sendTo b2 "in" 0) ["gamma", "delta"]
  _ <- waitUntil [eos] ((== 2) . length <$> outputValues b2 "out")
  outputValues b2 "out" >>= \vs -> bullet ("    read-committed observer sees: " <> show vs)

  bullet "Mode 2 + a failed transaction commit"
  mockFailTransactionCommits b2 1
  sendTo b2 "in" 0 "echo"
  _ <- waitUntil [eos] ((== 3) . length <$> outputValues b2 "out")
  raw <- mapMaybe (fmap unbytes . drValue) <$> mockRecords b2 "out"
  bullet ("    read-uncommitted peek sees " <> show (length raw) <> " records: " <> show raw)
  outputValues b2 "out" >>= \vs ->
    bullet ("    read-committed observer sees: " <> show vs <> "  (the aborted copy is filtered out)")
  stopInstance eos
