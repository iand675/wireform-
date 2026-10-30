{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.RebalanceListener
Description : KIP-415 / 429 cooperative rebalance listener

The Java client lets users register a
@ConsumerRebalanceListener@ to react when the group coordinator
revokes / assigns / loses partitions:

  * @onPartitionsAssigned(partitions)@ — fired before the
    consumer starts fetching the new partitions.
  * @onPartitionsRevoked(partitions)@ — fired during a
    cooperative rebalance, /before/ the consumer relinquishes
    the partitions (commit offsets, flush buffered work).
  * @onPartitionsLost(partitions)@ — fired when the broker
    fenced the consumer; offsets cannot be committed and any
    in-flight processing is junk.

Mirrors KIP-415 (incremental cooperative rebalance, exposing
the lost / revoked distinction) + KIP-429 (the actual protocol
side that drives the callbacks).

Tests for the cooperative rebalancer's pure decision layer
('Streams.AssignorSpec') already cover the assignment math; this
module wires the listener-callback shape on the consumer side.
-}
module Kafka.Client.RebalanceListener (
  -- * Listener
  RebalanceListener (..),
  noopRebalanceListener,

  -- * Convenience constructors
  logRebalanceListener,
  combineListeners,

  -- * Dispatch
  dispatchAssigned,
  dispatchRevoked,
  dispatchLost,
) where

import Control.Exception (SomeException, try)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kafka.Telemetry.Log qualified as KLog
import Kafka.Client.Consumer (TopicPartition (..))


-- | Java's @ConsumerRebalanceListener@.
data RebalanceListener = RebalanceListener
  { rlOnAssigned :: !([TopicPartition] -> IO ())
  , rlOnRevoked :: !([TopicPartition] -> IO ())
  , rlOnLost :: !([TopicPartition] -> IO ())
  }
  deriving stock (Generic)


noopRebalanceListener :: RebalanceListener
noopRebalanceListener =
  RebalanceListener
    { rlOnAssigned = \_ -> pure ()
    , rlOnRevoked = \_ -> pure ()
    , rlOnLost = \_ -> pure ()
    }


{- | Listener that emits one OpenTelemetry log record per event
("Kafka.Telemetry.Log"), at info level, with the partitions as
attributes.
-}
logRebalanceListener :: RebalanceListener
logRebalanceListener =
  RebalanceListener
    { rlOnAssigned = log_ "assigned"
    , rlOnRevoked = log_ "revoked"
    , rlOnLost = log_ "lost"
    }
  where
    log_ tag tps =
      KLog.logInfo KLog.kafkaInstrumentationLibrary ("consumer group: partitions " <> tag) [KLog.attrShow "messaging.kafka.partitions" tps]


combineListeners :: RebalanceListener -> RebalanceListener -> RebalanceListener
combineListeners a b =
  RebalanceListener
    { rlOnAssigned = \tps -> rlOnAssigned a tps >> rlOnAssigned b tps
    , rlOnRevoked = \tps -> rlOnRevoked a tps >> rlOnRevoked b tps
    , rlOnLost = \tps -> rlOnLost a tps >> rlOnLost b tps
    }


{- | Best-effort dispatch helpers. Catch + swallow exceptions so
a buggy listener can't tear the consumer down.
-}
dispatchAssigned
  , dispatchRevoked
  , dispatchLost
    :: RebalanceListener -> [TopicPartition] -> IO ()
dispatchAssigned l tps = catchIgnore (rlOnAssigned l tps)
dispatchRevoked l tps = catchIgnore (rlOnRevoked l tps)
dispatchLost l tps = catchIgnore (rlOnLost l tps)


catchIgnore :: IO () -> IO ()
catchIgnore m = do
  r <- try m :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()
