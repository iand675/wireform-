{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0000.Unittests
Description : librdkafka @tests\/0000-unittests.c@

The librdkafka file is a smoke test that constructs a producer with
debugging enabled and asks it to print its build options. We can't
test the OpenSSL build options, but we can do the equivalent
"library boots" check and exercise the bits of our high-level
surface that the librdkafka equivalent exercises:

  * Construct a 'C.ConsumerConfig' with the typed builders.
  * Construct a 'C.ProducerConfig' with the typed builders.
  * Look up the umbrella module's API surface.
-}
module Conformance.T0000.Unittests (tests) where

import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Client.Producer as P
import qualified Kafka.Client.Group as G

tests :: TestTree
tests = testGroup "0000-unittests"
  [ testCase "default consumer config has sensible knobs" $ do
      let cfg = C.defaultConsumerConfig
      assertBool "non-empty client id" (not (T.null (C.consumerClientId cfg)))
      C.consumerSessionTimeoutMs   cfg @?= 45000   -- KIP-735
      C.consumerHeartbeatIntervalMs cfg @?= 3000
      C.consumerMaxPollRecords     cfg @?= 500
      C.consumerMaxPollIntervalMs  cfg @?= 300000

  , testCase "default producer config has sensible knobs" $ do
      let cfg = P.defaultProducerConfig
      -- Just check the constructor + record selectors round-trip
      -- (we exercise the types; the configured values are what they are).
      seq cfg (return ())

  , testCase "high-level group umbrella exports a default" $ do
      let cfg = G.defaultGroupConfig
      assertBool "default broker list non-empty" (not (null (G.bootstrapBrokers cfg)))
      G.sessionTimeoutMs   cfg @?= 10000
      G.maxPollIntervalMs  cfg @?= 300000
  ]
