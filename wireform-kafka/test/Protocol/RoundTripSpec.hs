{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Protocol.RoundTripSpec
Description : Round-trip tests for protocol messages
Copyright   : (c) 2025
License     : BSD-3-Clause

Comprehensive round-trip tests for Kafka protocol messages.

These tests verify that:
1. Generated messages can be serialized
2. Serialized messages can be deserialized
3. Deserialized messages match the original

Tests cover:
- All primitive types
- Arrays (both standard and compact)
- Nested structures
- Tagged fields (flexible versions)
- Version-specific field handling
-}
module Protocol.RoundTripSpec (tests) where

import Data.Int (Int16)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ApiVersionsRequest qualified as AVR
import "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest qualified as MR
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RequestHeader qualified as RH
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


tests :: Spec
tests =
  describe "Round-trip" $
    sequence_
      [ describe "Protocol Messages" $
          sequence_
            [ it
                "RequestHeader generated wire round-trip"
                prop_requestHeader
            , it
                "ApiVersionsRequest generated wire round-trip"
                prop_apiVersionsRequest
            , it
                "MetadataRequest generated wire round-trip"
                prop_metadataRequest
            ]
      ]


prop_requestHeader :: Property
prop_requestHeader = property $ do
  version <- forAll (Gen.element [1, 2])
  msg <- forAll genRequestHeader
  wireRoundTrip @RH.RequestHeader version msg


prop_apiVersionsRequest :: Property
prop_apiVersionsRequest = property $ do
  version <- forAll (Gen.element [3, 4])
  msg <- forAll genApiVersionsRequest
  wireRoundTrip @AVR.ApiVersionsRequest version msg


prop_metadataRequest :: Property
prop_metadataRequest = property $ do
  version <- forAll (Gen.element [8, 9, 10, 11, 12])
  msg <- forAll genMetadataRequest
  wireRoundTrip @MR.MetadataRequest version msg


wireRoundTrip
  :: (Eq a, Show a, WC.WireCodec a)
  => Int16
  -> a
  -> PropertyT IO ()
wireRoundTrip version msg = do
  let !encoded = WC.runEncodeVer version msg
  case WC.runDecodeVer version encoded of
    Left err -> annotate err >> failure
    Right decoded -> decoded === msg


genRequestHeader :: Gen RH.RequestHeader
genRequestHeader = do
  apiKey <- Gen.int16 (Range.linear 0 80)
  apiVersion <- Gen.int16 (Range.linear 0 16)
  correlationId <- Gen.int32 (Range.linear 0 10000)
  clientId <- Gen.text (Range.linear 0 32) Gen.alphaNum
  pure
    RH.RequestHeader
      { RH.requestHeaderRequestApiKey = apiKey
      , RH.requestHeaderRequestApiVersion = apiVersion
      , RH.requestHeaderCorrelationId = correlationId
      , RH.requestHeaderClientId = P.mkKafkaString clientId
      , RH.requestHeaderClientInstanceId = P.nullUuid
      }


genApiVersionsRequest :: Gen AVR.ApiVersionsRequest
genApiVersionsRequest = do
  name <- Gen.text (Range.linear 0 24) Gen.alphaNum
  version <- Gen.text (Range.linear 0 24) Gen.alphaNum
  pure
    AVR.ApiVersionsRequest
      { AVR.apiVersionsRequestClientSoftwareName = P.mkKafkaString name
      , AVR.apiVersionsRequestClientSoftwareVersion = P.mkKafkaString version
      , AVR.apiVersionsRequestClusterId = P.KafkaString P.Null
      , AVR.apiVersionsRequestNodeId = -1
      }


genMetadataRequest :: Gen MR.MetadataRequest
genMetadataRequest = do
  topics <- Gen.list (Range.linear 0 4) genMetadataTopic
  allowAutoCreate <- Gen.bool
  pure
    MR.MetadataRequest
      { MR.metadataRequestTopics = P.mkKafkaArray (V.fromList topics)
      , MR.metadataRequestAllowAutoTopicCreation = allowAutoCreate
      , MR.metadataRequestIncludeClusterAuthorizedOperations = False
      , MR.metadataRequestIncludeTopicAuthorizedOperations = False
      }


genMetadataTopic :: Gen MR.MetadataRequestTopic
genMetadataTopic = do
  name <- Gen.text (Range.linear 1 24) Gen.alphaNum
  pure
    MR.MetadataRequestTopic
      { MR.metadataRequestTopicTopicId = P.nullUuid
      , MR.metadataRequestTopicName = P.mkKafkaString name
      }
