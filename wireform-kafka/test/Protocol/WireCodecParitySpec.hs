{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

{- | Round-trip tests for the native 'Wire' codec dispatch:
property-based @decode . encode == id@ checks plus exact-byte
unit checks for the on-wire layout.
-}
module Protocol.WireCodecParitySpec (tests) where

import Data.ByteString qualified as BS
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
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ResponseHeader qualified as RsH
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


tests :: Spec
tests =
  describe "Wire codec round-trips (native dispatch)" $
    sequence_
      [ describe "RequestHeader" $
          sequence_
            [ it
                "decode . encode == id"
                prop_requestHeader_roundTrip
            , it
                "v2 sample byte length (api=18, ver=3, corr=42, cid=\"abc\")"
                unit_requestHeader_v2Sample
            ]
      , describe "ResponseHeader" $
          sequence_
            [ it
                "decode . encode == id"
                prop_responseHeader_roundTrip
            , it
                "v0 / v1 sample byte lengths"
                unit_responseHeader_samples
            ]
      , describe "ApiVersionsRequest" $
          sequence_
            [ it
                "decode . encode == id"
                prop_apiVersionsRequest_roundTrip
            ]
      , describe "MetadataRequest (representative array-of-struct shape)" $
          sequence_
            [ it
                "v9 round-trips through the native codec"
                unit_metadataRequest_v9RoundTrip
            ]
      ]


------------------------------------------------------------------
-- RequestHeader
------------------------------------------------------------------

genRequestHeaderVersion :: Gen Int16
genRequestHeaderVersion = Gen.element [1, 2, 3]


genRequestHeader :: Gen RH.RequestHeader
genRequestHeader = do
  ak <- Gen.int16 (Range.linear 0 64)
  av <- Gen.int16 (Range.linear 0 16)
  cid <- Gen.int32 (Range.linear 0 1000)
  client <- Gen.text (Range.linear 0 32) Gen.alphaNum
  pure
    RH.RequestHeader
      { RH.requestHeaderRequestApiKey = ak
      , RH.requestHeaderRequestApiVersion = av
      , RH.requestHeaderCorrelationId = cid
      , RH.requestHeaderClientId = P.mkKafkaString client
      , RH.requestHeaderClientInstanceId = P.nullUuid
      }


prop_requestHeader_roundTrip :: Property
prop_requestHeader_roundTrip = property $ do
  v <- forAll genRequestHeaderVersion
  msg <- forAll genRequestHeader
  let !bs = WC.runEncodeVer @RH.RequestHeader v msg
  case WC.runDecodeVer @RH.RequestHeader v bs of
    Left err -> annotate err >> failure
    Right rt -> rt === msg


unit_requestHeader_v2Sample :: IO ()
unit_requestHeader_v2Sample = do
  let msg =
        RH.RequestHeader
          { RH.requestHeaderRequestApiKey = 18
          , RH.requestHeaderRequestApiVersion = 3
          , RH.requestHeaderCorrelationId = 42
          , RH.requestHeaderClientId = P.mkKafkaString "abc"
          , RH.requestHeaderClientInstanceId = P.nullUuid
          }
      !bs = WC.runEncodeVer @RH.RequestHeader 2 msg
  -- v2 RequestHeader layout: 2 (apiKey) + 2 (apiVersion) + 4 (corr)
  -- + 2 + 3 (clientId as INT16-prefixed string; 'flexibleVersions:
  -- none' on this field) + 1 (empty tagged-fields trailer).
  BS.length bs `shouldBe` 2 + 2 + 4 + 2 + 3 + 1


------------------------------------------------------------------
-- ResponseHeader
------------------------------------------------------------------

genResponseHeaderVersion :: Gen Int16
genResponseHeaderVersion = Gen.element [0, 1]


genResponseHeader :: Gen RsH.ResponseHeader
genResponseHeader = do
  cid <- Gen.int32 (Range.linear 0 (1024 * 1024))
  pure RsH.ResponseHeader {RsH.responseHeaderCorrelationId = cid}


prop_responseHeader_roundTrip :: Property
prop_responseHeader_roundTrip = property $ do
  v <- forAll genResponseHeaderVersion
  msg <- forAll genResponseHeader
  let !bs = WC.runEncodeVer @RsH.ResponseHeader v msg
  case WC.runDecodeVer @RsH.ResponseHeader v bs of
    Left err -> annotate err >> failure
    Right rt -> rt === msg


unit_responseHeader_samples :: IO ()
unit_responseHeader_samples = do
  let msg = RsH.ResponseHeader {RsH.responseHeaderCorrelationId = 0x4DEADBEE}
      !v0 = WC.runEncodeVer @RsH.ResponseHeader 0 msg
      !v1 = WC.runEncodeVer @RsH.ResponseHeader 1 msg
  BS.length v0 `shouldBe` 4
  BS.length v1 `shouldBe` 4 + 1 -- + empty tagged-fields trailer


------------------------------------------------------------------
-- ApiVersionsRequest
------------------------------------------------------------------

genApiVersionsRequestVersion :: Gen Int16
genApiVersionsRequestVersion = Gen.element [3, 4, 5, 6]


genApiVersionsRequest :: Gen AVR.ApiVersionsRequest
genApiVersionsRequest = do
  name <- Gen.text (Range.linear 0 24) Gen.alphaNum
  ver <- Gen.text (Range.linear 0 24) Gen.alphaNum
  pure
    AVR.ApiVersionsRequest
      { AVR.apiVersionsRequestClientSoftwareName = P.mkKafkaString name
      , AVR.apiVersionsRequestClientSoftwareVersion = P.mkKafkaString ver
      , AVR.apiVersionsRequestClusterId = P.KafkaString P.Null
      , AVR.apiVersionsRequestNodeId = -1
      }


prop_apiVersionsRequest_roundTrip :: Property
prop_apiVersionsRequest_roundTrip = property $ do
  v <- forAll genApiVersionsRequestVersion
  msg <- forAll genApiVersionsRequest
  let !bs = WC.runEncodeVer @AVR.ApiVersionsRequest v msg
  case WC.runDecodeVer @AVR.ApiVersionsRequest v bs of
    Left err -> annotate err >> failure
    Right rt -> rt === msg


------------------------------------------------------------------
-- MetadataRequest — exercises arrays + nested structs
------------------------------------------------------------------

unit_metadataRequest_v9RoundTrip :: IO ()
unit_metadataRequest_v9RoundTrip = do
  let msg =
        MR.MetadataRequest
          { MR.metadataRequestTopics =
              P.mkKafkaArray (mempty :: V.Vector MR.MetadataRequestTopic)
          , MR.metadataRequestAllowAutoTopicCreation = True
          , MR.metadataRequestIncludeClusterAuthorizedOperations = False
          , MR.metadataRequestIncludeTopicAuthorizedOperations = True
          }
      v = 9
      !bs = WC.runEncodeVer @MR.MetadataRequest v msg
  case WC.runDecodeVer @MR.MetadataRequest v bs of
    Left err -> error ("decodeMetadataRequest failed: " <> err)
    Right rt -> rt `shouldBe` msg
