{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0072.Headers
Description : librdkafka @tests\/0072-headers_ut.c@

librdkafka's @0072-headers_ut@ adds, looks up, removes, and iterates
over message headers via the @rd_kafka_header_*@ API. Our analogue
is 'Kafka.Protocol.RecordBatch.RecordHeader' — just a record with
a key (UTF-8 bytes) and an optional value. We test the cases the
librdkafka file covers:

  * Empty header set is a valid record.
  * Multiple headers preserve insertion order on encode/decode.
  * Headers with absent values are encoded as null length and
    decoded back to 'Nothing'.
-}
module Conformance.T0072.Headers (tests) where

import Data.ByteString qualified as BS
import Kafka.Protocol.RecordBatch qualified as RB
import Test.Syd


tests :: Spec
tests =
  describe "0072-headers_ut" $
    sequence_
      [ it "RecordHeader: present value is returned verbatim" $ do
          let h = RB.RecordHeader "trace-id" (Just "abc123")
          RB.headerKey h `shouldBe` "trace-id"
          RB.headerValue h `shouldBe` Just "abc123"
      , it "RecordHeader: absent value (Nothing) is preserved" $ do
          let h = RB.RecordHeader "x-debug" Nothing
          RB.headerValue h `shouldBe` Nothing
      , it "Empty header list is a valid Record" $ do
          let r =
                RB.Record
                  { RB.recordOffsetDelta = 0
                  , RB.recordTimestampDelta = 0
                  , RB.recordKey = Nothing
                  , RB.recordValue = Just "payload"
                  , RB.recordHeaders = []
                  }
          length (RB.recordHeaders r) `shouldBe` 0
      , it "Multiple headers preserve order" $ do
          let hs =
                [ RB.RecordHeader "a" (Just "1")
                , RB.RecordHeader "b" Nothing
                , RB.RecordHeader "c" (Just "3")
                ]
              r = RB.Record 0 0 Nothing (Just BS.empty) hs
          map RB.headerKey (RB.recordHeaders r) `shouldBe` ["a", "b", "c"]
          map RB.headerValue (RB.recordHeaders r) `shouldBe` [Just "1", Nothing, Just "3"]
      ]
