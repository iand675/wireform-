{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol (tests) where

import Control.Monad (forM_)
import Network.Connect.Protocol
import Test.Syd

tests :: Spec
tests =
  describe "Protocol" $ do
    it "unary content-type round-trips" $ do
      parseContentType (unaryContentType CodecProto) `shouldBe` Just (Unary, CodecProto)
      parseContentType (unaryContentType CodecJSON) `shouldBe` Just (Unary, CodecJSON)
    it "streaming content-type round-trips" $ do
      parseContentType (streamContentType CodecProto) `shouldBe` Just (Streaming, CodecProto)
      parseContentType (streamContentType CodecJSON) `shouldBe` Just (Streaming, CodecJSON)
    it "unknown content-type rejected" $ do
      parseContentType "text/plain" `shouldBe` Nothing
      parseContentType "application/grpc+proto" `shouldBe` Nothing
      parseContentType "application/octet-stream" `shouldBe` Nothing

    it "accepts UTF-8 and ignores other media type parameters" $ do
      parseContentType "APPLICATION/JSON; CHARSET=\"UTF-8\"; version=\"a;b\"" `shouldBe` Just (Unary, CodecJSON)
      parseContentType "\t application/connect+json;; charset=utf-8 ; \t" `shouldBe` Just (Streaming, CodecJSON)
      parseContentType "application/proto; charset=latin1" `shouldBe` Just (Unary, CodecProto)
      parseContentType "application/connect+proto; ignored=value" `shouldBe` Just (Streaming, CodecProto)
    it "rejects incompatible JSON charsets and malformed media types" $
      forM_ ["application", "application/json; charset=latin1", "application/connect+json; charset=utf-16", "application/json; charset=utf-8; charset=latin1", "application/json; charset=\""] $ \ct ->
        parseContentType ct `shouldBe` Nothing
    it "accepts zero and at most ten ASCII digits for a timeout" $ do
      parseTimeoutMillis "0000000000" `shouldBe` Just 0
      parseTimeoutMillis "9000000000" `shouldBe` Just (fromInteger (min 9000000000 (toInteger (maxBound `quot` 1000 :: Int))))
      forM_ ["", "+1", "-1", "1.0", "0x10", " 1", "10000000000", "\255"] $ \value ->
        parseTimeoutMillis value `shouldBe` Nothing
