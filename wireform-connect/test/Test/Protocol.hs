{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol (tests) where

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
