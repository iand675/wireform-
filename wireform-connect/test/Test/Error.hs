{-# LANGUAGE OverloadedStrings #-}

module Test.Error (tests) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Network.Connect.Error
import Network.GRPC.Spec (GrpcError (..))
import Network.HTTP.Types.Status (pattern Status)
import Test.Syd
import Test.Syd.Hedgehog ()

tests :: Spec
tests =
  describe "Error" $ do
    it "code name round-trips for all codes" $
      mapM_ (\c -> connectCodeFromName (connectCodeName c) `shouldBe` Just c) allConnectCodes
    it "status mapping matches the spec table" $ do
      connectCodeToHttpStatus GrpcCancelled `shouldBe` Status 499
      connectCodeToHttpStatus GrpcUnimplemented `shouldBe` Status 501
      connectCodeToHttpStatus GrpcInvalidArgument `shouldBe` Status 400
      connectCodeToHttpStatus GrpcUnauthenticated `shouldBe` Status 401
      connectCodeToHttpStatus GrpcUnavailable `shouldBe` Status 503
    it "HTTP-to-code inference matches the spec table" $ do
      httpStatusToConnectCode (Status 404) `shouldBe` GrpcUnimplemented
      httpStatusToConnectCode (Status 401) `shouldBe` GrpcUnauthenticated
      httpStatusToConnectCode (Status 429) `shouldBe` GrpcUnavailable
      httpStatusToConnectCode (Status 502) `shouldBe` GrpcUnavailable
      httpStatusToConnectCode (Status 418) `shouldBe` GrpcUnknown
    it "error envelope JSON round-trips" $ H.property $ do
      c <- H.forAll (Gen.element allConnectCodes)
      let err = ConnectError c (Just "boom") []
      decodeConnectError (encodeConnectError err) H.=== Right err
      let err2 = ConnectError c Nothing []
      decodeConnectError (encodeConnectError err2) H.=== Right err2
    it "rejects {} and {code:null}" $ do
      decodeConnectError (Aeson.object []) `shouldSatisfy` isLeft
      decodeConnectError (Aeson.object ["code" .= Aeson.Null]) `shouldSatisfy` isLeft
