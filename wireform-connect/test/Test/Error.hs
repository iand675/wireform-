{-# LANGUAGE OverloadedStrings #-}

module Test.Error (tests) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (isLeft)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Network.Connect.Error
import Network.HTTP.Types.Status (pattern Status)
import Network.GRPC.Spec (GrpcError (..))

tests :: Group
tests =
  Group
    "Error"
    [ ("code name round-trips for all codes", withTests 1 (property codeNames))
    , ("status mapping matches the spec table", withTests 1 (property statusTable))
    , ("HTTP-to-code inference matches the spec table", withTests 1 (property inferTable))
    , ("error envelope JSON round-trips", property envelopeRoundtrip)
    , ("rejects {} and {code:null}", withTests 1 (property rejectInvalid))
    ]

codeNames :: PropertyT IO ()
codeNames = mapM_ (\c -> connectCodeFromName (connectCodeName c) === Just c) allConnectCodes

statusTable :: PropertyT IO ()
statusTable = do
  connectCodeToHttpStatus GrpcCancelled === Status 499
  connectCodeToHttpStatus GrpcUnimplemented === Status 501
  connectCodeToHttpStatus GrpcInvalidArgument === Status 400
  connectCodeToHttpStatus GrpcUnauthenticated === Status 401
  connectCodeToHttpStatus GrpcUnavailable === Status 503

inferTable :: PropertyT IO ()
inferTable = do
  httpStatusToConnectCode (Status 404) === GrpcUnimplemented
  httpStatusToConnectCode (Status 401) === GrpcUnauthenticated
  httpStatusToConnectCode (Status 429) === GrpcUnavailable
  httpStatusToConnectCode (Status 502) === GrpcUnavailable
  httpStatusToConnectCode (Status 418) === GrpcUnknown

envelopeRoundtrip :: PropertyT IO ()
envelopeRoundtrip = do
  c <- forAll (Gen.element allConnectCodes)
  let err = ConnectError c (Just "boom") []
  decodeConnectError (encodeConnectError err) === Right err
  let err2 = ConnectError c Nothing []
  decodeConnectError (encodeConnectError err2) === Right err2

rejectInvalid :: PropertyT IO ()
rejectInvalid = do
  assert (isLeft (decodeConnectError (Aeson.object [])))
  assert (isLeft (decodeConnectError (Aeson.object ["code" .= Aeson.Null])))
