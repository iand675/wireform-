{-# LANGUAGE OverloadedStrings #-}

module Test.Protocol (tests) where

import Hedgehog
import Network.Connect.Protocol

tests :: Group
tests =
  Group
    "Protocol"
    [ ("unary content-type round-trips", withTests 1 (property roundtripUnary))
    , ("streaming content-type round-trips", withTests 1 (property roundtripStream))
    , ("unknown content-type rejected", withTests 1 (property rejectUnknown))
    ]

roundtripUnary :: PropertyT IO ()
roundtripUnary = do
  parseContentType (unaryContentType CodecProto) === Just (Unary, CodecProto)
  parseContentType (unaryContentType CodecJSON) === Just (Unary, CodecJSON)

roundtripStream :: PropertyT IO ()
roundtripStream = do
  parseContentType (streamContentType CodecProto) === Just (Streaming, CodecProto)
  parseContentType (streamContentType CodecJSON) === Just (Streaming, CodecJSON)

rejectUnknown :: PropertyT IO ()
rejectUnknown = do
  parseContentType "text/plain" === Nothing
  parseContentType "application/grpc+proto" === Nothing
  parseContentType "application/octet-stream" === Nothing
