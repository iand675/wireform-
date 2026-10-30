{-# LANGUAGE OverloadedStrings #-}

{- | Live-broker integration tests for kafka-streams. Skipped at run
time unless @WIREFORM_KAFKA_BROKER=host:port@ is set, mirroring
the convention of the existing @wireform-kafka-integration@ suite.
-}
module Main (main) where

import Streams.Integration.Live qualified as Live
import Streams.Integration.NativeDriver qualified as ND
import Streams.Integration.RoundTrip qualified as RT
import System.Environment (lookupEnv)
import Test.Syd


main :: IO ()
main = do
  brokers <- lookupEnv "WIREFORM_KAFKA_BROKER"
  case brokers of
    Nothing ->
      sydTest $
        describe "kafka-streams-integration" $
          sequence_
            [ it "skipped (set WIREFORM_KAFKA_BROKER to enable)" (pure () :: IO ())
            ]
    Just bs ->
      sydTest $
        describe "kafka-streams-integration" $
          sequence_
            [ RT.tests bs
            , ND.tests bs
            , Live.tests bs
            ]
