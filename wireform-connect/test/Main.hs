module Main (main) where

import Test.Syd (describe, sydTest)

import Test.Compression qualified
import Test.Envelope qualified
import Test.Error qualified
import Test.Interop qualified
import Test.Loopback qualified
import Test.Metadata qualified
import Test.Protocol qualified

main :: IO ()
main =
  sydTest $
    describe "wireform-connect" $ do
      Test.Protocol.tests
      Test.Error.tests
      Test.Envelope.tests
      Test.Metadata.tests
      Test.Compression.tests
      Test.Loopback.tests
      Test.Interop.tests
