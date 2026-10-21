module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Syd (describe, sydTest)

import Test.Compression qualified
import Test.Envelope qualified
import Test.Error qualified
import Test.Interop qualified
import Test.Loopback qualified
import Test.Metadata qualified
import Test.OpenAPI qualified
import Test.Protocol qualified
import Test.SharedService qualified

main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung forever (observed: blocked on an evaluated CAF).
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-connect" $ do
      Test.Protocol.tests
      Test.Error.tests
      Test.Envelope.tests
      Test.Metadata.tests
      Test.Compression.tests
      Test.Loopback.tests
      Test.SharedService.tests
      Test.Interop.tests
      Test.OpenAPI.tests
