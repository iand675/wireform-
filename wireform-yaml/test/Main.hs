module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Syd
import Test.YAML.Annotated qualified
import Test.YAML.Conformance qualified
import Test.YAML.Decode qualified
import Test.YAML.Derive qualified
import Test.YAML.Encode qualified
import Test.YAML.Roundtrip qualified
import Test.YAML.Security qualified


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  conf <- Test.YAML.Conformance.tests
  sydTest $
    describe "wireform-yaml" $
      sequence_
        [ Test.YAML.Decode.tests
        , Test.YAML.Encode.tests
        , Test.YAML.Roundtrip.tests
        , Test.YAML.Annotated.tests
        , Test.YAML.Security.tests
        , conf
        , Test.YAML.Derive.tests
        ]
