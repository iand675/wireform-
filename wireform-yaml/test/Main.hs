module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Syd
import Test.YAML.Annotated qualified
import Test.YAML.Class qualified
import Test.YAML.Conformance qualified
import Test.YAML.Decode qualified
import Test.YAML.Derive qualified
import Test.YAML.Encode qualified
import Test.YAML.JSON qualified
import Test.YAML.Pretty qualified
import Test.YAML.Roundtrip qualified
import Test.YAML.Security qualified


main :: IO ()
main = do
  -- Initialize Hedgehog's shared skip configuration before starting workers.
  _ <- resolveSkip Nothing
  conformance <- Test.YAML.Conformance.tests
  sydTest $
    describe "wireform-yaml" $ do
      Test.YAML.Decode.tests
      Test.YAML.Encode.tests
      Test.YAML.Class.tests
      Test.YAML.Derive.tests
      Test.YAML.JSON.tests
      Test.YAML.Pretty.tests
      Test.YAML.Annotated.tests
      Test.YAML.Roundtrip.tests
      Test.YAML.Security.tests
      conformance
