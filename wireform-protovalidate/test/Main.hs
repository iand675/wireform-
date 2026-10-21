module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Protovalidate.Advanced qualified
import Test.Protovalidate.Descriptor qualified
import Test.Protovalidate.Format qualified
import Test.Protovalidate.OpenAPI qualified
import Test.Protovalidate.OpenAPICompose qualified
import Test.Protovalidate.Refined qualified
import Test.Protovalidate.Schema qualified
import Test.Protovalidate.TH qualified
import Test.Protovalidate.Validation qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest
    $ describe
      "wireform-protovalidate"
    $ sequence_
      [ Test.Protovalidate.Format.tests
      , Test.Protovalidate.Validation.tests
      , Test.Protovalidate.Schema.tests
      , Test.Protovalidate.Descriptor.tests
      , Test.Protovalidate.Refined.tests
      , Test.Protovalidate.TH.tests
      , Test.Protovalidate.Advanced.tests
      , Test.Protovalidate.OpenAPI.tests
      , Test.Protovalidate.OpenAPICompose.tests
      ]
