module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.CEL.Conformance qualified
import Test.CEL.Properties qualified
import Test.CEL.TH qualified
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
      "wireform-cel"
    $ sequence_
      [ Test.CEL.Conformance.tests
      , Test.CEL.Properties.tests
      , Test.CEL.TH.tests
      ]
