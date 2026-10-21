module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Derive.Aeson qualified
import Test.Derive.Extension qualified
import Test.Derive.Fixtures qualified
import Test.Derive.Modifier qualified
import Test.Derive.NameStyle qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest tests


tests :: Spec
tests =
  describe "wireform-derive" $
    sequence_
      [ Test.Derive.NameStyle.tests
      , Test.Derive.Modifier.tests
      , Test.Derive.Extension.tests
      , Test.Derive.Fixtures.tests
      , Test.Derive.Aeson.tests
      ]
