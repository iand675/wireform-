module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Fory.Derive qualified
import Test.Fory.Direct qualified
import Test.Fory.Encoding qualified
import Test.Fory.MetaStringInterop qualified
import Test.Fory.SpecExtensions qualified
import Test.Fory.Value qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-fory" $
      sequence_
        [ Test.Fory.Encoding.tests
        , Test.Fory.Value.tests
        , Test.Fory.SpecExtensions.tests
        , Test.Fory.MetaStringInterop.tests
        , Test.Fory.Derive.tests
        , Test.Fory.Direct.tests
        ]
