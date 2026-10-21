module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.FlatBuffers.Builder qualified
import Test.FlatBuffers.Derive qualified
import Test.FlatBuffers.View qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-flatbuffers" $
      sequence_
        [ Test.FlatBuffers.Builder.tests
        , Test.FlatBuffers.Derive.tests
        , Test.FlatBuffers.View.tests
        ]
