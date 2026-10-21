{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Test.Bench qualified
import Test.Coverage qualified
import Test.Marker qualified
import Test.SVG qualified
import Test.Syd
import Test.Test qualified


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "wireform-stats" $
      sequence_
        [ Test.Marker.tests
        , Test.SVG.tests
        , Test.Bench.tests
        , Test.Test.tests
        , Test.Coverage.tests
        , trivial
        ]


trivial :: Spec
trivial = it "trivial sanity" $ True `shouldBe` True
