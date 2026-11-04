module Main (main) where

import System.Environment (lookupEnv)
import Test.Ion.Codec qualified
import Test.Ion.Conformance qualified
import Test.Ion.Derive qualified
import Test.Ion.Interop qualified
import Test.Syd


main :: IO ()
main = do
  reference <- lookupEnv "WIREFORM_ION_REFERENCE"
  sydTest $
    describe "wireform-ion-derive" $
      sequence_
        [ Test.Ion.Codec.ionTests
        , Test.Ion.Conformance.tests
        , Test.Ion.Derive.tests
        , maybe (pure ()) Test.Ion.Interop.tests reference
        ]
