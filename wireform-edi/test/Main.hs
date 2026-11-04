module Main (main) where

import System.Environment (lookupEnv)
import Test.EDI.Codec qualified as Codec
import Test.EDI.Derive qualified as Derive
import Test.EDI.Interop qualified as Interop
import Test.Syd


main :: IO ()
main = do
  reference <- lookupEnv "WIREFORM_EDIFACT_REFERENCE"
  sydTest $ do
    Codec.ediTests
    Derive.tests
    maybe (pure ()) Interop.tests reference
