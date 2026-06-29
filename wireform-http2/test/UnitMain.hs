module Main (main) where

import Test.Syd

import qualified Test.Frame
import qualified Test.FrameEdgeCases
import qualified Test.HPACK
import qualified Test.HPACKEdgeCases
import qualified Test.HPACKConcurrency
import qualified Test.HPACKDejaFu
import qualified Test.Connection
import qualified Test.Defensive

main :: IO ()
main = sydTest $ describe "wireform-http2" $ sequence_
  [ Test.Frame.tests
  , Test.FrameEdgeCases.tests
  , Test.HPACK.tests
  , Test.HPACKEdgeCases.tests
  , Test.HPACKConcurrency.tests
  , Test.HPACKDejaFu.tests
  , Test.Connection.tests
  , Test.Defensive.tests
  ]
