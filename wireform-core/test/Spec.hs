module Main where

import Test.Syd
import Wireform.Base64.Test qualified as Base64
import Wireform.FFI.Test qualified as FFI
import Wireform.Parser.Test qualified as Parser
import Wireform.Ring.Test qualified as Ring
import Wireform.Transport.SendBoundaryTest qualified as SendBoundary
import Wireform.Transport.SendTest qualified as Send


main :: IO ()
main = sydTest $ do
  Ring.spec
  Parser.spec
  Send.spec
  SendBoundary.spec
  Base64.spec
  FFI.spec
