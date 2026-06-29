module Main (main) where

import Control.Monad (unless)
import Hedgehog (Group, checkSequential)
import System.Exit (exitFailure)

import Test.Compression qualified
import Test.Envelope qualified
import Test.Error qualified
import Test.Interop qualified
import Test.Loopback qualified
import Test.Metadata qualified
import Test.Protocol qualified

allGroups :: [Group]
allGroups =
  [ Test.Protocol.tests
  , Test.Error.tests
  , Test.Envelope.tests
  , Test.Metadata.tests
  , Test.Compression.tests
  , Test.Loopback.tests
  , Test.Interop.tests
  ]

main :: IO ()
main = do
  results <- mapM checkSequential allGroups
  unless (and results) exitFailure
