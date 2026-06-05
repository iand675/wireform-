module Main (main) where

import Test.Syd

import qualified Test.EDI.Derive as Derive

main :: IO ()
main = sydTest Derive.tests
