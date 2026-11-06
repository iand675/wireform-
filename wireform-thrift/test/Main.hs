module Main (main) where

import Test.Syd
import Test.Thrift.CompactBool qualified
import Test.Thrift.Derive qualified


main :: IO ()
main =
  sydTest
    ( describe "wireform-thrift" $
        sequence_
          [ Test.Thrift.Derive.tests
          , Test.Thrift.CompactBool.tests
          ]
    )
