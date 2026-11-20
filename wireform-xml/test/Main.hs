module Main (main) where

import Test.Syd
import Test.XML.Derive qualified
import Test.XML.FastEntities qualified


main :: IO ()
main =
  sydTest $
    describe "wireform-xml-derive" $
      sequence_
        [ Test.XML.FastEntities.tests
        , Test.XML.Derive.tests
        ]
