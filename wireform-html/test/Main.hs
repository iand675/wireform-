module Main (main) where

import Test.HTML.Derive qualified
import Test.HTML.Entities qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-html-derive" $
      sequence_
        [ Test.HTML.Entities.tests
        , Test.HTML.Derive.tests
        ]
