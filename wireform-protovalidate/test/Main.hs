module Main (main) where

import Test.Protovalidate.Advanced qualified
import Test.Protovalidate.Descriptor qualified
import Test.Protovalidate.Format qualified
import Test.Protovalidate.OpenAPI qualified
import Test.Protovalidate.OpenAPICompose qualified
import Test.Protovalidate.Refined qualified
import Test.Protovalidate.Schema qualified
import Test.Protovalidate.TH qualified
import Test.Protovalidate.Validation qualified
import Test.Syd


main :: IO ()
main =
  sydTest
    $ describe
      "wireform-protovalidate"
    $ sequence_
      [ Test.Protovalidate.Format.tests
      , Test.Protovalidate.Validation.tests
      , Test.Protovalidate.Schema.tests
      , Test.Protovalidate.Descriptor.tests
      , Test.Protovalidate.Refined.tests
      , Test.Protovalidate.TH.tests
      , Test.Protovalidate.Advanced.tests
      , Test.Protovalidate.OpenAPI.tests
      , Test.Protovalidate.OpenAPICompose.tests
      ]
