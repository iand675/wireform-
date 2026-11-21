{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Vector qualified as V
import Test.Syd
import Test.Thrift.CompactBool qualified
import Test.Thrift.Derive qualified
import Thrift.Parser (parseThrift)
import Thrift.QQ (thrift)
import Thrift.Schema (ThriftSchema (tsTypedefs), ThriftType (..), ThriftTypedef (..))


[thrift|
namespace * test.idl
struct AnnotatedRecord {
  1: required list<i32 (test.inner = "true")> (test.container = "true") values
}
|]


idlTests :: Spec
idlTests = describe "IDL" $ do
  it "accepts wildcard namespaces and annotations on base and container types" $ do
    let source = "namespace * test.idl\n"
          <> "typedef string (unicode.encoding = 'UTF-16') Label (foo = 'bar')\n"
          <> "typedef list<double (cpp.fixed_point = '16')> (cpp.template = 'std::list') Samples"
    fmap tsTypedefs (parseThrift source) `shouldBe` Right
      [ThriftTypedef "Label" TString, ThriftTypedef "Samples" (TList TDouble)]
  it "compiles a quasiquote with a wildcard namespace and nested type annotations" $
    annotatedRecordValues (AnnotatedRecord (V.singleton 34)) `shouldBe` V.singleton 34


main :: IO ()
main =
  sydTest
    ( describe "wireform-thrift" $
        sequence_
          [ Test.Thrift.Derive.tests
          , Test.Thrift.CompactBool.tests
          , idlTests
          ]
    )
