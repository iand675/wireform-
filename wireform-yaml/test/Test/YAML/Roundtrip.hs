{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Roundtrip (tests) where

import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog (Property, forAll, property, tripping)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import YAML.Decode (decode)
import YAML.Decode qualified as D
import YAML.Encode (encode)
import YAML.Encode qualified as E
import YAML.Pretty qualified as P
import YAML.Value


tests :: Spec
tests =
  modifyMaxSuccess (const 1000) $
    describe "roundtrip properties" $
      sequence_
        [ it "scalar roundtrip" prop_scalarRoundtrip
        , it "value roundtrip" prop_valueRoundtrip
        , it "pretty value roundtrip across layout options" prop_prettyRoundtrip
        , modifyMaxSuccess (const 100) largeIntegerTests
        ]


genScalar :: H.Gen Value
genScalar =
  Gen.choice
    [ pure YNull
    , YBool <$> Gen.bool
    , YInt <$> Gen.integral (Range.linearFrom 0 (-1_000_000) 1_000_000)
    , YFloat <$> Gen.double (Range.linearFracFrom 0 (-1e100) 1e100)
    , YString <$> genSafeString
    ]


genSafeString :: H.Gen T.Text
genSafeString = Gen.text (Range.linear 0 32) (Gen.choice [Gen.unicode, Gen.element [' ', '\NUL', '\r', '\n', '\t', '\xFEFF', '\xFFFE', '\xFFFF', '\xA0', '\x85', '\x2009', '\x2028', '\x2029']])


genValue :: Int -> H.Gen Value
genValue 0 = genScalar
genValue n =
  Gen.choice
    [ genScalar
    , YSeq . V.fromList <$> Gen.list (Range.linear 0 5) (genValue (n - 1))
    , YMap . V.fromList <$> Gen.filter (\pairs -> let keys = fst <$> pairs in length keys == HS.size (HS.fromList ((\key -> case key of YString text -> text; _ -> "") <$> keys))) (Gen.list (Range.linear 0 5) genKV)
    ]
  where
    genKV = (,) <$> (YString <$> genSafeKey) <*> genValue (n - 1)
    genSafeKey = genSafeString


prop_scalarRoundtrip :: Property
prop_scalarRoundtrip = property $ do
  v <- forAll genScalar
  tripping v encode decode


prop_valueRoundtrip :: Property
prop_valueRoundtrip = property $ do
  v <- forAll (genValue 3)
  tripping v encode decode


prop_prettyRoundtrip :: Property
prop_prettyRoundtrip = property $ do
  value <- forAll (genValue 3)
  opts <- forAll (Gen.element [P.defaultOptions, P.compactOptions, P.defaultOptions {P.roIndent = 4}, P.defaultOptions {P.roCompactSeqOfMap = False}, P.defaultOptions {P.roStringStyleHint = P.HintAlwaysSingle}, P.defaultOptions {P.roStringStyleHint = P.HintAlwaysDouble}])
  tripping value (P.render opts) decode


largeIntegerTests :: Spec
largeIntegerTests = do
  it "round-trips large integers across signs" $ property $ do
    n <- forAll (Gen.integral (Range.linear (negate (2 ^ (256 :: Int))) (2 ^ (256 :: Int))))
    tripping (YInt n) E.encode D.decode
