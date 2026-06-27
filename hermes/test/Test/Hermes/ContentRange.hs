{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ContentRange (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ContentRange as CR
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String CR.ContentRange
parseOk bs = case runParser CR.contentRangeParser bs of
  OK c leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right c
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: CR.ContentRange -> ByteString
render = M.toStrictByteString . CR.renderContentRange


unit_satisfied :: Spec
unit_satisfied = it "satisfied bytes/total" $
  case parseOk "bytes 0-499/1234" of
    Right (CR.ContentRange unit (CR.RangeRespSatisfied 0 499 (Just 1234))) ->
      unit `shouldBe` ST.fromString "bytes"
    other -> error (show other)


unit_satisfied_star :: Spec
unit_satisfied_star = it "satisfied with unknown total" $
  case parseOk "bytes 0-9/*" of
    Right (CR.ContentRange _ (CR.RangeRespSatisfied 0 9 Nothing)) -> pure () :: IO ()
    other -> error (show other)


unit_unsatisfied :: Spec
unit_unsatisfied = it "unsatisfied range" $
  case parseOk "bytes */4096" of
    Right (CR.ContentRange _ (CR.RangeRespUnsatisfied (Just 4096))) -> pure () :: IO ()
    other -> error (show other)


unit_render_satisfied :: Spec
unit_render_satisfied =
  it "render satisfied" $
    let v =
          CR.ContentRange
            (ST.fromString "bytes")
            (CR.RangeRespSatisfied 0 99 (Just 200))
    in render v `shouldBe` "bytes 0-99/200"


unit_render_unsatisfied :: Spec
unit_render_unsatisfied =
  it "render unsatisfied" $
    let v =
          CR.ContentRange
            (ST.fromString "bytes")
            (CR.RangeRespUnsatisfied (Just 4096))
    in render v `shouldBe` "bytes */4096"


-- Property: satisfied form round-trips for arbitrary positions.
respGen :: Gen CR.RangeResp
respGen =
  Gen.choice
    [ do
        a <- Gen.word64 (Range.linear 0 1_000_000)
        b <- Gen.word64 (Range.linear a (a + 1_000_000))
        t <- Gen.maybe (Gen.word64 (Range.linear (b + 1) 2_000_000))
        pure (CR.RangeRespSatisfied a b t)
    , CR.RangeRespUnsatisfied
        <$> Gen.maybe (Gen.word64 (Range.linear 0 1_000_000))
    ]


prop_roundtrip :: Property
prop_roundtrip = property $ do
  resp <- forAll respGen
  let v = CR.ContentRange (ST.fromString "bytes") resp
      bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "ContentRange" $
    sequence_
      [ unit_satisfied
      , unit_satisfied_star
      , unit_unsatisfied
      , unit_render_satisfied
      , unit_render_unsatisfied
      , it "round-trip" prop_roundtrip
      ]
