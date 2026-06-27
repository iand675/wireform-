{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Priority (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Priority as P
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String P.Priority
parseOk bs = case runParser P.priorityParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: P.Priority -> ByteString
render = M.toStrictByteString . P.renderPriority


unit_parse :: Spec
unit_parse = it "parses urgency and bare incremental" $
  case parseOk "u=5, i" of
    Right (P.Priority 5 True) -> pure () :: IO ()
    other -> error (show other)


unit_parse_default_incremental :: Spec
unit_parse_default_incremental = it "incremental defaults to false" $
  case parseOk "u=0" of
    Right (P.Priority 0 False) -> pure () :: IO ()
    other -> error (show other)


unit_parse_explicit_false :: Spec
unit_parse_explicit_false = it "explicit i=?0 is false" $
  case parseOk "u=2, i=?0" of
    Right (P.Priority 2 False) -> pure () :: IO ()
    other -> error (show other)


unit_parse_explicit_true :: Spec
unit_parse_explicit_true = it "explicit i=?1 is true" $
  case parseOk "i=?1" of
    Right (P.Priority 3 True) -> pure () :: IO ()
    other -> error (show other)


unit_parse_empty :: Spec
unit_parse_empty = it "empty value yields defaults" $
  case parseOk "" of
    Right (P.Priority 3 False) -> pure () :: IO ()
    other -> error (show other)


unit_parse_out_of_range :: Spec
unit_parse_out_of_range = it "out-of-range urgency falls back to default" $
  case parseOk "u=9" of
    Right (P.Priority 3 False) -> pure () :: IO ()
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "renders urgency with incremental" $
    render (P.Priority 5 True) `shouldBe` "u=5, i"


unit_render_default :: Spec
unit_render_default =
  it "renders non-incremental without i" $
    render (P.Priority 3 False) `shouldBe` "u=3"


priorityGen :: Gen P.Priority
priorityGen =
  P.Priority
    <$> Gen.int (Range.constant 0 7)
    <*> Gen.bool


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll priorityGen
  let bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Priority" $
    sequence_
      [ unit_parse
      , unit_parse_default_incremental
      , unit_parse_explicit_false
      , unit_parse_explicit_true
      , unit_parse_empty
      , unit_parse_out_of_range
      , unit_render
      , unit_render_default
      , it "round-trip" prop_roundtrip
      ]
