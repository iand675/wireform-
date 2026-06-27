{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Range (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Range as R
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String R.Range
parseOk bs = case runParser R.rangeParser bs of
  OK r leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right r
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: R.Range -> ByteString
render = M.toStrictByteString . R.renderRange


unit_closed_range :: Spec
unit_closed_range = it "closed byte range" $
  case parseOk "bytes=0-499" of
    Right (R.ByteRanges (R.ByteRangeInt 0 (Just 499) :| [])) -> pure () :: IO ()
    other -> error (show other)


unit_open_range :: Spec
unit_open_range = it "open-ended range" $
  case parseOk "bytes=500-" of
    Right (R.ByteRanges (R.ByteRangeInt 500 Nothing :| [])) -> pure () :: IO ()
    other -> error (show other)


unit_suffix :: Spec
unit_suffix = it "suffix range" $
  case parseOk "bytes=-1024" of
    Right (R.ByteRanges (R.ByteRangeSuffix 1024 :| [])) -> pure () :: IO ()
    other -> error (show other)


unit_multi :: Spec
unit_multi = it "multiple byte ranges" $
  case parseOk "bytes=0-499, 1000-, -500" of
    Right (R.ByteRanges ne) ->
      NE.toList ne
        `shouldBe` [ R.ByteRangeInt 0 (Just 499)
                   , R.ByteRangeInt 1000 Nothing
                   , R.ByteRangeSuffix 500
                   ]
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "render closed + open + suffix" $
    let v =
          R.ByteRanges
            ( R.ByteRangeInt 0 (Just 99)
                :| [ R.ByteRangeInt 200 Nothing
                   , R.ByteRangeSuffix 50
                   ]
            )
    in render v `shouldBe` "bytes=0-99,200-,-50"


-- ---------------------------------------------------------------------------
-- Property: bytes ranges round-trip
-- ---------------------------------------------------------------------------

brGen :: Gen R.ByteRange
brGen =
  Gen.choice
    [ do
        a <- Gen.word64 (Range.linear 0 1_000_000)
        b <- Gen.word64 (Range.linear a (a + 1_000_000))
        pure (R.ByteRangeInt a (Just b))
    , R.ByteRangeInt <$> Gen.word64 (Range.linear 0 1_000_000) <*> pure Nothing
    , R.ByteRangeSuffix <$> Gen.word64 (Range.linear 1 1_000_000)
    ]


prop_roundtrip :: Property
prop_roundtrip = property $ do
  rs <- forAll (Gen.list (Range.linear 1 5) brGen)
  let r = R.ByteRanges (NE.fromList rs)
      bs = render r
  case parseOk bs of
    Right r' -> r === r'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Range" $
    sequence_
      [ unit_closed_range
      , unit_open_range
      , unit_suffix
      , unit_multi
      , unit_render
      , it "round-trip" prop_roundtrip
      ]
