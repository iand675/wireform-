{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Range (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
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


unit_typed_lookup :: Spec
unit_typed_lookup = it "parses mixed-case bytes and canonicalizes the ordered range list" $ do
  let fields = headerMapFromList [("Range", " \tByTeS= \t,,0007-0008 , -0,,100- ,\t")]
      value = R.ByteRanges (R.ByteRangeInt 7 (Just 8) :| [R.ByteRangeSuffix 0, R.ByteRangeInt 100 Nothing])
      typed = lookupHeader @R.Range
  typed fields `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList []))
    `shouldBe` [("range", "bytes=7-8,-0,100-")]
  typed (headerMapFromList [("Range", "bytes=0-1"), ("Range", "bytes=2-3")])
    `shouldSatisfy` isLeft


unit_range_bounds :: Spec
unit_range_bounds = it "accepts Word64 limits and rejects wrapping, reversed and malformed ranges" $ do
  let readRange wire = lookupHeader @R.Range (headerMapFromList [("Range", wire)])
      largest = maxBound
  readRange "bytes=18446744073709551615-18446744073709551615,-18446744073709551615"
    `shouldBe` Right (Just (R.ByteRanges (R.ByteRangeInt largest (Just largest) :| [R.ByteRangeSuffix largest])))
  mapM_
    (\wire -> readRange wire `shouldSatisfy` isLeft)
    [ "bytes=9-8"
    , "BYTES=0-1,8-7"
    , "bytes=18446744073709551616-"
    , "bytes=1-18446744073709551616"
    , "bytes=-18446744073709551616"
    , "bytes=, ,"
    , "bytes =0-1"
    , "bytes=0 -1"
    , "bytes=0-1,not-a-byte-range"
    ]


unit_extension :: Spec
unit_extension = it "retains extension units and accepts only visible bytes within an opaque member" $ do
  let readRange wire = lookupHeader @R.Range (headerMapFromList [("Range", wire)])
      visible = filter (/= 44) [33 .. 126]
      parses byte = isRight (readRange ("rows=" <> BS.singleton byte))
  readRange "bytes-extra=one,,TWO"
    `shouldBe` Right (Just (R.OtherRanges "bytes-extra" (R.RawRange "one" :| [R.RawRange "TWO"])))
  filter parses [minBound .. maxBound] `shouldBe` visible


-- ---------------------------------------------------------------------------
-- Property: bytes ranges round-trip
-- ---------------------------------------------------------------------------

brGen :: Gen R.ByteRange
brGen =
  Gen.choice
    [ do
        a <- Gen.word64 (Range.linear 0 maxBound)
        b <- Gen.word64 (Range.linear a maxBound)
        pure (R.ByteRangeInt a (Just b))
    , R.ByteRangeInt <$> Gen.word64 (Range.linear 0 maxBound) <*> pure Nothing
    , R.ByteRangeSuffix <$> Gen.word64 (Range.linear 0 maxBound)
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
      , unit_typed_lookup
      , unit_range_bounds
      , unit_extension
      , it "round-trip" prop_roundtrip
      ]
