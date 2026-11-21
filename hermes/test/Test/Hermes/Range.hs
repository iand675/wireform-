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
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, renderHeaderLines, setHeader)
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


unit_resolve :: Spec
unit_resolve = it "resolves typed field members against the current byte length" $ do
  let fields = headerMapFromList [("Range", "bytes=8-20,4-,11-,-3,-30,-0")]
  case lookupHeader @R.Range fields of
    Right (Just (R.ByteRanges ranges)) ->
      map (R.resolveByteRange 10) (NE.toList ranges)
        `shouldBe` [ Just (R.ResolvedByteRange 8 2)
                   , Just (R.ResolvedByteRange 4 6)
                   , Nothing
                   , Just (R.ResolvedByteRange 7 3)
                   , Just (R.ResolvedByteRange 0 10)
                   , Nothing
                   ]
    other -> expectationFailure (show other)


unit_resolve_limits :: Spec
unit_resolve_limits = it "checks empty, reversed, and maximum-length ranges before subtracting" $ do
  let largest = maxBound
  R.resolveByteRange largest (R.ByteRangeInt 0 (Just largest)) `shouldBe` Just (R.ResolvedByteRange 0 largest)
  R.resolveByteRange largest (R.ByteRangeInt (largest - 1) Nothing) `shouldBe` Just (R.ResolvedByteRange (largest - 1) 1)
  R.resolveByteRange largest (R.ByteRangeInt largest Nothing) `shouldBe` Nothing
  R.resolveByteRange 10 (R.ByteRangeInt 5 (Just 4)) `shouldBe` Nothing
  R.resolveByteRange 0 (R.ByteRangeInt 0 Nothing) `shouldBe` Nothing
  R.resolveByteRange 0 (R.ByteRangeSuffix 0) `shouldBe` Nothing
  R.resolveByteRange 0 (R.ByteRangeSuffix largest) `shouldBe` Just (R.ResolvedByteRange 0 0)


unit_range_checked_write :: Spec
unit_range_checked_write = it "rejects invalid typed Range writes without replacing existing lines" $ do
  let fields = headerMapFromList [("Range", "bytes=0-1")]
      extension unit entry = R.OtherRanges unit (R.RawRange entry :| [])
      invalid =
        [ R.ByteRanges (R.ByteRangeInt 0 (Just 1) :| [R.ByteRangeInt 8 (Just 7)])
        , extension "ByTeS" "opaque"
        , extension "two words" "opaque"
        , extension "rows" ""
        , extension "rows" "first,second"
        , extension "rows" "first second"
        ]
  mapM_
    ( \value -> do
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value fields `shouldBe` fields
    )
    invalid
  let valid = extension "rows" "0-3"
  lookupHeader @R.Range (setHeader valid fields) `shouldBe` Right (Just valid)


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
      , unit_resolve
      , unit_resolve_limits
      , unit_range_checked_write
      , it "round-trip" prop_roundtrip
      ]
