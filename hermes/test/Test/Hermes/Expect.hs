{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Expect (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.EarlyData as ED
import qualified Network.HTTP.Headers.Expect as E
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseExpect :: ByteString -> Either String E.Expect
parseExpect bs = case runParser E.expectParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseEarlyData :: ByteString -> Either String ED.EarlyData
parseEarlyData bs = case runParser ED.earlyDataParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderExpect :: E.Expect -> ByteString
renderExpect = M.toStrictByteString . E.renderExpect


renderEarlyData :: ED.EarlyData -> ByteString
renderEarlyData = M.toStrictByteString . ED.renderEarlyData


unit_expect_continue :: Spec
unit_expect_continue = it "parses 100-continue" $
  case parseExpect "100-continue" of
    Right (E.Expect (e :| [])) -> do
      E.expectationName e `shouldBe` ST.fromString "100-continue"
      E.expectationValue e `shouldBe` Nothing
      E.expectationParameters e `shouldBe` []
    other -> error (show other)


unit_expect_params :: Spec
unit_expect_params = it "parses an expectation list with value and params" $
  case parseExpect "100-continue, foo=bar;q=1" of
    Right (E.Expect (a :| [b])) -> do
      E.expectationName a `shouldBe` ST.fromString "100-continue"
      E.expectationName b `shouldBe` ST.fromString "foo"
      E.expectationValue b `shouldBe` Just (ST.fromString "bar")
      E.expectationParameters b `shouldBe` [(ST.fromString "q", Just (ST.fromString "1"))]
    other -> error (show other)


unit_expect_render :: Spec
unit_expect_render =
  it "renders 100-continue" $
    let v = E.Expect (E.Expectation (ST.fromString "100-continue") Nothing [] :| [])
    in renderExpect v `shouldBe` "100-continue"


unit_earlydata_parse :: Spec
unit_earlydata_parse = it "parses Early-Data 1" $
  case parseEarlyData "1" of
    Right v -> ED.earlyDataValue v `shouldBe` 1
    other -> error (show other)


unit_earlydata_render :: Spec
unit_earlydata_render =
  it "renders Early-Data 1" $
    renderEarlyData (ED.EarlyData 1) `shouldBe` "1"


tcharGen :: Gen Char
tcharGen = Gen.frequency [(4, Gen.alphaNum), (1, Gen.element ("!#$%&'*+-.^_`|~" :: String))]


tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) tcharGen


paramGen :: Gen (ST.ShortText, Maybe ST.ShortText)
paramGen = (,) <$> tokenGen <*> Gen.maybe tokenGen


expectationGen :: Gen E.Expectation
expectationGen =
  E.Expectation
    <$> tokenGen
    <*> Gen.maybe tokenGen
    <*> Gen.list (Range.linear 0 3) paramGen


expectGen :: Gen E.Expect
expectGen = do
  e <- expectationGen
  es <- Gen.list (Range.linear 0 2) expectationGen
  pure $ E.Expect (e :| es)


prop_expect_roundtrip :: Property
prop_expect_roundtrip = property $ do
  v <- forAll expectGen
  let bs = renderExpect v
  case parseExpect bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


earlyDataGen :: Gen ED.EarlyData
earlyDataGen = ED.EarlyData <$> Gen.int (Range.linear 0 1_000_000)


prop_earlydata_roundtrip :: Property
prop_earlydata_roundtrip = property $ do
  v <- forAll earlyDataGen
  let bs = renderEarlyData v
  case parseEarlyData bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Expect" $
    sequence_
      [ unit_expect_continue
      , unit_expect_params
      , unit_expect_render
      , unit_earlydata_parse
      , unit_earlydata_render
      , it "Expect round-trips" prop_expect_roundtrip
      , it "Early-Data round-trips" prop_earlydata_roundtrip
      ]
