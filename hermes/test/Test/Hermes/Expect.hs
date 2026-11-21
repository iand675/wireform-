{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Expect (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
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


unit_expect_sliced_continue :: Spec
unit_expect_sliced_continue = it "recognizes a bare continue token at every byte alignment" $ do
  let canonical = "100-continue"
      readField wire = lookupHeader @E.Expect (headerMapFromList [("Expect", wire)])
  forM_ [0 .. 7] $ \offset -> do
    let slice wire = BS.take (BS.length wire) (BS.drop offset (BS.replicate offset 0 <> wire <> "!"))
    forM_ [canonical, "100-CoNtInUe", "100-continu", "100-continue!", "100-continue=other"] $ \wire -> do
      readField (slice wire) `shouldBe` fmap Just (parseExpect wire)
      E.hasContinueExpectation [slice wire] `shouldBe` (wire == canonical || wire == "100-CoNtInUe")


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


unit_expect_field_lines :: Spec
unit_expect_field_lines = it "combines all Expect lines and preserves quoted extension bytes" $ do
  let word = "\"a" <> BS.pack [0xe9] <> ", b\""
      fields =
        headerMapFromList
          [ ("Expect", ", extension=" <> word <> " ; ; mode=fast,")
          , ("Expect", " , \t, ")
          , ("Expect", ", 100-Continue ")
          ]
  case lookupHeader @E.Expect fields of
    Right (Just value) ->
      lookupRawHeader "Expect" (setHeader value (headerMapFromList []))
        `shouldBe` Just (("extension=" <> word <> ";mode=fast, 100-Continue") :| [])
    other -> expectationFailure (show other)
  lookupHeader @E.Expect (headerMapFromList [("Expect", " , , \t")]) `shouldBe` Right (Just E.ExpectEmpty)
  E.expectationList E.ExpectEmpty `shouldBe` []
  let splitQuote = [("Expect", "extension=\"open"), ("Expect", "100-continue"), ("Expect", "close\"")]
  fmap (fmap renderExpect) (lookupHeader @E.Expect (headerMapFromList splitQuote))
    `shouldBe` Right (Just "extension=\"open, 100-continue, close\"")
  E.hasContinueExpectation (map snd splitQuote) `shouldBe` False
  E.hasContinueExpectation ["extension=\"100-continue\"", ", 100-Continue "] `shouldBe` True
  E.hasContinueExpectation ["100-continue=extension", "extension=\"100-continue\""] `shouldBe` False
  E.hasContinueExpectation ["100-continue", "extension;x=1"] `shouldBe` False
  mapM_
    (\wire -> parseExpect wire `shouldSatisfy` isLeft)
    ["extension;mode=fast", "extension=ready;mode", "extension=ready;mode =fast", "extension =ready"]


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
paramGen = (,) <$> tokenGen <*> (Just <$> tokenGen)


expectationGen :: Gen E.Expectation
expectationGen = do
  name <- tokenGen
  value <- Gen.maybe tokenGen
  params <- maybe (pure []) (const (Gen.list (Range.linear 0 3) paramGen)) value
  pure (E.Expectation name value params)


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


unit_earlydata_presence :: Spec
unit_earlydata_presence = it "interprets any received Early-Data instance as the marker" $ do
  let empty = headerMapFromList []
  lookupHeader @ED.EarlyData empty `shouldBe` Right Nothing
  mapM_
    ( \value -> do
        parseEarlyData value `shouldSatisfy` isLeft
        lookupHeader @ED.EarlyData (headerMapFromList [("Early-Data", value)]) `shouldBe` Right (Just (ED.EarlyData 1))
    )
    ["", "0", "01", "-1", "1;flag", "2, 1", "invalid"]
  lookupHeader @ED.EarlyData (headerMapFromList [("Early-Data", "0"), ("Early-Data", "1")]) `shouldBe` Right (Just (ED.EarlyData 1))
  lookupRawHeader "Early-Data" (setHeader (ED.EarlyData 0) empty) `shouldBe` Just ("1" :| [])


unit_expect_checked_write :: Spec
unit_expect_checked_write = it "rejects malformed or unrepresentable authored expectations" $ do
  let fields = headerMapFromList [("Expect", "100-continue"), ("Expect", "x=old")]
      checked entry = E.Expect (E.Expectation "100-continue" Nothing [] :| [entry])
  forM_
    [ E.Expectation "x,y" Nothing []
    , E.Expectation "wait" (Just "\"unterminated") []
    , E.Expectation "wait" Nothing [("limit", Just "1")]
    , E.Expectation "wait" (Just "fast") [("bad;name", Just "1")]
    , E.Expectation "wait" (Just "fast") [("flag", Nothing)]
    , E.Expectation "wait" (Just "fast") [("param", Just "")]
    ]
    $ \entry -> do
      renderHeaderLines defaultHeaderSettings (checked entry) `shouldBe` Nothing
      setHeader (checked entry) fields `shouldBe` fields
  let field = E.Expect (E.Expectation "100-continue" Nothing [] :| [])
  renderHeaderLines defaultHeaderSettings field `shouldBe` Just ("100-continue" :| [])
  renderHeaderLines defaultHeaderSettings E.ExpectEmpty `shouldBe` Just ("" :| [])


tests :: Spec
tests =
  describe "Expect" $
    sequence_
      [ unit_expect_continue
      , unit_expect_sliced_continue
      , unit_expect_params
      , unit_expect_render
      , unit_expect_field_lines
      , unit_expect_checked_write
      , unit_earlydata_parse
      , unit_earlydata_render
      , it "Expect round-trips" prop_expect_roundtrip
      , unit_earlydata_presence
      ]
