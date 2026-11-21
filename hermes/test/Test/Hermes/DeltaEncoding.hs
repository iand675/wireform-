{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DeltaEncoding (tests) where

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
import qualified Network.HTTP.Headers.AIM as AIM
import qualified Network.HTTP.Headers.DeltaBase as DB
import Network.HTTP.Headers.ETag (EntityTag (..))
import qualified Network.HTTP.Headers.IM as IM
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Parse / render helpers
-- ---------------------------------------------------------------------------

parseAIM :: ByteString -> Either String AIM.AIM
parseAIM bs = case runParser AIM.aIMParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


parseIM :: ByteString -> Either String IM.IM
parseIM bs = case runParser IM.iMParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


parseDeltaBase :: ByteString -> Either String DB.DeltaBase
parseDeltaBase bs = case runParser DB.deltaBaseParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


renderAIM :: AIM.AIM -> ByteString
renderAIM = M.toStrictByteString . AIM.renderAIM


renderIM :: IM.IM -> ByteString
renderIM = M.toStrictByteString . IM.renderIM


renderDeltaBase :: DB.DeltaBase -> ByteString
renderDeltaBase = M.toStrictByteString . DB.renderDeltaBase


-- ---------------------------------------------------------------------------
-- A-IM
-- ---------------------------------------------------------------------------

unit_aim_parse :: Spec
unit_aim_parse = it "parses A-IM list with optional q" $
  case parseAIM "vcdiff;q=0.5, gzip" of
    Right (AIM.AIM [AIM.WeightedIM t1 w1, AIM.WeightedIM t2 w2]) -> do
      t1 `shouldBe` ST.fromString "vcdiff"
      w1 `shouldBe` 0.5
      t2 `shouldBe` ST.fromString "gzip"
      w2 `shouldBe` 1.0
    other -> error (show other)


unit_aim_render :: Spec
unit_aim_render =
  it "renders A-IM list, omitting q=1" $
    let v =
          AIM.AIM
            [ AIM.WeightedIM (ST.fromString "vcdiff") 0.5
            , AIM.WeightedIM (ST.fromString "gzip") 1.0
            ]
    in renderAIM v `shouldBe` "vcdiff;q=0.5, gzip"


prop_aim_roundtrip :: Property
prop_aim_roundtrip = property $ do
  v <- forAll aimGen
  let bs = renderAIM v
  case parseAIM bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


unit_aim_fields :: Spec
unit_aim_fields = it "reads ordered A-IM field lines with an optional parameter before the weight" $ do
  let fields = headerMapFromList [("A-IM", ", vcdiff; flavor = \"a,b\" ; Q = 0.125, "), ("A-IM", "gzip, identity;q=0.,,")]
      value =
        AIM.AIM
          [ AIM.WeightedIMWithParameter "vcdiff" 0.125 (AIM.IMParameter "flavor" (Just (AIM.IMParameterQuoted "a,b")))
          , AIM.WeightedIM "gzip" 1
          , AIM.WeightedIM "identity" 0
          ]
  lookupHeader @AIM.AIM fields `shouldBe` Right (Just value)
  lookupRawHeader "A-IM" (setHeader value (headerMapFromList []))
    `shouldBe` Just ("vcdiff;flavor=\"a,b\";q=0.125, gzip, identity;q=0" :| [])
  lookupHeader @AIM.AIM (headerMapFromList [("A-IM", " ,\t, ")]) `shouldBe` Right (Just (AIM.AIM []))
  lookupHeader @AIM.AIM (headerMapFromList [("A-IM", "range;label=\"a"), ("A-IM", "b\";q=0.5")])
    `shouldBe` Right (Just (AIM.AIM [AIM.WeightedIMWithParameter "range" 0.5 (AIM.IMParameter "label" (Just (AIM.IMParameterQuoted "a,b")))]))
  parseAIM "range; flag; Q=1." `shouldBe` Right (AIM.AIM [AIM.WeightedIMWithParameter "range" 1 (AIM.IMParameter "flag" Nothing)])
  forM_ ["range;q", "range;q=1.001", "range;q=0.1234", "range;q=0.7;extra", "range;first;second", "range;flag;q=\"0.5\""] $ \bad ->
    lookupHeader @AIM.AIM (headerMapFromList [("A-IM", "gzip"), ("A-IM", bad)]) `shouldSatisfy` isLeft


checkedManipulationWrites :: Spec
checkedManipulationWrites = do
  let badParams =
        [ IM.IMParameter "Q" Nothing
        , IM.IMParameter "bad name" Nothing
        , IM.IMParameter "mode" (Just (IM.IMParameterToken "a;b"))
        , IM.IMParameter "mode" (Just (IM.IMParameterQuoted "a\DEL"))
        ]
  it "checks A-IM names and parameters before replacing the field" $ do
    let old = headerMapFromList [("A-IM", "gzip")]
        entries = AIM.WeightedIM "gzip" 1 : [AIM.WeightedIM "bad name" 0.5]
        bad = entries : map (\param -> [AIM.WeightedIM "gzip" 1, AIM.WeightedIMWithParameter "ext" 0.5 param]) badParams
    forM_ (map AIM.AIM bad) $ \value -> do
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value old `shouldBe` old
    renderHeaderLines defaultHeaderSettings (AIM.AIM [AIM.WeightedIM "identity" 1.5]) `shouldBe` Just ("identity" :| [])
  it "checks IM names and parameters, reserving identity for A-IM" $ do
    let old = headerMapFromList [("IM", "gzip")]
        first = IM.InstanceManipulation "gzip" Nothing
        bad = map (`IM.InstanceManipulation` Nothing) ["", "bad,name", "IdEnTiTy"]
          ++ map (IM.InstanceManipulation "ext" . Just) badParams
    forM_ bad $ \entry -> do
      let value = IM.IMEntries [first, entry]
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value old `shouldBe` old


unit_aim_authored_weights :: Spec
unit_aim_authored_weights =
  it "renders authored A-IM weights within the three-digit wire grammar" $
    renderAIM (AIM.AIM [AIM.WeightedIM "a" 0.0001, AIM.WeightedIM "b" 0.12345, AIM.WeightedIM "c" (0 / 0), AIM.WeightedIM "d" 1.5])
      `shouldBe` "a;q=0, b;q=0.123, c;q=0, d"


-- ---------------------------------------------------------------------------
-- IM
-- ---------------------------------------------------------------------------

unit_im_parse :: Spec
unit_im_parse = it "parses IM token list" $
  case parseIM "vcdiff, range" of
    Right (IM.IM [a, b]) -> do
      a `shouldBe` ST.fromString "vcdiff"
      b `shouldBe` ST.fromString "range"
    other -> error (show other)


unit_im_render :: Spec
unit_im_render =
  it "renders IM token list" $
    let v = IM.IM [ST.fromString "vcdiff", ST.fromString "range"]
    in renderIM v `shouldBe` "vcdiff, range"


prop_im_roundtrip :: Property
prop_im_roundtrip = property $ do
  v <- forAll imGen
  let bs = renderIM v
  case parseIM bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


unit_im_fields :: Spec
unit_im_fields = it "reads ordered IM field lines and retains quoted octets and flags" $ do
  let fields = headerMapFromList [("IM", ", vcdiff;flavor=\"a,\\\"" <> BS.singleton 255 <> "\", "), ("IM", "ext;raw=DATA, gzip;flag")]
      value =
        IM.IMEntries
          [ IM.InstanceManipulation "vcdiff" (Just (IM.IMParameter "flavor" (Just (IM.IMParameterQuoted "a,\"\xff"))))
          , IM.InstanceManipulation "ext" (Just (IM.IMParameter "raw" (Just (IM.IMParameterToken "DATA"))))
          , IM.InstanceManipulation "gzip" (Just (IM.IMParameter "flag" Nothing))
          ]
  lookupHeader @IM.IM fields `shouldBe` Right (Just value)
  IM.instanceManipulations value `shouldBe` ["vcdiff", "ext", "gzip"]
  lookupRawHeader "IM" (setHeader value (headerMapFromList []))
    `shouldBe` Just (("vcdiff;flavor=\"a,\\\"" <> BS.singleton 255 <> "\", ext;raw=DATA, gzip;flag") :| [])
  parseIM " \t, , " `shouldBe` Right (IM.IM [])
  parseIM "identity-ext" `shouldBe` Right (IM.IM ["identity-ext"])
  forM_ ["IDENTITY", "identity;ext", "range;Q", "range;q=0.5", "range;unclosed=\"x", "range;x=y;z"] $ \bad ->
    lookupHeader @IM.IM (headerMapFromList [("IM", "gzip"), ("IM", bad)]) `shouldSatisfy` isLeft


-- ---------------------------------------------------------------------------
-- Delta-Base
-- ---------------------------------------------------------------------------

unit_deltabase_parse :: Spec
unit_deltabase_parse = it "parses a strong entity-tag" $
  case parseDeltaBase "\"abc\"" of
    Right (DB.DeltaBase (StrongETag t)) -> t `shouldBe` ST.fromString "abc"
    other -> error (show other)


unit_deltabase_parse_weak :: Spec
unit_deltabase_parse_weak = it "parses a weak entity-tag" $
  case parseDeltaBase "W/\"xyz\"" of
    Right (DB.DeltaBase (WeakETag t)) -> t `shouldBe` ST.fromString "xyz"
    other -> error (show other)


unit_deltabase_render :: Spec
unit_deltabase_render =
  it "renders a strong entity-tag" $
    renderDeltaBase (DB.DeltaBase (StrongETag (ST.fromString "abc"))) `shouldBe` "\"abc\""


prop_deltabase_roundtrip :: Property
prop_deltabase_roundtrip = property $ do
  v <- forAll deltaBaseGen
  let bs = renderDeltaBase v
  case parseDeltaBase bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


unit_deltabase_fields :: Spec
unit_deltabase_fields = it "reads Delta-Base as one opaque entity tag" $ do
  let spelling = "W/\"" <> BS.singleton 255 <> "\""
      fields = headerMapFromList [("Delta-Base", " \t" <> spelling <> " ")]
  case lookupHeader @DB.DeltaBase fields of
    Right (Just value) ->
      lookupRawHeader "Delta-Base" (setHeader value (headerMapFromList [])) `shouldBe` Just (spelling :| [])
    other -> expectationFailure (show other)
  forM_ [["*"], ["\"v\", \"w\""], ["\"v\"", "\"w\""]] $ \bad ->
    lookupHeader @DB.DeltaBase (headerMapFromList (map (\field -> ("Delta-Base", field)) bad)) `shouldSatisfy` isLeft


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen =
  ST.fromString
    <$> Gen.element
      ["vcdiff", "diffe", "gzip", "deflate", "gdiff", "range", "identity"]


weightedGen :: Gen AIM.WeightedIM
weightedGen = do
  tag <- tokenGen
  weight <- (/ 1000) . fromIntegral <$> Gen.int (Range.linear 0 1000)
  Gen.choice
    [ pure (AIM.WeightedIM tag weight)
    , AIM.WeightedIMWithParameter tag weight <$> parameterGen
    ]


parameterGen :: Gen IM.IMParameter
parameterGen =
  IM.IMParameter
    <$> Gen.element ["flag", "x", "quality"]
    <*> Gen.maybe (Gen.element [IM.IMParameterToken "DATA", IM.IMParameterQuoted "abc,def", IM.IMParameterQuoted "\"${}\\\xff"])


aimGen :: Gen AIM.AIM
aimGen = AIM.AIM <$> Gen.list (Range.linear 1 5) weightedGen


imGen :: Gen IM.IM
imGen = IM.IMEntries <$> Gen.list (Range.linear 0 5) manipulationGen
  where
    manipulationGen = IM.InstanceManipulation <$> Gen.filter (/= "identity") tokenGen <*> Gen.maybe parameterGen


etagGen :: Gen EntityTag
etagGen = do
  s <- ST.fromString <$> Gen.string (Range.linear 0 12) (Gen.element ("abcdef0123456789" :: String))
  Gen.element [StrongETag s, WeakETag s]


deltaBaseGen :: Gen DB.DeltaBase
deltaBaseGen = DB.DeltaBase <$> etagGen


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "DeltaEncoding" $
    sequence_
      [ unit_aim_parse
      , unit_aim_render
      , unit_aim_fields
      , unit_aim_authored_weights
      , checkedManipulationWrites
      , it "A-IM round-trips" prop_aim_roundtrip
      , unit_im_parse
      , unit_im_render
      , unit_im_fields
      , it "IM round-trips" prop_im_roundtrip
      , unit_deltabase_parse
      , unit_deltabase_parse_weak
      , unit_deltabase_render
      , unit_deltabase_fields
      , it "Delta-Base round-trips" prop_deltabase_roundtrip
      ]
