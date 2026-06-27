{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DeltaEncoding (tests) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen =
  ST.fromString
    <$> Gen.element
      ["vcdiff", "diffe", "gzip", "deflate", "gdiff", "range", "identity"]


weightedGen :: Gen AIM.WeightedIM
weightedGen = AIM.WeightedIM <$> tokenGen <*> Gen.element [1.0, 0.5, 0.9, 0.1]


aimGen :: Gen AIM.AIM
aimGen = AIM.AIM <$> Gen.list (Range.linear 1 5) weightedGen


imGen :: Gen IM.IM
imGen = IM.IM <$> Gen.list (Range.linear 1 5) tokenGen


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
      , it "A-IM round-trips" prop_aim_roundtrip
      , unit_im_parse
      , unit_im_render
      , it "IM round-trips" prop_im_roundtrip
      , unit_deltabase_parse
      , unit_deltabase_parse_weak
      , unit_deltabase_render
      , it "Delta-Base round-trips" prop_deltabase_roundtrip
      ]
