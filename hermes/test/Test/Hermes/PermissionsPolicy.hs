{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.PermissionsPolicy (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.PermissionsPolicy as PP
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String PP.PermissionsPolicy
parseOk bs = case runParser PP.permissionsPolicyParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: PP.PermissionsPolicy -> ByteString
render = M.toStrictByteString . PP.renderPermissionsPolicy


unit_parse :: Spec
unit_parse = it "parses a mixed feature dictionary" $
  case parseOk "geolocation=(self \"https://example.com\"), camera=(), fullscreen=*" of
    Right (PP.PermissionsPolicy ds) ->
      ds
        `shouldBe` [ ("geolocation", [PP.AllowSelf, PP.AllowOrigin "https://example.com"])
                   , ("camera", [])
                   , ("fullscreen", [PP.AllowAll])
                   ]
    other -> error (show other)


unit_parse_src :: Spec
unit_parse_src = it "parses the src keyword inside an inner-list" $
  case parseOk "geolocation=(self src)" of
    Right (PP.PermissionsPolicy ds) ->
      ds `shouldBe` [("geolocation", [PP.AllowSelf, PP.AllowSrc])]
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "renders features as inner-lists separated by commas" $
    let v =
          PP.PermissionsPolicy
            [ ("geolocation", [PP.AllowSelf])
            , ("camera", [])
            ]
    in render v `shouldBe` "geolocation=(self), camera=()"


unit_render_origin :: Spec
unit_render_origin =
  it "quotes serialized origins on render" $
    let v = PP.PermissionsPolicy [("fullscreen", [PP.AllowAll, PP.AllowOrigin "https://a.test"])]
    in render v `shouldBe` "fullscreen=(* \"https://a.test\")"


-- Property: any well-formed dictionary round-trips through render/parse.
originGen :: Gen ST.ShortText
originGen =
  Gen.element
    [ "https://example.com"
    , "https://a.test"
    , "http://localhost:8080"
    , "https://cdn.example.org"
    ]


itemGen :: Gen PP.AllowListItem
itemGen =
  Gen.choice
    [ pure PP.AllowAll
    , pure PP.AllowSelf
    , pure PP.AllowSrc
    , PP.AllowOrigin <$> originGen
    ]


featureGen :: Gen ST.ShortText
featureGen = ST.fromString <$> Gen.string (Range.linear 3 10) Gen.lower


directiveGen :: Gen (ST.ShortText, [PP.AllowListItem])
directiveGen = (,) <$> featureGen <*> Gen.list (Range.linear 0 3) itemGen


ppGen :: Gen PP.PermissionsPolicy
ppGen = PP.PermissionsPolicy <$> Gen.list (Range.linear 1 4) directiveGen


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll ppGen
  let bs = render v
  case parseOk bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "PermissionsPolicy" $
    sequence_
      [ unit_parse
      , unit_parse_src
      , unit_render
      , unit_render_origin
      , it "round-trips" prop_roundtrip
      ]
