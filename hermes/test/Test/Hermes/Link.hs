{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for "Network.HTTP.Headers.Link" (RFC 8288 Web Linking).

Covers: parsing a single link-value with a quoted param, a
comma-separated list of link-values, bare-token and valueless
params, renderer output (including quoted-string escaping of @\"@),
and a render → parse round-trip property over generated values.
-}
module Test.Hermes.Link (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Link as L
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOk :: ByteString -> Either String L.Link
parseOk bs = case runParser L.linkParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: L.Link -> ByteString
render = M.toStrictByteString . L.renderLink


st :: String -> ST.ShortText
st = ST.pack


-- ---------------------------------------------------------------------------
-- Unit tests
-- ---------------------------------------------------------------------------

unit_quoted_rel :: Spec
unit_quoted_rel = it "single link-value with quoted rel param" $
  case parseOk "</foo>; rel=\"next\"" of
    Right (L.Link [L.LinkValue tgt [L.LinkParam name val]]) -> do
      tgt `shouldBe` st "/foo"
      name `shouldBe` st "rel"
      val `shouldBe` Just (L.LinkParamQuoted (st "next"))
    other -> error (show other)


unit_multi_value :: Spec
unit_multi_value = it "comma-separated list of link-values" $
  case parseOk "</a>; rel=\"prev\", </b>; rel=\"next\"" of
    Right (L.Link [a, b]) -> do
      L.linkTarget a `shouldBe` st "/a"
      L.linkTarget b `shouldBe` st "/b"
    other -> error (show other)


unit_token_and_bare :: Spec
unit_token_and_bare = it "bare token value and a valueless param" $
  case parseOk "</x>; rel=next; nofollow" of
    Right (L.Link [L.LinkValue tgt [p1, p2]]) -> do
      tgt `shouldBe` st "/x"
      L.linkParamName p1 `shouldBe` st "rel"
      L.linkParamValue p1 `shouldBe` Just (L.LinkParamToken (st "next"))
      L.linkParamName p2 `shouldBe` st "nofollow"
      L.linkParamValue p2 `shouldBe` Nothing
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "render link-value with quoted param" $
    let v =
          L.Link
            [ L.LinkValue
                (st "/foo")
                [L.LinkParam (st "rel") (Just (L.LinkParamQuoted (st "next")))]
            ]
    in render v `shouldBe` "</foo>; rel=\"next\""


unit_render_multi :: Spec
unit_render_multi =
  it "render multiple link-values with token params" $
    let v =
          L.Link
            [ L.LinkValue (st "/a") [L.LinkParam (st "rel") (Just (L.LinkParamToken (st "prev")))]
            , L.LinkValue (st "/b") [L.LinkParam (st "rel") (Just (L.LinkParamToken (st "next")))]
            ]
    in render v `shouldBe` "</a>; rel=prev, </b>; rel=next"


unit_quote_escape :: Spec
unit_quote_escape = it "renderer escapes DQUOTE in a quoted value and round-trips" $ do
  let v =
        L.Link
          [ L.LinkValue
              (st "/x")
              [L.LinkParam (st "title") (Just (L.LinkParamQuoted (st "a\"b")))]
          ]
  render v `shouldBe` "</x>; title=\"a\\\"b\""
  case parseOk (render v) of
    Right v' -> v' `shouldBe` v
    other -> error (show other)


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

tokenChar :: Gen Char
tokenChar = Gen.frequency [(52, Gen.alpha), (10, Gen.digit)]


tokenText :: Gen ST.ShortText
tokenText = ST.pack <$> Gen.list (Range.linear 1 10) tokenChar


-- URI-Reference chars that round-trip: URL-safe, never @<@\/@>@ or whitespace.
targetChar :: Gen Char
targetChar =
  Gen.frequency
    [ (40, Gen.alphaNum)
    , (10, Gen.element ("/.-_~:?=&%#@" :: String))
    ]


targetText :: Gen ST.ShortText
targetText = ST.pack <$> Gen.list (Range.linear 1 20) targetChar


-- Printable ASCII minus DQUOTE\/backslash, so the renderer's escape
-- logic isn't part of the round-trip equality (covered by a unit test).
quotedChar :: Gen Char
quotedChar = Gen.element [c | c <- [' ' .. '~'], c /= '"', c /= '\\']


quotedText :: Gen ST.ShortText
quotedText = ST.pack <$> Gen.list (Range.linear 0 20) quotedChar


paramGen :: Gen L.LinkParam
paramGen = do
  name <- tokenText
  val <-
    Gen.choice
      [ pure Nothing
      , Just . L.LinkParamToken <$> tokenText
      , Just . L.LinkParamQuoted <$> quotedText
      ]
  pure (L.LinkParam name val)


linkValueGen :: Gen L.LinkValue
linkValueGen = L.LinkValue <$> targetText <*> Gen.list (Range.linear 0 4) paramGen


-- ---------------------------------------------------------------------------
-- Property: render → parse round-trip
-- ---------------------------------------------------------------------------

prop_roundtrip :: Property
prop_roundtrip = property $ do
  vs <- forAll (Gen.list (Range.linear 1 4) linkValueGen)
  let l = L.Link vs
      bs = render l
  case parseOk bs of
    Right l' -> l' === l
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Link" $
    sequence_
      [ unit_quoted_rel
      , unit_multi_value
      , unit_token_and_bare
      , unit_render
      , unit_render_multi
      , unit_quote_escape
      , it "render/parse round-trips" prop_roundtrip
      ]
