{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AcceptEncoding (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.ContentCoding as CC
import qualified Network.HTTP.Headers.AcceptEncoding as AE
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String AE.AcceptEncoding
parseOk bs = case runParser AE.acceptEncodingParser bs of
  OK ae leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right ae
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: AE.AcceptEncoding -> ByteString
render = M.toStrictByteString . AE.renderAcceptEncoding


unit_single :: Spec
unit_single = it "single coding" $
  case parseOk "gzip" of
    Right (AE.AcceptEncoding [AE.WeightedEncoding (AE.NamedEncoding CC.GZip) 1]) -> pure () :: IO ()
    other -> error ("unexpected parse: " <> show other)


-- Regression for the wireform-core switch bug fixed alongside
-- this change: a switch-literal that is both a terminal /and/ a
-- prefix of a longer literal used to consume one byte past the
-- terminal before falling back to the terminal action. That
-- broke every quality-weighted Accept-* parser whose first
-- entry's @q=1.0@ ended at a separator (the parser would eat
-- the comma too, then refuse to continue).
unit_q1_followed_by_more :: Spec
unit_q1_followed_by_more = it "q=1.0 followed by another entry" $
  case parseOk "br;q=1.0, gzip;q=0.5, *;q=0" of
    Right (AE.AcceptEncoding xs) ->
      length xs `shouldBe` 3
    other -> error (show other)


unit_multi_with_q :: Spec
unit_multi_with_q = it "multi-coding with q values" $
  case parseOk "br;q=1.0, gzip;q=0.5, *;q=0" of
    Right (AE.AcceptEncoding [b, g, w]) -> do
      AE.encodingTag b `shouldBe` AE.NamedEncoding CC.Brotli
      AE.encodingWeight b `shouldBe` 1
      AE.encodingTag g `shouldBe` AE.NamedEncoding CC.GZip
      AE.encodingWeight g `shouldBe` 0.5
      AE.encodingTag w `shouldBe` AE.AnyEncoding
      AE.encodingWeight w `shouldBe` 0
    other -> error ("unexpected parse: " <> show other)


unit_render_omits_q1 :: Spec
unit_render_omits_q1 =
  it "renderer omits q=1.0" $
    let v =
          AE.AcceptEncoding
            [ AE.WeightedEncoding (AE.NamedEncoding CC.GZip) 1
            , AE.WeightedEncoding (AE.NamedEncoding CC.Brotli) 0.8
            ]
    in render v `shouldBe` "gzip, br;q=0.8"


-- ---------------------------------------------------------------------------
-- Property: render → parse round-trip
-- ---------------------------------------------------------------------------

codingGen :: Gen CC.ContentCoding
codingGen = Gen.element [CC.GZip, CC.Brotli, CC.Deflate, CC.ZStd, CC.Identity]


tagGen :: Gen AE.EncodingTag
tagGen =
  Gen.choice
    [ pure AE.AnyEncoding
    , AE.NamedEncoding <$> codingGen
    ]


-- Stick to the discrete weights the parser actually distinguishes
-- (0, 0.1, 0.5, 0.999, 1) so the round-trip doesn't get tripped
-- up by floating-point representation drift.
weightGen :: Gen Double
weightGen = Gen.element [0, 0.1, 0.5, 0.999, 1]


wencGen :: Gen AE.WeightedEncoding
wencGen = AE.WeightedEncoding <$> tagGen <*> weightGen


prop_roundtrip :: Property
prop_roundtrip = property $ do
  ws <- forAll (Gen.list (Range.linear 1 5) wencGen)
  let bs = render (AE.AcceptEncoding ws)
  case parseOk bs of
    Right (AE.AcceptEncoding ws') -> ws === ws'
    Left err -> error ("round-trip failed: " <> err <> " on " <> show bs)


tests :: Spec
tests =
  describe "AcceptEncoding" $
    sequence_
      [ unit_single
      , unit_q1_followed_by_more
      , unit_multi_with_q
      , unit_render_omits_q1
      , it "round-trip" prop_roundtrip
      ]
