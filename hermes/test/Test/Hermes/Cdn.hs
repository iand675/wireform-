{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Cdn (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Word (Word8)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.CDNCacheControl as CCC
import qualified Network.HTTP.Headers.CDNLoop as CL
import qualified Network.HTTP.Headers.CacheControl as CC
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


isWs :: Word8 -> Bool
isWs w = w == 0x20 || w == 0x09


parseCC :: ByteString -> Either String CCC.CDNCacheControl
parseCC bs = case runParser CCC.cdnCacheControlParser bs of
  OK v rest
    | BS.null (BS.dropWhile isWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


parseCL :: ByteString -> Either String CL.CDNLoop
parseCL bs = case runParser CL.cdnLoopParser bs of
  OK v rest
    | BS.null (BS.dropWhile isWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCC :: CCC.CDNCacheControl -> ByteString
renderCC = M.toStrictByteString . CCC.renderCDNCacheControl


renderCL :: CL.CDNLoop -> ByteString
renderCL = M.toStrictByteString . CL.renderCDNLoop


-- CDN-Cache-Control -----------------------------------------------------------

unit_cc_parse :: Spec
unit_cc_parse = it "parses public, max-age" $
  case parseCC "public, max-age=600" of
    Right (CCC.CDNCacheControl (CC.Public :| [CC.MaxAge 600])) -> pure () :: IO ()
    other -> error (show other)


unit_cc_render :: Spec
unit_cc_render =
  it "renders directive list" $
    let v = CCC.CDNCacheControl (CC.NoStore :| [CC.MaxAge 0])
    in renderCC v `shouldBe` "no-store, max-age=0"


genWord :: Gen Word
genWord = Gen.word (Range.linear 0 1_000_000)


genDirective :: Gen CC.CacheControlDirective
genDirective =
  Gen.choice
    [ pure CC.Public
    , pure CC.NoStore
    , pure CC.MustRevalidate
    , pure CC.ProxyRevalidate
    , pure CC.NoTransform
    , pure CC.Immutable
    , CC.MaxAge <$> genWord
    , CC.SMaxAge <$> genWord
    , CC.StaleWhileRevalidate <$> genWord
    , CC.StaleIfError <$> genWord
    ]


prop_cc_roundtrip :: Property
prop_cc_roundtrip = property $ do
  ds <- forAll (genNonEmpty genDirective)
  let v = CCC.CDNCacheControl ds
      bs = renderCC v
  case parseCC bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- CDN-Loop --------------------------------------------------------------------

unit_cl_parse :: Spec
unit_cl_parse = it "parses cdn-info entries with parameters" $
  case parseCL "foo123.foocdn.example, barcdn.example; trace=abc" of
    Right (CL.CDNLoop (a :| [b])) -> do
      CL.cdnInfoId a `shouldBe` ST.fromString "foo123.foocdn.example"
      CL.cdnInfoParameters a `shouldBe` []
      CL.cdnInfoId b `shouldBe` ST.fromString "barcdn.example"
      CL.cdnInfoParameters b `shouldBe` [(ST.fromString "trace", ST.fromString "abc")]
    other -> error (show other)


unit_cl_render :: Spec
unit_cl_render =
  it "renders cdn-info list" $
    let v =
          CL.CDNLoop
            ( CL.CDNInfo (ST.fromString "alpha.example") []
                :| [CL.CDNInfo (ST.fromString "beta.example") [(ST.fromString "k", ST.fromString "v")]]
            )
    in renderCL v `shouldBe` "alpha.example, beta.example;k=v"


genToken :: Gen ST.ShortText
genToken = ST.fromString <$> Gen.string (Range.linear 1 12) tokenChar
  where
    tokenChar = Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-.")


genCDNInfo :: Gen CL.CDNInfo
genCDNInfo =
  CL.CDNInfo
    <$> genToken
    <*> Gen.list (Range.linear 0 3) ((,) <$> genToken <*> genToken)


prop_cl_roundtrip :: Property
prop_cl_roundtrip = property $ do
  infos <- forAll (genNonEmpty genCDNInfo)
  let v = CL.CDNLoop infos
      bs = renderCL v
  case parseCL bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Helpers ---------------------------------------------------------------------

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = (:|) <$> g <*> Gen.list (Range.linear 0 4) g


tests :: Spec
tests =
  describe "Cdn" $
    sequence_
      [ unit_cc_parse
      , unit_cc_render
      , it "CDN-Cache-Control round-trip" prop_cc_roundtrip
      , unit_cl_parse
      , unit_cl_render
      , it "CDN-Loop round-trip" prop_cl_roundtrip
      ]
