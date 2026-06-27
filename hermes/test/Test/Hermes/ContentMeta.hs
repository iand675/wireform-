{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ContentMeta (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ContentLanguage as CL
import qualified Network.HTTP.Headers.ContentLocation as CLoc
import qualified Network.HTTP.Headers.MIMEVersion as MV
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseCL :: ByteString -> Either String CL.ContentLanguage
parseCL bs = case runParser CL.contentLanguageParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseCLoc :: ByteString -> Either String CLoc.ContentLocation
parseCLoc bs = case runParser CLoc.contentLocationParser bs of
  OK v "" -> Right v
  OK _ leftover -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseMV :: ByteString -> Either String MV.MIMEVersion
parseMV bs = case runParser MV.mimeVersionParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCL :: CL.ContentLanguage -> ByteString
renderCL = M.toStrictByteString . CL.renderContentLanguage


renderCLoc :: CLoc.ContentLocation -> ByteString
renderCLoc = M.toStrictByteString . CLoc.renderContentLocation


renderMV :: MV.MIMEVersion -> ByteString
renderMV = M.toStrictByteString . MV.renderMIMEVersion


-- Content-Language ----------------------------------------------------------

unit_cl_parse :: Spec
unit_cl_parse = it "parses a language-tag list" $
  case parseCL "en-US, fr" of
    Right (CL.ContentLanguage (a :| [b])) -> do
      CL.unLanguageTag a `shouldBe` ST.fromString "en-US"
      CL.unLanguageTag b `shouldBe` ST.fromString "fr"
    other -> error (show other)


unit_cl_render :: Spec
unit_cl_render =
  it "renders a language-tag list" $
    let v =
          CL.ContentLanguage
            (CL.LanguageTag (ST.fromString "de") :| [CL.LanguageTag (ST.fromString "en")])
    in renderCL v `shouldBe` "de, en"


genSubtag :: Gen String
genSubtag = Gen.list (Range.constant 1 8) (Gen.element ['a' .. 'z'])


genTag :: Gen CL.LanguageTag
genTag = do
  subtags <- Gen.list (Range.constant 1 3) genSubtag
  pure (CL.LanguageTag (ST.fromString (intercalate "-" subtags)))


prop_cl_roundtrip :: Property
prop_cl_roundtrip = property $ do
  tags <- forAll (Gen.nonEmpty (Range.constant 1 4) genTag)
  let v = CL.ContentLanguage tags
      bs = renderCL v
  case parseCL bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Content-Location ----------------------------------------------------------

unit_cloc_parse :: Spec
unit_cloc_parse = it "parses an opaque URI reference" $
  case parseCLoc "/index.html" of
    Right (CLoc.ContentLocation u) -> u `shouldBe` ST.fromString "/index.html"
    other -> error (show other)


unit_cloc_render :: Spec
unit_cloc_render =
  it "renders an opaque URI reference" $
    renderCLoc (CLoc.ContentLocation (ST.fromString "https://example.com/a"))
      `shouldBe` "https://example.com/a"


-- MIME-Version --------------------------------------------------------------

unit_mv_parse :: Spec
unit_mv_parse = it "parses major.minor" $
  case parseMV "1.0" of
    Right (MV.MIMEVersion 1 0) -> pure () :: IO ()
    other -> error (show other)


unit_mv_render :: Spec
unit_mv_render =
  it "renders major.minor" $
    renderMV (MV.MIMEVersion 1 0) `shouldBe` "1.0"


prop_mv_roundtrip :: Property
prop_mv_roundtrip = property $ do
  major <- forAll (Gen.int (Range.constant 0 99))
  minor <- forAll (Gen.int (Range.constant 0 99))
  let v = MV.MIMEVersion major minor
      bs = renderMV v
  case parseMV bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "ContentMeta" $
    sequence_
      [ unit_cl_parse
      , unit_cl_render
      , it "Content-Language round-trip" prop_cl_roundtrip
      , unit_cloc_parse
      , unit_cloc_render
      , unit_mv_parse
      , unit_mv_render
      , it "MIME-Version round-trip" prop_mv_roundtrip
      ]
