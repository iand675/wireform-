{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteContent (tests) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ContentBase as CB
import qualified Network.HTTP.Headers.ContentID as CID
import qualified Network.HTTP.Headers.ContentScriptType as CST
import qualified Network.HTTP.Headers.ContentStyleType as CStyT
import qualified Network.HTTP.Headers.ContentVersion as CV
import qualified Network.HTTP.Headers.DefaultStyle as DS
import qualified Network.HTTP.Headers.DerivedFrom as DF
import qualified Network.HTTP.Headers.DifferentialID as DID
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.URI as U
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser and require the whole value to be consumed.
runOk :: ParserT st String a -> ByteString -> Either String a
runOk p bs = case runParser p bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


-- ---------------------------------------------------------------------------
-- Content-Base
-- ---------------------------------------------------------------------------

unit_content_base_parse :: Spec
unit_content_base_parse = it "parses Content-Base absolute URI" $
  case runOk CB.contentBaseParser "http://example.com/base/" of
    Right (CB.ContentBase u) -> u `shouldBe` ST.fromString "http://example.com/base/"
    other -> error (show other)


unit_content_base_render :: Spec
unit_content_base_render =
  it "renders Content-Base" $
    M.toStrictByteString (CB.renderContentBase (CB.ContentBase (ST.fromString "http://example.com/base/")))
      `shouldBe` "http://example.com/base/"


-- ---------------------------------------------------------------------------
-- Content-Script-Type
-- ---------------------------------------------------------------------------

unit_content_script_type_parse :: Spec
unit_content_script_type_parse = it "parses Content-Script-Type media type" $
  case runOk CST.contentScriptTypeParser "text/javascript" of
    Right (CST.ContentScriptType t) -> t `shouldBe` ST.fromString "text/javascript"
    other -> error (show other)


unit_content_script_type_render :: Spec
unit_content_script_type_render =
  it "renders Content-Script-Type" $
    M.toStrictByteString (CST.renderContentScriptType (CST.ContentScriptType (ST.fromString "text/tcl")))
      `shouldBe` "text/tcl"


-- ---------------------------------------------------------------------------
-- Content-Style-Type
-- ---------------------------------------------------------------------------

unit_content_style_type_parse :: Spec
unit_content_style_type_parse = it "parses Content-Style-Type media type" $
  case runOk CStyT.contentStyleTypeParser "text/css" of
    Right (CStyT.ContentStyleType t) -> t `shouldBe` ST.fromString "text/css"
    other -> error (show other)


unit_content_style_type_render :: Spec
unit_content_style_type_render =
  it "renders Content-Style-Type" $
    M.toStrictByteString (CStyT.renderContentStyleType (CStyT.ContentStyleType (ST.fromString "text/css")))
      `shouldBe` "text/css"


-- ---------------------------------------------------------------------------
-- Content-Version
-- ---------------------------------------------------------------------------

unit_content_version_parse :: Spec
unit_content_version_parse = it "parses Content-Version quoted-string" $
  case runOk CV.contentVersionParser "\"Fred 19950116-12:26:48\"" of
    Right (CV.ContentVersion t) -> t `shouldBe` ST.fromString "Fred 19950116-12:26:48"
    other -> error (show other)


unit_content_version_render :: Spec
unit_content_version_render =
  it "renders Content-Version as quoted-string" $
    M.toStrictByteString (CV.renderContentVersion (CV.ContentVersion (ST.fromString "2.1.2")))
      `shouldBe` "\"2.1.2\""


-- ---------------------------------------------------------------------------
-- Default-Style
-- ---------------------------------------------------------------------------

unit_default_style_parse :: Spec
unit_default_style_parse = it "parses Default-Style title" $
  case runOk DS.defaultStyleParser "compact" of
    Right (DS.DefaultStyle t) -> t `shouldBe` ST.fromString "compact"
    other -> error (show other)


unit_default_style_render :: Spec
unit_default_style_render =
  it "renders Default-Style" $
    M.toStrictByteString (DS.renderDefaultStyle (DS.DefaultStyle (ST.fromString "compact")))
      `shouldBe` "compact"


-- ---------------------------------------------------------------------------
-- Derived-From
-- ---------------------------------------------------------------------------

unit_derived_from_parse :: Spec
unit_derived_from_parse = it "parses Derived-From quoted-string" $
  case runOk DF.derivedFromParser "\"2.1.1\"" of
    Right (DF.DerivedFrom t) -> t `shouldBe` ST.fromString "2.1.1"
    other -> error (show other)


unit_derived_from_render :: Spec
unit_derived_from_render =
  it "renders Derived-From as quoted-string" $
    M.toStrictByteString (DF.renderDerivedFrom (DF.DerivedFrom (ST.fromString "2.1.1")))
      `shouldBe` "\"2.1.1\""


-- ---------------------------------------------------------------------------
-- Content-ID
-- ---------------------------------------------------------------------------

unit_content_id_parse :: Spec
unit_content_id_parse = it "parses Content-ID checksum URN" $
  case runOk CID.contentIDParser "urn:md5:PEFjWBDv/sd9alS9BYuX0w==" of
    Right (CID.ContentID t) -> t `shouldBe` ST.fromString "urn:md5:PEFjWBDv/sd9alS9BYuX0w=="
    other -> error (show other)


unit_content_id_render :: Spec
unit_content_id_render =
  it "renders Content-ID" $
    M.toStrictByteString (CID.renderContentID (CID.ContentID (ST.fromString "urn:md5:PEFjWBDv/sd9alS9BYuX0w==")))
      `shouldBe` "urn:md5:PEFjWBDv/sd9alS9BYuX0w=="


-- ---------------------------------------------------------------------------
-- Differential-ID
-- ---------------------------------------------------------------------------

unit_differential_id_parse :: Spec
unit_differential_id_parse = it "parses Differential-ID checksum URN" $
  case runOk DID.differentialIDParser "urn:md5:3FS2oCnWPZptpN05oKBemA==" of
    Right (DID.DifferentialID t) -> t `shouldBe` ST.fromString "urn:md5:3FS2oCnWPZptpN05oKBemA=="
    other -> error (show other)


unit_differential_id_render :: Spec
unit_differential_id_render =
  it "renders Differential-ID" $
    M.toStrictByteString (DID.renderDifferentialID (DID.DifferentialID (ST.fromString "urn:md5:3FS2oCnWPZptpN05oKBemA==")))
      `shouldBe` "urn:md5:3FS2oCnWPZptpN05oKBemA=="


-- ---------------------------------------------------------------------------
-- URI
-- ---------------------------------------------------------------------------

unit_uri_parse :: Spec
unit_uri_parse = it "parses URI angle-bracketed list" $
  case runOk U.uriParser "<http://example.com/a>, <http://example.com/b>" of
    Right (U.URI (a :| [b])) -> do
      a `shouldBe` ST.fromString "http://example.com/a"
      b `shouldBe` ST.fromString "http://example.com/b"
    other -> error (show other)


unit_uri_render :: Spec
unit_uri_render =
  it "renders URI list" $
    let v = U.URI (ST.fromString "http://example.com/a" :| [ST.fromString "http://example.com/b"])
    in M.toStrictByteString (U.renderURI v) `shouldBe` "<http://example.com/a>, <http://example.com/b>"


-- ---------------------------------------------------------------------------
-- Round-trip properties (structured headers)
-- ---------------------------------------------------------------------------

-- | Tag contents within the quoted-string grammar, free of DQUOTE/backslash.
tagGen :: Gen ST.ShortText
tagGen = ST.fromString <$> Gen.string (Range.linear 0 16) (Gen.element ("abcABCxyz0123 .:-" :: String))


-- | URI element: no angle brackets, comma, or whitespace so it stays unambiguous.
uriPartGen :: Gen ST.ShortText
uriPartGen = ST.fromString <$> Gen.string (Range.linear 1 16) (Gen.element ("abcABC0123:/._-~" :: String))


uriGen :: Gen U.URI
uriGen = do
  x <- uriPartGen
  xs <- Gen.list (Range.linear 0 3) uriPartGen
  pure $ U.URI (x :| xs)


prop_content_version :: Property
prop_content_version = property $ do
  v <- forAll (CV.ContentVersion <$> tagGen)
  runOk CV.contentVersionParser (M.toStrictByteString (CV.renderContentVersion v)) === Right v


prop_derived_from :: Property
prop_derived_from = property $ do
  v <- forAll (DF.DerivedFrom <$> tagGen)
  runOk DF.derivedFromParser (M.toStrictByteString (DF.renderDerivedFrom v)) === Right v


prop_uri :: Property
prop_uri = property $ do
  v <- forAll uriGen
  runOk U.uriParser (M.toStrictByteString (U.renderURI v)) === Right v


tests :: Spec
tests =
  describe "ObsoleteContent" $
    sequence_
      [ unit_content_base_parse
      , unit_content_base_render
      , unit_content_script_type_parse
      , unit_content_script_type_render
      , unit_content_style_type_parse
      , unit_content_style_type_render
      , unit_content_version_parse
      , unit_content_version_render
      , unit_default_style_parse
      , unit_default_style_render
      , unit_derived_from_parse
      , unit_derived_from_render
      , unit_content_id_parse
      , unit_content_id_render
      , unit_differential_id_parse
      , unit_differential_id_render
      , unit_uri_parse
      , unit_uri_render
      , it "Content-Version round-trips" prop_content_version
      , it "Derived-From round-trips" prop_derived_from
      , it "URI round-trips" prop_uri
      ]
