{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.OData (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Isolation as IS
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.ODataEntityId as OE
import qualified Network.HTTP.Headers.ODataIsolation as OI
import qualified Network.HTTP.Headers.ODataMaxVersion as OMV
import qualified Network.HTTP.Headers.ODataVersion as OV
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ParserT () String a -> ByteString -> Either String a
parseOk p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


-- ---------------------------------------------------------------------------
-- OData-Version
-- ---------------------------------------------------------------------------

unit_version_parse :: Spec
unit_version_parse = it "parses 4.0" $
  case parseOk OV.oDataVersionParser "4.0" of
    Right (OV.ODataVersion 4 0) -> pure () :: IO ()
    other -> error (show other)


unit_version_parse_401 :: Spec
unit_version_parse_401 = it "parses 4.01" $
  case parseOk OV.oDataVersionParser "4.01" of
    Right (OV.ODataVersion 4 1) -> pure () :: IO ()
    other -> error (show other)


unit_version_render :: Spec
unit_version_render =
  it "renders major.minor" $
    M.toStrictByteString (OV.renderODataVersion (OV.ODataVersion 4 0)) `shouldBe` "4.0"


versionGen :: Gen OV.ODataVersion
versionGen =
  OV.ODataVersion
    <$> Gen.word (Range.linear 0 1_000)
    <*> Gen.word (Range.linear 0 1_000)


prop_version_roundtrip :: Property
prop_version_roundtrip = property $ do
  v <- forAll versionGen
  let bs = M.toStrictByteString (OV.renderODataVersion v)
  case parseOk OV.oDataVersionParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- OData-MaxVersion
-- ---------------------------------------------------------------------------

unit_maxversion_parse :: Spec
unit_maxversion_parse = it "parses 4.01" $
  case parseOk OMV.oDataMaxVersionParser "4.01" of
    Right (OMV.ODataMaxVersion 4 1) -> pure () :: IO ()
    other -> error (show other)


unit_maxversion_render :: Spec
unit_maxversion_render =
  it "renders major.minor" $
    M.toStrictByteString (OMV.renderODataMaxVersion (OMV.ODataMaxVersion 4 1)) `shouldBe` "4.1"


maxVersionGen :: Gen OMV.ODataMaxVersion
maxVersionGen =
  OMV.ODataMaxVersion
    <$> Gen.word (Range.linear 0 1_000)
    <*> Gen.word (Range.linear 0 1_000)


prop_maxversion_roundtrip :: Property
prop_maxversion_roundtrip = property $ do
  v <- forAll maxVersionGen
  let bs = M.toStrictByteString (OMV.renderODataMaxVersion v)
  case parseOk OMV.oDataMaxVersionParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- OData-EntityId
-- ---------------------------------------------------------------------------

unit_entityid_parse :: Spec
unit_entityid_parse = it "preserves the entity-id IRI" $
  case parseOk OE.oDataEntityIdParser "https://host/svc/Customers(1)" of
    Right (OE.ODataEntityId iri) ->
      iri `shouldBe` ST.fromString "https://host/svc/Customers(1)"
    other -> error (show other)


unit_entityid_render :: Spec
unit_entityid_render =
  it "renders the raw IRI" $
    M.toStrictByteString (OE.renderODataEntityId (OE.ODataEntityId (ST.fromString "https://host/svc/E(1)")))
      `shouldBe` "https://host/svc/E(1)"


entityIdGen :: Gen OE.ODataEntityId
entityIdGen =
  OE.ODataEntityId . ST.fromString
    <$> Gen.string (Range.linear 1 30) (Gen.element ("abcdefghijklmnopqrstuvwxyz0123456789:/.()-_" :: String))


prop_entityid_roundtrip :: Property
prop_entityid_roundtrip = property $ do
  v <- forAll entityIdGen
  let bs = M.toStrictByteString (OE.renderODataEntityId v)
  case parseOk OE.oDataEntityIdParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- OData-Isolation
-- ---------------------------------------------------------------------------

unit_odisolation_parse :: Spec
unit_odisolation_parse = it "parses snapshot" $
  case parseOk OI.oDataIsolationParser "snapshot" of
    Right OI.Snapshot -> pure () :: IO ()
    other -> error (show other)


unit_odisolation_render :: Spec
unit_odisolation_render = it "renders snapshot" $ do
  M.toStrictByteString (OI.renderODataIsolation OI.Snapshot) `shouldBe` "snapshot"
  M.toStrictByteString (OI.renderODataIsolation (OI.ODataIsolationOther (ST.fromString "future")))
    `shouldBe` "future"


tokenGen :: Gen ST.ShortText
tokenGen =
  Gen.filter (/= ST.fromString "snapshot") $
    ST.fromString <$> Gen.string (Range.linear 1 12) Gen.alpha


odIsolationGen :: Gen OI.ODataIsolation
odIsolationGen =
  Gen.choice [pure OI.Snapshot, OI.ODataIsolationOther <$> tokenGen]


prop_odisolation_roundtrip :: Property
prop_odisolation_roundtrip = property $ do
  v <- forAll odIsolationGen
  let bs = M.toStrictByteString (OI.renderODataIsolation v)
  case parseOk OI.oDataIsolationParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Isolation
-- ---------------------------------------------------------------------------

unit_isolation_parse :: Spec
unit_isolation_parse = it "parses snapshot" $
  case parseOk IS.isolationParser "snapshot" of
    Right IS.Snapshot -> pure () :: IO ()
    other -> error (show other)


unit_isolation_render :: Spec
unit_isolation_render = it "renders snapshot" $ do
  M.toStrictByteString (IS.renderIsolation IS.Snapshot) `shouldBe` "snapshot"
  M.toStrictByteString (IS.renderIsolation (IS.IsolationOther (ST.fromString "future")))
    `shouldBe` "future"


isolationGen :: Gen IS.Isolation
isolationGen =
  Gen.choice [pure IS.Snapshot, IS.IsolationOther <$> tokenGen]


prop_isolation_roundtrip :: Property
prop_isolation_roundtrip = property $ do
  v <- forAll isolationGen
  let bs = M.toStrictByteString (IS.renderIsolation v)
  case parseOk IS.isolationParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "OData" $
    sequence_
      [ unit_version_parse
      , unit_version_parse_401
      , unit_version_render
      , it "OData-Version round-trip" prop_version_roundtrip
      , unit_maxversion_parse
      , unit_maxversion_render
      , it "OData-MaxVersion round-trip" prop_maxversion_roundtrip
      , unit_entityid_parse
      , unit_entityid_render
      , it "OData-EntityId round-trip" prop_entityid_roundtrip
      , unit_odisolation_parse
      , unit_odisolation_render
      , it "OData-Isolation round-trip" prop_odisolation_roundtrip
      , unit_isolation_parse
      , unit_isolation_render
      , it "Isolation round-trip" prop_isolation_roundtrip
      ]
