{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.OData (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower, toUpper)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
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
  it "renders the OData-Version production" $ do
    M.toStrictByteString (OV.renderODataVersion (OV.ODataVersion 4 0)) `shouldBe` "4.0"
    M.toStrictByteString (OV.renderODataVersion (OV.ODataVersion 4 1)) `shouldBe` "4.01"


versionGen :: Gen OV.ODataVersion
versionGen = OV.ODataVersion 4 <$> Gen.word (Range.linear 0 9)


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
    Right (OMV.ODataMaxVersionWithLeadingZeros 4 1 1) -> pure () :: IO ()
    other -> error (show other)


unit_maxversion_render :: Spec
unit_maxversion_render =
  it "renders plain and zero-padded fractions distinctly" $ do
    M.toStrictByteString (OMV.renderODataMaxVersion (OMV.ODataMaxVersion 4 1)) `shouldBe` "4.1"
    M.toStrictByteString (OMV.renderODataMaxVersion (OMV.ODataMaxVersionWithLeadingZeros 4 1 1)) `shouldBe` "4.01"


maxVersionGen :: Gen OMV.ODataMaxVersion
maxVersionGen = do
  major <- Gen.word (Range.linear 0 maxBound)
  minor <- Gen.word (Range.linear 0 maxBound)
  zeros <- Gen.int (Range.linear 0 25)
  pure $
    if zeros == 0
      then OMV.ODataMaxVersion major minor
      else OMV.ODataMaxVersionWithLeadingZeros major minor zeros


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
entityIdGen = do
  prefix <- Gen.element ["", "/", "../", "https://example.com/", "urn:test:"]
  path <- Gen.string (Range.linear 1 30) (Gen.element ("abcdefghijklmnopqrstuvwxyz0123456789.()-_" :: String))
  suffix <- Gen.element ["", "?view=short", "#fragment", "?view=a%20b#fragment"]
  pure (OE.ODataEntityId (ST.fromString (prefix <> path <> suffix)))


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
  Gen.filter ((/= "snapshot") . BSC.map toLower . ST.toByteString) $
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


typedVersionAndIsolation :: Spec
typedVersionAndIsolation = describe "typed version and isolation fields" $ do
  it "round trips the OData-Version grammar through the header map" $
    forM_ [0 .. 9] $ \minor -> do
      let value = OV.ODataVersion 4 minor
          wire = if minor == 0 then "4.0" else "4.0" <> BSC.singleton (toEnum (fromEnum '0' + fromIntegral minor))
      lookupHeader @OV.ODataVersion (headerMapFromList [("OData-Version", "\t " <> wire <> " \t")])
        `shouldBe` Right (Just value)
      lookupRawHeader "OData-Version" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
  it "rejects decimal spellings outside the OData-Version grammar" $
    forM_ ["4.1", "4.00", "4.010", "4.10", "5.0", "04.01", "4.01, 4.0", "4.0\r"] $ \wire ->
      lookupHeader @OV.ODataVersion (headerMapFromList [("OData-Version", wire)]) `shouldSatisfy` isLeft
  it "preserves zero-padded maximum-version fractions including all-zero fractions" $ do
    forM_
      [ ("4.01", OMV.ODataMaxVersionWithLeadingZeros 4 1 1)
      , ("4.1", OMV.ODataMaxVersion 4 1)
      , ("4.0010", OMV.ODataMaxVersionWithLeadingZeros 4 10 2)
      , ("4.000", OMV.ODataMaxVersionWithLeadingZeros 4 0 2)
      ]
      $ \(wire, value) -> do
        lookupHeader @OMV.ODataMaxVersion (headerMapFromList [("OData-MaxVersion", "\t " <> wire <> " \t")])
          `shouldBe` Right (Just value)
        lookupRawHeader "OData-MaxVersion" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
  it "accepts the largest representable maximum version and rejects overflow" $ do
    let limit = M.toStrictByteString (M.wordDec maxBound)
        lookupMax wire = lookupHeader @OMV.ODataMaxVersion (headerMapFromList [("OData-MaxVersion", wire)])
    lookupMax (limit <> "." <> limit) `shouldBe` Right (Just (OMV.ODataMaxVersion maxBound maxBound))
    forM_ [limit <> "0.1", "1." <> limit <> "0", "1.", ".1", "4.01, 4.0", "+4.1", "4. 1"] $ \wire ->
      lookupMax wire `shouldSatisfy` isLeft
  it "accepts every ASCII case spelling of snapshot under both field names" $ do
    forM_ (traverse (\c -> [c, toUpper c]) "snapshot") $ \spelling -> do
      let wire = "\t " <> BSC.pack spelling <> " \t"
          headers = headerMapFromList [("Isolation", wire), ("OData-Isolation", wire)]
      lookupHeader @IS.Isolation headers `shouldBe` Right (Just IS.Snapshot)
      lookupHeader @OI.ODataIsolation headers `shouldBe` Right (Just OI.Snapshot)
    lookupRawHeader "Isolation" (setHeader IS.Snapshot (headerMapFromList [])) `shouldBe` Just ("snapshot" :| [])
    lookupRawHeader "OData-Isolation" (setHeader OI.Snapshot (headerMapFromList [])) `shouldBe` Just ("snapshot" :| [])
  it "preserves unsupported isolation tokens and rejects combined values" $ do
    let fields wire = headerMapFromList [("Isolation", wire), ("OData-Isolation", wire)]
    lookupHeader @IS.Isolation (fields "SnApShOt-v2") `shouldBe` Right (Just (IS.IsolationOther "SnApShOt-v2"))
    lookupHeader @OI.ODataIsolation (fields "SnApShOt-v2") `shouldBe` Right (Just (OI.ODataIsolationOther "SnApShOt-v2"))
    forM_ ["snapshot, snapshot", "\"snapshot\"", "snapshot;", "", "snapshot\xff"] $ \wire -> do
      lookupHeader @IS.Isolation (fields wire) `shouldSatisfy` isLeft
      lookupHeader @OI.ODataIsolation (fields wire) `shouldSatisfy` isLeft
  it "rejects repeated singleton fields" $ do
    let repeated name wire = headerMapFromList [(name, wire), (name, wire)]
    lookupHeader @OV.ODataVersion (repeated "OData-Version" "4.01") `shouldSatisfy` isLeft
    lookupHeader @OMV.ODataMaxVersion (repeated "OData-MaxVersion" "4.01") `shouldSatisfy` isLeft
    lookupHeader @IS.Isolation (repeated "Isolation" "snapshot") `shouldSatisfy` isLeft
    lookupHeader @OI.ODataIsolation (repeated "OData-Isolation" "snapshot") `shouldSatisfy` isLeft


typedEntityId :: Spec
typedEntityId = describe "typed entity references" $ do
  it "preserves absolute and relative URI references including percent escapes"
    $ forM_
      [ "https://example.com/Customers(17)"
      , "https://user@[2001:db8::1]:8443/Customers(17)"
      , "//[v1.example]/Customers(17)"
      , "../Customers('Caf%C3%a9')?x=a%20b#item"
      , "urn:example:customer:17"
      , "?id=17"
      , "#17"
      ]
    $ \wire -> do
      let value = OE.ODataEntityId (ST.fromString (BSC.unpack wire))
      lookupHeader @OE.ODataEntityId (headerMapFromList [("OData-EntityID", "\t " <> wire <> " \t")])
        `shouldBe` Right (Just value)
      lookupRawHeader "OData-EntityId" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
  it "retains raw Unicode on receipt and maps its UTF-8 octets to URI escapes on send"
    $ forM_
      [ ("https://üser@münich.example/Cafés(1)?q=東京#β", "https://%C3%BCser@m%C3%BCnich.example/Caf%C3%A9s(1)?q=%E6%9D%B1%E4%BA%AC#%CE%B2")
      , ("https://example.com/é/%e2%84%a2/😀", "https://example.com/%C3%A9/%e2%84%a2/%F0%9F%98%80")
      , ("é/a?query=\xe000", "%C3%A9/a?query=%EE%80%80")
      ]
    $ \(input, wire) -> do
      let value = OE.ODataEntityId (ST.fromText input)
      lookupHeader @OE.ODataEntityId (headerMapFromList [("OData-EntityId", TE.encodeUtf8 input)])
        `shouldBe` Right (Just value)
      lookupRawHeader "OData-EntityId" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
      M.toStrictByteString ("[" <> OE.renderODataEntityId value <> "]") `shouldBe` ("[" <> wire <> "]")
  it "rejects malformed reference syntax and raw Unicode outside its permitted components" $ do
    let malformed = ["", "\t", "x y", "1:x", "a%", "a%0g", "x#y#z", "https://[bad]/x", "https://host:abc/x", "<urn:example:id>", "x\r"]
        invalidUnicode = fmap TE.encodeUtf8 ["a/\x80", "a/\xfffe", "a/\xe000", "a#\xe000", "https://host:é/x", "https://[é]/x", "aé:path"]
    forM_ (malformed <> invalidUnicode) $ \wire ->
      lookupHeader @OE.ODataEntityId (headerMapFromList [("OData-EntityId", wire)]) `shouldSatisfy` isLeft
  it "rejects malformed UTF-8, leaving percent-encoded octets opaque" $ do
    let invalid = fmap BS.pack [[0xff], [0xc0, 0xaf], [0xc3], [0xc3, 0x25], [0xed, 0xa0, 0x80], [0xf4, 0x90, 0x80, 0x80], [0xf5, 0x80, 0x80, 0x80]]
        lookupEntity wire = lookupHeader @OE.ODataEntityId (headerMapFromList [("OData-EntityId", wire)])
    forM_ invalid $ \bytes -> lookupEntity ("/test/" <> bytes) `shouldSatisfy` isLeft
    lookupEntity "/test/%ff" `shouldBe` Right (Just (OE.ODataEntityId "/test/%ff"))
  it "rejects repeated entity-id lines" $
    lookupHeader @OE.ODataEntityId (headerMapFromList [("OData-EntityId", "Entity(1)"), ("OData-EntityID", "Entity(1)")])
      `shouldSatisfy` isLeft


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
      , typedVersionAndIsolation
      , typedEntityId
      ]
