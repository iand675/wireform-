{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Hermes.MiscProvisional (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.AMPCacheTransform as AMP
import qualified Network.HTTP.Headers.ConfigurationContext as CC
import qualified Network.HTTP.Headers.EDIINTFeatures as ED
import qualified Network.HTTP.Headers.HTTP2Settings as H2
import Network.HTTP.Headers.HeaderFieldName (hAMPCacheTransform)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OSLCCoreVersion as OCV
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers ---------------------------------------------------------------------

trimmed :: ByteString -> Bool
trimmed = BS.null . BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseAMP :: ByteString -> Either String AMP.AMPCacheTransform
parseAMP bs = case runParser AMP.ampCacheTransformParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseCC :: ByteString -> Either String CC.ConfigurationContext
parseCC bs = case runParser CC.configurationContextParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseED :: ByteString -> Either String ED.EDIINTFeatures
parseED bs = case runParser ED.ediintFeaturesParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseH2 :: ByteString -> Either String H2.HTTP2Settings
parseH2 bs = case runParser H2.http2SettingsParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


renderAMP :: AMP.AMPCacheTransform -> ByteString
renderAMP = M.toStrictByteString . AMP.renderAMPCacheTransform


renderCC :: CC.ConfigurationContext -> ByteString
renderCC = M.toStrictByteString . CC.renderConfigurationContext


renderED :: ED.EDIINTFeatures -> ByteString
renderED = M.toStrictByteString . ED.renderEDIINTFeatures


renderH2 :: H2.HTTP2Settings -> ByteString
renderH2 = M.toStrictByteString . H2.renderHTTP2Settings


-- Unit tests ------------------------------------------------------------------

unit_amp_parse :: Spec
unit_amp_parse = it "parses an AMP cache transform with version range" $
  case parseAMP "google;v=\"1..100\"" of
    Right v -> AMP.ampCacheTransformValue v `shouldBe` ST.fromString "google;v=\"1..100\""
    other -> error (show other)


unit_amp_render :: Spec
unit_amp_render =
  it "renders AMP cache transform verbatim" $
    renderAMP (AMP.AMPCacheTransform (ST.fromString "google;v=\"1..100\""))
      `shouldBe` "google;v=\"1..100\""


typedAMPTransforms :: Spec
typedAMPTransforms = describe "AMP-Cache-Transform alternatives" $ do
  let lookupAMP wire = lookupHeader @AMP.AMPCacheTransform (headerMapFromList (fmap ("AMP-Cache-Transform",) wire))
      parseEntries wire = lookupAMP wire >>= traverse AMP.ampTransforms
      noParams name = AMP.AMPTransform name []
      param name = AMP.AMPParameter name . Just
      text = param "v" . AMP.AMPString
      offered "google" = [2, 6, 5, 3]
      offered "any" = [8, 9]
      offered _ = []
  it "retains alternatives and their physical lines, including a version list split across lines" $ do
    let wire = "\tgoogle \t; v=\"1 .. 3" :| ["5\" , any\t"]
        expected = AMP.AMPTransform "google" [text "1 .. 3,5"] :| [noParams "any"]
        headers = headerMapFromList [("AMP-Cache-Transform", "\tgoogle \t; v=\"1 .. 3"), ("AMP-Cache-Transform", "5\" , any\t")]
    case lookupHeader @AMP.AMPCacheTransform headers of
      Right (Just field) -> do
        AMP.ampTransforms field `shouldBe` Right expected
        lookupRawHeader hAMPCacheTransform (setHeader field (headerMapFromList [])) `shouldBe` Just wire
        renderAMP field `shouldBe` "\tgoogle \t; v=\"1 .. 3,5\" , any\t"
      other -> expectationFailure (show other)
    parseEntries ["google;v=\"1..3,5\"", "any"]
      `shouldBe` Right (Just (AMP.AMPTransform "google" [text "1..3,5"] :| [noParams "any"]))
  it "preserves draft identifier punctuation, bare parameters, strings, exact numerics and binary" $ do
    let wire = "c_1-2/*;flag;s=\t\"a\\\"b\\\\c\";i=-9223372036854775808;f=-01.2030;b=*Zh==*"
        params =
          [ AMP.AMPParameter "flag" Nothing
          , param "s" (AMP.AMPString "a\"b\\c")
          , param "i" (AMP.AMPInteger minBound)
          , param "f" (AMP.AMPFloat (-12030) 4)
          , param "b" (AMP.AMPBytes "f")
          ]
        entries = AMP.AMPTransform "c_1-2/*" params :| []
    (parseAMP wire >>= AMP.ampTransforms) `shouldBe` Right entries
    (fmap renderAMP (AMP.ampCacheTransformFromEntries entries))
      `shouldBe` Right "c_1-2/*;flag;s=\"a\\\"b\\\\c\";i=-9223372036854775808;f=-1.2030;b=*Zg==*"
  it "rejects malformed outer syntax without discarding a valid neighboring alternative" $ do
    forM_ ["", ",any", "any,", "any,,google", "Google, any", "1cache, any", "go.ogle, any", "google;v=\"1\";v=\"2\", any", "google;=\"1\", any", "google;v =\"1\", any", "google;x=?1, any", "google;x=:Zg==:, any", "google;x=*Zg*, any", "google;x=*Zg===*, any", "google;x=\"a\\n\", any", "google;x=\"a\255\", any", "google;x=9223372036854775808, any", "google;x=-9223372036854775809, any", "google;x=00000000000000000000, any", "google;x=0.123456789012345, any"] $ \wire -> do
      lookupAMP [wire] `shouldSatisfy` isLeft
      parseAMP wire `shouldSatisfy` isLeft
    lookupAMP ["any", ""] `shouldSatisfy` isLeft
  it "supports the draft's minimum list and parameter capacities" $ do
    let parameters = BS.concat (map (\n -> ";p" <> M.toStrictByteString (M.intDec n)) [1 .. 256])
        alternatives = BSC.intercalate ", " (replicate 1023 "any" <> ["google" <> parameters])
    case parseAMP alternatives >>= AMP.ampTransforms of
      Right entries -> do
        NE.length entries `shouldBe` 1024
        length (AMP.ampTransformParameters (NE.last entries)) `shouldBe` 256
      failed -> expectationFailure (show failed)
  it "skips unsatisfiable alternatives and chooses the highest version of the first match" $ do
    forM_ ["google;v=\"bad\", any", "google;v=\"3..1\", any", "google;v=\"1..3,3\", any", "google;v=\"1,01\", any", "google;v=\"-1\", any", "google;v=\"2..9223372036854775808\", any", "google;v=\"\", any", "google;v=3, any", "google;v, any", "google;v=\"1..10\";extra=\"x\", any", "unknown, any"] $ \wire ->
      (parseAMP wire >>= AMP.selectAMPTransform offered) `shouldBe` Right (Just (AMP.AMPTransformSelection "any" 9))
    (parseAMP "google;v=\"5 , ,1 .. 3,\", any" >>= AMP.selectAMPTransform offered)
      `shouldBe` Right (Just (AMP.AMPTransformSelection "google" 5))
    (parseAMP "google;v=\"-0, 9223372036854775807\"" >>= AMP.ampTransforms >>= AMP.ampVersionConstraint . NE.head)
      `shouldBe` Right (AMP.AMPVersions [AMP.AMPVersion 0, AMP.AMPVersion 9223372036854775807])
    (parseAMP "unknown" >>= AMP.selectAMPTransform offered) `shouldBe` Right Nothing
  it "authors typed ranges and an exact response selection" $ do
    let selection = AMP.AMPTransformSelection "google" 5
        exact = AMP.ampCacheTransformFromSelection selection
    fmap renderAMP exact `shouldBe` Right "google;v=\"5\""
    (exact >>= AMP.ampResponseSelection) `shouldBe` Right selection
    AMP.ampCacheTransformFromSelection (AMP.AMPTransformSelection "Google" 5) `shouldSatisfy` isLeft
    AMP.ampCacheTransformFromSelection (AMP.AMPTransformSelection "any" maxBound) `shouldSatisfy` isLeft
    AMP.ampVersionParameter [AMP.AMPVersion 5, AMP.AMPVersionRange 1 3] `shouldBe` Right (text "5, 1..3")
    AMP.ampVersionParameter [AMP.AMPVersionRange 1 3, AMP.AMPVersion 3] `shouldSatisfy` isLeft
    AMP.ampCacheTransformFromEntries (AMP.AMPTransform "any" [param "b" (AMP.AMPFloat 5 (-1))] :| []) `shouldSatisfy` isLeft
    forM_ ["any", "google;v=\"1..2\"", "google;v=\"1,2\"", "google;v=\"1\", any"] $ \wire ->
      (parseAMP wire >>= AMP.ampResponseSelection) `shouldSatisfy` isLeft


unit_cc_parse :: Spec
unit_cc_parse = it "parses a configuration context URI" $
  case parseCC "https://example.com/configurations/myConfiguration1" of
    Right v -> CC.configurationContextValue v `shouldBe` "https://example.com/configurations/myConfiguration1"
    other -> error (show other)


unit_cc_render :: Spec
unit_cc_render =
  it "renders configuration context verbatim" $
    renderCC (CC.ConfigurationContext "https://example.com/configurations/myConfiguration1")
      `shouldBe` "https://example.com/configurations/myConfiguration1"


unit_ediint_parse :: Spec
unit_ediint_parse = it "parses a comma-separated feature list" $
  case parseED "CEM, multiple-attachments" of
    Right (ED.EDIINTFeatures (a :| [b])) -> do
      a `shouldBe` ST.fromString "CEM"
      b `shouldBe` ST.fromString "multiple-attachments"
    other -> error (show other)


unit_ediint_render :: Spec
unit_ediint_render =
  it "renders the feature list comma-separated" $
    renderED (ED.EDIINTFeatures (ST.fromString "CEM" :| [ST.fromString "multiple-attachments"]))
      `shouldBe` "CEM, multiple-attachments"


unit_ediint_field_lines :: Spec
unit_ediint_field_lines = it "combines EDIINT feature lines using the RFC 6017 alphabet" $ do
  let field = ED.EDIINTFeatures ("CeM" :| ["multiple-attachments", "Extension-42"])
      readFeatures = lookupHeader @ED.EDIINTFeatures . headerMapFromList . map ("EDIINT-Features",)
  readFeatures [" \tCeM \t", "multiple-attachments\t, Extension-42 "] `shouldBe` Right (Just field)
  readFeatures ["CeM, multiple-attachments,Extension-42"] `shouldBe` Right (Just field)
  let previous = headerMapFromList [("EDIINT-Features", "CEM"), ("EDIINT-Features", "Old")]
  lookupRawHeader "EDIINT-Features" (setHeader field previous)
    `shouldBe` Just ("CeM, multiple-attachments, Extension-42" :| [])
  forM_ ["", "CEM,", "CEM,,old", "CEM,  old", "new_feature", "new.feature", "CEM\xff"] $ \invalid ->
    readFeatures ["CEM", invalid] `shouldSatisfy` isLeft
  forM_ [minBound .. maxBound] $ \byte -> do
    let name = "A" <> BS.singleton byte <> "0"
        allowed = byte `BS.elem` "-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        decoded = fmap (fmap (fmap ST.toByteString . ED.ediintFeatures)) (readFeatures [name])
    if byte == 44
      then decoded `shouldBe` Right (Just ("A" :| ["0"]))
      else if allowed
        then decoded `shouldBe` Right (Just (name :| []))
        else decoded `shouldSatisfy` isLeft


unit_http2_parse :: Spec
unit_http2_parse = it "decodes the base64url SETTINGS payload" $
  do
    let wire = "AAMAAABkAAQAAP__"
        entries = H2.HTTP2Setting 3 100 :| [H2.HTTP2Setting 4 65535]
        value = H2.http2SettingsFromEntries entries
    parseH2 wire `shouldBe` Right value
    H2.http2SettingsPayload value `shouldBe` BS.pack [0, 3, 0, 0, 0, 100, 0, 4, 0, 0, 255, 255]
    H2.http2SettingsEntries value `shouldBe` Just entries
    lookupHeader @H2.HTTP2Settings (headerMapFromList [("HTTP2-Settings", " \t" <> wire <> "\t ")])
      `shouldBe` Right (Just value)


unit_http2_render :: Spec
unit_http2_render =
  it "encodes the payload as unpadded base64url" $
    renderH2 (H2.http2SettingsFromEntries (H2.HTTP2Setting 65535 4294967295 :| [])) `shouldBe` "________"


typedHTTP2Settings :: Spec
typedHTTP2Settings = describe "typed HTTP2-Settings field" $ do
  it "preserves unknown setting identifiers and duplicate updates in order" $ do
    let entries = H2.HTTP2Setting 3 100 :| [H2.HTTP2Setting 0xaaaa 0xff000001, H2.HTTP2Setting 3 200]
        value = H2.http2SettingsFromEntries entries
        fields = setHeader value (headerMapFromList [])
    lookupHeader @H2.HTTP2Settings fields `shouldBe` Right (Just value)
    lookupRawHeader "HTTP2-Settings" fields `shouldBe` Just (renderH2 value :| [])
    H2.http2SettingsEntries value `shouldBe` Just entries
  it "rejects duplicates, partial payloads, and invalid base64url" $ do
    forM_ ["", " \t", "AAAA", "AAAAAAAAAA", "AAAAAAAA=", "AAAAAA==", "+AAAAAAA", "/AAAAAAA", "AAAA AAA", "AAAA\nAAA"] $ \wire -> do
      parseH2 wire `shouldSatisfy` isLeft
      lookupHeader @H2.HTTP2Settings (headerMapFromList [("HTTP2-Settings", wire)]) `shouldSatisfy` isLeft
    lookupHeader @H2.HTTP2Settings (headerMapFromList [("HTTP2-Settings", "AAMAAABk"), ("HTTP2-Settings", "AAQAAP__")])
      `shouldSatisfy` isLeft
    H2.http2SettingsEntries (H2.HTTP2Settings "bad") `shouldBe` Nothing


-- Round-trip properties -------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 12)
      (Gen.element (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.+"))


prop_amp_roundtrip :: Property
prop_amp_roundtrip = property $ do
  entries <- forAll (NE.fromList <$> Gen.list (Range.linear 1 6) transform)
  let authored = AMP.ampCacheTransformFromEntries entries
  (authored >>= parseAMP . renderAMP >>= AMP.ampTransforms) === Right entries
  where
    identifier = BSC.pack . ('c' :) <$> Gen.string (Range.linear 0 24) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "_-*/"))
    value =
      Gen.choice
        [ AMP.AMPInteger <$> Gen.int64 (Range.linearBounded)
        , AMP.AMPFloat <$> Gen.int64 (Range.linear (-9999999999) 9999999999) <*> Gen.int (Range.linear 1 4)
        , AMP.AMPString . BSC.pack <$> Gen.string (Range.linear 0 25) (Gen.element [' ' .. '~'])
        , AMP.AMPBytes <$> Gen.bytes (Range.linear 0 25)
        ]
    transform = AMP.AMPTransform <$> identifier <*> (maybe [] pure <$> Gen.maybe (AMP.AMPParameter <$> identifier <*> Gen.maybe value))


prop_cc_roundtrip :: Property
prop_cc_roundtrip = property $ do
  v <- CC.ConfigurationContext . ("https://example.com/configurations/" <>) <$> forAll tokenGen
  case parseCC (renderCC v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


prop_ediint_roundtrip :: Property
prop_ediint_roundtrip = property $ do
  let featureName = ST.fromString <$> Gen.string (Range.linear 1 12) (Gen.element ('-' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))
  v <- ED.EDIINTFeatures <$> forAll (Gen.nonEmpty (Range.linear 1 5) featureName)
  case parseED (renderED v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


prop_http2_roundtrip :: Property
prop_http2_roundtrip = property $ do
  count <- forAll (Gen.int (Range.linear 1 6))
  v <- H2.HTTP2Settings <$> forAll (Gen.bytes (Range.singleton (count * 6)))
  case parseH2 (renderH2 v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


typedOSLCFields :: Spec
typedOSLCFields = describe "typed OSLC fields" $ do
  it "accepts valid URIs with queries and fragments and retains their wire spelling"
    $ forM_
      [ "https://example.com/configurations/Release%20A?view=summary#active"
      , "https://[2001:db8::1]:8443/configurations/v2"
      , "urn:example:config:42#current"
      ]
    $ \wire -> do
      let field = CC.ConfigurationContext (ST.fromString (BSC.unpack wire))
      lookupContext ("\t " <> wire <> " \t") `shouldBe` Right (Just field)
      lookupRawHeader "Configuration-Context" (setHeader field empty) `shouldBe` Just (wire :| [])
  it "rejects relative, bracketed, and malformed context values"
    $ forM_
      [ ""
      , "configurations/v2"
      , "//example.com/configs/a"
      , "#active"
      , "<https://example.com/configurations/v2>"
      , "https://example.com/bad%2"
      , "https://[bad]/cfg"
      , "https://example.com:bad/cfg"
      , "https://example.com/a b"
      , "urn:example:config#two#fragments"
      , "https://example.com/\xff"
      ]
    $ \wire -> lookupContext wire `shouldSatisfy` isLeft
  it "preserves version spelling and exposes unbounded decimal components" $ do
    forM_
      [ ("2.0", OCV.OSLCCoreVersion "2.0", OCV.OSLCCoreVersionNumbers 2 0)
      , ("3.0", OCV.OSLCCoreVersion "3.0", OCV.OSLCCoreVersionNumbers 3 0)
      , ("002.00401", OCV.OSLCCoreVersion "002.00401", OCV.OSLCCoreVersionNumbers 2 401)
      , ("010.0", OCV.OSLCCoreVersion "010.0", OCV.OSLCCoreVersionNumbers 10 0)
      , ("18446744073709551616.00018446744073709551617", OCV.OSLCCoreVersion "18446744073709551616.00018446744073709551617", OCV.OSLCCoreVersionNumbers 18_446_744_073_709_551_616 18_446_744_073_709_551_617)
      ]
      $ \(wire, field, numbers) -> do
        lookupVersion wire `shouldBe` Right (Just field)
        lookupVersion ("\t " <> wire <> " \t") `shouldBe` Right (Just field)
        OCV.oslcCoreVersionNumbers field `shouldBe` Just numbers
        lookupRawHeader "OSLC-Core-Version" (setHeader field empty) `shouldBe` Just (wire :| [])
  it "rejects version syntax outside the grammar and majors below two" $ do
    forM_ ["", "2", ".0", "2.", "1.0", "0001.0", "00.000", "+2.0", "2.-1", "2.0.1", "2. 0", "2.0, 3.0", "2.0\r"] $ \wire -> do
      lookupVersion wire `shouldSatisfy` isLeft
      OCV.oslcCoreVersionNumbers (OCV.OSLCCoreVersion (ST.fromString (BSC.unpack wire))) `shouldBe` Nothing
  it "rejects repeated OSLC singleton fields" $ do
    let duplicates name value = headerMapFromList [(name, value), (name, value)]
    lookupHeader @CC.ConfigurationContext (duplicates "Configuration-Context" "https://example.com/context") `shouldSatisfy` isLeft
    lookupHeader @OCV.OSLCCoreVersion (duplicates "OSLC-Core-Version" "3.0") `shouldSatisfy` isLeft
  where
    empty = headerMapFromList []
    lookupContext value = lookupHeader @CC.ConfigurationContext (headerMapFromList [("Configuration-Context", value)])
    lookupVersion value = lookupHeader @OCV.OSLCCoreVersion (headerMapFromList [("OSLC-Core-Version", value)])


tests :: Spec
tests =
  describe "MiscProvisional" $
    sequence_
      [ unit_amp_parse
      , unit_amp_render
      , typedAMPTransforms
      , unit_cc_parse
      , unit_cc_render
      , unit_ediint_parse
      , unit_ediint_render
      , unit_ediint_field_lines
      , unit_http2_parse
      , unit_http2_render
      , it "AMP-Cache-Transform round-trip" prop_amp_roundtrip
      , it "Configuration-Context round-trip" prop_cc_roundtrip
      , it "EDIINT-Features round-trip" prop_ediint_roundtrip
      , it "HTTP2-Settings round-trip" prop_http2_roundtrip
      , typedHTTP2Settings
      , typedOSLCFields
      ]
