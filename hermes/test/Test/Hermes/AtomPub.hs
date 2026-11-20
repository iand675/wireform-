{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AtomPub (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (
  HeaderMessage (..),
  defaultHeaderSettings,
  headerMapFromList,
  lookupHeader,
  lookupHeaderWith,
  lookupRawHeader,
  setHeader,
  withHeaderMessage,
 )
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Meter as Meter
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Slug as Slug
import qualified Network.HTTP.Headers.SoapAction as SA
import Test.Syd
import Test.Syd.Hedgehog ()


dropTrailingOws :: ByteString -> ByteString
dropTrailingOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


-- SLUG ----------------------------------------------------------------------

parseSlug :: ByteString -> Either String Slug.Slug
parseSlug bs = case runParser Slug.slugParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSlug :: Slug.Slug -> ByteString
renderSlug = M.toStrictByteString . Slug.renderSlug


unit_slug_parse :: Spec
unit_slug_parse = it "parses an opaque slug verbatim" $
  case parseSlug "The Beach at Vacation" of
    Right (Slug.Slug t) -> t `shouldBe` ST.fromString "The Beach at Vacation"
    other -> error (show other)


unit_slug_render :: Spec
unit_slug_render =
  it "renders a slug verbatim" $
    renderSlug (Slug.Slug (ST.fromString "Photo%20100.png")) `shouldBe` "Photo%20100.png"


slugGen :: Gen Text.Text
slugGen = Text.pack <$> Gen.string (Range.linear 0 24) (Gen.element slugChars)
  where
    slugChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.~% +,\n\t\0éè東🌍"


prop_slug_roundtrip :: Property
prop_slug_roundtrip = property $ do
  title <- forAll slugGen
  let v = Slug.slugFromText title
  let bs = renderSlug v
  case parseSlug bs of
    Right v' -> do
      v === v'
      Slug.slugDecodedText v' === Just title
    Left err -> error (err <> " on " <> show bs)


-- SoapAction ----------------------------------------------------------------

parseSoap :: ByteString -> Either String SA.SoapAction
parseSoap bs = case runParser SA.soapActionParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSoap :: SA.SoapAction -> ByteString
renderSoap = M.toStrictByteString . SA.renderSoapAction


unit_soap_parse :: Spec
unit_soap_parse = it "parses the URI out of the quoted SOAPAction value" $
  case parseSoap "\"http://example.org/Order\"" of
    Right (SA.SoapAction u) -> u `shouldBe` ST.fromString "http://example.org/Order"
    other -> error (show other)


unit_soap_render :: Spec
unit_soap_render =
  it "renders the URI inside double quotes" $
    renderSoap (SA.SoapAction (ST.fromString "urn:example:action")) `shouldBe` "\"urn:example:action\""


unit_soap_empty :: Spec
unit_soap_empty = it "parses the empty SOAPAction value" $
  case parseSoap "\"\"" of
    Right (SA.SoapAction u) -> u `shouldBe` ST.fromString ""
    other -> error (show other)


soapGen :: Gen ST.ShortText
soapGen = do
  prefix <- Gen.element ["", "../", "urn:", "https://example.org/", "#"]
  body <- Gen.string (Range.linear 1 30) (Gen.element uriChars)
  pure (ST.fromString (prefix <> body))
  where
    uriChars =
      ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "@!$&'()*+,;=-._~"


prop_soap_roundtrip :: Property
prop_soap_roundtrip = property $ do
  v <- SA.SoapAction <$> forAll soapGen
  let bs = renderSoap v
  case parseSoap bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Meter ---------------------------------------------------------------------

parseMeter :: ByteString -> Either String Meter.Meter
parseMeter bs = case runParser Meter.meterParser bs of
  OK v leftover
    | BS.null (dropTrailingOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderMeter :: Meter.Meter -> ByteString
renderMeter = M.toStrictByteString . Meter.renderMeter


unit_meter_parse :: Spec
unit_meter_parse = it "parses a comma list of meter-directives" $
  case parseMeter "count=3/2, will-report-and-limit, wont-limit" of
    Right (Meter.Meter ds) ->
      ds
        `shouldBe` [ Meter.MeterDirective (ST.fromString "count") (Just (ST.fromString "3/2"))
                   , Meter.MeterDirective (ST.fromString "will-report-and-limit") Nothing
                   , Meter.MeterDirective (ST.fromString "wont-limit") Nothing
                   ]
    other -> error (show other)


unit_meter_render :: Spec
unit_meter_render =
  it "renders directives separated by commas" $
    renderMeter
      ( Meter.Meter
          [ Meter.MeterDirective (ST.fromString "max-uses") (Just (ST.fromString "10"))
          , Meter.MeterDirective (ST.fromString "dont-report") Nothing
          ]
      )
      `shouldBe` "max-uses=10, dont-report"


meterNameGen :: Gen ST.ShortText
meterNameGen = (("ext-" <>) . ST.fromString) <$> Gen.string (Range.linear 1 12) (Gen.element nameChars)
  where
    nameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-"


meterValGen :: Gen ST.ShortText
meterValGen = ST.fromString <$> Gen.string (Range.linear 0 12) (Gen.element valChars)
  where
    valChars = ['a' .. 'z'] ++ ['0' .. '9'] ++ "-:/ \"\\"


meterDirGen :: Gen Meter.MeterDirective
meterDirGen = Meter.MeterDirective <$> meterNameGen <*> Gen.maybe meterValGen


meterGen :: Gen Meter.Meter
meterGen = Meter.Meter <$> Gen.list (Range.linear 0 5) meterDirGen


prop_meter_roundtrip :: Property
prop_meter_roundtrip = property $ do
  v <- forAll meterGen
  let bs = renderMeter v
  case parseMeter bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


typedSoapAction :: Spec
typedSoapAction = describe "typed SOAPAction" $ do
  it "distinguishes a missing field, absent intent, an empty URI, and an explicit reference" $ do
    let field wire = headerMapFromList [("SOAPAction", wire)]
        explicit = SA.SoapAction {SA.soapActionURI = "../order%20now#submit"}
    lookupHeader @SA.SoapAction (headerMapFromList []) `shouldBe` Right Nothing
    lookupHeader @SA.SoapAction (field " \t") `shouldBe` Right (Just SA.SoapActionNoIntent)
    lookupHeader @SA.SoapAction (field "\"\"") `shouldBe` Right (Just (SA.SoapAction ""))
    lookupHeader @SA.SoapAction (field " \"../order%20now#submit\"\t") `shouldBe` Right (Just explicit)
    map SA.soapActionIntent [SA.SoapActionNoIntent, SA.SoapAction "", explicit]
      `shouldBe` [SA.SoapNoIntent, SA.SoapRequestURI, SA.SoapExplicitAction "../order%20now#submit"]
    SA.soapActionURI SA.SoapActionNoIntent `shouldBe` ""
    lookupRawHeader "SOAPAction" (setHeader SA.SoapActionNoIntent (field "\"x\"")) `shouldBe` Just ("" :| [])
  it "validates the quoted URI reference and rejects repeated field lines" $ do
    forM_ ["order", "\"bad space\"", "\"bad%GG\"", "\"bad\\\"path\"", "\"x\r\"", "\"x\"junk", "\"http://[bad]/\""] $ \wire ->
      lookupHeader @SA.SoapAction (headerMapFromList [("SOAPAction", wire)]) `shouldSatisfy` isLeft
    lookupHeader @SA.SoapAction (headerMapFromList [("SOAPAction", ""), ("SOAPAction", "\"x\"")])
      `shouldSatisfy` isLeft


typedMeter :: Spec
typedMeter = describe "typed Meter" $ do
  it "reads mixed spellings, repeated lines, and counts with arbitrary precision" $ do
    let wire = " , COUNT=18446744073709551616/42, x"
        fields = headerMapFromList [("Meter", wire), ("Meter", ",wont-limit,w,")]
        directives =
          [ Meter.MeterDirective "COUNT" (Just "18446744073709551616/42")
          , Meter.MeterDirective "x" Nothing
          , Meter.MeterDirective "wont-limit" Nothing
          , Meter.MeterDirective "w" Nothing
          ]
        meter = Meter.Meter directives
        request = withHeaderMessage HeaderRequest defaultHeaderSettings
    lookupHeaderWith @Meter.Meter request fields `shouldBe` Right (Just meter)
    map Meter.meterDirectiveKind directives
      `shouldBe` map
        Just
        [ Meter.MeterCount (Meter.MeterUsageCounts 18446744073709551616 42)
        , Meter.MeterWontReport
        , Meter.MeterWontLimit
        , Meter.MeterWillReportAndLimit
        ]
    renderMeter meter `shouldBe` "COUNT=18446744073709551616/42, x, wont-limit, w"
  it "supports all response directives, including a timeout in minutes" $ do
    let kinds = [Meter.MeterMaxUses 3, Meter.MeterMaxReuses 6, Meter.MeterDoReport, Meter.MeterDontReport, Meter.MeterTimeoutMinutes 10, Meter.MeterWontAsk]
        directives = map Meter.meterDirectiveFromKind kinds
        meter = Meter.Meter directives
        response = withHeaderMessage HeaderResponse defaultHeaderSettings
    map Meter.meterDirectiveKind directives `shouldBe` map Just kinds
    lookupHeaderWith @Meter.Meter response (setHeader meter (headerMapFromList [])) `shouldBe` Right (Just meter)
    renderMeter meter `shouldBe` "u=3, r=6, d, e, t=10, n"
  it "checks direction and argument shape while retaining present empty lists" $ do
    let single wire = headerMapFromList [("Meter", wire)]
        request = withHeaderMessage HeaderRequest defaultHeaderSettings
        response = withHeaderMessage HeaderResponse defaultHeaderSettings
    forM_ ["count=3", "c=\"3/2\"", "count=3/", "count=/3", "t", "u=-1", "w=1", "n=\"\"", "r=2/3"] $ \wire ->
      lookupHeader @Meter.Meter (single wire) `shouldSatisfy` isLeft
    lookupHeaderWith @Meter.Meter request (single "w, max-reuses=10") `shouldSatisfy` isLeft
    lookupHeaderWith @Meter.Meter response (single "t=10,c=2/3") `shouldSatisfy` isLeft
    lookupHeader @Meter.Meter (headerMapFromList []) `shouldBe` Right Nothing
    lookupHeader @Meter.Meter (single ", , ") `shouldBe` Right (Just (Meter.Meter []))
    lookupRawHeader "Meter" (setHeader (Meter.Meter []) (single "w")) `shouldBe` Just ("" :| [])
  it "round-trips opaque quoted extension bytes" $ do
    let wire = "vendor=\"x,;\\\"\128\255\\\\\""
        expected = Meter.Meter [Meter.MeterDirective "vendor" (Just "x,;\"\128\255\\")]
    lookupHeader @Meter.Meter (headerMapFromList [("Meter", wire)]) `shouldBe` Right (Just expected)
    renderMeter expected `shouldBe` wire


tests :: Spec
tests =
  describe "AtomPub" $
    sequence_
      [ unit_slug_parse
      , unit_slug_render
      , it "retains percent-escape spelling while exposing the decoded title" $ do
          let wire = "The Beach at S%c3%a8te+, 100%25"
              value = Slug.Slug "The Beach at S%c3%a8te+, 100%25"
          lookupHeader @Slug.Slug (headerMapFromList [("Slug", wire)]) `shouldBe` Right (Just value)
          lookupRawHeader "Slug" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
          Slug.slugDecodedText value `shouldBe` Just "The Beach at Sète+, 100%"
          renderSlug (Slug.slugFromText " Sète+100% ") `shouldBe` "%20S%C3%A8te+100%25%20"
          renderSlug (Slug.slugFromText "  ") `shouldBe` "%20%20"
          renderSlug (Slug.slugFromText "The Beach at Vacation") `shouldBe` "The Beach at Vacation"
          parseSlug "" `shouldBe` Right (Slug.Slug "")
      , it "validates UTF-8 after unescaping and rejects malformed and repeated fields" $ do
          forM_ ["%", "%0", "%G0", "%C0%80", "%ED%A0%80", "%C3", "raw\195\169", "a\rb", "a\nb", "a\NUL", "a\DEL"] $ \wire -> do
            parseSlug wire `shouldSatisfy` isLeft
            lookupHeader @Slug.Slug (headerMapFromList [("Slug", wire)]) `shouldSatisfy` isLeft
          lookupHeader @Slug.Slug (headerMapFromList [("Slug", "a"), ("Slug", "b")]) `shouldSatisfy` isLeft
      , it "slug round-trip" prop_slug_roundtrip
      , unit_soap_parse
      , unit_soap_render
      , unit_soap_empty
      , it "soap-action round-trip" prop_soap_roundtrip
      , typedSoapAction
      , unit_meter_parse
      , unit_meter_render
      , it "meter round-trip" prop_meter_roundtrip
      , typedMeter
      ]
