{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.TCN (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType (..))
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.AcceptAdditions as AA
import qualified Network.HTTP.Headers.AcceptFeatures as AF
import qualified Network.HTTP.Headers.Alternates as Alt
import Network.HTTP.Headers.ContentLanguage (LanguageTag (..))
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Negotiate as Neg
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.TCN as TCN
import qualified Network.HTTP.Headers.VariantVary as VV
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOk :: (ByteString -> Result String a) -> ByteString -> Either String a
parseOk run bs = case run bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


tcharChars :: [Char]
tcharChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-.!#$%&'*+^_`|~"


-- | A letter-led token (never equal to the bare @*@ wildcard).
genName :: Gen ST.ShortText
genName = do
  c <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'])
  cs <- Gen.list (Range.linear 0 7) (Gen.element tcharChars)
  pure (ST.fromString (c : cs))


-- | A non-empty token used as a directive\/parameter value.
genVal :: Gen ST.ShortText
genVal = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tcharChars)


-- ---------------------------------------------------------------------------
-- TCN
-- ---------------------------------------------------------------------------

unit_tcn_parse :: Spec
unit_tcn_parse =
  it "parses a TCN directive list" $
    parseOk (runParser TCN.tcnParser) "list, keep"
      `shouldBe` Right (TCN.TCN [TCN.TCNDirective "list" Nothing, TCN.TCNDirective "keep" Nothing])


unit_tcn_render :: Spec
unit_tcn_render =
  it "renders a TCN directive list" $
    M.toStrictByteString
      (TCN.renderTCN (TCN.TCN [TCN.TCNDirective "choice" Nothing, TCN.TCNDirective "foo" (Just "bar")]))
      `shouldBe` "choice, foo=bar"


genTCN :: Gen TCN.TCN
genTCN = TCN.TCN <$> Gen.list (Range.linear 0 4) genDirective
  where
    genDirective = TCN.TCNDirective <$> genName <*> Gen.maybe genVal


prop_tcn_roundtrip :: Property
prop_tcn_roundtrip = property $ do
  v <- forAll genTCN
  let bs = M.toStrictByteString (TCN.renderTCN v)
  case parseOk (runParser TCN.tcnParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Negotiate
-- ---------------------------------------------------------------------------

unit_negotiate_parse :: Spec
unit_negotiate_parse =
  it "parses an rvsa-version list" $
    parseOk (runParser Neg.negotiateParser) "1.0, 2.5"
      `shouldBe` Right (Neg.Negotiate (Neg.NegotiateDirective "1.0" Nothing :| [Neg.NegotiateDirective "2.5" Nothing]))


unit_negotiate_render :: Spec
unit_negotiate_render =
  it "renders negotiate directives" $
    M.toStrictByteString
      (Neg.renderNegotiate (Neg.Negotiate (Neg.NegotiateDirective "trans" Nothing :| [Neg.NegotiateDirective "vlist" Nothing])))
      `shouldBe` "trans, vlist"


genNegotiate :: Gen Neg.Negotiate
genNegotiate = Neg.Negotiate <$> Gen.nonEmpty (Range.linear 1 4) genDirective
  where
    genDirective = Neg.NegotiateDirective <$> genName <*> Gen.maybe genVal


prop_negotiate_roundtrip :: Property
prop_negotiate_roundtrip = property $ do
  v <- forAll genNegotiate
  let bs = M.toStrictByteString (Neg.renderNegotiate v)
  case parseOk (runParser Neg.negotiateParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Variant-Vary
-- ---------------------------------------------------------------------------

unit_variantvary_parse_all :: Spec
unit_variantvary_parse_all =
  it "parses the * wildcard" $
    parseOk (runParser VV.variantVaryParser) "*" `shouldBe` Right VV.VariantVaryAll


unit_variantvary_parse_fields :: Spec
unit_variantvary_parse_fields =
  it "parses a field-name list" $
    parseOk (runParser VV.variantVaryParser) "Accept, Accept-Language"
      `shouldBe` Right (VV.VariantVaryFields ("Accept" :| ["Accept-Language"]))


unit_variantvary_render :: Spec
unit_variantvary_render =
  it "renders a field-name list" $
    M.toStrictByteString (VV.renderVariantVary (VV.VariantVaryFields ("Accept" :| ["Accept-Language"])))
      `shouldBe` "Accept, Accept-Language"


genVariantVary :: Gen VV.VariantVary
genVariantVary =
  Gen.choice
    [ pure VV.VariantVaryAll
    , VV.VariantVaryFields <$> Gen.nonEmpty (Range.linear 1 4) genName
    ]


prop_variantvary_roundtrip :: Property
prop_variantvary_roundtrip = property $ do
  v <- forAll genVariantVary
  let bs = M.toStrictByteString (VV.renderVariantVary v)
  case parseOk (runParser VV.variantVaryParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Accept-Additions
-- ---------------------------------------------------------------------------

unit_acceptadditions_parse :: Spec
unit_acceptadditions_parse =
  it "parses additions with parameters" $
    parseOk (runParser AA.acceptAdditionsParser) "Cream, Whisky;q=0.7"
      `shouldBe` Right
        ( AA.AcceptAdditions
            [ AA.Addition "Cream" []
            , AA.Addition "Whisky" [AA.AdditionParam "q" (Just "0.7")]
            ]
        )


unit_acceptadditions_render :: Spec
unit_acceptadditions_render =
  it "renders additions with parameters" $
    M.toStrictByteString
      ( AA.renderAcceptAdditions
          ( AA.AcceptAdditions
              [ AA.Addition "Cream" []
              , AA.Addition "Whisky" [AA.AdditionParam "q" (Just "0.7")]
              ]
          )
      )
      `shouldBe` "Cream, Whisky;q=0.7"


typedAcceptAdditions :: Spec
typedAcceptAdditions = it "combines field lines, validates the first quality weight, and preserves quoted bytes" $ do
  let readAdditions = lookupHeader @AA.AcceptAdditions . headerMapFromList . map (\wire -> ("Accept-Additions", wire))
      whisky = AA.Addition "Whisky" [AA.AdditionParam "Q" (Just "0.700"), AA.AdditionParam "name" (Just "dry, \x80\xff")]
      field = AA.AcceptAdditions [AA.Addition "Cream" [], whisky, AA.Addition "Stevia" []]
      input = [", Cream, , Whisky;Q=0.700;name=\"dry", "\x80\xff\", Stevia,"]
  readAdditions input `shouldBe` Right (Just field)
  readAdditions [", ", "\t,"] `shouldBe` Right (Just (AA.AcceptAdditions []))
  lookupRawHeader "Accept-Additions" (setHeader field (headerMapFromList []))
    `shouldBe` Just ("Cream, Whisky;Q=0.700;name=\"dry, \x80\xff\", Stevia" :| [])
  AA.additionQuality whisky `shouldBe` Just 0.7
  AA.additionQuality (AA.Addition "Cream" []) `shouldBe` Nothing
  let laterQ = AA.Addition "Whisky" [AA.AdditionParam "q" (Just "0."), AA.AdditionParam "q" (Just "extension")]
  readAdditions ["Whisky;q=0.;q=extension"] `shouldBe` Right (Just (AA.AcceptAdditions [laterQ]))
  AA.additionQuality laterQ `shouldBe` Just 0
  forM_ ["Whisky;q", "Whisky;q=", "Whisky;Q=1.1", "Whisky;q=0.1234", "Whisky;q=\"0.5\"", "Whisky;name=\"unfinished", "Whisky;name=\"\n\""] $ \invalid ->
    readAdditions ["Cream", invalid] `shouldSatisfy` isLeft


genAcceptAdditions :: Gen AA.AcceptAdditions
genAcceptAdditions = AA.AcceptAdditions <$> Gen.list (Range.linear 0 4) genAddition
  where
    genAddition = AA.Addition <$> genName <*> Gen.list (Range.linear 0 3) genParam
    genParam = Gen.choice [AA.AdditionParam <$> Gen.filter (\name -> name /= "q" && name /= "Q") genName <*> Gen.maybe genVal, pure (AA.AdditionParam "q" (Just "0.750"))]


prop_acceptadditions_roundtrip :: Property
prop_acceptadditions_roundtrip = property $ do
  v <- forAll genAcceptAdditions
  let bs = M.toStrictByteString (AA.renderAcceptAdditions v)
  case parseOk (runParser AA.acceptAdditionsParser) bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Accept-Features (raw-preserving)
-- ---------------------------------------------------------------------------

unit_acceptfeatures_parse :: Spec
unit_acceptfeatures_parse =
  it "preserves the raw Accept-Features value" $
    parseOk (runParser AF.acceptFeaturesParser) "blex, !blebber, colordepth={5}, *"
      `shouldBe` Right (AF.AcceptFeatures "blex, !blebber, colordepth={5}, *")


unit_acceptfeatures_render :: Spec
unit_acceptfeatures_render =
  it "renders the raw Accept-Features value" $
    M.toStrictByteString (AF.renderAcceptFeatures (AF.AcceptFeatures "blex, !blebber, *"))
      `shouldBe` "blex, !blebber, *"


-- ---------------------------------------------------------------------------
-- Alternates (raw-preserving)
-- ---------------------------------------------------------------------------

unit_alternates_parse :: Spec
unit_alternates_parse =
  it "preserves the raw Alternates value" $
    parseOk (runParser Alt.alternatesParser) "{\"paper.1\" 0.9 {type text/html}}, proxy-rvsa=\"1.0\""
      `shouldBe` Right (Alt.Alternates "{\"paper.1\" 0.9 {type text/html}}, proxy-rvsa=\"1.0\"")


unit_alternates_render :: Spec
unit_alternates_render =
  it "renders the raw Alternates value" $
    M.toStrictByteString (Alt.renderAlternates (Alt.Alternates "{\"a.html\" 1.0 {language en}}"))
      `shouldBe` "{\"a.html\" 1.0 {language en}}"


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

typedNegotiate :: Spec
typedNegotiate = describe "typed Negotiate" $ do
  it "combines repeated lines and projects version identifiers without losing spelling" $ do
    let entries =
          Neg.NegotiateDirective "TRANS" Nothing
            :| [ Neg.NegotiateDirective "0001.0025" Nothing
               , Neg.NegotiateDirective "guess-small" Nothing
               , Neg.NegotiateDirective "*" Nothing
               , Neg.NegotiateDirective "x-mode" (Just "fast")
               ]
        value = Neg.Negotiate entries
        fields = headerMapFromList [("Negotiate", "TRANS, ,0001.0025"), ("Negotiate", "guess-small, *, x-mode = fast")]
    lookupHeader @Neg.Negotiate fields `shouldBe` Right (Just value)
    case Neg.mkRvsaVersion 1 25 of
      Just version -> do
        fmap Neg.negotiateDirectiveKind entries
          `shouldBe` (Neg.NegotiateTrans :| [Neg.NegotiateAlgorithmVersion version, Neg.NegotiateGuessSmall, Neg.NegotiateAnyAlgorithm, Neg.NegotiateExtension "x-mode" (Just "fast")])
        Neg.negotiateDirectiveFromKind (Neg.NegotiateAlgorithmVersion version) `shouldBe` Neg.NegotiateDirective "1.25" Nothing
      Nothing -> expectationFailure "valid RVSA version was rejected"
    lookupRawHeader "Negotiate" (setHeader value (headerMapFromList []))
      `shouldBe` Just ("TRANS, 0001.0025, guess-small, *, x-mode=fast" :| [])
  it "limits a known version to four digits per component and retains extension tokens" $ do
    forM_ ["12345.1", "1.12345", "1.", "1.2x", "1.2.3", "+1.2", "1.-2"] $ \wire ->
      Neg.parseRvsaVersion wire `shouldBe` Nothing
    Neg.mkRvsaVersion 10000 0 `shouldBe` Nothing
    Neg.parseRvsaVersion "9999.0000" `shouldBe` Neg.mkRvsaVersion 9999 0
    Neg.negotiateDirectiveKind (Neg.NegotiateDirective "12345.1" Nothing)
      `shouldBe` Neg.NegotiateExtension "12345.1" Nothing
    Neg.negotiateDirectiveKind (Neg.NegotiateDirective "trans" (Just "custom"))
      `shouldBe` Neg.NegotiateExtension "trans" (Just "custom")
    forM_ ["", ", ,", "x=\"value\"", "x=", "trans| value"] $ \wire ->
      lookupHeader @Neg.Negotiate (headerMapFromList [("Negotiate", wire)]) `shouldSatisfy` isLeft


typedTCN :: Spec
typedTCN = describe "typed TCN" $ do
  it "preserves ordered response types, overrides, and opaque extension bytes" $ do
    let directives =
          [ TCN.TCNDirective "ChoIce" Nothing
          , TCN.TCNDirective "re-choose" Nothing
          , TCN.TCNDirective "x-note" (Just "a,\"\128\255\\")
          ]
        extensionWire = "x-note=\"a,\\\"\128\255\\\\\""
        value = TCN.TCN directives
        fields = headerMapFromList [("TCN", " , ChoIce"), ("TCN", "re-choose, " <> extensionWire)]
    lookupHeader @TCN.TCN fields `shouldBe` Right (Just value)
    map TCN.tcnDirectiveKind directives
      `shouldBe` [TCN.TCNResponse TCN.TCNChoice, TCN.TCNServerOverride TCN.TCNReChoose, TCN.TCNExtension "x-note" (Just "a,\"\128\255\\")]
    TCN.tcnDirectiveFromKind (TCN.TCNServerOverride TCN.TCNKeep) `shouldBe` TCN.TCNDirective "keep" Nothing
    lookupRawHeader "TCN" (setHeader value (headerMapFromList []))
      `shouldBe` Just ("ChoIce, re-choose, " <> extensionWire :| [])
  it "distinguishes an empty field from an absent field and rejects malformed extensions" $ do
    lookupHeader @TCN.TCN (headerMapFromList []) `shouldBe` Right Nothing
    lookupHeader @TCN.TCN (headerMapFromList [("TCN", ", , ")]) `shouldBe` Right (Just (TCN.TCN []))
    lookupHeader @TCN.TCN (headerMapFromList [("TCN", "choice"), ("TCN", "x-note=\"bad\r\"")])
      `shouldSatisfy` isLeft


typedVariantVary :: Spec
typedVariantVary = describe "typed Variant-Vary" $ do
  it "retains wire spelling across field lines and provides case-folded names" $ do
    let fields = headerMapFromList [("Variant-Vary", "Accept-Language, "), ("Variant-Vary", ", X-Edition, *suffix")]
        value = VV.VariantVaryFields ("Accept-Language" :| ["X-Edition", "*suffix"])
    lookupHeader @VV.VariantVary fields `shouldBe` Right (Just value)
    VV.variantVaryFieldNames value `shouldBe` Right (Just ("accept-language" :| ["x-edition", "*suffix"]))
    lookupRawHeader "Variant-Vary" (setHeader value (headerMapFromList []))
      `shouldBe` Just ("Accept-Language, X-Edition, *suffix" :| [])
  it "accepts only a lone wildcard or a nonempty field-name list" $ do
    lookupHeader @VV.VariantVary (headerMapFromList [("Variant-Vary", ", * ,")])
      `shouldBe` Right (Just VV.VariantVaryAll)
    VV.variantVaryFieldNames VV.VariantVaryAll `shouldBe` Right Nothing
    forM_ ["", " , ", "*, Accept", "Accept, *", "*,*", "bad name"] $ \wire ->
      lookupHeader @VV.VariantVary (headerMapFromList [("Variant-Vary", wire)]) `shouldSatisfy` isLeft
    lookupHeader @VV.VariantVary (headerMapFromList [("Variant-Vary", "Accept"), ("Variant-Vary", "*")])
      `shouldSatisfy` isLeft


typedAcceptFeatures :: Spec
typedAcceptFeatures = describe "typed Accept-Features" $ do
  it "projects every condition and preserves repeated field order and extension octets" $ do
    let first = "blex, !BLEBBER, colordepth={5}"
        second = "paper = A4, paper!=\"A%32\";mode=\"a,\\\"\255\\\\\", x-version=104, *"
        fields = headerMapFromList [("Accept-Features", first), ("Accept-Features", second)]
        tag name = either error id (AF.parseFeatureTag name)
        entry name condition = AF.FeatureEntry (AF.FeatureExpression (tag name) condition)
        entries =
          [ entry "blex" AF.FeaturePresent []
          , entry "blebber" AF.FeatureAbsent []
          , entry "colordepth" (AF.FeatureSpecial (AF.FeatureValue "5")) []
          , entry "paper" (AF.FeatureEqual (AF.FeatureValue "A4")) []
          , entry "paper" (AF.FeatureUnequal (AF.FeatureValue "A2")) [AF.FeatureExtension "mode" (Just "a,\"\255\\")]
          , entry "x-version" (AF.FeatureEqual (AF.FeatureValue "104")) []
          , AF.FeatureEntry AF.FeatureWildcard []
          ]
    case lookupHeader @AF.AcceptFeatures fields of
      Right (Just value) -> do
        AF.acceptFeatureEntries value `shouldBe` Right entries
        lookupRawHeader "Accept-Features" (setHeader value (headerMapFromList []))
          `shouldBe` Just ((first <> ", " <> second) :| [])
      result -> expectationFailure (show result)
    case AF.acceptFeaturesFromEntries entries of
      Right value ->
        lookupHeader @AF.AcceptFeatures (setHeader value (headerMapFromList [])) `shouldBe` Right (Just value)
      Left err -> expectationFailure err
  it "distinguishes an empty field, a wildcard, and a quoted tag named '*'" $ do
    let lookupFeatures wire = lookupHeader @AF.AcceptFeatures (headerMapFromList [("Accept-Features", wire)]) >>= traverse AF.acceptFeatureEntries
        star = either error id (AF.parseFeatureTag "\"*\"")
    lookupHeader @AF.AcceptFeatures (headerMapFromList []) `shouldBe` Right Nothing
    lookupFeatures ", , " `shouldBe` Right (Just [])
    lookupFeatures "*, \"*\"" `shouldBe` Right (Just [AF.FeatureEntry AF.FeatureWildcard [], AF.FeatureEntry (AF.FeatureExpression star AF.FeaturePresent) []])
    AF.parseFeatureTag "\"TaBlEs\"" `shouldBe` AF.parseFeatureTag "tables"
  it "rejects malformed operators, values and extensions in any field line" $ do
    forM_ ["paper=", "paper!={A}", "paper={}", "paper={A,B}", "paper=\"unclosed", "paper=\"\r\"", "paper=\"\255\"", "*;x=", "*;bad extension"] $ \wire ->
      lookupHeader @AF.AcceptFeatures (headerMapFromList [("Accept-Features", "blex"), ("Accept-Features", wire)])
        `shouldSatisfy` isLeft
  it "checks quoted-pair octets in feature values and extension values" $ do
    let lookupEntries wire =
          lookupHeader @AF.AcceptFeatures (headerMapFromList [("Accept-Features", wire)]) >>= traverse AF.acceptFeatureEntries
        tag = either error id (AF.parseFeatureTag "paper")
    forM_ [0 .. 255] $ \byte -> do
      let octet = BS.singleton byte
          opaque = byte == 9 || (byte >= 32 && byte /= 127)
          feature condition = AF.FeatureEntry (AF.FeatureExpression tag condition)
          featureWire = "paper=\"\\" <> octet <> "\""
          extensionWire = "paper;x=\"\\" <> octet <> "\""
      if opaque && byte < 128
        then lookupEntries featureWire `shouldBe` Right (Just [feature (AF.FeatureEqual (AF.FeatureValue octet)) []])
        else lookupEntries featureWire `shouldSatisfy` isLeft
      if opaque
        then lookupEntries extensionWire `shouldBe` Right (Just [feature AF.FeaturePresent [AF.FeatureExtension "x" (Just octet)]])
        else lookupEntries extensionWire `shouldSatisfy` isLeft
  it "round-trips arbitrary feature-value octets through typed authoring" $ property $ do
    bytes <- forAll (BS.pack <$> Gen.list (Range.linear 0 60) (Gen.word8 Range.constantBounded))
    let tag = either error id (AF.parseFeatureTag "\"paper!\"")
        entries =
          [ AF.FeatureEntry (AF.FeatureExpression tag (AF.FeatureEqual (AF.FeatureValue bytes))) []
          , AF.FeatureEntry (AF.FeatureExpression tag (AF.FeatureUnequal (AF.FeatureValue bytes))) []
          , AF.FeatureEntry (AF.FeatureExpression tag (AF.FeatureSpecial (AF.FeatureValue bytes))) []
          ]
    case AF.acceptFeaturesFromEntries entries of
      Left err -> error err
      Right header -> do
        let fields = setHeader header (headerMapFromList [])
        (lookupHeader @AF.AcceptFeatures fields >>= traverse AF.acceptFeatureEntries) === Right (Just entries)


typedAlternates :: Spec
typedAlternates = describe "typed Alternates" $ do
  it "decodes all attributes, predicate bags, ranges, factors and proxy versions" $ do
    let first =
          "{\"../paper%20one\" 0.9 {TYPE text/html;profile=\"a,b\"} {charset utf-8} {language en, fr-CA}"
            <> " {length 18446744073709551616} {features !textonly [blebber !wolx];+1.4-0.8 dpi=[300-] paper!=A2}"
            <> " {description \"Caf%C3%A9, etc.\" fr} {x-note x=(a,\"b}c\")}}"
        second = "{\"fallback\"}, proxy-rvsa=\"1.0, 2.5\", x-directive=\"a, b\""
        tag name = either error id (AF.parseFeatureTag name)
        predicate name condition = Alt.FeaturePredicate (tag name) condition
        requirement name condition = Alt.RequiredPredicate (predicate name condition)
        noFactors = Alt.FeatureFactors Nothing Nothing
        version major minor = maybe (error "invalid fixture RVSA version") id (Neg.mkRvsaVersion major minor)
        attrs =
          [ Alt.VariantType (MediaRange (MediaType "text" "html") [("profile", "a,b")])
          , Alt.VariantCharset "utf-8"
          , Alt.VariantLanguages (LanguageTag "en" :| [LanguageTag "fr-CA"])
          , Alt.VariantLength 18446744073709551616
          , Alt.VariantFeatures
              ( Alt.VariantFeature (requirement "textonly" AF.FeatureAbsent) noFactors
                  :| [ Alt.VariantFeature
                         (Alt.AnyPredicate (predicate "blebber" AF.FeaturePresent :| [predicate "wolx" AF.FeatureAbsent]))
                         (Alt.FeatureFactors (Alt.mkShortFloat 1400) (Alt.mkShortFloat 800))
                     , Alt.VariantFeature (requirement "dpi" (AF.FeatureSpecial (Alt.FeatureRange (Just 300) Nothing))) noFactors
                     , Alt.VariantFeature (requirement "paper" (AF.FeatureUnequal (AF.FeatureValue "A2"))) noFactors
                     ]
              )
          , Alt.VariantDescriptionAttribute (Alt.VariantDescription "Café, etc." (Just (LanguageTag "fr")))
          , Alt.VariantExtension "x-note" " x=(a,\"b}c\")"
          ]
        entries =
          Alt.AlternateVariant (Alt.Variant "../paper%20one" 0.9 attrs)
            :| [Alt.AlternateFallback "fallback", Alt.AlternateProxyRVSA [version 1 0, version 2 5], Alt.AlternateDirective "x-directive" (Just "a, b")]
        fields = headerMapFromList [("Alternates", first), ("Alternates", second)]
    case lookupHeader @Alt.Alternates fields of
      Right (Just value) -> do
        Alt.alternateEntries value `shouldBe` Right entries
        lookupRawHeader "Alternates" (setHeader value (headerMapFromList []))
          `shouldBe` Just ((first <> ", " <> second) :| [])
      result -> expectationFailure (show result)
    case Alt.alternatesFromEntries entries of
      Right value -> do
        Alt.alternateEntries value `shouldBe` Right entries
        (lookupHeader @Alt.Alternates (setHeader value (headerMapFromList [])) >>= traverse Alt.alternateEntries)
          `shouldBe` Right (Just entries)
      Left err -> expectationFailure err
  it "preserves repeated variants and supports empty proxy versions and unbounded ranges" $ do
    let lookupAlternates fields = lookupHeader @Alt.Alternates (headerMapFromList (map (\value -> ("Alternates", value)) fields))
    (lookupAlternates ["", "proxy-rvsa=\"\""] >>= traverse Alt.alternateEntries)
      `shouldBe` Right (Just (Alt.AlternateProxyRVSA [] :| []))
    lookupAlternates ["{\"same\" 1 {features dpi=[-] dpi=[-599]}}", "{\"same\" 0 {features dpi=[ 4 - 6 ];-999.999}}"]
      `shouldSatisfy` either (const False) (maybe False (const True))
    Alt.mkShortFloat 1000000 `shouldBe` Nothing
  it "accepts an Apache mod_negotiation variant list with an encoding extension" $ do
    let wire =
          "{\"test.en.html\" 0.9 {type text/html} {charset utf-8} {language en} {length 17}}, "
            <> "{\"test.fr.html\" 0.7 {type text/html} {charset utf-8} {language fr-ca} {encoding gzip} {length 23}}"
        variant uri quality lang rest =
          Alt.AlternateVariant
            ( Alt.Variant
                uri
                quality
                (Alt.VariantType (MediaRange (MediaType "text" "html") []) : Alt.VariantCharset "utf-8" : Alt.VariantLanguages (LanguageTag lang :| []) : rest)
            )
        expected =
          variant "test.en.html" 0.9 "en" [Alt.VariantLength 17]
            :| [variant "test.fr.html" 0.7 "fr-ca" [Alt.VariantExtension "encoding" " gzip", Alt.VariantLength 23]]
    case lookupHeader @Alt.Alternates (headerMapFromList [("Alternates", wire)]) of
      Right (Just value) -> do
        Alt.alternateEntries value `shouldBe` Right expected
        lookupRawHeader "Alternates" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
      result -> expectationFailure (show result)
  it "rejects invalid known attributes and duplicate fallback across lines" $ do
    let lookupAlternates fields = lookupHeader @Alt.Alternates (headerMapFromList (map (\value -> ("Alternates", value)) fields))
    forM_
      [ ""
      , ", ,"
      , "{\"bad space\" 1}"
      , "{\"bad%2g\" 1}"
      , "{\"a\" 1.1}"
      , "{\"a\" 0.1234}"
      , "{\"a\" 1 {type */*}}"
      , "{\"a\" 1 {type text/html;charset=ascii}}"
      , "{\"a\" 1 {length -1}}"
      , "{\"a\" 1 {length 1} {LENGTH 2}}"
      , "{\"a\" 1 {x-mode one} {X-MODE two}}"
      , "{\"a\" 1 {language , }}"
      , "{\"a\" 1 {features }}"
      , "{\"a\" 1 {features []}}"
      , "{\"a\" 1 {features dpi=[3]}}"
      , "{\"a\" 1 {features blink;+1000}}"
      , "{\"a\" 1 {features blink;-0.0001}}"
      , "{\"a\" 1 {description \"%FF\"}}"
      , "{\"a\" 1 {x-note \"unclosed}}"
      , "proxy-rvsa=1.0"
      , "proxy-rvsa=\"1.0, 12345.1\""
      ]
      $ \wire -> lookupAlternates [wire] `shouldSatisfy` isLeft
    lookupAlternates ["{\"fallback\"}", "{\"other\"}"] `shouldSatisfy` isLeft


tests :: Spec
tests =
  describe "TCN" $
    sequence_
      [ unit_tcn_parse
      , unit_tcn_render
      , it "TCN round-trip" prop_tcn_roundtrip
      , typedTCN
      , unit_negotiate_parse
      , unit_negotiate_render
      , it "Negotiate round-trip" prop_negotiate_roundtrip
      , typedNegotiate
      , unit_variantvary_parse_all
      , unit_variantvary_parse_fields
      , unit_variantvary_render
      , it "Variant-Vary round-trip" prop_variantvary_roundtrip
      , typedVariantVary
      , unit_acceptadditions_parse
      , unit_acceptadditions_render
      , typedAcceptAdditions
      , it "Accept-Additions round-trip" prop_acceptadditions_roundtrip
      , unit_acceptfeatures_parse
      , typedAcceptFeatures
      , unit_acceptfeatures_render
      , unit_alternates_parse
      , typedAlternates
      , unit_alternates_render
      ]
