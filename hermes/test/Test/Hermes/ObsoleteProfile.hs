{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteProfile (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Grammar.PICS
import Network.HTTP.Headers (Cardinality, HeaderRenderingResultToNonEmpty, KnownHeader, defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader, setRawHeader)
import qualified Network.HTTP.Headers.GetProfile as GP
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, hGetProfile, hP3P, hPICSLabel, hProfileObject, hSetProfile)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.P3P as P3
import qualified Network.HTTP.Headers.PICSLabel as PL
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.ProfileObject as PO
import qualified Network.HTTP.Headers.SetProfile as SP
import Test.Syd
import Test.Syd.Hedgehog ()


{- | Shared generator of opaque header values: non-empty printable ASCII.
These headers preserve their value verbatim, so any such string round-trips.
-}
genValue :: Gen ST.ShortText
genValue =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 48)
      (Gen.element ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ()=\";:/.,-_*" :: String))


-- GetProfile -----------------------------------------------------------------

unit_getProfile_parse :: Spec
unit_getProfile_parse = it "parses GetProfile value verbatim" $
  case runParser GP.getProfileParser "\"*\" \"user.name\"" of
    OK v "" -> GP.getProfileValue v `shouldBe` ST.fromString "\"*\" \"user.name\""
    other -> error (show other)


unit_getProfile_render :: Spec
unit_getProfile_render =
  it "renders GetProfile value verbatim" $
    M.toStrictByteString (GP.renderGetProfile (GP.GetProfile (ST.fromString "\"*\" \"user.name\"")))
      `shouldBe` "\"*\" \"user.name\""


prop_getProfile :: Property
prop_getProfile = property $ do
  v <- forAll genValue
  let val = GP.GetProfile v
      bs = M.toStrictByteString (GP.renderGetProfile val)
  case runParser GP.getProfileParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


-- SetProfile -----------------------------------------------------------------

unit_setProfile_parse :: Spec
unit_setProfile_parse = it "parses SetProfile value verbatim" $
  case runParser SP.setProfileParser "\"user.bdate=1970.01.01\"" of
    OK v "" -> SP.setProfileValue v `shouldBe` ST.fromString "\"user.bdate=1970.01.01\""
    other -> error (show other)


unit_setProfile_render :: Spec
unit_setProfile_render =
  it "renders SetProfile value verbatim" $
    M.toStrictByteString (SP.renderSetProfile (SP.SetProfile (ST.fromString "\"user.bdate=1970.01.01\"")))
      `shouldBe` "\"user.bdate=1970.01.01\""


prop_setProfile :: Property
prop_setProfile = property $ do
  v <- forAll genValue
  let val = SP.SetProfile v
      bs = M.toStrictByteString (SP.renderSetProfile val)
  case runParser SP.setProfileParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


-- ProfileObject --------------------------------------------------------------

unit_profileObject_parse :: Spec
unit_profileObject_parse = it "parses ProfileObject value verbatim" $
  case runParser PO.profileObjectParser "MIIBkzCCAX2gAwIBAgIBADAN" of
    OK v "" -> PO.profileObjectValue v `shouldBe` ST.fromString "MIIBkzCCAX2gAwIBAgIBADAN"
    other -> error (show other)


unit_profileObject_render :: Spec
unit_profileObject_render =
  it "renders ProfileObject value verbatim" $
    M.toStrictByteString (PO.renderProfileObject (PO.ProfileObject (ST.fromString "MIIBkzCCAX2gAwIBAgIBADAN")))
      `shouldBe` "MIIBkzCCAX2gAwIBAgIBADAN"


prop_profileObject :: Property
prop_profileObject = property $ do
  v <- forAll genValue
  let val = PO.ProfileObject v
      bs = M.toStrictByteString (PO.renderProfileObject val)
  case runParser PO.profileObjectParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


unit_opsWire :: Spec
unit_opsWire = describe "OPS HTTP field framing" $ do
  it "checks every octet and preserves accepted wire values through all three header codecs" $
    forM_ [0 .. 255] $ \byte -> do
      let wire = " \tpre" <> BS.singleton byte <> "post "
          accepted = byte == 9 || (byte >= 32 && byte /= 127)
          readValue name = headerMapFromList [(name, wire)]
          get = lookupHeader @GP.GetProfile (readValue "GetProfile")
          set = lookupHeader @SP.SetProfile (readValue "SetProfile")
          obj = lookupHeader @PO.ProfileObject (readValue "ProfileObject")
      if accepted
        then do
          let rendered name value = lookupRawHeader name (setHeader value (headerMapFromList []))
          fmap (fmap (rendered hGetProfile)) get `shouldBe` Right (Just (Just (wire :| [])))
          fmap (fmap (rendered hSetProfile)) set `shouldBe` Right (Just (Just (wire :| [])))
          fmap (fmap (rendered hProfileObject)) obj `shouldBe` Right (Just (Just (wire :| [])))
          fmap GP.getProfileBytes (GP.getProfileFromBytes wire) `shouldBe` Right wire
          fmap SP.setProfileBytes (SP.setProfileFromBytes wire) `shouldBe` Right wire
          fmap PO.profileObjectBytes (PO.profileObjectFromBytes wire) `shouldBe` Right wire
        else do
          get `shouldSatisfy` isLeft
          set `shouldSatisfy` isLeft
          obj `shouldSatisfy` isLeft
          GP.getProfileFromBytes wire `shouldSatisfy` isLeft
          SP.setProfileFromBytes wire `shouldSatisfy` isLeft
          PO.profileObjectFromBytes wire `shouldSatisfy` isLeft
  it "validates every byte position in a bounded, unaligned field slice" $
    forM_ [0 .. 16] $ \offset ->
      forM_ [0 .. 255] $ \byte -> do
        let pad = BS.pack [9, 32, 126, 128, 255, 32, 9, 255, 254, 128, 32, 33, 126, 9, 255, 32]
            wire = BS.take 17 (BS.drop 1 (BS.singleton 0 <> BS.take offset pad <> BS.singleton byte <> BS.drop offset pad <> BS.singleton 127))
        if byte == 9 || (byte >= 32 && byte /= 127)
          then fmap PO.profileObjectBytes (PO.profileObjectFromBytes wire) `shouldBe` Right wire
          else PO.profileObjectFromBytes wire `shouldSatisfy` isLeft
  it "has a safe legacy display for opaque bytes and rejects repeated field lines" $ do
    let wire = BS.pack [255, 128]
    case PO.profileObjectFromBytes wire of
      Right obj -> do
        PO.profileObjectValue obj `shouldBe` ST.fromString "\255\128"
        M.toStrictByteString (PO.renderProfileObject obj) `shouldBe` wire
      result -> expectationFailure (show result)
    lookupHeader @GP.GetProfile (headerMapFromList [("GetProfile", "one"), ("GetProfile", "two")]) `shouldSatisfy` isLeft
    lookupHeader @SP.SetProfile (headerMapFromList [("SetProfile", "one"), ("SetProfile", "two")]) `shouldSatisfy` isLeft
    lookupHeader @PO.ProfileObject (headerMapFromList [("ProfileObject", "one"), ("ProfileObject", "two")]) `shouldSatisfy` isLeft


-- PICS-Label -----------------------------------------------------------------

picsExample :: ByteString
picsExample = "(PICS-1.1 \"http://www.gcf.org/v2.5\" labels ratings (s 0))"


unit_picsLabel :: Spec
unit_picsLabel = describe "PICS-Label" $ do
  let readLabel wire = lookupHeader @PL.PICSLabel (headerMapFromList [("PICS-Label", wire)])
      readLabelList wire = readLabel wire >>= traverse PL.picsLabelList
      number = either error id . parsePicsNumber
  it "validates checked writes from the legacy text pattern" $ do
    let previous = headerMapFromList [("PICS-Label", picsExample)]
        bad = PL.PICSLabel "(PICS-1.1 \"relative\" labels ratings (s 0))"
    renderHeaderLines defaultHeaderSettings bad `shouldBe` Nothing
    setHeader bad previous `shouldBe` previous
  it "parses the response field and retains the accepted spelling through setHeader" $ do
    let emptyRating = PicsRating "s" (PicsScalar (number "0"))
        expected = PicsLabelList (PicsServiceLabels "http://www.gcf.org/v2.5" [] [PicsLabel (PicsSingleLabel [] (emptyRating :| []))] :| [])
        wire = "\t (pics-1.1 \"http://www.gcf.org/v2.5\" L r(s 0)) \t"
    readLabelList picsExample `shouldBe` Right (Just expected)
    case readLabel wire of
      Right (Just header) -> do
        PL.picsLabelList header `shouldBe` Right expected
        lookupRawHeader hPICSLabel (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
        M.toStrictByteString (PL.renderPICSLabel header) `shouldBe` wire
      result -> expectationFailure (show result)
    fmap (M.toStrictByteString . PL.renderPICSLabel) (PL.picsLabelFromList expected) `shouldBe` Right picsExample
    PL.picsLabelList (PL.PICSLabel "invalid") `shouldSatisfy` isLeft
  it "retains calendar timestamps, a signed offset, and exact rating spellings" $ do
    let wire = "(PICS-1.1 \"http://p.test/svc\" labels on \"1994.11.05T08:15-0500\" exp \"2000.02.29T23:60+1512\" r (color/hue +1. s (-0.0:1.5 2) empty () a%2Fb 3))"
        ratings =
          PicsRating "color/hue" (PicsScalar (number "+1."))
            :| [ PicsRating "s" (PicsMultiple [PicsRange (number "-0.0") (number "1.5"), PicsExact (number "2")])
               , PicsRating "empty" (PicsMultiple [])
               , PicsRating "a%2Fb" (PicsScalar (number "3"))
               ]
        options = [PicsOn (PicsDate 1994 11 5 8 15 (-300)), PicsUntil (PicsDate 2000 2 29 23 60 912)]
        expected = PicsLabelList (PicsServiceLabels "http://p.test/svc" [] [PicsLabel (PicsSingleLabel options ratings)] :| [])
    readLabelList wire `shouldBe` Right (Just expected)
    picsNumberValue (number "+1.") `shouldBe` 1
    isNegativeZero (picsNumberValue (number "-0.0")) `shouldBe` True
    picsNumberValue (number "1.5") `shouldBe` 1.5
    picsNumberValue (number ("0." <> BS.replicate 500 48 <> "1")) `shouldBe` 0
    isNegativeZero (picsNumberValue (number ("-0." <> BS.replicate 500 48 <> "1"))) `shouldBe` True
    (PL.picsLabelFromList expected >>= PL.picsLabelList) `shouldBe` Right expected
  it "inherits service options across individual labels and grouped trees" $ do
    let uri = "http://p.test/svc"
        wire = "(PICS-1.1 \"http://p.test/svc\" gen t for \"https://p.test/a/\" labels (r (s 1) generic f r (s 2)) () r (s 3))"
        label opts value = PicsSingleLabel opts (PicsRating "s" (PicsScalar (number value)) :| [])
        expected =
          PicsLabelList
            ( PicsServiceLabels
                uri
                [PicsGeneric True, PicsFor "https://p.test/a/"]
                [PicsLabelTree [label [] "1", label [PicsGeneric False] "2"], PicsLabelTree [], PicsLabel (label [] "3")]
                :| []
            )
    readLabelList wire `shouldBe` Right (Just expected)
    readLabelList "(PICS-1.1 \"http://p.test/svc\" generic true l r (s 1))" `shouldSatisfy` isLeft
    readLabelList "(PICS-1.1 \"http://p.test/svc\" for \"http://p.test\" l generic true r (s 1))"
      `shouldSatisfy` either (const False) (maybe False (const True))
  it "distinguishes service errors, label errors and a no-ratings entry" $ do
    let wire =
          "(PICS-1.1 \"https://p.test/deny\" error (request-denied \"wrong scope\") \
          \\"https://p.test/off\" error service-unavailable \
          \\"https://p.test/l\" l error (not-labeled \"https://p.test/doc\") error (request-denied) \
          \error (request-denied \"https://p.test/q\" \"scope\") error (no-ratings \"unknown service\"))"
        expected =
          PicsLabelList
            ( PicsServiceFailure "https://p.test/deny" (PicsServiceDenied ["wrong scope"])
                :| [ PicsServiceFailure "https://p.test/off" PicsServiceUnavailable
                   , PicsServiceLabels
                       "https://p.test/l"
                       []
                       [ PicsLabelFailure (PicsNotLabeled ["https://p.test/doc"])
                       , PicsLabelFailure (PicsLabelDenied Nothing [])
                       , PicsLabelFailure (PicsLabelDenied (Just "https://p.test/q") ["scope"])
                       ]
                   , PicsNoRatings ["unknown service"]
                   ]
            )
    readLabelList wire `shouldBe` Right (Just expected)
    (PL.picsLabelFromList expected >>= PL.picsLabelList) `shouldBe` Right expected
  it "decodes Base64 and distinguishes nested extension names, URLs, dates and decimals" $ do
    let wire =
          "(PICS-1.1 \"https://p.test/l\" l MIC-md5 \"AAAAAAAAAAAA AAAAAAAAAA==\" \
          \signature-RSA-MD5 \"AQI=\" comment \"one\" comment \"two\" \
          \extension (mandatory \"https://p.test/ext\" (\"comment%22\" -1. \"http://p.test/path\" \"2004.02.29T12:30+0000\")) \
          \extension (optional \"https://p.test/ext2\") r(s 0))"
        options =
          [ PicsMIC (BS.replicate 16 0)
          , PicsSignature (BS.pack [1, 2])
          , PicsComment "one"
          , PicsComment "two"
          , PicsExtension
              PicsMandatory
              "https://p.test/ext"
              [PicsDataList [PicsDataName "comment%22", PicsDataNumber (number "-1."), PicsDataURL "http://p.test/path", PicsDataDate (PicsDate 2004 2 29 12 30 0)]]
          , PicsExtension PicsOptional "https://p.test/ext2" []
          ]
        expected = PicsLabelList (PicsServiceLabels "https://p.test/l" [] [PicsLabel (PicsSingleLabel options (PicsRating "s" (PicsScalar (number "0")) :| []))] :| [])
    readLabelList wire `shouldBe` Right (Just expected)
    (PL.picsLabelFromList expected >>= PL.picsLabelList) `shouldBe` Right expected
  it "rejects invalid dates, duplicate options, duplicate categories, control bytes, and repeated field lines" $ do
    forM_
      [ ""
      , "(PICS-1.0 \"http://p.test/l\" l)"
      , "(PICS-1.1)"
      , "(PICS-1.1 \"rel\" l)"
      , "(PICS-1.1 \"http://p.test/l\" l r ())"
      , "(PICS-1.1 \"http://p.test/l\" l r (s 0 s 1))"
      , "(PICS-1.1 \"http://p.test/l\" l r (a/ 0))"
      , "(PICS-1.1 \"http://p.test/l\" l r (s 1e2))"
      , "(PICS-1.1 \"http://p.test/l\" l r (s (1:)))"
      , "(PICS-1.1 \"http://p.test/l\" l by \"bad%G0\" r (s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l exp \"1900.02.29T23:00+0000\" r (s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l exp \"2000.02.29t23:00+0000\" r (s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l exp \"2000.02.29T23:00+0060\" r (s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l exp \"2000.02.29T24:00+0000\" r (s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l md5 \"AQI=\" r(s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l signature-RSA-MD5 \"AQ\" r(s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l gen f generic false r(s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l extension (optional \"http://x.test/\") extension (mandatory \"http://x.test/\") r(s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l comment \"\255\" r(s 0))"
      , "(PICS-1.1 \"http://p.test/l\" l)\r\n "
      ]
      $ \wire -> readLabel wire `shouldSatisfy` isLeft
    lookupHeader @PL.PICSLabel (headerMapFromList [("PICS-Label", picsExample), ("PICS-Label", picsExample)])
      `shouldSatisfy` isLeft
    forM_ ["", ".", "+", ".5", "1..", "1e5", "Infinity", "+9999999999999999999999999999999999999999"] $ \value ->
      parsePicsNumber value `shouldSatisfy` isLeft


prop_picsLabel :: Property
prop_picsLabel = property $ do
  whole <- forAll (Gen.int (Range.linear (-100000) 100000))
  remainder <- forAll (Gen.int (Range.linear 0 999))
  let wire = BSC.pack (show whole <> "." <> show remainder)
      scalar = either error id (parsePicsNumber wire)
      item = PicsRating "subject" (PicsMultiple [PicsRange scalar scalar])
      entries = PicsLabelList (PicsServiceLabels "http://p.test/l" [PicsBy "it%27s"] [PicsLabel (PicsSingleLabel [] (item :| []))] :| [])
  (PL.picsLabelFromList entries >>= PL.picsLabelList) === Right entries
  picsNumberBytes scalar === wire


-- P3P ------------------------------------------------------------------------

unit_p3p :: Spec
unit_p3p = describe "P3P" $ do
  it "validates checked writes from authored text" $ do
    let previous = headerMapFromList [("P3P", "CP=\"NON\"")]
        bad = P3.P3P "CP=NON"
    renderHeaderLines defaultHeaderSettings bad `shouldBe` Nothing
    setHeader bad previous `shouldBe` previous
  let readPolicy wire = lookupHeader @P3.P3P (headerMapFromList [("P3P", wire)])
      resolve wire = readPolicy wire >>= traverse P3.p3pDirectives
      known claim = P3.P3PKnownCompact claim Nothing
      allCodes = "NOI ALL CAO IDC OTI NON DSP COR MON LAW NID CUR ADM DEV TAI PSA PSD IVA IVD CON HIS TEL OTP OUR DEL SAM UNR PUB OTR NOR STP LEG BUS IND PHY ONL UNI PUR FIN COM NAV INT DEM CNT STA POL HEA PRE LOC GOV OTC TST"
  it "decodes policy references, known claims, qualifiers and unknown directives" $ do
    let wire = "\tpolicyref = \"/w3c/p3p.xml#opts\" , CP=\"NON DSP COR ADMa DEVi IVDo OUR IND HEA TST VendorX CURa\", X=\"a,\\\"b\255\" \t"
        expected =
          P3.P3PPolicyRef "/w3c/p3p.xml#opts"
            :| [ P3.P3PCompactPolicy
                   ( known (P3.P3PAccess P3.P3PNone)
                       :| [ known P3.P3PDisputes
                          , known (P3.P3PRemedy P3.P3PCorrect)
                          , P3.P3PKnownCompact (P3.P3PPurpose P3.P3PAdmin) (Just P3.P3PAlways)
                          , P3.P3PKnownCompact (P3.P3PPurpose P3.P3PDevelop) (Just P3.P3POptIn)
                          , P3.P3PKnownCompact (P3.P3PPurpose P3.P3PIndividualDecision) (Just P3.P3POptOut)
                          , known (P3.P3PRecipient P3.P3POurs)
                          , known (P3.P3PRetention P3.P3PIndefinitely)
                          , known (P3.P3PCategory P3.P3PHealth)
                          , known P3.P3PTest
                          , P3.P3PUnknownCompact "VendorX"
                          , P3.P3PUnknownCompact "CURa"
                          ]
                   )
               , P3.P3PExtension "X" (Just (P3.P3PExtensionQuoted "a,\"b\255"))
               ]
    case readPolicy wire of
      Right (Just header) -> do
        P3.p3pDirectives header `shouldBe` Right expected
        lookupRawHeader hP3P (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
        M.toStrictByteString (P3.renderP3P header) `shouldBe` wire
      result -> expectationFailure (show result)
    (P3.p3pFromDirectives expected >>= P3.p3pDirectives) `shouldBe` Right expected
  it "recognizes the entire published vocabulary and renders it without changes" $ do
    let wire = "CP=\"" <> allCodes <> "\""
    case readPolicy wire of
      Right (Just header) -> do
        fmap (fmap length) (P3.p3pEffectiveCompactPolicy header) `shouldBe` Right (Just (length (BSC.words allCodes)))
        fmap (M.toStrictByteString . P3.renderP3P) (P3.p3pDirectives header >>= P3.p3pFromDirectives)
          `shouldBe` Right wire
      result -> expectationFailure (show result)
  it "keeps all field lines and chooses the first compact policy in order" $ do
    let first = "policyref=\"/p3p.xml\", cp=\"case sensitive\", fallback"
        second = " CP=\"ZZZ NON NON DEVi devI OURi\", CP=\"ALL\" "
        third = "CP=\"TST\""
        expected = [known (P3.P3PAccess P3.P3PNone), P3.P3PKnownCompact (P3.P3PPurpose P3.P3PDevelop) (Just P3.P3POptIn)]
    case lookupHeader @P3.P3P (headerMapFromList [("P3P", first), ("P3P", second), ("P3P", third)]) of
      Right (Just header) -> do
        P3.p3pEffectiveCompactPolicy header `shouldBe` Right (Just expected)
        lookupRawHeader hP3P (setHeader header (headerMapFromList [])) `shouldBe` Just (first :| [second, third])
      result -> expectationFailure (show result)
    resolve "cp=whatever, POLICYREF=x"
      `shouldBe` Right
        (Just (P3.P3PExtension "cp" (Just (P3.P3PExtensionToken "whatever")) :| [P3.P3PExtension "POLICYREF" (Just (P3.P3PExtensionToken "x"))]))
    (readPolicy "CP=\"future\"" >>= traverse P3.p3pEffectiveCompactPolicy) `shouldBe` Right (Just (Just []))
    (readPolicy "policyref=\"\"" >>= traverse P3.p3pEffectiveCompactPolicy) `shouldBe` Right (Just Nothing)
  it "rejects invalid wire syntax and checked authoring with noncanonical typed values" $ do
    forM_
      [ ""
      , ",,"
      , "policyref"
      , "policyref=/p3p.xml"
      , "policyref=\"/bad%XY\""
      , "policyref=\"/has space\""
      , "CP"
      , "CP=NON"
      , "CP=\"\""
      , "CP=\"NON  DSP\""
      , "CP=\" NON\""
      , "CP=\"NON \""
      , "CP=\"NON\tDSP\""
      , "CP=\"\\NON\""
      , "x="
      , "x=\"unterminated"
      , "CP=\"NON\"\r\n"
      , "CP=\"NON\", [bad]"
      ]
      $ \wire -> readPolicy wire `shouldSatisfy` isLeft
    lookupHeader @P3.P3P (headerMapFromList [("P3P", "CP=\"NON\""), ("P3P", "CP=")]) `shouldSatisfy` isLeft
    P3.p3pDirectives (P3.P3P "CP=") `shouldSatisfy` isLeft
    forM_
      [ P3.P3PPolicyRef "/bad%XZ"
      , P3.P3PCompactPolicy (P3.P3PKnownCompact (P3.P3PPurpose P3.P3PCurrent) (Just P3.P3PAlways) :| [])
      , P3.P3PCompactPolicy (P3.P3PUnknownCompact "NON" :| [])
      , P3.P3PExtension "bad/name" Nothing
      , P3.P3PExtension "X" (Just (P3.P3PExtensionQuoted "\r"))
      ]
      $ \bad -> P3.p3pFromDirectives (bad :| []) `shouldSatisfy` isLeft


prop_p3p :: Property
prop_p3p = property $ do
  octets <- forAll (BS.pack <$> Gen.list (Range.linear 0 64) (Gen.element (9 : [32 .. 126] <> [128 .. 255])))
  qualifier <- forAll (Gen.maybe (Gen.element [P3.P3PAlways, P3.P3POptIn, P3.P3POptOut]))
  let entries =
        P3.P3PCompactPolicy (P3.P3PKnownCompact (P3.P3PRecipient P3.P3POtherRecipient) qualifier :| [P3.P3PUnknownCompact "vendor"])
          :| [P3.P3PExtension "test" (Just (P3.P3PExtensionQuoted octets))]
  (P3.p3pFromDirectives entries >>= P3.p3pDirectives) === Right entries


badProfileText :: [ST.ShortText]
badProfileText = ["a\n", "a\r", "a\NUL", "a\DEL", "a\x0b"]


rejectProfile :: (KnownHeader a, HeaderRenderingResultToNonEmpty (Cardinality a)) => HeaderFieldName -> a -> Expectation
rejectProfile name value = do
  let fields = setRawHeader name ("old" :| ["other"]) (headerMapFromList [])
  renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
  setHeader value fields `shouldBe` fields


tests :: Spec
tests =
  describe "ObsoleteProfile" $
    sequence_
      [ it "checks authored GetProfile framing" $ mapM_ (rejectProfile hGetProfile . GP.GetProfile) badProfileText
      , it "checks authored SetProfile framing" $ mapM_ (rejectProfile hSetProfile . SP.SetProfile) badProfileText
      , it "checks authored ProfileObject framing" $ mapM_ (rejectProfile hProfileObject . PO.ProfileObject) badProfileText
      , unit_getProfile_parse
      , unit_getProfile_render
      , it "GetProfile round-trip" prop_getProfile
      , unit_setProfile_parse
      , unit_setProfile_render
      , it "SetProfile round-trip" prop_setProfile
      , unit_profileObject_parse
      , unit_profileObject_render
      , it "ProfileObject round-trip" prop_profileObject
      , unit_opsWire
      , unit_picsLabel
      , it "PICS-Label round-trip" prop_picsLabel
      , unit_p3p
      , it "P3P round-trip" prop_p3p
      ]
