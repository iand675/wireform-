{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DefactoMisc (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), RFC8941String (..), RFC8941Token (..), Result (..), StructuredFieldParameter (..), runParser)
import qualified Network.HTTP.Headers.SaveData as SD
import qualified Network.HTTP.Headers.XPoweredBy as XP
import qualified Network.HTTP.Headers.XRobotsTag as XR
import qualified Network.HTTP.Headers.XUACompatible as UA
import Test.Syd
import Test.Syd.Hedgehog ()


-- | True when only optional whitespace is left over.
clean :: ByteString -> Bool
clean = BS.null . BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseWith :: (ByteString -> Result String a) -> ByteString -> Either String a
parseWith run bs = case run bs of
  OK v leftover
    | clean leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


-- Token generator within the RFC 9110 token grammar (no '=', ',', ':' or SP).
genToken :: Gen ST.ShortText
genToken = ST.fromString <$> Gen.string (Range.linear 1 8) tchar
  where
    tchar = Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-.")


------------------------------------------------------------------------
-- X-UA-Compatible
------------------------------------------------------------------------

uaParse :: ByteString -> Either String UA.XUACompatible
uaParse = parseWith (runParser UA.xUACompatibleParser)


uaRender :: UA.XUACompatible -> ByteString
uaRender = M.toStrictByteString . UA.renderXUACompatible


unit_ua_parse :: Spec
unit_ua_parse = it "parses IE=edge,chrome=1" $
  case uaParse "IE=edge,chrome=1" of
    Right (UA.XUACompatible (a :| [b])) -> do
      UA.uaCompatEngine a `shouldBe` ST.fromString "IE"
      UA.uaCompatMode a `shouldBe` ST.fromString "edge"
      UA.uaCompatEngine b `shouldBe` ST.fromString "chrome"
      UA.uaCompatMode b `shouldBe` ST.fromString "1"
    other -> error (show other)


unit_ua_render :: Spec
unit_ua_render =
  it "renders engine=mode lists through both APIs" $ do
    let edge = UA.XUACompatible (UA.UACompatDirective "IE" "edge" :| [])
        longList = UA.XUACompatible (UA.UACompatDirective "iE" "EDGE" :|
          [UA.UACompatDirective "chrome" "1", UA.UACompatDirective "" (ST.fromString (replicate 4096 'é'))])
        typed :: UA.XUACompatible -> Maybe (NonEmpty ByteString)
        typed value = lookupRawHeader "X-UA-Compatible" (setHeader value (headerMapFromList []))
    uaRender edge `shouldBe` "IE=edge"
    typed edge `shouldBe` Just ("IE=edge" :| [])
    typed longList `shouldBe` Just (uaRender longList :| [])


unit_ua_field_lines :: Spec
unit_ua_field_lines = it "preserves every browser mode and exposes typed IE declarations" $ do
  let readModes = lookupHeader @UA.XUACompatible . headerMapFromList . map (\wire -> ("X-UA-Compatible", wire))
      fields = [", iE=EmulateIE8", "IE=9, , IE=EDGE, chrome=1, IE=other,"]
      previous = headerMapFromList [("X-UA-Compatible", "IE=7"), ("X-UA-Compatible", "chrome=1")]
  case readModes fields of
    Right (Just value) -> do
      map UA.ieDocumentMode (NE.toList (UA.uaCompatDirectives value))
        `shouldBe` [Just (UA.IEEmulate 8), Just (UA.IEVersion 9), Just UA.IEEdge, Nothing, Nothing]
      lookupRawHeader "X-UA-Compatible" (setHeader value previous)
        `shouldBe` Just ("iE=EmulateIE8, IE=9, IE=EDGE, chrome=1, IE=other" :| [])
    outcome -> expectationFailure (show outcome)
  mapM_ (\fields' -> readModes fields' `shouldSatisfy` isLeft)
    [[",", "\t,"], ["IE=7", "chrome="], ["IE=7", "IE=\"edge\""], ["IE=7", "IE=edge\xff"]]
  UA.ieDocumentMode (UA.UACompatDirective "IE" "EmulateIE11") `shouldBe` Just (UA.IEEmulate 11)
  UA.ieDocumentMode (UA.UACompatDirective "iE" "eMuLaTeIe0007") `shouldBe` Just (UA.IEEmulate 7)
  UA.ieDocumentMode (UA.UACompatDirective "IE" "000") `shouldBe` Just (UA.IEVersion 0)
  UA.ieDocumentMode (UA.UACompatDirective "IE" "EmulateIE") `shouldBe` Nothing
  UA.ieDocumentMode (UA.UACompatDirective "IE" "+11") `shouldBe` Nothing
  UA.ieDocumentMode (UA.UACompatDirective "IE" "99999999999999999999999") `shouldBe` Nothing
  let spellings = map ST.fromString . traverse (\c -> [c, toUpper c])
  mapM_ (\engine -> mapM_ (checkEdge engine) (spellings "edge")) (spellings "ie")
  where
    checkEdge engine mode = do
      let directive = UA.UACompatDirective engine mode
          value = UA.XUACompatible (directive :| [])
          wire = ST.toByteString engine <> "=" <> ST.toByteString mode
      UA.ieDocumentMode directive `shouldBe` Just UA.IEEdge
      lookupRawHeader "X-UA-Compatible" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])


genUA :: Gen UA.XUACompatible
genUA = UA.XUACompatible <$> Gen.nonEmpty (Range.linear 1 4) directive
  where
    directive = UA.UACompatDirective <$> genToken <*> genToken


prop_ua_roundtrip :: Property
prop_ua_roundtrip = property $ do
  v <- forAll genUA
  let bs = uaRender v
  lookupRawHeader "X-UA-Compatible" (setHeader v (headerMapFromList [])) === Just (bs :| [])
  case uaParse bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


------------------------------------------------------------------------
-- X-Powered-By
------------------------------------------------------------------------

xpParse :: ByteString -> Either String XP.XPoweredBy
xpParse = parseWith (runParser XP.xPoweredByParser)


xpRender :: XP.XPoweredBy -> ByteString
xpRender = M.toStrictByteString . XP.renderXPoweredBy


unit_xp_parse :: Spec
unit_xp_parse = it "parses opaque product string verbatim" $
  case xpParse "PHP/8.2.0" of
    Right (XP.XPoweredBy v) -> v `shouldBe` ST.fromString "PHP/8.2.0"
    other -> error (show other)


unit_xp_render :: Spec
unit_xp_render =
  it "renders opaque product string verbatim" $
    xpRender (XP.XPoweredBy (ST.fromString "Express")) `shouldBe` "Express"


------------------------------------------------------------------------
-- X-Robots-Tag
------------------------------------------------------------------------

xrParse :: ByteString -> Either String XR.XRobotsTag
xrParse = parseWith (runParser XR.xRobotsTagParser)


xrRender :: XR.XRobotsTag -> ByteString
xrRender = M.toStrictByteString . XR.renderXRobotsTag


unit_xr_flags :: Spec
unit_xr_flags = it "parses a bare directive list" $
  case xrParse "noindex, nofollow" of
    Right (XR.XRobotsTag Nothing (a :| [b])) -> do
      XR.robotsDirectiveName a `shouldBe` ST.fromString "noindex"
      XR.robotsDirectiveValue a `shouldBe` Nothing
      XR.robotsDirectiveName b `shouldBe` ST.fromString "nofollow"
      XR.robotsDirectiveValue b `shouldBe` Nothing
    other -> error (show other)


unit_xr_botname :: Spec
unit_xr_botname = it "parses a leading bot-name prefix" $
  case xrParse "googlebot: noindex" of
    Right (XR.XRobotsTag (Just bot) (a :| [])) -> do
      bot `shouldBe` ST.fromString "googlebot"
      XR.robotsDirectiveName a `shouldBe` ST.fromString "noindex"
      XR.robotsDirectiveValue a `shouldBe` Nothing
    other -> error (show other)


unit_xr_valued :: Spec
unit_xr_valued = it "parses a valued directive without treating it as a bot name" $
  case xrParse "max-snippet: 50" of
    Right (XR.XRobotsTag Nothing (a :| [])) -> do
      XR.robotsDirectiveName a `shouldBe` ST.fromString "max-snippet"
      XR.robotsDirectiveValue a `shouldBe` Just (ST.fromString "50")
    other -> error (show other)


unit_xr_render :: Spec
unit_xr_render =
  it "renders bot-name and a valued directive" $
    let v =
          XR.XRobotsTag
            (Just (ST.fromString "googlebot"))
            ( XR.RobotsDirective (ST.fromString "max-snippet") (Just (ST.fromString "50"))
                :| [XR.RobotsDirective (ST.fromString "nofollow") Nothing]
            )
    in xrRender v `shouldBe` "googlebot: max-snippet: 50, nofollow"


unit_xr_field_lines :: Spec
unit_xr_field_lines = it "preserves separate crawler scopes and dates in repeated X-Robots-Tag fields" $ do
  let fields =
        "noimageindex"
          :| [ "Googlebot: nofollow"
             , "unavailable_after: 25 Jun 2010 15:00:00 PST"
             , "otherbot: unavailable_after: Sunday, 06-Nov-94 08:49:37 GMT, noindex"
             ]
      headerMap = headerMapFromList . map (\value -> ("X-Robots-Tag", value))
      globalFlag = XR.RobotsRuleGroup Nothing (XR.RobotsDirective "noimageindex" Nothing :| [])
      google = XR.RobotsRuleGroup (Just "Googlebot") (XR.RobotsDirective "nofollow" Nothing :| [])
      globalDate = XR.RobotsRuleGroup Nothing
        (XR.RobotsDirective "unavailable_after" (Just "25 Jun 2010 15:00:00 PST") :| [])
      other = XR.RobotsRuleGroup (Just "otherbot")
        (XR.RobotsDirective "unavailable_after" (Just "Sunday, 06-Nov-94 08:49:37 GMT")
          :| [XR.RobotsDirective "noindex" Nothing])
      expected = globalFlag :| [google, globalDate, other]
  case lookupHeader @XR.XRobotsTag (headerMap (foldr (:) [] fields)) of
    Right (Just value) -> do
      XR.xRobotsRuleGroups value `shouldBe` expected
      XR.xRobotsDirectives value `shouldBe` XR.robotsGroupDirectives globalFlag
      value `shouldBe` XR.xRobotsTagFromGroups expected
      lookupRawHeader "X-Robots-Tag" (setHeader value (headerMap ["all"])) `shouldBe` Just fields
    outcome -> expectationFailure (show outcome)
  xrParse "UNAVAILABLE_AFTER: 2020-09-21T12:30:00Z, nofollow"
    `shouldBe` Right (XR.XRobotsTag Nothing
      (XR.RobotsDirective "UNAVAILABLE_AFTER" (Just "2020-09-21T12:30:00Z") :| [XR.RobotsDirective "nofollow" Nothing]))
  lookupHeader @XR.XRobotsTag (headerMap ["noindex", "max-video-preview: "]) `shouldSatisfy` isLeft


genBotName :: Gen ST.ShortText
genBotName = Gen.element (map ST.fromString ["googlebot", "bingbot", "otherbot"])


genDirective :: Gen XR.RobotsDirective
genDirective =
  Gen.choice
    [ flip XR.RobotsDirective Nothing
        <$> Gen.element
          (map ST.fromString ["noindex", "nofollow", "none", "noarchive", "nosnippet", "notranslate", "all"])
    , do
        key <- Gen.element (map ST.fromString ["max-snippet", "max-image-preview", "max-video-preview", "unavailable_after"])
        XR.RobotsDirective key . Just <$> genToken
    ]


genXR :: Gen XR.XRobotsTag
genXR = XR.XRobotsTag <$> Gen.maybe genBotName <*> Gen.nonEmpty (Range.linear 1 4) genDirective


prop_xr_roundtrip :: Property
prop_xr_roundtrip = property $ do
  v <- forAll genXR
  let bs = xrRender v
  case xrParse bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


------------------------------------------------------------------------
-- Save-Data
------------------------------------------------------------------------

sdParse :: ByteString -> Either String SD.SaveData
sdParse = parseWith (runParser SD.saveDataParser)


sdRender :: SD.SaveData -> ByteString
sdRender = M.toStrictByteString . SD.renderSaveData


unit_sd_parse :: Spec
unit_sd_parse = it "parses the on token" $
  case sdParse "on" of
    Right (SD.SaveData v) -> v `shouldBe` ST.fromString "on"
    other -> error (show other)


unit_sd_render :: Spec
unit_sd_render =
  it "renders the on token" $
    sdRender (SD.SaveData (ST.fromString "on")) `shouldBe` "on"


unit_sd_fields :: Spec
unit_sd_fields = it "preserves token order and parameters and exposes the final preference" $ do
  let field values = headerMapFromList (map (\bytes -> ("Save-Data", bytes)) values)
      value =
        SD.SaveDataList
          [ SD.SaveDataItem (RFC8941Token "on") [StructuredFieldParameter "label" (String (RFC8941String "a,b"))]
          , SD.SaveDataItem (RFC8941Token "future/value") [StructuredFieldParameter "sample" (Integer 2), StructuredFieldParameter "flag" (Boolean True)]
          ]
      wire = "on;label=\"a,b\", future/value;sample=2;flag"
  lookupHeader @SD.SaveData (field [" \ton;label=\"a,b\"  ", "future/value;sample=1;flag=?1;sample=2\t"])
    `shouldBe` Right (Just value)
  SD.saveDataPreference value `shouldBe` Just (RFC8941Token "future/value")
  SD.saveDataPreference (SD.SaveData "ON") `shouldBe` Just (RFC8941Token "ON")
  SD.saveDataPreference (SD.SaveDataList []) `shouldBe` Nothing
  lookupHeader @SD.SaveData (field []) `shouldBe` Right Nothing
  lookupHeader @SD.SaveData (field [""]) `shouldBe` Right (Just (SD.SaveDataList []))
  lookupRawHeader "Save-Data" (setHeader value (field [])) `shouldBe` Just (wire :| [])
  mapM_
    (\bad -> lookupHeader @SD.SaveData (field bad) `shouldSatisfy` isLeft)
    [["(on)"], ["\"on\""], ["1"], ["on, ?1"], ["on;\tflag"], ["on;flag="], ["on,"], ["on,, off"], ["on", ""]]


tests :: Spec
tests =
  describe "DefactoMisc" $
    sequence_
      [ describe "X-UA-Compatible" $
          sequence_
            [ unit_ua_parse
            , unit_ua_render
            , unit_ua_field_lines
            , it "round-trip" prop_ua_roundtrip
            ]
      , describe "X-Powered-By" $
          sequence_
            [ unit_xp_parse
            , unit_xp_render
            ]
      , describe "X-Robots-Tag" $
          sequence_
            [ unit_xr_flags
            , unit_xr_botname
            , unit_xr_valued
            , unit_xr_render
            , unit_xr_field_lines
            , it "round-trip" prop_xr_roundtrip
            ]
      , describe "Save-Data" $
          sequence_
            [ unit_sd_parse
            , unit_sd_render
            , unit_sd_fields
            ]
      ]
