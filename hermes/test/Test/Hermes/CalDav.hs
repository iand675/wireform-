{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.CalDav (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.CalDAVTimezones as CTZ
import qualified Network.HTTP.Headers.CalManagedID as CMI
import qualified Network.HTTP.Headers.IfScheduleTagMatch as ISTM
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OSLCCoreVersion as OCV
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.ScheduleReply as SR
import qualified Network.HTTP.Headers.ScheduleTag as STg
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a standalone parser and require that it consume the whole input.
parseAll :: ParserT st String a -> ByteString -> Either String a
parseAll p bs = case runParser p bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


-- Generators -----------------------------------------------------------------

genTagText :: Gen ST.ShortText
genTagText = ST.fromString <$> Gen.string (Range.linear 0 24) Gen.alphaNum


genIdText :: Gen ST.ShortText
genIdText =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 16)
      (Gen.element (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"))


genVersion :: Gen ST.ShortText
genVersion = do
  major <- Gen.int (Range.linear 1 9)
  minor <- Gen.int (Range.linear 0 99)
  pure (ST.fromString (show major <> "." <> show minor))


-- Schedule-Reply -------------------------------------------------------------

renderSR :: SR.ScheduleReply -> ByteString
renderSR = M.toStrictByteString . SR.renderScheduleReply


unit_scheduleReply_parse :: Spec
unit_scheduleReply_parse = it "Schedule-Reply parses T and F" $ do
  parseAll SR.scheduleReplyParser "T" `shouldBe` Right (SR.ScheduleReply True)
  parseAll SR.scheduleReplyParser "F" `shouldBe` Right (SR.ScheduleReply False)


unit_scheduleReply_render :: Spec
unit_scheduleReply_render = it "Schedule-Reply renders T/F" $ do
  renderSR (SR.ScheduleReply True) `shouldBe` "T"
  renderSR (SR.ScheduleReply False) `shouldBe` "F"


prop_scheduleReply :: Property
prop_scheduleReply = property $ do
  b <- forAll Gen.bool
  let v = SR.ScheduleReply b
  parseAll SR.scheduleReplyParser (renderSR v) === Right v


-- Schedule-Tag ---------------------------------------------------------------

renderSTg :: STg.ScheduleTag -> ByteString
renderSTg = M.toStrictByteString . STg.renderScheduleTag


unit_scheduleTag_parse :: Spec
unit_scheduleTag_parse =
  it "Schedule-Tag parses an opaque-tag" $
    parseAll STg.scheduleTagParser "\"abc123\""
      `shouldBe` Right (STg.ScheduleTag (ST.fromString "abc123"))


unit_scheduleTag_render :: Spec
unit_scheduleTag_render =
  it "Schedule-Tag renders quoted" $
    renderSTg (STg.ScheduleTag (ST.fromString "abc123")) `shouldBe` "\"abc123\""


prop_scheduleTag :: Property
prop_scheduleTag = property $ do
  t <- forAll genTagText
  let v = STg.ScheduleTag t
  parseAll STg.scheduleTagParser (renderSTg v) === Right v


-- If-Schedule-Tag-Match ------------------------------------------------------

renderISTM :: ISTM.IfScheduleTagMatch -> ByteString
renderISTM = M.toStrictByteString . ISTM.renderIfScheduleTagMatch


unit_ifScheduleTagMatch_parse :: Spec
unit_ifScheduleTagMatch_parse =
  it "If-Schedule-Tag-Match parses a schedule-tag" $
    parseAll ISTM.ifScheduleTagMatchParser "\"tag-1\""
      `shouldBe` Right (ISTM.IfScheduleTagMatch (STg.ScheduleTag (ST.fromString "tag-1")))


unit_ifScheduleTagMatch_render :: Spec
unit_ifScheduleTagMatch_render =
  it "If-Schedule-Tag-Match renders quoted" $
    renderISTM (ISTM.IfScheduleTagMatch (STg.ScheduleTag (ST.fromString "tag-1")))
      `shouldBe` "\"tag-1\""


prop_ifScheduleTagMatch :: Property
prop_ifScheduleTagMatch = property $ do
  t <- forAll genTagText
  let v = ISTM.IfScheduleTagMatch (STg.ScheduleTag t)
  parseAll ISTM.ifScheduleTagMatchParser (renderISTM v) === Right v


-- Cal-Managed-ID -------------------------------------------------------------

renderCMI :: CMI.CalManagedID -> ByteString
renderCMI = M.toStrictByteString . CMI.renderCalManagedID


unit_calManagedID_parse :: Spec
unit_calManagedID_parse =
  it "Cal-Managed-ID parses an opaque managed-id" $
    parseAll CMI.calManagedIDParser "AB12-CD34"
      `shouldBe` Right (CMI.CalManagedID (ST.fromString "AB12-CD34"))


unit_calManagedID_render :: Spec
unit_calManagedID_render =
  it "Cal-Managed-ID renders verbatim" $
    renderCMI (CMI.CalManagedID (ST.fromString "AB12-CD34")) `shouldBe` "AB12-CD34"


prop_calManagedID :: Property
prop_calManagedID = property $ do
  t <- forAll genIdText
  let v = CMI.CalManagedID t
  parseAll CMI.calManagedIDParser (renderCMI v) === Right v


-- CalDAV-Timezones -----------------------------------------------------------

renderCTZ :: CTZ.CalDAVTimezones -> ByteString
renderCTZ = M.toStrictByteString . CTZ.renderCalDAVTimezones


unit_calDAVTimezones_parse :: Spec
unit_calDAVTimezones_parse = it "CalDAV-Timezones parses T and F" $ do
  parseAll CTZ.calDAVTimezonesParser "T" `shouldBe` Right (CTZ.CalDAVTimezones True)
  parseAll CTZ.calDAVTimezonesParser "F" `shouldBe` Right (CTZ.CalDAVTimezones False)


unit_calDAVTimezones_render :: Spec
unit_calDAVTimezones_render = it "CalDAV-Timezones renders T/F" $ do
  renderCTZ (CTZ.CalDAVTimezones True) `shouldBe` "T"
  renderCTZ (CTZ.CalDAVTimezones False) `shouldBe` "F"


prop_calDAVTimezones :: Property
prop_calDAVTimezones = property $ do
  b <- forAll Gen.bool
  let v = CTZ.CalDAVTimezones b
  parseAll CTZ.calDAVTimezonesParser (renderCTZ v) === Right v


-- OSLC-Core-Version ----------------------------------------------------------

renderOCV :: OCV.OSLCCoreVersion -> ByteString
renderOCV = M.toStrictByteString . OCV.renderOSLCCoreVersion


unit_oslcCoreVersion_parse :: Spec
unit_oslcCoreVersion_parse =
  it "OSLC-Core-Version parses a version" $
    parseAll OCV.oslcCoreVersionParser "2.0"
      `shouldBe` Right (OCV.OSLCCoreVersion (ST.fromString "2.0"))


unit_oslcCoreVersion_render :: Spec
unit_oslcCoreVersion_render =
  it "OSLC-Core-Version renders verbatim" $
    renderOCV (OCV.OSLCCoreVersion (ST.fromString "2.0")) `shouldBe` "2.0"


prop_oslcCoreVersion :: Property
prop_oslcCoreVersion = property $ do
  t <- forAll genVersion
  let v = OCV.OSLCCoreVersion t
  parseAll OCV.oslcCoreVersionParser (renderOCV v) === Right v


tests :: Spec
tests =
  describe "CalDav" $
    sequence_
      [ unit_scheduleReply_parse
      , unit_scheduleReply_render
      , it "Schedule-Reply round-trip" prop_scheduleReply
      , unit_scheduleTag_parse
      , unit_scheduleTag_render
      , it "Schedule-Tag round-trip" prop_scheduleTag
      , unit_ifScheduleTagMatch_parse
      , unit_ifScheduleTagMatch_render
      , it "If-Schedule-Tag-Match round-trip" prop_ifScheduleTagMatch
      , unit_calManagedID_parse
      , unit_calManagedID_render
      , it "Cal-Managed-ID round-trip" prop_calManagedID
      , unit_calDAVTimezones_parse
      , unit_calDAVTimezones_render
      , it "CalDAV-Timezones round-trip" prop_calDAVTimezones
      , unit_oslcCoreVersion_parse
      , unit_oslcCoreVersion_render
      , it "OSLC-Core-Version round-trip" prop_oslcCoreVersion
      ]
