{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Prefer (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Prefer as P
import qualified Network.HTTP.Headers.PreferenceApplied as PA
import qualified Network.HTTP.Headers.RepeatabilityClientID as RC
import qualified Network.HTTP.Headers.RepeatabilityFirstSent as RF
import qualified Network.HTTP.Headers.RepeatabilityRequestID as RR
import qualified Network.HTTP.Headers.RepeatabilityResult as RS
import Test.Syd
import Test.Syd.Hedgehog ()


ok :: Result String a -> Either String a
ok = \case
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


-- ---------------------------------------------------------------------------
-- Generators (token-only grammar, so values round-trip verbatim)
-- ---------------------------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.string (Range.linear 1 8) (Gen.element tchars)
  where
    tchars = ['a' .. 'z'] <> ['0' .. '9'] <> "-"


paramGen :: Gen (ST.ShortText, Maybe ST.ShortText)
paramGen = (,) <$> tokenGen <*> Gen.maybe tokenGen


preferenceGen :: Gen P.Preference
preferenceGen =
  P.Preference
    <$> tokenGen
    <*> Gen.maybe tokenGen
    <*> Gen.list (Range.linear 0 2) paramGen


preferGen :: Gen P.Prefer
preferGen = P.Prefer <$> Gen.nonEmpty (Range.linear 1 3) preferenceGen


appliedPreferenceGen :: Gen PA.AppliedPreference
appliedPreferenceGen =
  PA.AppliedPreference
    <$> tokenGen
    <*> Gen.maybe tokenGen
    <*> Gen.list (Range.linear 0 2) paramGen


preferenceAppliedGen :: Gen PA.PreferenceApplied
preferenceAppliedGen =
  PA.PreferenceApplied <$> Gen.nonEmpty (Range.linear 1 3) appliedPreferenceGen


-- A canonical-ish IMF-fixdate string. The weekday name is arbitrary (the
-- parser ignores it), so we only require it to be a valid 3-letter token.
imfDateGen :: Gen ByteString
imfDateGen = do
  dow <- Gen.element ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
  dd <- Gen.int (Range.linear 1 28)
  mon <- Gen.element ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  yyyy <- Gen.int (Range.linear 1970 2099)
  hh <- Gen.int (Range.linear 0 23)
  mm <- Gen.int (Range.linear 0 59)
  ss <- Gen.int (Range.linear 0 59)
  pure $
    BC.pack $
      dow
        <> ", "
        <> pad2 dd
        <> " "
        <> mon
        <> " "
        <> show yyyy
        <> " "
        <> pad2 hh
        <> ":"
        <> pad2 mm
        <> ":"
        <> pad2 ss
        <> " GMT"
  where
    pad2 n = let s = show n in if length s < 2 then '0' : s else s


-- ---------------------------------------------------------------------------
-- Prefer
-- ---------------------------------------------------------------------------

unit_prefer_parse :: Spec
unit_prefer_parse = it "parses a preference with parameters plus a valued preference" $
  case ok (runParser P.preferParser "respond-async;wait=100, return=representation") of
    Right (P.Prefer (a :| [b])) -> do
      P.preferenceName a `shouldBe` ST.fromString "respond-async"
      P.preferenceValue a `shouldBe` Nothing
      P.preferenceParameters a `shouldBe` [(ST.fromString "wait", Just (ST.fromString "100"))]
      P.preferenceName b `shouldBe` ST.fromString "return"
      P.preferenceValue b `shouldBe` Just (ST.fromString "representation")
      P.preferenceParameters b `shouldBe` []
    other -> error (show other)


unit_prefer_render :: Spec
unit_prefer_render =
  it "renders preferences with value and parameters" $
    let v =
          P.Prefer
            ( P.Preference (ST.fromString "respond-async") Nothing [(ST.fromString "wait", Just (ST.fromString "100"))]
                :| [P.Preference (ST.fromString "return") (Just (ST.fromString "minimal")) []]
            )
    in M.toStrictByteString (P.renderPrefer v) `shouldBe` "respond-async;wait=100, return=minimal"


prop_prefer_roundtrip :: Property
prop_prefer_roundtrip = property $ do
  v <- forAll preferGen
  let bs = M.toStrictByteString (P.renderPrefer v)
  case ok (runParser P.preferParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Preference-Applied
-- ---------------------------------------------------------------------------

unit_applied_parse :: Spec
unit_applied_parse = it "parses an applied preference with a value" $
  case ok (runParser PA.preferenceAppliedParser "return=representation") of
    Right (PA.PreferenceApplied (a :| [])) -> do
      PA.appliedPreferenceName a `shouldBe` ST.fromString "return"
      PA.appliedPreferenceValue a `shouldBe` Just (ST.fromString "representation")
      PA.appliedPreferenceParameters a `shouldBe` []
    other -> error (show other)


unit_applied_render :: Spec
unit_applied_render =
  it "renders applied preferences" $
    let v =
          PA.PreferenceApplied
            ( PA.AppliedPreference (ST.fromString "respond-async") Nothing []
                :| [PA.AppliedPreference (ST.fromString "return") (Just (ST.fromString "minimal")) []]
            )
    in M.toStrictByteString (PA.renderPreferenceApplied v) `shouldBe` "respond-async, return=minimal"


prop_applied_roundtrip :: Property
prop_applied_roundtrip = property $ do
  v <- forAll preferenceAppliedGen
  let bs = M.toStrictByteString (PA.renderPreferenceApplied v)
  case ok (runParser PA.preferenceAppliedParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Repeatability-Client-ID / Repeatability-Request-ID
-- ---------------------------------------------------------------------------

unit_client_id_parse :: Spec
unit_client_id_parse = it "parses a client-id UUID token" $
  case ok (runParser RC.repeatabilityClientIDParser "0e8a7e9f-a3c4-4b2e-9f1a-1234567890ab") of
    Right cid -> RC.repeatabilityClientID cid `shouldBe` ST.fromString "0e8a7e9f-a3c4-4b2e-9f1a-1234567890ab"
    other -> error (show other)


unit_client_id_render :: Spec
unit_client_id_render =
  it "renders a client-id UUID token" $
    let v = RC.RepeatabilityClientID (ST.fromString "0e8a7e9f-a3c4-4b2e-9f1a-1234567890ab")
    in M.toStrictByteString (RC.renderRepeatabilityClientID v) `shouldBe` "0e8a7e9f-a3c4-4b2e-9f1a-1234567890ab"


unit_request_id_parse :: Spec
unit_request_id_parse = it "parses a request-id UUID token" $
  case ok (runParser RR.repeatabilityRequestIDParser "8b59abcd-1234-4321-abcd-001122334455") of
    Right rid -> RR.repeatabilityRequestID rid `shouldBe` ST.fromString "8b59abcd-1234-4321-abcd-001122334455"
    other -> error (show other)


unit_request_id_render :: Spec
unit_request_id_render =
  it "renders a request-id UUID token" $
    let v = RR.RepeatabilityRequestID (ST.fromString "8b59abcd-1234-4321-abcd-001122334455")
    in M.toStrictByteString (RR.renderRepeatabilityRequestID v) `shouldBe` "8b59abcd-1234-4321-abcd-001122334455"


prop_request_id_roundtrip :: Property
prop_request_id_roundtrip = property $ do
  t <- forAll tokenGen
  let v = RR.RepeatabilityRequestID t
      bs = M.toStrictByteString (RR.renderRepeatabilityRequestID v)
  case ok (runParser RR.repeatabilityRequestIDParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Repeatability-First-Sent
-- ---------------------------------------------------------------------------

unit_first_sent_parse :: Spec
unit_first_sent_parse = it "parses an IMF-fixdate" $
  case ok (runParser RF.repeatabilityFirstSentParser "Tue, 28 Feb 2012 12:34:56 GMT") of
    Right _ -> pure () :: IO ()
    other -> error (show other)


unit_first_sent_render :: Spec
unit_first_sent_render = it "renders the canonical IMF-fixdate" $
  case ok (runParser RF.repeatabilityFirstSentParser "Tue, 28 Feb 2012 12:34:56 GMT") of
    Right v -> M.toStrictByteString (RF.renderRepeatabilityFirstSent v) `shouldBe` "Tue, 28 Feb 2012 12:34:56 GMT"
    other -> error (show other)


prop_first_sent_roundtrip :: Property
prop_first_sent_roundtrip = property $ do
  s <- forAll imfDateGen
  case ok (runParser RF.repeatabilityFirstSentParser s) of
    Right v ->
      let bs = M.toStrictByteString (RF.renderRepeatabilityFirstSent v)
      in case ok (runParser RF.repeatabilityFirstSentParser bs) of
          Right v' -> v === v'
          Left err -> error (err <> " on " <> show bs)
    Left err -> error (err <> " on " <> show s)


-- ---------------------------------------------------------------------------
-- Repeatability-Result
-- ---------------------------------------------------------------------------

unit_result_parse :: Spec
unit_result_parse = it "parses accepted and rejected" $ do
  ok (runParser RS.repeatabilityResultParser "accepted") `shouldBe` Right RS.Accepted
  ok (runParser RS.repeatabilityResultParser "rejected") `shouldBe` Right RS.Rejected


unit_result_render :: Spec
unit_result_render = it "renders the canonical lower-case form" $ do
  M.toStrictByteString (RS.renderRepeatabilityResult RS.Accepted) `shouldBe` "accepted"
  M.toStrictByteString (RS.renderRepeatabilityResult RS.Rejected) `shouldBe` "rejected"


prop_result_roundtrip :: Property
prop_result_roundtrip = property $ do
  v <- forAll (Gen.element [RS.Accepted, RS.Rejected])
  let bs = M.toStrictByteString (RS.renderRepeatabilityResult v)
  case ok (runParser RS.repeatabilityResultParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Prefer" $
    sequence_
      [ unit_prefer_parse
      , unit_prefer_render
      , it "Prefer round-trip" prop_prefer_roundtrip
      , unit_applied_parse
      , unit_applied_render
      , it "Preference-Applied round-trip" prop_applied_roundtrip
      , unit_client_id_parse
      , unit_client_id_render
      , unit_request_id_parse
      , unit_request_id_render
      , it "Repeatability-Request-ID round-trip" prop_request_id_roundtrip
      , unit_first_sent_parse
      , unit_first_sent_render
      , it "Repeatability-First-Sent round-trip" prop_first_sent_roundtrip
      , unit_result_parse
      , unit_result_render
      , it "Repeatability-Result round-trip" prop_result_roundtrip
      ]
