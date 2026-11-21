{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Prefer (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toUpper)
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import Network.HTTP.Headers.HeaderFieldName (hRepeatabilityFirstSent, hRepeatabilityResult)
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
    <*> pure []


preferenceAppliedGen :: Gen PA.PreferenceApplied
preferenceAppliedGen =
  PA.PreferenceApplied <$> Gen.nonEmpty (Range.linear 1 3) appliedPreferenceGen


firstSentGen :: Gen RF.RepeatabilityFirstSent
firstSentGen = do
  year <- Gen.integral (Range.linear 1970 2099)
  month <- Gen.int (Range.linear 1 12)
  day <- Gen.int (Range.linear 1 28)
  seconds <- Gen.integral (Range.linear 0 86399)
  pure (RF.RepeatabilityFirstSent (UTCTime (fromGregorian year month day) (secondsToDiffTime seconds)))


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


unit_prefer_field_lines :: Spec
unit_prefer_field_lines = it "handles BWS, empty list positions, and the first case-insensitive preference" $ do
  let headers =
        headerMapFromList
          [ ("Prefer", " ,\tReturn \t= \"minimal\" ;; reason \t= \"user, \\\"requested\\\"\";")
          , ("Prefer", "\t, ,")
          , ("Prefer", "return=representation, flag=\"\",")
          ]
      first = P.Preference "Return" (Just "\"minimal\"") [("reason", Just "\"user, \\\"requested\\\"\"")]
      canonical = "Return=\"minimal\";reason=\"user, \\\"requested\\\"\", return=representation, flag=\"\""
  case lookupHeader @P.Prefer headers of
    Right (Just value) -> do
      P.lookupPreference "RETURN" value `shouldBe` Just first
      P.decodePreferenceValue (P.preferenceValue first) `shouldBe` Right (Just "minimal")
      P.decodePreferenceValue (Just "\"user, \\\"requested\\\"\"") `shouldBe` Right (Just "user, \"requested\"")
      P.decodePreferenceValue (P.preferenceValue =<< P.lookupPreference "flag" value) `shouldBe` Right Nothing
      headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("prefer", canonical)]
    other -> expectationFailure (show other)
  mapM_
    (\wire -> lookupHeader @P.Prefer (headerMapFromList [("Prefer", wire)]) `shouldSatisfy` isLeft)
    [" , , ", "wait = ", "wait=10;reason = ", "return=\"unterminated"]


unit_prefer_opaque_words :: Spec
unit_prefer_opaque_words = it "preserves each allowed quoted-pair octet in preferences and parameters" $ do
  let check byte = do
        let octet = BS.singleton byte
            rawValue = "\"\\" <> octet <> "\""
            wire = "extension=" <> rawValue <> ";argument=" <> rawValue
            allowed = byte == 9 || (byte >= 32 && byte <= 126) || byte >= 128
        case lookupHeader @P.Prefer (headerMapFromList [("Prefer", wire)]) of
          Right (Just value@(P.Prefer (entry :| []))) -> do
            allowed `shouldBe` True
            P.decodePreferenceValue (P.preferenceValue entry) `shouldBe` Right (Just octet)
            map (P.decodePreferenceValue . snd) (P.preferenceParameters entry) `shouldBe` [Right (Just octet)]
            headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("prefer", wire)]
          Left _ -> allowed `shouldBe` False
          other -> expectationFailure (show other)
  mapM_ check [minBound .. maxBound]


unit_prefer_checked_write :: Spec
unit_prefer_checked_write = it "preserves the original list when an authored word or name is malformed" $ do
  let fields = headerMapFromList [("Prefer", "respond-async"), ("Prefer", "return=minimal")]
      valid = P.Preference "wait" (Just "10") []
      reject entry = do
        let value = P.Prefer (valid :| [entry])
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value fields `shouldBe` fields
  mapM_ reject $
    map (\name -> P.Preference name Nothing []) ["", "wait, return", "re turn", "x\xff"]
      <> map (\word -> P.Preference "return" (Just word) []) ["", "minimal,wait=10", "\"unfinished", "\"value\"\t", "\"a\\\DEL\""]
      <> [P.Preference "x" Nothing [("a;b", Nothing)], P.Preference "x" Nothing [("a", Just "\"a\"b\"")]]


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


unit_applied_field_lines :: Spec
unit_applied_field_lines = it "reads repeated applied preferences and rejects request parameter syntax" $ do
  let quoted = "\"ABC" <> BS.pack [0xc3, 0xa9, 0xe9] <> "\""
      headers = headerMapFromList [("Preference-Applied", "  extension \t = " <> quoted <> ","), ("Preference-Applied", " , \t"), ("Preference-Applied", "return=minimal")]
  case lookupHeader @PA.PreferenceApplied headers of
    Right (Just value@(PA.PreferenceApplied (extension :| [_]))) -> do
      P.decodePreferenceValue (PA.appliedPreferenceValue extension) `shouldBe` Right (Just ("ABC" <> BS.pack [0xc3, 0xa9, 0xe9]))
      headerMapToList (setHeader value (headerMapFromList []))
        `shouldBe` [("preference-applied", "extension=" <> quoted <> ", return=minimal")]
    other -> expectationFailure (show other)
  mapM_
    (\wire -> lookupHeader @PA.PreferenceApplied (headerMapFromList [("Preference-Applied", wire)]) `shouldSatisfy` isLeft)
    ["return=minimal;reason=cache", "return=minimal;", "return= ", " , , "]
  let legacy = PA.PreferenceApplied (PA.AppliedPreference "return" (Just "minimal") [("reason", Just "cache")] :| [])
  headerMapToList (setHeader legacy (headerMapFromList [])) `shouldBe` [("preference-applied", "return=minimal")]


unit_applied_checked_write :: Spec
unit_applied_checked_write = it "checks authored applied names and complete token or quoted values" $ do
  let fields = headerMapFromList [("Preference-Applied", "return=minimal"), ("Preference-Applied", "respond-async")]
      valid = PA.AppliedPreference "respond-async" Nothing []
      reject entry = do
        let value = PA.PreferenceApplied (valid :| [entry])
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value fields `shouldBe` fields
  mapM_ reject
    [ PA.AppliedPreference "return=representation" Nothing []
    , PA.AppliedPreference "return" (Just "minimal;reason=cache") []
    , PA.AppliedPreference "extension" (Just "\"a\\\"") []
    , PA.AppliedPreference "extension" (Just "\"a\n\"") []
    ]


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
  value <- forAll firstSentGen
  let bytes = M.toStrictByteString (RF.renderRepeatabilityFirstSent value)
  ok (runParser RF.repeatabilityFirstSentParser bytes) === Right value


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


repeatabilityHeaders :: Spec
repeatabilityHeaders = describe "repeatability fields" $ do
  it "preserves the spelling of UUIDs and opaque identifier tokens" $ do
    let client = "0E8a7E9f-A3c4-4b2e-9f1a-1234567890AB"
        request = "retry_01+attempt"
        headers =
          headerMapFromList
            [ ("Repeatability-Client-ID", "\t " <> client <> " \t")
            , ("Repeatability-Request-ID", "\t " <> request <> " \t")
            ]
        typedClient = RC.RepeatabilityClientID "0E8a7E9f-A3c4-4b2e-9f1a-1234567890AB"
        typedRequest = RR.RepeatabilityRequestID "retry_01+attempt"
        rendered = setHeader typedClient $ setHeader typedRequest $ headerMapFromList []
    lookupHeader @RC.RepeatabilityClientID headers `shouldBe` Right (Just typedClient)
    lookupHeader @RR.RepeatabilityRequestID headers `shouldBe` Right (Just typedRequest)
    lookupRawHeader "Repeatability-Client-ID" rendered `shouldBe` Just (client :| [])
    lookupRawHeader "Repeatability-Request-ID" rendered `shouldBe` Just (request :| [])
  it "rejects invalid identifier token bytes and combined identifiers" $
    forM_ ["", " ", "client id", "\"uuid\"", "first, second", "client\xff"] $ \input -> do
      lookupHeader @RC.RepeatabilityClientID (headerMapFromList [("Repeatability-Client-ID", input)]) `shouldSatisfy` isLeft
      lookupHeader @RR.RepeatabilityRequestID (headerMapFromList [("Repeatability-Request-ID", input)]) `shouldSatisfy` isLeft
  it "rejects invalid authored client identifiers before replacing the field" $ do
    let fields = headerMapFromList [("Repeatability-Client-ID", "old")]
    forM_ ["", "client id", "first, second", "client\xff"] $ \input -> do
      let value = RC.RepeatabilityClientID input
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields
  it "rejects invalid authored request identifiers before replacing the field" $ do
    let fields = headerMapFromList [("Repeatability-Request-ID", "old")]
    forM_ ["", "request id", "first, second", "request\xff"] $ \input -> do
      let value = RR.RepeatabilityRequestID input
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields
  it "checks every octet in a bare or OWS-padded identifier" $ do
    let tchars = "!#$%&'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    forM_ [0 .. 255] $ \byte -> do
      let token = BS.pack [120, byte, 121]
          expected = byte `BS.elem` tchars
      forM_ [token, "\t" <> token <> " "] $ \wire -> do
        let headers = headerMapFromList [("Repeatability-Client-ID", wire), ("Repeatability-Request-ID", wire)]
        isRight (lookupHeader @RC.RepeatabilityClientID headers) `shouldBe` expected
        isRight (lookupHeader @RR.RepeatabilityRequestID headers) `shouldBe` expected
  it "accepts every ASCII case spelling of either result and renders lowercase" $ do
    forM_ [("accepted", RS.Accepted), ("rejected", RS.Rejected)] $ \(wire, result) -> do
      let spellings = traverse (\letter -> [letter, toUpper letter]) wire
      forM_ spellings $ \spelling ->
        lookupHeader @RS.RepeatabilityResult (headerMapFromList [("Repeatability-Result", "\t " <> BSC.pack spelling <> " \t")])
          `shouldBe` Right (Just result)
      lookupRawHeader hRepeatabilityResult (setHeader result (headerMapFromList []))
        `shouldBe` Just (BSC.pack wire :| [])
  it "rejects unknown results, keyword prefixes and combined results" $
    forM_ ["", "accept", "accepteD-more", "accepted, rejected", "\"accepted\"", "pending", "accepte\xff"] $ \input ->
      lookupHeader @RS.RepeatabilityResult (headerMapFromList [("Repeatability-Result", input)]) `shouldSatisfy` isLeft
  it "accepts all HTTP-date forms on receipt and emits IMF-fixdate" $ do
    let canonical = "Tue, 28 Feb 2012 12:34:56 GMT"
        firstSent = RF.RepeatabilityFirstSent (UTCTime (fromGregorian 2012 2 28) (secondsToDiffTime 45296))
    forM_ [canonical, "Tuesday, 28-Feb-12 12:34:56 GMT", "Tue Feb 28 12:34:56 2012"] $ \input ->
      lookupHeader @RF.RepeatabilityFirstSent (headerMapFromList [("Repeatability-First-Sent", "\t " <> input <> " \t")])
        `shouldBe` Right (Just firstSent)
    lookupRawHeader hRepeatabilityFirstSent (setHeader firstSent (headerMapFromList []))
      `shouldBe` Just (canonical :| [])
  it "rejects repeated singleton lines even when their values match" $ do
    let repeated name value = headerMapFromList [(name, value), (name, value)]
    lookupHeader @RC.RepeatabilityClientID (repeated "Repeatability-Client-ID" "client") `shouldSatisfy` isLeft
    lookupHeader @RR.RepeatabilityRequestID (repeated "Repeatability-Request-ID" "request") `shouldSatisfy` isLeft
    lookupHeader @RF.RepeatabilityFirstSent (repeated "Repeatability-First-Sent" "Tue, 28 Feb 2012 12:34:56 GMT") `shouldSatisfy` isLeft
    lookupHeader @RS.RepeatabilityResult (repeated "Repeatability-Result" "accepted") `shouldSatisfy` isLeft


tests :: Spec
tests =
  describe "Prefer" $
    sequence_
      [ unit_prefer_parse
      , unit_prefer_render
      , unit_prefer_field_lines
      , unit_prefer_opaque_words
      , unit_prefer_checked_write
      , it "Prefer round-trip" prop_prefer_roundtrip
      , unit_applied_parse
      , unit_applied_render
      , unit_applied_field_lines
      , unit_applied_checked_write
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
      , repeatabilityHeaders
      ]
