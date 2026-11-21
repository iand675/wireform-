{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.OAuthDpop (tests) where

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
import Network.HTTP.Headers (HeaderMap, defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.DPoP as DP
import qualified Network.HTTP.Headers.DPoPNonce as DN
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, toCIByteString)
import qualified Network.HTTP.Headers.Hobareg as H
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OptionalWWWAuthenticate as OW
import Network.HTTP.Headers.Parsing.Util (RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.ReplayNonce as RN
import Network.HTTP.Headers.WWWAuthenticate (
  AuthChallenge (..),
  AuthScheme (..),
  ChallengeContents (..),
  CredentialParam (..),
 )
import Network.HTTP.Status (StatusCode (..))
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Parse helpers (one per header; trailing OWS tolerated where applicable)
-- ---------------------------------------------------------------------------

dropWs :: ByteString -> ByteString
dropWs = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseDPoP :: ByteString -> Either String DP.DPoP
parseDPoP bs = case runParser DP.dPoPParser bs of
  OK v rest
    | BS.null rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


parseDPoPNonce :: ByteString -> Either String DN.DPoPNonce
parseDPoPNonce bs = case runParser DN.dPoPNonceParser bs of
  OK v rest
    | BS.null rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


parseReplay :: ByteString -> Either String RN.ReplayNonce
parseReplay bs = case runParser RN.replayNonceParser bs of
  OK v rest
    | BS.null (dropWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


parseHobareg :: ByteString -> Either String H.Hobareg
parseHobareg bs = case runParser H.hobaregParser bs of
  OK v rest
    | BS.null (dropWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


parseOWA :: ByteString -> Either String OW.OptionalWWWAuthenticate
parseOWA bs = case runParser OW.optionalWWWAuthenticateParser bs of
  OK v rest
    | BS.null (dropWs rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


fieldLines :: HeaderFieldName -> [ByteString] -> HeaderMap
fieldLines name = headerMapFromList . map (\value -> (toCIByteString name, value))


-- ---------------------------------------------------------------------------
-- DPoP
-- ---------------------------------------------------------------------------

unit_dpop_parse :: Spec
unit_dpop_parse = it "parses a DPoP proof JWT verbatim" $
  case parseDPoP "eyJ0eXAiOiJkcG9wK2p3dCJ9.eyJzdWIiOiJ4In0.c2lnbmF0dXJl" of
    Right (DP.DPoP t) ->
      t `shouldBe` ST.fromString "eyJ0eXAiOiJkcG9wK2p3dCJ9.eyJzdWIiOiJ4In0.c2lnbmF0dXJl"
    other -> error (show other)


unit_dpop_render :: Spec
unit_dpop_render =
  it "renders a DPoP proof JWT verbatim" $
    M.toStrictByteString (DP.renderDPoP (DP.DPoP (ST.fromString "aaa.bbb.ccc")))
      `shouldBe` "aaa.bbb.ccc"


unit_dpop_fields :: Spec
unit_dpop_fields = it "validates every token68 octet and requires a single DPoP field" $ do
  let field = fieldLines "DPoP"
      token = "ALPHA.123-_.~+/=="
      value = DP.DPoP "ALPHA.123-_.~+/=="
  lookupHeader @DP.DPoP (field [" \t" <> token <> "\t "]) `shouldBe` Right (Just value)
  lookupRawHeader "DPoP" (setHeader value (field [])) `shouldBe` Just (token :| [])
  forM_ [[""], ["===="], ["a=b"], ["a,b"], ["abc", "def"]] $ \bad ->
    lookupHeader @DP.DPoP (field bad) `shouldSatisfy` isLeft
  forM_ [minBound .. maxBound] $ \octet -> do
    let wire = BS.pack [0x41, octet, 0x42]
    if BS.elem octet (BSC.pack (base64urlChars <> ".~+/"))
      then lookupHeader @DP.DPoP (field [wire]) `shouldBe` Right (Just (DP.DPoP (ST.fromString (BSC.unpack wire))))
      else lookupHeader @DP.DPoP (field [wire]) `shouldSatisfy` isLeft


-- ---------------------------------------------------------------------------
-- DPoP-Nonce
-- ---------------------------------------------------------------------------

unit_dpopnonce_parse :: Spec
unit_dpopnonce_parse = it "parses an opaque DPoP nonce" $
  case parseDPoPNonce "eyJ7S_zG7e7Il5uBJP1Z2RkR" of
    Right (DN.DPoPNonce t) -> t `shouldBe` ST.fromString "eyJ7S_zG7e7Il5uBJP1Z2RkR"
    other -> error (show other)


unit_dpopnonce_render :: Spec
unit_dpopnonce_render =
  it "renders an opaque DPoP nonce verbatim" $
    M.toStrictByteString (DN.renderDPoPNonce (DN.DPoPNonce (ST.fromString "nonce-123_~")))
      `shouldBe` "nonce-123_~"


unit_dpopnonce_fields :: Spec
unit_dpopnonce_fields = it "keeps opaque NQCHAR bytes including commas in one DPoP-Nonce field" $ do
  let field = fieldLines "DPoP-Nonce"
      value = DN.DPoPNonce "one,two"
  lookupHeader @DN.DPoPNonce (field ["\tone,two \t"]) `shouldBe` Right (Just value)
  lookupRawHeader "DPoP-Nonce" (setHeader value (field [])) `shouldBe` Just ("one,two" :| [])
  forM_ [[""], [" \t"], ["one", "two"]] $ \bad ->
    lookupHeader @DN.DPoPNonce (field bad) `shouldSatisfy` isLeft
  forM_ [minBound .. maxBound] $ \octet -> do
    let wire = BS.pack [0x41, octet, 0x42]
    if octet >= 0x21 && octet <= 0x7e && octet /= 0x22 && octet /= 0x5c
      then lookupHeader @DN.DPoPNonce (field [wire]) `shouldBe` Right (Just (DN.DPoPNonce (ST.fromString (BSC.unpack wire))))
      else lookupHeader @DN.DPoPNonce (field [wire]) `shouldSatisfy` isLeft


-- ---------------------------------------------------------------------------
-- Replay-Nonce
-- ---------------------------------------------------------------------------

unit_replay_parse :: Spec
unit_replay_parse = it "parses a base64url replay nonce" $
  case parseReplay "oFvnlf8Y9eK4Q9wQ-CKZ4g" of
    Right (RN.ReplayNonce t) -> t `shouldBe` ST.fromString "oFvnlf8Y9eK4Q9wQ-CKZ4g"
    other -> error (show other)


unit_replay_render :: Spec
unit_replay_render =
  it "renders a replay nonce verbatim" $
    M.toStrictByteString (RN.renderReplayNonce (RN.ReplayNonce (ST.fromString "abc_DEF-123")))
      `shouldBe` "abc_DEF-123"


unit_replay_fields :: Spec
unit_replay_fields = it "validates the unpadded Base64url alphabet and canonical tail of one Replay-Nonce" $ do
  let field = fieldLines "Replay-Nonce"
  forM_ ["Zg", "Zm8", "Zm9v", "-_8"] $ \wire -> do
    let value = RN.ReplayNonce (ST.fromString (BSC.unpack wire))
    lookupHeader @RN.ReplayNonce (field ["\t" <> wire <> " "]) `shouldBe` Right (Just value)
    lookupRawHeader "Replay-Nonce" (setHeader value (field [])) `shouldBe` Just (wire :| [])
  forM_ [[""], ["Z"], ["Zh"], ["Zm9"], ["Zg=="], ["+/8"], ["AA, AA"], ["Zg", "Zg"]] $ \bad ->
    lookupHeader @RN.ReplayNonce (field bad) `shouldSatisfy` isLeft


unit_checked_writers :: Spec
unit_checked_writers = do
  describe "DPoP" $
    it "checks authored wire values before writing" $ do
      let previous = fieldLines "DPoP" ["first.second.third"]
          valid = DP.DPoP " \tALPHA.123-_.~+/==\t "
      renderHeaderLines defaultHeaderSettings valid `shouldBe` Just (" \tALPHA.123-_.~+/==\t " :| [])
      forM_ ["", "==", "first=second", "first,second", "first second", "proof\r\n"] $ \text -> do
        let invalid = DP.DPoP text
        renderHeaderLines defaultHeaderSettings invalid `shouldBe` Nothing
        setHeader invalid previous `shouldBe` previous
  describe "DPoP-Nonce" $
    it "checks authored wire values before writing" $ do
      let previous = fieldLines "DPoP-Nonce" ["example-nonce"]
          valid = DN.DPoPNonce " \tone,two\t "
      renderHeaderLines defaultHeaderSettings valid `shouldBe` Just (" \tone,two\t " :| [])
      forM_ ["", "one two", "one\"two", "one\\two", "example\r\n", "é"] $ \text -> do
        let invalid = DN.DPoPNonce text
        renderHeaderLines defaultHeaderSettings invalid `shouldBe` Nothing
        setHeader invalid previous `shouldBe` previous
  describe "Replay-Nonce" $
    it "checks authored wire values before writing" $ do
      let previous = fieldLines "Replay-Nonce" ["AA"]
          valid = RN.ReplayNonce " \t-_8\t "
      renderHeaderLines defaultHeaderSettings valid `shouldBe` Just (" \t-_8\t " :| [])
      forM_ ["", "Z", "Zh", "Zm9", "Zg==", "+/8", "AA\r\n"] $ \text -> do
        let invalid = RN.ReplayNonce text
        renderHeaderLines defaultHeaderSettings invalid `shouldBe` Nothing
        setHeader invalid previous `shouldBe` previous


-- ---------------------------------------------------------------------------
-- Hobareg
-- ---------------------------------------------------------------------------

unit_hobareg_register :: Spec
unit_hobareg_register =
  it "parses the case-insensitive regok status" $
    parseHobareg "ReGoK" `shouldBe` Right H.HobaregRegister


unit_hobareg_inprogress :: Spec
unit_hobareg_inprogress =
  it "parses reginwork" $
    parseHobareg "reginwork" `shouldBe` Right H.HobaregInProgress


unit_hobareg_render :: Spec
unit_hobareg_render = it "renders both Hobareg tokens" $ do
  M.toStrictByteString (H.renderHobareg H.HobaregRegister) `shouldBe` "regok"
  M.toStrictByteString (H.renderHobareg H.HobaregInProgress) `shouldBe` "reginwork"


unit_hobareg_fields :: Spec
unit_hobareg_fields = it "interprets only one valid regok field as the registration-success signal" $ do
  let field = fieldLines "Hobareg"
  lookupHeader @H.Hobareg (field []) `shouldBe` Right Nothing
  lookupHeader @H.Hobareg (field ["\t REGOK "]) `shouldBe` Right (Just H.HobaregRegister)
  lookupHeader @H.Hobareg (field ["\tregok\t \t"]) `shouldBe` Right (Just H.HobaregRegister)
  lookupRawHeader "Hobareg" (setHeader H.HobaregRegister (field [])) `shouldBe` Just ("regok" :| [])
  forM_ [[""], ["    "], [" rego"], ["\t reGOk\xff"], ["\tregok\n"], ["register"], ["regokx"], ["regok, regok"], ["regok", "regok"], ["regok", "reginwork"]] $ \lines' ->
    lookupHeader @H.Hobareg (field lines') `shouldBe` Right (Just H.HobaregInProgress)


unit_hobareg_registration :: Spec
unit_hobareg_registration = it "checks the final response status and all Hobareg field lines" $ do
  let field = fieldLines "Hobareg"
      empty = field []
      valid = field ["\t ReGoK "]
      repeated = field ["regok", "regok"]
      invalid = field ["regok, regok"]
      succeeds code = H.hobaregRegistrationSucceeded (StatusCode code)
  forM_ [200, 204, 299] $ \code -> do
    succeeds code valid empty `shouldBe` True
    succeeds code empty valid `shouldBe` True
    succeeds code valid valid `shouldBe` False
    succeeds code empty repeated `shouldBe` False
    succeeds code valid invalid `shouldBe` False
    succeeds code empty empty `shouldBe` False
  forM_ [199, 300, 304, 401, 500] $ \code -> do
    succeeds code valid empty `shouldBe` False
    succeeds code empty valid `shouldBe` False


-- ---------------------------------------------------------------------------
-- Optional-WWW-Authenticate
-- ---------------------------------------------------------------------------

unit_owa_parse :: Spec
unit_owa_parse = it "parses an advertised challenge list" $
  case parseOWA "Bearer, Basic realm=\"simple\"" of
    Right (OW.OptionalWWWAuthenticate [c1, c2]) -> do
      challengeScheme c1 `shouldBe` AuthScheme "Bearer"
      challengeContents c1 `shouldBe` ChallengeBare
      challengeScheme c2 `shouldBe` AuthScheme "Basic"
    other -> error (show other)


unit_owa_render :: Spec
unit_owa_render =
  it "renders an advertised challenge list" $
    let v =
          OW.OptionalWWWAuthenticate
            [ AuthChallenge (AuthScheme "Bearer") ChallengeBare
            , AuthChallenge (AuthScheme "Newauth") (ChallengeParams [("realm", "apps")])
            ]
    in M.toStrictByteString (OW.renderOptionalWWWAuthenticate v)
         `shouldBe` "Bearer, Newauth realm=apps"


unit_owa_fields :: Spec
unit_owa_fields = it "keeps ordered challenges across lines and omits an empty authored list" $ do
  let field = fieldLines "Optional-WWW-Authenticate"
      value =
        OW.OptionalWWWAuthenticate
          [ AuthChallenge (AuthScheme "Basic") (ChallengeParams [("realm", CredentialParamString (RFC8941String "a,b"))])
          , AuthChallenge (AuthScheme "Bearer") (ChallengeToken68 "abc==")
          ]
  lookupHeader @OW.OptionalWWWAuthenticate (field ["Basic realm=\"a,b\"", ", Bearer abc=="]) `shouldBe` Right (Just value)
  lookupRawHeader "Optional-WWW-Authenticate" (setHeader value (field [])) `shouldBe` Just ("Basic realm=\"a,b\", Bearer abc==" :| [])
  lookupHeader @OW.OptionalWWWAuthenticate (setHeader (OW.OptionalWWWAuthenticate []) (field ["Basic realm=example", "Bearer abc=="]))
    `shouldBe` Right Nothing
  lookupHeader @OW.OptionalWWWAuthenticate (field [" , ", ","]) `shouldSatisfy` isLeft


-- ---------------------------------------------------------------------------
-- Generators + round-trip properties
-- ---------------------------------------------------------------------------

base64urlChars :: [Char]
base64urlChars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_"


tokenGen :: Gen ST.ShortText
tokenGen =
  ST.fromString
    <$> Gen.string (Range.linear 1 6) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))


nonceGen :: Gen ST.ShortText
nonceGen = ST.fromString <$> Gen.string (Range.linear 1 24) (Gen.element ('!' : ['#' .. '['] <> [']' .. '~']))


replayNonceGen :: Gen ST.ShortText
replayNonceGen = do
  size <- Gen.filter ((/= 1) . (`rem` 4)) (Gen.int (Range.linear 2 24))
  prefix <- Gen.string (Range.singleton (size - 1)) (Gen.element base64urlChars)
  lastChar <- Gen.element $ case size `rem` 4 of
    2 -> "AQgw"
    3 -> "AEIMQUYcgkosw048"
    _ -> base64urlChars
  pure (ST.fromString (prefix <> [lastChar]))


jwtGen :: Gen ST.ShortText
jwtGen = do
  let seg = Gen.string (Range.linear 1 10) (Gen.element base64urlChars)
  a <- seg
  b <- seg
  c <- seg
  pure (ST.fromString (a <> "." <> b <> "." <> c))


contentsGen :: Gen ChallengeContents
contentsGen =
  Gen.choice
    [ pure ChallengeBare
    , ChallengeParams . zipWith distinct ["a-", "b-", "c-"] <$> Gen.list (Range.linear 1 3) paramGen
    ]
  where
    distinct prefix (name, value) = (prefix <> name, value)
    paramGen = do
      k <- tokenGen
      v <- tokenGen
      pure (k, CredentialParamToken v)


challengeGen :: Gen AuthChallenge
challengeGen = AuthChallenge . AuthScheme <$> tokenGen <*> contentsGen


owaGen :: Gen OW.OptionalWWWAuthenticate
owaGen = OW.OptionalWWWAuthenticate <$> Gen.list (Range.linear 1 3) challengeGen


prop_dpop_roundtrip :: Property
prop_dpop_roundtrip = property $ do
  v <- forAll (DP.DPoP <$> jwtGen)
  let bs = M.toStrictByteString (DP.renderDPoP v)
  parseDPoP bs === Right v


prop_dpopnonce_roundtrip :: Property
prop_dpopnonce_roundtrip = property $ do
  v <- forAll (DN.DPoPNonce <$> nonceGen)
  let bs = M.toStrictByteString (DN.renderDPoPNonce v)
  parseDPoPNonce bs === Right v


prop_replay_roundtrip :: Property
prop_replay_roundtrip = property $ do
  v <- forAll (RN.ReplayNonce <$> replayNonceGen)
  let bs = M.toStrictByteString (RN.renderReplayNonce v)
  parseReplay bs === Right v


prop_hobareg_roundtrip :: Property
prop_hobareg_roundtrip = property $ do
  v <- forAll (Gen.element [H.HobaregRegister, H.HobaregInProgress])
  let bs = M.toStrictByteString (H.renderHobareg v)
  parseHobareg bs === Right v


prop_owa_roundtrip :: Property
prop_owa_roundtrip = property $ do
  v <- forAll owaGen
  let bs = M.toStrictByteString (OW.renderOptionalWWWAuthenticate v)
  parseOWA bs === Right v


tests :: Spec
tests =
  describe "OAuthDpop" $
    sequence_
      [ unit_dpop_parse
      , unit_dpop_render
      , unit_dpop_fields
      , unit_dpopnonce_parse
      , unit_dpopnonce_render
      , unit_dpopnonce_fields
      , unit_replay_parse
      , unit_replay_render
      , unit_replay_fields
      , unit_checked_writers
      , unit_hobareg_register
      , unit_hobareg_inprogress
      , unit_hobareg_render
      , unit_hobareg_fields
      , unit_hobareg_registration
      , unit_owa_parse
      , unit_owa_render
      , unit_owa_fields
      , it "DPoP round-trips" prop_dpop_roundtrip
      , it "DPoP-Nonce round-trips" prop_dpopnonce_roundtrip
      , it "Replay-Nonce round-trips" prop_replay_roundtrip
      , it "Hobareg round-trips" prop_hobareg_roundtrip
      , it "Optional-WWW-Authenticate round-trips" prop_owa_roundtrip
      ]
