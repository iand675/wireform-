{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.OAuthDpop (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.DPoP as DP
import qualified Network.HTTP.Headers.DPoPNonce as DN
import qualified Network.HTTP.Headers.Hobareg as H
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OptionalWWWAuthenticate as OW
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.ReplayNonce as RN
import Network.HTTP.Headers.WWWAuthenticate (
  AuthChallenge (..),
  AuthScheme (..),
  ChallengeContents (..),
  CredentialParam (..),
 )
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


-- ---------------------------------------------------------------------------
-- Hobareg
-- ---------------------------------------------------------------------------

unit_hobareg_register :: Spec
unit_hobareg_register =
  it "parses register" $
    parseHobareg "register" `shouldBe` Right H.HobaregRegister


unit_hobareg_inprogress :: Spec
unit_hobareg_inprogress =
  it "parses reg-in-progress" $
    parseHobareg "reg-in-progress" `shouldBe` Right H.HobaregInProgress


unit_hobareg_render :: Spec
unit_hobareg_render = it "renders both Hobareg tokens" $ do
  M.toStrictByteString (H.renderHobareg H.HobaregRegister) `shouldBe` "register"
  M.toStrictByteString (H.renderHobareg H.HobaregInProgress) `shouldBe` "reg-in-progress"


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
nonceGen = ST.fromString <$> Gen.string (Range.linear 1 24) (Gen.element base64urlChars)


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
    , ChallengeParams <$> Gen.list (Range.linear 1 3) paramGen
    ]
  where
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
  v <- forAll (RN.ReplayNonce <$> nonceGen)
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
      , unit_dpopnonce_parse
      , unit_dpopnonce_render
      , unit_replay_parse
      , unit_replay_render
      , unit_hobareg_register
      , unit_hobareg_inprogress
      , unit_hobareg_render
      , unit_owa_parse
      , unit_owa_render
      , it "DPoP round-trips" prop_dpop_roundtrip
      , it "DPoP-Nonce round-trips" prop_dpopnonce_roundtrip
      , it "Replay-Nonce round-trips" prop_replay_roundtrip
      , it "Hobareg round-trips" prop_hobareg_roundtrip
      , it "Optional-WWW-Authenticate round-trips" prop_owa_roundtrip
      ]
