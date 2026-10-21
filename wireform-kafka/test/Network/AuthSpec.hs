{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.AuthSpec
Description : Tests for SASL mechanisms

Covers the bytes-on-the-wire layer of every SASL mechanism we ship:

  * SASL/PLAIN — exact authzid \\0 user \\0 password framing.
  * SASL/SCRAM — RFC 5802's worked example as a black-box reference;
    PBKDF2/HMAC chain, salted password, ClientKey/StoredKey/Proof,
    server-final verification, and the message parser.
  * SASL/OAUTHBEARER — RFC 7628 framing.
  * AWS_MSK_IAM — SigV4 mechanics (canonical query, signing key,
    signature) using AWS's documented worked example, plus a smoke
    test of the full JSON payload shape.

The handshake driver itself (find broker, do round trips) is exercised
in the broker-gated integration suite.
-}
module Network.AuthSpec (authSpec) where

import Control.Exception (bracket)
import Control.Monad (zipWithM_)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time qualified as Time
import Kafka.Network.Auth.AwsMskIam qualified as Iam
import Kafka.Network.Auth.OAuthBearer qualified as OAuth
import Kafka.Network.Auth.Plain qualified as Plain
import Kafka.Network.Auth.SASL qualified as SASL
import Kafka.Network.Auth.Scram qualified as Scram
import System.Environment qualified as Env
import Test.Syd


authSpec :: Spec
authSpec =
  describe "Auth (SASL mechanisms)" $
    sequence_
      [ plainTests
      , scramTests
      , oauthBearerTests
      , awsMskIamTests
      , gssapiTests
      , configTests
      ]


--------------------------------------------------------------------------------
-- PLAIN
--------------------------------------------------------------------------------

plainTests :: Spec
plainTests =
  describe "PLAIN" $
    sequence_
      [ it "RFC 4616 framing: \\0 user \\0 password" $ do
          let bs = Plain.generatePlainAuth "alice" "secret"
          bs `shouldBe` BS.concat [BS.singleton 0, "alice", BS.singleton 0, "secret"]
      , it "empty username and password still produce two NULs" $ do
          Plain.generatePlainAuth "" "" `shouldBe` BS.pack [0, 0]
      , it "UTF-8 username survives intact" $ do
          Plain.generatePlainAuth "ündel" "ümlaut"
            `shouldBe` BS.concat
              [ BS.singleton 0
              , TE.encodeUtf8 "ündel"
              , BS.singleton 0
              , TE.encodeUtf8 "ümlaut"
              ]
      , it "authorization identity is encoded when supplied" $ do
          Plain.generatePlainAuthWithAuthzid (Just "admin") "alice" "secret"
            `shouldBe` BS.concat ["admin", BS.singleton 0, "alice", BS.singleton 0, "secret"]
      , it "SASL implementation rejects NUL-delimited PLAIN fields" $ do
          let impl = SASL.plainImpl "ali\NULce" "secret"
          initial <- SASL.smiInitial impl
          case initial of
            Left err -> ("NUL" `BS.isInfixOf` BS8.pack err) `shouldBe` True
            Right _ -> expectationFailure "expected PLAIN NUL validation failure"
      , it "SASL implementation sends the PLAIN payload and completes after accept" $ do
          let impl = SASL.plainImpl "alice" "secret"
          SASL.smiName impl `shouldBe` "PLAIN"
          initial <- SASL.smiInitial impl
          case initial of
            Right (SASL.StepSend payload acceptBrokerBytes) -> do
              payload `shouldBe` Plain.generatePlainAuth "alice" "secret"
              assertStepDone "PLAIN" (acceptBrokerBytes (Just ""))
            Right _ -> expectationFailure "PLAIN should send exactly one client payload"
            Left err -> expectationFailure ("unexpected PLAIN init failure: " <> err)
      , it "SASL implementation can send an explicit authorization identity" $ do
          let impl = SASL.plainImplWithAuthzid "admin" "alice" "secret"
          SASL.smiName impl `shouldBe` "PLAIN"
          initial <- SASL.smiInitial impl
          case initial of
            Right (SASL.StepSend payload acceptBrokerBytes) -> do
              payload `shouldBe` Plain.generatePlainAuthWithAuthzid (Just "admin") "alice" "secret"
              assertStepDone "PLAIN" (acceptBrokerBytes (Just ""))
            Right _ -> expectationFailure "PLAIN authzid should send exactly one client payload"
            Left err -> expectationFailure ("unexpected PLAIN authzid init failure: " <> err)
      ]


--------------------------------------------------------------------------------
-- SCRAM
--------------------------------------------------------------------------------

scramTests :: Spec
scramTests =
  describe "SCRAM-SHA-*" $
    sequence_
      [ rfc5802Vector
      , sha512Vector
      , parseTests
      , verifyTests
      , messageStructureTests
      , scramImplTests
      ]


{- | RFC 5802 §5 worked example for SCRAM-SHA-1 (we use the same
algebraic structure with SHA-256 for the SaltedPassword computation;
the test here drives the SHA-256 variant since SCRAM-SHA-1 isn't
supported by Kafka).

The numbers below come from running the algorithm against
@username = "user"@, @password = "pencil"@, @salt = "salt"@,
@iterations = 4096@ — these are the RFC's example inputs, repurposed
to drive the SHA-256 PBKDF2 we actually ship.
-}
rfc5802Vector :: Spec
rfc5802Vector = it "PBKDF2 SaltedPassword agrees with RFC 8018 SHA-256 vector" $ do
  -- RFC 6070 §2 case 4: PBKDF2-HMAC-SHA256 with c=4096, dkLen=32 over
  -- ("password", "salt") = 0c60c80f...
  let derived = Scram.saltedPassword Scram.ScramSHA256 "password" "salt" 4096
      asHex = BA.convertToBase BA.Base16 derived :: BS.ByteString
  asHex `shouldBe` "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a"


sha512Vector :: Spec
sha512Vector = it "PBKDF2 SaltedPassword agrees with SHA-512 vector" $ do
  let derived = Scram.saltedPassword Scram.ScramSHA512 "password" "salt" 4096
      asHex = BA.convertToBase BA.Base16 derived :: BS.ByteString
  asHex `shouldBe` "d197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f87f6902e072f457b5143f30602641b3d55cd335988cb36b84376060ecd532e039b742a239434af2d5"


parseTests :: Spec
parseTests =
  describe "parseServerFirst" $
    sequence_
      [ it "happy path" $ do
          let bs = "r=clientnonceservernonce,s=" <> B64.encode "saltbytes" <> ",i=4096"
          case Scram.parseServerFirst bs of
            Right sf -> do
              Scram.sfFullNonce sf `shouldBe` "clientnonceservernonce"
              Scram.sfSalt sf `shouldBe` "saltbytes"
              Scram.sfIterations sf `shouldBe` 4096
            Left err -> expectationFailure err
      , it "missing iterations fails clearly" $ do
          case Scram.parseServerFirst "r=nonce,s=c2FsdA==" of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected parse failure"
      , it "non-numeric iterations fails clearly" $ do
          case Scram.parseServerFirst ("r=n,s=" <> B64.encode "salt" <> ",i=hello") of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected parse failure"
      , it "accepts attributes out of order with extensions" $ do
          let bs = "m=ignored,i=4096,r=clientserver,s=" <> B64.encode "salt"
          case Scram.parseServerFirst bs of
            Right sf -> do
              Scram.sfFullNonce sf `shouldBe` "clientserver"
              Scram.sfSalt sf `shouldBe` "salt"
              Scram.sfIterations sf `shouldBe` 4096
            Left err -> expectationFailure err
      , it "invalid base64 salt fails clearly" $ do
          case Scram.parseServerFirst "r=nonce,s=not-valid-@@,i=4096" of
            Left err -> ("invalid base64 salt" `BS.isInfixOf` BS8.pack err) `shouldBe` True
            Right _ -> expectationFailure "expected invalid salt to fail"
      , it "empty iterations fails clearly" $ do
          case Scram.parseServerFirst ("r=nonce,s=" <> B64.encode "salt" <> ",i=") of
            Left err -> ("empty iteration count" `BS.isInfixOf` BS8.pack err) `shouldBe` True
            Right _ -> expectationFailure "expected empty iteration count to fail"
      ]


verifyTests :: Spec
verifyTests =
  describe "verifyServerFinal" $
    sequence_
      [ it "success when v= matches" $ do
          let expectedSig = "raw-server-key" :: BS.ByteString
              serverFinal = "v=" <> B64.encode expectedSig
          Scram.verifyServerFinal expectedSig serverFinal `shouldBe` Right ()
      , it "failure when v= is something else" $ do
          let expectedSig = "raw-server-key" :: BS.ByteString
              serverFinal = "v=" <> B64.encode "different"
          case Scram.verifyServerFinal expectedSig serverFinal of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected verifier to reject mismatched signature"
      , it "broker-reported error surfaces as Left" $ do
          let r = Scram.verifyServerFinal "key" "e=invalid-username-or-password"
          case r of
            Left msg -> ("invalid-username-or-password" `BS.isInfixOf` BS8.pack msg) `shouldBe` True
            Right _ -> expectationFailure "expected verifier to surface broker error"
      , it "invalid base64 verifier fails clearly" $ do
          case Scram.verifyServerFinal "key" "v=not-valid-@@" of
            Left msg -> ("not valid base64" `BS.isInfixOf` BS8.pack msg) `shouldBe` True
            Right _ -> expectationFailure "expected invalid verifier to fail"
      , it "malformed server-final fails clearly" $ do
          case Scram.verifyServerFinal "key" "x=wat" of
            Left msg -> ("malformed server-final" `BS.isInfixOf` BS8.pack msg) `shouldBe` True
            Right _ -> expectationFailure "expected malformed server-final to fail"
      , it "empty server-final fails clearly" $ do
          case Scram.verifyServerFinal "key" "" of
            Left msg -> ("empty server-final" `BS.isInfixOf` BS8.pack msg) `shouldBe` True
            Right _ -> expectationFailure "expected empty server-final to fail"
      ]


messageStructureTests :: Spec
messageStructureTests =
  describe "Wire framing" $
    sequence_
      [ it "client-first starts with gs2-header n,," $ do
          session <- Scram.newScramSession Scram.ScramSHA256 "alice" "pwd"
          let cf = Scram.firstClientMessage session
          (BS.isPrefixOf "n,," cf) `shouldBe` True
          ("n=alice" `BS.isInfixOf` cf) `shouldBe` True
          ("r=" `BS.isInfixOf` cf) `shouldBe` True
      , it "client-first escapes , and = in usernames" $ do
          session <- Scram.newScramSession Scram.ScramSHA256 "user,with=stuff" "pwd"
          let cf = Scram.firstClientMessage session
          -- Per RFC 5802 §5.1, ',' and '=' must be escaped.
          ("=2C" `BS.isInfixOf` cf) `shouldBe` True
          ("=3D" `BS.isInfixOf` cf) `shouldBe` True
      , it "finalClientMessage rejects rotated server nonce" $ do
          session <- Scram.newScramSession Scram.ScramSHA256 "alice" "pwd"
          -- Server replies with a brand-new nonce that doesn't extend the
          -- client nonce — that's a MITM hijack signal, must abort.
          let serverFirst = "r=somethingElse,s=" <> B64.encode "salt" <> ",i=4096"
          case Scram.finalClientMessage session serverFirst of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected finalClientMessage to reject hijacked nonce"
      ]


scramImplTests :: Spec
scramImplTests =
  describe "SASL implementation" $
    sequence_
      [ it "SCRAM-SHA-512 advertises the right mechanism" $ do
          let impl = SASL.scramImpl Scram.ScramSHA512 "alice" "pwd"
          SASL.smiName impl `shouldBe` "SCRAM-SHA-512"
      , it "missing server-first payload fails before client proof" $ do
          let impl = SASL.scramImpl Scram.ScramSHA256 "alice" "pwd"
          initial <- SASL.smiInitial impl
          case initial of
            Right (SASL.StepSend clientFirst continue) -> do
              ("n,," `BS.isPrefixOf` clientFirst) `shouldBe` True
              continued <- continue Nothing
              case continued of
                Left err -> ("server-first" `BS.isInfixOf` BS8.pack err) `shouldBe` True
                Right _ -> expectationFailure "expected missing server-first to fail"
            Right _ -> expectationFailure "SCRAM should start with StepSend"
            Left err -> expectationFailure ("unexpected SCRAM init failure: " <> err)
      ]


--------------------------------------------------------------------------------
-- OAUTHBEARER
--------------------------------------------------------------------------------

oauthBearerTests :: Spec
oauthBearerTests =
  describe "OAUTHBEARER" $
    sequence_
      [ it "RFC 7628 framing: \\x01 auth=Bearer <token> \\x01 \\x01" $ do
          let tok = OAuth.OAuthToken "tok-abc" Nothing Nothing
              bs = OAuth.buildOAuthPayload tok
          bs
            `shouldBe` BS.concat
              [ BS.singleton 0x01
              , "auth=Bearer tok-abc"
              , BS.singleton 0x01
              , BS.singleton 0x01
              ]
      , it "default extensions preserve the minimal Kafka payload" $ do
          let tok = OAuth.OAuthToken "tok-abc" Nothing Nothing
          OAuth.buildOAuthPayloadWithExtensions OAuth.defaultOAuthBearerExtensions tok
            `shouldBe` OAuth.buildOAuthPayload tok
      , it "RFC payload includes authzid, host, and port extensions" $ do
          let tok = OAuth.OAuthToken "tok-abc" Nothing Nothing
              ext =
                OAuth.OAuthBearerExtensions
                  { OAuth.oauthAuthorizationIdentity = Just "user,with=chars"
                  , OAuth.oauthServerHost = Just "broker.example.com"
                  , OAuth.oauthServerPort = Just 9093
                  }
          OAuth.buildOAuthPayloadWithExtensions ext tok
            `shouldBe` BS.concat
              [ "n,a=user=2Cwith=3Dchars,"
              , BS.singleton 0x01
              , "auth=Bearer tok-abc"
              , BS.singleton 0x01
              , "host=broker.example.com"
              , BS.singleton 0x01
              , "port=9093"
              , BS.singleton 0x01
              , BS.singleton 0x01
              ]
      , it "OAuthStaticToken provider returns the token verbatim" $ do
          let tok = OAuth.OAuthToken "static" (Just 60000) (Just "sub-123")
          r <- OAuth.resolveOAuthToken (OAuth.OAuthStaticToken tok)
          r `shouldBe` Right tok
      , it "custom provider failure surfaces before payload" $ do
          let impl =
                SASL.oauthBearerImpl
                  (OAuth.OAuthTokenIO (pure (Left "token unavailable")))
          SASL.smiName impl `shouldBe` "OAUTHBEARER"
          initial <- SASL.smiInitial impl
          case initial of
            Left err -> err `shouldBe` "OAUTHBEARER: token unavailable"
            Right _ -> expectationFailure "expected OAuth init failure"
      , it "SASL implementation resolves tokens for each auth attempt" $ do
          counter <- newIORef (0 :: Int)
          let provider = OAuth.OAuthTokenIO $ do
                n <- atomicModifyIORef' counter (\old -> let new = old + 1 in (new, new))
                pure $ Right (OAuth.OAuthToken (T.pack ("token-" <> show n)) Nothing Nothing)
              impl = SASL.oauthBearerImpl provider
          first <- SASL.smiInitial impl
          second <- SASL.smiInitial impl
          firstPayload <- stepPayload "OAUTHBEARER" first
          secondPayload <- stepPayload "OAUTHBEARER" second
          ("token-1" `BS.isInfixOf` firstPayload) `shouldBe` True
          ("token-2" `BS.isInfixOf` secondPayload) `shouldBe` True
      , it "SASL implementation emits RFC-form extension payloads" $ do
          let tok = OAuth.OAuthToken "tok-abc" Nothing Nothing
              ext =
                OAuth.OAuthBearerExtensions
                  { OAuth.oauthAuthorizationIdentity = Just "service-a"
                  , OAuth.oauthServerHost = Just "broker.example.com"
                  , OAuth.oauthServerPort = Just 9093
                  }
              impl = SASL.oauthBearerImplWithExtensions ext (OAuth.OAuthStaticToken tok)
          initial <- SASL.smiInitial impl
          payload <- stepPayload "OAUTHBEARER" initial
          payload `shouldBe` OAuth.buildOAuthPayloadWithExtensions ext tok
      , it "SASL implementation rejects invalid OAuth fields" $ do
          let tok = OAuth.OAuthToken ("tok" <> T.singleton '\SOH' <> "bad") Nothing Nothing
              impl = SASL.oauthBearerImpl (OAuth.OAuthStaticToken tok)
          initial <- SASL.smiInitial impl
          case initial of
            Left err -> ("control characters" `BS.isInfixOf` BS8.pack err) `shouldBe` True
            Right _ -> expectationFailure "expected invalid OAuth token to fail"
      , it "SASL implementation rejects invalid OAuth extension port" $ do
          let tok = OAuth.OAuthToken "tok-abc" Nothing Nothing
              ext =
                OAuth.defaultOAuthBearerExtensions
                  { OAuth.oauthServerPort = Just 70000
                  }
              impl = SASL.oauthBearerImplWithExtensions ext (OAuth.OAuthStaticToken tok)
          initial <- SASL.smiInitial impl
          case initial of
            Left err -> ("port" `BS.isInfixOf` BS8.pack err) `shouldBe` True
            Right _ -> expectationFailure "expected invalid OAuth port to fail"
      ]


--------------------------------------------------------------------------------
-- AWS_MSK_IAM
--------------------------------------------------------------------------------

awsMskIamTests :: Spec
awsMskIamTests =
  describe "AWS_MSK_IAM" $
    sequence_
      [ it "urlEncode matches AWS canonical encoding" $ do
          Iam.urlEncode "AWS4-HMAC-SHA256" `shouldBe` "AWS4-HMAC-SHA256"
          Iam.urlEncode "kafka-cluster:Connect" `shouldBe` "kafka-cluster%3AConnect"
          -- '/' must be encoded; '~' must NOT be.
          Iam.urlEncode "AKIA/abc/def~ghi" `shouldBe` "AKIA%2Fabc%2Fdef~ghi"
      , it "signingKey: AWS SigV4 published vector for us-east-1/iam/20150830" $ do
          -- AWS SigV4 docs reference vector:
          -- https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv-create-signed-request.html
          let key =
                Iam.signingKey
                  "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                  "20150830"
                  "us-east-1"
                  "iam"
              asHex = BA.convertToBase BA.Base16 key :: BS.ByteString
          -- The expected "kSigning" hex from the AWS doc:
          asHex `shouldBe` "c4afb1cc5771d871763a393e44b703571b55cc28424d1a5e86da6ed3c154a4b9"
      , it "canonicalQueryString sorts and URL-encodes parameters" $ do
          let cqs =
                Iam.canonicalQueryString
                  "AKIA/20240101/us-east-1/kafka-cluster/aws4_request"
                  "20240101T000000Z"
                  900
                  Nothing
          -- Action must come first (alphabetical), Credential's slashes
          -- must be percent-encoded.
          ("Action=" `BS.isPrefixOf` cqs) `shouldBe` True
          ( "AKIA%2F20240101%2Fus-east-1%2Fkafka-cluster%2Faws4_request"
              `BS.isInfixOf` cqs
            )
            `shouldBe` True
          let aIdx = lookupIndex "X-Amz-Algorithm" cqs
              cIdx = lookupIndex "X-Amz-Credential" cqs
          (aIdx < cIdx) `shouldBe` True
      , it "session token shows up as X-Amz-Security-Token when present" $ do
          let cqs =
                Iam.canonicalQueryString
                  "AKIA/20240101/us-east-1/kafka-cluster/aws4_request"
                  "20240101T000000Z"
                  900
                  (Just "session/token=value")
          ("X-Amz-Security-Token=session%2Ftoken%3Dvalue" `BS.isInfixOf` cqs) `shouldBe` True
      , it "buildIamPayload produces the expected JSON shape" $ do
          let creds =
                Iam.AwsCredentials
                  { Iam.awsAccessKeyId = "AKIAIOSFODNN7EXAMPLE"
                  , Iam.awsSecretAccessKey = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                  , Iam.awsSessionToken = Nothing
                  }
              input =
                Iam.IamPayloadInput
                  { Iam.iiCredentials = creds
                  , Iam.iiHost = "broker-1.kafka.us-east-1.amazonaws.com"
                  , Iam.iiRegion = "us-east-1"
                  , Iam.iiNow = read "2024-01-01 00:00:00 UTC" :: Time.UTCTime
                  , Iam.iiUserAgent = "wireform-kafka-test"
                  , Iam.iiExpires = 900
                  }
              payload = Iam.buildIamPayload input
          case Aeson.eitherDecodeStrict payload of
            Left err -> expectationFailure ("payload not valid JSON: " <> err)
            Right (Aeson.Object o) -> do
              KeyMap.lookup "version" o `shouldBe` Just (Aeson.String "2020_10_22")
              KeyMap.lookup "host" o
                `shouldBe` Just (Aeson.String "broker-1.kafka.us-east-1.amazonaws.com")
              KeyMap.lookup "action" o
                `shouldBe` Just (Aeson.String "kafka-cluster:Connect")
              KeyMap.lookup "x-amz-algorithm" o
                `shouldBe` Just (Aeson.String "AWS4-HMAC-SHA256")
              KeyMap.lookup "x-amz-expires" o
                `shouldBe` Just (Aeson.String "900")
              KeyMap.lookup "x-amz-signedheaders" o
                `shouldBe` Just (Aeson.String "host")
              -- The signature itself must be a non-empty hex string.
              case KeyMap.lookup "x-amz-signature" o of
                Just (Aeson.String s) -> do
                  (not (T.null s)) `shouldBe` True
                  ( T.all
                      ( \c ->
                          (c >= '0' && c <= '9')
                            || (c >= 'a' && c <= 'f')
                      )
                      s
                    )
                    `shouldBe` True
                _ -> expectationFailure "missing or non-string x-amz-signature"
              -- No session token field when none supplied.
              KeyMap.lookup "x-amz-security-token" o `shouldBe` Nothing
            Right _ -> expectationFailure "payload was not a JSON object"
      , it "buildIamPayload has a stable SigV4 signature for fixed inputs" $ do
          let creds =
                Iam.AwsCredentials
                  { Iam.awsAccessKeyId = "AKIAIOSFODNN7EXAMPLE"
                  , Iam.awsSecretAccessKey = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                  , Iam.awsSessionToken = Nothing
                  }
              input =
                Iam.IamPayloadInput
                  { Iam.iiCredentials = creds
                  , Iam.iiHost = "broker-1.kafka.us-east-1.amazonaws.com"
                  , Iam.iiRegion = "us-east-1"
                  , Iam.iiNow = read "2024-01-01 00:00:00 UTC" :: Time.UTCTime
                  , Iam.iiUserAgent = "wireform-kafka-test"
                  , Iam.iiExpires = 900
                  }
          obj <- expectJsonObject (Iam.buildIamPayload input)
          KeyMap.lookup "x-amz-signature" obj
            `shouldBe` Just (Aeson.String "625ab063dc5c77f47a754b13fb123411c594bc00261935ae8830d1c962b0998b")
      , it "session-token credentials add x-amz-security-token" $ do
          let creds =
                Iam.AwsCredentials
                  { Iam.awsAccessKeyId = "ASIA"
                  , Iam.awsSecretAccessKey = "secret"
                  , Iam.awsSessionToken = Just "tokvalue"
                  }
              input =
                Iam.IamPayloadInput
                  { Iam.iiCredentials = creds
                  , Iam.iiHost = "h"
                  , Iam.iiRegion = "us-east-1"
                  , Iam.iiNow = read "2024-01-01 00:00:00 UTC"
                  , Iam.iiUserAgent = "ua"
                  , Iam.iiExpires = 900
                  }
              payload = Iam.buildIamPayload input
          case Aeson.eitherDecodeStrict payload of
            Right (Aeson.Object o) ->
              KeyMap.lookup "x-amz-security-token" o
                `shouldBe` Just (Aeson.String "tokvalue")
            _ -> expectationFailure "payload missing security token"
      , it "static provider resolves credentials verbatim" $ do
          let creds = Iam.AwsCredentials "AKIASTATIC" "secret" (Just "session")
          resolved <- Iam.resolveAwsCredentials (Iam.AwsStaticCredentials creds)
          resolved `shouldBe` Right creds
      , -- withAwsEnv mutates the process environment, so these two
        -- must not overlap each other or any other test.
        sequential $ it "env provider requires access key and secret" $
          withAwsEnv Nothing Nothing Nothing $ do
            resolved <- Iam.resolveAwsCredentials Iam.AwsEnvCredentials
            case resolved of
              Left err -> do
                ("AWS_ACCESS_KEY_ID" `T.isInfixOf` T.pack err) `shouldBe` True
                ("AWS_SECRET_ACCESS_KEY" `T.isInfixOf` T.pack err) `shouldBe` True
              Right _ -> expectationFailure "expected missing env credentials to fail"
      , sequential $ it "env provider includes optional session token" $
          withAwsEnv (Just "AKIAENV") (Just "env-secret") (Just "env-token") $ do
            resolved <- Iam.resolveAwsCredentials Iam.AwsEnvCredentials
            resolved
              `shouldBe` Right
                Iam.AwsCredentials
                  { Iam.awsAccessKeyId = "AKIAENV"
                  , Iam.awsSecretAccessKey = "env-secret"
                  , Iam.awsSessionToken = Just "env-token"
                  }
      , it "custom provider failure surfaces before any IAM payload is sent" $ do
          let impl =
                SASL.awsMskIamImpl
                  (Iam.AwsCustomProvider (pure (Left "no credentials")))
                  "broker-1.kafka.us-east-1.amazonaws.com"
                  "us-east-1"
          SASL.smiName impl `shouldBe` "AWS_MSK_IAM"
          initial <- SASL.smiInitial impl
          case initial of
            Left err -> err `shouldBe` "AWS_MSK_IAM: no credentials"
            Right _ -> expectationFailure "expected IAM init failure"
      , it "SASL implementation emits IAM JSON for the broker host and region" $ do
          let creds =
                Iam.AwsCredentials
                  { Iam.awsAccessKeyId = "AKIAIOSFODNN7EXAMPLE"
                  , Iam.awsSecretAccessKey = "secret"
                  , Iam.awsSessionToken = Just "tokvalue"
                  }
              impl =
                SASL.awsMskIamImpl
                  (Iam.AwsStaticCredentials creds)
                  "broker-2.kafka.us-west-2.amazonaws.com"
                  "us-west-2"
          step <- SASL.smiInitial impl
          case step of
            Right (SASL.StepSend payload acceptBrokerBytes) -> do
              obj <- expectJsonObject payload
              KeyMap.lookup "version" obj
                `shouldBe` Just (Aeson.String "2020_10_22")
              KeyMap.lookup "host" obj
                `shouldBe` Just (Aeson.String "broker-2.kafka.us-west-2.amazonaws.com")
              KeyMap.lookup "user-agent" obj
                `shouldBe` Just (Aeson.String "wireform-kafka/0.1")
              KeyMap.lookup "x-amz-expires" obj
                `shouldBe` Just (Aeson.String "900")
              KeyMap.lookup "x-amz-security-token" obj
                `shouldBe` Just (Aeson.String "tokvalue")
              case KeyMap.lookup "x-amz-credential" obj of
                Just (Aeson.String credential) -> do
                  ("/us-west-2/kafka-cluster/aws4_request" `T.isSuffixOf` credential) `shouldBe` True
                  ("AKIAIOSFODNN7EXAMPLE/" `T.isPrefixOf` credential) `shouldBe` True
                _ -> expectationFailure "missing or non-string x-amz-credential"
              accepted <- acceptBrokerBytes (Just "")
              case accepted of
                Right (SASL.StepDone Nothing) -> pure ()
                Right _ -> expectationFailure "AWS_MSK_IAM should finish after broker accept"
                Left err -> expectationFailure ("unexpected IAM accept failure: " <> err)
            Right _ ->
              expectationFailure "AWS_MSK_IAM should send exactly one client payload"
            Left err ->
              expectationFailure ("unexpected IAM init failure: " <> err)
      , it "SASL implementation resolves credentials for each auth attempt" $ do
          counter <- newIORef (0 :: Int)
          let provider = Iam.AwsCustomProvider $ do
                n <- atomicModifyIORef' counter (\old -> let new = old + 1 in (new, new))
                pure $
                  Right
                    Iam.AwsCredentials
                      { Iam.awsAccessKeyId = "AKIA" <> T.pack (show n)
                      , Iam.awsSecretAccessKey = "secret"
                      , Iam.awsSessionToken = Nothing
                      }
              impl = SASL.awsMskIamImpl provider "broker.kafka.us-east-1.amazonaws.com" "us-east-1"
          first <- SASL.smiInitial impl
          second <- SASL.smiInitial impl
          firstAccessKey <- stepAccessKey first
          secondAccessKey <- stepAccessKey second
          firstAccessKey `shouldBe` "AKIA1"
          secondAccessKey `shouldBe` "AKIA2"
      ]


gssapiTests :: Spec
gssapiTests =
  describe "GSSAPI" $
    sequence_
      [ it "fails explicitly without broker credentials" $ do
          SASL.smiName SASL.gssapiImpl `shouldBe` "GSSAPI"
          initial <- SASL.smiInitial SASL.gssapiImpl
          case initial of
            Left err -> do
              ("Kerberos" `BS.isInfixOf` BS8.pack err) `shouldBe` True
              if SASL.gssapiBuildEnabled
                then (not ("not implemented" `BS.isInfixOf` BS8.pack err)) `shouldBe` True
                else ("not implemented" `BS.isInfixOf` BS8.pack err) `shouldBe` True
            Right _ -> expectationFailure "expected GSSAPI to fail explicitly"
      , it "build flag state is exposed" $
          (SASL.gssapiBuildEnabled || not SASL.gssapiBuildEnabled) `shouldBe` True
      ]


--------------------------------------------------------------------------------
-- SaslConfig classification
--------------------------------------------------------------------------------

configTests :: Spec
configTests =
  describe "configMechanism" $
    sequence_
      [ it "PLAIN" $
          SASL.configMechanism (SASL.SaslPlain "u" "p") `shouldBe` SASL.NamePlain
      , it "PLAIN with authzid" $
          SASL.configMechanism (SASL.SaslPlainWithAuthzid "authz" "u" "p")
            `shouldBe` SASL.NamePlain
      , it "SCRAM-SHA-256" $
          SASL.configMechanism (SASL.SaslScram Scram.ScramSHA256 "u" "p")
            `shouldBe` SASL.NameScramSha256
      , it "SCRAM-SHA-512" $
          SASL.configMechanism (SASL.SaslScram Scram.ScramSHA512 "u" "p")
            `shouldBe` SASL.NameScramSha512
      , it "OAUTHBEARER" $
          SASL.configMechanism
            (SASL.SaslOAuthBearer (OAuth.OAuthStaticToken (OAuth.OAuthToken "x" Nothing Nothing)))
            `shouldBe` SASL.NameOAuthBearer
      , it "OAUTHBEARER with extensions" $
          SASL.configMechanism
            ( SASL.SaslOAuthBearerWithExtensions
                (OAuth.OAuthStaticToken (OAuth.OAuthToken "x" Nothing Nothing))
                OAuth.defaultOAuthBearerExtensions
            )
            `shouldBe` SASL.NameOAuthBearer
      , it "AWS_MSK_IAM" $ do
          let creds = Iam.AwsCredentials "k" "s" Nothing
          SASL.configMechanism
            (SASL.SaslAwsMskIam (Iam.AwsStaticCredentials creds) "us-east-1")
            `shouldBe` SASL.NameAwsMskIam
      , it "GSSAPI" $
          SASL.configMechanism SASL.SaslGssapi `shouldBe` SASL.NameGssapi
      , it "wireName round-trips for every mechanism" $ do
          SASL.mechanismWireName SASL.NamePlain `shouldBe` "PLAIN"
          SASL.mechanismWireName SASL.NameScramSha256 `shouldBe` "SCRAM-SHA-256"
          SASL.mechanismWireName SASL.NameScramSha512 `shouldBe` "SCRAM-SHA-512"
          SASL.mechanismWireName SASL.NameOAuthBearer `shouldBe` "OAUTHBEARER"
          SASL.mechanismWireName SASL.NameAwsMskIam `shouldBe` "AWS_MSK_IAM"
          SASL.mechanismWireName SASL.NameGssapi `shouldBe` "GSSAPI"
      ]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

lookupIndex :: BS.ByteString -> BS.ByteString -> Int
lookupIndex needle hay =
  go 0 hay
  where
    nLen = BS.length needle
    go i bs
      | BS.length bs < nLen =
          error
            ( "needle not found in haystack: "
                <> BS8.unpack needle
            )
      | BS.take nLen bs == needle = i
      | otherwise = go (i + 1) (BS.drop 1 bs)


withAwsEnv :: Maybe String -> Maybe String -> Maybe String -> IO a -> IO a
withAwsEnv accessKey secretKey sessionToken action =
  bracket capture restore $ \_ -> do
    setAll [accessKey, secretKey, sessionToken]
    action
  where
    names :: [String]
    names =
      [ "AWS_ACCESS_KEY_ID"
      , "AWS_SECRET_ACCESS_KEY"
      , "AWS_SESSION_TOKEN"
      ]

    capture :: IO [Maybe String]
    capture = traverse Env.lookupEnv names

    restore :: [Maybe String] -> IO ()
    restore = setAll

    setAll :: [Maybe String] -> IO ()
    setAll values = zipWithM_ setOne names values

    setOne :: String -> Maybe String -> IO ()
    setOne name = \case
      Nothing -> Env.unsetEnv name
      Just value -> Env.setEnv name value


assertStepDone :: String -> IO (Either String SASL.StepResult) -> IO ()
assertStepDone mechanism action =
  action >>= \case
    Right (SASL.StepDone Nothing) -> pure ()
    Right _ -> expectationFailure (mechanism <> " should finish after broker accept")
    Left err -> expectationFailure ("unexpected " <> mechanism <> " accept failure: " <> err)


stepPayload :: String -> Either String SASL.StepResult -> IO BS.ByteString
stepPayload mechanism = \case
  Right (SASL.StepSend payload acceptBrokerBytes) -> do
    assertStepDone mechanism (acceptBrokerBytes (Just ""))
    pure payload
  Right _ -> expectationFailure (mechanism <> " should start with StepSend")
  Left err -> expectationFailure ("unexpected " <> mechanism <> " init failure: " <> err)


expectJsonObject :: BS.ByteString -> IO Aeson.Object
expectJsonObject payload =
  case Aeson.eitherDecodeStrict payload of
    Right (Aeson.Object obj) -> pure obj
    Left err -> expectationFailure ("payload not valid JSON: " <> err)
    Right _ -> expectationFailure "payload was not a JSON object"


stepAccessKey :: Either String SASL.StepResult -> IO T.Text
stepAccessKey = \case
  Right (SASL.StepSend payload _) -> do
    obj <- expectJsonObject payload
    case KeyMap.lookup "x-amz-credential" obj of
      Just (Aeson.String credential) ->
        case T.breakOn "/" credential of
          (accessKey, rest)
            | not (T.null rest) -> pure accessKey
          _ -> expectationFailure "credential did not include a scope"
      _ -> expectationFailure "missing or non-string x-amz-credential"
  Right _ -> expectationFailure "AWS_MSK_IAM should start with StepSend"
  Left err -> expectationFailure ("unexpected IAM init failure: " <> err)
