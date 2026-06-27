{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for "Network.HTTP.Headers.WWWAuthenticate".

The most important things to cover are:

* Scheme-aware splitting: the @\",\"@ that separates challenges
  must be distinguished from the @\",\"@ that separates
  @auth-param@s inside a single challenge.
* Quoted-string disambiguation: a comma inside a quoted-string
  value must not split a challenge.
* token68 vs auth-param: a payload like @abc.def_ghi+jKL/=@ must
  be parsed as a token68, while @realm=...@ kicks in the
  auth-param branch.
* RFC 9110 §5.6.1 extended list form (leading / trailing /
  stacked commas).
* Renderer correctness: quoted-string escaping for @\"@ and
  @\\\\@; renderer + parser round-trip is structurally stable.
-}
module Test.Hermes.WWWAuthenticate (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (
  RFC8941String,
  Result (..),
  mkRFC8941String,
  runParser,
  unsafeToRFC8941String,
 )
import qualified Network.HTTP.Headers.WWWAuthenticate as W
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOk :: ByteString -> Either String [W.AuthChallenge]
parseOk bs = case runParser W.challengesParser bs of
  OK cs leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right cs
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: [W.AuthChallenge] -> ByteString
render = M.toStrictByteString . W.renderWWWAuthenticate . W.WWWAuthenticate


st :: ByteString -> ST.ShortText
st bs = case ST.fromByteString bs of
  Just s -> s
  Nothing -> error "non-UTF-8 short text in test"


scheme :: ByteString -> W.AuthScheme
scheme = W.AuthScheme . st


bytesOfRFC8941 :: RFC8941String -> ByteString
bytesOfRFC8941 = ST.toByteString . unsafeToRFC8941String


paramToken :: ByteString -> ByteString -> (ST.ShortText, W.CredentialParam)
paramToken k v = (st k, W.CredentialParamToken (st v))


{- | Build a quoted-string credential parameter from arbitrary
ASCII bytes. Bytes that fall outside the RFC 8941 string set
(e.g. CR / LF) cause a test-time failure.
-}
paramString :: ByteString -> ByteString -> (ST.ShortText, W.CredentialParam)
paramString k v = case mkRFC8941String (st v) of
  Just s -> (st k, W.CredentialParamString s)
  Nothing -> error ("paramString: value not RFC8941-stringable: " <> show v)


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- An RFC 9110 'token' fragment (alpha / digit) — keep generated
-- values trivially round-trippable.
tokenChar :: Gen Char
tokenChar =
  Gen.frequency
    [ (52, Gen.alpha)
    , (10, Gen.digit)
    ]


tokenBS :: Gen ByteString
tokenBS =
  BS.pack . map (fromIntegral . fromEnum)
    <$> Gen.list (Range.linear 1 10) tokenChar


quotedStringContent :: Gen ByteString
quotedStringContent = do
  -- Restrict to printable ASCII minus DQUOTE / backslash so the
  -- renderer's escape logic isn't part of the round-trip
  -- equality check. We have a dedicated unit test for the
  -- escape behaviour.
  let safe w = w >= 0x20 && w <= 0x7E && w /= 0x22 && w /= 0x5C
  bs <- Gen.bytes (Range.linear 0 20)
  pure (BS.map (\w -> if safe w then w else 0x61) bs)


paramGen :: Gen (ST.ShortText, W.CredentialParam)
paramGen = do
  k <- tokenBS
  Gen.choice
    [ paramToken k <$> tokenBS
    , paramString k <$> quotedStringContent
    ]


challengeGen :: Gen W.AuthChallenge
challengeGen = do
  s <- tokenBS
  ps <- Gen.list (Range.linear 0 4) paramGen
  pure
    W.AuthChallenge
      { W.challengeScheme = scheme s
      , W.challengeContents =
          if null ps
            then W.ChallengeBare
            else W.ChallengeParams ps
      }


-- ---------------------------------------------------------------------------
-- Unit tests
-- ---------------------------------------------------------------------------

unit_basic :: Spec
unit_basic = it "Basic realm" $
  case parseOk "Basic realm=\"example\"" of
    Right [ch] -> do
      W.challengeScheme ch `shouldBe` scheme "Basic"
      case W.challengeContents ch of
        W.ChallengeParams ps -> length ps `shouldBe` 1
        _ -> error "expected ChallengeParams"
    other -> error ("unexpected parse result: " <> show other)


unit_multi_challenge :: Spec
unit_multi_challenge =
  it "multi-challenge values" $
    let raw = "Basic realm=\"x\", Bearer realm=\"y\", scope=\"r\""
    in case parseOk raw of
        Right [b, br] -> do
          W.challengeScheme b `shouldBe` scheme "Basic"
          W.challengeScheme br `shouldBe` scheme "Bearer"
          case W.challengeContents br of
            W.ChallengeParams ps ->
              length ps `shouldBe` 2
            _ -> error "expected ChallengeParams for Bearer"
        other -> error ("expected exactly two challenges, got: " <> show other)


unit_token68 :: Spec
unit_token68 = it "token68 payload" $
  case parseOk "Bearer abc.def_ghi+jKL/=" of
    Right [ch] -> case W.challengeContents ch of
      W.ChallengeToken68 t -> t `shouldBe` "abc.def_ghi+jKL/="
      _ -> error "expected ChallengeToken68"
    other -> error ("unexpected parse: " <> show other)


unit_digest_qop_list :: Spec
unit_digest_qop_list =
  it "Digest challenge with multi-token qop" $
    let raw =
          "Digest realm=\"api\", qop=\"auth,auth-int\", nonce=\"abc\", \
          \algorithm=SHA-256, opaque=\"o\""
    in case parseOk raw of
        Right [ch] -> case W.challengeContents ch of
          W.ChallengeParams ps ->
            length ps `shouldBe` 5
          _ -> error "expected ChallengeParams"
        other -> error ("expected one challenge, got: " <> show other)


unit_empty_list_form :: Spec
unit_empty_list_form = it "RFC 9110 §5.6.1 stacked-comma form" $
  case parseOk ", Basic realm=\"x\" ,," of
    Right [ch] -> W.challengeScheme ch `shouldBe` scheme "Basic"
    other -> error ("expected one challenge, got: " <> show other)


unit_quoted_with_comma :: Spec
unit_quoted_with_comma = it "comma inside quoted-string is not a separator" $
  case parseOk "Bearer realm=\"a, b\"" of
    Right [ch] -> case W.challengeContents ch of
      W.ChallengeParams [(k, W.CredentialParamString s)] -> do
        k `shouldBe` st "realm"
        bytesOfRFC8941 s `shouldBe` "a, b"
      _ -> error "expected single quoted param"
    other -> error ("expected one challenge, got: " <> show other)


unit_quoted_escape_render :: Spec
unit_quoted_escape_render = it "renderer escapes \" and \\ in quoted-string" $ do
  let v = "a\"b\\c"
      ch =
        W.AuthChallenge
          { W.challengeScheme = scheme "Basic"
          , W.challengeContents = W.ChallengeParams [paramString "realm" v]
          }
      out = render [ch]
  (if "\\\"" `BS.isInfixOf` out then pure () else expectationFailure ("backslash-quote in: " <> show out))
  (if "\\\\" `BS.isInfixOf` out then pure () else expectationFailure ("double-backslash in: " <> show out))
  -- And the round-trip recovers the original payload.
  case parseOk out of
    Right [ch'] -> case W.challengeContents ch' of
      W.ChallengeParams [(_, W.CredentialParamString s)] ->
        bytesOfRFC8941 s `shouldBe` v
      _ -> error "expected single quoted param after round-trip"
    other -> error ("round-trip parse failed: " <> show other)


-- ---------------------------------------------------------------------------
-- Property: render → parse round-trip
-- ---------------------------------------------------------------------------

prop_roundtrip :: Property
prop_roundtrip = property $ do
  cs <- forAll (Gen.list (Range.linear 1 4) challengeGen)
  let rendered = render cs
  H.annotateShow rendered
  case parseOk rendered of
    Left err -> H.annotate err >> H.failure
    Right cs' -> structuralEq cs cs' === True
  where
    structuralEq xs ys =
      length xs == length ys
        && all
          ( \(a, b) ->
              W.challengeScheme a == W.challengeScheme b
                && contentsEq (W.challengeContents a) (W.challengeContents b)
          )
          (zip xs ys)
    contentsEq W.ChallengeBare W.ChallengeBare = True
    contentsEq W.ChallengeBare (W.ChallengeParams []) = True
    contentsEq (W.ChallengeParams []) W.ChallengeBare = True
    contentsEq (W.ChallengeToken68 a) (W.ChallengeToken68 b) = a == b
    contentsEq (W.ChallengeParams a) (W.ChallengeParams b) =
      length a == length b && all paramEq (zip a b)
    contentsEq _ _ = False
    paramEq ((k1, v1), (k2, v2)) = k1 == k2 && credEq v1 v2
    credEq (W.CredentialParamToken a) (W.CredentialParamToken b) = a == b
    credEq (W.CredentialParamString a) (W.CredentialParamString b) = a == b
    -- Cross-shape: a token-valued param renders byte-identically
    -- to the quoted-string form of the same payload; the parser
    -- can pick either shape on parse. Accept both.
    credEq (W.CredentialParamToken t) (W.CredentialParamString s) =
      ST.toShortByteString t == ST.toShortByteString (unsafeToRFC8941String s)
    credEq (W.CredentialParamString s) (W.CredentialParamToken t) =
      ST.toShortByteString t == ST.toShortByteString (unsafeToRFC8941String s)


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "WWWAuthenticate" $
    sequence_
      [ unit_basic
      , unit_multi_challenge
      , unit_token68
      , unit_digest_qop_list
      , unit_empty_list_form
      , unit_quoted_with_comma
      , unit_quoted_escape_render
      , it "challenges round-trip through render → parse" prop_roundtrip
      ]
