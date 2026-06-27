{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Digest (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ContentDigest as CD
import qualified Network.HTTP.Headers.ContentMD5 as MD5
import qualified Network.HTTP.Headers.Digest as DG
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.ReprDigest as RD
import qualified Network.HTTP.Headers.WantContentDigest as WCD
import qualified Network.HTTP.Headers.WantDigest as WD
import qualified Network.HTTP.Headers.WantReprDigest as WRD
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toEither :: Result String a -> Either String a
toEither r = case r of
  OK v rest
    | BS.null (BS.dropWhile ws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e
  where
    ws w = w == 0x20 || w == 0x09


-- A dictionary @key@ starting with a lowercase letter (RFC 8941 §3.1.2).
genKey :: Gen ST.ShortText
genKey = do
  c <- Gen.element ['a' .. 'z']
  rest <- Gen.list (Range.linear 0 8) (Gen.element (['a' .. 'z'] ++ ['0' .. '9'] ++ "-._"))
  pure (ST.pack (c : rest))


genNE :: Gen a -> Gen (NonEmpty a)
genNE g = do
  x <- g
  xs <- Gen.list (Range.linear 0 3) g
  pure (x :| xs)


genContentDigest :: Gen CD.ContentDigest
genContentDigest = fmap CD.ContentDigest (genNE entry)
  where
    entry = do
      k <- genKey
      v <- Gen.bytes (Range.linear 0 64)
      pure (CD.ContentDigestEntry k v [])


genReprDigest :: Gen RD.ReprDigest
genReprDigest = fmap RD.ReprDigest (genNE entry)
  where
    entry = do
      k <- genKey
      v <- Gen.bytes (Range.linear 0 64)
      pure (RD.ReprDigestEntry k v [])


genWantContentDigest :: Gen WCD.WantContentDigest
genWantContentDigest = fmap WCD.WantContentDigest (genNE entry)
  where
    entry = do
      k <- genKey
      p <- Gen.int (Range.linear 0 10)
      pure (WCD.WantContentDigestEntry k p [])


genWantReprDigest :: Gen WRD.WantReprDigest
genWantReprDigest = fmap WRD.WantReprDigest (genNE entry)
  where
    entry = do
      k <- genKey
      p <- Gen.int (Range.linear 0 10)
      pure (WRD.WantReprDigestEntry k p [])


-- ---------------------------------------------------------------------------
-- Content-Digest (RFC 9530 §2)
-- ---------------------------------------------------------------------------

unit_contentDigest_parse :: Spec
unit_contentDigest_parse = it "parses a sha-256 content-digest" $
  case toEither (runParser CD.contentDigestParser "sha-256=:RK/0qy18MlBSVnWgNXKtPNwc4yQQnGzpdvc6tNkXLDg=:") of
    Right (CD.ContentDigest (e :| [])) -> do
      CD.contentDigestAlgorithm e `shouldBe` ST.fromString "sha-256"
      BS.length (CD.contentDigestValue e) `shouldBe` 32
    other -> error (show other)


unit_contentDigest_render :: Spec
unit_contentDigest_render =
  it "renders a content-digest dictionary" $
    let v = CD.ContentDigest (CD.ContentDigestEntry (ST.fromString "sha-256") (BS.pack [0, 0, 0]) [] :| [])
    in M.toStrictByteString (CD.renderContentDigest v) `shouldBe` "sha-256=:AAAA:"


prop_contentDigest :: Property
prop_contentDigest = property $ do
  v <- forAll genContentDigest
  toEither (runParser CD.contentDigestParser (M.toStrictByteString (CD.renderContentDigest v))) === Right v


-- ---------------------------------------------------------------------------
-- Repr-Digest (RFC 9530 §3)
-- ---------------------------------------------------------------------------

unit_reprDigest_parse :: Spec
unit_reprDigest_parse = it "parses a sha-256 repr-digest" $
  case toEither (runParser RD.reprDigestParser "sha-256=:RK/0qy18MlBSVnWgNXKtPNwc4yQQnGzpdvc6tNkXLDg=:") of
    Right (RD.ReprDigest (e :| [])) -> do
      RD.reprDigestAlgorithm e `shouldBe` ST.fromString "sha-256"
      BS.length (RD.reprDigestValue e) `shouldBe` 32
    other -> error (show other)


unit_reprDigest_render :: Spec
unit_reprDigest_render =
  it "renders a repr-digest dictionary" $
    let v = RD.ReprDigest (RD.ReprDigestEntry (ST.fromString "sha-256") (BS.pack [0, 0, 0]) [] :| [])
    in M.toStrictByteString (RD.renderReprDigest v) `shouldBe` "sha-256=:AAAA:"


prop_reprDigest :: Property
prop_reprDigest = property $ do
  v <- forAll genReprDigest
  toEither (runParser RD.reprDigestParser (M.toStrictByteString (RD.renderReprDigest v))) === Right v


-- ---------------------------------------------------------------------------
-- Want-Content-Digest (RFC 9530 §4)
-- ---------------------------------------------------------------------------

unit_wantContentDigest_parse :: Spec
unit_wantContentDigest_parse = it "parses a want-content-digest dictionary" $
  case toEither (runParser WCD.wantContentDigestParser "sha-256=1, sha-512=3") of
    Right (WCD.WantContentDigest (a :| [b])) -> do
      WCD.wantContentDigestAlgorithm a `shouldBe` ST.fromString "sha-256"
      WCD.wantContentDigestPreference a `shouldBe` 1
      WCD.wantContentDigestAlgorithm b `shouldBe` ST.fromString "sha-512"
      WCD.wantContentDigestPreference b `shouldBe` 3
    other -> error (show other)


unit_wantContentDigest_render :: Spec
unit_wantContentDigest_render =
  it "renders a want-content-digest dictionary" $
    let v =
          WCD.WantContentDigest
            ( WCD.WantContentDigestEntry (ST.fromString "sha-256") 3 []
                :| [WCD.WantContentDigestEntry (ST.fromString "sha-512") 10 []]
            )
    in M.toStrictByteString (WCD.renderWantContentDigest v) `shouldBe` "sha-256=3, sha-512=10"


prop_wantContentDigest :: Property
prop_wantContentDigest = property $ do
  v <- forAll genWantContentDigest
  toEither (runParser WCD.wantContentDigestParser (M.toStrictByteString (WCD.renderWantContentDigest v))) === Right v


-- ---------------------------------------------------------------------------
-- Want-Repr-Digest (RFC 9530 §4)
-- ---------------------------------------------------------------------------

unit_wantReprDigest_parse :: Spec
unit_wantReprDigest_parse = it "parses a want-repr-digest dictionary" $
  case toEither (runParser WRD.wantReprDigestParser "sha-512=10, sha-256=3") of
    Right (WRD.WantReprDigest (a :| [b])) -> do
      WRD.wantReprDigestAlgorithm a `shouldBe` ST.fromString "sha-512"
      WRD.wantReprDigestPreference a `shouldBe` 10
      WRD.wantReprDigestPreference b `shouldBe` 3
    other -> error (show other)


unit_wantReprDigest_render :: Spec
unit_wantReprDigest_render =
  it "renders a want-repr-digest dictionary" $
    let v =
          WRD.WantReprDigest
            ( WRD.WantReprDigestEntry (ST.fromString "sha-256") 3 []
                :| [WRD.WantReprDigestEntry (ST.fromString "sha-512") 10 []]
            )
    in M.toStrictByteString (WRD.renderWantReprDigest v) `shouldBe` "sha-256=3, sha-512=10"


prop_wantReprDigest :: Property
prop_wantReprDigest = property $ do
  v <- forAll genWantReprDigest
  toEither (runParser WRD.wantReprDigestParser (M.toStrictByteString (WRD.renderWantReprDigest v))) === Right v


-- ---------------------------------------------------------------------------
-- Digest (RFC 3230, obsolete)
-- ---------------------------------------------------------------------------

unit_digest_parse :: Spec
unit_digest_parse = it "parses an obsolete Digest list" $
  case toEither (runParser DG.digestParser "md5=HUXZLQLMuI/KZ5KDcJPcOA==, sha-256=X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE=") of
    Right (DG.Digest (a :| [b])) -> do
      DG.digestAlgorithm a `shouldBe` ST.fromString "md5"
      DG.digestEncoded a `shouldBe` ST.fromString "HUXZLQLMuI/KZ5KDcJPcOA=="
      DG.digestAlgorithm b `shouldBe` ST.fromString "sha-256"
      DG.digestEncoded b `shouldBe` ST.fromString "X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE="
    other -> error (show other)


unit_digest_render :: Spec
unit_digest_render =
  it "renders an obsolete Digest list" $
    let v = DG.Digest (DG.DigestValue (ST.fromString "md5") (ST.fromString "HUXZLQLMuI/KZ5KDcJPcOA==") :| [])
    in M.toStrictByteString (DG.renderDigest v) `shouldBe` "md5=HUXZLQLMuI/KZ5KDcJPcOA=="


-- ---------------------------------------------------------------------------
-- Want-Digest (RFC 3230, obsolete)
-- ---------------------------------------------------------------------------

unit_wantDigest_parse :: Spec
unit_wantDigest_parse = it "parses an obsolete Want-Digest list with q-values" $
  case toEither (runParser WD.wantDigestParser "md5;q=0.3, sha;q=1, sha-256") of
    Right (WD.WantDigest (a :| [b, c])) -> do
      WD.wantDigestAlgorithm a `shouldBe` ST.fromString "md5"
      WD.wantDigestQ a `shouldBe` Just (ST.fromString "0.3")
      WD.wantDigestAlgorithm b `shouldBe` ST.fromString "sha"
      WD.wantDigestQ b `shouldBe` Just (ST.fromString "1")
      WD.wantDigestAlgorithm c `shouldBe` ST.fromString "sha-256"
      WD.wantDigestQ c `shouldBe` Nothing
    other -> error (show other)


unit_wantDigest_render :: Spec
unit_wantDigest_render =
  it "renders an obsolete Want-Digest list" $
    let v =
          WD.WantDigest
            ( WD.WantDigestValue (ST.fromString "md5") (Just (ST.fromString "0.3"))
                :| [ WD.WantDigestValue (ST.fromString "sha") (Just (ST.fromString "1"))
                   , WD.WantDigestValue (ST.fromString "sha-256") Nothing
                   ]
            )
    in M.toStrictByteString (WD.renderWantDigest v) `shouldBe` "md5;q=0.3, sha;q=1, sha-256"


-- ---------------------------------------------------------------------------
-- Content-MD5 (RFC 1864, obsolete)
-- ---------------------------------------------------------------------------

unit_contentMD5_parse :: Spec
unit_contentMD5_parse = it "parses a Content-MD5 base64 digest" $
  case toEither (runParser MD5.contentMD5Parser "Q2hlY2sgSW50ZWdyaXR5IQ==") of
    Right v -> MD5.contentMD5Digest v `shouldBe` BSC.pack "Check Integrity!"
    other -> error (show other)


unit_contentMD5_render :: Spec
unit_contentMD5_render =
  it "renders a Content-MD5 base64 digest" $
    let v = MD5.ContentMD5 (BSC.pack "Check Integrity!")
    in M.toStrictByteString (MD5.renderContentMD5 v) `shouldBe` "Q2hlY2sgSW50ZWdyaXR5IQ=="


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Digest" $
    sequence_
      [ unit_contentDigest_parse
      , unit_contentDigest_render
      , it "content-digest round-trip" prop_contentDigest
      , unit_reprDigest_parse
      , unit_reprDigest_render
      , it "repr-digest round-trip" prop_reprDigest
      , unit_wantContentDigest_parse
      , unit_wantContentDigest_render
      , it "want-content-digest round-trip" prop_wantContentDigest
      , unit_wantReprDigest_parse
      , unit_wantReprDigest_render
      , it "want-repr-digest round-trip" prop_wantReprDigest
      , unit_digest_parse
      , unit_digest_render
      , unit_wantDigest_parse
      , unit_wantDigest_render
      , unit_contentMD5_parse
      , unit_contentMD5_render
      ]
