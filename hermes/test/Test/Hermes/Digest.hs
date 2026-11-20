{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Digest (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, lookupRawHeader, setHeader)
import qualified Network.HTTP.Headers.ContentDigest as CD
import qualified Network.HTTP.Headers.ContentMD5 as MD5
import qualified Network.HTTP.Headers.Digest as DG
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), Result (..), runParser)
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


genDigestEntries :: (ST.ShortText -> Gen a) -> Gen (NonEmpty a)
genDigestEntries make = do
  x <- genKey >>= make . ("a-" <>)
  count <- Gen.int (Range.linear 0 3)
  xs <- traverse (\prefix -> genKey >>= make . (prefix <>)) (take count ["b-", "c-", "d-"])
  pure (x :| xs)


genContentDigest :: Gen CD.ContentDigest
genContentDigest = fmap CD.ContentDigest (genDigestEntries entry)
  where
    entry k = do
      v <- Gen.bytes (Range.linear 0 64)
      pure (CD.ContentDigestEntry k v [])


genReprDigest :: Gen RD.ReprDigest
genReprDigest = fmap RD.ReprDigest (genDigestEntries entry)
  where
    entry k = do
      v <- Gen.bytes (Range.linear 0 64)
      pure (RD.ReprDigestEntry k v [])


genWantContentDigest :: Gen WCD.WantContentDigest
genWantContentDigest = fmap WCD.WantContentDigest (genDigestEntries entry)
  where
    entry k = do
      p <- Gen.int (Range.linear 0 10)
      pure (WCD.WantContentDigestEntry k p [])


genWantReprDigest :: Gen WRD.WantReprDigest
genWantReprDigest = fmap WRD.WantReprDigest (genDigestEntries entry)
  where
    entry k = do
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


dictionaryCases :: Spec
dictionaryCases = describe "RFC 9530 dictionaries" $ do
  it "combines integrity field lines and keeps the last value at each key's first position" $ do
    let fields name =
          headerMapFromList
            [ (name, "sha-256=?0, extension=:AQ==:")
            , (name, "sha-256=:Ag:;sample=1;true=?1;sample=@3")
            ]
        parameters = [("sample", Just (DateValue 3)), ("true", Nothing)]
        cd = CD.ContentDigest (CD.ContentDigestEntry "sha-256" "\x02" parameters :| [CD.ContentDigestEntry "extension" "\x01" []])
        rd = RD.ReprDigest (RD.ReprDigestEntry "sha-256" "\x02" parameters :| [RD.ReprDigestEntry "extension" "\x01" []])
        render = "sha-256=:Ag==:;sample=@3;true, extension=:AQ==:"
    lookupHeader @CD.ContentDigest (fields "Content-Digest") `shouldBe` Right (Just cd)
    lookupHeader @RD.ReprDigest (fields "Repr-Digest") `shouldBe` Right (Just rd)
    headerMapToList (setHeader cd (headerMapFromList [])) `shouldBe` [("content-digest", render)]
    headerMapToList (setHeader rd (headerMapFromList [])) `shouldBe` [("repr-digest", render)]
  it "rejects a malformed member or the wrong final member type" $ do
    let malformed = ["sha-256=:AQ==:,", "sha-256=:AQ==:;\tx", "sha-256=:AQ==:;x=\"bad\\e\"", "sha-256=:AQ==:, sha-256=?0", "sha-256=(:AQ==:)"]
    mapM_
      ( \value -> do
          lookupHeader @CD.ContentDigest (headerMapFromList [("Content-Digest", value)]) `shouldSatisfy` isLeft
          lookupHeader @RD.ReprDigest (headerMapFromList [("Repr-Digest", value)]) `shouldSatisfy` isLeft
      )
      malformed
  it "combines integrity preference fields and enforces weights from zero through ten" $ do
    let fields name = headerMapFromList [(name, "sha-256=11, sha-512=10"), (name, "sha-256=0")]
    lookupHeader @WCD.WantContentDigest (fields "Want-Content-Digest")
      `shouldBe` Right (Just (WCD.WantContentDigest (WCD.WantContentDigestEntry "sha-256" 0 [] :| [WCD.WantContentDigestEntry "sha-512" 10 []])))
    lookupHeader @WRD.WantReprDigest (fields "Want-Repr-Digest")
      `shouldBe` Right (Just (WRD.WantReprDigest (WRD.WantReprDigestEntry "sha-256" 0 [] :| [WRD.WantReprDigestEntry "sha-512" 10 []])))
    mapM_
      ( \value -> do
          lookupHeader @WCD.WantContentDigest (headerMapFromList [("Want-Content-Digest", value)]) `shouldSatisfy` isLeft
          lookupHeader @WRD.WantReprDigest (headerMapFromList [("Want-Repr-Digest", value)]) `shouldSatisfy` isLeft
      )
      ["sha-256=-1", "sha-256=11", "sha-256=1.0", "sha-256", "sha-256=(1 2)"]
    isLeft (WCD.renderWantContentDigestChecked (WCD.WantContentDigest (WCD.WantContentDigestEntry "sha-256" 11 [] :| []))) `shouldBe` True
    isLeft (WRD.renderWantReprDigestChecked (WRD.WantReprDigest (WRD.WantReprDigestEntry "sha-256" (-1) [] :| []))) `shouldBe` True
  it "accepts received empty dictionaries and omits them when sending" $ do
    let blank name = headerMapFromList [(name, " ")]
    lookupHeader @CD.ContentDigest (blank "Content-Digest") `shouldBe` Right (Just CD.ContentDigestEmpty)
    lookupHeader @RD.ReprDigest (blank "Repr-Digest") `shouldBe` Right (Just RD.ReprDigestEmpty)
    lookupHeader @WCD.WantContentDigest (blank "Want-Content-Digest") `shouldBe` Right (Just WCD.WantContentDigestEmpty)
    lookupHeader @WRD.WantReprDigest (blank "Want-Repr-Digest") `shouldBe` Right (Just WRD.WantReprDigestEmpty)
  it "clears Content-Digest and Repr-Digest when given empty dictionaries" $ do
    let fields = headerMapFromList [("Content-Digest", "sha-256=:AAAA:"), ("Repr-Digest", "sha-256=:AAAA:")]
    headerMapToList (setHeader CD.ContentDigestEmpty fields) `shouldBe` [("repr-digest", "sha-256=:AAAA:")]
    headerMapToList (setHeader RD.ReprDigestEmpty fields) `shouldBe` [("content-digest", "sha-256=:AAAA:")]
  it "clears digest preferences only when explicitly empty" $ do
    let fields = headerMapFromList [("Want-Content-Digest", "sha-256=2"), ("Want-Repr-Digest", "sha-256=2")]
        invalidContent = WCD.WantContentDigest (WCD.WantContentDigestEntry "sha-256" 11 [] :| [])
        invalidRepr = WRD.WantReprDigest (WRD.WantReprDigestEntry "sha-256" (-1) [] :| [])
    setHeader invalidContent fields `shouldBe` fields
    setHeader invalidRepr fields `shouldBe` fields
    headerMapToList (setHeader WCD.WantContentDigestEmpty fields) `shouldBe` [("want-repr-digest", "sha-256=2")]
    headerMapToList (setHeader WRD.WantReprDigestEmpty fields) `shouldBe` [("want-content-digest", "sha-256=2")]
  it "uses dictionary merge semantics when combining and rendering constructed values" $ do
    let first = CD.ContentDigestEntry "example" "\x01" []
        second = CD.ContentDigestEntry "example" "\x02" [("p", Just (Integer 1)), ("p", Just (Boolean True))]
        other = CD.ContentDigestEntry "other" "" []
        merged = CD.ContentDigest (first :| [other]) <> CD.ContentDigest (second :| [])
    CD.contentDigestEntryList merged `shouldBe` [second, other]
    M.toStrictByteString (CD.renderContentDigest (CD.ContentDigest (first :| [other, second])))
      `shouldBe` "example=:Ag==:;p, other=::"


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


legacyDigestFields :: Spec
legacyDigestFields = describe "legacy digest fields through typed lookup and setting" $ do
  it "represents an explicitly empty field and treats it as the identity when appending" $ do
    let emptyFields = headerMapFromList [("Digest", ",,\t"), ("Digest", ""), ("Want-Digest", ", \t,"), ("Want-Digest", "")]
        digest = DG.Digest (DG.DigestValue "unixsum" "17" :| [])
        wanted = WD.WantDigest (WD.WantDigestValue "unixsum" Nothing :| [])
    lookupHeader @DG.Digest emptyFields `shouldBe` Right (Just DG.DigestEmpty)
    lookupHeader @WD.WantDigest emptyFields `shouldBe` Right (Just WD.WantDigestEmpty)
    lookupRawHeader "Digest" (setHeader DG.DigestEmpty emptyFields) `shouldBe` Just ("" :| [])
    lookupRawHeader "Want-Digest" (setHeader WD.WantDigestEmpty emptyFields) `shouldBe` Just ("" :| [])
    DG.digestValueList DG.DigestEmpty `shouldBe` []
    WD.wantDigestValueList WD.WantDigestEmpty `shouldBe` []
    DG.DigestEmpty <> digest `shouldBe` digest
    wanted <> WD.WantDigestEmpty `shouldBe` wanted
  it "combines Digest lines and retains both registered and quoted extension spellings" $ do
    let fields =
          headerMapFromList
            [ ("Digest", " , MD5 = HUXZLQLMuI/KZ5KDcJPcOA==,\t"),
              ("Digest", ","),
              ("Digest", "crc32c=A72A4DF, ,Adler32=03da0195, unixsum=65535, UNIXcksum=4294967295"),
              ("Digest", "ext=\"a\\;,\xff\""),
              ("Digest", "ext2=:a=b?")
            ]
        wire = "MD5=HUXZLQLMuI/KZ5KDcJPcOA==, crc32c=A72A4DF, Adler32=03da0195, unixsum=65535, UNIXcksum=4294967295, ext=\"a\\;,\xff\", ext2=:a=b?"
    case lookupHeader @DG.Digest fields of
      Right (Just value) -> do
        M.toStrictByteString (DG.renderDigest value) `shouldBe` wire
        lookupRawHeader "Digest" (setHeader value fields) `shouldBe` Just (wire :| [])
        lookupHeader @DG.Digest (setHeader value fields) `shouldBe` Right (Just value)
      other -> expectationFailure (show other)
  it "accepts the RFC 3230 SHA example and validates width, padding, and algorithm-specific syntax" $ do
    lookupHeader @DG.Digest (headerMapFromList [("Digest", "sha=thvDyvhfIqlvFe+A9MYgxAfm1q5=")])
      `shouldBe` Right (Just (DG.Digest (DG.DigestValue "sha" "thvDyvhfIqlvFe+A9MYgxAfm1q5=" :| [])))
    let sha512 = BS.replicate 86 65 <> "=="
    lookupHeader @DG.Digest (headerMapFromList [("Digest", "sHa-512=" <> sha512)])
      `shouldBe` Right (Just (DG.Digest (DG.DigestValue "sHa-512" (ST.fromString (replicate 86 'A' <> "==")) :| [])))
    forM_
      [ "MD5=AA==", "md5=HUXZLQLMuI_KZ5KDcJPcOA==", "md5=HUXZLQLMuI/KZ5KDcJPcOA=", "sha-256=AA==",
        "sha-512=AA==", "CRC32c=123456789", "ADLER32=GG", "UNIXsum=65536", "UNIXcksum=4294967296",
        "UNIXsum=-1", "UNIXcksum=12x", "contentMD5=HUXZLQLMuI/KZ5KDcJPcOA==",
        "ext=", "ext=foo;bar", "ext=\"unterminated", "ext=\"a\"tail", "ext=\"a\n\""
      ] $ \wire ->
        lookupHeader @DG.Digest (headerMapFromList [("Digest", "CRC32c=1"), ("Digest", wire)]) `shouldSatisfy` isLeft
  it "combines Want-Digest lines and preserves valid qvalues exactly" $ do
    let value = WD.WantDigest (WD.WantDigestValue "SHA" (Just "0.030") :| [WD.WantDigestValue "contentMD5" Nothing, WD.WantDigestValue "ext" (Just "1.")])
        fields = headerMapFromList [("Want-Digest", ", SHA \t; Q = 0.030 ,, "), ("Want-Digest", ""), ("Want-Digest", "contentMD5, ext;q=1.")]
    lookupHeader @WD.WantDigest fields `shouldBe` Right (Just value)
    lookupRawHeader "Want-Digest" (setHeader value fields) `shouldBe` Just ("SHA;q=0.030, contentMD5, ext;q=1." :| [])
    fmap WD.wantDigestQuality (WD.wantDigestValueList value) `shouldBe` [Just 0.03, Just 1, Just 1]
    WD.wantDigestQuality (WD.WantDigestValue "ext" (Just "0")) `shouldBe` Just 0
    WD.wantDigestQuality (WD.WantDigestValue "ext" (Just "1.001")) `shouldBe` Nothing
    forM_ ["", ".2", "-1", "2", "00", "0..1", "1.001", "1.0000", "0.0000", "0.0junk", "\"1\"", "1;q=0"] $ \q ->
      lookupHeader @WD.WantDigest (headerMapFromList [("Want-Digest", "sha"), ("Want-Digest", "md5;q=" <> q)])
        `shouldSatisfy` isLeft


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


typedContentMD5 :: Spec
typedContentMD5 = describe "typed Content-MD5" $ do
  it "reads the RFC 1864 digest with HTTP field whitespace and emits canonical Base64" $ do
    let wire = "Q2hlY2sgSW50ZWdyaXR5IQ=="
        value = MD5.ContentMD5 "Check Integrity!"
    MD5.mkContentMD5 "Check Integrity!" `shouldBe` Just value
    MD5.mkContentMD5 "short" `shouldBe` Nothing
    lookupHeader @MD5.ContentMD5 (headerMapFromList [("Content-MD5", "\t" <> wire <> "\t ")])
      `shouldBe` Right (Just value)
    lookupRawHeader "Content-MD5" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
  it "rejects duplicate fields, wrong widths, bad alphabets and noncanonical pad bits" $ do
    forM_ ["", "YWJj", "AAAAAAAAAAAAAAAAAAAAAA", "AAAAAAAAAAAAAAAAAAAAAA= ", "AAAAAAAAAAAAAAAAAAAAAAAA", "Q2hlY2sgSW50ZWdyaXR5IR==", "_2hlY2sgSW50ZWdyaXR5IQ=="] $ \wire ->
      lookupHeader @MD5.ContentMD5 (headerMapFromList [("Content-MD5", wire)]) `shouldSatisfy` isLeft
    let wire = "Q2hlY2sgSW50ZWdyaXR5IQ=="
    lookupHeader @MD5.ContentMD5 (headerMapFromList [("Content-MD5", wire), ("Content-MD5", wire)]) `shouldSatisfy` isLeft
  it "round-trips an arbitrary sixteen-byte digest through typed lookup" $ property $ do
    bytes <- forAll (Gen.bytes (Range.singleton 16))
    let value = MD5.ContentMD5 bytes
    lookupHeader @MD5.ContentMD5 (setHeader value (headerMapFromList [])) === Right (Just value)


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
      , dictionaryCases
      , unit_digest_parse
      , unit_digest_render
      , unit_wantDigest_parse
      , unit_wantDigest_render
      , legacyDigestFields
      , unit_contentMD5_parse
      , unit_contentMD5_render
      , typedContentMD5
      ]
