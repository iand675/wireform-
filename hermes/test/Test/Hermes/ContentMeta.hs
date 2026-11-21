{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ContentMeta (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.ContentCoding (ContentCoding (..))
import Network.HTTP.ContentNegotiation (MediaType (..))
import Network.HTTP.Grammar.ExtendedValue (ExtendedValue (..), decodeExtendedValue, decodeExtendedValueText, parseExtendedValue, renderExtendedValue)
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import Network.HTTP.Headers.ContentDisposition (ContentDisposition (..), ContentDispositionFilenameError (..), contentDispositionFilename, mkContentDispositionFilename)
import Network.HTTP.Headers.ContentEncoding (ContentEncoding (..), contentCodings)
import qualified Network.HTTP.Headers.ContentLanguage as CL
import qualified Network.HTTP.Headers.ContentLocation as CLoc
import Network.HTTP.Headers.ContentType (ContentType (..), contentTypeParameters)
import qualified Network.HTTP.Headers.MIMEVersion as MV
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Network.HTTP.Headers.SetCookie (SetCookies (..))
import Test.Syd
import Test.Syd.Hedgehog ()


dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseCL :: ByteString -> Either String CL.ContentLanguage
parseCL bs = case runParser CL.contentLanguageParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseCLoc :: ByteString -> Either String CLoc.ContentLocation
parseCLoc bs = case runParser CLoc.contentLocationParser bs of
  OK v "" -> Right v
  OK _ leftover -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseMV :: ByteString -> Either String MV.MIMEVersion
parseMV bs = case runParser MV.mimeVersionParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCL :: CL.ContentLanguage -> ByteString
renderCL = M.toStrictByteString . CL.renderContentLanguage


renderCLoc :: CLoc.ContentLocation -> ByteString
renderCLoc = M.toStrictByteString . CLoc.renderContentLocation


renderMV :: MV.MIMEVersion -> ByteString
renderMV = M.toStrictByteString . MV.renderMIMEVersion


-- Content-Language ----------------------------------------------------------

unit_cl_parse :: Spec
unit_cl_parse = it "parses a language-tag list" $
  case parseCL "en-US, fr" of
    Right (CL.ContentLanguage (a :| [b])) -> do
      CL.unLanguageTag a `shouldBe` ST.fromString "en-US"
      CL.unLanguageTag b `shouldBe` ST.fromString "fr"
    other -> error (show other)


unit_cl_render :: Spec
unit_cl_render =
  it "renders a language-tag list" $
    let v =
          CL.ContentLanguage
            (CL.LanguageTag (ST.fromString "de") :| [CL.LanguageTag (ST.fromString "en")])
    in renderCL v `shouldBe` "de, en"


genTag :: Gen CL.LanguageTag
genTag = do
  primary <- ST.pack <$> Gen.list (Range.constant 2 8) (Gen.element ['a' .. 'z'])
  private <- Gen.list (Range.constant 0 3) (ST.pack <$> Gen.list (Range.constant 1 8) (Gen.element ['a' .. 'z']))
  pure (CL.LanguageTag (if null private then primary else primary <> "-x" <> foldMap ("-" <>) private))


prop_cl_roundtrip :: Property
prop_cl_roundtrip = property $ do
  tags <- forAll (Gen.nonEmpty (Range.constant 1 4) genTag)
  let v = CL.ContentLanguage tags
      bs = renderCL v
  case parseCL bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


unit_cl_fields :: Spec
unit_cl_fields = it "checks BCP 47 tags across sparse and repeated fields" $ do
  let wire = "zh-cmn-Hans-CN, es-419, i-klingon, X-pvt, en-u-ca-gregory"
      fields = headerMapFromList [("Content-Language", "zh-cmn-Hans-CN,, es-419"), ("content-language", "i-klingon, X-pvt, en-u-ca-gregory")]
      tags = fmap CL.LanguageTag ["zh-cmn-Hans-CN", "es-419", "i-klingon", "X-pvt", "en-u-ca-gregory"]
  case lookupHeader @CL.ContentLanguage fields of
    Right (Just value) -> do
      CL.contentLanguageTagList value `shouldBe` tags
      renderCL value `shouldBe` wire
    other -> expectationFailure (show other)
  lookupHeader @CL.ContentLanguage (headerMapFromList [("content-language", " , ")])
    `shouldBe` Right (Just CL.ContentLanguageEmpty)
  lookupHeader @CL.ContentLanguage (headerMapFromList []) `shouldBe` Right Nothing
  let invalid = ["*", "en--US", "en_US", "en-x", "x", "en-u", "en-a-foo-A-bar", "de-1901-1901", "en-US-Latn", "abcdefghx"]
  map (isLeft . parseCL) invalid `shouldBe` replicate (length invalid) True


unit_cl_invalid_write :: Spec
unit_cl_invalid_write = it "rejects malformed authored Content-Language tags" $ do
  let original = headerMapFromList [("Content-Language", "en-US"), ("Content-Language", "es-419")]
  forM_ ["", "*", "en--US", "en_US", "fr, de", "en-u", "de-1901-1901", "en-a-foo-A-bar"] $ \tag -> do
    let value = CL.ContentLanguage (CL.LanguageTag "en-US" :| [CL.LanguageTag tag])
    renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
    setHeader value original `shouldBe` original
  renderHeaderLines defaultHeaderSettings CL.ContentLanguageEmpty `shouldBe` Just ("" :| [])
  let valid = CL.ContentLanguage (CL.LanguageTag "i-klingon" :| [CL.LanguageTag "x-pvt", CL.LanguageTag "en-u-ca-gregory"])
  renderHeaderLines defaultHeaderSettings valid `shouldBe` Just ("i-klingon, x-pvt, en-u-ca-gregory" :| [])


-- Content-Location ----------------------------------------------------------

unit_cloc_parse :: Spec
unit_cloc_parse = it "parses a fragmentless URI reference" $
  case parseCLoc "/index.html" of
    Right (CLoc.ContentLocation u) -> u `shouldBe` ST.fromString "/index.html"
    other -> error (show other)


unit_cloc_render :: Spec
unit_cloc_render =
  it "renders an opaque URI reference" $
    renderCLoc (CLoc.ContentLocation (ST.fromString "https://example.com/a"))
      `shouldBe` "https://example.com/a"


-- MIME-Version --------------------------------------------------------------

unit_mv_parse :: Spec
unit_mv_parse = it "parses major.minor" $
  case parseMV "1.0" of
    Right (MV.MIMEVersion 1 0) -> pure () :: IO ()
    other -> error (show other)


unit_mv_render :: Spec
unit_mv_render =
  it "renders major.minor" $
    renderMV (MV.MIMEVersion 1 0) `shouldBe` "1.0"


typedMIMEVersion :: Spec
typedMIMEVersion = describe "typed MIME-Version" $ do
  it "ignores nested comments and quoted pairs around the version tokens" $ do
    let wire = " \t(produced (by MIME)) 01 (major) . (minor \\))000 (end)"
        expected = MV.MIMEVersion 1 0
    parseMV wire `shouldBe` Right expected
    lookupHeader @MV.MIMEVersion (headerMapFromList [("MIME-Version", wire)]) `shouldBe` Right (Just expected)
    lookupRawHeader "MIME-Version" (setHeader expected (headerMapFromList [])) `shouldBe` Just ("1.0" :| [])
  it "checks component overflow, rejects malformed tokens and enforces a single field line" $ do
    let limit = BSC.pack (show (maxBound :: Int))
        tooBig = BSC.pack (show (toInteger (maxBound :: Int) + 1))
    parseMV (limit <> "." <> limit) `shouldBe` Right (MV.MIMEVersion maxBound maxBound)
    forM_ ["", "1.", ".0", "-1.0", "1.0.3", "1 (x) 2.0", "1.0 (unterminated", "1.0 (x\r)", tooBig <> ".0", "1." <> tooBig] $ \wire -> do
      parseMV wire `shouldSatisfy` isLeft
      lookupHeader @MV.MIMEVersion (headerMapFromList [("MIME-Version", wire)]) `shouldSatisfy` isLeft
    lookupHeader @MV.MIMEVersion (headerMapFromList [("MIME-Version", "1.0"), ("MIME-Version", "2.0")])
      `shouldSatisfy` isLeft
  it "rejects malformed authored MIME-Version components" $ do
    let original = headerMapFromList [("MIME-Version", "1.0"), ("MIME-Version", "2.0")]
    forM_ [MV.MIMEVersion (-1) 1, MV.MIMEVersion 1 minBound] $ \value -> do
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original


prop_mv_roundtrip :: Property
prop_mv_roundtrip = property $ do
  major <- forAll (Gen.int (Range.constant 0 99))
  minor <- forAll (Gen.int (Range.constant 0 99))
  let v = MV.MIMEVersion major minor
      bs = renderMV v
  case parseMV bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "ContentMeta" $
    sequence_
      [ unit_cl_parse
      , unit_cl_render
      , unit_cl_fields
      , unit_cl_invalid_write
      , it "Content-Language round-trip" prop_cl_roundtrip
      , unit_cloc_parse
      , unit_cloc_render
      , unit_mv_parse
      , unit_mv_render
      , it "MIME-Version round-trip" prop_mv_roundtrip
      , typedMIMEVersion
      , representationHeaders
      ]


representationHeaders :: Spec
representationHeaders = do
  it "sets repeated typed fields in order and handles a renderer that emits no fields" $ do
    let fields = headerMapFromList [("Set-Cookie", "a=1"), ("Set-Cookie", "b=2")]
    case lookupHeader @SetCookies fields of
      Right (Just cookies) -> do
        headerMapToList (setHeader cookies (headerMapFromList [("Set-Cookie", "old=1")])) `shouldBe` headerMapToList fields
        setHeader (SetCookies []) fields `shouldBe` fields
      other -> expectationFailure (show other)
  describe "Content-Type" $ do
    it "preserves the case and delimiters in a quoted multipart boundary" $ do
      let wire = "multipart/form-data;boundary=\"Aa:B, 01\""
          fields = headerMapFromList [("Content-Type", " \t" <> wire <> "\t")]
          value = ParameterizedContentType (MediaType "multipart" "form-data") (("boundary", "Aa:B, 01") :| [])
      lookupHeader @ContentType fields `shouldBe` Right (Just value)
      contentTypeParameters value `shouldBe` [("boundary", "Aa:B, 01")]
      headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("content-type", wire)]
    it "accepts a bare media type and preserves the original constructor" $
      lookupHeader @ContentType (headerMapFromList [("content-type", "application/json")])
        `shouldBe` Right (Just (ContentType (MediaType "application" "json")))
    it "rejects invalid authored Content-Type names and parameter values" $ do
      let previous = headerMapFromList [("Content-Type", "text/plain"), ("Content-Type", "text/html")]
          valid = MediaType "text" "plain"
      forM_
        [ ContentType (MediaType "text" "")
        , ContentType (MediaType "" "")
        , ContentType (MediaType "text" "plain,html")
        , ParameterizedContentType valid (("bad name", "value") :| [])
        , ParameterizedContentType valid (("title", "bad\DEL") :| [])
        ]
        $ \value -> do
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value previous `shouldBe` previous
    it "rejects wildcards, malformed parameters and repeated singleton fields" $ do
      let parse = lookupHeader @ContentType . headerMapFromList
      map (isLeft . parse) [[("content-type", "text/*")], [("content-type", "*/json")], [("content-type", "application/*+json")], [("content-type", "text/html;broken=")], [("content-type", "text/html"), ("content-type", "application/json")]]
        `shouldBe` replicate 5 True
    it "round-trips opaque bytes in quoted parameters" $ do
      let wire = "application/example;parameter=\"" <> BS.pack [0x80, 0x5c, 0xff, 0x22]
          canonical = "application/example;parameter=\"" <> BS.pack [0x80, 0xff, 0x22]
          value = ParameterizedContentType (MediaType "application" "example") (("parameter", ST.pack ['\x80', '\xff']) :| [])
      lookupHeader @ContentType (headerMapFromList [("content-type", wire)]) `shouldBe` Right (Just value)
      headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("content-type", canonical)]
    it "round-trips empty, token and quoted parameter values" $
      property $ do
        parameter <- forAll (ST.pack <$> Gen.list (Range.linear 0 60) (Gen.element ("abcdABCD0123;,:\"\\ \t" :: String)))
        let value = ParameterizedContentType (MediaType "application" "custom") (("param", parameter) :| [])
        lookupHeader @ContentType (setHeader value (headerMapFromList [])) === Right (Just value)
  describe "Content-Encoding" $ do
    it "rejects invalid authored Content-Encoding names across the chain" $ do
      let previous = headerMapFromList [("Content-Encoding", "gzip"), ("Content-Encoding", "br")]
      forM_ [ContentEncoding (Custom ""), ContentEncodingChain GZip (Brotli :| [Custom "br,zstd"])] $ \value -> do
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value previous `shouldBe` previous
    it "round-trips a chain with repeated fields, aliases and custom names" $ do
      let fields = headerMapFromList [("Content-Encoding", " X-GZIP, , Br"), ("content-encoding", "gzip-extra, Deflate ")]
          value = ContentEncodingChain GZip (Brotli :| [Custom "gzip-extra", Deflate])
          serialized = setHeader value (headerMapFromList [])
      lookupHeader @ContentEncoding fields `shouldBe` Right (Just value)
      contentCodings value `shouldBe` [GZip, Brotli, Custom "gzip-extra", Deflate]
      headerMapToList serialized `shouldBe` [("content-encoding", "gzip, br, gzip-extra, deflate")]
      lookupHeader @ContentEncoding serialized `shouldBe` Right (Just value)
    it "round-trips a present empty list and distinguishes it from an absent field" $ do
      let empty = headerMapFromList []
          sparse = headerMapFromList [("content-encoding", " \t, "), ("Content-Encoding", ""), ("Content-Encoding", ",\t")]
          serialized = setHeader ContentEncodingEmpty empty
      lookupHeader @ContentEncoding empty `shouldBe` Right Nothing
      lookupHeader @ContentEncoding sparse `shouldBe` Right (Just ContentEncodingEmpty)
      headerMapToList serialized `shouldBe` [("content-encoding", "")]
      lookupHeader @ContentEncoding serialized `shouldBe` Right (Just ContentEncodingEmpty)
      contentCodings ContentEncodingEmpty `shouldBe` []
    it "rejects invalid entries in later lines instead of decoding part of the chain" $
      isLeft (lookupHeader @ContentEncoding (headerMapFromList [("content-encoding", "gzip"), ("content-encoding", "br;q=1")])) `shouldBe` True
    it "preserves case for unknown codings and recognizes known whole tokens case insensitively" $ do
      lookupHeader @ContentEncoding (headerMapFromList [("content-encoding", "GZIP-Extension")]) `shouldBe` Right (Just (ContentEncoding (Custom "GZIP-Extension")))
      lookupHeader @ContentEncoding (headerMapFromList [("content-encoding", "ZsTd")]) `shouldBe` Right (Just (ContentEncoding ZStd))
  describe "Content-Disposition" $ do
    it "validates authored parameter syntax and duplicate names atomically" $ do
      let existing = headerMapFromList [("Content-Disposition", "inline")]
          invalid =
            [ ContentDisposition "bad type" []
            , ContentDisposition "attachment" [("bad=name", "a")]
            , ContentDisposition "attachment" [("filename", "a\nb")]
            , ContentDisposition "attachment" [("*", "UTF-8''a")]
            , ContentDisposition "attachment" [("filename*", "UTF-8''a%XX")]
            , ContentDisposition "attachment" [("note*", "UTF-8'en--GB'file")]
            , ContentDisposition "attachment" [("filename", "a"), ("meta", "b"), ("FILENAME", "c")]
            ]
      forM_ invalid $ \value -> do
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value existing `shouldBe` existing
    it "authors plain and UTF-8 filenames through typed setHeader" $ do
      let authored dtype name fallback = do
            value <- mkContentDispositionFilename dtype name fallback
            let fields = setHeader value (headerMapFromList [])
            pure (headerMapToList fields, fmap (>>= contentDispositionFilename) (lookupHeader @ContentDisposition fields))
      authored "attachment" "report; \"draft\".txt" Nothing
        `shouldBe` Right ([("content-disposition", "attachment;filename=\"report; \\\"draft\\\".txt\"")], Right (Just "report; \"draft\".txt"))
      authored "inline" "résumé 科学.txt" (Just "resume.txt")
        `shouldBe` Right ([("content-disposition", "inline;filename=resume.txt;filename*=UTF-8''r%C3%A9sum%C3%A9%20%E7%A7%91%E5%AD%A6.txt")], Right (Just "résumé 科学.txt"))
      authored "attachment" "100%.txt" Nothing
        `shouldBe` Right ([("content-disposition", "attachment;filename=100%.txt")], Right (Just "100%.txt"))
      authored "attachment" "note%20draft.txt" Nothing
        `shouldBe` Right ([("content-disposition", "attachment;filename*=UTF-8''note%2520draft.txt")], Right (Just "note%20draft.txt"))
      authored "attachment" "one\\two.txt" Nothing
        `shouldBe` Right ([("content-disposition", "attachment;filename*=UTF-8''one%5Ctwo.txt")], Right (Just "one\\two.txt"))
    it "rejects invalid disposition tokens and unsuitable fallback text" $ do
      forM_ ["", "bad token", "inline;name", "año"] $ \dtype ->
        mkContentDispositionFilename dtype "file.txt" Nothing `shouldBe` Left InvalidDispositionType
      forM_ ["new\nline", "new\rline", "tab\tname", "name\x7f", "name\x85"] $ \filename ->
        mkContentDispositionFilename "attachment" filename Nothing `shouldBe` Left InvalidFilenameCharacters
      forM_ ["caf\xe9.txt", "note%20draft", "note\\draft", "tab\tname"] $ \fallback ->
        mkContentDispositionFilename "attachment" "résumé.txt" (Just fallback) `shouldBe` Left InvalidFilenameFallback
    it "round-trips authored advisory filename text" $
      property $ do
        name <- forAll (T.pack <$> Gen.list (Range.linear 0 45) (Gen.element ("fileA1 %2a;\"\\éド📄\x0301" :: String)))
        fallback <- forAll (Gen.element [Nothing, Just "file.txt"])
        case mkContentDispositionFilename "attachment" name fallback of
          Left failure -> error (show failure)
          Right value ->
            fmap (>>= contentDispositionFilename) (lookupHeader @ContentDisposition (setHeader value (headerMapFromList [])))
              === Right (Just name)
    it "round-trips an escaped filename and keeps filename* on the wire" $ do
      let value = ContentDisposition "attachment" [("filename", "one, two\".txt"), ("filename*", "UTF-8''caf%C3%A9.txt")]
          fields = setHeader value (headerMapFromList [])
      headerMapToList fields `shouldBe` [("content-disposition", "attachment;filename=\"one, two\\\".txt\";filename*=UTF-8''caf%C3%A9.txt")]
      lookupHeader @ContentDisposition fields `shouldBe` Right (Just value)
      lookupHeader @ContentDisposition (headerMapFromList [("content-disposition", "attachment;filename = \"one, two\\\".txt\";filename*= UTF-8''caf%C3%A9.txt")])
        `shouldBe` Right (Just value)
    it "prefers a decodable extended filename and preserves its original spelling" $ do
      let wire = "attachment;FILENAME=plain.txt;Filename*=uTf-8'en-GB'%e2%82%AC+rates.txt"
          fields = headerMapFromList [("content-disposition", wire)]
      case lookupHeader @ContentDisposition fields of
        Right (Just value) -> do
          contentDispositionFilename value `shouldBe` Just "\x20ac+rates.txt"
          headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("content-disposition", wire)]
        other -> expectationFailure (show other)
      let filename wireValue =
            fmap (>>= contentDispositionFilename) (lookupHeader @ContentDisposition (headerMapFromList [("content-disposition", wireValue)]))
      filename "attachment; filename*=ISO-8859-1''caf%E9.txt" `shouldBe` Right (Just "caf\xe9.txt")
      filename "attachment; filename*=UTF-8''%ff; filename=plain%20.txt" `shouldBe` Right (Just "plain%20.txt")
      filename "attachment; filename*=x-unknown''bytes; filename=plain.txt" `shouldBe` Right (Just "plain.txt")
    it "rejects duplicate names and malformed extended syntax" $ do
      let parse wire = lookupHeader @ContentDisposition (headerMapFromList [("content-disposition", wire)])
          invalid =
            [ "attachment;filename=one; FILENAME=two"
            , "attachment;name*=UTF-8''one; NAME*=UTF-8''two"
            , "attachment;filename=one; size=2; FILENAME=two"
            , "attachment;name*=UTF-8''one; size=2; NAME*=UTF-8''two"
            , "attachment;filename*=UTF-8''a%ZZ"
            , "attachment;filename*=UTF-8''a%A"
            , "attachment;filename*=\"UTF-8''report.txt\""
            , "attachment;filename*=UTF-8'en--GB'file"
            , "attachment;filename*=UTF-8''a*b"
            , "attachment;filename*=''name.txt"
            ]
      map (isLeft . parse) invalid `shouldBe` replicate (length invalid) True
      parse "attachment;x^=one;X~=two"
        `shouldBe` Right (Just (ContentDisposition "attachment" [("x^", "one"), ("X~", "two")]))
      parse "attachment;x^=one;size=2;X~=two"
        `shouldBe` Right (Just (ContentDisposition "attachment" [("x^", "one"), ("size", "2"), ("X~", "two")]))
    it "validates extended filenames supplied through the record constructor" $ do
      let filename spelling = contentDispositionFilename (ContentDisposition "attachment" [("filename", "plain.txt"), ("filename*", spelling)])
      filename "iSo-8859-1'fr-FR'caf%E9+.txt" `shouldBe` Just "caf\xe9+.txt"
      filename "UTF-8''" `shouldBe` Just ""
      forM_ ["UTF-8''a%XZ", "UTF-8'en--GB'file", "UTF-8''file txt", "UTF-8x''file"] $ \spelling ->
        filename spelling `shouldBe` Just "plain.txt"
    it "preserves opaque regular filename bytes" $ do
      let wire = "attachment;filename=\"caf" <> BS.pack [0xe9] <> ".txt\""
          fields = headerMapFromList [("content-disposition", wire)]
      case lookupHeader @ContentDisposition fields of
        Right (Just value) -> do
          contentDispositionFilename value `shouldBe` Just "caf\xe9.txt"
          headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("content-disposition", wire)]
        other -> expectationFailure (show other)
    it "decodes and renders an extended parameter with its language and octets" $ do
      let wire = "UTF-8'en-GB'%e2%82%ac%20a+b%27%2a"
          octets = BS.pack [0xe2, 0x82, 0xac] <> " a+b'*"
          value = ExtendedValue "UTF-8" (Just "en-GB") octets
      parseExtendedValue wire `shouldBe` Right value
      decodeExtendedValue value `shouldBe` Just "\x20ac a+b'*"
      decodeExtendedValueText wire `shouldBe` decodeExtendedValue value
      M.toStrictByteString (renderExtendedValue value) `shouldBe` "UTF-8'en-GB'%E2%82%AC%20a+b%27%2A"
    it "renders all extended parameter octets with canonical percent escapes" $ do
      let octets = BS.concat (replicate 18 (BS.pack [0 .. 255]))
          value = ExtendedValue "UTF-8" Nothing octets
          wire = M.toStrictByteString (renderExtendedValue value)
          attr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!#$&+-.^_" <> BS.pack [96] <> "|~"
          hex = BS.index "0123456789ABCDEF" . fromIntegral
          encode byte
            | BS.elem byte attr = BS.singleton byte
            | otherwise = let (hi, lo) = quotRem byte 16 in BS.pack [37, hex hi, hex lo]
      wire `shouldBe` ("UTF-8''" <> BS.concatMap encode octets)
      parseExtendedValue wire `shouldBe` Right value
      let latin1 = value {extendedCharset = "Iso-8859-1"}
      decodeExtendedValueText (M.toStrictByteString (renderExtendedValue latin1)) `shouldBe` decodeExtendedValue latin1
