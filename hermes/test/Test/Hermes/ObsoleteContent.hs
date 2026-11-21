{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteContent (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Word (Word8)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.ContentNegotiation (MediaType (..))
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.ContentBase as CB
import qualified Network.HTTP.Headers.ContentID as CID
import qualified Network.HTTP.Headers.ContentScriptType as CST
import qualified Network.HTTP.Headers.ContentStyleType as CStyT
import qualified Network.HTTP.Headers.ContentType as CT
import qualified Network.HTTP.Headers.ContentVersion as CV
import qualified Network.HTTP.Headers.DefaultStyle as DS
import qualified Network.HTTP.Headers.DerivedFrom as DF
import qualified Network.HTTP.Headers.DifferentialID as DID
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.URI as U
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser and require the whole value to be consumed.
runOk :: ParserT st String a -> ByteString -> Either String a
runOk p bs = case runParser p bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


-- ---------------------------------------------------------------------------
-- Content-Base
-- ---------------------------------------------------------------------------

unit_content_base_parse :: Spec
unit_content_base_parse = it "parses Content-Base absolute URI" $
  case runOk CB.contentBaseParser "http://example.com/base/" of
    Right (CB.ContentBase u) -> u `shouldBe` ST.fromString "http://example.com/base/"
    other -> error (show other)


unit_content_base_render :: Spec
unit_content_base_render =
  it "renders Content-Base" $
    M.toStrictByteString (CB.renderContentBase (CB.ContentBase (ST.fromString "http://example.com/base/")))
      `shouldBe` "http://example.com/base/"


-- ---------------------------------------------------------------------------
-- Content-Script-Type
-- ---------------------------------------------------------------------------

unit_content_script_type_parse :: Spec
unit_content_script_type_parse = it "parses Content-Script-Type media type" $
  case runOk CST.contentScriptTypeParser "text/javascript" of
    Right (CST.ContentScriptType t) -> t `shouldBe` ST.fromString "text/javascript"
    other -> error (show other)


unit_content_script_type_render :: Spec
unit_content_script_type_render =
  it "renders Content-Script-Type" $
    M.toStrictByteString (CST.renderContentScriptType (CST.ContentScriptType (ST.fromString "text/tcl")))
      `shouldBe` "text/tcl"


-- ---------------------------------------------------------------------------
-- Content-Style-Type
-- ---------------------------------------------------------------------------

unit_content_style_type_parse :: Spec
unit_content_style_type_parse = it "parses Content-Style-Type media type" $
  case runOk CStyT.contentStyleTypeParser "text/css" of
    Right (CStyT.ContentStyleType t) -> t `shouldBe` ST.fromString "text/css"
    other -> error (show other)


unit_content_style_type_render :: Spec
unit_content_style_type_render =
  it "renders Content-Style-Type" $
    M.toStrictByteString (CStyT.renderContentStyleType (CStyT.ContentStyleType (ST.fromString "text/css")))
      `shouldBe` "text/css"


-- ---------------------------------------------------------------------------
-- Content-Version
-- ---------------------------------------------------------------------------

unit_content_version_parse :: Spec
unit_content_version_parse = it "parses Content-Version quoted-string" $
  case runOk CV.contentVersionParser "\"Fred 19950116-12:26:48\"" of
    Right (CV.ContentVersion t) -> t `shouldBe` ST.fromString "Fred 19950116-12:26:48"
    other -> error (show other)


unit_content_version_render :: Spec
unit_content_version_render =
  it "renders Content-Version as quoted-string" $
    M.toStrictByteString (CV.renderContentVersion (CV.ContentVersion (ST.fromString "2.1.2")))
      `shouldBe` "\"2.1.2\""


-- ---------------------------------------------------------------------------
-- Default-Style
-- ---------------------------------------------------------------------------

unit_default_style_parse :: Spec
unit_default_style_parse = it "parses Default-Style title" $
  case runOk DS.defaultStyleParser "compact" of
    Right (DS.DefaultStyle t) -> t `shouldBe` ST.fromString "compact"
    other -> error (show other)


unit_default_style_render :: Spec
unit_default_style_render =
  it "renders Default-Style as a quoted title" $
    M.toStrictByteString (DS.renderDefaultStyle (DS.DefaultStyle (ST.fromString "compact")))
      `shouldBe` "\"compact\""


-- ---------------------------------------------------------------------------
-- Derived-From
-- ---------------------------------------------------------------------------

unit_derived_from_parse :: Spec
unit_derived_from_parse = it "parses Derived-From quoted-string" $
  case runOk DF.derivedFromParser "\"2.1.1\"" of
    Right (DF.DerivedFrom t) -> t `shouldBe` ST.fromString "2.1.1"
    other -> error (show other)


unit_derived_from_render :: Spec
unit_derived_from_render =
  it "renders Derived-From as quoted-string" $
    M.toStrictByteString (DF.renderDerivedFrom (DF.DerivedFrom (ST.fromString "2.1.1")))
      `shouldBe` "\"2.1.1\""


-- ---------------------------------------------------------------------------
-- Content-ID
-- ---------------------------------------------------------------------------

unit_content_id_parse :: Spec
unit_content_id_parse = it "parses Content-ID checksum URN" $
  case runOk CID.contentIDParser "urn:md5:PEFjWBDv/sd9alS9BYuX0w==" of
    Right (CID.ContentID t) -> t `shouldBe` ST.fromString "urn:md5:PEFjWBDv/sd9alS9BYuX0w=="
    other -> error (show other)


unit_content_id_render :: Spec
unit_content_id_render =
  it "renders Content-ID" $
    M.toStrictByteString (CID.renderContentID (CID.ContentID (ST.fromString "urn:md5:PEFjWBDv/sd9alS9BYuX0w==")))
      `shouldBe` "urn:md5:PEFjWBDv/sd9alS9BYuX0w=="


-- ---------------------------------------------------------------------------
-- Differential-ID
-- ---------------------------------------------------------------------------

unit_differential_id_parse :: Spec
unit_differential_id_parse = it "parses Differential-ID checksum URN" $
  case runOk DID.differentialIDParser "urn:md5:3FS2oCnWPZptpN05oKBemA==" of
    Right (DID.DifferentialID t) -> t `shouldBe` ST.fromString "urn:md5:3FS2oCnWPZptpN05oKBemA=="
    other -> error (show other)


unit_differential_id_render :: Spec
unit_differential_id_render =
  it "renders Differential-ID" $
    M.toStrictByteString (DID.renderDifferentialID (DID.DifferentialID (ST.fromString "urn:md5:3FS2oCnWPZptpN05oKBemA==")))
      `shouldBe` "urn:md5:3FS2oCnWPZptpN05oKBemA=="


-- ---------------------------------------------------------------------------
-- URI
-- ---------------------------------------------------------------------------

unit_uri_parse :: Spec
unit_uri_parse = it "parses URI angle-bracketed list" $
  case runOk U.uriParser "<http://example.com/a>, <http://example.com/b>" of
    Right (U.URI (a :| [b])) -> do
      a `shouldBe` ST.fromString "http://example.com/a"
      b `shouldBe` ST.fromString "http://example.com/b"
    other -> error (show other)


unit_uri_render :: Spec
unit_uri_render =
  it "renders URI list" $
    let v = U.URI (ST.fromString "http://example.com/a" :| [ST.fromString "http://example.com/b"])
    in M.toStrictByteString (U.renderURI v) `shouldBe` "<http://example.com/a>, <http://example.com/b>"


-- ---------------------------------------------------------------------------
-- Round-trip properties (structured headers)
-- ---------------------------------------------------------------------------

-- | Tag contents within the quoted-string grammar, free of DQUOTE/backslash.
tagGen :: Gen ST.ShortText
tagGen = ST.fromString <$> Gen.string (Range.linear 0 16) (Gen.element ("abcABCxyz0123 .:-" :: String))


-- | Absolute and relative URI references, including the empty reference.
uriPartGen :: Gen ST.ShortText
uriPartGen = do
  prefix <- Gen.element ["", "./", "../", "/", "urn:example:", "https://example.org/"]
  path <- Gen.string (Range.linear 0 16) (Gen.element ("abcABC0123/._-~," :: String))
  suffix <- Gen.element ["", "?q=a%20b,c", "#fragment", "?q=1#part"]
  pure (ST.fromString (prefix <> path <> suffix))


uriGen :: Gen U.URI
uriGen = do
  x <- uriPartGen
  xs <- Gen.list (Range.linear 0 3) uriPartGen
  pure $ U.URI (x :| xs)


prop_content_version :: Property
prop_content_version = property $ do
  v <- forAll (CV.ContentVersion <$> tagGen)
  runOk CV.contentVersionParser (M.toStrictByteString (CV.renderContentVersion v)) === Right v


prop_derived_from :: Property
prop_derived_from = property $ do
  v <- forAll (DF.DerivedFrom <$> tagGen)
  runOk DF.derivedFromParser (M.toStrictByteString (DF.renderDerivedFrom v)) === Right v


prop_uri :: Property
prop_uri = property $ do
  v <- forAll uriGen
  runOk U.uriParser (M.toStrictByteString (U.renderURI v)) === Right v


typedHtmlMetadata :: Spec
typedHtmlMetadata = describe "typed HTML metadata" $ do
  it "uses the last media type in field order and exposes its parameters" $ do
    let script = "Text/JavaScript ; version=1.8"
        style = "text/css; charset=utf-8; title=\"Compact, alternate\""
        headers =
          headerMapFromList
            [ ("Content-Script-Type", "obsolete value")
            , ("Content-Style-Type", "text/dsssl")
            , ("content-script-type", "\t " <> script <> " \t")
            , ("content-style-type", style)
            ]
        expectedScript = CT.ParameterizedContentType (MediaType "Text" "JavaScript") (("version", "1.8") :| [])
        expectedStyle = CT.ParameterizedContentType (MediaType "text" "css") (("charset", "utf-8") :| [("title", "Compact, alternate")])
    case lookupHeader @CST.ContentScriptType headers of
      Right (Just value) -> do
        CST.contentScriptMediaType value `shouldBe` Right expectedScript
        lookupRawHeader "Content-Script-Type" (setHeader value empty) `shouldBe` Just (script :| [])
      other -> expectationFailure (show other)
    case lookupHeader @CStyT.ContentStyleType headers of
      Right (Just value) -> do
        CStyT.contentStyleMediaType value `shouldBe` Right expectedStyle
        lookupRawHeader "Content-Style-Type" (setHeader value empty) `shouldBe` Just (style :| [])
      other -> expectationFailure (show other)
  it "preserves the wire bytes of opaque media parameters" $ do
    let wire = "text/css; title=\"a" <> BS.pack [255, 92, 34] <> "b\""
    case lookupHeader @CStyT.ContentStyleType (headerMapFromList [("Content-Style-Type", wire)]) of
      Right (Just value) -> do
        CStyT.contentStyleTypeBytes value `shouldBe` SBS.toShort wire
        fmap CT.contentTypeParameters (CStyT.contentStyleMediaType value) `shouldBe` Right [("title", "aÿ\"b")]
        lookupRawHeader "Content-Style-Type" (setHeader value empty) `shouldBe` Just (wire :| [])
      other -> expectationFailure (show other)
  it "rejects a malformed final media type even when an earlier value was valid" $ do
    let invalid = ["", "javascript", "text/", "text/*", "*/css", "text /css", "text/css, text/tcl", "text/css; charset=", "text/css; label=\"open", "text/css\r\n"]
        headers name wire = headerMapFromList [(name, "text/css"), (name, wire)]
    forM_ invalid $ \wire -> do
      lookupHeader @CST.ContentScriptType (headers "Content-Script-Type" wire) `shouldSatisfy` isLeft
      lookupHeader @CStyT.ContentStyleType (headers "Content-Style-Type" wire) `shouldSatisfy` isLeft
  it "rejects invalid authored Content-Script-Type fields" $ do
    let previous = headerMapFromList [("Content-Script-Type", "text/javascript"), ("Content-Script-Type", "text/tcl")]
    forM_ ["", "text/*", "text/javascript, text/tcl", "text/javascript; version="] $ \wire -> do
      let value = CST.ContentScriptType wire
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value previous `shouldBe` previous
  it "rejects invalid authored Content-Style-Type fields" $ do
    let previous = headerMapFromList [("Content-Style-Type", "text/css"), ("Content-Style-Type", "text/dsssl")]
    forM_ ["", "*/css", "text/css; label=\"open", "text/css; charset="] $ \wire -> do
      let value = CStyT.ContentStyleType wire
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value previous `shouldBe` previous
  it "uses the last Default-Style and emits quoted title bytes" $ do
    let selected = DS.DefaultStyleBytes (SBS.pack [32, 65, 44, 32, 9, 255, 34, 32])
        quoted = "\" A, \t" <> BS.pack [255] <> "\\\" \""
        headerMap = headerMapFromList [("Default-Style", "\"wide\""), ("default-style", "\t " <> quoted <> " \t")]
    lookupHeader @DS.DefaultStyle headerMap `shouldBe` Right (Just selected)
    lookupRawHeader "Default-Style" (setHeader selected empty) `shouldBe` Just (quoted :| [])
    lookupStyle " \tCompact, wide\t " `shouldBe` Right (Just (DS.DefaultStyle "Compact, wide"))
    lookupStyle "\"\"" `shouldBe` Right (Just (DS.DefaultStyle ""))
  it "rejects controls and incomplete quoted titles" $ do
    forM_ ["plain\x00", "plain\x7f", "plain\r\n", "\"unterminated", "\"title\" extra", "\"a\", \"b\""] $ \wire ->
      lookupStyle wire `shouldSatisfy` isLeft
  where
    empty = headerMapFromList []
    lookupStyle wire = lookupHeader @DS.DefaultStyle (headerMapFromList [("Default-Style", wire)])


typedVersionTags :: Spec
typedVersionTags = describe "typed version tags" $ do
  it "copies an opaque version into Derived-From without a text conversion" $ do
    let input = "\t \"v\\q" <> BS.pack [9, 255] <> "\\\"\\\\\" \t"
        body = SBS.pack [118, 113, 9, 255, 34, 92]
        canonical = "\"vq" <> BS.pack [9, 255] <> "\\\"\\\\\""
    case lookupVersion input of
      Right (Just version) -> do
        CV.contentVersionTagBytes version `shouldBe` body
        CV.contentVersionTag version `shouldBe` "vq\tÿ\"\\"
        let derived = DF.DerivedFromBytes (CV.contentVersionTagBytes version)
        lookupDerived canonical `shouldBe` Right (Just derived)
        lookupDerived input `shouldBe` Right (Just derived)
        lookupRawHeader "Derived-From" (setHeader derived empty) `shouldBe` Just (canonical :| [])
        lookupRawHeader "Content-Version" (setHeader version empty) `shouldBe` Just (canonical :| [])
        M.toStrictByteString ("[" <> DF.renderDerivedFrom derived <> "]") `shouldBe` ("[" <> canonical <> "]")
      other -> expectationFailure (show other)
  it "handles every octet as a quoted literal or quoted pair" $
    forM_ [0 .. 255] $ \byte -> do
      let octet = BS.singleton byte
          body = SBS.toShort octet
          literal = "\"" <> octet <> "\""
          escaped = "\"\\" <> octet <> "\""
          valid = byte == 9 || byte >= 32 && byte /= 127
      if valid && byte /= 34 && byte /= 92
        then do
          lookupVersion literal `shouldBe` Right (Just (CV.ContentVersionBytes body))
          lookupDerived literal `shouldBe` Right (Just (DF.DerivedFromBytes body))
        else assertBad literal
      if valid
        then do
          lookupVersion escaped `shouldBe` Right (Just (CV.ContentVersionBytes body))
          lookupDerived escaped `shouldBe` Right (Just (DF.DerivedFromBytes body))
          let canonical = if byte == 34 || byte == 92 then escaped else literal
          lookupRawHeader "Content-Version" (setHeader (CV.ContentVersionBytes body) empty) `shouldBe` Just (canonical :| [])
          lookupRawHeader "Derived-From" (setHeader (DF.DerivedFromBytes body) empty) `shouldBe` Just (canonical :| [])
        else assertBad escaped
  it "rejects authored invalid Content-Version octets before replacing existing lines" $ do
    let original = headerMapFromList [("Content-Version", "\"a\""), ("Content-Version", "\"b\"")]
    forM_ ([0 .. 8] <> [10 .. 31] <> [127]) $ \byte -> do
      let value = CV.ContentVersionBytes (SBS.pack [118, byte, 50])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
  it "rejects authored invalid Derived-From octets before replacing existing lines" $ do
    let original = headerMapFromList [("Derived-From", "\"a\""), ("Derived-From", "\"b\"")]
    forM_ ([0 .. 8] <> [10 .. 31] <> [127]) $ \byte -> do
      let value = DF.DerivedFromBytes (SBS.pack [118, byte, 50])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
  it "accepts an empty quoted tag and rejects malformed or combined fields" $ do
    lookupVersion "\t \"\" \t" `shouldBe` Right (Just (CV.ContentVersionBytes ""))
    lookupDerived "\t \"\" \t" `shouldBe` Right (Just (DF.DerivedFromBytes ""))
    forM_ ["", "\t", "tag", "\"unterminated", "\"dangling\\", "\"a\", \"b\"", "\"tag\" extra", "\"a\"\r\n"] assertBad
  it "rejects repeated singleton tag fields" $ do
    let repeated name = headerMapFromList [(name, "\"tag\""), (name, "\"tag\"")]
    lookupHeader @CV.ContentVersion (repeated "Content-Version") `shouldSatisfy` isLeft
    lookupHeader @DF.DerivedFrom (repeated "Derived-From") `shouldSatisfy` isLeft
  where
    empty = headerMapFromList []
    lookupVersion wire = lookupHeader @CV.ContentVersion (headerMapFromList [("Content-Version", wire)])
    lookupDerived wire = lookupHeader @DF.DerivedFrom (headerMapFromList [("Derived-From", wire)])
    assertBad wire = do
      lookupVersion wire `shouldSatisfy` isLeft
      lookupDerived wire `shouldSatisfy` isLeft


typedURIFields :: Spec
typedURIFields = describe "typed legacy URI fields" $ do
  it "preserves absolute Content-Base spellings through the header map"
    $ forM_
      [ "https://example.org/documents/Base%20Folder/"
      , "http://[2001:db8::1]:8080/path?q=v%2f"
      , "file:///tmp/base/"
      , "x-custom:base%7e?foo=1"
      ]
    $ \wire -> do
      let value = CB.ContentBase (ST.fromString (BSC.unpack wire))
      lookupBase (" \t" <> wire <> "\t ") `shouldBe` Right (Just value)
      lookupRawHeader "Content-Base" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
  it "rejects relative, malformed, and fragment-bearing Content-Base values"
    $ forM_
      [ ""
      , " \t"
      , "/relative"
      , "//example.org/base"
      , "../base"
      , "#frag"
      , "https://example.org/base#frag"
      , "https://example.org/base%2"
      , "https://host:bad/a"
      , "https://[bad]/a"
      , "9x:base"
      , "https://example.org/a b"
      , "https://example.org/\xff"
      ]
    $ \wire -> lookupBase wire `shouldSatisfy` isLeft
  it "rejects repeated Content-Base fields" $
    lookupHeader @CB.ContentBase (headerMapFromList [("Content-Base", "https://one.example/"), ("Content-Base", "https://two.example/")])
      `shouldSatisfy` isLeft
  it "combines URI lines in order and retains commas inside brackets" $ do
    let headers =
          headerMapFromList
            [ ("URI", ", \t <https://example.org/a,b?x=1,2>, ")
            , ("Uri", "\t ")
            , ("uri", "<../manual%20page#sec>, , <#frag>, <>")
            ]
        value = U.URI ("https://example.org/a,b?x=1,2" :| ["../manual%20page#sec", "#frag", ""])
        rendered = "<https://example.org/a,b?x=1,2>, <../manual%20page#sec>, <#frag>, <>"
    lookupHeader @U.URI headers `shouldBe` Right (Just value)
    lookupRawHeader "URI" (setHeader value (headerMapFromList [])) `shouldBe` Just (rendered :| [])
  it "requires a bracketed member and validates each reference" $ do
    let invalid =
          [ ""
          , ", , "
          , "https://example.org/base"
          , "<https://example.org/> <../other>"
          , "<a%20b"
          , "<a%z0>"
          , "<a b>"
          , "<a#b#c>"
          , "<9x:path>"
          , "<http://[bad]/>"
          , "<http://host:bad/>"
          , "<http://\xff/>"
          , "<<relative>>"
          , "<a> trailing"
          , "<a>; vary=type"
          , "<a\r>"
          ]
    forM_ invalid $ \wire -> lookupHeader @U.URI (headerMapFromList [("URI", wire)]) `shouldSatisfy` isLeft
    lookupHeader @U.URI (headerMapFromList [("URI", "<valid>"), ("URI", "<broken%>")])
      `shouldSatisfy` isLeft
  where
    lookupBase wire = lookupHeader @CB.ContentBase (headerMapFromList [("Content-Base", wire)])


typedIdentifiers :: Spec
typedIdentifiers = describe "typed content identifiers" $ do
  it "preserves MIME message IDs, including literals, quoting and comments" $
    forM_
      [ "<root.42+alt@example.org>"
      , "<part@[IPv6:2001:db8::1]>"
      , "<part@[]>"
      , "<\"part/42\".next@example.org>"
      , "(outer (nested)) <part (owner) @ example.org> (done)"
      , "<part@[a\\]b]>"
      ]
      assertContentID
  it "accepts OData batch IDs with the RFC 3986 unreserved characters" $ do
    let unreserved = "-._~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    forM_ [0 .. 255] $ \byte -> do
      let wire = "request-" <> BS.singleton byte <> "42"
      if byte `BS.elem` unreserved
        then assertContentID wire
        else case byte of
          58 -> assertContentID wire -- A colon introduces an absolute URI.
          _ -> lookupContentID wire `shouldSatisfy` isLeft
  it "preserves composite DRP identifiers in order, including escaped commas"
    $ forM_
      [ "urn:md5:PEFjWBDv/sd9alS9BYuX0w=="
      , "http://www.acme.com/Example/,urn:x-version:5"
      , "https://example.org/a%2Cb?view=short,urn:serial:12,ftp://example.org/Hello%20World"
      ]
    $ \wire -> do
      assertContentID wire
      let value = DID.DifferentialID (ST.fromString (BSC.unpack wire))
      lookupDifferentialID ("\t " <> wire <> " \t") `shouldBe` Right (Just value)
      lookupRawHeader "Differential-ID" (setHeader value (headerMapFromList []))
        `shouldBe` Just (wire :| [])
  it "rejects empty members, nonabsolute DRP members, and malformed URI syntax" $ do
    forM_
      [ ""
      , "\t"
      , "urn:x:1,"
      , "urn:x:1,,urn:x:2"
      , "urn:x:1,part-2"
      , "part-1,part-2"
      , "urn:x:1, urn:x:2"
      , "https://[bad]/x"
      , "https://host:bad/x"
      , "https://host/%x0"
      , "urn:x:1#fragment"
      , "https://example.org/\xff"
      ]
      $ \wire -> do
        lookupContentID wire `shouldSatisfy` isLeft
        lookupDifferentialID wire `shouldSatisfy` isLeft
    forM_ ["request-42", "<part@example.org>", "../item"] $ \wire ->
      lookupDifferentialID wire `shouldSatisfy` isLeft
  it "rejects malformed MIME IDs and trailing input"
    $ forM_
      [ "<>"
      , "<root>"
      , "<@example.org>"
      , "<part@>"
      , "<part..42@example.org>"
      , "<part@.example.org>"
      , "<part@a..b>"
      , "<part@[a[bad]>"
      , "<a@b><c@d>"
      , "<part@x> trailing"
      , "<part@x> (unclosed"
      , "<part@x> (\xff)"
      , "<é@x>"
      , "<part@x>\r\n"
      , "<\"unterminated@x>"
      ]
    $ \wire -> lookupContentID wire `shouldSatisfy` isLeft
  it "enforces singleton field lines even for a composite DRP identifier" $ do
    let repeated name = headerMapFromList [(name, "urn:x:1"), (name, "urn:x:2")]
    lookupHeader @CID.ContentID (repeated "Content-ID") `shouldSatisfy` isLeft
    lookupHeader @DID.DifferentialID (repeated "Differential-ID") `shouldSatisfy` isLeft
  where
    lookupContentID wire = lookupHeader @CID.ContentID (headerMapFromList [("Content-ID", wire)])
    lookupDifferentialID wire = lookupHeader @DID.DifferentialID (headerMapFromList [("Differential-ID", wire)])
    assertContentID wire = do
      let value = CID.ContentID (ST.fromString (BSC.unpack wire))
      lookupContentID wire `shouldBe` Right (Just value)
      lookupContentID ("\t " <> wire <> " \t") `shouldBe` Right (Just value)
      lookupRawHeader "Content-ID" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])


tests :: Spec
tests =
  describe "ObsoleteContent" $
    sequence_
      [ unit_content_base_parse
      , unit_content_base_render
      , unit_content_script_type_parse
      , unit_content_script_type_render
      , unit_content_style_type_parse
      , unit_content_style_type_render
      , unit_content_version_parse
      , unit_content_version_render
      , unit_default_style_parse
      , unit_default_style_render
      , it "rejects control octets in authored Default-Style titles" $ do
          let fields = headerMapFromList [("Default-Style", "\"previous\"")]
              reject :: Word8 -> Expectation
              reject byte = do
                let value = DS.DefaultStyleBytes (SBS.pack [65, byte, 66])
                renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
                setHeader value fields `shouldBe` fields
          mapM_ reject [0, 10, 13, 31, 127]
          renderHeaderLines defaultHeaderSettings (DS.DefaultStyle " A\tB ") `shouldBe` Just ("\" A\tB \"" :| [])
      , unit_derived_from_parse
      , unit_derived_from_render
      , unit_content_id_parse
      , unit_content_id_render
      , unit_differential_id_parse
      , unit_differential_id_render
      , unit_uri_parse
      , unit_uri_render
      , it "Content-Version round-trips" prop_content_version
      , it "Derived-From round-trips" prop_derived_from
      , it "URI round-trips" prop_uri
      , typedHtmlMetadata
      , typedVersionTags
      , typedURIFields
      , typedIdentifiers
      ]
