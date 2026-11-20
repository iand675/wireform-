{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the WebDAV extension header family (RFC 2518 / 3253 / 3648 / 4437).
module Test.Hermes.WebDavExt (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
import qualified Network.HTTP.Headers.ApplyToRedirectRef as ATR
import qualified Network.HTTP.Headers.DASL as DASL
import qualified Network.HTTP.Headers.Label as Label
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OrderingType as OT
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.Position as Pos
import qualified Network.HTTP.Headers.RedirectRef as RR
import qualified Network.HTTP.Headers.StatusURI as SU
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run a parser, requiring all non-OWS input to be consumed.
runP :: ParserT () String a -> ByteString -> Either String a
runP p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


-- ---------------------------------------------------------------------------
-- DASL (RFC 5323)
-- ---------------------------------------------------------------------------

unit_dasl_parse :: Spec
unit_dasl_parse =
  it "DASL parses a query-grammar coded-URL" $
    runP DASL.daslParser "<DAV:basicsearch>"
      `shouldBe` Right (DASL.DASL "<DAV:basicsearch>")


unit_dasl_render :: Spec
unit_dasl_render =
  it "DASL renders verbatim" $
    M.toStrictByteString (DASL.renderDASL (DASL.DASL "<DAV:basicsearch>"))
      `shouldBe` "<DAV:basicsearch>"


genDasl :: Gen DASL.DASL
genDasl = DASL.DASLGrammars <$> ((:|) <$> grammar <*> Gen.list (Range.linear 0 4) grammar)
  where
    grammar = (\name -> "<DAV:" <> ST.fromString name <> ">") <$> Gen.string (Range.linear 1 24) Gen.alphaNum


prop_dasl_roundtrip :: Property
prop_dasl_roundtrip = property $ do
  v <- forAll genDasl
  let bs = M.toStrictByteString (DASL.renderDASL v)
  runP DASL.daslParser bs === Right v


unit_dasl_typed :: Spec
unit_dasl_typed = it "combines DASL grammar identifiers across physical fields" $ do
  let field wire = ("DASL", wire)
      typed = lookupHeader @DASL.DASL . headerMapFromList
      value = DASL.DASLGrammars ("<DAV:basicsearch>" :| ["<urn:example:search>", "<https://search.example>"])
  typed [field " , <DAV:basicsearch>, <urn:example:search>", field " <https://search.example>,"]
    `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList []))
    `shouldBe` [("dasl", "<DAV:basicsearch>, <urn:example:search>, <https://search.example>")]
  mapM_
    (\wire -> typed [field wire] `shouldSatisfy` isLeft)
    ["", ",,", "DAV:basicsearch", "</not-absolute>", "<DAV:basicsearch#fragment>", "<DAV:basicsearch> x"]


-- ---------------------------------------------------------------------------
-- Label (RFC 3253)
-- ---------------------------------------------------------------------------

unit_label_parse :: Spec
unit_label_parse =
  it "Label parses a token label-name" $
    runP Label.labelParser "stable-v1.0"
      `shouldBe` Right (Label.Label "stable-v1.0")


unit_label_render :: Spec
unit_label_render =
  it "Label renders the label name" $
    M.toStrictByteString (Label.renderLabel (Label.Label "stable-v1.0"))
      `shouldBe` "stable-v1.0"


genToken :: Gen ST.ShortText
genToken =
  ST.fromString
    <$> Gen.string (Range.linear 1 16) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-._"))


prop_label_roundtrip :: Property
prop_label_roundtrip = property $ do
  v <- forAll (Label.Label <$> genToken)
  let bs = M.toStrictByteString (Label.renderLabel v)
  runP Label.labelParser bs === Right v


-- ---------------------------------------------------------------------------
-- Ordering-Type (RFC 3648)
-- ---------------------------------------------------------------------------

unit_orderingType_parse :: Spec
unit_orderingType_parse =
  it "Ordering-Type parses a URI reference" $
    runP OT.orderingTypeParser "http://example.com/orderings/custom"
      `shouldBe` Right (OT.OrderingType "http://example.com/orderings/custom")


unit_orderingType_render :: Spec
unit_orderingType_render =
  it "Ordering-Type renders the URI verbatim" $
    M.toStrictByteString (OT.renderOrderingType (OT.OrderingType "DAV:custom"))
      `shouldBe` "DAV:custom"


genUri :: Gen ST.ShortText
genUri =
  ST.fromString . ("https://example.test/" <>)
    <$> Gen.string (Range.linear 1 24) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> ":/.-_~"))


prop_orderingType_roundtrip :: Property
prop_orderingType_roundtrip = property $ do
  v <- forAll (OT.OrderingType <$> genUri)
  let bs = M.toStrictByteString (OT.renderOrderingType v)
  runP OT.orderingTypeParser bs === Right v


-- ---------------------------------------------------------------------------
-- Position (RFC 3648)
-- ---------------------------------------------------------------------------

unit_position_first :: Spec
unit_position_first =
  it "Position parses first" $
    runP Pos.positionParser "first" `shouldBe` Right Pos.PositionFirst


unit_position_before :: Spec
unit_position_before =
  it "Position parses before <segment>" $
    runP Pos.positionParser "before chapter1"
      `shouldBe` Right (Pos.PositionBefore "chapter1")


unit_position_render :: Spec
unit_position_render =
  it "Position renders after <segment>" $
    M.toStrictByteString (Pos.renderPosition (Pos.PositionAfter "intro"))
      `shouldBe` "after intro"


genSegment :: Gen ST.ShortText
genSegment =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 12)
      (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-._~"))


genPosition :: Gen Pos.Position
genPosition =
  Gen.choice
    [ pure Pos.PositionFirst
    , pure Pos.PositionLast
    , Pos.PositionBefore <$> genSegment
    , Pos.PositionAfter <$> genSegment
    ]


prop_position_roundtrip :: Property
prop_position_roundtrip = property $ do
  v <- forAll genPosition
  let bs = M.toStrictByteString (Pos.renderPosition v)
  runP Pos.positionParser bs === Right v


-- ---------------------------------------------------------------------------
-- Apply-To-Redirect-Ref (RFC 4437)
-- ---------------------------------------------------------------------------

unit_atr_parse :: Spec
unit_atr_parse = it "Apply-To-Redirect-Ref parses T and F" $ do
  runP ATR.applyToRedirectRefParser "T" `shouldBe` Right (ATR.ApplyToRedirectRef True)
  runP ATR.applyToRedirectRefParser "F" `shouldBe` Right (ATR.ApplyToRedirectRef False)


unit_atr_render :: Spec
unit_atr_render = it "Apply-To-Redirect-Ref renders T and F" $ do
  M.toStrictByteString (ATR.renderApplyToRedirectRef (ATR.ApplyToRedirectRef True))
    `shouldBe` "T"
  M.toStrictByteString (ATR.renderApplyToRedirectRef (ATR.ApplyToRedirectRef False))
    `shouldBe` "F"


prop_atr_roundtrip :: Property
prop_atr_roundtrip = property $ do
  v <- forAll (ATR.ApplyToRedirectRef <$> Gen.bool)
  let bs = M.toStrictByteString (ATR.renderApplyToRedirectRef v)
  runP ATR.applyToRedirectRefParser bs === Right v


-- ---------------------------------------------------------------------------
-- Redirect-Ref (RFC 4437)
-- ---------------------------------------------------------------------------

unit_redirectRef_parse :: Spec
unit_redirectRef_parse =
  it "Redirect-Ref parses a link target" $
    runP RR.redirectRefParser "http://example.com/target"
      `shouldBe` Right (RR.RedirectRef "http://example.com/target")


unit_redirectRef_render :: Spec
unit_redirectRef_render =
  it "Redirect-Ref renders the target verbatim" $
    M.toStrictByteString (RR.renderRedirectRef (RR.RedirectRef "/relative/path"))
      `shouldBe` "/relative/path"


prop_redirectRef_roundtrip :: Property
prop_redirectRef_roundtrip = property $ do
  v <- forAll (RR.RedirectRef <$> Gen.choice [genUri, Gen.element ["../target", "//example.test/", "?q=caf%C3%A9", "#version", ""]])
  let bs = M.toStrictByteString (RR.renderRedirectRef v)
  runP RR.redirectRefParser bs === Right v


-- ---------------------------------------------------------------------------
-- Status-URI (RFC 2518)
-- ---------------------------------------------------------------------------

unit_statusURI_single :: Spec
unit_statusURI_single =
  it "Status-URI parses one status/coded-URL pair" $
    runP SU.statusURIParser "423 <http://example.com/locked>"
      `shouldBe` Right (SU.StatusURI [SU.StatusURIEntry 423 "http://example.com/locked"])


unit_statusURI_multi :: Spec
unit_statusURI_multi =
  it "Status-URI parses multiple pairs" $
    runP SU.statusURIParser "404 <http://a.test/x> 423 <http://b.test/y>"
      `shouldBe` Right
        ( SU.StatusURI
            [ SU.StatusURIEntry 404 "http://a.test/x"
            , SU.StatusURIEntry 423 "http://b.test/y"
            ]
        )


unit_statusURI_render :: Spec
unit_statusURI_render =
  it "Status-URI renders code and coded-URL" $
    M.toStrictByteString
      (SU.renderStatusURI (SU.StatusURI [SU.StatusURIEntry 102 "http://h.test/p"]))
      `shouldBe` "102 <http://h.test/p>"


genStatusEntry :: Gen SU.StatusURIEntry
genStatusEntry =
  SU.StatusURIEntry
    <$> Gen.integral (Range.linear 100 599)
    <*> genUri


genStatusURI :: Gen SU.StatusURI
genStatusURI = SU.StatusURI <$> Gen.list (Range.linear 1 3) genStatusEntry


prop_statusURI_roundtrip :: Property
prop_statusURI_roundtrip = property $ do
  v <- forAll genStatusURI
  let bs = M.toStrictByteString (SU.renderStatusURI v)
  runP SU.statusURIParser bs === Right v


typedExtensions :: Spec
typedExtensions = describe "typed extensions" $ do
  it "round-trips a URL-escaped Unicode Label and retains its original spelling" $ do
    let field wire = ("Label", wire)
        typed = lookupHeader @Label.Label . headerMapFromList
        raw = "release%20B.3/caf%c3%a9+%25"
        constructed = Label.labelFromText "Café / +%"
    case typed [field (" \t" <> raw <> " ")] of
      Right (Just value) -> do
        Label.labelText value `shouldBe` Just "release B.3/café+%"
        headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("label", raw)]
      result -> expectationFailure (show result)
    headerMapToList (setHeader constructed (headerMapFromList []))
      `shouldBe` [("label", "Caf%C3%A9%20%2F%20%2B%25")]
    typed [field ""] `shouldBe` Right (Just (Label.Label ""))
    typed [field "one", field "two"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> typed [field wire] `shouldSatisfy` isLeft)
      ["space here", "raw#mark", "a%", "%3", "%XX", "%80", "%C0%AF", "%ED%A0%80", "%C3", BS.pack [0xc3, 0xa9]]

  it "validates Position keywords and a single percent-escaped segment" $ do
    let field wire = ("Position", wire)
        typed = lookupHeader @Pos.Position . headerMapFromList
    typed [field " \tBeFoRe\tentry%2fname;rev=2 "]
      `shouldBe` Right (Just (Pos.PositionBefore "entry%2fname;rev=2"))
    typed [field "AfTeR %2fetc"] `shouldBe` Right (Just (Pos.PositionAfter "%2fetc"))
    typed [field "first", field "last"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> typed [field wire] `shouldSatisfy` isLeft)
      ["firstly", "after", "afterfile", "after file/name", "before ?q=1", "after file#x", "before fi%2le", "after %"]

  it "distinguishes an absolute ordering URI from a general redirect target" $ do
    let ordering wire = ("Ordering-Type", wire)
        redirect wire = ("Redirect-Ref", wire)
        readOrdering = lookupHeader @OT.OrderingType . headerMapFromList
        readRedirect = lookupHeader @RR.RedirectRef . headerMapFromList
    readOrdering [ordering " DAV:unordered "] `shouldBe` Right (Just (OT.OrderingType "DAV:unordered"))
    readOrdering [ordering "http://example.test"] `shouldBe` Right (Just (OT.OrderingType "http://example.test"))
    readOrdering [ordering "urn:first", ordering "urn:second"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> readOrdering [ordering wire] `shouldSatisfy` isLeft)
      ["", "relative", "//example.test", "/list", "DAV:custom#fragment", "DAV:%7Z"]
    readRedirect [redirect " ../docs/file%20name?rev=2#heading "]
      `shouldBe` Right (Just (RR.RedirectRef "../docs/file%20name?rev=2#heading"))
    readRedirect [redirect "//example.test/path"]
      `shouldBe` Right (Just (RR.RedirectRef "//example.test/path"))
    readRedirect [redirect "/one", redirect "/two"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> readRedirect [redirect wire] `shouldSatisfy` isLeft)
      ["space here", "%8", "https://[bad-ip]/", "/two#fr#agment"]

  it "canonicalizes the redirect-reference flag and rejects duplicates" $ do
    let field wire = ("Apply-To-Redirect-Ref", wire)
        typed = lookupHeader @ATR.ApplyToRedirectRef . headerMapFromList
    typed [field "\tt\t"] `shouldBe` Right (Just (ATR.ApplyToRedirectRef True))
    typed [field " f "] `shouldBe` Right (Just (ATR.ApplyToRedirectRef False))
    typed [field "F", field "T"] `shouldSatisfy` isLeft
    typed [field "false"] `shouldSatisfy` isLeft

  it "checks three-digit HTTP status codes and coded URL boundaries" $ do
    let field wire = ("Status-URI", wire)
        typed = lookupHeader @SU.StatusURI . headerMapFromList
        value = SU.StatusURI [SU.StatusURIEntry 100 "https://example.test", SU.StatusURIEntry 599 "urn:example:item"]
    typed [field " 100<https://example.test>599 \t<urn:example:item> "]
      `shouldBe` Right (Just value)
    headerMapToList (setHeader value (headerMapFromList []))
      `shouldBe` [("status-uri", "100 <https://example.test> 599 <urn:example:item>")]
    typed [field ""] `shouldBe` Right (Just (SU.StatusURI []))
    typed [field "200 <urn:one>", field "404 <urn:two>"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> typed [field wire] `shouldSatisfy` isLeft)
      ["099 <urn:one>", "600 <urn:one>", "0100 <urn:one>", "99 <urn:one>", "200 </relative>", "200 <urn:x#frag>", "200 < urn:x>", "200 <urn:one>, 201 <urn:two>"]

  it "round-trips authored Unicode label text through typed lookup" $ property $ do
    characters <- forAll (Gen.string (Range.linear 0 30) Gen.unicode)
    let value = Label.labelFromText (T.pack characters)
        fields = setHeader value (headerMapFromList [])
    (fmap (>>= Label.labelText) (lookupHeader @Label.Label fields)) === Right (Just (T.pack characters))


-- ---------------------------------------------------------------------------
-- Aggregate
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "WebDavExt" $
    sequence_
      [ unit_dasl_parse
      , unit_dasl_render
      , it "DASL round-trip" prop_dasl_roundtrip
      , unit_dasl_typed
      , unit_label_parse
      , unit_label_render
      , it "Label round-trip" prop_label_roundtrip
      , unit_orderingType_parse
      , unit_orderingType_render
      , it "Ordering-Type round-trip" prop_orderingType_roundtrip
      , unit_position_first
      , unit_position_before
      , unit_position_render
      , it "Position round-trip" prop_position_roundtrip
      , unit_atr_parse
      , unit_atr_render
      , it "Apply-To-Redirect-Ref round-trip" prop_atr_roundtrip
      , unit_redirectRef_parse
      , unit_redirectRef_render
      , it "Redirect-Ref round-trip" prop_redirectRef_roundtrip
      , unit_statusURI_single
      , unit_statusURI_multi
      , unit_statusURI_render
      , it "Status-URI round-trip" prop_statusURI_roundtrip
      , typedExtensions
      ]
