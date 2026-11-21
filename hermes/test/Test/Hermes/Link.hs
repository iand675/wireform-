{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for "Network.HTTP.Headers.Link" (RFC 8288 Web Linking).

Covers: parsing a single link-value with a quoted param, a
comma-separated list of link-values, bare-token and valueless
params, renderer output (including quoted-string escaping of @\"@),
and a render → parse round-trip property over generated values.
-}
module Test.Hermes.Link (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.ContentNegotiation (MediaType (..))
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
import Network.HTTP.Headers.ContentLanguage (LanguageTag (..))
import qualified Network.HTTP.Headers.Link as L
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOk :: ByteString -> Either String L.Link
parseOk bs = case runParser L.linkParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: L.Link -> ByteString
render = M.toStrictByteString . L.renderLink


st :: String -> ST.ShortText
st = ST.pack


-- ---------------------------------------------------------------------------
-- Unit tests
-- ---------------------------------------------------------------------------

unit_quoted_rel :: Spec
unit_quoted_rel = it "single link-value with quoted rel param" $
  case parseOk "</foo>; rel=\"next\"" of
    Right (L.Link [L.LinkValue tgt [L.LinkParam name val]]) -> do
      tgt `shouldBe` st "/foo"
      name `shouldBe` st "rel"
      val `shouldBe` Just (L.LinkParamQuoted (st "next"))
    other -> error (show other)


unit_multi_value :: Spec
unit_multi_value = it "comma-separated list of link-values" $
  case parseOk "</a>; rel=\"prev\", </b>; rel=\"next\"" of
    Right (L.Link [a, b]) -> do
      L.linkTarget a `shouldBe` st "/a"
      L.linkTarget b `shouldBe` st "/b"
    other -> error (show other)


unit_token_and_bare :: Spec
unit_token_and_bare = it "bare token value and a valueless param" $
  case parseOk "</x>; rel=next; nofollow" of
    Right (L.Link [L.LinkValue tgt [p1, p2]]) -> do
      tgt `shouldBe` st "/x"
      L.linkParamName p1 `shouldBe` st "rel"
      L.linkParamValue p1 `shouldBe` Just (L.LinkParamToken (st "next"))
      L.linkParamName p2 `shouldBe` st "nofollow"
      L.linkParamValue p2 `shouldBe` Nothing
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "render link-value with quoted param" $
    let v =
          L.Link
            [ L.LinkValue
                (st "/foo")
                [L.LinkParam (st "rel") (Just (L.LinkParamQuoted (st "next")))]
            ]
    in render v `shouldBe` "</foo>; rel=\"next\""


unit_render_multi :: Spec
unit_render_multi =
  it "render multiple link-values with token params" $
    let v =
          L.Link
            [ L.LinkValue (st "/a") [L.LinkParam (st "rel") (Just (L.LinkParamToken (st "prev")))]
            , L.LinkValue (st "/b") [L.LinkParam (st "rel") (Just (L.LinkParamToken (st "next")))]
            ]
    in render v `shouldBe` "</a>; rel=prev, </b>; rel=next"


unit_quote_escape :: Spec
unit_quote_escape = it "renderer escapes DQUOTE in a quoted value and round-trips" $ do
  let v =
        L.Link
          [ L.LinkValue
              (st "/x")
              [L.LinkParam (st "title") (Just (L.LinkParamQuoted (st "a\"b")))]
          ]
  render v `shouldBe` "</x>; title=\"a\\\"b\""
  case parseOk (render v) of
    Right v' -> v' `shouldBe` v
    other -> error (show other)


unit_wire_references :: Spec
unit_wire_references = it "validates URI references and combines field lines with empty list elements" $ do
  let fields =
        headerMapFromList
          [ ("Link", " , <../chapter,2?lang=de#examples>; rel=next,")
          , ("Link", "\t, ,")
          , ("Link", "<https://[2001:db8::1]/caf%C3%A9>; rel=alternate, <>; rel=self")
          ]
  case lookupHeader @L.Link fields of
    Right (Just value@(L.Link links)) -> do
      map L.linkTarget links `shouldBe` ["../chapter,2?lang=de#examples", "https://[2001:db8::1]/caf%C3%A9", ""]
      headerMapToList (setHeader value (headerMapFromList []))
        `shouldBe` [("link", "<../chapter,2?lang=de#examples>; rel=next, <https://[2001:db8::1]/caf%C3%A9>; rel=alternate, <>; rel=self")]
    other -> expectationFailure (show other)
  mapM_
    (\bad -> lookupHeader @L.Link (headerMapFromList [("Link", "</ok>; rel=next"), ("Link", bad)]) `shouldSatisfy` isLeft)
    ["</bad escape>", "</bad%2>", "<http://[2001:db8::gg]/>", "<http://example.test:wrong/>", "</x#part#other>", "</x>; rel="]


unit_opaque_quotes :: Spec
unit_opaque_quotes = it "round-trips the permitted quoted-pair octets through the typed renderer" $ do
  let field = "</next>; title=\""
      allowed byte = byte == 9 || (byte >= 32 && byte <= 126) || byte >= 128
      testByte byte =
        let octet = BS.singleton byte
            wire = field <> "\\" <> octet <> "\""
        in if allowed byte
             then case parseOk wire of
               Right value -> do
                 let escape = if byte == 34 || byte == 92 then "\\" else ""
                 render value `shouldBe` (field <> escape <> octet <> "\"")
               other -> expectationFailure (show other)
             else parseOk wire `shouldSatisfy` isLeft
  mapM_ testByte [minBound .. maxBound]


unit_authored_quotes :: Spec
unit_authored_quotes = it "writes Latin-1, authored Unicode, and escapes into a composed builder" $ do
  let prefix = BS.replicate 2048 97
      body = ST.pack (replicate 2048 'a' <> "\xe9\x100\x1f642\t\NUL\DEL\"\\")
      entry = L.LinkValue "/x" [L.LinkParam "title" (Just (L.LinkParamQuoted body)), L.LinkParam "flag" Nothing]
      field = "</x>; title=\"" <> prefix <> BS.pack [0xe9, 0xc4, 0x80, 0xf0, 0x9f, 0x99, 0x82, 9, 63, 63, 92, 34, 92, 92] <> "\"; flag"
      validBody = ST.pack ("\xe9\x100\x1f642\t\"\\")
      valid = L.LinkValue "/x" [L.LinkParam "rel" (Just (L.LinkParamToken "next")), L.LinkParam "title" (Just (L.LinkParamQuoted validBody))]
      validField = "</x>; rel=next; title=\"" <> BS.pack [0xe9, 0xc4, 0x80, 0xf0, 0x9f, 0x99, 0x82, 9, 92, 34, 92, 92] <> "\""
  M.toStrictByteString ("before " <> L.renderLinkValue entry <> " after") `shouldBe` ("before " <> field <> " after")
  headerMapToList (setHeader (L.Link [valid]) (headerMapFromList [])) `shouldBe` [("link", validField)]
  render (L.Link []) `shouldBe` ""


unit_parameter_semantics :: Spec
unit_parameter_semantics = it "projects the first relation and anchor and decodes an extended title" $ do
  let wire = "</next>; REL=\"next  https://relations.example/Feature#part\"; rel=prev; Anchor=\"../catalog#start\"; anchor=\"/ignored\"; title=fallback; TITLE*=UTF-8'en'%E2%82%AC"
  case parseOk wire of
    Right (L.Link [value]) -> do
      L.linkRelations value `shouldBe` Right (L.RegisteredRelation "next" :| [L.ExtensionRelation "https://relations.example/Feature#part"])
      L.linkAnchor value `shouldBe` Right (Just "../catalog#start")
      L.linkTitle value `shouldBe` Just "€"
      L.lookupLinkParam "rEL" value `shouldBe` Just (L.LinkParam "REL" (Just (L.LinkParamQuoted "next  https://relations.example/Feature#part")))
    other -> expectationFailure (show other)
  L.ExtensionRelation "https://relations.example/Feature" `shouldBe` L.ExtensionRelation "HTTPS://RELATIONS.EXAMPLE/feature"


unit_invalid_parameters :: Spec
unit_invalid_parameters = it "reports invalid relation and anchor values and falls back to the plain title" $ do
  let wire = "</x>; rel=\"next\tprev\"; rel=next; anchor=\"/bad%ZZ\"; title*=UTF-8''%FF; title=fallback"
  case parseOk wire of
    Right (L.Link [value]) -> do
      L.linkRelations value `shouldSatisfy` isLeft
      L.linkAnchor value `shouldSatisfy` isLeft
      L.linkTitle value `shouldBe` Just "fallback"
    other -> expectationFailure (show other)
  L.linkRelations (L.LinkValue "/x" []) `shouldSatisfy` isLeft


unit_language_and_type_hints :: Spec
unit_language_and_type_hints = it "projects ordered language hints and the first type through typed field round trips" $ do
  let wire = "</guide>; rel=alternate; HREFLANG=en-GB; lang=en; hreflang=\"zh-Hant-TW\"; TYPE=\"Application/vnd.example+json\"; hreflang=x-example"
      parsed = lookupHeader @L.Link (headerMapFromList [("link", wire)])
  case parsed of
    Right (Just link@(L.Link [value])) -> do
      L.linkLanguages value `shouldBe` Right [LanguageTag "en-GB", LanguageTag "zh-Hant-TW", LanguageTag "x-example"]
      L.linkMediaType value `shouldBe` Right (Just (MediaType "Application" "vnd.example+json"))
      let fields = setHeader link (headerMapFromList [])
      headerMapToList fields `shouldBe` [("link", wire)]
      lookupHeader @L.Link fields `shouldBe` parsed
    other -> expectationFailure (show other)
  L.linkLanguages (L.LinkValue "/x" []) `shouldBe` Right []
  L.linkMediaType (L.LinkValue "/x" []) `shouldBe` Right Nothing


unit_invalid_hints :: Spec
unit_invalid_hints = it "rejects invalid projected hints and honors the media name length limit" $ do
  let project select wire = do
        L.Link values <- parseOk wire
        case values of
          [value] -> select value
          _ -> Left "expected a single link"
      typeHint spelling = "</x>; type=\"" <> spelling <> "\""
      validName = BS.replicate 127 97
      longName = BS.replicate 128 97
  mapM_
    (\wire -> project L.linkLanguages wire `shouldSatisfy` isLeft)
    ["</x>; hreflang", "</x>; hreflang=en; hreflang=en--GB", "</x>; hreflang=\"en,fr\"", "</x>; hreflang=*"]
  project L.linkMediaType "</x>; type; type=\"text/html\"" `shouldSatisfy` isLeft
  project L.linkMediaType (typeHint (validName <> "/" <> validName))
    `shouldBe` Right (Just (MediaType (st (replicate 127 'a')) (st (replicate 127 'a'))))
  mapM_
    (\spelling -> project L.linkMediaType (typeHint spelling) `shouldSatisfy` isLeft)
    ["text/*", "text /html", "text/html;charset=utf-8", "text/%html", "!text/html", "text/h%tml", longName <> "/html", "text/" <> longName]


unit_extended_title_precedence :: Spec
unit_extended_title_precedence = it "uses the first extended title and its first plain fallback" $ do
  let title wire = do
        L.Link values <- parseOk wire
        case values of
          [value] -> Right (L.linkTitle value)
          _ -> Left "expected a single link"
  title "</x>; title=plain; TITLE*=\"iSo-8859-1'fr'caf%E9+.txt\"; title*=UTF-8''ignored"
    `shouldBe` Right (Just "caf\xe9+.txt")
  title "</x>; title*=UTF-8''bad%z; TITLE=plain; title*=UTF-8''ignored; title=second"
    `shouldBe` Right (Just "plain")
  title "</x>; title*; title*=UTF-8''ignored" `shouldBe` Right Nothing


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

tokenChar :: Gen Char
tokenChar = Gen.frequency [(52, Gen.alpha), (10, Gen.digit)]


tokenText :: Gen ST.ShortText
tokenText = ST.pack <$> Gen.list (Range.linear 1 10) tokenChar


targetText :: Gen ST.ShortText
targetText = do
  segment <- tokenText
  Gen.element
    [ ""
    , segment
    , "/" <> segment <> "%20item"
    , "../" <> segment <> "?a=2#top"
    , "//example.test/" <> segment
    , "https://[2001:db8::1]/" <> segment
    ]


quotedChar :: Gen Char
quotedChar = Gen.element (['\t'] <> [' ' .. '~'] <> ['\x80' .. '\xff'])


quotedText :: Gen ST.ShortText
quotedText = ST.pack <$> Gen.list (Range.linear 0 20) quotedChar


paramGen :: Gen L.LinkParam
paramGen = do
  name <- tokenText
  val <-
    Gen.choice
      [ pure Nothing
      , Just . L.LinkParamToken <$> tokenText
      , Just . L.LinkParamQuoted <$> quotedText
      ]
  pure (L.LinkParam name val)


linkValueGen :: Gen L.LinkValue
linkValueGen = L.LinkValue <$> targetText <*> Gen.list (Range.linear 0 4) paramGen


-- ---------------------------------------------------------------------------
-- Property: render → parse round-trip
-- ---------------------------------------------------------------------------

prop_roundtrip :: Property
prop_roundtrip = property $ do
  vs <- forAll (Gen.list (Range.linear 1 4) linkValueGen)
  let l = L.Link vs
      bs = render l
  case parseOk bs of
    Right l' -> l' === l
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Link" $
    sequence_
      [ unit_quoted_rel
      , unit_multi_value
      , unit_token_and_bare
      , unit_render
      , unit_render_multi
      , unit_quote_escape
      , unit_wire_references
      , unit_opaque_quotes
      , unit_authored_quotes
      , unit_parameter_semantics
      , unit_invalid_parameters
      , unit_language_and_type_hints
      , unit_invalid_hints
      , unit_extended_title_precedence
      , it "render/parse round-trips" prop_roundtrip
      ]
