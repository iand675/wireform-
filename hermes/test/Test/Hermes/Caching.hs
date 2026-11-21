{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Caching (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, lookupHeaderWith, renderHeaderLines, setHeader, withHeaderDateReference)
import qualified Network.HTTP.Headers.CacheControl as CC
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.Pragma as P
import qualified Network.HTTP.Headers.Warning as W
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers -------------------------------------------------------------------

dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parsePragma :: ByteString -> Either String P.Pragma
parsePragma bs = case runParser P.pragmaParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseWarning :: ByteString -> Either String W.Warning
parseWarning bs = case runParser W.warningParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderPragma :: P.Pragma -> ByteString
renderPragma = M.toStrictByteString . P.renderPragma


renderWarning :: W.Warning -> ByteString
renderWarning = M.toStrictByteString . W.renderWarning


mkTime :: Integer -> Int -> Int -> Integer -> UTCTime
mkTime year month day secs =
  UTCTime (fromGregorian year month day) (secondsToDiffTime secs)


cacheControlTests :: Spec
cacheControlTests = describe "Cache-Control" $ do
  it "combines repeated lines, preserving order and accepting empty list members" $ do
    let empty = headerMapFromList []
        fields =
          headerMapFromList
            [ ("Cache-Control", " \t, MAX-AGE=10, ,")
            , ("cache-control", "")
            , ("Cache-Control", " \tNo-Store, max-age=20 \t ")
            ]
    lookupHeader @CC.CacheControl empty `shouldBe` Right Nothing
    lookupHeader @CC.CacheControl fields `shouldBe` Right (Just (CC.CacheControl (CC.MaxAge 10 :| [CC.NoStore, CC.MaxAge 20])))
    lookupHeader @CC.CacheControl (headerMapFromList [("Cache-Control", ", ,\t")]) `shouldBe` Right (Just CC.CacheControlEmpty)
    headerMapToList (setHeader CC.CacheControlEmpty empty) `shouldBe` [("cache-control", "")]
    CC.cacheControlDirectiveList CC.CacheControlEmpty `shouldBe` []

  it "types the remaining RFC 9111 directives and handles their request and response roles" $ do
    let request = [CC.MaxStale Nothing, CC.MaxStale (Just 12), CC.MinFresh 7, CC.OnlyIfCached]
        response = [CC.MustUnderstand, CC.NoStore]
    CC.parseCacheControlDirectives "MAX-STALE, max-stale=\"12\", Min-Fresh=7, ONLY-IF-CACHED, Must-Understand, no-store"
      `shouldBe` Right (request <> response)
    CC.usableDirectives (CC.usableRequestDirectives (request <> response))
      `shouldBe` (request <> [CC.NoStore])
    CC.usableDirectives (CC.usableResponseDirectives (request <> response))
      `shouldBe` response

  it "matches the list grammar for canonical single directives" $ do
    let check value directive = do
          CC.parseCacheControlDirectives value `shouldBe` Right [directive]
          CC.parseCacheControlHeader value `shouldBe` Right (directive :| [])
          lookupHeader @CC.CacheControl (headerMapFromList [("Cache-Control", value)])
            `shouldBe` Right (Just (CC.CacheControl (directive :| [])))
    check "no-cache" (CC.NoCache Nothing)
    check "no-store" CC.NoStore
    check "only-if-cached" CC.OnlyIfCached
    check "max-age=000000000" (CC.MaxAge 0)
    check "max-age=999999999" (CC.MaxAge 999999999)
    check "max-age=1000000000" (CC.MaxAge 1000000000)
    check " Max-Age=\"32\" " (CC.MaxAge 32)
    CC.parseCacheControlDirectives "max-age=12x" `shouldSatisfy` isLeft
    CC.parseCacheControlDirectives "max-age=12, no-store" `shouldBe` Right [CC.MaxAge 12, CC.NoStore]

  it "decodes field names and renders qualified directives with quotes" $ do
    let fields = CC.Private (Just "Set-Cookie, X-Example")
        noCache = CC.NoCache (Just "X-Other")
        expected = CC.CacheControl (fields :| [noCache])
        wire = "private=\"Set-Cookie, X-Example\",no-cache=\"X-Other\""
        headers value = headerMapFromList [("Cache-Control", value)]
    lookupHeader @CC.CacheControl (headers "private=\"Set-Cookie, X\\-Example\", No-Cache=X-Other")
      `shouldBe` Right (Just expected)
    CC.cacheDirectiveFieldNames fields `shouldBe` Just ("set-cookie" :| ["x-example"])
    CC.cacheDirectiveFieldNames noCache `shouldBe` Just ("x-other" :| [])
    CC.privateFor ("set-cookie" :| ["x-example"]) `shouldBe` CC.Private (Just "set-cookie, x-example")
    CC.noCacheFor ("x-other" :| []) `shouldBe` CC.NoCache (Just "x-other")
    headerMapToList (setHeader expected (headerMapFromList [])) `shouldBe` [("cache-control", wire)]
    lookupHeader @CC.CacheControl (headers wire) `shouldBe` Right (Just expected)

  it "applies immutable to freshness and allows qualified private fields in shared caching" $ do
    let directives = [CC.Immutable, CC.StaleWhileRevalidate 30, CC.Private (Just "X-Example"), CC.SMaxAge 60]
    CC.parseCacheControlDirectives "immutable=\"ignored\", stale-while-revalidate=30, private=X-Example, s-maxage=60"
      `shouldBe` Right directives
    null (CC.validateCacheDirectives directives) `shouldBe` False

  it "round-trips extensions with reserved prefixes, empty arguments, escapes and opaque bytes" $ do
    let wire = "Publicity=full,no-cache-v2=\"\",X-Label=\"a, \\\"\\\\\t" <> BS.pack [0x80, 0xff] <> "\""
        headers = headerMapFromList [("Cache-Control", wire)]
        expected =
          CC.CacheControl
            ( CC.Unknown "Publicity" (Just "full")
                :| [CC.Unknown "no-cache-v2" (Just ""), CC.Unknown "X-Label" (Just "a, \"\\\t\x80\xff")]
            )
    lookupHeader @CC.CacheControl headers `shouldBe` Right (Just expected)
    headerMapToList (setHeader expected (headerMapFromList [])) `shouldBe` [("cache-control", wire)]

  it "rejects invalid arguments and leftover input" $
    mapM_
      (\value -> CC.parseCacheControlDirectives value `shouldSatisfy` isLeft)
      [ "private=\"\""
      , "private=\"x:field\""
      , "no-cache=\"X Y\""
      , "no-store=yes"
      , "max-age"
      , "max-age=12x"
      , "max-stale="
      , "min-fresh=+1"
      , "only-if-cached=\"true\""
      , "public no-store"
      ]
  it "validates qualified fields and extension directives before replacing any lines" $ do
    let fields = headerMapFromList [("Cache-Control", "public"), ("Cache-Control", "max-age=60")]
        reject directive = do
          let value = CC.CacheControl (CC.MaxAge 10 :| [directive])
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value fields `shouldBe` fields
    mapM_ reject
      [ CC.Private (Just "")
      , CC.Private (Just " , \t, ")
      , CC.Private (Just "a:b")
      , CC.NoCache (Just "X Access")
      , CC.Unknown "x,public" Nothing
      , CC.Unknown "x\xff" Nothing
      , CC.Unknown "MAX-AGE" (Just "12")
      , CC.Unknown "private" Nothing
      , CC.Unknown "extension" (Just "x\DEL")
      ]
    let value = CC.CacheControl (CC.Private (Just " , Set-Cookie, , ") :| [CC.Unknown "max-age-hint" (Just "30")])
    renderHeaderLines defaultHeaderSettings value `shouldBe` Just ("private=\" , Set-Cookie, , \",max-age-hint=30" :| [])
    lookupHeader @CC.CacheControl (setHeader value fields) `shouldBe` Right (Just value)


-- Pragma --------------------------------------------------------------------

unit_pragma_parse :: Spec
unit_pragma_parse = it "parses no-cache and an extension directive" $
  case parsePragma "no-cache, custom=val" of
    Right (P.Pragma (a :| [b])) -> do
      P.pragmaName a `shouldBe` ST.fromString "no-cache"
      P.pragmaValue a `shouldBe` Nothing
      P.pragmaName b `shouldBe` ST.fromString "custom"
      P.pragmaValue b `shouldBe` Just (P.PragmaToken (ST.fromString "val"))
    other -> error (show other)


unit_pragma_quoted :: Spec
unit_pragma_quoted = it "parses a quoted extension value" $
  case parsePragma "foo=\"a b\"" of
    Right (P.Pragma (a :| [])) -> do
      P.pragmaName a `shouldBe` ST.fromString "foo"
      P.pragmaValue a `shouldBe` Just (P.PragmaQuoted (RFC8941String (ST.fromString "a b")))
    other -> error (show other)


unit_pragma_render :: Spec
unit_pragma_render =
  it "renders directives joined by commas" $
    let v =
          P.Pragma
            ( P.PragmaDirective (ST.fromString "no-cache") Nothing
                :| [P.PragmaDirective (ST.fromString "x") (Just (P.PragmaToken (ST.fromString "y")))]
            )
    in renderPragma v `shouldBe` "no-cache, x=y"


prop_pragma_roundtrip :: Property
prop_pragma_roundtrip = property $ do
  v <- forAll pragmaGen
  let bs = renderPragma v
  case parsePragma bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Warning -------------------------------------------------------------------

unit_warning_parse :: Spec
unit_warning_parse = it "parses a warning-value with agent, text, and date" $
  case parseWarning "112 - \"network down\" \"Sat, 25 Aug 2012 23:34:45 GMT\"" of
    Right (W.Warning (a :| [])) -> do
      W.warnCode a `shouldBe` 112
      W.warnAgent a `shouldBe` ST.fromString "-"
      W.warnText a `shouldBe` RFC8941String (ST.fromString "network down")
      W.warnDate a `shouldBe` Just (mkTime 2012 8 25 84885)
    other -> error (show other)


unit_warning_parse_nodate :: Spec
unit_warning_parse_nodate = it "parses a warning-value without a date" $
  case parseWarning "199 example.net:80 \"misc\"" of
    Right (W.Warning (a :| [])) -> do
      W.warnCode a `shouldBe` 199
      W.warnAgent a `shouldBe` ST.fromString "example.net:80"
      W.warnText a `shouldBe` RFC8941String (ST.fromString "misc")
      W.warnDate a `shouldBe` Nothing
    other -> error (show other)


unit_warning_render :: Spec
unit_warning_render =
  it "renders warn-code zero-padded to three digits" $
    let v = W.Warning (W.WarningValue 12 (ST.fromString "proxy") (RFC8941String (ST.fromString "stale")) Nothing :| [])
    in renderWarning v `shouldBe` "012 proxy \"stale\""


unit_warning_render_date :: Spec
unit_warning_render_date =
  it "renders a warn-date in quotes" $
    let v =
          W.Warning
            ( W.WarningValue 113 (ST.fromString "-") (RFC8941String (ST.fromString "heuristic")) (Just (mkTime 1994 11 6 31777))
                :| []
            )
    in renderWarning v `shouldBe` "113 - \"heuristic\" \"Sun, 06 Nov 1994 08:49:37 GMT\""


prop_warning_roundtrip :: Property
prop_warning_roundtrip = property $ do
  v <- forAll warningGen
  let bs = renderWarning v
  case parseWarning bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Generators ----------------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tchars)
  where
    tchars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-"


-- Quoted-string body restricted to qdtext that never needs escaping, so the
-- round-trip equality does not depend on the escape path (covered elsewhere).
quotedBodyGen :: Gen ST.ShortText
quotedBodyGen = ST.fromString <$> Gen.list (Range.linear 0 10) (Gen.element qchars)
  where
    qchars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " .-_/:"


pragmaValueGen :: Gen P.PragmaValue
pragmaValueGen =
  Gen.choice
    [ P.PragmaToken <$> tokenGen
    , P.PragmaQuoted . RFC8941String <$> quotedBodyGen
    ]


pragmaGen :: Gen P.Pragma
pragmaGen = P.Pragma <$> Gen.nonEmpty (Range.linear 1 4) directiveGen
  where
    directiveGen = P.PragmaDirective <$> tokenGen <*> Gen.maybe pragmaValueGen


warnCodeGen :: Gen Word
warnCodeGen = Gen.word (Range.linear 0 999)


agentGen :: Gen ST.ShortText
agentGen =
  Gen.choice
    [ tokenGen
    , (<> ":8080") <$> tokenGen
    , Gen.element ["example.net", "[::1]", "[2001:db8::1]:443", "[vF.a:b]:"]
    ]


utcTimeGen :: Gen UTCTime
utcTimeGen = do
  year <- Gen.integral (Range.linear 1900 9999) :: Gen Integer
  month <- Gen.integral (Range.linear 1 12) :: Gen Int
  day <- Gen.integral (Range.linear 1 28) :: Gen Int
  secs <- Gen.integral (Range.linear 0 86399) :: Gen Integer
  pure (mkTime year month day secs)


warningGen :: Gen W.Warning
warningGen = W.Warning <$> Gen.nonEmpty (Range.linear 1 4) valueGen
  where
    valueGen =
      W.WarningValue
        <$> warnCodeGen
        <*> agentGen
        <*> (RFC8941String <$> quotedBodyGen)
        <*> Gen.maybe utcTimeGen


legacyFieldTests :: Spec
legacyFieldTests = describe "legacy cache fields" $ do
  it "checks authored Pragma names, tokens, and quoted text before replacing field lines" $ do
    let fields = headerMapFromList [("Pragma", "no-cache"), ("Pragma", "a=b")]
        reject directive = do
          let value = P.Pragma (P.PragmaDirective "no-cache" Nothing :| [directive])
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value fields `shouldBe` fields
    mapM_ reject
      [ P.PragmaDirective "bad, name" Nothing
      , P.PragmaDirective "x" (Just (P.PragmaToken ""))
      , P.PragmaDirective "x" (Just (P.PragmaToken "\"ok\""))
      , P.PragmaDirective "x" (Just (P.PragmaToken "a b"))
      , P.PragmaDirective "x" (Just (P.PragmaQuoted (RFC8941String "a\DEL")))
      ]
  it "rejects authored Warning agents, text, and dates that do not fit the grammar" $ do
    let fields = headerMapFromList [("Warning", "112 - \"stale\""), ("Warning", "199 - \"old\"")]
        valid = W.WarningValue 112 "-" (RFC8941String "new") Nothing
        reject entry = do
          let value = W.Warning (valid :| [entry])
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value fields `shouldBe` fields
    mapM_ reject
      [ valid {W.warnAgent = "edge node"}
      , valid {W.warnAgent = "[:::1]:80"}
      , valid {W.warnAgent = "host:fast"}
      , valid {W.warnAgent = "edge "}
      , valid {W.warnText = RFC8941String "a\DEL"}
      , valid {W.warnDate = Just (mkTime 1899 12 31 0)}
      , valid {W.warnDate = Just (mkTime 10000 1 1 0)}
      , valid {W.warnDate = Just (mkTime 2020 1 1 86401)}
      ]
  it "combines Pragma directives and preserves HTTP quoted octets" $ do
    let fields = headerMapFromList . map (\wire -> ("Pragma", wire))
        readPragma = lookupHeader @P.Pragma . fields
        value = P.Pragma (P.PragmaDirective "no-cache" Nothing :| [P.PragmaDirective "x" (Just (P.PragmaQuoted (RFC8941String "a, \xff x\"\\")))])
    readPragma [", no-cache,, x=\"a", "\xff \\x\\\"\\\\\",", " , \t"] `shouldBe` Right (Just value)
    headerMapToList (setHeader value (fields ["old"]))
      `shouldBe` [("pragma", "no-cache, x=\"a, \xff x\\\"\\\\\"")]
    mapM_ (\wire -> readPragma ["no-cache", wire] `shouldSatisfy` isLeft)
      ["x=", "x =a", "x=\"abc", "x=\"a\n\"", "x=a junk"]
    readPragma [" ,", "\t"] `shouldSatisfy` isLeft
  it "validates Warning agents and preserves HTTP quoted text and order" $ do
    let fields = headerMapFromList . map (\wire -> ("Warning", wire))
        readWarning = lookupHeader @W.Warning . fields
        value =
          W.Warning (W.WarningValue 110 "edge#1" (RFC8941String "a, \xff x") Nothing
            :| [W.WarningValue 199 "[::1]:70000" (RFC8941String "b") Nothing])
    readWarning [",110 edge#1 \"a", "\xff \\x\",, 199 [::1]:70000 \"b\" ,"] `shouldBe` Right (Just value)
    headerMapToList (setHeader value (fields ["old"]))
      `shouldBe` [("warning", "110 edge#1 \"a, \xff x\", 199 [::1]:70000 \"b\"")]
    mapM_ (\agent -> readWarning ["110 " <> agent <> " \"text\""] `shouldSatisfy` isLeft)
      ["@", "user@edge.test", "[:::1]", "[::1]:fast", "edge.test:fast", "edge\t", "\"edge\""]
    readWarning [" ,", "\t"] `shouldSatisfy` isLeft
  it "uses the reference clock for a Warning date" $ do
    let settings = withHeaderDateReference (mkTime 2020 1 1 5) defaultHeaderSettings
        fields = headerMapFromList [("Warning", "112 - \"stale\" \"Wednesday, 01-Jan-70 00:00:05 GMT\"")]
        value = W.Warning (W.WarningValue 112 "-" (RFC8941String "stale") (Just (mkTime 2070 1 1 5)) :| [])
    lookupHeaderWith @W.Warning settings fields `shouldBe` Right (Just value)
  it "preserves the old Warning when an authored code exceeds three digits" $ do
    let fields = headerMapFromList [("Warning", "112 - \"stale\"")]
        invalid = W.Warning (W.WarningValue 1000 "-" (RFC8941String "new") Nothing :| [])
    setHeader invalid fields `shouldBe` fields


-- Suite ---------------------------------------------------------------------

tests :: Spec
tests =
  describe "Caching" $
    sequence_
      [ cacheControlTests
      , legacyFieldTests
      , unit_pragma_parse
      , unit_pragma_quoted
      , unit_pragma_render
      , it "Pragma render/parse round-trip" prop_pragma_roundtrip
      , unit_warning_parse
      , unit_warning_parse_nodate
      , unit_warning_render
      , unit_warning_render_date
      , it "Warning render/parse round-trip" prop_warning_roundtrip
      ]
