{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Tracing (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, renderHeaderLines, renderToHeaders, setHeader)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Origin (parseCorsRequestOrigin)
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.ServerTiming as STm
import qualified Network.HTTP.Headers.TimingAllowOrigin as TAO
import qualified Network.HTTP.Headers.Traceparent as TP
import qualified Network.HTTP.Headers.Tracestate as TSt
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Accept a parse whose only leftover is optional whitespace.
trailingOk :: ByteString -> Bool
trailingOk = BS.null . BS.dropWhile (\w -> w == 0x20 || w == 0x09)


-- Traceparent ---------------------------------------------------------------

tpParse :: ByteString -> Either String TP.Traceparent
tpParse bs = case runParser TP.traceparentParser bs of
  OK v rest
    | trailingOk rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


tpRender :: TP.Traceparent -> ByteString
tpRender = M.toStrictByteString . TP.renderTraceparent


unit_tp_parse :: Spec
unit_tp_parse = it "parses a sampled traceparent" $
  case tpParse "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" of
    Right tp -> do
      TP.traceparentVersion tp `shouldBe` 0
      TP.traceparentTraceId tp `shouldBe` ST.fromString "4bf92f3577b34da6a3ce929d0e0e4736"
      TP.traceparentParentId tp `shouldBe` ST.fromString "00f067aa0ba902b7"
      TP.traceparentFlags tp `shouldBe` 1
    other -> error (show other)


unit_tp_render :: Spec
unit_tp_render =
  it "renders a traceparent" $
    let v =
          TP.Traceparent
            254
            (ST.fromString "4bf92f3577b34da6a3ce929d0e0e4736")
            (ST.fromString "00f067aa0ba902b7")
            0
    in tpRender v `shouldBe` "fe-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00"


traceparentIds :: ByteString
traceparentIds = "-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-"


unit_tp_field :: Spec
unit_tp_field = it "enforces one field, lowercase hex, known-version length and nonzero IDs" $ do
  let readField raw = lookupHeader @TP.Traceparent (headerMapFromList [("traceparent", raw)])
      wire = "00" <> traceparentIds <> "a9"
      expected = TP.Traceparent 0 "4bf92f3577b34da6a3ce929d0e0e4736" "00f067aa0ba902b7" 0xa9
  readField (" \t" <> wire <> "\t") `shouldBe` Right (Just expected)
  headerMapToList (setHeader expected (headerMapFromList [])) `shouldBe` [("traceparent", wire)]
  mapM_
    (\raw -> readField raw `shouldSatisfy` isLeft)
    [ "ff" <> traceparentIds <> "a9"
    , "FE" <> traceparentIds <> "a9"
    , "00-4Bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
    , "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067Aa0ba902b7-01"
    , "00" <> traceparentIds <> "A9"
    , "00-00000000000000000000000000000000-00f067aa0ba902b7-01"
    , "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01"
    , wire <> "-extra"
    ]
  mapM_ (\n -> readField (BS.take n wire) `shouldSatisfy` isLeft) [0 .. BS.length wire - 1]
  lookupHeader @TP.Traceparent (headerMapFromList [("traceparent", wire), ("traceparent", wire)])
    `shouldSatisfy` isLeft


unit_tp_future :: Spec
unit_tp_future = it "retains opaque suffixes only after the future-version delimiter" $ do
  let readField raw = lookupHeader @TP.Traceparent (headerMapFromList [("traceparent", raw)])
      base = "01" <> traceparentIds <> "09"
      opaque = "Vendor/EX=Q-01"
      value = TP.TraceparentWithExtension 1 "4bf92f3577b34da6a3ce929d0e0e4736" "00f067aa0ba902b7" 9 opaque
  readField (base <> "-" <> opaque <> " \t") `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("traceparent", base <> "-" <> opaque)]
  let sliced = BS.take 3 (BS.drop 1 "x\255-=y")
      authored = TP.TraceparentWithExtension 254 "abc" "" 255 sliced
  renderToHeaders defaultHeaderSettings authored `shouldBe` tpRender authored
  renderToHeaders defaultHeaderSettings (value {TP.traceparentExtension = ""}) `shouldBe` base <> "-"
  readField base `shouldBe` Right (Just (TP.Traceparent 1 "4bf92f3577b34da6a3ce929d0e0e4736" "00f067aa0ba902b7" 9))
  readField (base <> "extra") `shouldSatisfy` isLeft
  readField (base <> "-a\r\nb") `shouldSatisfy` isLeft


hexText :: Int -> Gen ST.ShortText
hexText n = ST.fromString <$> Gen.list (Range.singleton n) (Gen.element hexChars)
  where
    hexChars = ['0' .. '9'] <> ['a' .. 'f']


tpGen :: Gen TP.Traceparent
tpGen =
  TP.Traceparent
    <$> Gen.word8 (Range.linear 0 254)
    <*> Gen.filter (/= ST.replicate 32 "0") (hexText 32)
    <*> Gen.filter (/= ST.replicate 16 "0") (hexText 16)
    <*> Gen.word8 Range.constantBounded


prop_tp_roundtrip :: Property
prop_tp_roundtrip = property $ do
  v <- forAll tpGen
  let bs = tpRender v
  renderToHeaders defaultHeaderSettings v === bs
  case tpParse bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Tracestate ----------------------------------------------------------------

tsParse :: ByteString -> Either String TSt.Tracestate
tsParse bs = case runParser TSt.tracestateParser bs of
  OK v rest
    | trailingOk rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


tsRender :: TSt.Tracestate -> ByteString
tsRender = M.toStrictByteString . TSt.renderTracestate


unit_ts_parse :: Spec
unit_ts_parse = it "parses a vendor list" $
  case tsParse "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE" of
    Right ts ->
      NE.toList (TSt.tracestateMembers ts)
        `shouldBe` [ TSt.TracestateMember (ST.fromString "rojo") (ST.fromString "00f067aa0ba902b7")
                   , TSt.TracestateMember (ST.fromString "congo") (ST.fromString "t61rcWkgMzE")
                   ]
    other -> error (show other)


unit_ts_render :: Spec
unit_ts_render = it "renders the same vendor list through the typed field and builder" $ do
  let v = TSt.Tracestate (TSt.TracestateMember "rojo" "00f067aa0ba902b7" :| [TSt.TracestateMember "congo" "t61rcWkgMzE"])
      authored = TSt.Tracestate (TSt.TracestateMember "" "" :| [TSt.TracestateMember "café" " ☕ ", TSt.TracestateMember "last" ""])
  tsRender v `shouldBe` "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  mapM_
    (\state -> renderToHeaders defaultHeaderSettings state `shouldBe` tsRender state)
    [v, authored, TSt.TracestateEmpty]


unit_ts_field :: Spec
unit_ts_field = it "combines field lines in order and preserves spaces within values" $ do
  let value = TSt.Tracestate (TSt.TracestateMember "rojo" " a b" :| [TSt.TracestateMember "7tenant@svc" "two"])
      fields = headerMapFromList [("tracestate", " ,rojo= a b \t"), ("tracestate", ""), ("tracestate", "\t7tenant@svc=two,")]
      readField raw = lookupHeader @TSt.Tracestate (headerMapFromList [("tracestate", raw)])
  lookupHeader fields `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("tracestate", "rojo= a b,7tenant@svc=two")]
  readField " ,\t," `shouldBe` Right (Just TSt.TracestateEmpty)
  tsRender TSt.TracestateEmpty `shouldBe` ""
  readField "rojo=one,rojo=two" `shouldSatisfy` isLeft
  lookupHeader @TSt.Tracestate (headerMapFromList [("tracestate", "rojo=one"), ("tracestate", "rojo=two")])
    `shouldSatisfy` isLeft


unit_ts_limits :: Spec
unit_ts_limits = it "applies key, value and combined member limits at their boundary" $ do
  let readField raw = lookupHeader @TSt.Tracestate (headerMapFromList [("tracestate", raw)])
      simple = BS.replicate 256 97
      tenant = BS.replicate 241 49
      system = BS.replicate 14 97
      value = BS.replicate 256 118
      wide = simple <> "=" <> value <> "," <> tenant <> "@" <> system <> "=" <> value
  fmap (fmap tsRender) (readField (wide <> "    ")) `shouldBe` Right (Just wide)
  readField (BS.replicate 31 44) `shouldBe` Right (Just TSt.TracestateEmpty)
  fmap (fmap tsRender) (readField (BS.replicate 31 44 <> "a=x")) `shouldBe` Right (Just "a=x")
  fmap (fmap tsRender) (readField ("a=x" <> BS.replicate 31 44)) `shouldBe` Right (Just "a=x")
  lookupHeader @TSt.Tracestate (headerMapFromList [("tracestate", BS.replicate 31 44), ("tracestate", "")])
    `shouldSatisfy` isLeft
  mapM_
    (\raw -> readField raw `shouldSatisfy` isLeft)
    [ "a" <> simple <> "=v"
    , "1" <> tenant <> "@" <> system <> "=v"
    , tenant <> "@a" <> system <> "=v"
    , "a=" <> value <> "v"
    , BS.replicate 32 44
    , BS.replicate 31 44 <> "a=x,"
    , "a=x," <> BS.replicate 29 44 <> "a=y"
    , "1simple=v"
    , "simple@=v"
    , "@system=v"
    , "tenant@1system=v"
    , "tenant@one@two=v"
    , "Upper=v"
    , "a="
    , "a=  "
    , "a=\tx"
    , "a=x\x80"
    , "a=x=y"
    , "a =v"
    ]


tsKeyGen :: Gen ST.ShortText
tsKeyGen = do
  first <- Gen.element ['a' .. 'z']
  rest <- Gen.list (Range.linear 0 8) (Gen.element keyChars)
  pure (ST.fromString (first : rest))
  where
    keyChars = ['a' .. 'z'] <> ['0' .. '9'] <> "_-*/"


tsValueGen :: Gen ST.ShortText
tsValueGen = ST.fromString <$> Gen.list (Range.linear 1 16) (Gen.element valChars)
  where
    valChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']


tsGen :: Gen TSt.Tracestate
tsGen = TSt.Tracestate . NE.nubBy ((==) `on` TSt.tracestateKey) <$> Gen.nonEmpty (Range.linear 1 4) memberGen
  where
    memberGen = TSt.TracestateMember <$> tsKeyGen <*> tsValueGen


prop_ts_roundtrip :: Property
prop_ts_roundtrip = property $ do
  v <- forAll tsGen
  let bs = tsRender v
  case tsParse bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Server-Timing -------------------------------------------------------------

stParse :: ByteString -> Either String STm.ServerTiming
stParse bs = case runParser STm.serverTimingParser bs of
  OK v rest
    | trailingOk rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


stRender :: STm.ServerTiming -> ByteString
stRender = M.toStrictByteString . STm.renderServerTiming


unit_st_parse :: Spec
unit_st_parse = it "parses metrics with dur and desc" $
  case stParse "miss, db;dur=53, app;dur=47.2;desc=\"Total time\"" of
    Right st ->
      STm.serverTimingMetrics st
        `shouldBe` [ STm.ServerTimingMetric (ST.fromString "miss") []
                   , STm.ServerTimingMetric
                       (ST.fromString "db")
                       [STm.ServerTimingParam (ST.fromString "dur") (ST.fromString "53")]
                   , STm.ServerTimingMetric
                       (ST.fromString "app")
                       [ STm.ServerTimingParam (ST.fromString "dur") (ST.fromString "47.2")
                       , STm.ServerTimingParam (ST.fromString "desc") (ST.fromString "Total time")
                       ]
                   ]
    other -> error (show other)


unit_st_render :: Spec
unit_st_render =
  it "renders bare tokens and quoted descriptions" $
    let v =
          STm.ServerTiming
            [ STm.ServerTimingMetric
                (ST.fromString "db")
                [STm.ServerTimingParam (ST.fromString "dur") (ST.fromString "53.2")]
            , STm.ServerTimingMetric
                (ST.fromString "cache")
                [STm.ServerTimingParam (ST.fromString "desc") (ST.fromString "Cache hit")]
            ]
    in stRender v `shouldBe` "db;dur=53.2, cache;desc=\"Cache hit\""


unit_st_field_lines :: Spec
unit_st_field_lines = it "preserves metrics and opaque descriptions across fields" $ do
  let readTiming = lookupHeader @STm.ServerTiming . headerMapFromList . map (\field -> ("Server-Timing", field))
      firstMetric = STm.ServerTimingMetric "db" [STm.ServerTimingParam "dur" "1", STm.ServerTimingParam "dur" "2"]
      secondMetric = STm.ServerTimingMetric "db" [STm.ServerTimingParam "desc" "read, \xe9; \"quote\""]
      expected = STm.ServerTiming [firstMetric, secondMetric]
      wire = "db;dur=1;dur=2, db;desc=\"read, \xe9; \\\"quote\\\"\""
  readTiming [" , db; dur = 1;dur=2,", " , db;desc=\"read", "\xe9; \\\"quote\\\"\", "] `shouldBe` Right (Just expected)
  STm.serverTimingParameter "dur" firstMetric `shouldBe` Just "1"
  STm.serverTimingParameter "DUR" firstMetric `shouldBe` Nothing
  headerMapToList (setHeader expected (headerMapFromList [])) `shouldBe` [("server-timing", wire)]
  stRender expected `shouldBe` wire
  readTiming [wire] `shouldBe` Right (Just expected)
  readTiming [" , ", "\t"] `shouldBe` Right (Just (STm.ServerTiming []))
  headerMapToList (setHeader (STm.ServerTiming []) (headerMapFromList [("Server-Timing", "old")])) `shouldBe` []
  mapM_ (\invalid -> readTiming ["db", invalid] `shouldSatisfy` isLeft)
    ["bad metric", "cache;desc=\"open", "cache;dur=1 more", "cache;dur=", "cache;desc=\"\n\""]


stTokenGen :: Gen ST.ShortText
stTokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tokenChars)
  where
    tokenChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']


stGen :: Gen STm.ServerTiming
stGen = STm.ServerTiming <$> Gen.list (Range.linear 1 4) metricGen
  where
    metricGen = STm.ServerTimingMetric <$> stTokenGen <*> Gen.list (Range.linear 0 3) paramGen
    paramGen = STm.ServerTimingParam <$> stTokenGen <*> stTokenGen


prop_st_roundtrip :: Property
prop_st_roundtrip = property $ do
  v <- forAll stGen
  let bs = stRender v
  case stParse bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Timing-Allow-Origin -------------------------------------------------------

taoParse :: ByteString -> Either String TAO.TimingAllowOrigin
taoParse bs = case runParser TAO.timingAllowOriginParser bs of
  OK v rest
    | trailingOk rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


taoRender :: TAO.TimingAllowOrigin -> ByteString
taoRender = M.toStrictByteString . TAO.renderTimingAllowOrigin


unit_tao_wildcard :: Spec
unit_tao_wildcard = it "parses the wildcard" $
  case taoParse "*" of
    Right tao ->
      NE.toList (TAO.timingAllowOriginItems tao) `shouldBe` [TAO.TimingAllowOriginAny]
    other -> error (show other)


unit_tao_list :: Spec
unit_tao_list = it "parses an origin list" $
  case taoParse "https://example.com, https://cdn.example.com:8443" of
    Right tao ->
      NE.toList (TAO.timingAllowOriginItems tao)
        `shouldBe` [ TAO.TimingAllowOriginOrigin (ST.fromString "https://example.com")
                   , TAO.TimingAllowOriginOrigin (ST.fromString "https://cdn.example.com:8443")
                   ]
    other -> error (show other)


unit_tao_render :: Spec
unit_tao_render =
  it "renders origins and the wildcard" $
    let v =
          TAO.TimingAllowOrigin
            (TAO.TimingAllowOriginOrigin (ST.fromString "https://a.test") :| [TAO.TimingAllowOriginAny])
    in taoRender v `shouldBe` "https://a.test, *"


unit_tao_field_lines :: Spec
unit_tao_field_lines = it "combines origins, null and wildcard without swallowing list delimiters" $ do
  let fields =
        headerMapFromList
          [ ("Timing-Allow-Origin", " \t,https://[2001:db8::1]:8443,null")
          , ("Timing-Allow-Origin", "")
          , ("Timing-Allow-Origin", ",https://b%2C.example,https://a{b}.test,*\t")
          ]
      expected =
        TAO.TimingAllowOrigin
          ( TAO.TimingAllowOriginOrigin "https://[2001:db8::1]:8443"
              :| [ TAO.TimingAllowOriginOrigin "null"
                 , TAO.TimingAllowOriginOrigin "https://b%2C.example"
                 , TAO.TimingAllowOriginOrigin "https://a{b}.test"
                 , TAO.TimingAllowOriginAny
                 ]
          )
  lookupHeader @TAO.TimingAllowOrigin fields `shouldBe` Right (Just expected)
  headerMapToList (setHeader expected (headerMapFromList []))
    `shouldBe` [("timing-allow-origin", "https://[2001:db8::1]:8443, null, https://b%2C.example, https://a{b}.test, *")]
  mapM_
    (\wire -> lookupHeader @TAO.TimingAllowOrigin (headerMapFromList [("Timing-Allow-Origin", wire)]) `shouldSatisfy` isLeft)
    ["bare-name", "nullx", "https://a.example/", "https://a.example:", "https://a.example:65536", "https://[invalid]", "https://bad%2.example", ", ,"]


unit_tao_fetch_members :: Spec
unit_tao_fetch_members = it "recognizes complete Fetch list members across field lines" $ do
  let origin = either error id (parseCorsRequestOrigin "https://a.test")
      opaque = either error id (parseCorsRequestOrigin "null")
      quotedHost = either error id (parseCorsRequestOrigin "https://a\"b.test")
      fields = headerMapFromList . map (\wire -> ("Timing-Allow-Origin", wire))
      allows = TAO.timingAllowOriginMatches origin . fields
  mapM_
    (\values -> allows values `shouldBe` True)
    [ [" \t, https://a.test\t, null"]
    , ["malformed", " https://a.test"]
    , ["not an origin, *  "]
    , ["\"quoted, \\\"*, member\"", "*"]
    , ["\"quoted", "member\", *"]
    ]
  mapM_
    (\values -> allows values `shouldBe` False)
    [ []
    , ["\t , "]
    , ["\"*\""]
    , ["prefix\"quoted, *"]
    , ["\"quoted", "*"]
    , ["https://a.test:443, https://A.test"]
    , ["https://a.test\r"]
    , ["*suffix, https://a.test/"]
    ]
  TAO.timingAllowOriginMatches opaque (fields ["null"]) `shouldBe` True
  TAO.timingAllowOriginMatches quotedHost (fields ["https://a\"b.test"]) `shouldBe` True


unit_tao_response_checks :: Spec
unit_tao_response_checks = it "applies response taint, navigation origins and prior failures" $ do
  let origin = either error id (parseCorsRequestOrigin "https://a.test")
      basic = TAO.TimingAllowContext origin False False True
      response wire = headerMapFromList [("Timing-Allow-Origin", wire)]
      empty = headerMapFromList []
      crossNavigation = basic {TAO.timingCrossOriginNavigation = True}
      tainted = basic {TAO.timingBasicResponse = False}
  TAO.timingAllowCheck basic empty `shouldBe` True
  TAO.timingAllowCheck crossNavigation empty `shouldBe` False
  TAO.timingAllowCheck crossNavigation (response "*") `shouldBe` True
  TAO.timingAllowCheck tainted empty `shouldBe` False
  TAO.timingAllowCheck tainted (response "https://a.test") `shouldBe` True
  TAO.timingAllowCheck (basic {TAO.timingAllowFailed = True}) (response "*") `shouldBe` False
  TAO.navigationTimingAllowCheck origin [] `shouldBe` True
  TAO.navigationTimingAllowCheck origin [response "*", empty, response "https://a.test"] `shouldBe` True
  TAO.navigationTimingAllowCheck origin [response "https://b.test", response "*"] `shouldBe` False
  TAO.navigationTimingAllowCheck origin [empty, response ""] `shouldBe` False


taoGen :: Gen TAO.TimingAllowOrigin
taoGen = TAO.TimingAllowOrigin <$> Gen.nonEmpty (Range.linear 1 4) itemGen
  where
    itemGen =
      Gen.choice
        [ pure TAO.TimingAllowOriginAny
        , TAO.TimingAllowOriginOrigin <$> originGen
        ]
    originGen = do
      host <- Gen.list (Range.linear 1 8) (Gen.element ['a' .. 'z'])
      pure (ST.fromString ("https://" <> host <> ".test"))


prop_tao_roundtrip :: Property
prop_tao_roundtrip = property $ do
  v <- forAll taoGen
  let bs = taoRender v
  case taoParse bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


checkedTimingOrigins :: Spec
checkedTimingOrigins = it "checks authored Timing-Allow-Origin members without changing their spelling" $ do
  let fields = headerMapFromList [("Timing-Allow-Origin", "https://old.test"), ("Timing-Allow-Origin", "null")]
      value item = TAO.TimingAllowOrigin (TAO.TimingAllowOriginOrigin "null" :| [item])
  mapM_ (\name -> do
    let invalid = value (TAO.TimingAllowOriginOrigin name)
    renderHeaderLines defaultHeaderSettings invalid `shouldBe` Nothing
    setHeader invalid fields `shouldBe` fields)
    ["", "*", " https://edge.test", "https://edge.test/path", "https://a,b.test", "https://a\"b.test", "https://[:::]", "https://host:65536"]
  renderHeaderLines defaultHeaderSettings (value (TAO.TimingAllowOriginOrigin "https://edge.test:00080"))
    `shouldBe` Just ("null, https://edge.test:00080" :| [])


checkedServerTimingWrites :: Spec
checkedServerTimingWrites = it "validates authored Server-Timing names and quoted values before writing" $ do
  let previous = headerMapFromList [("Server-Timing", "old;dur=1")]
      value metric = STm.ServerTiming [STm.ServerTimingMetric "db" [], metric]
      bad =
        [ STm.ServerTimingMetric "bad metric" []
        , STm.ServerTimingMetric "cache" [STm.ServerTimingParam "" "value"]
        , STm.ServerTimingMetric "cache" [STm.ServerTimingParam "desc" "a\DEL"]
        ]
  mapM_ (\metric -> do
    renderHeaderLines defaultHeaderSettings (value metric) `shouldBe` Nothing
    setHeader (value metric) previous `shouldBe` previous) bad
  let valid = value (STm.ServerTimingMetric "cache" [STm.ServerTimingParam "desc" "café\tread"])
  renderHeaderLines defaultHeaderSettings valid `shouldBe` Just ("db, cache;desc=\"caf\xe9\tread\"" :| [])


checkedTraceparentWrites :: Spec
checkedTraceparentWrites = it "rejects malformed identifiers, reserved versions and invalid authored extensions" $ do
  let traceID = "4bf92f3577b34da6a3ce929d0e0e4736"
      parentID = "00f067aa0ba902b7"
      previous = headerMapFromList [("traceparent", "00" <> traceparentIds <> "01")]
      invalid =
        [ TP.Traceparent 255 traceID parentID 0
        , TP.Traceparent 0 "4Bf92f3577b34da6a3ce929d0e0e4736" parentID 0
        , TP.Traceparent 0 traceID "00f067Aa0ba902b7" 0
        , TP.Traceparent 0 traceID "0000000000000000" 0
        , TP.Traceparent 0 "00000000000000000000000000000000" parentID 0
        , TP.Traceparent 1 "a" parentID 0
        , TP.TraceparentWithExtension 0 traceID parentID 0 ""
        ] ++ map (TP.TraceparentWithExtension 1 traceID parentID 0) ["a\DEL", "a\NULb", "a\r\nb", "ends-in-space ", "ends-in-tab\t"]
  mapM_ (\value -> do
    renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
    setHeader value previous `shouldBe` previous) invalid
  let base = "fe" <> traceparentIds <> "ff-"
  renderHeaderLines defaultHeaderSettings (TP.TraceparentWithExtension 254 traceID parentID 255 "")
    `shouldBe` Just (base :| [])
  renderHeaderLines defaultHeaderSettings (TP.TraceparentWithExtension 254 traceID parentID 255 " \t\xff/a")
    `shouldBe` Just ((base <> " \t\xff/a") :| [])


checkedTracestateWrites :: Spec
checkedTracestateWrites = it "checks authored members, exact key bounds and list uniqueness" $ do
  let first = TSt.TracestateMember "good" "first"
      measure n c = ST.fromString (replicate n c)
      previous = headerMapFromList [("tracestate", "existing=one"), ("tracestate", "previous=two")]
      value rest = TSt.Tracestate (first :| rest)
      invalidNames = ["", "No", "1without", "tenant@1system", "tenant@", "@system", "tenant@sys@more", "space here",
                      measure 257 'a', measure 242 '1' <> "@a", "a@" <> measure 15 'a']
      invalidValues = ["", " ", "trailing ", "with\ttab", "with,comma", "with=equals", "é", measure 257 'a']
      invalidMembers = map (`TSt.TracestateMember` "valid") invalidNames ++ map (TSt.TracestateMember "valid") invalidValues
      many = map (\i -> TSt.TracestateMember (ST.fromString ("k" ++ show i)) "v") [1..32 :: Int]
      invalid = map (value . pure) invalidMembers ++ [value [first], value (take 10 many ++ [first]), value many]
  mapM_ (\candidate -> do
    renderHeaderLines defaultHeaderSettings candidate `shouldBe` Nothing
    setHeader candidate previous `shouldBe` previous) invalid
  let accepted = value (take 31 many)
      boundary = value [TSt.TracestateMember (measure 241 '1' <> "@" <> measure 14 'a') (" " <> measure 255 'v')]
  lookupHeader @TSt.Tracestate (setHeader accepted previous) `shouldBe` Right (Just accepted)
  lookupHeader @TSt.Tracestate (setHeader boundary previous) `shouldBe` Right (Just boundary)
  renderHeaderLines defaultHeaderSettings TSt.TracestateEmpty `shouldBe` Just ("" :| [])


tests :: Spec
tests =
  describe "Tracing" $
    sequence_
      [ checkedTimingOrigins
      , unit_tp_parse
      , unit_tp_render
      , unit_tp_field
      , unit_tp_future
      , checkedTraceparentWrites
      , it "traceparent round-trip" prop_tp_roundtrip
      , unit_ts_parse
      , unit_ts_render
      , unit_ts_field
      , unit_ts_limits
      , checkedTracestateWrites
      , it "tracestate round-trip" prop_ts_roundtrip
      , unit_st_parse
      , unit_st_render
      , unit_st_field_lines
      , checkedServerTimingWrites
      , it "server-timing round-trip" prop_st_roundtrip
      , unit_tao_wildcard
      , unit_tao_list
      , unit_tao_render
      , unit_tao_field_lines
      , unit_tao_fetch_members
      , unit_tao_response_checks
      , it "timing-allow-origin round-trip" prop_tao_roundtrip
      ]
