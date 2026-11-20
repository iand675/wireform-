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
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, renderToHeaders, setHeader)
import qualified Network.HTTP.Headers.Mason as M
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
unit_ts_render =
  it "renders a vendor list" $
    let v =
          TSt.Tracestate
            ( TSt.TracestateMember (ST.fromString "rojo") (ST.fromString "00f067aa0ba902b7")
                :| [TSt.TracestateMember (ST.fromString "congo") (ST.fromString "t61rcWkgMzE")]
            )
    in tsRender v `shouldBe` "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"


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
  lookupHeader @TSt.Tracestate (headerMapFromList [("tracestate", BS.replicate 31 44), ("tracestate", "")])
    `shouldSatisfy` isLeft
  mapM_
    (\raw -> readField raw `shouldSatisfy` isLeft)
    [ "a" <> simple <> "=v"
    , "1" <> tenant <> "@" <> system <> "=v"
    , tenant <> "@a" <> system <> "=v"
    , "a=" <> value <> "v"
    , BS.replicate 32 44
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
          , ("Timing-Allow-Origin", ",https://b%2C.example,*\t")
          ]
      expected =
        TAO.TimingAllowOrigin
          ( TAO.TimingAllowOriginOrigin "https://[2001:db8::1]:8443"
              :| [TAO.TimingAllowOriginOrigin "null", TAO.TimingAllowOriginOrigin "https://b%2C.example", TAO.TimingAllowOriginAny]
          )
  lookupHeader @TAO.TimingAllowOrigin fields `shouldBe` Right (Just expected)
  headerMapToList (setHeader expected (headerMapFromList []))
    `shouldBe` [("timing-allow-origin", "https://[2001:db8::1]:8443, null, https://b%2C.example, *")]
  mapM_
    (\wire -> lookupHeader @TAO.TimingAllowOrigin (headerMapFromList [("Timing-Allow-Origin", wire)]) `shouldSatisfy` isLeft)
    ["bare-name", "nullx", "https://a.example/", "https://a.example:", "https://a.example:65536", "https://[invalid]", "https://bad%2.example", ", ,"]


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


tests :: Spec
tests =
  describe "Tracing" $
    sequence_
      [ unit_tp_parse
      , unit_tp_render
      , unit_tp_field
      , unit_tp_future
      , it "traceparent round-trip" prop_tp_roundtrip
      , unit_ts_parse
      , unit_ts_render
      , unit_ts_field
      , unit_ts_limits
      , it "tracestate round-trip" prop_ts_roundtrip
      , unit_st_parse
      , unit_st_render
      , unit_st_field_lines
      , it "server-timing round-trip" prop_st_roundtrip
      , unit_tao_wildcard
      , unit_tao_list
      , unit_tao_render
      , unit_tao_field_lines
      , it "timing-allow-origin round-trip" prop_tao_roundtrip
      ]
