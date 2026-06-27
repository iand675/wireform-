{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Tracing (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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
            255
            (ST.fromString "4bf92f3577b34da6a3ce929d0e0e4736")
            (ST.fromString "00f067aa0ba902b7")
            0
    in tpRender v `shouldBe` "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00"


hexText :: Int -> Gen ST.ShortText
hexText n = ST.fromString <$> Gen.list (Range.singleton n) (Gen.element hexChars)
  where
    hexChars = ['0' .. '9'] <> ['a' .. 'f']


tpGen :: Gen TP.Traceparent
tpGen =
  TP.Traceparent
    <$> Gen.word8 Range.constantBounded
    <*> hexText 32
    <*> hexText 16
    <*> Gen.word8 Range.constantBounded


prop_tp_roundtrip :: Property
prop_tp_roundtrip = property $ do
  v <- forAll tpGen
  let bs = tpRender v
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
tsGen = TSt.Tracestate <$> Gen.nonEmpty (Range.linear 1 4) memberGen
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
      , it "traceparent round-trip" prop_tp_roundtrip
      , unit_ts_parse
      , unit_ts_render
      , it "tracestate round-trip" prop_ts_roundtrip
      , unit_st_parse
      , unit_st_render
      , it "server-timing round-trip" prop_st_roundtrip
      , unit_tao_wildcard
      , unit_tao_list
      , unit_tao_render
      , it "timing-allow-origin round-trip" prop_tao_roundtrip
      ]
