{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DefactoCorrelation (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.XCorrelationID as XC
import qualified Network.HTTP.Headers.XRequestID as XR
import qualified Network.HTTP.Headers.XRequestStart as XS
import qualified Network.HTTP.Headers.XTraceID as XT
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser, treating any trailing optional whitespace as fully consumed.
parseFull :: ParserT () String a -> ByteString -> Either String a
parseFull p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


-- An opaque token alphabet: enough to cover UUIDs and similar correlation ids.
tokenChars :: [Char]
tokenChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_."


idGen :: Gen ST.ShortText
idGen = ST.fromString <$> Gen.list (Range.linear 1 48) (Gen.element tokenChars)


-- A host-specific epoch timestamp: digits with an optional fractional part.
tsGen :: Gen ST.ShortText
tsGen = do
  whole <- Gen.list (Range.linear 1 16) (Gen.element ['0' .. '9'])
  frac <- Gen.maybe (Gen.list (Range.linear 1 6) (Gen.element ['0' .. '9']))
  pure $ ST.fromString $ whole <> maybe "" ('.' :) frac


reqStartGen :: Gen XS.XRequestStart
reqStartGen = XS.XRequestStart <$> Gen.bool <*> tsGen


------------------------------------------------------------------------
-- X-Request-ID
------------------------------------------------------------------------

unit_requestID_parse :: Spec
unit_requestID_parse =
  it "parses an opaque UUID token verbatim" $
    parseFull XR.xRequestIDParser "550e8400-e29b-41d4-a716-446655440000"
      `shouldBe` Right (XR.XRequestID "550e8400-e29b-41d4-a716-446655440000")


unit_requestID_render :: Spec
unit_requestID_render =
  it "renders the token verbatim" $
    M.toStrictByteString (XR.renderXRequestID (XR.XRequestID "abc-123"))
      `shouldBe` "abc-123"


prop_requestID :: Property
prop_requestID = property $ do
  tok <- forAll idGen
  let v = XR.XRequestID tok
      bs = M.toStrictByteString (XR.renderXRequestID v)
  case parseFull XR.xRequestIDParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


------------------------------------------------------------------------
-- X-Correlation-ID
------------------------------------------------------------------------

unit_correlationID_parse :: Spec
unit_correlationID_parse =
  it "parses an opaque correlation token verbatim" $
    parseFull XC.xCorrelationIDParser "txn-9f8c2a"
      `shouldBe` Right (XC.XCorrelationID "txn-9f8c2a")


unit_correlationID_render :: Spec
unit_correlationID_render =
  it "renders the token verbatim" $
    M.toStrictByteString (XC.renderXCorrelationID (XC.XCorrelationID "txn-9f8c2a"))
      `shouldBe` "txn-9f8c2a"


prop_correlationID :: Property
prop_correlationID = property $ do
  tok <- forAll idGen
  let v = XC.XCorrelationID tok
      bs = M.toStrictByteString (XC.renderXCorrelationID v)
  case parseFull XC.xCorrelationIDParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


------------------------------------------------------------------------
-- X-Trace-ID
------------------------------------------------------------------------

unit_traceID_parse :: Spec
unit_traceID_parse =
  it "parses an opaque trace identifier verbatim" $
    parseFull XT.xTraceIDParser "4bf92f3577b34da6a3ce929d0e0e4736"
      `shouldBe` Right (XT.XTraceID "4bf92f3577b34da6a3ce929d0e0e4736")


unit_traceID_render :: Spec
unit_traceID_render =
  it "renders the identifier verbatim" $
    M.toStrictByteString (XT.renderXTraceID (XT.XTraceID "4bf92f3577b34da6"))
      `shouldBe` "4bf92f3577b34da6"


prop_traceID :: Property
prop_traceID = property $ do
  tok <- forAll idGen
  let v = XT.XTraceID tok
      bs = M.toStrictByteString (XT.renderXTraceID v)
  case parseFull XT.xTraceIDParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


------------------------------------------------------------------------
-- X-Request-Start
------------------------------------------------------------------------

unit_requestStart_parse_nginx :: Spec
unit_requestStart_parse_nginx =
  it "parses the nginx t= form, keeping the prefix" $
    parseFull XS.xRequestStartParser "t=1700000000.123"
      `shouldBe` Right (XS.XRequestStart True "1700000000.123")


unit_requestStart_parse_heroku :: Spec
unit_requestStart_parse_heroku =
  it "parses the bare Heroku epoch-ms form" $
    parseFull XS.xRequestStartParser "1700000000000"
      `shouldBe` Right (XS.XRequestStart False "1700000000000")


unit_requestStart_render_prefixed :: Spec
unit_requestStart_render_prefixed =
  it "renders the t= marker when present" $
    M.toStrictByteString (XS.renderXRequestStart (XS.XRequestStart True "1700000000"))
      `shouldBe` "t=1700000000"


unit_requestStart_render_bare :: Spec
unit_requestStart_render_bare =
  it "renders a bare timestamp without a marker" $
    M.toStrictByteString (XS.renderXRequestStart (XS.XRequestStart False "1700000000000"))
      `shouldBe` "1700000000000"


prop_requestStart :: Property
prop_requestStart = property $ do
  v <- forAll reqStartGen
  let bs = M.toStrictByteString (XS.renderXRequestStart v)
  case parseFull XS.xRequestStartParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "DefactoCorrelation" $ do
    describe "X-Request-ID" $
      sequence_
        [ unit_requestID_parse
        , unit_requestID_render
        , it "round-trip" prop_requestID
        ]
    describe "X-Correlation-ID" $
      sequence_
        [ unit_correlationID_parse
        , unit_correlationID_render
        , it "round-trip" prop_correlationID
        ]
    describe "X-Trace-ID" $
      sequence_
        [ unit_traceID_parse
        , unit_traceID_render
        , it "round-trip" prop_traceID
        ]
    describe "X-Request-Start" $
      sequence_
        [ unit_requestStart_parse_nginx
        , unit_requestStart_parse_heroku
        , unit_requestStart_render_prefixed
        , unit_requestStart_render_bare
        , it "round-trip" prop_requestStart
        ]
