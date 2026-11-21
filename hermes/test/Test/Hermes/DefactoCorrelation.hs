{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DefactoCorrelation (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio ((%))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.XCorrelationID as XC
import qualified Network.HTTP.Headers.XPoweredBy as XP
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


unit_opaqueWire :: Spec
unit_opaqueWire = describe "opaque field bytes" $ do
  it "checks all 256 octets through typed lookup and checked construction" $
    forM_ [0 .. 255] $ \byte -> do
      let wire = " \ttrace-" <> BS.singleton byte <> "\t\255 "
          expected = byte == 9 || (byte >= 32 && byte /= 127)
          headers name = headerMapFromList [(name, wire)]
          check result =
            if expected then result `shouldBe` Right (Just wire) else result `shouldSatisfy` isLeft
      check (fmap (fmap XR.xRequestIDBytes) (lookupHeader @XR.XRequestID (headers "X-Request-ID")))
      check (fmap (fmap XC.xCorrelationIDBytes) (lookupHeader @XC.XCorrelationID (headers "X-Correlation-ID")))
      check (fmap (fmap XT.xTraceIDBytes) (lookupHeader @XT.XTraceID (headers "X-Trace-ID")))
      check (fmap (fmap XP.xPoweredByBytes) (lookupHeader @XP.XPoweredBy (headers "X-Powered-By")))
      check (fmap (Just . XR.xRequestIDBytes) (XR.xRequestIDFromBytes wire))
      check (fmap (Just . XC.xCorrelationIDBytes) (XC.xCorrelationIDFromBytes wire))
      check (fmap (Just . XT.xTraceIDBytes) (XT.xTraceIDFromBytes wire))
      check (fmap (Just . XP.xPoweredByBytes) (XP.xPoweredByFromBytes wire))
  it "retains the original bytes in both rendering paths" $ do
    let wire = "\t\255/abc \t"
        empty = headerMapFromList []
        request = either error id (XR.xRequestIDFromBytes wire)
        correlation = either error id (XC.xCorrelationIDFromBytes wire)
        trace = either error id (XT.xTraceIDFromBytes wire)
        poweredBy = either error id (XP.xPoweredByFromBytes wire)
    lookupRawHeader hXRequestID (setHeader request empty) `shouldBe` Just (wire :| [])
    lookupRawHeader hXCorrelationID (setHeader correlation empty) `shouldBe` Just (wire :| [])
    lookupRawHeader hXTraceID (setHeader trace empty) `shouldBe` Just (wire :| [])
    lookupRawHeader hXPoweredBy (setHeader poweredBy empty) `shouldBe` Just (wire :| [])
    M.toStrictByteString (XR.renderXRequestID request) `shouldBe` wire
    M.toStrictByteString (XC.renderXCorrelationID correlation) `shouldBe` wire
    M.toStrictByteString (XT.renderXTraceID trace) `shouldBe` wire
    M.toStrictByteString (XP.renderXPoweredBy poweredBy) `shouldBe` wire
    XR.xRequestID request `shouldBe` ST.fromString "\t\255/abc \t"
  it "rejects ambiguous duplicate singleton fields" $ do
    let headers name = headerMapFromList [(name, "one"), (name, "two")]
    lookupHeader @XR.XRequestID (headers "X-Request-ID") `shouldSatisfy` isLeft
    lookupHeader @XC.XCorrelationID (headers "X-Correlation-ID") `shouldSatisfy` isLeft
    lookupHeader @XT.XTraceID (headers "X-Trace-ID") `shouldSatisfy` isLeft
    lookupHeader @XP.XPoweredBy (headers "X-Powered-By") `shouldSatisfy` isLeft


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


unit_requestStartEpoch :: Spec
unit_requestStartEpoch = do
  it "reads exact decimal values across documented units and floating-point forms" $ do
    let readEpoch bytes = fmap toRational (XS.xRequestStartFromBytes bytes >>= XS.xRequestStartEpoch)
    readEpoch "t=1700000000.123" `shouldBe` Right (1700000000123 % 1000)
    readEpoch "1700000000123" `shouldBe` Right 1700000000123
    readEpoch "t=1700000000123456" `shouldBe` Right 1700000000123456
    readEpoch "\tt=+0017.000E+8 \t" `shouldBe` Right 1700000000
    readEpoch "-0.00001e-2" `shouldBe` Right (-1 % 10000000)
    readEpoch "18446744073709551616.00000000000000000001"
      `shouldBe` Right (1844674407370955161600000000000000000001 % 100000000000000000000)
  it "preserves wire spelling and can author an exact timestamp" $ do
    let wire = "\t t=0001700000000.12300 \t"
        readHeader = lookupHeader @XS.XRequestStart (headerMapFromList [("X-Request-Start", wire)])
    case readHeader of
      Right (Just parsed) -> do
        XS.xRequestStartTPrefixed parsed `shouldBe` True
        XS.xRequestStartTimestamp parsed `shouldBe` "0001700000000.12300"
        lookupRawHeader hXRequestStart (setHeader parsed (headerMapFromList [])) `shouldBe` Just (wire :| [])
        M.toStrictByteString (XS.renderXRequestStart parsed) `shouldBe` wire
        let authored = XS.xRequestStartFromEpoch True <$> XS.xRequestStartEpoch parsed
        (authored >>= XS.xRequestStartFromBytes . XS.xRequestStartBytes >>= XS.xRequestStartEpoch) `shouldBe` XS.xRequestStartEpoch parsed
      other -> expectationFailure (show other)
  it "rejects incomplete, malformed, out-of-range and repeated timestamps" $ do
    forM_ ["", "t=", "t= ", "t=123 45", "+", "t=1.", "1..2", "1.1a", "1e", "1e+", "NaN", "0xff", "1e999999999999999999999", "1e-999999999999999999999", "1\255", "1\n"] $ \wire -> do
      XS.xRequestStartFromBytes wire `shouldSatisfy` isLeft
      parseFull XS.xRequestStartParser wire `shouldSatisfy` isLeft
    lookupHeader @XS.XRequestStart (headerMapFromList [("X-Request-Start", "1"), ("X-Request-Start", "2")]) `shouldSatisfy` isLeft


tests :: Spec
tests =
  describe "DefactoCorrelation" $ do
    it "checks authored X-Request-ID bytes" $ mapM_ (rejectOpaque "X-Request-ID" . XR.XRequestID) invalidOpaque
    it "checks authored X-Correlation-ID bytes" $ mapM_ (rejectOpaque "X-Correlation-ID" . XC.XCorrelationID) invalidOpaque
    it "checks authored X-Trace-ID bytes" $ mapM_ (rejectOpaque "X-Trace-ID" . XT.XTraceID) invalidOpaque
    it "checks authored X-Powered-By bytes" $ mapM_ (rejectOpaque "X-Powered-By" . XP.XPoweredBy) invalidOpaque
    it "checks legacy request timestamps before replacing the field" $ do
      forM_ ["", " 123", "1.2.3", "1e+", "1\n", "NaN", "1e999999999999999999999"] $ \stamp ->
        rejectOpaque "X-Request-Start" (XS.XRequestStart True stamp)
      renderHeaderLines defaultHeaderSettings (XS.XRequestStart True "-1.2e3") `shouldBe` Just ("t=-1.2e3" :| [])
    unit_opaqueWire
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
    describe "X-Request-Start" $ do
      unit_requestStartEpoch
      sequence_
        [ unit_requestStart_parse_nginx
        , unit_requestStart_parse_heroku
        , unit_requestStart_render_prefixed
        , unit_requestStart_render_bare
        , it "round-trip" prop_requestStart
        ]


invalidOpaque :: [ST.ShortText]
invalidOpaque = ["a\r", "a\n", "a\NUL", "a\DEL", "a\x0b"]


rejectOpaque :: (KnownHeader a, HeaderRenderingResultToNonEmpty (Cardinality a)) => HeaderFieldName -> a -> Expectation
rejectOpaque name value = do
  let fields = setRawHeader name ("old" :| ["other"]) (headerMapFromList [])
  renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
  setHeader value fields `shouldBe` fields
