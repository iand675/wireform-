module Test.Hermes.StructuredFields (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short.Unsafe as ST
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Grammar.Parser as P
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.AcceptCH as ACH
import qualified Network.HTTP.Headers.CacheStatus as CS
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), ParserT, RFC8941String (..), Result (..), rfc8941Integer, rfc8941ItemValue, rfc8941Token, runParser, structuredFieldKey)
import qualified Network.HTTP.Headers.PermissionsPolicy as PP
import qualified Network.HTTP.Headers.ProxyStatus as PS
import qualified Network.HTTP.Headers.Rendering.Util as R
import qualified Network.HTTP.Headers.ReportingEndpoints as RE
import qualified Network.HTTP.Headers.SaveData as SD
import qualified Network.HTTP.Headers.SecPurpose as SP
import Test.Syd
import Test.Syd.Hedgehog ()


parseEntire :: ParserT st String a -> ByteString -> Either String a
parseEntire p input = case runParser p input of
  OK value "" -> Right value
  OK _ rest -> Left ("Unconsumed bytes: " <> show rest)
  Fail -> Left "Invalid Structured Field item"
  Err e -> Left e


tests :: Spec
tests = describe "Structured Fields" $ do
  it "checks every byte in the first and subsequent positions of keys and tokens" $ do
    let lower = "abcdefghijklmnopqrstuvwxyz"
        upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        digits = "0123456789"
        keyStart = lower <> "*"
        keyTail = keyStart <> digits <> "_-."
        tokenStart = lower <> upper <> "*"
        tokenTail = tokenStart <> digits <> "!#$%&'+-.^`_|~/:"
        check parser prefix allowed =
          filter (\byte -> isRight (parseEntire parser (prefix <> BS.singleton byte))) [minBound .. maxBound]
            `shouldBe` filter (`BS.elem` allowed) [minBound .. maxBound]
    check structuredFieldKey "" keyStart
    check structuredFieldKey "a" keyTail
    check rfc8941Token "" tokenStart
    check rfc8941Token "a" tokenTail
    check rfc8941Token "AbCdE0123456789" tokenTail
  it "reads and writes RFC Boolean literals" $ do
    parseEntire rfc8941ItemValue "?0" `shouldBe` Right (Boolean False)
    parseEntire rfc8941ItemValue "?1" `shouldBe` Right (Boolean True)
    M.toStrictByteString (R.rfc8941Boolean False) `shouldBe` "?0"
    M.toStrictByteString (R.rfc8941Boolean True) `shouldBe` "?1"
  it "keeps the sign and scale of decimals and emits their shortest decimal form" $ do
    parseEntire rfc8941ItemValue "0.05" `shouldBe` Right (Decimal 0.05)
    parseEntire rfc8941ItemValue "-0.001" `shouldBe` Right (Decimal (-0.001))
    parseEntire rfc8941ItemValue "999999999999.999" `shouldBe` Right (Decimal 999999999999.999)
    M.toStrictByteString (R.rfc8941Decimal (-0.005)) `shouldBe` "-0.005"
    M.toStrictByteString (R.rfc8941Decimal (-1.1)) `shouldBe` "-1.1"
    M.toStrictByteString (R.rfc8941Decimal 12) `shouldBe` "12.0"
  it "round-trips signed decimal thousandths across the specified range" $
    property $ do
      thousandths <- forAll $ Gen.integral (Range.linear (-999999999999999) 999999999999999)
      let value = Decimal (fromRational (toRational (thousandths :: Integer) / 1000))
      parseEntire rfc8941ItemValue (M.toStrictByteString (R.rfc8941ItemValue value)) === Right value
  it "enforces integer and decimal digit limits including leading zeros" $ do
    parseEntire rfc8941Integer "-999999999999999" `shouldBe` Right (-999999999999999)
    map (isLeft . parseEntire rfc8941Integer) ["1000000000000000", "0000000000000000", "1.0"]
      `shouldBe` replicate 3 True
    map (isLeft . parseEntire rfc8941ItemValue) ["-1000000000000000", "1000000000000.0", "1.0000", "1.", ".1"]
      `shouldBe` replicate 5 True
  it "consumes both byte-sequence delimiters and accepts omitted Base64 padding" $ do
    parseEntire rfc8941ItemValue ":aGk=:" `shouldBe` Right (Binary "hi")
    parseEntire rfc8941ItemValue ":aGk:" `shouldBe` Right (Binary "hi")
    parseEntire rfc8941ItemValue "::" `shouldBe` Right (Binary "")
    map (isLeft . parseEntire rfc8941ItemValue) [":aGk=", ":a:", ":aG=k:", ":a Gk=:"]
      `shouldBe` replicate 4 True
    M.toStrictByteString (R.rfc8941ItemValue (Binary "hi")) `shouldBe` ":aGk=:"
  it "copies printable string spans and escapes quotes and backslashes at either edge" $ do
    let body = BS.pack ([34, 92] <> [32 .. 126] <> [92, 34])
        value = String (RFC8941String (ST.fromByteStringUnsafe body))
        escape byte
          | byte == 34 || byte == 92 = BS.pack [92, byte]
          | otherwise = BS.singleton byte
        wire = "\"" <> BS.concatMap escape body <> "\""
    M.toStrictByteString (R.rfc8941ItemValue value) `shouldBe` wire
    parseEntire rfc8941ItemValue wire `shouldBe` Right value
  it "renders compact strings across builder buffer boundaries" $
    flip mapM_ [0, 1, 119, 120, 121, 125, 126, 127, 128, 4096] $ \size -> do
      let bytes = BS.replicate size 97
          value = RFC8941String (ST.fromByteStringUnsafe bytes)
          builder = "prefix=" <> R.rfc8941String value <> ";suffix"
      M.toStrictByteString builder `shouldBe` "prefix=\"" <> bytes <> "\";suffix"
  it "uses comma-space when combining field lines inside a structured string" $ do
    let parseItem = P.parseStructuredFieldLines "test item" P.structuredFieldItem
        plain = P.StructuredFieldItem (String (RFC8941String "foo, bar")) []
        display = P.StructuredFieldItem (DisplayString "foo, bar") []
    parseItem ("\"foo" :| ["bar\""]) `shouldBe` Right plain
    parseItem ("%\"foo" :| ["bar\""]) `shouldBe` Right display
  it "accepts empty Lists after spaces and permits tabs after a member" $ do
    let parseList = parseEntire (P.structuredFieldList P.structuredFieldMemberParser)
        member = P.StructuredItem (P.StructuredFieldItem (Integer 1) [])
    parseList "   " `shouldBe` Right []
    parseList "1\t" `shouldBe` Right [member]
    parseList "1\t,\t1" `shouldBe` Right [member, member]
    map (isLeft . parseList) ["\t", " \t ", "1,\t"] `shouldBe` replicate 3 True
  it "omits empty Lists and Dictionaries while preserving the explicit Accept-CH reset" $ do
    let fields =
          headerMapFromList
            [ ("Permissions-Policy", "camera=()")
            , ("Reporting-Endpoints", "default=\"/reports\"")
            , ("Save-Data", "on")
            , ("Sec-Purpose", "prefetch")
            , ("Accept-CH", "DPR")
            , ("Content-Type", "text/plain")
            ]
        policy = PP.PermissionsPolicy []
        reporting = RE.ReportingEndpoints []
        saveData = SD.SaveDataList []
        purpose = SP.SecPurposeList []
        resetHints = ACH.AcceptCH []
    renderHeaderLines defaultHeaderSettings policy `shouldBe` Nothing
    renderHeaderLines defaultHeaderSettings reporting `shouldBe` Nothing
    renderHeaderLines defaultHeaderSettings saveData `shouldBe` Nothing
    renderHeaderLines defaultHeaderSettings purpose `shouldBe` Nothing
    renderHeaderLines defaultHeaderSettings resetHints `shouldBe` Just ("" :| [])
    (setHeader resetHints . setHeader policy . setHeader reporting . setHeader saveData . setHeader purpose) fields
      `shouldBe` headerMapFromList [("Accept-CH", ""), ("Content-Type", "text/plain")]
  it "clears every prior Cache-Status line with an empty list" $ do
    let rest = [("Content-Type", "text/plain")]
        fields = headerMapFromList ([("Cache-Status", "edge;hit"), ("Cache-Status", "origin;fwd=uri-miss")] <> rest)
    setHeader (CS.CacheStatus []) fields `shouldBe` headerMapFromList rest
  it "clears every prior Proxy-Status line with an empty list" $ do
    let rest = [("Content-Type", "text/plain")]
        fields = headerMapFromList ([("Proxy-Status", "edge"), ("Proxy-Status", "origin;received-status=200")] <> rest)
    setHeader (PS.ProxyStatus []) fields `shouldBe` headerMapFromList rest
  it "retains the field separator inside a Cache-Status extension string" $ do
    let fields = headerMapFromList [("Cache-Status", "edge;label=\"first"), ("Cache-Status", "second\", origin")]
    case lookupHeader @CS.CacheStatus fields of
      Right (Just value) ->
        headerMapToList (setHeader value (headerMapFromList []))
          `shouldBe` [("cache-status", "edge;label=\"first, second\", origin")]
      result -> expectationFailure (show result)
  it "preserves Cache-Status flags, decimal extensions and byte sequences" $ do
    let fields = headerMapFromList [("Cache-Status", "edge;fwd=uri-miss;stored;collapsed=?0;cost=0.05;key-id=:aGk=:"), ("Cache-Status", "origin;hit=?0")]
    case lookupHeader @CS.CacheStatus fields of
      Right (Just value@(CS.CacheStatus [edge, origin])) -> do
        CS.cacheStatusHitOrForward edge `shouldBe` Right (CS.ForwardInfo CS.UriMiss Nothing (Just True) (Just False))
        CS.cacheStatusHitOrForward origin `shouldBe` Left CS.NoHit
        CS.cacheStatusUnknownParameters (CS.cacheStatusCommonFields edge)
          `shouldBe` [("cost", Just (Decimal 0.05)), ("key-id", Just (Binary "hi"))]
        let rendered = setHeader value (headerMapFromList [])
        headerMapToList rendered `shouldBe` [("cache-status", "edge;fwd=uri-miss;stored;collapsed=?0;cost=0.05;key-id=:aGk=:, origin;hit=?0")]
        lookupHeader @CS.CacheStatus rendered `shouldBe` Right (Just value)
      result -> expectationFailure (show result)
  it "accepts a Cache-Status member with no hit or forwarding parameter" $
    case lookupHeader @CS.CacheStatus (headerMapFromList [("cache-status", "edge;ttl=25")]) of
      Right (Just (CS.CacheStatus [edge])) -> CS.cacheStatusHitOrForward edge `shouldBe` Left CS.HitUnspecified
      result -> expectationFailure (show result)
  it "retains forwarding-only parameters for intermediaries that did not forward" $ do
    let fields =
          headerMapFromList
            [ ("Cache-Status", "edge;hit;stored;fwd-status=304;note;collapsed=?0")
            , ("Cache-Status", "origin;stored=?0")
            ]
    case lookupHeader @CS.CacheStatus fields of
      Right (Just value@(CS.CacheStatus [edge, origin])) -> do
        CS.cacheStatusHitOrForward edge `shouldBe` Left CS.Hit
        CS.cacheStatusHitOrForward origin `shouldBe` Left CS.HitUnspecified
        CS.cacheStatusUnknownParameters (CS.cacheStatusCommonFields edge)
          `shouldBe` [("stored", Nothing), ("fwd-status", Just (Integer 304)), ("note", Nothing), ("collapsed", Just (Boolean False))]
        headerMapToList (setHeader value fields)
          `shouldBe` [("cache-status", "edge;hit;stored;fwd-status=304;note;collapsed=?0, origin;stored=?0")]
      result -> expectationFailure (show result)
  it "parses ordered Cache-Status fields and replaces parameters before validating them" $ do
    let fields =
          headerMapFromList
            [ ("Cache-Status", " reverse;hit;ttl=bad;z=@12;x=%\"caf%c3%a9\";ttl=-5;z=@13")
            , ("Cache-Status", "\"shared cache\";fwd=stale;fwd-status=304;stored=?1\t")
            ]
    case lookupHeader @CS.CacheStatus fields of
      Right (Just value@(CS.CacheStatus [reverseProxy, shared])) -> do
        CS.cacheStatusTtl (CS.cacheStatusCommonFields reverseProxy) `shouldBe` Just (-5)
        CS.cacheStatusUnknownParameters (CS.cacheStatusCommonFields reverseProxy)
          `shouldBe` [("z", Just (DateValue 13)), ("x", Just (DisplayString "café"))]
        CS.cacheStatusHitOrForward shared `shouldBe` Right (CS.ForwardInfo CS.Stale (Just 304) (Just True) Nothing)
        either (const Nothing) (Just . CS.effectiveForwardStatus 200) (CS.cacheStatusHitOrForward shared) `shouldBe` Just 304
        headerMapToList (setHeader value (headerMapFromList []))
          `shouldBe` [("cache-status", "reverse;hit;ttl=-5;z=@13;x=%\"caf%c3%a9\", \"shared cache\";fwd=stale;fwd-status=304;stored")]
      result -> expectationFailure (show result)
    headerMapToList (setHeader (CS.CacheStatus []) (headerMapFromList [])) `shouldBe` []
  it "defaults a forwarded member's next-hop status to the enclosing response" $
    case lookupHeader @CS.CacheStatus (headerMapFromList [("cache-status", "edge;fwd=uri-miss")]) of
      Right (Just (CS.CacheStatus [edge])) ->
        fmap (CS.effectiveForwardStatus 206) (CS.cacheStatusHitOrForward edge) `shouldBe` Right 206
      result -> expectationFailure (show result)
  it "accumulates Cache-Status errors for malformed registered parameter values" $
    case lookupHeader @CS.CacheStatus (headerMapFromList [("Cache-Status", "edge;ttl;key;detail, origin;fwd=stale;fwd-status=600")]) of
      Left failures -> length failures `shouldBe` 4
      result -> expectationFailure (show result)
