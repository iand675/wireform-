{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteProtocol (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Grammar.LegacyProtocol
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, lookupHeader, lookupHeaderWith, lookupRawHeader, setHeader, withHeaderDateReference)
import Network.HTTP.Headers.HeaderFieldName (hMethodCheck, hMethodCheckExpires, hProtocol, hProtocolInfo, hProtocolQuery, hProtocolRequest, hProxyFeatures, hProxyInstruction, hRefererRoot, hSecurityScheme)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.MethodCheck as MethodCheck
import qualified Network.HTTP.Headers.MethodCheckExpires as MethodCheckExpires
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.Protocol as Protocol
import qualified Network.HTTP.Headers.ProtocolInfo as ProtocolInfo
import qualified Network.HTTP.Headers.ProtocolQuery as ProtocolQuery
import qualified Network.HTTP.Headers.ProtocolRequest as ProtocolRequest
import qualified Network.HTTP.Headers.ProxyFeatures as ProxyFeatures
import qualified Network.HTTP.Headers.ProxyInstruction as ProxyInstruction
import qualified Network.HTTP.Headers.PublicHeader as PublicHeader
import qualified Network.HTTP.Headers.RefererRoot as RefererRoot
import qualified Network.HTTP.Headers.Safe as Safe
import qualified Network.HTTP.Headers.SecurityScheme as SecurityScheme
import qualified Network.HTTP.Methods as Method
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser and require the leftover to be only optional whitespace.
parseOk :: ParserT st String a -> ByteString -> Either String a
parseOk p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


renderBS :: (a -> M.Builder) -> a -> ByteString
renderBS f = M.toStrictByteString . f


unit_protocol :: Spec
unit_protocol = describe "Protocol" $ do
  it "reads and renders the PICS response declaration" $ do
    let canonical = "{PICS-1.1 {headers PICS-Label}}"
        wire = " \t{pics-1.1 {HeAdErS pics-label }}\t"
    lookupHeader @Protocol.Protocol (headerMapFromList [("Protocol", BS.copy canonical)])
      `shouldBe` Right (Just Protocol.protocolPicsLabels)
    let sliced = BS.take (BS.length canonical) (BS.drop 1 ("x" <> canonical <> "y"))
    lookupHeader @Protocol.Protocol (headerMapFromList [("Protocol", sliced)])
      `shouldBe` Right (Just Protocol.protocolPicsLabels)
    forM_ [0 .. BS.length canonical - 1] $ \offset -> do
      let wireVariant = BS.take offset canonical <> "!" <> BS.drop (offset + 1) canonical
          expected = either (const Nothing) Just (parseOk Protocol.protocolParser wireVariant)
          actual = either (const Nothing) id $
            lookupHeader @Protocol.Protocol (headerMapFromList [("Protocol", wireVariant)])
      actual `shouldBe` expected
    let extension = "{foo {str opt}}"
        fields = headerMapFromList [("Protocol", canonical), ("Protocol", extension)]
    case lookupHeader @Protocol.Protocol fields of
      Right (Just header) -> do
        Protocol.protocolEntries header `shouldBe` Right
          (ProtocolPicsLabels :| [ProtocolExtension (LegacyDirective (LegacyProtocolToken "foo") [LegacyStrength LegacyOptional])])
        lookupRawHeader hProtocol (setHeader header (headerMapFromList []))
          `shouldBe` Just (canonical :| [extension])
      result -> expectationFailure (show result)
    (lookupHeader @Protocol.Protocol (headerMapFromList [("Protocol", wire)]) >>= traverse Protocol.protocolEntries)
      `shouldBe` Right (Just (ProtocolPicsLabels :| []))
    case parseOk Protocol.protocolParser wire of
      Right header -> lookupRawHeader hProtocol (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
      result -> expectationFailure (show result)
    fmap (renderBS Protocol.renderProtocol) (Protocol.protocolFromEntries (ProtocolPicsLabels :| []))
      `shouldBe` Right canonical
  it "reads the older PEP use directive and preserves typed header-name patterns" $ do
    let first = "{http://www.w3.org/UPP {via http://www.SET.org/PEPSpec}}"
        second = "{http://www.SET.org/PEPSpec {str req} {headers Field X-PEP-*} {scope conn}}"
        expected =
          ProtocolExtension (LegacyDirective (LegacyProtocolURI "http://www.w3.org/UPP") [LegacyVia (LegacyProtocolURI "http://www.SET.org/PEPSpec")])
            :| [ ProtocolExtension
                   ( LegacyDirective
                       (LegacyProtocolURI "http://www.SET.org/PEPSpec")
                       [LegacyStrength LegacyRequired, LegacyHeaders [LegacyHeaderName "Field", LegacyHeaderPrefix "X-PEP-"], LegacyScope LegacyConnection]
                   )
               ]
    case lookupHeader @Protocol.Protocol (headerMapFromList [("Protocol", first), ("Protocol", second)]) of
      Right (Just header) -> do
        Protocol.protocolEntries header `shouldBe` Right expected
        lookupRawHeader hProtocol (setHeader header (headerMapFromList [])) `shouldBe` Just (first :| [second])
      result -> expectationFailure (show result)
  it "rejects malformed PICS declarations and invalid strength on PEP use directives" $ do
    forM_ ["", "{PICS-1.1 labels}", "{PICS-1.1 {headers Bad}}", "{PICS-1.1 {headers PICS-Label extra}}", "{foo {str ref}}", "{foo {str opt} {STR req}}", "{foo {scope somewhere}}", "{foo} {bad"] $ \wire ->
      lookupHeader @Protocol.Protocol (headerMapFromList [("Protocol", wire)]) `shouldSatisfy` isLeft


unit_protocol_request :: Spec
unit_protocol_request = describe "Protocol-Request" $ do
  it "reads PICS label options, named extensions and multiple rating services" $ do
    let wire = "\t{PICS-1.1 {params FuLL {probe +test \"a%28test%29\"} {services \"http://www.gcf.org/v2.5\" \"irc://irc.test/room\"}}} "
        expected =
          ProtocolPicsRequest (PicsLabelRequest (Just PicsFull) [PicsToken "probe" :| [PicsToken "+test", PicsQuotedName "a%28test%29"]] ("http://www.gcf.org/v2.5" :| ["irc://irc.test/room"]))
    case lookupHeader @ProtocolRequest.ProtocolRequest (headerMapFromList [("Protocol-Request", wire)]) of
      Right (Just header) -> do
        ProtocolRequest.protocolRequestEntries header `shouldBe` Right (expected :| [])
        lookupRawHeader hProtocolRequest (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
      result -> expectationFailure (show result)
    (ProtocolRequest.protocolRequestFromEntries (expected :| []) >>= ProtocolRequest.protocolRequestEntries)
      `shouldBe` Right (expected :| [])
  it "distinguishes omitted completeness from a future completeness value" $ do
    let readRequest value = lookupHeader @ProtocolRequest.ProtocolRequest (headerMapFromList [("Protocol-Request", value)]) >>= traverse ProtocolRequest.protocolRequestEntries
        entry value = Right (Just (ProtocolPicsRequest (PicsLabelRequest value [] ("http://p.test/" :| [])) :| []))
    readRequest "{PICS-1.1 {params {services \"http://p.test/\"}}}" `shouldBe` entry Nothing
    readRequest "{PICS-1.1 {params Future8 {services \"http://p.test/\"}}}" `shouldBe` entry (Just (PicsOtherCompleteness "Future8"))
    forM_ [("Minimal", PicsMinimal), ("short", PicsShort), ("signed", PicsSigned)] $ \(mode, expected) ->
      readRequest ("{PICS-1.1 {params " <> mode <> " {services \"http://p.test/\"}}}") `shouldBe` entry (Just expected)
  it "rejects incomplete services, invalid PICS values, and via on a request directive" $ do
    forM_
      [ ""
      , "{PICS-1.1 {params}}"
      , "{PICS-1.1 {params bad-x {services \"http://p.test/\"}}}"
      , "{PICS-1.1 {params {services}}}"
      , "{PICS-1.1 {params {services \"relative\"}}}"
      , "{PICS-1.1 {params {services http://p.test/}}}"
      , "{PICS-1.1 {params {services \"http://p.test/%XX\"}}}"
      , "{PICS-1.1 {params {services \"http://p.test/\"} {x}}}"
      , "{PICS-1.1 {params {\"services\" x} {services \"http://p.test/\"}}}"
      , "{PICS-1.1 {params {x \"%G0\"} {services \"http://p.test/\"}}}"
      , "{PICS-1.1 {params {x \"a\\b\"} {services \"http://p.test/\"}}}"
      , "{foo {via bar}}"
      , "{foo {str req}}\r\n"
      ]
      $ \wire -> lookupHeader @ProtocolRequest.ProtocolRequest (headerMapFromList [("Protocol-Request", wire)]) `shouldSatisfy` isLeft
    ProtocolRequest.protocolRequestEntries (ProtocolRequest.ProtocolRequest "invalid") `shouldSatisfy` isLeft
  it "reads the JEPI extension demand" $ do
    let wire = "{http://www.w3.org/UPP {str req} {for /PayButton}}"
        expected = ProtocolExtensionRequest (LegacyDirective (LegacyProtocolURI "http://www.w3.org/UPP") [LegacyStrength LegacyRequired, LegacyFor [LegacyTarget "/PayButton" False]])
    (lookupHeader @ProtocolRequest.ProtocolRequest (headerMapFromList [("Protocol-Request", wire)]) >>= traverse ProtocolRequest.protocolRequestEntries)
      `shouldBe` Right (Just (expected :| []))


unit_protocol_info :: Spec
unit_protocol_info = describe "Protocol-Info" $ do
  it "reads capabilities and nested parameters in the JEPI example's wire order" $ do
    let first = "{http://www.w3.org/UPP {for /PaymentPages/*}} {http://www.SET.org/PEPSpec {params {upp {instrument-brand VISA}}} {for /PaymentPages/*}}"
        second = ", {legacy {str ref} {via alt} {vendor \"x,\\\"y\"}}"
        expected =
          LegacyDirective (LegacyProtocolURI "http://www.w3.org/UPP") [LegacyFor [LegacyTarget "/PaymentPages/" True]]
            :| [ LegacyDirective
                   (LegacyProtocolURI "http://www.SET.org/PEPSpec")
                   [LegacyParameters [LegacyBag (LegacyProtocolToken "upp") [LegacyBag (LegacyProtocolToken "instrument-brand") [LegacyToken "VISA"]]], LegacyFor [LegacyTarget "/PaymentPages/" True]]
               , LegacyDirective (LegacyProtocolToken "legacy") [LegacyStrength LegacyRefused, LegacyVia (LegacyProtocolToken "alt"), LegacyExtension (LegacyBag (LegacyProtocolToken "vendor") [LegacyQuoted "x,\"y"])]
               ]
    case lookupHeader @ProtocolInfo.ProtocolInfo (headerMapFromList [("Protocol-Info", first), ("Protocol-Info", second)]) of
      Right (Just header) -> do
        ProtocolInfo.protocolInfoEntries header `shouldBe` Right expected
        lookupRawHeader hProtocolInfo (setHeader header (headerMapFromList [])) `shouldBe` Just (first :| [second])
      result -> expectationFailure (show result)
    (ProtocolInfo.protocolInfoFromEntries expected >>= ProtocolInfo.protocolInfoEntries) `shouldBe` Right expected


unit_protocol_query :: Spec
unit_protocol_query = describe "Protocol-Query" $ do
  it "reads unquoted relative URI wildcards, and keeps empty realm lists" $ do
    let wire = " {http://www.w3.org/UPP {for /PaymentPages/* ../x?foo=bar /literal%2A} {str opt}}, {alt {for} {scope origin}}  "
        expected =
          LegacyDirective
            (LegacyProtocolURI "http://www.w3.org/UPP")
            [ LegacyFor [LegacyTarget "/PaymentPages/" True, LegacyTarget "../x?foo=bar" False, LegacyTarget "/literal%2A" False]
            , LegacyStrength LegacyOptional
            ]
            :| [LegacyDirective (LegacyProtocolToken "alt") [LegacyFor [], LegacyScope LegacyOrigin]]
    case lookupHeader @ProtocolQuery.ProtocolQuery (headerMapFromList [("Protocol-Query", wire)]) of
      Right (Just header) -> do
        ProtocolQuery.protocolQueryEntries header `shouldBe` Right expected
        lookupRawHeader hProtocolQuery (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
      result -> expectationFailure (show result)
  it "rejects misplaced reserved attributes, malformed URLs and incomplete list members" $ do
    forM_
      [ ""
      , ",,"
      , "{http://[broken]/ {scope origin}}"
      , "{foo {for /a%XG}}"
      , "{foo {str req}}"
      , "{foo {str ref}}"
      , "{foo {via bar}}"
      , "{foo {params}{params}}"
      , "{foo {scope conn trailing}}"
      , "{foo {params {nested \"incomplete}}}"
      , "{foo}junk"
      , "{foo}{bar}"
      , "{foo}\r\n"
      ]
      $ \wire -> lookupHeader @ProtocolQuery.ProtocolQuery (headerMapFromList [("Protocol-Query", wire)]) `shouldSatisfy` isLeft
    lookupHeader @ProtocolQuery.ProtocolQuery (headerMapFromList [("Protocol-Query", "{foo}"), ("Protocol-Query", "junk")]) `shouldSatisfy` isLeft
    ProtocolQuery.protocolQueryFromEntries (LegacyDirective (LegacyProtocolToken "foo") [LegacyVia (LegacyProtocolToken "bar")] :| []) `shouldSatisfy` isLeft
  it "round-trips nested extension items and opaque quoted octets" $ property $ do
    octets <- forAll (BS.pack <$> Gen.list (Range.linear 0 48) (Gen.element (9 : [32 .. 126] <> [128 .. 255])))
    let entries = LegacyDirective (LegacyProtocolURI "http://p.test/ext") [LegacyParameters [LegacyBag (LegacyProtocolToken "child") [LegacyQuoted octets]]] :| []
    (ProtocolQuery.protocolQueryFromEntries entries >>= ProtocolQuery.protocolQueryEntries) === Right entries
    (ProtocolInfo.protocolInfoFromEntries entries >>= ProtocolInfo.protocolInfoEntries) === Right entries


unit_method_check :: Spec
unit_method_check = describe "Method-Check" $ do
  it "accepts one case-sensitive HTTP method with extension methods" $ do
    let check wire = lookupHeader @MethodCheck.MethodCheck (headerMapFromList [("Method-Check", wire)])
        expectMethod wire = fmap (traverse MethodCheck.methodCheckMethod) (check wire)
    expectMethod " \tPATCH\t " `shouldBe` Right (Right (Just Method.PATCH))
    expectMethod "patch" `shouldBe` Right (Just <$> Method.mkMethod "patch")
    expectMethod "X-VERIFY" `shouldBe` Right (Just <$> Method.mkMethod "X-VERIFY")
    parseOk MethodCheck.methodCheckParser "QUERY" `shouldBe` MethodCheck.methodCheckFromMethod Method.QUERY
    case MethodCheck.methodCheckFromMethod Method.PATCH of
      Right header -> lookupRawHeader hMethodCheck (setHeader header (headerMapFromList [])) `shouldBe` Just ("PATCH" :| [])
      Left failure -> expectationFailure failure
  it "rejects lists, spaces, malformed method bytes and duplicate lines" $ do
    forM_ ["", "(GET PUT DELETE)", "GET, PATCH", "GE T", "GE/T", "GET\255", "GET\r\n"] $ \wire ->
      lookupHeader @MethodCheck.MethodCheck (headerMapFromList [("Method-Check", wire)]) `shouldSatisfy` isLeft
    lookupHeader @MethodCheck.MethodCheck (headerMapFromList [("Method-Check", "GET"), ("Method-Check", "PUT")])
      `shouldSatisfy` isLeft


unit_method_check_expires :: Spec
unit_method_check_expires = describe "Method-Check-Expires" $ do
  let stamp = UTCTime (fromGregorian 2015 10 21) (secondsToDiffTime 26880)
      canonical = "Wed, 21 Oct 2015 07:28:00 GMT"
      check wire = lookupHeader @MethodCheckExpires.MethodCheckExpires (headerMapFromList [("Method-Check-Expires", wire)])
  it "reads all HTTP-date forms and preserves validated wire bytes" $ do
    forM_ [canonical, "Wednesday, 21-Oct-15 07:28:00 GMT", "Wed Oct 21 07:28:00 2015"] $ \wire -> do
      case check wire of
        Right (Just header) -> do
          MethodCheckExpires.methodCheckExpiresTime header `shouldBe` Right stamp
          lookupRawHeader hMethodCheckExpires (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
        result -> expectationFailure (show result)
    let authored = MethodCheckExpires.methodCheckExpiresFromTime (addUTCTime 0.9 stamp)
    MethodCheckExpires.methodCheckExpiresTime authored `shouldBe` Right stamp
    M.toStrictByteString (MethodCheckExpires.renderMethodCheckExpires authored) `shouldBe` canonical
  it "uses the reference clock for RFC 850 dates and rejects malformed or repeated fields" $ do
    let now = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 5)
        target = UTCTime (fromGregorian 2070 1 1) (secondsToDiffTime 5)
        settings = withHeaderDateReference now defaultHeaderSettings
        field = headerMapFromList [("Method-Check-Expires", "\t Wednesday, 01-Jan-70 00:00:05 GMT ")]
    (lookupHeaderWith @MethodCheckExpires.MethodCheckExpires settings field >>= traverse MethodCheckExpires.methodCheckExpiresTime)
      `shouldBe` Right (Just target)
    forM_ ["0", "3600", "Wed, 31 Feb 2015 07:28:00 GMT", canonical <> ", " <> canonical, canonical <> "\r\n"] $ \wire ->
      check wire `shouldSatisfy` isLeft
    lookupHeader @MethodCheckExpires.MethodCheckExpires (headerMapFromList [("Method-Check-Expires", canonical), ("Method-Check-Expires", canonical)])
      `shouldSatisfy` isLeft


unit_proxy_features :: Spec
unit_proxy_features = describe "Proxy-Features" $ do
  let readFeatures wire = lookupHeader @ProxyFeatures.ProxyFeatures (headerMapFromList [("Proxy-Features", wire)])
      details wire = readFeatures wire >>= traverse ProxyFeatures.proxyFeaturesDetails
      offer uri options = ProxyFeatures.ProxyLog (ProxyFeatures.ProxyLogOffer uri options)
      expiry = ProxyFeatures.ProxyLogExpiryHours
      expected name options = Right (Just (ProxyFeatures.ProxyFeaturesDetails name options))
  it "reads the draft example and retains spelling through the typed header map" $ do
    let uri = "http://proxy.w3.org/logs/xxxxx.org"
        wire = "\t proxy.w3.org; log=" <> uri <> "; expiry=24 \t"
    details wire `shouldBe` expected "proxy.w3.org" (offer uri (expiry 24 :| []) :| [])
    case readFeatures wire of
      Right (Just header) -> do
        renderBS ProxyFeatures.renderProxyFeatures header `shouldBe` wire
        lookupRawHeader hProxyFeatures (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
        ProxyFeatures.proxyFeaturesValue header `shouldBe` ProxyFeatures.proxyFeaturesValue (ProxyFeatures.ProxyFeatures "\t proxy.w3.org; log=http://proxy.w3.org/logs/xxxxx.org; expiry=24 \t")
      result -> expectationFailure (show result)
  it "reads DNS names, historical logging spellings, and options in wire order" $ do
    details "Proxy.TEST.; NotIfY; LOGING"
      `shouldBe` expected "Proxy.TEST." (ProxyFeatures.ProxyNotify :| [ProxyFeatures.ProxyLogging])
    let uri = "https://proxy.test/a;mode=fast;notify/path%3Btag"
        manyHours = 18446744073709551616
    details ("p; log=" <> uri <> "; expires=18446744073709551616; notify")
      `shouldBe` expected "p" (offer uri (expiry manyHours :| []) :| [ProxyFeatures.ProxyNotify])
    let longName = BS.intercalate "." (replicate 3 (BS.replicate 63 97) <> [BS.replicate 61 97])
    forM_ [longName, longName <> "."] $ \name ->
      details (name <> "; notify") `shouldBe` expected name (ProxyFeatures.ProxyNotify :| [])
    readFeatures (longName <> "a; notify") `shouldSatisfy` isLeft
    readFeatures (BS.replicate 64 97 <> "; notify") `shouldSatisfy` isLeft
  it "round-trips checked authoring for every generated 24-bit mask" $ property $ do
    mask <- forAll (Gen.word32 (Range.linear 0 0xffffff))
    hours <- forAll (Gen.word64 (Range.linear 0 maxBound))
    case ProxyFeatures.mkProxyPeakHours mask of
      Nothing -> Just mask === Nothing
      Just peak -> do
        ProxyFeatures.proxyPeakHoursMask peak === mask
        let authored =
              ProxyFeatures.ProxyFeaturesDetails
                "proxy.test"
                (offer "https://proxy.test/sink" (expiry (fromIntegral hours) :| [ProxyFeatures.ProxyLogPeakHours peak]) :| [])
        (ProxyFeatures.proxyFeaturesFromDetails authored >>= ProxyFeatures.proxyFeaturesDetails) === Right authored
  it "checks authoring, renders canonical directives, and accepts the maximum peak mask" $ do
    ProxyFeatures.mkProxyPeakHours 0x1000000 `shouldBe` Nothing
    case ProxyFeatures.mkProxyPeakHours 0xffffff of
      Just peak -> do
        let uri = "https://p.test/sink"
            wire = "p; log=https://p.test/sink; expires=24; peak=ffffff"
            options = expiry 24 :| [ProxyFeatures.ProxyLogPeakHours peak]
            authored = ProxyFeatures.ProxyFeaturesDetails "p" (offer uri options :| [])
        details "p; Log=https://p.test/sink; expiry=24; peak=FfFfFf"
          `shouldBe` Right (Just authored)
        fmap (renderBS ProxyFeatures.renderProxyFeatures) (ProxyFeatures.proxyFeaturesFromDetails authored) `shouldBe` Right wire
        ProxyFeatures.proxyFeaturesFromDetails (ProxyFeatures.ProxyFeaturesDetails "p" (offer uri options :| [ProxyFeatures.ProxyLogging]))
          `shouldSatisfy` isLeft
      Nothing -> expectationFailure "expected a valid 24-bit peak-hours mask"
  it "rejects malformed directives, DNS names, log URIs and repeated field lines" $ do
    forM_
      [ ""
      , "proxy.test"
      , "proxy.test;"
      , "proxy.test; summary"
      , "proxy.test; notify, logging"
      , "p; notify;"
      , "p; logging=x"
      , ".; notify"
      , ".p; notify"
      , "p..test; notify"
      , "-p; notify"
      , "p-.test; notify"
      , "p-.; notify"
      , "p_:80; notify"
      , "p\255; notify"
      , "p; notify\r\n"
      , "p; log=http://example.test"
      , "p; log=/relative; expiry=24"
      , "p; log=https://p.test/%Q0; expiry=24"
      , "p; log=https://p.test/#frag; expiry=24"
      , "p; log=https://p.test/; notify"
      , "p; log=https://p.test/; expiry="
      , "p; log=https://p.test/; expiry=-24"
      , "p; log=https://p.test/; expiry=+24"
      , "p; log=https://p.test/; peak=fffff"
      , "p; log=https://p.test/; peak=fffffff"
      , "p; log=https://p.test/; peak=fffgff"
      , "p; log=https://p.test/; expiry=24; logging"
      , "p; loging; log=https://p.test/; expiry=24"
      ]
      $ \wire -> readFeatures wire `shouldSatisfy` isLeft
    lookupHeader @ProxyFeatures.ProxyFeatures (headerMapFromList [("Proxy-Features", "p; notify"), ("Proxy-Features", "q; logging")])
      `shouldSatisfy` isLeft


unit_proxy_instruction :: Spec
unit_proxy_instruction = describe "Proxy-Instruction" $ do
  let readInstruction wire = lookupHeader @ProxyInstruction.ProxyInstruction (headerMapFromList [("Proxy-Instruction", wire)])
  it "reads every command and preserves original case and whitespace" $
    forM_ [("LoG", ProxyInstruction.ProxyCommandLog), ("summary", ProxyInstruction.ProxyCommandSummary), ("INFORM", ProxyInstruction.ProxyCommandInform)] $
      \(token, command) -> do
        let wire = " \tProxy.TEST.;\t" <> token <> "\t "
            details = ProxyInstruction.ProxyInstructionDetails "Proxy.TEST." command
        case readInstruction wire of
          Right (Just header) -> do
            ProxyInstruction.proxyInstructionDetails header `shouldBe` Right details
            renderBS ProxyInstruction.renderProxyInstruction header `shouldBe` wire
            lookupRawHeader hProxyInstruction (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
          result -> expectationFailure (show result)
  it "authors commands and rejects malformed, multiple, or repeated instructions" $ do
    let authored = ProxyInstruction.ProxyInstructionDetails "proxy.w3.org" ProxyInstruction.ProxyCommandSummary
    fmap (renderBS ProxyInstruction.renderProxyInstruction) (ProxyInstruction.proxyInstructionFromDetails authored)
      `shouldBe` Right "proxy.w3.org; summary"
    ProxyInstruction.proxyInstructionFromDetails (ProxyInstruction.ProxyInstructionDetails "bad/name" ProxyInstruction.ProxyCommandLog)
      `shouldSatisfy` isLeft
    forM_ ["", "p", "p; notify", "p; log; inform", "p; log, q; inform", "p; log=x", "p; summary/", "p; log\r\n", "-p; log"] $ \wire ->
      readInstruction wire `shouldSatisfy` isLeft
    lookupHeader @ProxyInstruction.ProxyInstruction (headerMapFromList [("Proxy-Instruction", "p; log"), ("Proxy-Instruction", "p; inform")])
      `shouldSatisfy` isLeft


unit_security_scheme :: Spec
unit_security_scheme = describe "Security-Scheme" $ do
  it "reads the S-HTTP/1.4 literal and retains accepted case" $ do
    parseOk SecurityScheme.securitySchemeParser "s-hTtp/1.4"
      `shouldBe` Right (SecurityScheme.SecurityScheme "s-hTtp/1.4")
    renderBS SecurityScheme.renderSecurityScheme SecurityScheme.secureHTTP14
      `shouldBe` "S-HTTP/1.4"
    lookupHeader @SecurityScheme.SecurityScheme (headerMapFromList [("Security-Scheme", " \ts-hTtp/1.4 ")])
      `shouldBe` Right (Just (SecurityScheme.SecurityScheme "s-hTtp/1.4"))
    lookupRawHeader hSecurityScheme (setHeader SecurityScheme.secureHTTP14 (headerMapFromList []))
      `shouldBe` Just ("S-HTTP/1.4" :| [])
  it "rejects different versions, suffixes, and repeated lines" $ do
    forM_ ["S-HTTP/1.3", "S-HTTP/1.40", "HTTPS/1.4", "S-HTTP /1.4", "S-HTTP/1.4, S-HTTP/1.4", "S-HTTP/1.4\r\n"] $ \wire ->
      lookupHeader @SecurityScheme.SecurityScheme (headerMapFromList [("Security-Scheme", wire)]) `shouldSatisfy` isLeft
    lookupHeader @SecurityScheme.SecurityScheme (headerMapFromList [("Security-Scheme", "S-HTTP/1.4"), ("Security-Scheme", "S-HTTP/1.4")])
      `shouldSatisfy` isLeft


unit_referer_root :: Spec
unit_referer_root = describe "Referer-Root" $ do
  let readRoot wire = lookupHeader @RefererRoot.RefererRoot (headerMapFromList [("Referer-Root", wire)])
  it "parses an explicit port, IPv6, and null and preserves the source spelling" $ do
    let wire = "\t hTTps://[2001:db8::1]:00443 "
    (readRoot wire >>= traverse RefererRoot.refererRootOrigin)
      `shouldBe` Right (Just (RefererRoot.RefererRootHost "hTTps" "[2001:db8::1]" 443))
    (readRoot "null" >>= traverse RefererRoot.refererRootOrigin)
      `shouldBe` Right (Just RefererRoot.RefererRootNull)
    (readRoot "null://example.test:0" >>= traverse RefererRoot.refererRootOrigin)
      `shouldBe` Right (Just (RefererRoot.RefererRootHost "null" "example.test" 0))
    case readRoot wire of
      Right (Just header) -> lookupRawHeader hRefererRoot (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
      result -> expectationFailure (show result)
    let origin = RefererRoot.RefererRootHost "https" "example.test" 443
    fmap (renderBS RefererRoot.renderRefererRoot) (RefererRoot.refererRootFromOrigin origin)
      `shouldBe` Right "https://example.test:443"
  it "rejects missing ports, non-root URI components and duplicate lines" $ do
    forM_ ["", "NULL", "https://example.test", "https://example.test.:443", "https://example.test:", "https://example.test:65536", "https://example.test:443/", "https://example.test:443?q=x", "https://example.test:443#a", "https://user@example.test:443", "https://[2001:db8::1:443", "null, null"] $ \wire ->
      readRoot wire `shouldSatisfy` isLeft
    lookupHeader @RefererRoot.RefererRoot (headerMapFromList [("Referer-Root", "null"), ("Referer-Root", "null")])
      `shouldSatisfy` isLeft
    RefererRoot.refererRootFromOrigin (RefererRoot.RefererRootHost "https" "example.test/path" 443) `shouldSatisfy` isLeft


-- Structured: Public ---------------------------------------------------------

unit_public :: Spec
unit_public = it "Public parses a comma-separated method list" $
  case parseOk PublicHeader.publicHeaderParser "OPTIONS, GET, HEAD" of
    Right v ->
      NE.toList (PublicHeader.publicMethods v)
        `shouldBe` map ST.fromString ["OPTIONS", "GET", "HEAD"]
    other -> error (show other)


unit_render_public :: Spec
unit_render_public =
  it "Public renders a comma-separated method list" $
    renderBS
      PublicHeader.renderPublicHeader
      (PublicHeader.PublicHeader (ST.fromString "GET" :| [ST.fromString "HEAD"]))
      `shouldBe` "GET, HEAD"


unit_public_field_lines :: Spec
unit_public_field_lines = it "Public combines method lists across lines and skips empty members" $ do
  let field = PublicHeader.PublicHeader ("OPTIONS" :| ["MGET", "mhead", "GET-BULK", "HEAD"])
      readPublic = lookupHeader @PublicHeader.PublicHeader . headerMapFromList . map (\wire -> ("Public", wire))
  readPublic [",\t", "OPTIONS, , MGET,", "mhead, GET-BULK, HEAD"] `shouldBe` Right (Just field)
  readPublic ["OPTIONS, MGET, mhead, GET-BULK, HEAD"] `shouldBe` Right (Just field)
  lookupRawHeader "Public" (setHeader field (headerMapFromList [("Public", "PATCH"), ("Public", "DELETE")]))
    `shouldBe` Just ("OPTIONS, MGET, mhead, GET-BULK, HEAD" :| [])
  forM_ [[", ", "\t"], ["GET", "HEAD /"], ["GET", "\"HEAD\""], ["GET", "HEAD\xff"]] $ \lines' ->
    readPublic lines' `shouldSatisfy` isLeft


genMethod :: Gen ST.ShortText
genMethod = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element ['A' .. 'Z'])


genPublic :: Gen PublicHeader.PublicHeader
genPublic = PublicHeader.PublicHeader <$> Gen.nonEmpty (Range.linear 1 5) genMethod


prop_public :: Property
prop_public = property $ do
  v <- forAll genPublic
  let bs = renderBS PublicHeader.renderPublicHeader v
  case parseOk PublicHeader.publicHeaderParser bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


-- Structured: Safe -----------------------------------------------------------

unit_safe :: Spec
unit_safe = it "Safe parses yes/no case-insensitively" $ do
  parseOk Safe.safeParser "yes" `shouldBe` Right Safe.SafeYes
  parseOk Safe.safeParser "NO" `shouldBe` Right Safe.SafeNo


unit_render_safe :: Spec
unit_render_safe = it "Safe renders canonical lowercase" $ do
  renderBS Safe.renderSafe Safe.SafeYes `shouldBe` "yes"
  renderBS Safe.renderSafe Safe.SafeNo `shouldBe` "no"


unit_safe_field_lines :: Spec
unit_safe_field_lines = it "Safe reads one complete flag or conditional token" $ do
  let readSafe = lookupHeader @Safe.Safe . headerMapFromList . map (\wire -> ("Safe", wire))
      condition = Safe.SafeIf "User-Awake"
  forM_ ["yes", "yeS", "yEs", "yES", "Yes", "YeS", "YEs", "YES"] $ \wire -> do
    readSafe [wire] `shouldBe` Right (Just Safe.SafeYes)
    readSafe [" \t" <> wire <> "\t"] `shouldBe` Right (Just Safe.SafeYes)
  forM_ ["no", "nO", "No", "NO"] $ \wire ->
    readSafe [wire] `shouldBe` Right (Just Safe.SafeNo)
  readSafe ["\tIF-User-Awake "] `shouldBe` Right (Just condition)
  lookupRawHeader "Safe" (setHeader condition (headerMapFromList [])) `shouldBe` Just ("if-User-Awake" :| [])
  forM_ [["yes", "yes"], ["yes", "no"], ["yes, no"], ["nope"], ["yes-"], ["if-"], ["if-awake, sleep"], ["yes\n"], ["if-\xff"]] $ \fields ->
    readSafe fields `shouldSatisfy` isLeft


genSafe :: Gen Safe.Safe
genSafe = Gen.choice [pure Safe.SafeYes, pure Safe.SafeNo, Safe.SafeIf <$> genMethod]


prop_safe :: Property
prop_safe = property $ do
  v <- forAll genSafe
  let bs = renderBS Safe.renderSafe v
  case parseOk Safe.safeParser bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


tests :: Spec
tests =
  describe "ObsoleteProtocol" $
    sequence_
      [ unit_protocol
      , unit_protocol_request
      , unit_protocol_info
      , unit_protocol_query
      , unit_method_check
      , unit_method_check_expires
      , unit_proxy_features
      , unit_proxy_instruction
      , unit_security_scheme
      , unit_referer_root
      , unit_public
      , unit_render_public
      , unit_public_field_lines
      , it "Public round-trips" prop_public
      , unit_safe
      , unit_render_safe
      , unit_safe_field_lines
      , it "Safe round-trips" prop_safe
      ]
