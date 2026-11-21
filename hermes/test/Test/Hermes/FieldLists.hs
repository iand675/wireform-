module Test.Hermes.FieldLists (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers
import Network.HTTP.Headers.AcceptRanges (AcceptRanges (..))
import Network.HTTP.Headers.Allow (Allow (..), HttpMethod (..))
import Network.HTTP.Headers.Connection (Connection (..))
import Network.HTTP.Headers.HeaderFieldName (hAllow, hVary, unsafeUnknownHeaderFromBytestring)
import Network.HTTP.Headers.KeepAlive (KeepAlive (..), KeepAliveParameter (..), keepAliveFromParameters, keepAliveMaxRequests, keepAliveParameters, keepAliveTimeout)
import Network.HTTP.Headers.Trailer (Trailer (..), isForbiddenTrailerName)
import Network.HTTP.Headers.Upgrade (Protocol (..), Upgrade (..))
import Network.HTTP.Headers.Vary (Vary (..), VaryDirective (..))
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests = describe "HTTP field lists" $ do
  it "preserves Allow methods across field lines and recognizes whole tokens" $ do
    let headers = headerMapFromList [("Allow", " , GET, GET-BULK\t, "), ("Allow", "get,, PATCH")]
        methods = Allow [GET, CustomMethod "GET-BULK", CustomMethod "get", PATCH]
    lookupHeader headers `shouldBe` Right (Just methods)
    lookupRawHeader hAllow (setHeader methods headers) `shouldBe` Just ("GET, GET-BULK, get, PATCH" :| [])
    lookupHeader @Allow (headerMapFromList [("Allow", "GET"), ("Allow", "PATCH bad")])
      `shouldSatisfy` isLeft
  it "distinguishes an empty Allow field from absence" $ do
    lookupHeader @Allow (headerMapFromList []) `shouldBe` Right Nothing
    lookupHeader @Allow (headerMapFromList [("Allow", " , ,\t")]) `shouldBe` Right (Just (Allow []))
    lookupRawHeader hAllow (setHeader (Allow []) (headerMapFromList [])) `shouldBe` Just ("" :| [])
  it "rejects malformed authored Allow methods" $ do
    let original = headerMapFromList [("Allow", "GET"), ("Allow", "PATCH")]
    forM_ ["", "two words", "GET,POST", "GÉT"] $ \method -> do
      let value = Allow [GET, CustomMethod method]
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
  it "agrees with the method sequence for arbitrary field line groupings" $ property $ do
    fields <-
      forAll $
        Gen.list (Range.linear 1 5) $
          Gen.list (Range.linear 0 5) $
            Gen.element [("GET", GET), ("HEAD", HEAD), ("PATCH-SAFE", CustomMethod "PATCH-SAFE"), ("Post", CustomMethod "Post")]
    let toField members = ("Allow", ", " <> BS.intercalate "\t, ," (map fst members) <> ", ")
        headers = headerMapFromList (map toField fields)
    lookupHeader headers === Right (Just (Allow (concatMap (map snd) fields)))
  it "combines Vary fields case-insensitively and recognizes a bare wildcard" $ do
    let headers = headerMapFromList [("Vary", " Accept-Encoding, ,"), ("Vary", "*cache, *, X-Locale")]
        vary = Vary (VaryHeader "accept-encoding" :| [VaryHeader "*cache", VaryAll, VaryHeader "x-locale"])
    lookupHeader headers `shouldBe` Right (Just vary)
    lookupRawHeader hVary (setHeader vary headers) `shouldBe` Just ("accept-encoding, *cache, *, x-locale" :| [])
    lookupHeader @Vary (headerMapFromList [("Vary", " , ")]) `shouldBe` Right (Just VaryEmpty)
    lookupHeader @Vary (headerMapFromList [("Vary", "accept-language, accept-language-ext, origin")])
      `shouldBe` Right (Just (Vary (VaryHeader "accept-language" :| [VaryHeader "accept-language-ext", VaryHeader "origin"])))
    lookupHeader @Vary (headerMapFromList [("Vary", "Accept, X Bad")]) `shouldSatisfy` isLeft
  it "rejects malformed authored Vary field names" $ do
    let original = headerMapFromList [("Vary", "accept"), ("Vary", "origin")]
    forM_ ["", "two words", "accept,origin", "x-\xff"] $ \name -> do
      let value = Vary (VaryHeader "accept" :| [VaryHeader (unsafeUnknownHeaderFromBytestring name)])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
  it "keeps Connection options in order and accepts an empty list" $ do
    let headers = headerMapFromList [("Connection", " keep-alive ,,"), ("Connection", " X-Hop, close")]
        value = Connection ("keep-alive" :| ["X-Hop", "close"])
    lookupHeader headers `shouldBe` Right (Just value)
    renderToHeaders defaultHeaderSettings value `shouldBe` "keep-alive, X-Hop, close"
    lookupHeader @Connection (headerMapFromList [("Connection", " , ")]) `shouldBe` Right (Just ConnectionEmpty)
    lookupHeader @Connection (headerMapFromList [("Connection", "close"), ("Connection", "bad=option")])
      `shouldSatisfy` isLeft
  it "rejects malformed authored Connection options" $ do
    let original = headerMapFromList [("Connection", "keep-alive"), ("Connection", "close")]
    forM_ ["" :| [], "two words" :| [], "keep-alive" :| ["bad=option"], "close" :| ["upgrade,foo"]] $ \options -> do
      let value = Connection options
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
  it "keeps Accept-Ranges units across field lines and reserves none" $ do
    let units = AcceptRangesUnits ("bytes" :| ["Blocks", "rows"])
        input = headerMapFromList [("Accept-Ranges", "bytes"), ("Accept-Ranges", "Blocks, rows")]
    lookupHeader @AcceptRanges input `shouldBe` Right (Just units)
    lookupRawHeader "Accept-Ranges" (setHeader units input) `shouldBe` Just ("bytes, Blocks, rows" :| [])
    lookupHeader @AcceptRanges (headerMapFromList [("Accept-Ranges", ", bytes, "), ("Accept-Ranges", " rows")])
      `shouldBe` Right (Just (AcceptRangesUnits ("bytes" :| ["rows"])))
    lookupHeader @AcceptRanges (headerMapFromList [("Accept-Ranges", " NoNe ")])
      `shouldBe` Right (Just AcceptRangesNone)
    lookupHeader @AcceptRanges (headerMapFromList [("Accept-Ranges", "bytes"), ("Accept-Ranges", "none")])
      `shouldSatisfy` isLeft
    lookupHeader @AcceptRanges (headerMapFromList [("Accept-Ranges", ", ,")]) `shouldSatisfy` isLeft
  it "rejects authored invalid or mixed Accept-Ranges units" $ do
    let original = headerMapFromList [("Accept-Ranges", "bytes"), ("Accept-Ranges", "rows")]
    forM_ ["" :| [], "two words" :| [], "bytes,rows" :| [], "bytes" :| ["NoNe"], "NONE" :| ["rows"]] $ \units -> do
      let value = AcceptRangesUnits units
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
    renderHeaderLines defaultHeaderSettings AcceptRangesNone `shouldBe` Just ("none" :| [])
  it "retains and escapes quoted Keep-Alive parameters" $ do
    let headers = headerMapFromList [("Keep-Alive", "timeout=10,,"), ("Keep-Alive", "max=20, note=\"idle, \\\"warm\\\"\"")]
        value = KeepAlive [("timeout", "10"), ("max", "20"), ("note", "idle, \"warm\"")]
    lookupHeader headers `shouldBe` Right (Just value)
    renderToHeaders defaultHeaderSettings value `shouldBe` "timeout=10, max=20, note=\"idle, \\\"warm\\\"\""
    lookupHeader @KeepAlive (headerMapFromList [("Keep-Alive", "timeout=")]) `shouldSatisfy` isLeft
  it "combines Keep-Alive fields before unquoting HTTP parameter octets" $ do
    let headers = headerMapFromList [("Keep-Alive", ", TIMEOUT=10, note=\"idle"), ("Keep-Alive", "\xff \\x\" ,,")]
        value = KeepAlive [("TIMEOUT", "10"), ("note", "idle, \xff x")]
    lookupHeader headers `shouldBe` Right (Just value)
    renderToHeaders defaultHeaderSettings value `shouldBe` "TIMEOUT=10, note=\"idle, \xff x\""
  it "accepts a flag extension in Keep-Alive" $ do
    let wire = "timeout=10, experimental, empty=\"\", max=20"
        headers = headerMapFromList [("Keep-Alive", wire)]
        parameters =
          [ KeepAliveParameter "timeout" (Just "10")
          , KeepAliveParameter "experimental" Nothing
          , KeepAliveParameter "empty" (Just "")
          , KeepAliveParameter "max" (Just "20")
          ]
        value = keepAliveFromParameters parameters
    lookupHeader @KeepAlive headers `shouldBe` Right (Just value)
    keepAliveParameters value `shouldBe` parameters
    keepAliveParams value `shouldBe` [("timeout", "10"), ("empty", ""), ("max", "20")]
    headerMapToList (setHeader value headers) `shouldBe` [("keep-alive", wire)]
  it "reads bounded Keep-Alive limits and reports malformed or repeated hints" $ do
    let value = KeepAlive [("TIMEOUT", "0010"), ("MaX", "18446744073709551615")]
    keepAliveTimeout value `shouldBe` Right (Just 10)
    keepAliveMaxRequests value `shouldBe` Right (Just maxBound)
    keepAliveTimeout (KeepAlive []) `shouldBe` Right Nothing
    keepAliveTimeout (keepAliveFromParameters [KeepAliveParameter "timeout" Nothing]) `shouldSatisfy` isLeft
    keepAliveTimeout (KeepAlive [("timeout", "1"), ("TIMEOUT", "2")]) `shouldSatisfy` isLeft
    keepAliveTimeout (keepAliveFromParameters [KeepAliveParameter "timeout" Nothing, KeepAliveParameter "TIMEOUT" (Just "2")])
      `shouldBe` Left "Repeated Keep-Alive limit"
    mapM_ (\wire -> keepAliveMaxRequests (KeepAlive [("max", wire)]) `shouldSatisfy` isLeft)
      ["", "18446744073709551616", "-1", "+1", "1 ", "1x"]
  it "rejects malformed authored Keep-Alive names and values" $ do
    let fields = headerMapFromList [("Keep-Alive", "timeout=10"), ("Keep-Alive", "max=20")]
        keep entry = keepAliveFromParameters [KeepAliveParameter "timeout" (Just "10"), entry]
    forM_ [KeepAliveParameter "" Nothing, KeepAliveParameter "max=20" Nothing, KeepAliveParameter "notice" (Just "a\DELb")] $ \entry -> do
      renderHeaderLines defaultHeaderSettings (keep entry) `shouldBe` Nothing
      setHeader (keep entry) fields `shouldBe` fields
    renderHeaderLines defaultHeaderSettings (KeepAlive []) `shouldBe` Just ("" :| [])
  it "combines Trailer field names while skipping empty members" $ do
    let value = Trailer ("Expires" :| ["X-Checksum"])
    lookupHeader (headerMapFromList [("Trailer", ", Expires, "), ("Trailer", "X-Checksum")]) `shouldBe` Right (Just value)
    renderToHeaders defaultHeaderSettings value `shouldBe` ["Expires, X-Checksum"]
    lookupHeader @Trailer (headerMapFromList [("Trailer", "")]) `shouldBe` Right (Just TrailerEmpty)
    lookupHeader @Trailer (headerMapFromList [("Trailer", "Expires:")]) `shouldSatisfy` isLeft
    lookupHeader @Trailer (headerMapFromList [("Trailer", "Content-Digest"), ("Trailer", "X-Checksum")])
      `shouldBe` Right (Just (Trailer ("Content-Digest" :| ["X-Checksum"])))
  it "validates every authored Trailer name before replacing the declaration" $ do
    let fields = headerMapFromList [("Trailer", "Content-Digest"), ("Trailer", "X-Checksum")]
    forM_ ["", "x:field", "x field", "X-First,X-Second", "X-\xff"] $ \name -> do
      let value = Trailer ("Content-Digest" :| [name])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields
    renderHeaderLines defaultHeaderSettings (Trailer ("CONTENT-DIGEST" :| ["x!check"])) `shouldBe` Just ("CONTENT-DIGEST, x!check" :| [])
    renderHeaderLines defaultHeaderSettings TrailerEmpty `shouldBe` Just ("" :| [])
  it "labels known head-only fields by case-insensitive name for outbound trailers" $ do
    mapM_ (\name -> isForbiddenTrailerName name `shouldBe` True)
      [ "Authorization", "Connection", "CONTENT-LENGTH", "Content-Type", "Content-Encoding"
      , "content-range", "Expect", "Host", "Keep-Alive", "Proxy-Authenticate", "Proxy-Authorization"
      , "Proxy-Connection", "Range", "TE", "Trailer", "Transfer-Encoding", "Upgrade"
      ]
    mapM_ (\name -> isForbiddenTrailerName name `shouldBe` False)
      ["", "Content-Digest", "content-digEst", "Repr-Digest", "grpc-status", "X-Result", "x-content-type"]
  it "keeps Upgrade protocols in preference order across field lines" $ do
    let value = Upgrade (Protocol "HTTP" (Just "2.0") :| [Protocol "websocket" Nothing])
    lookupHeader (headerMapFromList [("Upgrade", " HTTP/2.0 , "), ("Upgrade", "websocket,")])
      `shouldBe` Right (Just value)
    renderToHeaders defaultHeaderSettings value `shouldBe` ["HTTP/2.0, websocket"]
    lookupHeader @Upgrade (headerMapFromList [("Upgrade", " , ")]) `shouldBe` Right (Just UpgradeEmpty)
    lookupHeader @Upgrade (headerMapFromList [("Upgrade", "HTTP/")]) `shouldSatisfy` isLeft
    lookupHeader @Upgrade (headerMapFromList [("Upgrade", "websocket"), ("Upgrade", "IRC/6.9")])
      `shouldBe` Right (Just (Upgrade (Protocol "websocket" Nothing :| [Protocol "IRC" (Just "6.9")])))
    lookupHeader @Upgrade (headerMapFromList [("Upgrade", "websocket/"), ("Upgrade", "13")]) `shouldSatisfy` isLeft
  it "rejects malformed authored Upgrade names and versions" $ do
    let original = headerMapFromList [("Upgrade", "websocket"), ("Upgrade", "IRC/6.9")]
    forM_ [Protocol "" Nothing, Protocol "HTTP/2" Nothing, Protocol "HTTP" (Just ""), Protocol "HTTP" (Just "2.0, rta")] $ \protocol -> do
      let value = Upgrade (Protocol "websocket" Nothing :| [protocol])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value original `shouldBe` original
