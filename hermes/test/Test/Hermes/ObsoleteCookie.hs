{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteCookie (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.Cookie2 as C2
import Network.HTTP.Headers.HeaderFieldName (hCookie2, hSetCookie2)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (parseFieldLine)
import qualified Network.HTTP.Headers.SetCookie2 as SC2
import Test.Syd
import Test.Syd.Hedgehog ()


readVersion :: ByteString -> Either String (Maybe Integer)
readVersion wire =
  fmap
    (fmap toInteger)
    (lookupHeader @C2.Cookie2 (headerMapFromList [("Cookie2", wire)]) >>= traverse C2.cookie2Version)


responseEntries :: [ByteString] -> Either String (Maybe [NonEmpty SC2.Cookie2Definition])
responseEntries linesIn =
  lookupHeader @SC2.SetCookie2s (headerMapFromList (map (\wire -> ("Set-Cookie2", wire)) linesIn))
    >>= traverse (traverse SC2.setCookie2Entries . SC2.getSetCookie2s)


propCookie2 :: Property
propCookie2 = property $ do
  version <- forAll (Gen.integral (Range.linear 0 (2 ^ (100 :: Int))))
  let fields = setHeader (C2.cookie2FromVersion (fromInteger version)) (headerMapFromList [])
  (lookupHeader @C2.Cookie2 fields >>= traverse C2.cookie2Version) === Right (Just (fromInteger version))


propSetCookie2 :: Property
propSetCookie2 = property $ do
  let quotedOctets = 9 : [32 .. 126] <> [128 .. 255]
  payload <- BS.pack <$> forAll (Gen.list (Range.linear 0 32) (Gen.element quotedOctets))
  ports <- forAll (Gen.nonEmpty (Range.linear 1 4) (Gen.word16 Range.constantBounded))
  version <- forAll (Gen.word64 Range.constantBounded)
  let cookies =
        SC2.Cookie2Definition "session" payload (fromIntegral version) [SC2.Cookie2Port (Just ports), SC2.Cookie2Secure]
          :| [SC2.Cookie2Definition "locale" "da-DK" 1 [SC2.Cookie2Extension "Custom" (Just payload)]]
  case SC2.setCookie2FromEntries cookies of
    Left err -> error err
    Right header -> do
      let rendered = M.toStrictByteString (SC2.renderSetCookie2 header)
      responseEntries [rendered] === Right (Just [cookies])
      parseFieldLine "Set-Cookie2" SC2.setCookie2Parser rendered === Right header


tests :: Spec
tests = describe "ObsoleteCookie" $ do
  describe "Cookie2" $ do
    it "validates checked writes from legacy cookie text" $ do
      let previous = headerMapFromList [("Cookie2", "$Version=1")]
      forM_ ["", "$Version=+1", "$Version=\" 1\"", "$Version=1, $Version=2"] $ \wire -> do
        let value = C2.Cookie2 wire
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value previous `shouldBe` previous
      renderHeaderLines defaultHeaderSettings (C2.Cookie2 " \t$vErSiOn=\"0001\" ")
        `shouldBe` Just (" \t$vErSiOn=\"0001\" " :| [])
    it "retains both version-one spellings in copied and sliced inputs" $ do
      forM_ ["$Version=1", "$Version=\"1\""] $ \wire ->
        forM_ [0 .. 15] $ \offset -> do
          let sliced = BS.take (BS.length wire) (BS.drop offset (BS.replicate offset 0 <> wire <> "x"))
          case lookupHeader @C2.Cookie2 (headerMapFromList [("Cookie2", sliced)]) of
            Right (Just value) -> do
              C2.cookie2Version value `shouldBe` Right 1
              renderHeaderLines defaultHeaderSettings value `shouldBe` Just (wire :| [])
            result -> expectationFailure (show result)
      renderHeaderLines defaultHeaderSettings (C2.cookie2FromVersion 1) `shouldBe` Just ("$Version=1" :| [])
    it "reads decimal versions in token and quoted form and retains the wire spelling" $ do
      let wire = " \t$vErSiOn = \"0001\" \t"
      readVersion wire `shouldBe` Right (Just 1)
      readVersion "$Version=18446744073709551616" `shouldBe` Right (Just 18446744073709551616)
      readVersion "$Version=\"1\"" `shouldBe` Right (Just 1)
      let fields = headerMapFromList [("Cookie2", wire)]
      case lookupHeader @C2.Cookie2 fields of
        Right (Just header) -> do
          lookupRawHeader hCookie2 (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
          parseFieldLine "Cookie2" C2.cookie2Parser wire `shouldBe` Right header
        result -> expectationFailure (show result)
      M.toStrictByteString (C2.renderCookie2 (C2.Cookie2 "$Version=\"1\"")) `shouldBe` "$Version=\"1\""
    it "rejects malformed versions, extraneous directives and extra field lines" $ do
      forM_ ["", "$Version", "$Version=", "Version=1", "$Version=+1", "$Version=-1", "$Version=\" 1\"", "$Version=a", "$Version=1, $Version=2", "$Version=1; Path=/"] $ \wire ->
        readVersion wire `shouldSatisfy` isLeft
      lookupHeader @C2.Cookie2 (headerMapFromList [("Cookie2", "$Version=1"), ("Cookie2", "$Version=2")])
        `shouldSatisfy` isLeft
    it "round-trips arbitrary-precision cookie versions" propCookie2
  describe "Set-Cookie2" $ do
    it "validates checked writes from legacy cookie text atomically" $ do
      let previous = headerMapFromList [("Set-Cookie2", "a=x; Version=1"), ("Set-Cookie2", "b=y; Version=2")]
          first = "n=\"a,b\"; Version=\"1\"; Version=ignored"
          second = "m=z; Version=2; Port"
      renderHeaderLines defaultHeaderSettings (SC2.SetCookie2s []) `shouldBe` Nothing
      setHeader (SC2.SetCookie2s []) previous `shouldBe` previous
      case lookupHeader @SC2.SetCookie2s (headerMapFromList [("Set-Cookie2", first)]) of
        Right (Just (SC2.SetCookie2s [parsed])) -> do
          forM_ ["", "n=x", "n=x; Version=1; Port=1234", "n=x; Version=1, m=y"] $ \wire -> do
            let invalid = SC2.SetCookie2 wire
                authored = SC2.SetCookie2 second
            forM_ [[invalid, parsed, authored], [parsed, invalid, authored], [parsed, authored, invalid]] $ \lines' -> do
              let value = SC2.SetCookie2s lines'
              renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
              setHeader value previous `shouldBe` previous
          let accepted = SC2.SetCookie2s [parsed, SC2.SetCookie2 second]
          renderHeaderLines defaultHeaderSettings accepted `shouldBe` Just (first :| [ST.toByteString second])
        result -> expectationFailure (show result)
    it "recognizes every RFC attribute and preserves folded and repeated field lines" $ do
      let first =
            "Customer=\"A, B\"; Version=\"1\"; path=\"/acme\"; Port=\"80, 8000\"; Secure; Discard;"
              <> " Comment=\"Caf\195\169\"; CommentURL=\"HTTP://[::1]:8080/acme?x=1\";"
              <> " Domain=\"example.test\"; Max-Age=\"18446744073709551616\"; X-Note=\"a\\\"\255\", "
              <> "Locale=da; Version=2; Port"
          second = "Secure=\"\"; Version=0001"
          entries =
            SC2.Cookie2Definition
              "Customer"
              "A, B"
              1
              [ SC2.Cookie2Path "/acme"
              , SC2.Cookie2Port (Just (80 :| [8000]))
              , SC2.Cookie2Secure
              , SC2.Cookie2Discard
              , SC2.Cookie2Comment "Café"
              , SC2.Cookie2CommentURL "HTTP://[::1]:8080/acme?x=1"
              , SC2.Cookie2Domain "example.test"
              , SC2.Cookie2MaxAge 18446744073709551616
              , SC2.Cookie2Extension "X-Note" (Just "a\"\255")
              ]
              :| [SC2.Cookie2Definition "Locale" "da" 2 [SC2.Cookie2Port Nothing]]
      responseEntries [first, second] `shouldBe` Right (Just [entries, SC2.Cookie2Definition "Secure" "" 1 [] :| []])
      case lookupHeader @SC2.SetCookie2s (headerMapFromList [("Set-Cookie2", first), ("Set-Cookie2", second)]) of
        Right (Just header) ->
          lookupRawHeader hSetCookie2 (setHeader header (headerMapFromList [])) `shouldBe` Just (first :| [second])
        result -> expectationFailure (show result)
      M.toStrictByteString (SC2.renderSetCookie2 (SC2.SetCookie2 "Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\""))
        `shouldBe` "Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\""
    it "projects the first matching attribute without losing duplicate wire bytes" $ do
      let wire = "id=x; version=1; Version=ignored; Path=\"/a\"; path=\"/b,c\"; Feature; FEATURE=x"
          expected = SC2.Cookie2Definition "id" "x" 1 [SC2.Cookie2Path "/a", SC2.Cookie2Extension "Feature" Nothing]
      responseEntries [wire] `shouldBe` Right (Just [expected :| []])
      case lookupHeader @SC2.SetCookie2s (headerMapFromList [("Set-Cookie2", wire)]) of
        Right (Just header) ->
          lookupRawHeader hSetCookie2 (setHeader header (headerMapFromList [])) `shouldBe` Just (wire :| [])
        result -> expectationFailure (show result)
    it "checks numeric bounds, quoted URL syntax, member boundaries and required attributes" $ do
      responseEntries ["id=x; Version=1; CommentURL=\"http://example.test\"; Port=\", 0, 65535,\""]
        `shouldBe` Right (Just [SC2.Cookie2Definition "id" "x" 1 [SC2.Cookie2CommentURL "http://example.test", SC2.Cookie2Port (Just (0 :| [65535]))] :| []])
      forM_
        [ ""
        , ", ,"
        , "id=x"
        , "id=; Version=1"
        , "$Reserved=x; Version=1"
        , "id=x; Version=-1"
        , "id=x; Version=+1"
        , "id=x; Version=1; Port=80"
        , "id=x; Version=1; Port=\"\""
        , "id=x; Version=1; Port=\"65536\""
        , "id=x; Version=1; Port=\"+1\""
        , "id=x; Version=1; Max-Age=-1"
        , "id=x; Version=1; Secure=true"
        , "id=x; Version=1; CommentURL=\"/relative\""
        , "id=x; Version=1; CommentURL=\"ftp://example.test/\""
        , "id=x; Version=1; CommentURL=\"http://example.test/#fragment\""
        , "id=x; Version=1; CommentURL=\"http:///path\""
        , "id=x; Version=1; Comment=\"\255\""
        , "id=x; Version=1; x=\"bad\r\""
        , "id=x; Version=1, other=x; Path=\"/b\""
        ]
        $ \wire -> responseEntries [wire] `shouldSatisfy` isLeft
    it "round-trips ordered cookies and opaque values through typed authoring" propSetCookie2
