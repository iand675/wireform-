{-# LANGUAGE OverloadedStrings #-}

module Test.Parser (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Network.HTTP1.Parser
import Network.HTTP1.Types
import Test.QuickCheck
import Test.Syd


tests :: Spec
tests =
  describe "Parser" $
    sequence_
      [ requestLineTests
      , statusLineTests
      , headerBlockTests
      , requestFramingTests
      , responseFramingTests
      , chunkHeaderTests
      , smugglingGuardTests
      ]


------------------------------------------------------------------------

requestLineTests :: Spec
requestLineTests =
  describe "request line" $
    sequence_
      [ it "GET / HTTP/1.1" $
          parseRequestLine "GET / HTTP/1.1"
            `shouldBe` Right (GET, "/", HTTP_1_1)
      , it "POST with absolute path" $
          parseRequestLine "POST /api/v1/things HTTP/1.1"
            `shouldBe` Right (POST, "/api/v1/things", HTTP_1_1)
      , it "extension method" $
          parseRequestLine "PROPFIND / HTTP/1.1"
            `shouldBe` Right (methodFromBytes "PROPFIND", "/", HTTP_1_1)
      , it "missing target" $
          parseRequestLine "GET  HTTP/1.1"
            `shouldBe` Left ParseBadRequestLine
      , it "unsupported version" $
          parseRequestLine "GET / HTTP/2.0"
            `shouldBe` Left ParseUnsupportedVersion
      ]


statusLineTests :: Spec
statusLineTests =
  describe "status line" $
    sequence_
      [ it "200 OK" $
          fmap dropReason (parseStatusLine "HTTP/1.1 200 OK")
            `shouldBe` Right (HTTP_1_1, Status 200)
      , it "404 Not Found" $
          fmap dropReason (parseStatusLine "HTTP/1.1 404 Not Found")
            `shouldBe` Right (HTTP_1_1, NotFound)
      , it "non-numeric code" $
          parseStatusLine "HTTP/1.1 OK OK" `shouldBe` Left ParseBadStatusLine
      ]
  where
    dropReason (a, b, _) = (a, b)


------------------------------------------------------------------------

headerBlockTests :: Spec
headerBlockTests =
  describe "header block" $
    sequence_
      [ it "single header" $
          parseHeaderBlock "Host: example.com"
            `shouldBe` Right [("Host", "example.com")]
      , it "two headers, CRLF separator" $
          parseHeaderBlock "Host: example.com\r\nAccept: */*"
            `shouldBe` Right [("Host", "example.com"), ("Accept", "*/*")]
      , it "OWS trimming on value" $
          parseHeaderBlock "X-Test:   spaces and tabs\t  "
            `shouldBe` Right [("X-Test", "spaces and tabs")]
      , it "rejects bare LF in value" $
          parseHeaderBlock "X-Bad: line1\nline2"
            `shouldBe` Left ParseInvalidHeaderValue
      , it "rejects empty name" $
          parseHeaderBlock ": value"
            `shouldBe` Left ParseBadHeaderName
      , it "rejects obs-fold" $
          parseHeaderBlock " continuation"
            `shouldBe` Left ParseInvalidHeaderValue
      , it "rejects NUL in value" $
          parseHeaderBlock "X: bad\x00stuff"
            `shouldBe` Left ParseInvalidHeaderValue
      , it "accepts obs-text (high bytes)" $
          let high = BS.pack [0x58, 0x3a, 0x20, 0xc3, 0xa9]
          in parseHeaderBlock high `shouldBe` Right [("X", BS.pack [0xc3, 0xa9])]
      ]


------------------------------------------------------------------------

requestFramingTests :: Spec
requestFramingTests =
  describe "request framing" $
    sequence_
      [ it "GET no body" $
          runReq "GET / HTTP/1.1\r\nHost: x\r\n\r\n"
            `shouldBe` Right (GET, NoBody)
      , it "POST with content-length" $
          runReq "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\n\r\n"
            `shouldBe` Right (POST, ContentLength 4)
      , it "POST chunked" $
          runReq "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n"
            `shouldBe` Right (POST, Chunked)
      , it "TE with non-chunked-last is rejected" $
          runReq "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: gzip\r\n\r\n"
            `shouldBe` Left ParseChunkedNotFinal
      , it "HTTP/1.1 GET missing Host rejected" $
          runReq "GET / HTTP/1.1\r\n\r\n"
            `shouldBe` Left ParseMissingHost
      ]
  where
    runReq bs = fmap (\(r, f) -> (requestMethod r, f)) (parseHeadAndFraming bs)


parseHeadAndFraming :: ByteString -> Either ParseError (Request, Framing)
parseHeadAndFraming bs = case BS.breakSubstring "\r\n\r\n" bs of
  (block, rest)
    | BS.null rest -> parseRequest block
    | otherwise -> parseRequest block


------------------------------------------------------------------------

responseFramingTests :: Spec
responseFramingTests =
  describe "response framing" $
    sequence_
      [ it "204 has no body even if CL present" $
          runResp GET "HTTP/1.1 204 No Content\r\nContent-Length: 5\r\n\r\n"
            `shouldBe` Right NoBody
      , it "HEAD has no body" $
          runResp HEAD "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
            `shouldBe` Right NoBody
      , it "1.0 no framing => close-delimited" $
          runResp GET "HTTP/1.0 200 OK\r\n\r\n"
            `shouldBe` Right CloseDelimited
      , it "1.1 no framing => zero-length" $
          runResp GET "HTTP/1.1 200 OK\r\n\r\n"
            `shouldBe` Right NoBody
      ]
  where
    runResp m bs = case BS.breakSubstring "\r\n\r\n" bs of
      (block, _) -> fmap snd (parseResponse m block)


------------------------------------------------------------------------

chunkHeaderTests :: Spec
chunkHeaderTests =
  describe "chunked TE" $
    sequence_
      [ it "0 size" $
          parseChunkHeader "0\r\n" 0
            `shouldBe` Right (Just (ChunkHeader 0 3))
      , it "hex size" $
          parseChunkHeader "ff\r\n" 0
            `shouldBe` Right (Just (ChunkHeader 0xff 4))
      , it "with extension" $
          parseChunkHeader "10;ext=value\r\n" 0
            `shouldBe` Right (Just (ChunkHeader 0x10 14))
      , it "needs more input" $
          parseChunkHeader "10" 0
            `shouldBe` Right Nothing
      , it "rejects bare CR in extension" $
          parseChunkHeader "10\rnotlf" 0
            `shouldBe` Left ParseBadChunkHeader
      , it "decimal sizes round-trip via hex" $
          property $ \n ->
            n >= 0 && n < (2 ^ (40 :: Int)) ==>
              let hex = showHex' n ""
                  input = BS.pack (map (fromIntegral . fromEnum) hex) <> "\r\n"
              in parseChunkHeader input 0
                   == Right (Just (ChunkHeader (fromIntegral (n :: Int)) (length hex + 2)))
      ]


showHex' :: Int -> String -> String
showHex' n acc
  | n < 16 = digit n : acc
  | otherwise = showHex' (n `div` 16) (digit (n `mod` 16) : acc)
  where
    digit d
      | d < 10 = toEnum (d + 48)
      | otherwise = toEnum (d - 10 + 97)


------------------------------------------------------------------------
-- Request smuggling guards
------------------------------------------------------------------------

smugglingGuardTests :: Spec
smugglingGuardTests =
  describe "request smuggling guards" $
    sequence_
      [ it "CL + TE both present" $
          runFraming "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\nTransfer-Encoding: chunked\r\n\r\n"
            `shouldBe` Left ParseLengthAndTransferEncoding
      , it "duplicate disagreeing CL" $
          runFraming "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\nContent-Length: 5\r\n\r\n"
            `shouldBe` Left ParseLengthConflict
      , it "duplicate agreeing CL is fine" $
          runFraming "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\nContent-Length: 4\r\n\r\n"
            `shouldBe` Right (ContentLength 4)
      , it "non-numeric CL rejected" $
          runFraming "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: lots\r\n\r\n"
            `shouldBe` Left ParseInvalidLength
      , it "negative CL rejected" $
          runFraming "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: -1\r\n\r\n"
            `shouldBe` Left ParseInvalidLength
      , it "HTTP/1.1 missing Host rejected" $
          runFraming "GET / HTTP/1.1\r\nContent-Length: 0\r\n\r\n"
            `shouldBe` Left ParseMissingHost
      , it "HTTP/1.1 multiple Host rejected" $
          runFraming "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n"
            `shouldBe` Left ParseMultipleHosts
      , it "HTTP/1.0 missing Host is fine" $
          runFraming "GET / HTTP/1.0\r\n\r\n"
            `shouldBe` Right NoBody
      ]
  where
    runFraming :: ByteString -> Either ParseError Framing
    runFraming bs = case BS.breakSubstring "\r\n\r\n" bs of
      (block, _) -> fmap snd (parseRequest block)
