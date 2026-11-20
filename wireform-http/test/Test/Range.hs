{-# LANGUAGE OverloadedStrings #-}

{- |
Tests for "Network.HTTP.Client.Range".

The hermes-side parser/renderer is already tested in
@hermes-tests@; this module covers the wireform-flavoured
projection (the 'ByteRange' from/to/suffix split) plus the
helpers that are only at this layer:
'parseContentRangeFull', 'parseAcceptRanges', and the
@multipart/byteranges@ body parser.
-}
module Test.Range (tests) where

import Data.ByteString qualified as BS
import Network.HTTP.Client.Range
import Network.HTTP.Client.Request qualified as Request
import Network.HTTP.Client.URI qualified as URI
import Network.HTTP.Headers.ContentRange qualified as HCR
import Network.HTTP.Types.Header qualified as H
import Test.Syd


tests :: Spec
tests =
  describe "Network.HTTP.Client.Range" $
    sequence_
      [ describe "rangeHeader" $
          sequence_
            [ it "closed range" $
                rangeHeader [byteRange 0 99] `shouldBe` "bytes=0-99"
            , it "open range from offset" $
                rangeHeader [byteRangeFrom 1000] `shouldBe` "bytes=1000-"
            , it "suffix range" $
                rangeHeader [byteRangeSuffix 500] `shouldBe` "bytes=-500"
            , it "comma-joined multi range" $
                rangeHeader [byteRange 0 99, byteRangeFrom 200, byteRangeSuffix 50]
                  `shouldBe` "bytes=0-99,200-,-50"
            ]
      , describe "parseRange" $
          sequence_
            [ it "round-trips closed" $
                parseRange "bytes=0-99" `shouldBe` Just [byteRange 0 99]
            , it "round-trips open" $
                parseRange "bytes=1000-" `shouldBe` Just [byteRangeFrom 1000]
            , it "round-trips suffix" $
                parseRange "bytes=-500" `shouldBe` Just [byteRangeSuffix 500]
            , it "round-trips multi" $
                parseRange "bytes=0-99,200-,-50"
                  `shouldBe` Just [byteRange 0 99, byteRangeFrom 200, byteRangeSuffix 50]
            , it "rejects other range-units" $
                parseRange "rows=0-9" `shouldBe` Nothing
            , it "projects mixed-case byte ranges and rejects invalid offsets" $ do
                parseRange "ByTeS=,0-99,,200-" `shouldBe` Just [byteRange 0 99, byteRangeFrom 200]
                parseRange "bytes=8-7" `shouldBe` Nothing
                parseRange "bytes=18446744073709551616-" `shouldBe` Nothing
            ]
      , describe "parseContentRange / parseContentRangeFull" $
          sequence_
            [ it "satisfied returns the legacy shape" $
                parseContentRange "bytes 0-499/1234"
                  `shouldBe` Just (ContentRange 0 499 (Just 1234))
            , it "satisfied with unknown total" $
                parseContentRange "bytes 0-9/*"
                  `shouldBe` Just (ContentRange 0 9 Nothing)
            , it "unsatisfied is Nothing through parseContentRange" $
                parseContentRange "bytes */4096" `shouldBe` Nothing
            , it "unsatisfied is detectable through parseContentRangeFull" $
                case parseContentRangeFull "bytes */4096" of
                  Just (HCR.ContentRange _ (HCR.RangeRespUnsatisfied (Just 4096))) -> pure ()
                  other -> expectationFailure (show other)
            , it "projects only a satisfied byte range and retains extensions through the full parser" $ do
                parseContentRange "BYTES 0-9/10" `shouldBe` Just (ContentRange 0 9 (Just 10))
                parseContentRange "bytes 0-9/9" `shouldBe` Nothing
                parseContentRange "rows 0-9/10" `shouldBe` Nothing
                parseContentRangeFull "rows 0-9/10"
                  `shouldBe` Just (HCR.ContentRange "rows" (HCR.RangeRespSatisfied 0 9 (Just 10)))
            ]
      , describe "parseAcceptRanges" $
          sequence_
            [ it "literal none" $
                parseAcceptRanges "none" `shouldBe` Just AcceptRangesNone
            , it "single unit" $
                parseAcceptRanges "bytes" `shouldBe` Just (AcceptRangesUnits ["bytes"])
            , it "comma list" $
                parseAcceptRanges "bytes, custom-unit"
                  `shouldBe` Just (AcceptRangesUnits ["bytes", "custom-unit"])
            , it "rejects junk" $
                parseAcceptRanges "" `shouldBe` Nothing
            ]
      , describe "parseMultipartByteranges" $
          sequence_
            [ it "two parts with explicit boundaries" $
                let boundary = "BOUNDARY"
                    body =
                      BS.concat
                        [ "--BOUNDARY\r\n"
                        , "Content-Type: text/plain\r\n"
                        , "Content-Range: bytes 0-9/200\r\n"
                        , "\r\n"
                        , "0123456789"
                        , "\r\n--BOUNDARY\r\n"
                        , "Content-Type: text/plain\r\n"
                        , "Content-Range: bytes 100-109/200\r\n"
                        , "\r\n"
                        , "ABCDEFGHIJ"
                        , "\r\n--BOUNDARY--\r\n"
                        ]
                in case parseMultipartByteranges boundary body of
                     Just [a, b] -> do
                       mbContentType a `shouldBe` Just "text/plain"
                       mbContentType b `shouldBe` Just "text/plain"
                       crStart (mbRange a) `shouldBe` 0
                       crEnd (mbRange a) `shouldBe` 9
                       crTotal (mbRange a) `shouldBe` Just 200
                       mbBody a `shouldBe` "0123456789"
                       crStart (mbRange b) `shouldBe` 100
                       crEnd (mbRange b) `shouldBe` 109
                       mbBody b `shouldBe` "ABCDEFGHIJ"
                     other -> error (show other)
            , it "skips a part with no Content-Range" $
                let boundary = "B"
                    body =
                      BS.concat
                        [ "--B\r\n"
                        , "Content-Type: text/plain\r\n"
                        , "\r\n"
                        , "garbage"
                        , "\r\n--B\r\n"
                        , "Content-Range: bytes 0-2/3\r\n"
                        , "\r\n"
                        , "abc"
                        , "\r\n--B--\r\n"
                        ]
                in case parseMultipartByteranges boundary body of
                     Just [part] -> mbBody part `shouldBe` "abc"
                     other -> expectationFailure (show other)
            , it "preserves delimiter text inside binary content and preserves payload line endings" $ do
                let payload = "abc--B\r\ndef\r\n"
                    body = "--B\r\nContent-Range: bytes 0-12/60\r\n\r\n" <> payload <> "\r\n--B--\r\n"
                fmap (map mbBody) (parseMultipartByteranges "B" body) `shouldBe` Just [payload]
            , it "accepts transport padding, preamble, epilogue, and folded MIME headers" $ do
                let boundary = "part 1"
                    body = "preamble --part 1\r\n--part 1 \t\r\nContent-Range:\r\n\tbytes 3-5/10\r\n\r\nabc\r\n--part 1--\t\r\nignored"
                parseMultipartByteranges boundary body `shouldBe`
                  Just [MultipartByterange (ContentRange 3 5 (Just 10)) Nothing "abc"]
            , it "keeps support for LF-delimited multipart responses" $
                parseMultipartByteranges "B" "--B\nContent-Range: bytes 0-4/8\n\nA\r\n\r\n\n--B--"
                  `shouldBe` Just [MultipartByterange (ContentRange 0 4 (Just 8)) Nothing "A\r\n\r\n"]
            , it "keeps a bare LF and boundary marker inside a CRLF-framed payload" $ do
                let payload = "a\n--B--\r\nb"
                    body = "--B\r\nContent-Range: bytes 0-9/12\r\n\r\n" <> payload <> "\r\n--B--"
                fmap (map mbBody) (parseMultipartByteranges "B" body) `shouldBe` Just [payload]
            , it "keeps a payload's last CR byte before an LF delimiter" $
                fmap (map mbBody) (parseMultipartByteranges "B" "--B\nContent-Range: bytes 0-1/3\n\nA\r\n--B--")
                  `shouldBe` Just ["A\r"]
            , it "preserves folding whitespace inside a quoted MIME parameter" $ do
                let body = "--B\r\nContent-Range: bytes 0-2/8\r\nContent-Type: text/plain; name=\"one\r\n\t  two\"\r\n\r\nabc\r\n--B--"
                fmap (map mbContentType) (parseMultipartByteranges "B" body)
                  `shouldBe` Just [Just "text/plain; name=\"one\t  two\""]
            , it "requires valid opening and closing delimiters" $ do
                let part = "Content-Range: bytes 0-2/10\r\n\r\nabc"
                parseMultipartByteranges "B" (part <> "\r\n--B--") `shouldBe` Nothing
                parseMultipartByteranges "B" ("--B\r\n" <> part) `shouldBe` Nothing
                parseMultipartByteranges "B" ("--B\r\n" <> part <> "\r\n--B--junk") `shouldBe` Nothing
                parseMultipartByteranges "B " ("--B \r\n" <> part <> "\r\n--B --") `shouldBe` Nothing
            , it "drops ambiguous, incomplete, and length-mismatched parts" $ do
                let part bytes = "--B\r\n" <> bytes <> "\r\n"
                    body = BS.concat
                      [ part "Content-Range: bytes 0-1/10\r\ncontent-range: bytes 3-4/10\r\n\r\nxy"
                      , part "Content-Range: bytes 0-3/10\r\n\r\nxy"
                      , part "Content-Range: bytes 0-0/10\r\n\r\nxy"
                      , part "Content-Range: bytes 0-18446744073709551615/*\r\n\r\nx"
                      , part "Content-Range: bytes 4-5/10"
                      , part "Content-Range: bytes 6-8/10\r\nContent-Type: text/plain\r\nContent-Type: text/plain\r\n\r\nabc"
                      , part "Content-Range: bytes 6-8/10\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\nabc"
                      , part "Content-Range: bytes 6-8/10\r\nContent-Transfer-Encoding: binary\r\n\r\nabc"
                      , "--B--"
                      ]
                parseMultipartByteranges "B" body `shouldBe`
                  Just [MultipartByterange (ContentRange 6 8 (Just 10)) Nothing "abc"]
            ]
      , describe "withRange" $
          sequence_
            [ it "replaces all earlier Range lines and preserves the validator" $ do
                let original = rangeRequest
                      { Request.headers = [("range", "bytes=1-2"), (H.hIfRange, "\"v1\""), (H.hRange, "bytes=4-")]
                      }
                Request.headers (withRange [byteRange 5 9, byteRangeFrom 20] original)
                  `shouldBe` [(H.hRange, "bytes=5-9,20-"), (H.hIfRange, "\"v1\"")]
            , it "removes Range and If-Range when an empty or reversed range is requested" $ do
                let original = rangeRequest
                      { Request.headers = [(H.hRange, "bytes=1-2"), ("X-Keep", "yes"), (H.hIfRange, "\"v1\"")]
                      }
                Request.headers (withRange [] original) `shouldBe` [("X-Keep", "yes")]
                Request.headers (withRange [byteRangeFrom 3, byteRange 8 7] original) `shouldBe` [("X-Keep", "yes")]
            ]
      ]

rangeRequest :: Request.Request ()
rangeRequest = Request.get (either (error . show) id (URI.parseTemplate "https://example.com/download"))
