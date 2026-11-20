module Test.CBOR (cborTests) where

import CBOR.Class (FromCBOR, decodeCBOR, encodeCBOR, encodeCBORDirect)
import CBOR.Decode (decode, decodeSequence)
import CBOR.Encode (encode, encodeDeterministic, encodeSequence)
import CBOR.Encoding qualified as Encoding
import CBOR.JSON (fromJSON, toJSON)
import CBOR.Stream qualified as Stream
import CBOR.TagRegistry (defaultCBORTagRegistry, lookupTag, thValidate)
import CBOR.Value qualified as C
import Control.Exception (ErrorCall, evaluate, try)
import Data.Aeson qualified as Aeson
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8, Word16, Word32, Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import Test.Syd
import Test.Syd.Hedgehog ()


cborTests :: Spec
cborTests =
  describe "CBOR" $
    sequence_
      [ rfc8949AppendixA
      , rfc8949ConformanceVectors
      , streamingConformance
      , propertyRoundtrips
      , edgeCases
      , numericConformance
      , jsonTests
      , deterministicTests
      , sequenceTests
      ]


streamingConformance :: Spec
streamingConformance = describe "Incremental CBOR" $ do
  it "suspends inside a byte string chunk and resumes across later chunks" $ do
    let wire = BS.pack [0x5f, 0x43, 1, 2, 3, 0x40, 0x42, 4, 5, 0xff]
        expected = C.ByteString (BS.pack [1, 2, 3, 4, 5])
    checkStreamSplits wire expected
    checkByteFeeds (BS.unpack wire) expected

  it "feeds a nested CBOR item one byte at a time" $ do
    let wire = BS.pack [0x9f, 0xbf, 0x01, 0x7f, 0x62, 0xc3, 0xa9, 0x61, 0x21, 0xff, 0xff, 0xd8, 0x64, 0x82, 0x00, 0x19, 0x01, 0x00, 0xff]
        expected =
          C.Array (V.fromList
            [ C.Map (V.singleton (C.UInt 1, C.TextString "é!"))
            , C.Tag 100 (C.Array (V.fromList [C.UInt 0, C.UInt 256]))
            ])
    checkStreamSplits wire expected
    checkByteFeeds (BS.unpack wire) expected

  it "can reuse the same continuation after suspending inside a header" $ do
    let step = Stream.streamDecode (BS.pack [0x82, 0x59, 0x00])
    expectDone
      (C.Array (V.fromList [C.ByteString "a", C.UInt 2]))
      (BS.singleton 0xf6)
      (Stream.feedMore step (BS.pack [1, 0x61, 2, 0xf6]))
    expectDone
      (C.Array (V.fromList [C.ByteString "bc", C.UInt 3]))
      BS.empty
      (Stream.feedMore step (BS.pack [2, 0x62, 0x63, 3]))

  it "rejects an overflowing combined string size before waiting for its body" $ do
    let limit = encode (C.UInt (fromIntegral (maxBound :: Int)))
        check prefix major =
          case Stream.feedMore (Stream.streamDecode (BS.pack (prefix ++ [major .|. BS.head limit]))) (BS.tail limit) of
            Stream.Fail _ -> pure ()
            step -> expectationFailure ("Expected impossible length to fail, got " ++ show step)
    check [0x5f, 0x41, 0x61] 0x40
    check [0x7f, 0x61, 0x61] 0x60

  it "rejects RFC 8949 Appendix F syntax errors at every split" $ do
    let reserved = concatMap (\major -> map (\arg -> [major + arg]) [28 .. 30]) [0, 32 .. 224]
        wires = reserved ++
          [ [0xf8, 0], [0xf8, 1], [0xf8, 0x18], [0xf8, 0x1f]
          , [0x5f, 0, 0xff], [0x5f, 0x21, 0xff], [0x5f, 0x61, 0, 0xff]
          , [0x5f, 0x80, 0xff], [0x5f, 0xa0, 0xff], [0x5f, 0xc0, 0, 0xff]
          , [0x5f, 0xe0, 0xff], [0x7f, 0x41, 0, 0xff]
          , [0x5f, 0x5f, 0x41, 0, 0xff, 0xff], [0x7f, 0x7f, 0x61, 0, 0xff, 0xff]
          , [0xff], [0x81, 0xff], [0x82, 0, 0xff], [0xa1, 0xff], [0xa1, 0xff, 0]
          , [0xa1, 0, 0xff], [0xa2, 0, 0, 0xff], [0x9f, 0x81, 0xff]
          , [0x9f, 0x82, 0x9f, 0x81, 0x9f, 0x9f, 0xff, 0xff, 0xff, 0xff]
          , [0xbf, 0, 0xff], [0xbf, 0, 0, 0, 0xff], [0x1f], [0x3f], [0xdf]
          ]
        check wire split = do
          let (prefix, rest) = BS.splitAt split wire
          case Stream.feedMore (Stream.streamDecode prefix) rest of
            Stream.Fail _ -> pure ()
            step -> expectationFailure ("Expected syntax failure for " ++ show wire ++ ", got " ++ show step)
    mapM_ (\bytes -> let wire = BS.pack bytes in mapM_ (check wire) [0 .. BS.length wire]) wires

  it "rejects truncated RFC 8949 Appendix F inputs at EOF" $ do
    let wires =
          [ [0x18], [0x19], [0x1a], [0x1b], [0x19, 1], [0x1a, 1, 2]
          , [0x1b, 1, 2, 3, 4, 5, 6, 7], [0x38], [0x58], [0x78], [0x98]
          , [0x9a, 1, 0xff, 0], [0xb8], [0xd8], [0xf8], [0xf9, 0], [0xfa, 0, 0], [0xfb, 0, 0, 0]
          , [0x41], [0x61], [0x5a, 0xff, 0xff, 0xff, 0xff, 0]
          , (0x5b : replicate 8 0xff) ++ [1, 2, 3]
          , [0x7a, 0xff, 0xff, 0xff, 0xff, 0]
          , (0x7b : 0x7f : replicate 7 0xff) ++ [1, 2, 3]
          , [0x81], replicate 9 0x81, [0x82, 0], [0xa1], [0xa2, 1, 2], [0xa1, 0], [0xa2, 0, 0, 0]
          , [0xc0], [0x5f, 0x41, 0], [0x7f, 0x61, 0]
          , [0x9f], [0x9f, 1, 2], [0xbf], [0xbf, 1, 2, 1, 2], [0x81, 0x9f], [0x9f, 0x80, 0]
          , [0x9f, 0x9f, 0x9f, 0x9f, 0x9f, 0xff, 0xff, 0xff, 0xff]
          , [0x9f, 0x81, 0x9f, 0x81, 0x9f, 0x9f, 0xff, 0xff, 0xff]
          ]
    mapM_ (shouldReject . BS.pack) wires

  it "agrees with the strict decoder on arbitrary short inputs and leftover bytes" $
    withTests 2000 $ property $ do
      wire <- forAll (Gen.bytes (Range.linear 0 32))
      width <- forAll (Gen.int (Range.linear 1 8))
      let expected = firstStrictItem wire
          observed = collect (Stream.streamDecode BS.empty) (splitBytes width wire)
      observed === expected
  where
    firstStrictItem wire = seek 1
      where
        seek n
          | n > BS.length wire = Nothing
          | otherwise = case decode (BS.take n wire) of
              Right value -> Just (encode value, BS.drop n wire)
              Left _ -> seek (n + 1)

    collect (Stream.Done value rest) chunks = Just (encode value, BS.concat (rest : chunks))
    collect (Stream.Partial more) (chunk : chunks) = collect (more chunk) chunks
    collect _ _ = Nothing

    splitBytes width bytes
      | BS.null bytes = []
      | otherwise = let (chunk, rest) = BS.splitAt width bytes in chunk : splitBytes width rest


checkStreamSplits :: BS.ByteString -> C.Value -> IO ()
checkStreamSplits wire expected = mapM_ check [0 .. BS.length wire - 1]
  where
    trailing = BS.singleton 0xf6

    check split = do
      let (prefix, rest) = BS.splitAt split wire
      case Stream.streamDecode prefix of
        step@(Stream.Partial _) -> do
          case Stream.feedMore step BS.empty of
            Stream.Fail _ -> pure ()
            eof -> expectationFailure ("Expected truncation, got " ++ show eof)
          expectDone expected trailing (Stream.feedMore step (rest <> trailing))
        early -> expectationFailure ("Expected partial input at split " ++ show split ++ ", got " ++ show early)


checkByteFeeds :: [Word8] -> C.Value -> IO ()
checkByteFeeds = go (Stream.streamDecode BS.empty)
  where
    go step [] expected = expectDone expected BS.empty step
    go step@(Stream.Partial _) (byte : rest) expected =
      go (Stream.feedMore step (BS.singleton byte)) rest expected
    go early _ _ = expectationFailure ("Finished before the last feed: " ++ show early)


expectDone :: C.Value -> BS.ByteString -> Stream.DecodeStep C.Value -> IO ()
expectDone expected trailing = \case
  Stream.Done value leftover -> do
    value `shouldBe` expected
    leftover `shouldBe` trailing
  step -> expectationFailure ("Expected a complete item, got " ++ show step)


-- | RFC 8949 Appendix A test vectors: exact byte sequences.
rfc8949AppendixA :: Spec
rfc8949AppendixA =
  describe "RFC 8949 Appendix A test vectors" $
    sequence_
      [ describe "Unsigned integers" $
          sequence_
            [ it "0 = [0x00]" $
                encode (C.UInt 0) `shouldBe` BS.pack [0x00]
            , it "1 = [0x01]" $
                encode (C.UInt 1) `shouldBe` BS.pack [0x01]
            , it "10 = [0x0a]" $
                encode (C.UInt 10) `shouldBe` BS.pack [0x0a]
            , it "23 = [0x17]" $
                encode (C.UInt 23) `shouldBe` BS.pack [0x17]
            , it "24 = [0x18, 0x18]" $
                encode (C.UInt 24) `shouldBe` BS.pack [0x18, 0x18]
            , it "25 = [0x18, 0x19]" $
                encode (C.UInt 25) `shouldBe` BS.pack [0x18, 0x19]
            , it "100 = [0x18, 0x64]" $
                encode (C.UInt 100) `shouldBe` BS.pack [0x18, 0x64]
            , it "1000 = [0x19, 0x03, 0xe8]" $
                encode (C.UInt 1000) `shouldBe` BS.pack [0x19, 0x03, 0xe8]
            , it "1000000 = [0x1a, 0x00, 0x0f, 0x42, 0x40]" $
                encode (C.UInt 1000000) `shouldBe` BS.pack [0x1a, 0x00, 0x0f, 0x42, 0x40]
            , it "1000000000000 = [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]" $
                encode (C.UInt 1000000000000) `shouldBe` BS.pack [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]
            ]
      , describe "Negative integers" $
          sequence_
            [ it "-1 = [0x20]" $
                encode (C.NInt 0) `shouldBe` BS.pack [0x20]
            , it "-10 = [0x29]" $
                encode (C.NInt 9) `shouldBe` BS.pack [0x29]
            , it "-100 = [0x38, 0x63]" $
                encode (C.NInt 99) `shouldBe` BS.pack [0x38, 0x63]
            , it "-1000 = [0x39, 0x03, 0xe7]" $
                encode (C.NInt 999) `shouldBe` BS.pack [0x39, 0x03, 0xe7]
            ]
      , describe "Simple values / booleans" $
          sequence_
            [ it "false = [0xf4]" $
                encode (C.Bool False) `shouldBe` BS.pack [0xf4]
            , it "true = [0xf5]" $
                encode (C.Bool True) `shouldBe` BS.pack [0xf5]
            , it "null = [0xf6]" $
                encode C.Null `shouldBe` BS.pack [0xf6]
            , it "undefined = [0xf7]" $
                encode C.Undefined `shouldBe` BS.pack [0xf7]
            ]
      , describe "Text strings" $
          sequence_
            [ it "\"\" = [0x60]" $
                encode (C.TextString "") `shouldBe` BS.pack [0x60]
            , it "\"a\" = [0x61, 0x61]" $
                encode (C.TextString "a") `shouldBe` BS.pack [0x61, 0x61]
            , it "\"IETF\" = [0x64, 0x49, 0x45, 0x54, 0x46]" $
                encode (C.TextString "IETF") `shouldBe` BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]
            , it "\"\\\"\\\\\" = [0x62, 0x22, 0x5c]" $
                encode (C.TextString "\"\\") `shouldBe` BS.pack [0x62, 0x22, 0x5c]
            , it "\"\\u00fc\" = [0x62, 0xc3, 0xbc]" $
                encode (C.TextString "\252") `shouldBe` BS.pack [0x62, 0xc3, 0xbc]
            ]
      , describe "Byte strings" $
          sequence_
            [ it "h'' = [0x40]" $
                encode (C.ByteString BS.empty) `shouldBe` BS.pack [0x40]
            , it "h'01020304' = [0x44, 0x01, 0x02, 0x03, 0x04]" $
                encode (C.ByteString (BS.pack [1, 2, 3, 4])) `shouldBe` BS.pack [0x44, 0x01, 0x02, 0x03, 0x04]
            ]
      , describe "Arrays" $
          sequence_
            [ it "[] = [0x80]" $
                encode (C.Array V.empty) `shouldBe` BS.pack [0x80]
            , it "[1, 2, 3] = [0x83, 0x01, 0x02, 0x03]" $
                encode (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
                  `shouldBe` BS.pack [0x83, 0x01, 0x02, 0x03]
            , it "[1, [2, 3], [4, 5]]" $
                encode
                  ( C.Array
                      ( V.fromList
                          [ C.UInt 1
                          , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                          , C.Array (V.fromList [C.UInt 4, C.UInt 5])
                          ]
                      )
                  )
                  `shouldBe` BS.pack [0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05]
            , it "25 element array" $
                encode (C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1 .. 25 :: Int]]))
                  `shouldBe` BS.pack ([0x98, 25] ++ [1 .. 23] ++ [0x18, 24, 0x18, 25])
            ]
      , describe "Maps" $
          sequence_
            [ it "{} = [0xa0]" $
                encode (C.Map V.empty) `shouldBe` BS.pack [0xa0]
            , it "{1: 2, 3: 4}" $
                encode (C.Map (V.fromList [(C.UInt 1, C.UInt 2), (C.UInt 3, C.UInt 4)]))
                  `shouldBe` BS.pack [0xa2, 0x01, 0x02, 0x03, 0x04]
            ]
      , describe "Tags" $
          sequence_
            [ it "tag 1 with integer" $
                encode (C.Tag 1 (C.UInt 1363896240))
                  `shouldBe` BS.pack [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0]
            ]
      , describe "Decode test vectors" $
          sequence_
            [ it "decode 0" $
                decode (BS.pack [0x00]) `shouldBe` Right (C.UInt 0)
            , it "decode 1" $
                decode (BS.pack [0x01]) `shouldBe` Right (C.UInt 1)
            , it "decode 23" $
                decode (BS.pack [0x17]) `shouldBe` Right (C.UInt 23)
            , it "decode 24" $
                decode (BS.pack [0x18, 0x18]) `shouldBe` Right (C.UInt 24)
            , it "decode 100" $
                decode (BS.pack [0x18, 0x64]) `shouldBe` Right (C.UInt 100)
            , it "decode 1000" $
                decode (BS.pack [0x19, 0x03, 0xe8]) `shouldBe` Right (C.UInt 1000)
            , it "decode 1000000" $
                decode (BS.pack [0x1a, 0x00, 0x0f, 0x42, 0x40]) `shouldBe` Right (C.UInt 1000000)
            , it "decode -1" $
                decode (BS.pack [0x20]) `shouldBe` Right (C.NInt 0)
            , it "decode -100" $
                decode (BS.pack [0x38, 0x63]) `shouldBe` Right (C.NInt 99)
            , it "decode false" $
                decode (BS.pack [0xf4]) `shouldBe` Right (C.Bool False)
            , it "decode true" $
                decode (BS.pack [0xf5]) `shouldBe` Right (C.Bool True)
            , it "decode null" $
                decode (BS.pack [0xf6]) `shouldBe` Right C.Null
            , it "decode \"\"" $
                decode (BS.pack [0x60]) `shouldBe` Right (C.TextString "")
            , it "decode \"a\"" $
                decode (BS.pack [0x61, 0x61]) `shouldBe` Right (C.TextString "a")
            , it "decode \"IETF\"" $
                decode (BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]) `shouldBe` Right (C.TextString "IETF")
            , it "decode []" $
                decode (BS.pack [0x80]) `shouldBe` Right (C.Array V.empty)
            , it "decode [1,2,3]" $
                decode (BS.pack [0x83, 0x01, 0x02, 0x03])
                  `shouldBe` Right (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
            , it "decode {}" $
                decode (BS.pack [0xa0]) `shouldBe` Right (C.Map V.empty)
            , it "decode {1:2, 3:4}" $
                decode (BS.pack [0xa2, 0x01, 0x02, 0x03, 0x04])
                  `shouldBe` Right (C.Map (V.fromList [(C.UInt 1, C.UInt 2), (C.UInt 3, C.UInt 4)]))
            ]
      , describe "Decode indefinite-length" $
          sequence_
            [ it "indefinite array [_ 1, 2]" $
                decode (BS.pack [0x9f, 0x01, 0x02, 0xff])
                  `shouldBe` Right (C.Array (V.fromList [C.UInt 1, C.UInt 2]))
            , it "indefinite map {_ 1:2}" $
                decode (BS.pack [0xbf, 0x01, 0x02, 0xff])
                  `shouldBe` Right (C.Map (V.fromList [(C.UInt 1, C.UInt 2)]))
            , it "rejects a UTF-8 code point split across CBOR text chunks" $
                shouldReject (BS.pack [0x7f, 0x61, 0xc3, 0x61, 0xa9, 0xff])
            , it "accepts a UTF-8 code point split across transport chunks" $ do
                let first = BS.pack [0x7f, 0x62, 0xc3]
                    rest = BS.pack [0xa9, 0x60, 0x61, 0x21, 0xff]
                    expected = C.TextString "é!"
                decode (first <> rest) `shouldBe` Right expected
                case Stream.feedMore (Stream.streamDecode first) (rest <> BS.singleton 0x01) of
                  Stream.Done value leftover -> do
                    value `shouldBe` expected
                    leftover `shouldBe` BS.singleton 0x01
                  step -> expectationFailure ("Expected decoded text, got " ++ show step)
            ]
      , describe "Decode lengths and counts" $
          sequence_
            [ it "rejects byte strings, text, arrays, maps and chunks with overflowing lengths" $ do
                let prefixes = [[0x5b], [0x7b], [0x9b], [0xbb], [0x5f, 0x5b], [0x7f, 0x7b]]
                    sizes = [replicate 8 0xff, 0x7f : replicate 7 0xff]
                mapM_ (\prefix -> mapM_ (\size -> shouldReject (BS.pack (prefix ++ size))) sizes) prefixes
            , it "rejects representable counts that exceed the minimum possible payload" $ do
                shouldReject (BS.pack [0x85, 0x01])
                shouldReject (BS.pack [0xa2, 0x01, 0x02, 0x03])
            ]
      , it "fails on malformed streaming input as soon as it is detected" $ do
          let malformed = map BS.pack [[0xff], [0x61, 0xff], [0x9f, 0xf8, 0x00]]
          mapM_ (\wire -> case Stream.streamDecode wire of
            Stream.Fail _ -> pure ()
            step -> expectationFailure ("Expected immediate stream failure, got " ++ show step)) malformed
          let partial = Stream.streamDecode (BS.singleton 0xf8)
          case partial of
            Stream.Partial _ -> pure ()
            step -> expectationFailure ("Expected more bytes, got " ++ show step)
          case Stream.feedMore partial (BS.singleton 0x20) of
            Stream.Done value rest -> do
              value `shouldBe` C.Simple 32
              rest `shouldBe` BS.empty
            step -> expectationFailure ("Expected completed simple value, got " ++ show step)
      , describe "Float encoding/decoding" $
          sequence_
            [ it "float32 100000.0" $ do
                let val = C.Float32 100000.0
                    bs = encode val
                BS.index bs 0 `shouldBe` 0xfa
                decode bs `shouldBe` Right val
            , it "float64 1.1" $ do
                let val = C.Float64 1.1
                    bs = encode val
                BS.index bs 0 `shouldBe` 0xfb
                decode bs `shouldBe` Right val
            ]
      ]

shouldReject :: BS.ByteString -> IO ()
shouldReject bytes = do
  isLeft (decode bytes) `shouldBe` True
  isLeft (decodeSequence bytes) `shouldBe` True
  case Stream.feedMore (Stream.streamDecode bytes) BS.empty of
    Stream.Fail _ -> pure ()
    step -> expectationFailure ("Expected stream rejection, got " ++ show step)

numericConformance :: Spec
numericConformance = describe "Numeric conformance" $ do
  it "rejects every two-byte simple value below 32" $
    mapM_ (\n -> shouldReject (BS.pack [0xf8, n])) [0 .. 31]

  it "rejects reserved simple codes before encoding" $ do
    result <- try (evaluate (encode (C.Simple 24))) :: IO (Either ErrorCall BS.ByteString)
    isLeft result `shouldBe` True

  it "encodes binary16 subnormals and rounds ties to even through both encoders" $ do
    let unit = 2 ** (-24) :: Float
        cases =
          [ (unit, [0x00, 0x01])
          , (2 ** (-14) - unit, [0x03, 0xff])
          , (2 ** (-14) - unit / 2, [0x04, 0x00])
          , (1 + 2 ** (-11), [0x3c, 0x00])
          , (1 + 3 * 2 ** (-11), [0x3c, 0x02])
          , (-unit / 2, [0x80, 0x00])
          , (65520, [0x7c, 0x00])
          ]
    mapM_
      (\(number, bits) -> do
          let expected = BS.pack (0xf9 : bits)
          encode (C.Float16 number) `shouldBe` expected
          Encoding.encodingToByteString (Encoding.float16 number) `shouldBe` expected)
      cases

  it "preserves the smallest binary16 subnormal through decode and encode" $ do
    let wire = BS.pack [0xf9, 0x00, 0x01]
    (encode <$> decode wire) `shouldBe` Right wire
    case Stream.streamDecode wire of
      Stream.Done value _ -> encode value `shouldBe` wire
      step -> expectationFailure ("Expected decoded subnormal, got " ++ show step)

  it "rejects integer overflow instead of wrapping into fixed-width types" $ do
    let readAs :: forall a. FromCBOR a => C.Value -> Either String a
        readAs = decodeCBOR . encode
    isLeft (readAs @Int8 (C.UInt 128)) `shouldBe` True
    isLeft (readAs @Int16 (C.NInt 32768)) `shouldBe` True
    isLeft (readAs @Int32 (C.UInt 2147483648)) `shouldBe` True
    isLeft (readAs @Int64 (C.NInt 9223372036854775808)) `shouldBe` True
    isLeft (readAs @Int (C.UInt 18446744073709551615)) `shouldBe` True
    isLeft (readAs @Word (C.NInt 0)) `shouldBe` True
    isLeft (readAs @Word8 (C.UInt 256)) `shouldBe` True
    isLeft (readAs @Word16 (C.UInt 65536)) `shouldBe` True
    isLeft (readAs @Word32 (C.UInt 4294967296)) `shouldBe` True
    readAs @Int8 (C.NInt 127) `shouldBe` Right minBound
    readAs @Int64 (C.UInt 9223372036854775807) `shouldBe` Right maxBound
    readAs @Word32 (C.UInt 4294967295) `shouldBe` Right maxBound
    readAs @Word64 (C.UInt maxBound) `shouldBe` Right maxBound

  it "uses bignum tags at the 64-bit integer boundaries" $ do
    let limit = 18446744073709551616 :: Integer
        magnitude = 0x49 : 0x01 : replicate 8 0
        cases =
          [ (limit - 1, BS.pack (0x1b : replicate 8 0xff))
          , (-limit, BS.pack (0x3b : replicate 8 0xff))
          , (limit, BS.pack (0xc2 : magnitude))
          , (-limit - 1, BS.pack (0xc3 : magnitude))
          ]
    mapM_
      (\(number, expected) -> do
          encodeCBOR number `shouldBe` expected
          encodeCBORDirect number `shouldBe` expected
          decodeCBOR @Integer expected `shouldBe` Right number)
      cases
    decodeCBOR @Natural (BS.pack (0xc2 : magnitude)) `shouldBe` Right (fromInteger limit)
    encodeCBORDirect (fromInteger limit :: Natural) `shouldBe` BS.pack (0xc2 : magnitude)
    isLeft (decodeCBOR @Natural (BS.pack (0xc3 : magnitude))) `shouldBe` True
    isLeft (decodeCBOR @Integer (BS.pack [0xc2, 0x01])) `shouldBe` True

  it "uses the shortest exact floating point width for deterministic output" $ do
    encodeDeterministic (C.Float64 1.5) `shouldBe` BS.pack [0xf9, 0x3e, 0x00]
    encodeDeterministic (C.Float64 1000000.5) `shouldBe` BS.pack [0xfa, 0x49, 0x74, 0x24, 0x08]
    encodeDeterministic (C.Float32 (-0.0)) `shouldBe` BS.pack [0xf9, 0x80, 0x00]
    encodeDeterministic (C.Float64 (1 / 0)) `shouldBe` BS.pack [0xf9, 0x7c, 0x00]
    encodeDeterministic (C.Float64 (0 / 0)) `shouldBe` BS.pack [0xf9, 0x7e, 0x00]
    encodeDeterministic (C.Float64 1.1) `shouldBe` encode (C.Float64 1.1)
    let value = C.Map (V.fromList [(C.TextString "pad", C.UInt 2), (C.Float64 1.5, C.UInt 1)])
    encodeDeterministic value
      `shouldBe` BS.pack [0xa2, 0xf9, 0x3e, 0x00, 0x01, 0x63, 0x70, 0x61, 0x64, 0x02]

  it "accepts a half-precision epoch timestamp in the default tag registry" $ do
    let wire = encodeDeterministic (C.Tag 1 (C.Float64 1.5))
    case (lookupTag 1 defaultCBORTagRegistry, decode wire) of
      (Just handler, Right (C.Tag 1 value)) ->
        thValidate handler value `shouldBe` Right (C.Float16 1.5)
      _ -> expectationFailure "Expected an epoch tag and its handler"


{- | RFC 8949 Appendix A conformance vectors (embedded).
Each entry: (description, hex bytes, expected decoded Value, roundtrip?)
We test decode of hex bytes matches expected value, and for roundtrip entries,
re-encoding produces the original bytes.
-}
rfc8949ConformanceVectors :: Spec
rfc8949ConformanceVectors =
  describe "RFC 8949 Appendix A conformance vectors" $
    sequence_
      [ describe "Decode conformance" $ mapM_ mkDecodeTest decodeVectors
      , describe "Roundtrip conformance" $ mapM_ mkRoundtripTest roundtripVectors
      , describe "Decode-only (indefinite-length)" $ mapM_ mkDecodeTest indefiniteVectors
      ]
  where
    mkDecodeTest (name, hexBytes, expected) =
      it name $ do
        let wire = BS.pack hexBytes
        decode wire `shouldBe` Right expected
        checkStreamSplits wire expected

    mkRoundtripTest (name, hexBytes, expected) =
      it name $ do
        let bs = BS.pack hexBytes
        decode bs `shouldBe` Right expected
        encode expected `shouldBe` bs
        checkStreamSplits bs expected

    -- Vectors where roundtrip==true and we have a decoded value
    roundtripVectors :: [(String, [Word8], C.Value)]
    roundtripVectors =
      -- Unsigned integers
      [ ("uint 0", [0x00], C.UInt 0)
      , ("uint 1", [0x01], C.UInt 1)
      , ("uint 10", [0x0a], C.UInt 10)
      , ("uint 23", [0x17], C.UInt 23)
      , ("uint 24", [0x18, 0x18], C.UInt 24)
      , ("uint 25", [0x18, 0x19], C.UInt 25)
      , ("uint 100", [0x18, 0x64], C.UInt 100)
      , ("uint 1000", [0x19, 0x03, 0xe8], C.UInt 1000)
      , ("uint 1000000", [0x1a, 0x00, 0x0f, 0x42, 0x40], C.UInt 1000000)
      ,
        ( "uint 1000000000000"
        , [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]
        , C.UInt 1000000000000
        )
      ,
        ( "uint max64"
        , [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
        , C.UInt 18446744073709551615
        )
      , -- Negative integers
        ("nint -1", [0x20], C.NInt 0)
      , ("nint -10", [0x29], C.NInt 9)
      , ("nint -100", [0x38, 0x63], C.NInt 99)
      , ("nint -1000", [0x39, 0x03, 0xe7], C.NInt 999)
      , -- Booleans and special
        ("false", [0xf4], C.Bool False)
      , ("true", [0xf5], C.Bool True)
      , ("null", [0xf6], C.Null)
      , ("undefined", [0xf7], C.Undefined)
      , -- Simple values
        ("simple(16)", [0xf0], C.Simple 16)
      , ("simple(32)", [0xf8, 0x20], C.Simple 32)
      , ("simple(255)", [0xf8, 0xff], C.Simple 255)
      , -- Byte strings
        ("bytes empty", [0x40], C.ByteString BS.empty)
      ,
        ( "bytes 01020304"
        , [0x44, 0x01, 0x02, 0x03, 0x04]
        , C.ByteString (BS.pack [0x01, 0x02, 0x03, 0x04])
        )
      , -- Text strings
        ("text empty", [0x60], C.TextString "")
      , ("text \"a\"", [0x61, 0x61], C.TextString "a")
      , ("text \"IETF\"", [0x64, 0x49, 0x45, 0x54, 0x46], C.TextString "IETF")
      ,
        ( "text \"\\\"\\\\\""
        , [0x62, 0x22, 0x5c]
        , C.TextString "\"\\"
        )
      ,
        ( "text \"\\u00fc\""
        , [0x62, 0xc3, 0xbc]
        , C.TextString "\252"
        )
      ,
        ( "text \"\\u6c34\""
        , [0x63, 0xe6, 0xb0, 0xb4]
        , C.TextString "\27700"
        )
      ,
        ( "text \"\\ud800\\udd51\" (U+10151)"
        , [0x64, 0xf0, 0x90, 0x85, 0x91]
        , C.TextString "\x10151"
        )
      , -- Arrays
        ("array empty", [0x80], C.Array V.empty)
      ,
        ( "array [1,2,3]"
        , [0x83, 0x01, 0x02, 0x03]
        , C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3])
        )
      ,
        ( "array [1,[2,3],[4,5]]"
        , [0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05]
        , C.Array
            ( V.fromList
                [ C.UInt 1
                , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                , C.Array (V.fromList [C.UInt 4, C.UInt 5])
                ]
            )
        )
      ,
        ( "array 25 elements"
        , [0x98, 0x19] ++ [0x01 .. 0x17] ++ [0x18, 0x18, 0x18, 0x19]
        , C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1 .. 25 :: Int]])
        )
      , -- Maps
        ("map empty", [0xa0], C.Map V.empty)
      ,
        ( "map {\"a\":1, \"b\":[2,3]}"
        , [0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03]
        , C.Map
            ( V.fromList
                [ (C.TextString "a", C.UInt 1)
                , (C.TextString "b", C.Array (V.fromList [C.UInt 2, C.UInt 3]))
                ]
            )
        )
      ,
        ( "map {\"a\":\"A\",\"b\":\"B\",\"c\":\"C\",\"d\":\"D\",\"e\":\"E\"}"
        ,
          [ 0xa5
          , 0x61
          , 0x61
          , 0x61
          , 0x41
          , 0x61
          , 0x62
          , 0x61
          , 0x42
          , 0x61
          , 0x63
          , 0x61
          , 0x43
          , 0x61
          , 0x64
          , 0x61
          , 0x44
          , 0x61
          , 0x65
          , 0x61
          , 0x45
          ]
        , C.Map
            ( V.fromList
                [ (C.TextString "a", C.TextString "A")
                , (C.TextString "b", C.TextString "B")
                , (C.TextString "c", C.TextString "C")
                , (C.TextString "d", C.TextString "D")
                , (C.TextString "e", C.TextString "E")
                ]
            )
        )
      , -- Tags

        ( "tag 0 date string"
        ,
          [ 0xc0
          , 0x74
          , 0x32
          , 0x30
          , 0x31
          , 0x33
          , 0x2d
          , 0x30
          , 0x33
          , 0x2d
          , 0x32
          , 0x31
          , 0x54
          , 0x32
          , 0x30
          , 0x3a
          , 0x30
          , 0x34
          , 0x3a
          , 0x30
          , 0x30
          , 0x5a
          ]
        , C.Tag 0 (C.TextString "2013-03-21T20:04:00Z")
        )
      ,
        ( "tag 1 epoch"
        , [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0]
        , C.Tag 1 (C.UInt 1363896240)
        )
      ,
        ( "tag 23 h'01020304'"
        , [0xd7, 0x44, 0x01, 0x02, 0x03, 0x04]
        , C.Tag 23 (C.ByteString (BS.pack [0x01, 0x02, 0x03, 0x04]))
        )
      ,
        ( "tag 24 h'6449455446'"
        , [0xd8, 0x18, 0x45, 0x64, 0x49, 0x45, 0x54, 0x46]
        , C.Tag 24 (C.ByteString (BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]))
        )
      ,
        ( "tag 32 URI"
        ,
          [ 0xd8
          , 0x20
          , 0x76
          , 0x68
          , 0x74
          , 0x74
          , 0x70
          , 0x3a
          , 0x2f
          , 0x2f
          , 0x77
          , 0x77
          , 0x77
          , 0x2e
          , 0x65
          , 0x78
          , 0x61
          , 0x6d
          , 0x70
          , 0x6c
          , 0x65
          , 0x2e
          , 0x63
          , 0x6f
          , 0x6d
          ]
        , C.Tag 32 (C.TextString "http://www.example.com")
        )
      , -- Mixed array+map

        ( "[\"a\", {\"b\": \"c\"}]"
        , [0x82, 0x61, 0x61, 0xa1, 0x61, 0x62, 0x61, 0x63]
        , C.Array
            ( V.fromList
                [ C.TextString "a"
                , C.Map (V.fromList [(C.TextString "b", C.TextString "c")])
                ]
            )
        )
      ]

    -- Decode-only vectors: we verify decoding works, but re-encoding may differ
    decodeVectors :: [(String, [Word8], C.Value)]
    decodeVectors =
      -- Float16 decode tests (half-precision are decoded to Float16)
      [ ("half 0.0", [0xf9, 0x00, 0x00], C.Float16 0.0)
      , ("half -0.0", [0xf9, 0x80, 0x00], C.Float16 (-0.0))
      , ("half 1.0", [0xf9, 0x3c, 0x00], C.Float16 1.0)
      , ("half 1.5", [0xf9, 0x3e, 0x00], C.Float16 1.5)
      , ("half 65504", [0xf9, 0x7b, 0xff], C.Float16 65504.0)
      , ("half -4.0", [0xf9, 0xc4, 0x00], C.Float16 (-4.0))
      , -- Float32 decode tests

        ( "float32 100000.0"
        , [0xfa, 0x47, 0xc3, 0x50, 0x00]
        , C.Float32 100000.0
        )
      , -- Float64 decode tests

        ( "float64 1.1"
        , [0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a]
        , C.Float64 1.1
        )
      ,
        ( "float64 -4.1"
        , [0xfb, 0xc0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66]
        , C.Float64 (-4.1)
        )
      ,
        ( "float64 1e300"
        , [0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c]
        , C.Float64 1.0e300
        )
      , -- Bignum tagged (>64-bit) — just verify we can decode the tag

        ( "bignum 2^64"
        , [0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        , C.Tag 2 (C.ByteString (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
        )
      ,
        ( "neg bignum -2^64-1"
        , [0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
        , C.Tag 3 (C.ByteString (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
        )
      , -- neg max Word64

        ( "nint -18446744073709551616"
        , [0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
        , C.NInt 18446744073709551615
        )
      , -- tag 1 with float

        ( "tag 1 epoch float"
        , [0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00]
        , C.Tag 1 (C.Float64 1363896240.5)
        )
      ]

    -- Indefinite-length test vectors
    indefiniteVectors :: [(String, [Word8], C.Value)]
    indefiniteVectors =
      [
        ( "indef array []"
        , [0x9f, 0xff]
        , C.Array V.empty
        )
      ,
        ( "indef array [1,[2,3],[4,5]]"
        , [0x9f, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff, 0xff]
        , C.Array
            ( V.fromList
                [ C.UInt 1
                , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                , C.Array (V.fromList [C.UInt 4, C.UInt 5])
                ]
            )
        )
      ,
        ( "indef [1,[2,3],[4,5]] v2"
        , [0x9f, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05, 0xff]
        , C.Array
            ( V.fromList
                [ C.UInt 1
                , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                , C.Array (V.fromList [C.UInt 4, C.UInt 5])
                ]
            )
        )
      ,
        ( "mixed def/indef [1,[2,3],indef[4,5]]"
        , [0x83, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff]
        , C.Array
            ( V.fromList
                [ C.UInt 1
                , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                , C.Array (V.fromList [C.UInt 4, C.UInt 5])
                ]
            )
        )
      ,
        ( "mixed [1,indef[2,3],[4,5]]"
        , [0x83, 0x01, 0x9f, 0x02, 0x03, 0xff, 0x82, 0x04, 0x05]
        , C.Array
            ( V.fromList
                [ C.UInt 1
                , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                , C.Array (V.fromList [C.UInt 4, C.UInt 5])
                ]
            )
        )
      ,
        ( "indef 25 elements"
        , [0x9f] ++ [0x01 .. 0x17] ++ [0x18, 0x18, 0x18, 0x19, 0xff]
        , C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1 .. 25 :: Int]])
        )
      ,
        ( "indef map {\"a\":1,\"b\":[2,3]}"
        , [0xbf, 0x61, 0x61, 0x01, 0x61, 0x62, 0x9f, 0x02, 0x03, 0xff, 0xff]
        , C.Map
            ( V.fromList
                [ (C.TextString "a", C.UInt 1)
                , (C.TextString "b", C.Array (V.fromList [C.UInt 2, C.UInt 3]))
                ]
            )
        )
      ,
        ( "[\"a\", indef{\"b\":\"c\"}]"
        , [0x82, 0x61, 0x61, 0xbf, 0x61, 0x62, 0x61, 0x63, 0xff]
        , C.Array
            ( V.fromList
                [ C.TextString "a"
                , C.Map (V.fromList [(C.TextString "b", C.TextString "c")])
                ]
            )
        )
      ,
        ( "indef {\"Fun\":true,\"Amt\":-2}"
        , [0xbf, 0x63, 0x46, 0x75, 0x6e, 0xf5, 0x63, 0x41, 0x6d, 0x74, 0x21, 0xff]
        , C.Map
            ( V.fromList
                [ (C.TextString "Fun", C.Bool True)
                , (C.TextString "Amt", C.NInt 1)
                ]
            )
        )
      ,
        ( "indef text \"streaming\""
        , [0x7f, 0x65, 0x73, 0x74, 0x72, 0x65, 0x61, 0x64, 0x6d, 0x69, 0x6e, 0x67, 0xff]
        , C.TextString "streaming"
        )
      ]


-- | Property-based roundtrip tests.
propertyRoundtrips :: Spec
propertyRoundtrips =
  describe "Property roundtrips" $
    sequence_
      [ it "UInt roundtrip" $ property $ do
          n <- forAll $ Gen.word64 Range.linearBounded
          let val = C.UInt n
          decode (encode val) === Right val
      , it "NInt roundtrip" $ property $ do
          n <- forAll $ Gen.word64 Range.linearBounded
          let val = C.NInt n
          decode (encode val) === Right val
      , it "Bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          let val = C.Bool b
          decode (encode val) === Right val
      , it "Null roundtrip" $ property $ do
          decode (encode C.Null) === Right C.Null
      , it "ByteString roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 512)
          let val = C.ByteString bs
          decode (encode val) === Right val
      , it "TextString roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 256) Gen.unicode
          let val = C.TextString t
          decode (encode val) === Right val
      , it "Float32 roundtrip" $ property $ do
          f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
          let val = C.Float32 f
          decode (encode val) === Right val
      , it "Float64 roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
          let val = C.Float64 d
          decode (encode val) === Right val
      , it "Array of UInts roundtrip" $ property $ do
          ns <-
            forAll $
              Gen.list
                (Range.linear 0 50)
                (Gen.word64 (Range.linear 0 0xffffffff))
          let val = C.Array (V.fromList (map C.UInt ns))
          decode (encode val) === Right val
      , it "Map roundtrip" $ property $ do
          entries <- forAll $ Gen.list (Range.linear 0 30) $ do
            k <- Gen.text (Range.linear 1 32) Gen.alphaNum
            v <- Gen.word64 (Range.linear 0 0xffff)
            pure (C.TextString k, C.UInt v)
          let val = C.Map (V.fromList entries)
          decode (encode val) === Right val
      , it "Tag roundtrip" $ property $ do
          tagNum <- forAll $ Gen.word64 (Range.linear 0 0xffff)
          n <- forAll $ Gen.word64 (Range.linear 0 0xffffffff)
          let val = C.Tag tagNum (C.UInt n)
          decode (encode val) === Right val
      , it "Nested arrays roundtrip" $ property $ do
          inner <-
            forAll $
              Gen.list
                (Range.linear 0 10)
                (Gen.word64 (Range.linear 0 255))
          let val =
                C.Array
                  ( V.fromList
                      [ C.UInt 1
                      , C.Array (V.fromList (map C.UInt inner))
                      , C.TextString "nested"
                      ]
                  )
          decode (encode val) === Right val
      ]


-- | Edge cases.
edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "large integer (max Word64)" $ do
          let val = C.UInt maxBound
          decode (encode val) `shouldBe` Right val
      , it "large negative integer (max Word64)" $ do
          let val = C.NInt maxBound
          decode (encode val) `shouldBe` Right val
      , it "deeply nested" $ do
          let nest 0 = C.UInt 42
              nest n = C.Array (V.singleton (nest (n - 1)))
              val = nest (20 :: Int)
          decode (encode val) `shouldBe` Right val
      , it "tagged tagged value" $ do
          let val = C.Tag 0 (C.Tag 1 (C.TextString "epoch"))
          decode (encode val) `shouldBe` Right val
      , it "map with mixed key types" $ do
          let val =
                C.Map
                  ( V.fromList
                      [ (C.UInt 1, C.TextString "one")
                      , (C.TextString "two", C.UInt 2)
                      , (C.Bool True, C.Null)
                      ]
                  )
          decode (encode val) `shouldBe` Right val
      , it "empty byte string" $ do
          let val = C.ByteString BS.empty
          decode (encode val) `shouldBe` Right val
      , it "empty text string" $ do
          let val = C.TextString ""
          decode (encode val) `shouldBe` Right val
      , it "simple value 16" $ do
          let val = C.Simple 16
          decode (encode val) `shouldBe` Right val
      , it "simple value 255" $ do
          let val = C.Simple 255
          decode (encode val) `shouldBe` Right val
      , it "decode empty input" $
          case decode BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      ]


-- | JSON conversion tests.
jsonTests :: Spec
jsonTests =
  describe "JSON conversion" $
    sequence_
      [ it "UInt to JSON" $
          toJSON (C.UInt 42) `shouldBe` Aeson.Number 42
      , it "NInt to JSON" $
          toJSON (C.NInt 0) `shouldBe` Aeson.Number (-1)
      , it "Bool to JSON" $ do
          toJSON (C.Bool True) `shouldBe` Aeson.Bool True
          toJSON (C.Bool False) `shouldBe` Aeson.Bool False
      , it "Null to JSON" $
          toJSON C.Null `shouldBe` Aeson.Null
      , it "TextString to JSON" $
          toJSON (C.TextString "hello") `shouldBe` Aeson.String "hello"
      , it "uses unpadded base64url for byte strings" $
          toJSON (C.ByteString (BS.pack [0xff, 0xef])) `shouldBe` Aeson.String "_-8"
      , it "Array to JSON" $
          toJSON (C.Array (V.fromList [C.UInt 1, C.UInt 2]))
            `shouldBe` Aeson.Array (V.fromList [Aeson.Number 1, Aeson.Number 2])
      , it "Tag to JSON" $ do
          let json = toJSON (C.Tag 1 (C.UInt 42))
          case json of
            Aeson.Object _ -> pure ()
            _ -> expectationFailure "expected JSON object for tag"
      , it "fromJSON null" $
          fromJSON Aeson.Null `shouldBe` C.Null
      , it "fromJSON bool" $
          fromJSON (Aeson.Bool True) `shouldBe` C.Bool True
      , it "fromJSON string" $
          fromJSON (Aeson.String "hi") `shouldBe` C.TextString "hi"
      , it "fromJSON positive int" $
          fromJSON (Aeson.Number 42) `shouldBe` C.UInt 42
      , it "fromJSON negative int" $
          fromJSON (Aeson.Number (-1)) `shouldBe` C.NInt 0
      , it "preserves both ends of CBOR's basic integer range" $ do
          let boundaryValues = [C.UInt 9223372036854775809, C.UInt maxBound, C.NInt 9223372036854775808, C.NInt maxBound]
          mapM_ (\value -> fromJSON (toJSON value) `shouldBe` value) boundaryValues
          fromJSON (Aeson.Number (-1.5)) `shouldBe` C.Float64 (-1.5)
      , it "keeps extra fields on an object with tag and value keys" $ do
          let value = C.Map (V.fromList [(C.TextString "tag", C.UInt 1), (C.TextString "value", C.UInt 2), (C.TextString "name", C.TextString "client")])
              json = toJSON value
          toJSON (fromJSON json) `shouldBe` json
          fromJSON (toJSON (C.Tag 1 (C.UInt 2))) `shouldBe` C.Tag 1 (C.UInt 2)
      , it "fromJSON array" $
          fromJSON (Aeson.Array (V.fromList [Aeson.Number 1]))
            `shouldBe` C.Array (V.fromList [C.UInt 1])
      ]


deterministicTests :: Spec
deterministicTests =
  describe "Deterministic encoding" $
    sequence_
      [ it "sorted map keys by length first" $ do
          let val =
                C.Map
                  ( V.fromList
                      [ (C.TextString "bb", C.UInt 2)
                      , (C.UInt 1, C.UInt 1)
                      , (C.TextString "a", C.UInt 3)
                      ]
                  )
              bs = encodeDeterministic val
          case decode bs of
            Right (C.Map kvs) -> do
              let keys = V.toList (V.map fst kvs)
              keys `shouldBe` [C.UInt 1, C.TextString "a", C.TextString "bb"]
            other -> expectationFailure $ "expected Map, got: " ++ show other
      , it "deterministic roundtrip" $ do
          let val =
                C.Map
                  ( V.fromList
                      [ (C.TextString "z", C.UInt 1)
                      , (C.TextString "a", C.UInt 2)
                      ]
                  )
              bs = encodeDeterministic val
          case decode bs of
            Right decoded -> do
              case decoded of
                C.Map kvs -> V.map fst kvs `shouldBe` V.fromList [C.TextString "a", C.TextString "z"]
                _ -> expectationFailure "expected map"
            Left err -> expectationFailure err
      , it "deterministic preserves non-map values" $ do
          let val = C.Array (V.fromList [C.UInt 3, C.UInt 1, C.UInt 2])
          decode (encodeDeterministic val) `shouldBe` Right val
      , it "deterministic encoding is valid CBOR" $ do
          let val =
                C.Map
                  ( V.fromList
                      [ (C.TextString "key", C.UInt 42)
                      , (C.UInt 1, C.TextString "val")
                      ]
                  )
          case decode (encodeDeterministic val) of
            Right _ -> pure ()
            Left err -> expectationFailure err
      ]


sequenceTests :: Spec
sequenceTests =
  describe "CBOR Sequences (RFC 8742)" $
    sequence_
      [ it "encode/decode empty sequence" $ do
          let vals = V.empty
              bs = encodeSequence vals
          BS.length bs `shouldBe` 0
          decodeSequence bs `shouldBe` Right V.empty
      , it "encode/decode single item sequence" $ do
          let vals = V.singleton (C.UInt 42)
              bs = encodeSequence vals
          decodeSequence bs `shouldBe` Right vals
      , it "encode/decode multi-item sequence" $ do
          let vals = V.fromList [C.UInt 1, C.TextString "hello", C.Bool True]
              bs = encodeSequence vals
          decodeSequence bs `shouldBe` Right vals
      , it "sequence is concatenation of individual encodings" $ do
          let v1 = C.UInt 1
              v2 = C.TextString "hi"
              seq' = encodeSequence (V.fromList [v1, v2])
              cat = encode v1 <> encode v2
          seq' `shouldBe` cat
      , it "sequence roundtrip" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 20) (Gen.word64 (Range.linear 0 0xffff))
          let vals = V.fromList (map C.UInt ns)
          decodeSequence (encodeSequence vals) === Right vals
      ]
