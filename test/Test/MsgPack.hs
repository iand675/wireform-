module Test.MsgPack (msgPackTests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MsgPack.Decode (decode)
import MsgPack.Encode (encode)
import MsgPack.Encoding qualified as Encoding
import MsgPack.JSON (fromJSON, toJSON)
import MsgPack.Stream qualified as Stream
import MsgPack.Value qualified as MV
import Test.Syd
import Test.Syd.Hedgehog ()


msgPackTests :: Spec
msgPackTests =
  describe "MsgPack Encode/Decode" $
    sequence_
      [ propertyRoundtrip
      , unitExactEncoding
      , unitEdgeCases
      , unitLargeContainers
      , unitExtRoundtrip
      , unitTimestampRoundtrip
      , jsonConversionTests
      , conformanceVectors
      ]


--------------------------------------------------------------------------------
-- Property: roundtrip for each primitive type
--------------------------------------------------------------------------------

propertyRoundtrip :: Spec
propertyRoundtrip =
  describe "Roundtrip (property)" $
    sequence_
      [ it "Nil" $ property $ do
          let v = MV.Nil
          decode (encode v) === Right v
      , it "Bool" $ property $ do
          b <- forAll Gen.bool
          let v = MV.Bool b
          decode (encode v) === Right v
      , it "positive Word64" $ property $ do
          n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          let v = MV.Word n
          decode (encode v) === Right v
      , it "negative Int64" $ property $ do
          n <- forAll $ Gen.int64 (Range.linear minBound (-1))
          let v = MV.Int n
          decode (encode v) === Right v
      , it "Int64 full range" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          let v = if n >= 0 then MV.Word (fromIntegral n) else MV.Int n
              encoded = encode v
              decoded = decode encoded
          case decoded of
            Right (MV.Word w) | n >= 0 -> fromIntegral w === n
            Right (MV.Int i) | n < 0 -> i === n
            other -> do
              annotate (show other)
              failure
      , it "Float" $ property $ do
          f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
          let v = MV.Float f
          decode (encode v) === Right v
      , it "Double" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
          let v = MV.Double d
          decode (encode v) === Right v
      , it "String" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
          let v = MV.String t
          decode (encode v) === Right v
      , it "Binary" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 256)
          let v = MV.Binary bs
          decode (encode v) === Right v
      , it "Array of ints" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 30) (Gen.word64 (Range.linear 0 0xFFFF))
          let v = MV.Array (V.fromList (map MV.Word ns))
          decode (encode v) === Right v
      , it "Map of string->int" $ property $ do
          entries <- forAll $ Gen.list (Range.linear 0 20) $ do
            k <- Gen.text (Range.linear 1 16) Gen.alphaNum
            n <- Gen.int64 (Range.linear (-1000) 1000)
            pure (k, n)
          let v =
                MV.Map
                  ( V.fromList
                      [ (MV.String k, if n >= 0 then MV.Word (fromIntegral n) else MV.Int n)
                      | (k, n) <- entries
                      ]
                  )
          decode (encode v) === Right v
      ]


--------------------------------------------------------------------------------
-- Unit: exact byte encoding
--------------------------------------------------------------------------------

unitExactEncoding :: Spec
unitExactEncoding =
  describe "Exact byte encoding" $
    sequence_
      [ it "nil = 0xc0" $
          encode MV.Nil `shouldBe` BS.pack [0xc0]
      , it "false = 0xc2" $
          encode (MV.Bool False) `shouldBe` BS.pack [0xc2]
      , it "true = 0xc3" $
          encode (MV.Bool True) `shouldBe` BS.pack [0xc3]
      , -- fixint
        it "fixint 0 = 0x00" $
          encode (MV.Word 0) `shouldBe` BS.pack [0x00]
      , it "fixint 127 = 0x7f" $
          encode (MV.Word 127) `shouldBe` BS.pack [0x7f]
      , it "fixint -1 = 0xff" $
          encode (MV.Int (-1)) `shouldBe` BS.pack [0xff]
      , it "fixint -32 = 0xe0" $
          encode (MV.Int (-32)) `shouldBe` BS.pack [0xe0]
      , -- uint8
        it "uint8 128 = 0xcc 0x80" $
          encode (MV.Word 128) `shouldBe` BS.pack [0xcc, 0x80]
      , it "uint8 255 = 0xcc 0xff" $
          encode (MV.Word 255) `shouldBe` BS.pack [0xcc, 0xff]
      , -- uint16
        it "uint16 256 = 0xcd 0x01 0x00" $
          encode (MV.Word 256) `shouldBe` BS.pack [0xcd, 0x01, 0x00]
      , -- int8
        it "int8 -33 = 0xd0 0xdf" $
          encode (MV.Int (-33)) `shouldBe` BS.pack [0xd0, 0xdf]
      , it "int8 -128 = 0xd0 0x80" $
          encode (MV.Int (-128)) `shouldBe` BS.pack [0xd0, 0x80]
      , -- int16
        it "int16 -129 = 0xd1 0xff 0x7f" $
          encode (MV.Int (-129)) `shouldBe` BS.pack [0xd1, 0xff, 0x7f]
      , -- fixstr
        it "fixstr \"\" = 0xa0" $
          encode (MV.String "") `shouldBe` BS.pack [0xa0]
      , it "fixstr \"hello\" = 0xa5 + bytes" $
          encode (MV.String "hello") `shouldBe` BS.pack [0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f]
      , -- fixarray
        it "fixarray [] = 0x90" $
          encode (MV.Array V.empty) `shouldBe` BS.pack [0x90]
      , it "fixarray [1,2] = 0x92 0x01 0x02" $
          encode (MV.Array (V.fromList [MV.Word 1, MV.Word 2]))
            `shouldBe` BS.pack [0x92, 0x01, 0x02]
      , -- fixmap
        it "fixmap {} = 0x80" $
          encode (MV.Map V.empty) `shouldBe` BS.pack [0x80]
      , it "fixmap {1:2} = 0x81 0x01 0x02" $
          encode (MV.Map (V.fromList [(MV.Word 1, MV.Word 2)]))
            `shouldBe` BS.pack [0x81, 0x01, 0x02]
      , -- float32
        it "float32 0.0" $
          encode (MV.Float 0.0) `shouldBe` BS.pack [0xca, 0x00, 0x00, 0x00, 0x00]
      , -- float64
        it "float64 0.0" $
          encode (MV.Double 0.0) `shouldBe` BS.pack [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      , -- bin8
        it "bin8 empty" $
          encode (MV.Binary BS.empty) `shouldBe` BS.pack [0xc4, 0x00]
      , it "bin8 [0xDE, 0xAD]" $
          encode (MV.Binary (BS.pack [0xDE, 0xAD])) `shouldBe` BS.pack [0xc4, 0x02, 0xDE, 0xAD]
      ]


--------------------------------------------------------------------------------
-- Unit: edge cases
--------------------------------------------------------------------------------

unitEdgeCases :: Spec
unitEdgeCases =
  describe "Edge cases" $
    sequence_
      [ it "empty array roundtrip" $ do
          let v = MV.Array V.empty
          decode (encode v) `shouldBe` Right v
      , it "empty map roundtrip" $ do
          let v = MV.Map V.empty
          decode (encode v) `shouldBe` Right v
      , it "empty string roundtrip" $ do
          let v = MV.String ""
          decode (encode v) `shouldBe` Right v
      , it "empty binary roundtrip" $ do
          let v = MV.Binary BS.empty
          decode (encode v) `shouldBe` Right v
      , it "nil roundtrip" $ do
          decode (encode MV.Nil) `shouldBe` Right MV.Nil
      , it "max Word64" $ do
          let v = MV.Word maxBound
          decode (encode v) `shouldBe` Right v
      , it "min Int64" $ do
          let v = MV.Int minBound
          decode (encode v) `shouldBe` Right v
      , it "max Int64 as Word" $ do
          let v = MV.Word (fromIntegral (maxBound :: Int64))
          decode (encode v) `shouldBe` Right v
      , it "Word 0" $ do
          let v = MV.Word 0
          decode (encode v) `shouldBe` Right v
      , it "Int -1" $ do
          let v = MV.Int (-1)
          decode (encode v) `shouldBe` Right v
      , it "nested array" $ do
          let v = MV.Array (V.fromList [MV.Array (V.fromList [MV.Word 1, MV.Word 2]), MV.String "x"])
          decode (encode v) `shouldBe` Right v
      , it "nested map" $ do
          let v =
                MV.Map
                  ( V.fromList
                      [(MV.String "inner", MV.Map (V.fromList [(MV.Word 1, MV.Bool True)]))]
                  )
          decode (encode v) `shouldBe` Right v
      , it "decode empty input fails" $
          case decode BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected failure on empty input"
      , it "str8 (32+ bytes)" $ do
          let t = T.replicate 32 "x"
              v = MV.String t
          decode (encode v) `shouldBe` Right v
      , it "str16 (256+ bytes)" $ do
          let t = T.replicate 300 "a"
              v = MV.String t
          decode (encode v) `shouldBe` Right v
      , it "bin16 (256+ bytes)" $ do
          let bs = BS.replicate 300 0x42
              v = MV.Binary bs
          decode (encode v) `shouldBe` Right v
      ]


--------------------------------------------------------------------------------
-- Unit: large containers (> 15 elements, triggers array16/map16)
--------------------------------------------------------------------------------

unitLargeContainers :: Spec
unitLargeContainers =
  describe "Large containers (array16/map16)" $
    sequence_
      [ it "array with 16 elements" $ do
          let v = MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [0 .. 15 :: Int]])
          decode (encode v) `shouldBe` Right v
      , it "array with 20 elements" $ do
          let v = MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [0 .. 19 :: Int]])
              bs = encode v
          BS.index bs 0 `shouldBe` 0xdc -- array16 tag
          decode bs `shouldBe` Right v
      , it "array with 100 elements" $ do
          let v = MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [0 .. 99 :: Int]])
          decode (encode v) `shouldBe` Right v
      , it "map with 16 entries" $ do
          let v =
                MV.Map
                  ( V.fromList
                      [(MV.String (T.pack (show i)), MV.Word (fromIntegral i)) | i <- [0 .. 15 :: Int]]
                  )
          decode (encode v) `shouldBe` Right v
      , it "map with 20 entries" $ do
          let v =
                MV.Map
                  ( V.fromList
                      [(MV.String (T.pack (show i)), MV.Word (fromIntegral i)) | i <- [0 .. 19 :: Int]]
                  )
              bs = encode v
          BS.index bs 0 `shouldBe` 0xde -- map16 tag
          decode bs `shouldBe` Right v
      ]


--------------------------------------------------------------------------------
-- Unit: Ext type roundtrip
--------------------------------------------------------------------------------

unitExtRoundtrip :: Spec
unitExtRoundtrip =
  describe "Ext type roundtrip" $
    sequence_
      [ it "fixext1" $ do
          let v = MV.Ext 1 (BS.pack [0x42])
          decode (encode v) `shouldBe` Right v
      , it "fixext2" $ do
          let v = MV.Ext 2 (BS.pack [0x42, 0x43])
          decode (encode v) `shouldBe` Right v
      , it "fixext4" $ do
          let v = MV.Ext 3 (BS.pack [0x01, 0x02, 0x03, 0x04])
          decode (encode v) `shouldBe` Right v
      , it "fixext8" $ do
          let v = MV.Ext 4 (BS.replicate 8 0xAA)
          decode (encode v) `shouldBe` Right v
      , it "fixext16" $ do
          let v = MV.Ext 5 (BS.replicate 16 0xBB)
          decode (encode v) `shouldBe` Right v
      , it "ext8 (3 bytes)" $ do
          let v = MV.Ext 10 (BS.pack [0x01, 0x02, 0x03])
          decode (encode v) `shouldBe` Right v
      , it "ext8 (100 bytes)" $ do
          let v = MV.Ext 42 (BS.replicate 100 0xCC)
          decode (encode v) `shouldBe` Right v
      , it "negative ext type" $ do
          let v = MV.Ext (-5) (BS.pack [0x01])
          decode (encode v) `shouldBe` Right v
      ]


--------------------------------------------------------------------------------
-- Unit: Timestamp roundtrip
--------------------------------------------------------------------------------

unitTimestampRoundtrip :: Spec
unitTimestampRoundtrip =
  describe "Timestamp roundtrip" $
    sequence_
      [ it "timestamp32 (seconds only)" $ do
          let v = MV.Timestamp 1000 0
          decode (encode v) `shouldBe` Right v
      , it "timestamp32 (max uint32 seconds)" $ do
          let v = MV.Timestamp 0xFFFFFFFF 0
          decode (encode v) `shouldBe` Right v
      , it "timestamp32 (zero)" $ do
          let v = MV.Timestamp 0 0
          decode (encode v) `shouldBe` Right v
      , it "timestamp64 (with nanoseconds)" $ do
          let v = MV.Timestamp 1000 500000000
          decode (encode v) `shouldBe` Right v
      , it "timestamp96 (negative seconds)" $ do
          let v = MV.Timestamp (-1) 0
          decode (encode v) `shouldBe` Right v
      , it "timestamp96 (large negative)" $ do
          let v = MV.Timestamp (-1000000) 123456
          decode (encode v) `shouldBe` Right v
      , it "matches canonical timestamp64 and timestamp96 bytes at the nanosecond limit" $ do
          let check seconds bytes = do
                let value = MV.Timestamp seconds 999999999
                    wire = BS.pack bytes
                encode value `shouldBe` wire
                Encoding.encodingToByteString (Encoding.timestamp seconds 999999999) `shouldBe` wire
                decode wire `shouldBe` Right value
                case Stream.feedMore (Stream.streamDecode (BS.init wire)) (BS.singleton (BS.last wire)) of
                  Stream.Done actual rest -> do
                    actual `shouldBe` value
                    rest `shouldBe` BS.empty
                  step -> expectationFailure ("expected timestamp, got " ++ show step)
          check 0 [0xd7, 0xff, 0xee, 0x6b, 0x27, 0xfc, 0, 0, 0, 0]
          check (-1) ([0xc7, 12, 0xff, 0x3b, 0x9a, 0xc9, 0xff] ++ replicate 8 0xff)
      , it "rejects malformed timestamp64 and timestamp96 in direct and streaming decodes" $ do
          let check bytes = do
                let wire = BS.pack bytes
                isLeft (decode wire) `shouldBe` True
                case Stream.feedMore
                  (Stream.streamDecode (BS.cons 0x91 (BS.init wire)))
                  (BS.singleton (BS.last wire)) of
                  Stream.Fail _ -> pure ()
                  step -> expectationFailure ("expected invalid timestamp to fail, got " ++ show step)
          check [0xd7, 0xff, 0xee, 0x6b, 0x28, 0, 0, 0, 0, 0]
          check ([0xc7, 12, 0xff, 0x3b, 0x9a, 0xca, 0] ++ replicate 8 0xff)
      , it "rejects encoding an out-of-range timestamp, including extension type -1" $ do
          let reject bytes = do
                result <- try (evaluate (BS.length bytes)) :: IO (Either ErrorCall Int)
                isLeft result `shouldBe` True
              payload = BS.pack [0xee, 0x6b, 0x28, 0, 0, 0, 0, 0]
          mapM_
            ( \seconds -> do
                reject (encode (MV.Timestamp seconds 1000000000))
                reject (Encoding.encodingToByteString (Encoding.timestamp seconds 1000000000))
            )
            [0, -1]
          reject (encode (MV.Ext (-1) payload))
          reject (Encoding.encodingToByteString (Encoding.ext (-1) payload))
      ]


--------------------------------------------------------------------------------
-- JSON conversion tests
--------------------------------------------------------------------------------

jsonConversionTests :: Spec
jsonConversionTests =
  describe "JSON conversion" $
    sequence_
      [ it "nil -> null -> nil" $ do
          let v = MV.Nil
          fromJSON (toJSON v) `shouldBe` v
      , it "bool true roundtrip" $ do
          let v = MV.Bool True
          fromJSON (toJSON v) `shouldBe` v
      , it "bool false roundtrip" $ do
          let v = MV.Bool False
          fromJSON (toJSON v) `shouldBe` v
      , it "small int roundtrip" $ do
          let v = MV.Int (-42)
          fromJSON (toJSON v) `shouldBe` v
      , it "word roundtrip" $ do
          let v = MV.Word 12345
              result = fromJSON (toJSON v)
          case result of
            MV.Int n -> fromIntegral n `shouldBe` (12345 :: Word64)
            MV.Word n -> n `shouldBe` 12345
            other -> expectationFailure $ "unexpected: " ++ show other
      , it "string roundtrip" $ do
          let v = MV.String "hello world"
          fromJSON (toJSON v) `shouldBe` v
      , it "array roundtrip" $ do
          let v = MV.Array (V.fromList [MV.Nil, MV.Bool True, MV.String "x"])
              result = fromJSON (toJSON v)
          case result of
            MV.Array elems -> V.length elems `shouldBe` 3
            other -> expectationFailure $ "unexpected: " ++ show other
      , it "map with string keys -> object" $ do
          let v = MV.Map (V.fromList [(MV.String "a", MV.Bool True)])
              result = fromJSON (toJSON v)
          case result of
            MV.Map kvs -> V.length kvs `shouldBe` 1
            other -> expectationFailure $ "unexpected: " ++ show other
      ]


--------------------------------------------------------------------------------
-- Conformance: kawanet/msgpack-test-suite vectors (embedded)
-- For each entry, we verify that the FIRST msgpack encoding (the most compact
-- canonical form) decodes to the expected value.
--------------------------------------------------------------------------------

parseHexDash :: String -> [Word8]
parseHexDash = go
  where
    go [] = []
    go ('-' : rest) = go rest
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = error "parseHexDash: odd number of hex digits"

    digitToInt :: Char -> Int
    digitToInt c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = error $ "digitToInt: invalid hex: " ++ [c]


conformanceVectors :: Spec
conformanceVectors =
  describe "kawanet/msgpack-test-suite conformance" $
    sequence_
      [ describe "nil" $ sequence_ nilDecodeTests
      , describe "bool" $ sequence_ boolDecodeTests
      , describe "positive numbers" $ sequence_ positiveNumberDecodeTests
      , describe "negative numbers" $ sequence_ negativeNumberDecodeTests
      , describe "strings" $ sequence_ stringDecodeTests
      , describe "binary" $ sequence_ binaryDecodeTests
      , describe "arrays" $ sequence_ arrayDecodeTests
      , describe "maps" $ sequence_ mapDecodeTests
      , describe "ext" $ sequence_ extDecodeTests
      ]
  where
    mkDecodeTest :: String -> String -> MV.Value -> Spec
    mkDecodeTest name hex expected =
      it (name ++ " [" ++ hex ++ "]") $
        decode (BS.pack (parseHexDash hex)) `shouldBe` Right expected

    nilDecodeTests =
      [mkDecodeTest "nil" "c0" MV.Nil]

    boolDecodeTests =
      [ mkDecodeTest "false" "c2" (MV.Bool False)
      , mkDecodeTest "true" "c3" (MV.Bool True)
      ]

    positiveNumberDecodeTests =
      [ mkDecodeTest "0 fixint" "00" (MV.Word 0)
      , mkDecodeTest "1 fixint" "01" (MV.Word 1)
      , mkDecodeTest "127 fixint" "7f" (MV.Word 127)
      , mkDecodeTest "128 uint8" "cc-80" (MV.Word 128)
      , mkDecodeTest "255 uint8" "cc-ff" (MV.Word 255)
      , mkDecodeTest "256 uint16" "cd-01-00" (MV.Word 256)
      , mkDecodeTest "65535 uint16" "cd-ff-ff" (MV.Word 65535)
      , mkDecodeTest "65536 uint32" "ce-00-01-00-00" (MV.Word 65536)
      , mkDecodeTest "2147483647 uint32" "ce-7f-ff-ff-ff" (MV.Word 2147483647)
      , mkDecodeTest "2147483648 uint32" "ce-80-00-00-00" (MV.Word 2147483648)
      , mkDecodeTest "4294967295 uint32" "ce-ff-ff-ff-ff" (MV.Word 4294967295)
      , mkDecodeTest "4294967296 uint64" "cf-00-00-00-01-00-00-00-00" (MV.Word 4294967296)
      , mkDecodeTest "max uint64" "cf-ff-ff-ff-ff-ff-ff-ff-ff" (MV.Word 18446744073709551615)
      ]

    negativeNumberDecodeTests =
      [ mkDecodeTest "-1 fixint" "ff" (MV.Int (-1))
      , mkDecodeTest "-32 fixint" "e0" (MV.Int (-32))
      , mkDecodeTest "-33 int8" "d0-df" (MV.Int (-33))
      , mkDecodeTest "-128 int8" "d0-80" (MV.Int (-128))
      , mkDecodeTest "-256 int16" "d1-ff-00" (MV.Int (-256))
      , mkDecodeTest "-32768 int16" "d1-80-00" (MV.Int (-32768))
      , mkDecodeTest "-65536 int32" "d2-ff-ff-00-00" (MV.Int (-65536))
      , mkDecodeTest "-2147483648 int32" "d2-80-00-00-00" (MV.Int (-2147483648))
      ]

    stringDecodeTests =
      [ mkDecodeTest "empty string" "a0" (MV.String "")
      , mkDecodeTest "\"a\"" "a1-61" (MV.String "a")
      , mkDecodeTest
          "31-char fixstr"
          "bf-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31"
          (MV.String "1234567890123456789012345678901")
      , mkDecodeTest
          "32-char str8"
          "d9-20-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31-32"
          (MV.String "12345678901234567890123456789012")
      , -- UTF-8 strings
        mkDecodeTest
          "Cyrillic"
          "b2-d0-9a-d0-b8-d1-80-d0-b8-d0-bb-d0-bb-d0-b8-d1-86-d0-b0"
          (MV.String "\1050\1080\1088\1080\1083\1083\1080\1094\1072")
      , mkDecodeTest
          "Hiragana"
          "ac-e3-81-b2-e3-82-89-e3-81-8c-e3-81-aa"
          (MV.String "\12402\12425\12364\12394")
      , mkDecodeTest
          "Korean"
          "a6-ed-95-9c-ea-b8-80"
          (MV.String "\54620\44544")
      , mkDecodeTest
          "Emoji heart"
          "a3-e2-9d-a4"
          (MV.String "\10084")
      , mkDecodeTest
          "Emoji beer"
          "a4-f0-9f-8d-ba"
          (MV.String "\127866")
      ]

    binaryDecodeTests =
      [ mkDecodeTest "empty bin" "c4-00" (MV.Binary BS.empty)
      , mkDecodeTest "bin [0x01]" "c4-01-01" (MV.Binary (BS.pack [0x01]))
      , mkDecodeTest "bin [0x00,0xff]" "c4-02-00-ff" (MV.Binary (BS.pack [0x00, 0xff]))
      ]

    arrayDecodeTests =
      [ mkDecodeTest "empty array" "90" (MV.Array V.empty)
      , mkDecodeTest "array [1]" "91-01" (MV.Array (V.fromList [MV.Word 1]))
      , mkDecodeTest
          "array [1..15]"
          "9f-01-02-03-04-05-06-07-08-09-0a-0b-0c-0d-0e-0f"
          (MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [1 .. 15 :: Int]]))
      , mkDecodeTest
          "array [1..16] array16"
          "dc-00-10-01-02-03-04-05-06-07-08-09-0a-0b-0c-0d-0e-0f-10"
          (MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [1 .. 16 :: Int]]))
      , mkDecodeTest
          "array [\"a\"]"
          "91-a1-61"
          (MV.Array (V.fromList [MV.String "a"]))
      , mkDecodeTest
          "nested [[]]"
          "91-90"
          (MV.Array (V.fromList [MV.Array V.empty]))
      , mkDecodeTest
          "nested [{}]"
          "91-80"
          (MV.Array (V.fromList [MV.Map V.empty]))
      ]

    mapDecodeTests =
      [ mkDecodeTest "empty map" "80" (MV.Map V.empty)
      , mkDecodeTest
          "map {\"a\":1}"
          "81-a1-61-01"
          (MV.Map (V.fromList [(MV.String "a", MV.Word 1)]))
      , mkDecodeTest
          "map {\"a\":\"A\"}"
          "81-a1-61-a1-41"
          (MV.Map (V.fromList [(MV.String "a", MV.String "A")]))
      , mkDecodeTest
          "nested {\"a\":{}}"
          "81-a1-61-80"
          (MV.Map (V.fromList [(MV.String "a", MV.Map V.empty)]))
      , mkDecodeTest
          "nested {\"a\":[]}"
          "81-a1-61-90"
          (MV.Map (V.fromList [(MV.String "a", MV.Array V.empty)]))
      ]

    extDecodeTests =
      [ mkDecodeTest
          "fixext1 type=1"
          "d4-01-10"
          (MV.Ext 1 (BS.pack [0x10]))
      , mkDecodeTest
          "fixext2 type=2"
          "d5-02-20-21"
          (MV.Ext 2 (BS.pack [0x20, 0x21]))
      , mkDecodeTest
          "fixext4 type=3"
          "d6-03-30-31-32-33"
          (MV.Ext 3 (BS.pack [0x30, 0x31, 0x32, 0x33]))
      , mkDecodeTest
          "fixext8 type=4"
          "d7-04-40-41-42-43-44-45-46-47"
          (MV.Ext 4 (BS.pack [0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47]))
      , mkDecodeTest
          "fixext16 type=5"
          "d8-05-50-51-52-53-54-55-56-57-58-59-5a-5b-5c-5d-5e-5f"
          (MV.Ext 5 (BS.pack [0x50 .. 0x5f]))
      , mkDecodeTest
          "ext8 type=6 empty"
          "c7-00-06"
          (MV.Ext 6 BS.empty)
      , mkDecodeTest
          "ext8 type=7 3bytes"
          "c7-03-07-70-71-72"
          (MV.Ext 7 (BS.pack [0x70, 0x71, 0x72]))
      ]
