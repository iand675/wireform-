module Test.CBOR (cborTests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8, Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified CBOR.Value as C
import CBOR.Encode (encode, encodeDeterministic, encodeDeterministicLengthFirst, encodeSequence)
import CBOR.Decode (decode, decodeSequence)
import CBOR.JSON (toJSON, fromJSON)

cborTests :: TestTree
cborTests = testGroup "CBOR"
  [ rfc8949AppendixA
  , rfc8949ConformanceVectors
  , propertyRoundtrips
  , edgeCases
  , jsonTests
  , deterministicTests
  , sequenceTests
  ]

-- | RFC 8949 Appendix A test vectors: exact byte sequences.
rfc8949AppendixA :: TestTree
rfc8949AppendixA = testGroup "RFC 8949 Appendix A test vectors"
  [ testGroup "Unsigned integers"
      [ testCase "0 = [0x00]" $
          encode (C.UInt 0) @?= BS.pack [0x00]
      , testCase "1 = [0x01]" $
          encode (C.UInt 1) @?= BS.pack [0x01]
      , testCase "10 = [0x0a]" $
          encode (C.UInt 10) @?= BS.pack [0x0a]
      , testCase "23 = [0x17]" $
          encode (C.UInt 23) @?= BS.pack [0x17]
      , testCase "24 = [0x18, 0x18]" $
          encode (C.UInt 24) @?= BS.pack [0x18, 0x18]
      , testCase "25 = [0x18, 0x19]" $
          encode (C.UInt 25) @?= BS.pack [0x18, 0x19]
      , testCase "100 = [0x18, 0x64]" $
          encode (C.UInt 100) @?= BS.pack [0x18, 0x64]
      , testCase "1000 = [0x19, 0x03, 0xe8]" $
          encode (C.UInt 1000) @?= BS.pack [0x19, 0x03, 0xe8]
      , testCase "1000000 = [0x1a, 0x00, 0x0f, 0x42, 0x40]" $
          encode (C.UInt 1000000) @?= BS.pack [0x1a, 0x00, 0x0f, 0x42, 0x40]
      , testCase "1000000000000 = [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]" $
          encode (C.UInt 1000000000000) @?= BS.pack [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]
      ]

  , testGroup "Negative integers"
      [ testCase "-1 = [0x20]" $
          encode (C.NInt 0) @?= BS.pack [0x20]
      , testCase "-10 = [0x29]" $
          encode (C.NInt 9) @?= BS.pack [0x29]
      , testCase "-100 = [0x38, 0x63]" $
          encode (C.NInt 99) @?= BS.pack [0x38, 0x63]
      , testCase "-1000 = [0x39, 0x03, 0xe7]" $
          encode (C.NInt 999) @?= BS.pack [0x39, 0x03, 0xe7]
      ]

  , testGroup "Simple values / booleans"
      [ testCase "false = [0xf4]" $
          encode (C.Bool False) @?= BS.pack [0xf4]
      , testCase "true = [0xf5]" $
          encode (C.Bool True) @?= BS.pack [0xf5]
      , testCase "null = [0xf6]" $
          encode C.Null @?= BS.pack [0xf6]
      , testCase "undefined = [0xf7]" $
          encode C.Undefined @?= BS.pack [0xf7]
      ]

  , testGroup "Text strings"
      [ testCase "\"\" = [0x60]" $
          encode (C.TextString "") @?= BS.pack [0x60]
      , testCase "\"a\" = [0x61, 0x61]" $
          encode (C.TextString "a") @?= BS.pack [0x61, 0x61]
      , testCase "\"IETF\" = [0x64, 0x49, 0x45, 0x54, 0x46]" $
          encode (C.TextString "IETF") @?= BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]
      , testCase "\"\\\"\\\\\" = [0x62, 0x22, 0x5c]" $
          encode (C.TextString "\"\\") @?= BS.pack [0x62, 0x22, 0x5c]
      , testCase "\"\\u00fc\" = [0x62, 0xc3, 0xbc]" $
          encode (C.TextString "\252") @?= BS.pack [0x62, 0xc3, 0xbc]
      ]

  , testGroup "Byte strings"
      [ testCase "h'' = [0x40]" $
          encode (C.ByteString BS.empty) @?= BS.pack [0x40]
      , testCase "h'01020304' = [0x44, 0x01, 0x02, 0x03, 0x04]" $
          encode (C.ByteString (BS.pack [1,2,3,4])) @?= BS.pack [0x44, 0x01, 0x02, 0x03, 0x04]
      ]

  , testGroup "Arrays"
      [ testCase "[] = [0x80]" $
          encode (C.Array V.empty) @?= BS.pack [0x80]
      , testCase "[1, 2, 3] = [0x83, 0x01, 0x02, 0x03]" $
          encode (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
            @?= BS.pack [0x83, 0x01, 0x02, 0x03]
      , testCase "[1, [2, 3], [4, 5]]" $
          encode (C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
            @?= BS.pack [0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05]
      , testCase "25 element array" $
          encode (C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1..25 :: Int]]))
            @?= BS.pack ([0x98, 25] ++ [1..23] ++ [0x18, 24, 0x18, 25])
      ]

  , testGroup "Maps"
      [ testCase "{} = [0xa0]" $
          encode (C.Map V.empty) @?= BS.pack [0xa0]
      , testCase "{1: 2, 3: 4}" $
          encode (C.Map (V.fromList [(C.UInt 1, C.UInt 2), (C.UInt 3, C.UInt 4)]))
            @?= BS.pack [0xa2, 0x01, 0x02, 0x03, 0x04]
      ]

  , testGroup "Tags"
      [ testCase "tag 1 with integer" $
          encode (C.Tag 1 (C.UInt 1363896240))
            @?= BS.pack [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0]
      ]

  , testGroup "Decode test vectors"
      [ testCase "decode 0" $
          decode (BS.pack [0x00]) @?= Right (C.UInt 0)
      , testCase "decode 1" $
          decode (BS.pack [0x01]) @?= Right (C.UInt 1)
      , testCase "decode 23" $
          decode (BS.pack [0x17]) @?= Right (C.UInt 23)
      , testCase "decode 24" $
          decode (BS.pack [0x18, 0x18]) @?= Right (C.UInt 24)
      , testCase "decode 100" $
          decode (BS.pack [0x18, 0x64]) @?= Right (C.UInt 100)
      , testCase "decode 1000" $
          decode (BS.pack [0x19, 0x03, 0xe8]) @?= Right (C.UInt 1000)
      , testCase "decode 1000000" $
          decode (BS.pack [0x1a, 0x00, 0x0f, 0x42, 0x40]) @?= Right (C.UInt 1000000)
      , testCase "decode -1" $
          decode (BS.pack [0x20]) @?= Right (C.NInt 0)
      , testCase "decode -100" $
          decode (BS.pack [0x38, 0x63]) @?= Right (C.NInt 99)
      , testCase "decode false" $
          decode (BS.pack [0xf4]) @?= Right (C.Bool False)
      , testCase "decode true" $
          decode (BS.pack [0xf5]) @?= Right (C.Bool True)
      , testCase "decode null" $
          decode (BS.pack [0xf6]) @?= Right C.Null
      , testCase "decode \"\"" $
          decode (BS.pack [0x60]) @?= Right (C.TextString "")
      , testCase "decode \"a\"" $
          decode (BS.pack [0x61, 0x61]) @?= Right (C.TextString "a")
      , testCase "decode \"IETF\"" $
          decode (BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]) @?= Right (C.TextString "IETF")
      , testCase "decode []" $
          decode (BS.pack [0x80]) @?= Right (C.Array V.empty)
      , testCase "decode [1,2,3]" $
          decode (BS.pack [0x83, 0x01, 0x02, 0x03])
            @?= Right (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
      , testCase "decode {}" $
          decode (BS.pack [0xa0]) @?= Right (C.Map V.empty)
      , testCase "decode {1:2, 3:4}" $
          decode (BS.pack [0xa2, 0x01, 0x02, 0x03, 0x04])
            @?= Right (C.Map (V.fromList [(C.UInt 1, C.UInt 2), (C.UInt 3, C.UInt 4)]))
      ]

  , testGroup "Decode indefinite-length"
      [ testCase "indefinite array [_ 1, 2]" $
          decode (BS.pack [0x9f, 0x01, 0x02, 0xff])
            @?= Right (C.Array (V.fromList [C.UInt 1, C.UInt 2]))
      , testCase "indefinite map {_ 1:2}" $
          decode (BS.pack [0xbf, 0x01, 0x02, 0xff])
            @?= Right (C.Map (V.fromList [(C.UInt 1, C.UInt 2)]))
      ]

  , testGroup "Float encoding/decoding"
      [ testCase "float32 100000.0" $ do
          let val = C.Float32 100000.0
              bs  = encode val
          BS.index bs 0 @?= 0xfa
          decode bs @?= Right val
      , testCase "float64 1.1" $ do
          let val = C.Float64 1.1
              bs  = encode val
          BS.index bs 0 @?= 0xfb
          decode bs @?= Right val
      ]
  ]

-- | RFC 8949 Appendix A conformance vectors (embedded).
-- Each entry: (description, hex bytes, expected decoded Value, roundtrip?)
-- We test decode of hex bytes matches expected value, and for roundtrip entries,
-- re-encoding produces the original bytes.
rfc8949ConformanceVectors :: TestTree
rfc8949ConformanceVectors = testGroup "RFC 8949 Appendix A conformance vectors"
  [ testGroup "Decode conformance" $ map mkDecodeTest decodeVectors
  , testGroup "Roundtrip conformance" $ map mkRoundtripTest roundtripVectors
  , testGroup "Decode-only (indefinite-length)" $ map mkDecodeTest indefiniteVectors
  ]
  where
    mkDecodeTest (name, hexBytes, expected) =
      testCase name $ decode (BS.pack hexBytes) @?= Right expected

    mkRoundtripTest (name, hexBytes, expected) =
      testCase name $ do
        let bs = BS.pack hexBytes
        decode bs @?= Right expected
        encode expected @?= bs

    -- Vectors where roundtrip==true and we have a decoded value
    roundtripVectors :: [(String, [Word8], C.Value)]
    roundtripVectors =
      -- Unsigned integers
      [ ("uint 0",         [0x00], C.UInt 0)
      , ("uint 1",         [0x01], C.UInt 1)
      , ("uint 10",        [0x0a], C.UInt 10)
      , ("uint 23",        [0x17], C.UInt 23)
      , ("uint 24",        [0x18, 0x18], C.UInt 24)
      , ("uint 25",        [0x18, 0x19], C.UInt 25)
      , ("uint 100",       [0x18, 0x64], C.UInt 100)
      , ("uint 1000",      [0x19, 0x03, 0xe8], C.UInt 1000)
      , ("uint 1000000",   [0x1a, 0x00, 0x0f, 0x42, 0x40], C.UInt 1000000)
      , ("uint 1000000000000",
          [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00],
          C.UInt 1000000000000)
      , ("uint max64",
          [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
          C.UInt 18446744073709551615)
      -- Negative integers
      , ("nint -1",        [0x20], C.NInt 0)
      , ("nint -10",       [0x29], C.NInt 9)
      , ("nint -100",      [0x38, 0x63], C.NInt 99)
      , ("nint -1000",     [0x39, 0x03, 0xe7], C.NInt 999)
      -- Booleans and special
      , ("false",          [0xf4], C.Bool False)
      , ("true",           [0xf5], C.Bool True)
      , ("null",           [0xf6], C.Null)
      , ("undefined",      [0xf7], C.Undefined)
      -- Simple values
      , ("simple(16)",     [0xf0], C.Simple 16)
      , ("simple(24)",     [0xf8, 0x18], C.Simple 24)
      , ("simple(255)",    [0xf8, 0xff], C.Simple 255)
      -- Byte strings
      , ("bytes empty",    [0x40], C.ByteString BS.empty)
      , ("bytes 01020304", [0x44, 0x01, 0x02, 0x03, 0x04],
          C.ByteString (BS.pack [0x01, 0x02, 0x03, 0x04]))
      -- Text strings
      , ("text empty",     [0x60], C.TextString "")
      , ("text \"a\"",     [0x61, 0x61], C.TextString "a")
      , ("text \"IETF\"",  [0x64, 0x49, 0x45, 0x54, 0x46], C.TextString "IETF")
      , ("text \"\\\"\\\\\"",
          [0x62, 0x22, 0x5c], C.TextString "\"\\")
      , ("text \"\\u00fc\"",
          [0x62, 0xc3, 0xbc], C.TextString "\252")
      , ("text \"\\u6c34\"",
          [0x63, 0xe6, 0xb0, 0xb4], C.TextString "\27700")
      , ("text \"\\ud800\\udd51\" (U+10151)",
          [0x64, 0xf0, 0x90, 0x85, 0x91], C.TextString "\x10151")
      -- Arrays
      , ("array empty",    [0x80], C.Array V.empty)
      , ("array [1,2,3]",  [0x83, 0x01, 0x02, 0x03],
          C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
      , ("array [1,[2,3],[4,5]]",
          [0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05],
          C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
      , ("array 25 elements",
          [0x98, 0x19] ++ [0x01..0x17] ++ [0x18, 0x18, 0x18, 0x19],
          C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1..25 :: Int]]))
      -- Maps
      , ("map empty",      [0xa0], C.Map V.empty)
      , ("map {\"a\":1, \"b\":[2,3]}",
          [0xa2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03],
          C.Map (V.fromList
            [ (C.TextString "a", C.UInt 1)
            , (C.TextString "b", C.Array (V.fromList [C.UInt 2, C.UInt 3]))
            ]))
      , ("map {\"a\":\"A\",\"b\":\"B\",\"c\":\"C\",\"d\":\"D\",\"e\":\"E\"}",
          [0xa5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61, 0x42,
           0x61, 0x63, 0x61, 0x43, 0x61, 0x64, 0x61, 0x44, 0x61, 0x65, 0x61, 0x45],
          C.Map (V.fromList
            [ (C.TextString "a", C.TextString "A")
            , (C.TextString "b", C.TextString "B")
            , (C.TextString "c", C.TextString "C")
            , (C.TextString "d", C.TextString "D")
            , (C.TextString "e", C.TextString "E")
            ]))
      -- Tags
      , ("tag 0 date string",
          [0xc0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2d, 0x30, 0x33, 0x2d, 0x32,
           0x31, 0x54, 0x32, 0x30, 0x3a, 0x30, 0x34, 0x3a, 0x30, 0x30, 0x5a],
          C.Tag 0 (C.TextString "2013-03-21T20:04:00Z"))
      , ("tag 1 epoch",
          [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0],
          C.Tag 1 (C.UInt 1363896240))
      , ("tag 23 h'01020304'",
          [0xd7, 0x44, 0x01, 0x02, 0x03, 0x04],
          C.Tag 23 (C.ByteString (BS.pack [0x01, 0x02, 0x03, 0x04])))
      , ("tag 24 h'6449455446'",
          [0xd8, 0x18, 0x45, 0x64, 0x49, 0x45, 0x54, 0x46],
          C.Tag 24 (C.ByteString (BS.pack [0x64, 0x49, 0x45, 0x54, 0x46])))
      , ("tag 32 URI",
          [0xd8, 0x20, 0x76, 0x68, 0x74, 0x74, 0x70, 0x3a, 0x2f, 0x2f,
           0x77, 0x77, 0x77, 0x2e, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c,
           0x65, 0x2e, 0x63, 0x6f, 0x6d],
          C.Tag 32 (C.TextString "http://www.example.com"))
      -- Mixed array+map
      , ("[\"a\", {\"b\": \"c\"}]",
          [0x82, 0x61, 0x61, 0xa1, 0x61, 0x62, 0x61, 0x63],
          C.Array (V.fromList
            [ C.TextString "a"
            , C.Map (V.fromList [(C.TextString "b", C.TextString "c")])
            ]))
      ]

    -- Decode-only vectors: we verify decoding works, but re-encoding may differ
    decodeVectors :: [(String, [Word8], C.Value)]
    decodeVectors =
      -- Float16 decode tests (half-precision are decoded to Float16)
      [ ("half 0.0",   [0xf9, 0x00, 0x00], C.Float16 0.0)
      , ("half -0.0",  [0xf9, 0x80, 0x00], C.Float16 (-0.0))
      , ("half 1.0",   [0xf9, 0x3c, 0x00], C.Float16 1.0)
      , ("half 1.5",   [0xf9, 0x3e, 0x00], C.Float16 1.5)
      , ("half 65504", [0xf9, 0x7b, 0xff], C.Float16 65504.0)
      , ("half -4.0",  [0xf9, 0xc4, 0x00], C.Float16 (-4.0))
      -- Float32 decode tests
      , ("float32 100000.0",
          [0xfa, 0x47, 0xc3, 0x50, 0x00], C.Float32 100000.0)
      -- Float64 decode tests
      , ("float64 1.1",
          [0xfb, 0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a],
          C.Float64 1.1)
      , ("float64 -4.1",
          [0xfb, 0xc0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66],
          C.Float64 (-4.1))
      , ("float64 1e300",
          [0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c],
          C.Float64 1.0e300)
      -- Bignum tagged (>64-bit) — just verify we can decode the tag
      , ("bignum 2^64",
          [0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
          C.Tag 2 (C.ByteString (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])))
      , ("neg bignum -2^64-1",
          [0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
          C.Tag 3 (C.ByteString (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])))
      -- neg max Word64
      , ("nint -18446744073709551616",
          [0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
          C.NInt 18446744073709551615)
      -- tag 1 with float
      , ("tag 1 epoch float",
          [0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00],
          C.Tag 1 (C.Float64 1363896240.5))
      ]

    -- Indefinite-length test vectors
    indefiniteVectors :: [(String, [Word8], C.Value)]
    indefiniteVectors =
      [ ("indef array []",
          [0x9f, 0xff],
          C.Array V.empty)
      , ("indef array [1,[2,3],[4,5]]",
          [0x9f, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff, 0xff],
          C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
      , ("indef [1,[2,3],[4,5]] v2",
          [0x9f, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05, 0xff],
          C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
      , ("mixed def/indef [1,[2,3],indef[4,5]]",
          [0x83, 0x01, 0x82, 0x02, 0x03, 0x9f, 0x04, 0x05, 0xff],
          C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
      , ("mixed [1,indef[2,3],[4,5]]",
          [0x83, 0x01, 0x9f, 0x02, 0x03, 0xff, 0x82, 0x04, 0x05],
          C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
      , ("indef 25 elements",
          [0x9f] ++ [0x01..0x17] ++ [0x18, 0x18, 0x18, 0x19, 0xff],
          C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1..25 :: Int]]))
      , ("indef map {\"a\":1,\"b\":[2,3]}",
          [0xbf, 0x61, 0x61, 0x01, 0x61, 0x62, 0x9f, 0x02, 0x03, 0xff, 0xff],
          C.Map (V.fromList
            [ (C.TextString "a", C.UInt 1)
            , (C.TextString "b", C.Array (V.fromList [C.UInt 2, C.UInt 3]))
            ]))
      , ("[\"a\", indef{\"b\":\"c\"}]",
          [0x82, 0x61, 0x61, 0xbf, 0x61, 0x62, 0x61, 0x63, 0xff],
          C.Array (V.fromList
            [ C.TextString "a"
            , C.Map (V.fromList [(C.TextString "b", C.TextString "c")])
            ]))
      , ("indef {\"Fun\":true,\"Amt\":-2}",
          [0xbf, 0x63, 0x46, 0x75, 0x6e, 0xf5, 0x63, 0x41, 0x6d, 0x74, 0x21, 0xff],
          C.Map (V.fromList
            [ (C.TextString "Fun", C.Bool True)
            , (C.TextString "Amt", C.NInt 1)
            ]))
      , ("indef text \"streaming\"",
          [0x7f, 0x65, 0x73, 0x74, 0x72, 0x65, 0x61, 0x64, 0x6d, 0x69, 0x6e, 0x67, 0xff],
          C.TextString "streaming")
      ]

-- | Property-based roundtrip tests.
propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "UInt roundtrip" $ property $ do
      n <- forAll $ Gen.word64 Range.linearBounded
      let val = C.UInt n
      decode (encode val) === Right val

  , testProperty "NInt roundtrip" $ property $ do
      n <- forAll $ Gen.word64 Range.linearBounded
      let val = C.NInt n
      decode (encode val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = C.Bool b
      decode (encode val) === Right val

  , testProperty "Null roundtrip" $ property $ do
      decode (encode C.Null) === Right C.Null

  , testProperty "ByteString roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 512)
      let val = C.ByteString bs
      decode (encode val) === Right val

  , testProperty "TextString roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 256) Gen.unicode
      let val = C.TextString t
      decode (encode val) === Right val

  , testProperty "Float32 roundtrip" $ property $ do
      f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
      let val = C.Float32 f
      decode (encode val) === Right val

  , testProperty "Float64 roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
      let val = C.Float64 d
      decode (encode val) === Right val

  , testProperty "Array of UInts roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 50)
                       (Gen.word64 (Range.linear 0 0xffffffff))
      let val = C.Array (V.fromList (map C.UInt ns))
      decode (encode val) === Right val

  , testProperty "Map roundtrip" $ property $ do
      entries <- forAll $ Gen.list (Range.linear 0 30) $ do
        k <- Gen.text (Range.linear 1 32) Gen.alphaNum
        v <- Gen.word64 (Range.linear 0 0xffff)
        pure (C.TextString k, C.UInt v)
      let val = C.Map (V.fromList entries)
      decode (encode val) === Right val

  , testProperty "Tag roundtrip" $ property $ do
      tagNum <- forAll $ Gen.word64 (Range.linear 0 0xffff)
      n <- forAll $ Gen.word64 (Range.linear 0 0xffffffff)
      let val = C.Tag tagNum (C.UInt n)
      decode (encode val) === Right val

  , testProperty "Nested arrays roundtrip" $ property $ do
      inner <- forAll $ Gen.list (Range.linear 0 10)
                          (Gen.word64 (Range.linear 0 255))
      let val = C.Array (V.fromList
                  [ C.UInt 1
                  , C.Array (V.fromList (map C.UInt inner))
                  , C.TextString "nested"
                  ])
      decode (encode val) === Right val
  ]

-- | Edge cases.
edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "large integer (max Word64)" $ do
      let val = C.UInt maxBound
      decode (encode val) @?= Right val

  , testCase "large negative integer (max Word64)" $ do
      let val = C.NInt maxBound
      decode (encode val) @?= Right val

  , testCase "deeply nested" $ do
      let nest 0 = C.UInt 42
          nest n = C.Array (V.singleton (nest (n - 1)))
          val = nest (20 :: Int)
      decode (encode val) @?= Right val

  , testCase "tagged tagged value" $ do
      let val = C.Tag 0 (C.Tag 1 (C.TextString "epoch"))
      decode (encode val) @?= Right val

  , testCase "map with mixed key types" $ do
      let val = C.Map (V.fromList
                  [ (C.UInt 1, C.TextString "one")
                  , (C.TextString "two", C.UInt 2)
                  , (C.Bool True, C.Null)
                  ])
      decode (encode val) @?= Right val

  , testCase "decoder rejects array length larger than remaining bytes" $ do
      -- 0x9b is array(8-byte length); 0xFF...FF declares ~2^64 elements.
      -- Allocating that mutable vector blows up; decoder must reject.
      let bs = BS.pack [ 0x9B
                       , 0xFF, 0xFF, 0xFF, 0xFF
                       , 0xFF, 0xFF, 0xFF, 0xFF
                       ]
      case decode bs of
        Left _  -> pure ()
        Right v -> assertFailure $
          "expected rejection of impossible array, got: " ++ show v

  , testCase "decoder rejects map length larger than remaining bytes" $ do
      let bs = BS.pack [ 0xBB
                       , 0xFF, 0xFF, 0xFF, 0xFF
                       , 0xFF, 0xFF, 0xFF, 0xFF
                       ]
      case decode bs of
        Left _  -> pure ()
        Right v -> assertFailure $
          "expected rejection of impossible map, got: " ++ show v

  , testCase "empty byte string" $ do
      let val = C.ByteString BS.empty
      decode (encode val) @?= Right val

  , testCase "empty text string" $ do
      let val = C.TextString ""
      decode (encode val) @?= Right val

  , testCase "simple value 16" $ do
      let val = C.Simple 16
      decode (encode val) @?= Right val

  , testCase "simple value 255" $ do
      let val = C.Simple 255
      decode (encode val) @?= Right val

  , testCase "decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"
  ]

-- | JSON conversion tests.
jsonTests :: TestTree
jsonTests = testGroup "JSON conversion"
  [ testCase "UInt to JSON" $
      toJSON (C.UInt 42) @?= Aeson.Number 42

  , testCase "NInt to JSON" $
      toJSON (C.NInt 0) @?= Aeson.Number (-1)

  , testCase "Bool to JSON" $ do
      toJSON (C.Bool True) @?= Aeson.Bool True
      toJSON (C.Bool False) @?= Aeson.Bool False

  , testCase "Null to JSON" $
      toJSON C.Null @?= Aeson.Null

  , testCase "TextString to JSON" $
      toJSON (C.TextString "hello") @?= Aeson.String "hello"

  , testCase "Array to JSON" $
      toJSON (C.Array (V.fromList [C.UInt 1, C.UInt 2]))
        @?= Aeson.Array (V.fromList [Aeson.Number 1, Aeson.Number 2])

  , testCase "Tag to JSON" $ do
      let json = toJSON (C.Tag 1 (C.UInt 42))
      case json of
        Aeson.Object _ -> pure ()
        _ -> assertFailure "expected JSON object for tag"

  , testCase "fromJSON null" $
      fromJSON Aeson.Null @?= C.Null

  , testCase "fromJSON bool" $
      fromJSON (Aeson.Bool True) @?= C.Bool True

  , testCase "fromJSON string" $
      fromJSON (Aeson.String "hi") @?= C.TextString "hi"

  , testCase "fromJSON positive int" $
      fromJSON (Aeson.Number 42) @?= C.UInt 42

  , testCase "fromJSON negative int" $
      fromJSON (Aeson.Number (-1)) @?= C.NInt 0

  , testCase "fromJSON array" $
      fromJSON (Aeson.Array (V.fromList [Aeson.Number 1]))
        @?= C.Array (V.fromList [C.UInt 1])
  ]

deterministicTests :: TestTree
deterministicTests = testGroup "Deterministic encoding"
  [ testCase "RFC 8949 §4.2.1 bytewise lex ordering" $ do
      -- UInt 1 encodes to [0x01]; "a" -> [0x61, 0x61]; "bb" -> [0x62, 0x62, 0x62].
      -- Bytewise lex sort puts 0x01 first, then 0x61.., then 0x62..
      -- which happens to coincide with length-first here.
      let val = C.Map (V.fromList
            [ (C.TextString "bb", C.UInt 2)
            , (C.UInt 1, C.UInt 1)
            , (C.TextString "a", C.UInt 3)
            ])
          bs = encodeDeterministic val
      case decode bs of
        Right (C.Map kvs) -> do
          let keys = V.toList (V.map fst kvs)
          keys @?= [C.UInt 1, C.TextString "a", C.TextString "bb"]
        other -> assertFailure $ "expected Map, got: " ++ show other

  , testCase "RFC 8949 §4.2.1 short string sorts before longer with smaller first byte" $ do
      -- Lex: "ab" (0x62 0x61 0x62) vs "b" (0x61 0x62) -> compare 0x61 vs 0x62
      -- so text "b" (header 0x61, payload 0x62) sorts before text "ab"
      -- (header 0x62, payload 0x61 0x62) bytewise.
      let val = C.Map (V.fromList
            [ (C.TextString "ab", C.UInt 1)
            , (C.TextString "b", C.UInt 2)
            ])
          bs = encodeDeterministic val
      case decode bs of
        Right (C.Map kvs) ->
          V.toList (V.map fst kvs) @?= [C.TextString "b", C.TextString "ab"]
        other -> assertFailure $ "expected Map, got: " ++ show other

  , testCase "RFC 8949 vs RFC 7049 length-first divergence" $ do
      -- Constructed so the two sort orders disagree:
      --   UInt 100        -> [0x18, 0x64]   (2 bytes, first byte 0x18)
      --   TextString ""   -> [0x60]         (1 byte, first byte 0x60)
      -- Bytewise lex (RFC 8949): 0x18 < 0x60 -> UInt 100 first.
      -- Length-first (RFC 7049): 1 byte < 2 bytes -> TextString "" first.
      let val = C.Map (V.fromList
            [ (C.TextString "", C.UInt 1)
            , (C.UInt 100, C.UInt 2)
            ])
      case decode (encodeDeterministic val) of
        Right (C.Map kvs) ->
          V.toList (V.map fst kvs) @?= [C.UInt 100, C.TextString ""]
        other -> assertFailure $ "8949: expected Map, got: " ++ show other
      case decode (encodeDeterministicLengthFirst val) of
        Right (C.Map kvs) ->
          V.toList (V.map fst kvs) @?= [C.TextString "", C.UInt 100]
        other -> assertFailure $ "7049: expected Map, got: " ++ show other

  , testCase "deterministic roundtrip" $ do
      let val = C.Map (V.fromList
            [ (C.TextString "z", C.UInt 1)
            , (C.TextString "a", C.UInt 2)
            ])
          bs = encodeDeterministic val
      case decode bs of
        Right decoded -> do
          case decoded of
            C.Map kvs -> V.map fst kvs @?= V.fromList [C.TextString "a", C.TextString "z"]
            _ -> assertFailure "expected map"
        Left err -> assertFailure err

  , testCase "deterministic preserves non-map values" $ do
      let val = C.Array (V.fromList [C.UInt 3, C.UInt 1, C.UInt 2])
      decode (encodeDeterministic val) @?= Right val

  , testCase "deterministic encoding is valid CBOR" $ do
      let val = C.Map (V.fromList
            [ (C.TextString "key", C.UInt 42)
            , (C.UInt 1, C.TextString "val")
            ])
      case decode (encodeDeterministic val) of
        Right _ -> pure ()
        Left err -> assertFailure err
  ]

sequenceTests :: TestTree
sequenceTests = testGroup "CBOR Sequences (RFC 8742)"
  [ testCase "encode/decode empty sequence" $ do
      let vals = V.empty
          bs = encodeSequence vals
      BS.length bs @?= 0
      decodeSequence bs @?= Right V.empty

  , testCase "encode/decode single item sequence" $ do
      let vals = V.singleton (C.UInt 42)
          bs = encodeSequence vals
      decodeSequence bs @?= Right vals

  , testCase "encode/decode multi-item sequence" $ do
      let vals = V.fromList [C.UInt 1, C.TextString "hello", C.Bool True]
          bs = encodeSequence vals
      decodeSequence bs @?= Right vals

  , testCase "sequence is concatenation of individual encodings" $ do
      let v1 = C.UInt 1
          v2 = C.TextString "hi"
          seq' = encodeSequence (V.fromList [v1, v2])
          cat  = encode v1 <> encode v2
      seq' @?= cat

  , testProperty "sequence roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 20) (Gen.word64 (Range.linear 0 0xffff))
      let vals = V.fromList (map C.UInt ns)
      decodeSequence (encodeSequence vals) === Right vals
  ]
