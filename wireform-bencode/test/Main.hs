{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (exitFailure)

import Bencode.Decode (decode)
import Bencode.Encode (encode)
import qualified Bencode.Value as B
import qualified Data.Vector as V

main :: IO ()
main = do
  -- Sanity: definite valid integers round-trip.
  expectOk    "i0e"        (B.BInteger 0)
  expectOk    "i42e"       (B.BInteger 42)
  expectOk    "i-42e"      (B.BInteger (-42))
  expectOk    "i123456789e" (B.BInteger 123456789)

  -- BEP-3 forbids leading zeros.
  expectFail  "i03e"       "leading zero"
  expectFail  "i007e"      "leading zeros"
  -- BEP-3 forbids negative zero.
  expectFail  "i-0e"       "negative zero"
  -- Negative leading zeros also forbidden.
  expectFail  "i-03e"      "leading zero after sign"
  -- Empty integer literal.
  expectFail  "ie"         "empty integer literal"

  -- Dict-key canonicalisation per BEP-3:
  --   Keys must appear in sorted order, compared as raw byte strings.
  --   Duplicate keys are ill-formed; defensive writers should drop
  --   later occurrences.
  let unsortedDict = B.BDict $ V.fromList
        [ ("z", B.BInteger 1)
        , ("a", B.BInteger 2)
        , ("m", B.BInteger 3)
        ]
      enc = encode unsortedDict
  -- Canonical wire form: d 1:a i2e 1:m i3e 1:z i1e e
  expectBytes "unsorted dict gets sorted on encode"
    enc "d1:ai2e1:mi3e1:zi1ee"

  -- Duplicate keys: keep the first, drop the rest.
  let dupDict = B.BDict $ V.fromList
        [ ("a", B.BInteger 1)
        , ("a", B.BInteger 99)
        ]
  expectBytes "duplicate keys collapse, first wins"
    (encode dupDict) "d1:ai1ee"

  -- Decode emits sorted entries already, so an encode -> decode -> encode
  -- cycle is byte-identical (canonical).
  case decode enc of
    Left e -> failTest ("decode roundtrip: " ++ e)
    Right v
      | encode v == enc ->
          putStrLn "OK: encode/decode/encode is byte-canonical"
      | otherwise ->
          failTest "encode/decode/encode lost canonical form"

  -- Byte-order key compare (not Unicode-codepoint): keys "B" (0x42)
  -- and "a" (0x61) sort with "B" first in the BEP-3 byte order.
  let asciiCase = B.BDict $ V.fromList
        [ ("a", B.BInteger 1)
        , ("B", B.BInteger 2)
        ]
  expectBytes "byte-order compare puts 'B' (0x42) before 'a' (0x61)"
    (encode asciiCase) "d1:Bi2e1:ai1ee"

  putStrLn "All Bencode integer-literal tests passed."

expectOk :: BS.ByteString -> B.Value -> IO ()
expectOk bs expected = case decode bs of
  Right v
    | v == expected -> putStrLn ("OK: " ++ show bs ++ " -> " ++ show v)
    | otherwise -> failTest $
        show bs ++ " mismatch: got " ++ show v ++ ", expected " ++ show expected
  Left e -> failTest (show bs ++ ": " ++ e)

expectBytes :: String -> BS.ByteString -> BS.ByteString -> IO ()
expectBytes label got expected
  | got == expected = putStrLn ("OK: " ++ label)
  | otherwise = failTest $
      label ++ ": expected " ++ show expected ++ ", got " ++ show got

expectFail :: BS.ByteString -> String -> IO ()
expectFail bs label = case decode bs of
  Left _  -> putStrLn ("OK: rejected " ++ show bs ++ " (" ++ label ++ ")")
  Right v -> failTest $
    "should have rejected " ++ show bs ++ " but got " ++ show v

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
