{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit (exitFailure)

import ASN1.Decode (decode)
import ASN1.Value

main :: IO ()
main = do
  -- Definite-length SEQUENCE round-trip baseline.
  -- SEQUENCE { INTEGER 1, INTEGER 2 } definite form:
  --   30 06   = SEQUENCE, length 6
  --     02 01 01  = INTEGER 1
  --     02 01 02  = INTEGER 2
  expectValue
    "definite SEQUENCE"
    (BS.pack [0x30, 0x06, 0x02, 0x01, 0x01, 0x02, 0x01, 0x02])
    (Sequence (V.fromList [Integer 1, Integer 2]))

  -- Indefinite-length SEQUENCE — the same payload using BER's 0x80
  -- length and a 0x00 0x00 end-of-contents marker:
  --   30 80   = SEQUENCE, indefinite length
  --     02 01 01  = INTEGER 1
  --     02 01 02  = INTEGER 2
  --   00 00   = end of contents
  expectValue
    "indefinite SEQUENCE"
    (BS.pack [0x30, 0x80, 0x02, 0x01, 0x01, 0x02, 0x01, 0x02, 0x00, 0x00])
    (Sequence (V.fromList [Integer 1, Integer 2]))

  -- Indefinite-length SET with a UTF-8 string and a NULL.
  --   31 80
  --     0C 03 'a' 'b' 'c'
  --     05 00
  --   00 00
  expectValue
    "indefinite SET"
    (BS.pack
      [ 0x31, 0x80
      , 0x0C, 0x03, 0x61, 0x62, 0x63
      , 0x05, 0x00
      , 0x00, 0x00
      ])
    (Set (V.fromList [UTF8String (T.pack "abc"), Null]))

  -- Nested indefinite SEQUENCE inside an indefinite SEQUENCE.
  expectValue
    "nested indefinite SEQUENCE"
    (BS.pack
      [ 0x30, 0x80
      , 0x30, 0x80
      , 0x02, 0x01, 0x07
      , 0x00, 0x00
      , 0x02, 0x01, 0x08
      , 0x00, 0x00
      ])
    (Sequence (V.fromList
      [ Sequence (V.singleton (Integer 7))
      , Integer 8
      ]))

  -- Indefinite length on a primitive (BOOLEAN) must be rejected.
  case decode (BS.pack [0x01, 0x80, 0x00, 0x00]) of
    Right v -> failTest $
      "decoder accepted indefinite length on primitive: " ++ show v
    Left _ -> putStrLn "OK: indefinite length on primitive rejected"

  -- 0xFF reserved length octet must be rejected.
  case decode (BS.pack [0x30, 0xFF, 0x01, 0x02, 0x03]) of
    Right v -> failTest $
      "decoder accepted reserved length 0xFF: " ++ show v
    Left _ -> putStrLn "OK: reserved length 0xFF rejected"

  putStrLn "All ASN.1 BER indefinite-length tests passed."

expectValue :: String -> BS.ByteString -> Value -> IO ()
expectValue label bs expected = case decode bs of
  Left e -> failTest (label ++ ": " ++ e)
  Right got
    | got == expected -> putStrLn ("OK: " ++ label)
    | otherwise -> failTest $
        label ++ " mismatch: expected " ++ show expected
          ++ ", got " ++ show got

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
