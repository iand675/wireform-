{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit (exitFailure)

import ASN1.Decode (decode)
import ASN1.Encode (encode)
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

  -- DER §11.5/§11.6: SET and SET-OF children must be emitted in
  -- ascending byte order of their full DER encodings.
  let unsorted = Set $ V.fromList [Integer 3, Integer 1, Integer 2]
      enc = encode unsorted
  -- After sort: 02 01 01 < 02 01 02 < 02 01 03.
  -- Total inner length = 9, so TLV is: 31 09 02 01 01 02 01 02 02 01 03.
  expectBytes "SET sorts children by DER bytes"
    enc (BS.pack [0x31, 0x09
                 , 0x02, 0x01, 0x01
                 , 0x02, 0x01, 0x02
                 , 0x02, 0x01, 0x03
                 ])

  -- A SET round-trip preserves the (sorted) order.
  case decode enc of
    Left e -> failTest ("SET round-trip: " ++ e)
    Right (Set vs)
      | V.toList vs == [Integer 1, Integer 2, Integer 3] ->
          putStrLn "OK: SET round-trip preserves canonical order"
      | otherwise -> failTest $
          "SET round-trip mismatch: " ++ show vs
    Right v -> failTest $
      "SET decoded as non-Set: " ++ show v

  -- BIT STRING with non-zero unused on empty data must normalise to
  -- unused = 0 per DER §8.6.2.3.
  let badEmptyBs = BitString 5 BS.empty
      encEmpty = encode badEmptyBs
  expectBytes "Empty BIT STRING normalises unused to 0"
    encEmpty (BS.pack [0x03, 0x01, 0x00])

  -- OID with first sub-identifier > 127: per X.690 §8.19, the
  -- combined first sub-identifier (40*X1 + X2) is base-128 encoded,
  -- so OID 2.999 has combined = 1079 = 0x437 = bytes 0x88 0x37.
  -- Wireform's encoder previously truncated this to a single byte.
  let oidBigSecond = OID (V.fromList [2, 999])
      encOidBig = encode oidBigSecond
  expectBytes "OID 2.999 base-128 first sub-identifier"
    encOidBig (BS.pack [0x06, 0x02, 0x88, 0x37])
  case decode encOidBig of
    Left e -> failTest ("OID 2.999 round-trip: " ++ e)
    Right (OID v)
      | V.toList v == [2, 999] ->
          putStrLn "OK: OID 2.999 round-trip"
      | otherwise -> failTest $ "OID 2.999 mismatch: " ++ show v
    Right v -> failTest $ "OID decoded as non-OID: " ++ show v

  -- A multi-component OID with the joint-iso-itu-t arc:
  -- 2.5.4.3 (commonName).
  let oidCN = OID (V.fromList [2, 5, 4, 3])
      encCN = encode oidCN
  -- 40*2 + 5 = 85 < 128 so single byte 0x55, then 0x04, 0x03.
  expectBytes "OID 2.5.4.3 single-byte first sub-identifier"
    encCN (BS.pack [0x06, 0x03, 0x55, 0x04, 0x03])

  -- X.690 §8.19.2 forbids non-minimal base-128 sub-identifiers — i.e.
  -- a leading @0x80@ octet that represents a useless seven-bit zero
  -- prefix. The decoder must reject the ill-formed encoding rather
  -- than silently parse it.
  let nonMinimalOID = BS.pack
        [ 0x06        -- OID tag
        , 0x03        -- length 3
        , 0x80, 0x80, 0x05  -- sub-identifier: 0x80 leading 0x80 -> non-minimal
        ]
  case decode nonMinimalOID of
    Left _  -> putStrLn "OK: non-minimal base-128 OID rejected"
    Right v -> failTest $
      "expected rejection of non-minimal base-128 OID, got " ++ show v

  putStrLn "All ASN.1 BER indefinite-length tests passed."

expectBytes :: String -> BS.ByteString -> BS.ByteString -> IO ()
expectBytes label got expected
  | got == expected = putStrLn ("OK: " ++ label)
  | otherwise = failTest $
      label ++ ": got " ++ show (BS.unpack got)
        ++ ", expected " ++ show (BS.unpack expected)

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
