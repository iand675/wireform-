{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Word (Word32)
import System.Exit (exitFailure)

import MsgPack.Decode (decode)
import MsgPack.Encode (encode)
import qualified MsgPack.Value as MV

main :: IO ()
main = do
  -- Round-trip every documented variant of the MessagePack timestamp
  -- ext type (-1) so the writer + decoder pair stays in sync.

  -- Timestamp 32: ns == 0, seconds in [0, 2^32). Wire shape:
  --   d6 ff <BE32 secs>  (6 bytes total)
  let ts32 = MV.Timestamp 1234567890 0
      enc32 = encode ts32
  expectLen "ts32 length" enc32 6
  expectByte "ts32 fixext4 marker"   enc32 0 0xd6
  expectByte "ts32 ext type=-1"      enc32 1 0xff
  expectRoundTrip "ts32" ts32

  -- Timestamp 64: ns < 2^30, seconds in [0, 2^34). Wire shape:
  --   d7 ff <BE64 packed>  (10 bytes total)
  let ts64 = MV.Timestamp 5_000_000_000 999_999_999
      enc64 = encode ts64
  expectLen "ts64 length" enc64 10
  expectByte "ts64 fixext8 marker"   enc64 0 0xd7
  expectByte "ts64 ext type=-1"      enc64 1 0xff
  expectRoundTrip "ts64" ts64

  -- Spec compliance: nanos > 999_999_999 must NOT be silently
  -- truncated by the 30-bit nanos field. A bad value (here
  -- 1_500_000_000) must fall through to timestamp 96 so the writer
  -- preserves the full Word32 input.
  let nsBad   = 1_500_000_000 :: Word32
      tsBad   = MV.Timestamp 5_000_000_000 nsBad
      encBad  = encode tsBad
  expectLen "oversize-nanos -> ts96 length" encBad 15
  expectByte "oversize-nanos uses ext8 (0xc7)" encBad 0 0xc7
  expectByte "oversize-nanos length=12"        encBad 1 12
  expectByte "oversize-nanos type=-1"          encBad 2 0xff
  expectRoundTrip "oversize-nanos preserves payload" tsBad

  -- Timestamp 96: negative seconds (pre-1970).
  let tsNeg = MV.Timestamp (-1234567 :: Int64) 0
      encNeg = encode tsNeg
  expectLen "ts96 negative length" encNeg 15
  expectByte "ts96 negative ext8"  encNeg 0 0xc7
  expectByte "ts96 negative len=12" encNeg 1 12
  expectByte "ts96 negative type=-1" encNeg 2 0xff
  expectRoundTrip "ts96 negative seconds" tsNeg

  -- Timestamp 64 boundary: max representable seconds.
  let tsMax64 = MV.Timestamp 0x3FFFFFFFF 999_999_999
  expectRoundTrip "ts64 boundary (max secs/ns)" tsMax64

  -- Just over the timestamp 64 seconds boundary -> timestamp 96.
  let tsOverflow = MV.Timestamp 0x400000000 0
      encOver = encode tsOverflow
  expectLen "ts96 over 34-bit boundary length" encOver 15
  expectRoundTrip "ts96 over 34-bit boundary" tsOverflow

  -- Decoder DoS protection: declared array/map/string lengths must
  -- not exceed the bytes left in the input. Without this guard a
  -- 4-byte msg ("\xdd\xff\xff\xff\xff" — array32 with 4 GB count)
  -- would happily allocate a 4-billion-element MV.
  let arr32Huge = BS.pack [0xdd, 0xFF, 0xFF, 0xFF, 0xFF]
  case decode arr32Huge of
    Left _  -> putStrLn "OK: array32 with absurd count rejected"
    Right v -> failTest $
      "array32 huge-count must be rejected, got: " ++ show v

  let map32Huge = BS.pack [0xdf, 0xFF, 0xFF, 0xFF, 0xFF]
  case decode map32Huge of
    Left _  -> putStrLn "OK: map32 with absurd count rejected"
    Right v -> failTest $
      "map32 huge-count must be rejected, got: " ++ show v

  let str32Huge = BS.pack [0xdb, 0xFF, 0xFF, 0xFF, 0xFF]
  case decode str32Huge of
    Left _  -> putStrLn "OK: str32 with absurd length rejected"
    Right v -> failTest $
      "str32 huge-length must be rejected, got: " ++ show v

  putStrLn "All MsgPack timestamp encoding tests passed."

expectRoundTrip :: String -> MV.Value -> IO ()
expectRoundTrip label val = case decode (encode val) of
  Left e -> failTest (label ++ ": decode failed: " ++ e)
  Right v
    | v == val -> putStrLn ("OK: " ++ label ++ " round-trip")
    | otherwise -> failTest $
        label ++ " mismatch: got " ++ show v ++ ", expected " ++ show val

expectLen :: String -> BS.ByteString -> Int -> IO ()
expectLen label bs n
  | BS.length bs == n = putStrLn ("OK: " ++ label ++ " = " ++ show n)
  | otherwise = failTest $
      label ++ ": expected " ++ show n
        ++ " bytes, got " ++ show (BS.length bs)

expectByte :: String -> BS.ByteString -> Int -> Int -> IO ()
expectByte label bs i expected
  | i < BS.length bs && BS.index bs i == fromIntegral expected =
      putStrLn ("OK: " ++ label)
  | i < BS.length bs = failTest $
      label ++ ": byte[" ++ show i ++ "] = "
        ++ show (BS.index bs i) ++ ", expected " ++ show expected
  | otherwise = failTest $
      label ++ ": byte[" ++ show i ++ "] out of range"

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
