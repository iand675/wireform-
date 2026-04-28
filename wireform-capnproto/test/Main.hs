{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (exitFailure)

import CapnProto.Packed (pack, unpack)

main :: IO ()
main = do
  -- The Cap'n Proto packed-encoding spec example: a 16-word message
  -- with mostly zero data should round-trip exactly.
  let zeros n = BS.replicate (n * 8) 0
  expectRound "all-zero one word" (zeros 1)
  expectRound "all-zero ten words" (zeros 10)
  expectRound "all-zero 256 words" (zeros 256)
  expectRound "single non-zero byte mid-word"
    (BS.pack
      [ 0x00, 0x00, 0x00, 0x42, 0x00, 0x00, 0x00, 0x00 ])
  expectRound "every byte non-zero"
    (BS.pack [0x01 .. 0x08])
  expectRound "all 0xff"
    (BS.replicate 16 0xff)
  expectRound "mixed words"
    (BS.concat
      [ BS.replicate 8 0
      , BS.pack [0xde, 0xad, 0xbe, 0xef, 0, 0, 0, 0]
      , BS.replicate 16 0
      , BS.pack [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
      ])

  -- Hand-built packed input matching the spec example.
  -- Source: https://capnproto.org/encoding.html#packing
  -- Input word: 08 00 00 00 03 00 02 00
  --   non-zero byte positions: 0, 4, 6  (LSB = byte 0)
  --   tag bits:  0b0101 0001 = 0x51
  --   non-zero values written in order: 08 03 02
  let inputWord = BS.pack [0x08, 0, 0, 0, 0x03, 0, 0x02, 0]
      expectedPacked = BS.pack [0x51, 0x08, 0x03, 0x02]
  let packed = pack inputWord
  if packed == expectedPacked
    then putStrLn "OK: spec example pack matches expected"
    else failTest $
      "spec example pack mismatch: got " ++ show (BS.unpack packed)
        ++ ", expected " ++ show (BS.unpack expectedPacked)
  case unpack packed of
    Right back | back == inputWord ->
      putStrLn "OK: spec example unpack returns original word"
    Right back -> failTest $
      "spec example unpack mismatch: got " ++ show (BS.unpack back)
    Left e -> failTest ("spec example unpack: " ++ e)

  -- Pack of non-aligned input is right-padded to a word; unpack
  -- yields the padded form (8 bytes), not the original 5.
  let unaligned = BS.pack [0x01, 0x02, 0x03, 0x04, 0x05]
      packedUnal = pack unaligned
      expectedUnal = BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0, 0, 0]
  case unpack packedUnal of
    Right back | back == expectedUnal ->
      putStrLn "OK: unaligned input zero-padded to a word"
    Right back -> failTest $
      "unaligned padding mismatch: " ++ show (BS.unpack back)
    Left e -> failTest ("unaligned unpack: " ++ e)

  putStrLn "All Cap'n Proto packed-encoding tests passed."

expectRound :: String -> BS.ByteString -> IO ()
expectRound label bs = do
  let packed = pack bs
  case unpack packed of
    Right back
      | back == bs -> putStrLn ("OK: round-trip " ++ label)
      | otherwise  -> failTest $
          label ++ ": round-trip mismatch (got "
            ++ show (BS.length back) ++ " bytes, want "
            ++ show (BS.length bs) ++ ")"
    Left e -> failTest (label ++ ": unpack failed: " ++ e)

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
