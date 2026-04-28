{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (exitFailure)

import Bond.Decode (decode)
import Bond.Encode (encode)
import Bond.Value (BondType (..), Value (..))

main :: IO ()
main = do
  -- BMP code points: each char encodes to a single 16-bit LE code unit.
  -- Bond compact-binary spec: count = 4 (UTF-16 code units, varint),
  -- followed by 8 bytes of UTF-16-LE data.
  let bmp = WString "héllo"  -- 5 BMP code points
  expectRoundTrip "BMP wstring" bmp
  let encBmp = encode bmp
  expectByte "BMP wstring count varint" encBmp 0 5
  -- Each code unit is 2 bytes; 'h' = 0x68, 0x00, 'é' = 0xE9 0x00, ...
  expectByte "BMP wstring first low byte"  encBmp 1 0x68
  expectByte "BMP wstring first high byte" encBmp 2 0x00

  -- Supplementary-plane code point (U+1F600 GRINNING FACE) encodes
  -- to a high+low surrogate pair (4 bytes total, count = 2).
  let emoji = WString "\x1F600"
  expectRoundTrip "supplementary-plane wstring" emoji
  let encEmoji = encode emoji
  expectByte "emoji count varint = 2 code units" encEmoji 0 2
  -- Surrogate pair for U+1F600:
  --   n = 0x1F600 - 0x10000 = 0xF600
  --   hi = 0xD800 + (0xF600 >> 10) = 0xD800 + 0x3D = 0xD83D
  --   lo = 0xDC00 + (0xF600 & 0x3FF) = 0xDC00 + 0x200 = 0xDE00
  expectByte "emoji hi-surrogate low byte"  encEmoji 1 0x3D
  expectByte "emoji hi-surrogate high byte" encEmoji 2 0xD8
  expectByte "emoji lo-surrogate low byte"  encEmoji 3 0x00
  expectByte "emoji lo-surrogate high byte" encEmoji 4 0xDE

  -- Empty wstring: count = 0, no data bytes.
  let emptyW = WString ""
  expectRoundTrip "empty wstring" emptyW
  let encEmpty = encode emptyW
  expect "empty wstring is exactly 1 byte" (BS.length encEmpty == 1)
  expectByte "empty wstring count = 0" encEmpty 0 0

  -- A regular UTF-8 string is unaffected (encoded as before).
  let s = String "héllo"
  expectRoundTrip "regular string" s

  -- A wstring decoder must reject an orphan low surrogate.
  let badBytes = BS.pack
        [ 0x01      -- count = 1 (varint)
        , 0x00, 0xDC  -- low-surrogate code unit (0xDC00) on its own
        ]
  case decode BT_WSTRING badBytes of
    Right v -> failTest $
      "should have rejected orphan low surrogate, got " ++ show v
    Left _ -> putStrLn "OK: orphan low surrogate rejected"

  putStrLn "All Bond compact-binary wstring tests passed."

expectRoundTrip :: String -> Value -> IO ()
expectRoundTrip label val = do
  let typeOf v = case v of
        WString _ -> BT_WSTRING
        String _  -> BT_STRING
        _ -> error "unsupported"
  case decode (typeOf val) (encode val) of
    Left e  -> failTest (label ++ ": decode failed: " ++ e)
    Right v
      | v == val -> putStrLn ("OK: " ++ label ++ " round-trip")
      | otherwise -> failTest $
          label ++ " mismatch: got " ++ show v ++ ", expected " ++ show val

expect :: String -> Bool -> IO ()
expect what ok =
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

expectByte :: String -> BS.ByteString -> Int -> Int -> IO ()
expectByte label bs i expected
  | i < BS.length bs && fromIntegral (BS.index bs i) == expected =
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
