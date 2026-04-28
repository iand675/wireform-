{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32)
import GHC.Float (castFloatToWord32)
import System.Exit (exitFailure)

import qualified CBOR.Decode as D
import qualified CBOR.Stream as S
import qualified CBOR.Value as V

main :: IO ()
main = do
  -- Half-float decoding (RFC 8949 \xC2\xA73.4 + Appendix C reference).
  -- Inputs are 16-bit IEEE binary16 patterns; both the eager and the
  -- streaming decoders must produce the same Float.

  expectHalf "+0.0"     0x0000  0.0
  expectHalf "-0.0"     0x8000  (-0.0)
  -- 1.0: sign=0, exp=15 (bias 15), mant=0
  expectHalf "+1.0"     0x3c00  1.0
  expectHalf "-2.0"     0xc000  (-2.0)
  expectHalf "+Inf"     0x7c00  (1/0)
  expectHalf "-Inf"     0xfc00  (negate (1/0))

  -- NaN payload preservation (the bug just fixed). A half-float with
  -- non-zero mantissa must propagate that mantissa into the
  -- single-precision result via a 13-bit left shift, not collapse to
  -- canonical quiet-NaN 0x7fc00000.
  let halfNaN = 0x7e01 :: Word16  -- exp=0x1f, mant=0x201
      hD = decodeOne     halfNaN
      hS = decodeOneStream halfNaN
      isNaN' f = f /= f
      -- The 10-bit mantissa shifted into bits 13..22 of binary32:
      expectedSingleBits = 0x7f800000 .|. (0x201 `shiftL` 13) :: Word32
  expect "Decode half-NaN with payload preserves mantissa"
    (isNaN' hD && castFloatToWord32 hD == expectedSingleBits)
  expect "Stream half-NaN with payload preserves mantissa"
    (isNaN' hS && castFloatToWord32 hS == expectedSingleBits)
  expect "Decode and Stream agree on half-NaN bits"
    (castFloatToWord32 hD == castFloatToWord32 hS)

  -- A NaN with a different payload should produce a different bit pattern.
  let halfNaN2 = 0x7eaa :: Word16
      hD2 = decodeOne halfNaN2
  expect "Different half-NaN payloads produce different bit patterns"
    (castFloatToWord32 hD /= castFloatToWord32 hD2)

  putStrLn "All CBOR half-float tests passed."

-- | Encode a half-float CBOR item byte sequence (major 7, info 25,
-- followed by the 2 big-endian bytes of the half).
halfBytes :: Word16 -> BS.ByteString
halfBytes h = BS.pack
  [ 0xf9                                                   -- major 7, info 25
  , fromIntegral ((h `shiftR` 8) .&. 0xff) :: Word8
  , fromIntegral (h .&. 0xff)              :: Word8
  ]

decodeOne :: Word16 -> Float
decodeOne h = case D.decode (halfBytes h) of
  Right (V.Float16 f) -> f
  other               -> errorx "CBOR.Decode" other

decodeOneStream :: Word16 -> Float
decodeOneStream h = case S.streamDecode (halfBytes h) of
  S.Done (V.Float16 f) _leftover -> f
  S.Done other _ -> errorx "CBOR.Stream" other
  S.Partial _    -> errorx "CBOR.Stream" ("Partial" :: String)
  S.Fail e       -> errorx "CBOR.Stream" e

errorx :: Show a => String -> a -> b
errorx tag x = error (tag ++ ": unexpected " ++ show x)

expectHalf :: String -> Word16 -> Float -> IO ()
expectHalf label h expected = do
  let f = decodeOne h
      g = decodeOneStream h
      okF = if isNaN expected
              then isNaN f
              else f == expected
      okG = castFloatToWord32 f == castFloatToWord32 g
  if okF && okG
    then putStrLn ("OK: half " ++ label)
    else failTest $
      label ++ ": expected " ++ show expected
        ++ ", Decode -> " ++ show f
        ++ ", Stream -> " ++ show g

expect :: String -> Bool -> IO ()
expect what ok =
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
