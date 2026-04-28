{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (exitFailure)

import Bencode.Decode (decode)
import qualified Bencode.Value as B

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

  putStrLn "All Bencode integer-literal tests passed."

expectOk :: BS.ByteString -> B.Value -> IO ()
expectOk bs expected = case decode bs of
  Right v
    | v == expected -> putStrLn ("OK: " ++ show bs ++ " -> " ++ show v)
    | otherwise -> failTest $
        show bs ++ " mismatch: got " ++ show v ++ ", expected " ++ show expected
  Left e -> failTest (show bs ++ ": " ++ e)

expectFail :: BS.ByteString -> String -> IO ()
expectFail bs label = case decode bs of
  Left _  -> putStrLn ("OK: rejected " ++ show bs ++ " (" ++ label ++ ")")
  Right v -> failTest $
    "should have rejected " ++ show bs ++ " but got " ++ show v

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
