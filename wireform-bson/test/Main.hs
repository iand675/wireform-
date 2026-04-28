{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import System.Exit (exitFailure)

import BSON.Decode (decode)
import BSON.Encode (encode)
import qualified BSON.Value as B

main :: IO ()
main = do
  -- Round-trip the deprecated DBPointer (BSON tag 0x0C) which the
  -- previous decoder rejected outright.
  let oid = BS.pack [0x55, 0x16, 0x9A, 0x1B, 0x55, 0x16, 0x9A, 0x1B, 0x55, 0x16, 0x9A, 0x1B]
      doc = B.Document (V.singleton ("ref", B.DBPointer "users" oid))
      bs = encode doc
  case decode bs of
    Left e -> failTest ("DBPointer round-trip: " ++ e)
    Right got
      | got == doc -> putStrLn "OK: DBPointer round-trip"
      | otherwise  -> failTest $ "DBPointer mismatch: " ++ show got

  -- Spot-check: legacy fixtures with all primitive tags still round-trip.
  let everything = B.Document (V.fromList
        [ ("d",   B.Double 3.14)
        , ("s",   B.String "hi")
        , ("i32", B.Int32 42)
        , ("i64", B.Int64 1234567890123)
        , ("o",   B.ObjectId oid)
        , ("u",   B.Undefined)
        , ("min", B.MinKey)
        , ("max", B.MaxKey)
        , ("dbp", B.DBPointer "ns.coll" oid)
        ])
      bs2 = encode everything
  case decode bs2 of
    Left e -> failTest ("primitives round-trip: " ++ e)
    Right got
      | got == everything -> putStrLn "OK: primitives + DBPointer round-trip"
      | otherwise -> failTest $ "primitives mismatch: " ++ show got

  putStrLn "All BSON tests passed."

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
