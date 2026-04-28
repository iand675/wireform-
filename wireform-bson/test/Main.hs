{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import System.Exit (exitFailure)

import BSON.Decode (decode)
import BSON.Encode (encode, encodeChecked)
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

  -- BSON cstring keys cannot contain a NUL byte; the spec defines them
  -- as NUL-terminated UTF-8 byte sequences, so a key with embedded NUL
  -- silently truncates the document on parse. encodeChecked rejects it.
  let badKey = B.Document (V.singleton ("a\0b", B.Int32 1))
  case encodeChecked badKey of
    Left _  -> putStrLn "OK: encodeChecked rejects NUL in key"
    Right _ -> failTest "expected encodeChecked to reject NUL key"

  let badNested = B.Document (V.singleton ("x",
                    B.Document (V.singleton ("y\0", B.Int32 2))))
  case encodeChecked badNested of
    Left _  -> putStrLn "OK: encodeChecked rejects NUL in nested key"
    Right _ -> failTest "expected encodeChecked to reject NUL nested key"

  case encodeChecked everything of
    Right _ -> putStrLn "OK: encodeChecked accepts NUL-free keys"
    Left e  -> failTest ("encodeChecked false positive: " ++ e)

  -- BSON document size MUST be at least 5 (4-byte length + NUL
  -- terminator). A declared size below that is malformed and must
  -- be rejected.
  let tinyDoc = BS.pack [0x04, 0x00, 0x00, 0x00]
  case decode tinyDoc of
    Left _  -> putStrLn "OK: document size < 5 rejected"
    Right v -> failTest $
      "expected rejection of tiny document, got " ++ show v

  -- BSON string MUST end with a NUL terminator. A four-byte length
  -- with a non-NUL final byte is malformed.
  let badStr = BS.pack
        [ 0x0E, 0x00, 0x00, 0x00     -- doc size
        , 0x02                        -- type=String
        , 0x73, 0x00                  -- key 's' + NUL
        , 0x03, 0x00, 0x00, 0x00     -- string length = 3
        , 0x68, 0x69, 0x21            -- 'h','i','!' (no NUL)
        , 0x00                        -- doc terminator
        ]
  case decode badStr of
    Left _  -> putStrLn "OK: string missing NUL terminator rejected"
    Right v -> failTest $
      "expected rejection of NUL-less string, got " ++ show v

  putStrLn "All BSON tests passed."

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
