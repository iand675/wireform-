{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import System.Exit (exitFailure)

import ORC.Read
  ( decodeStringColumn
  , decodeStringDictColumn
  , decodeStringDictColumnSized
  )
import ORC.Stripe
  ( ColumnEncoding (..)
  , ColumnEncodingKind (..)
  , Stream (..)
  , StripeFooter (..)
  , decodeStripeFooter
  , encodeStripeFooter
  )
import ORC.Write (encodeRLEv2Direct, encodeStringDirectColumn)

main :: IO ()
main = do
  -- StripeFooter round-trip including column encodings.
  let sf = StripeFooter
        { sfStreams = V.fromList
            [ Stream { stKind = 0, stColumn = 0, stLength = 5 }
            , Stream { stKind = 1, stColumn = 0, stLength = 7 }
            , Stream { stKind = 2, stColumn = 0, stLength = 3 }
            ]
        , sfColumnEncodings = V.fromList
            [ ColumnEncoding CekDirectV2 0
            , ColumnEncoding CekDictionaryV2 16
            ]
        }
  case decodeStripeFooter (encodeStripeFooter sf) of
    Left e -> failTest ("decodeStripeFooter: " ++ e)
    Right sf'
      | sf' /= sf -> failTest ("StripeFooter mismatch:\n  expected " ++ show sf
                                 ++ "\n  got      " ++ show sf')
      | otherwise -> putStrLn "OK: StripeFooter round-trip with ColumnEncoding"

  -- DICTIONARY_V2 string column via decodeStringColumn dispatch.
  let dictTexts = V.fromList [T.pack "alpha", T.pack "beta", T.pack "gamma"]
      (dictData, lengthBs) = encodeStringDirectColumn dictTexts
      -- Three indices that reference the dictionary in non-sorted order.
      indexBs = encodeRLEv2Direct (VP.fromList [2, 0, 1, 0]) False
  case decodeStringColumn 4 indexBs lengthBs dictData Nothing of
    Left e -> failTest ("DICTIONARY_V2 decodeStringColumn: " ++ e)
    Right got
      | got == V.fromList (fmap (Just . T.pack) ["gamma", "alpha", "beta", "alpha"]) ->
          putStrLn "OK: decodeStringColumn dispatches to DICTIONARY_V2"
      | otherwise ->
          failTest $ "DICTIONARY_V2 mismatch: " ++ show got

  -- decodeStringDictColumnSized rejects truncated LENGTH stream.
  case decodeStringDictColumnSized 0 4 dictData lengthBs indexBs Nothing of
    Right _ -> failTest "decodeStringDictColumnSized accepted wrong dictionarySize"
    Left _ -> putStrLn "OK: decodeStringDictColumnSized rejects mismatched dictionarySize"

  -- DIRECT path still works (empty dictionary stream).
  let directTexts = V.fromList [T.pack "x", T.pack "yy", T.pack "zzz"]
      (directData, directLen) = encodeStringDirectColumn directTexts
  case decodeStringColumn 3 directData directLen BS.empty Nothing of
    Left e -> failTest ("DIRECT decodeStringColumn: " ++ e)
    Right got
      | got == V.fromList (fmap (Just . T.pack) ["x", "yy", "zzz"]) ->
          putStrLn "OK: decodeStringColumn DIRECT path"
      | otherwise -> failTest $ "DIRECT mismatch: " ++ show got

  -- decodeStringDictColumn (legacy, no explicit size) still works.
  case decodeStringDictColumn 4 dictData lengthBs indexBs Nothing of
    Left e -> failTest ("legacy decodeStringDictColumn: " ++ e)
    Right got
      | got == V.fromList (fmap (Just . T.pack) ["gamma", "alpha", "beta", "alpha"]) ->
          putStrLn "OK: legacy decodeStringDictColumn"
      | otherwise -> failTest $ "legacy mismatch: " ++ show got

  putStrLn "All ORC stripe-footer / DICTIONARY_V2 tests passed."

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
