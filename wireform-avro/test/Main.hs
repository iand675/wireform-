{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit (exitFailure)

import Avro.Container
  ( compressBlock
  , decompressBlock
  , readContainer
  , writeContainerWith
  )
import qualified Avro.Value as AV
import Avro.Schema
  ( AvroField (..)
  , AvroSchema (..)
  , AvroType (..)
  )

main :: IO ()
main = do
  codecRoundTrip "null"
  codecRoundTrip "deflate"
#ifdef HAVE_SNAPPY
  codecRoundTrip "snappy"
  let !raw = BS.replicate 4096 0x33
      !comp = compressBlock "snappy" raw
  expect "snappy CRC32 trailer present" (BS.length comp >= 4)
  case decompressBlock "snappy" (BS.take (BS.length comp - 1) comp) of
    Right out
      | out == raw -> failTest "snappy decoder accepted truncated CRC32 trailer"
      | otherwise  -> failTest "snappy decoder accepted truncated frame"
    Left _ -> putStrLn "OK: snappy rejects truncated CRC32 trailer"
  let lastIdx = BS.length comp - 1
      !badTrailer = BS.take lastIdx comp <> BS.singleton 0xFF
  case decompressBlock "snappy" badTrailer of
    Right out
      | out == raw -> failTest "snappy decoder accepted bad CRC32"
      | otherwise  -> failTest "snappy decoder produced wrong bytes"
    Left _ -> putStrLn "OK: snappy rejects bad CRC32 trailer"
#endif
#ifdef HAVE_ZSTD
  codecRoundTrip "zstandard"
#endif
#ifdef HAVE_BZIP2
  codecRoundTrip "bzip2"
#endif

  let schema = AvroRecord
        { avroRecordName       = "Person"
        , avroRecordNamespace  = Nothing
        , avroRecordDoc        = Nothing
        , avroRecordAliases    = V.empty
        , avroRecordFields     = V.fromList
            [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
            , AvroField "age"  (AvroPrimitive AvroLong)   Nothing Nothing V.empty Nothing Map.empty
            ]
        , avroRecordProps      = Map.empty
        }
      vals = V.fromList
        [ AV.Record (V.fromList [AV.String "Ada Lovelace", AV.Long 36])
        , AV.Record (V.fromList [AV.String "Grace Hopper", AV.Long 85])
        , AV.Record (V.fromList [AV.String "Edsger Dijkstra", AV.Long 72])
        ]
  containerRoundTrip schema vals "null"
  containerRoundTrip schema vals "deflate"
#ifdef HAVE_SNAPPY
  containerRoundTrip schema vals "snappy"
#endif
#ifdef HAVE_ZSTD
  containerRoundTrip schema vals "zstandard"
#endif
#ifdef HAVE_BZIP2
  containerRoundTrip schema vals "bzip2"
#endif

  putStrLn "All Avro container codec tests passed."

codecRoundTrip :: String -> IO ()
codecRoundTrip codec = do
  let raw = BS.pack [fromIntegral (i `mod` 256) | i <- [(0 :: Int) .. 1023]]
      comp = compressBlock (T.pack codec) raw
  case decompressBlock (T.pack codec) comp of
    Left e -> failTest (codec ++ " decompress: " ++ e)
    Right out
      | out /= raw ->
          failTest (codec ++ " round-trip mismatch")
      | otherwise ->
          putStrLn ("OK: codec " ++ codec ++ " round-trip")

containerRoundTrip
  :: AvroType
  -> V.Vector AV.Value
  -> String
  -> IO ()
containerRoundTrip schema vals codec = do
  let bs = writeContainerWith (T.pack codec) schema vals
  case readContainer bs of
    Left e -> failTest ("OCF " ++ codec ++ " readContainer: " ++ e)
    Right (_writerSchema, decodedVals)
      | decodedVals /= vals ->
          failTest ("OCF " ++ codec ++ " values mismatch")
      | otherwise ->
          putStrLn ("OK: container " ++ codec ++ " round-trip")

expect :: String -> Bool -> IO ()
expect what ok =
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
