{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import System.Exit (exitFailure)

import Avro.Container
  ( compressBlock
  , decompressBlock
  , readContainer
  , writeContainerWith
  )
import Avro.Decode (decodeAvroResolved)
import Avro.Encode (encodeAvro)
import Avro.Fingerprint (avroFingerprint, parsingCanonicalForm)
import Avro.IDL (parseAvroIDL)
import Avro.IDLConvert (idlToType)
import qualified Avro.IDL as IDL
import Avro.Schema (LogicalType (..))
import qualified Avro.Value as AV
import Avro.Schema
  ( AvroField (..)
  , AvroSchema (..)
  , AvroType (..)
  )

main :: IO ()
main = do
  -- CRC-64-AVRO fingerprint, verified against the Java reference
  -- implementation in the Avro 1.11 spec ('SchemaNormalization.fingerprint64'
  -- with @"null"@ → @"null"@ PCF).
  expectFingerprint (AvroPrimitive AvroNull) "\"null\"" "63dd24e7cc258f8a"

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

  -- Schema resolution: string <-> bytes promotion (Avro 1.11 spec).
  let strSchema   = AvroPrimitive AvroString
      bytesSchema = AvroPrimitive AvroBytes
      hello       = AV.String "hello"
      helloBytes  = AV.Bytes (BSC.pack "hello")
      strEncoded  = encodeAvro strSchema hello
  case decodeAvroResolved strSchema bytesSchema strEncoded of
    Right v
      | v == helloBytes ->
          putStrLn "OK: schema resolution string -> bytes"
      | otherwise -> failTest $ "string -> bytes value mismatch: " ++ show v
    Left e -> failTest ("string -> bytes resolution: " ++ e)
  let bytesEncoded = encodeAvro bytesSchema helloBytes
  case decodeAvroResolved bytesSchema strSchema bytesEncoded of
    Right v
      | v == hello ->
          putStrLn "OK: schema resolution bytes -> string"
      | otherwise -> failTest $ "bytes -> string value mismatch: " ++ show v
    Left e -> failTest ("bytes -> string resolution: " ++ e)
  -- bytes -> string promotion must reject invalid UTF-8.
  let badBytes = AV.Bytes (BS.pack [0xff, 0xfe, 0xfd])
      badEnc = encodeAvro bytesSchema badBytes
  case decodeAvroResolved bytesSchema strSchema badEnc of
    Right _ -> failTest "bytes -> string accepted invalid UTF-8"
    Left _  -> putStrLn "OK: bytes -> string rejects invalid UTF-8"

  -- Avro IDL: logical-type keywords parse and convert to the right
  -- AvroLogical wrappers.
  let idlSrc = T.pack $ unlines
        [ "protocol P {"
        , "  record R {"
        , "    date d;"
        , "    time_ms t;"
        , "    timestamp_ms ts;"
        , "    uuid u;"
        , "  }"
        , "}"
        ]
  case parseAvroIDL idlSrc of
    Left e -> failTest ("Avro IDL parse: " ++ show e)
    Right idl ->
      case V.toList (IDL.aidlDeclarations idl) of
        [decl] -> do
          let ty = idlToType decl
          case ty of
            AvroRecord{avroRecordFields = fs} -> do
              let logicals = mapMaybe (asLogical . avroFieldType) (V.toList fs)
              if logicals ==
                   [ DateLogical
                   , TimeMillisLogical
                   , TimestampMillisLogical
                   , UuidLogical
                   ]
                then putStrLn "OK: Avro IDL date/time_ms/timestamp_ms/uuid"
                else failTest $ "IDL logicals mismatch: " ++ show logicals
            _ -> failTest $ "expected record, got " ++ show ty
        ds -> failTest $ "expected exactly one decl, got " ++ show (length ds)

  putStrLn "All Avro container codec tests passed."

asLogical :: AvroType -> Maybe LogicalType
asLogical AvroLogical{avroLogicalType = lt} = Just lt
asLogical _ = Nothing

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

expectFingerprint :: AvroType -> BS.ByteString -> String -> IO ()
expectFingerprint sch expectedPcf expectedHex = do
  let pcf = parsingCanonicalForm sch
  if pcf /= expectedPcf
    then failTest $
      "PCF mismatch: expected " ++ show expectedPcf
        ++ ", got " ++ show pcf
    else do
      let fpBytes = avroFingerprint sch
          -- Reference values are the little-endian view of the 64-bit fingerprint.
          asLE = sum [ fromIntegral (BS.index fpBytes i) * (256 :: Integer) ^ i
                     | i <- [0 .. 7] ]
          actualHex = pad16 (showHex' asLE)
      if actualHex == expectedHex
        then putStrLn ("OK: fingerprint " ++ show expectedPcf ++ " = " ++ actualHex)
        else failTest $
          "fingerprint " ++ show expectedPcf
            ++ " expected " ++ expectedHex
            ++ " got " ++ actualHex
  where
    showHex' :: Integer -> String
    showHex' n =
      let go 0 acc = acc
          go x acc =
            let (q, r) = x `divMod` 16
                d = if r < 10 then toEnum (fromEnum '0' + fromIntegral r)
                              else toEnum (fromEnum 'a' + fromIntegral (r - 10))
            in go q (d : acc)
      in if n == 0 then "0" else go n ""
    pad16 s = replicate (16 - length s) '0' ++ s

expect :: String -> Bool -> IO ()
expect what ok =
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
