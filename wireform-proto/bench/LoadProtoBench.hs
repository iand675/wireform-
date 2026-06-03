{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-missing-signatures
                 -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

-- | Micro-benchmarks for the @loadProto@-generated codec hot
-- paths: wire encode \/ decode and JSON encode \/ decode for a
-- representative singular-scalar message, a repeated-scalar
-- message, a oneof message, and a message holding a known
-- enum field (exercises the open-enum representation and its
-- catch-all wrapper). The bench is hand-rolled (no criterion
-- dep) so it ships without dragging extra deps into
-- wireform-proto.
module Main where

import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import System.CPUTime (getCPUTime)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import Data.Reflection (Given (..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)
import Proto.TH (loadProto)
import qualified Proto.Decode as PD
import qualified Proto.Encode as PE
import qualified Proto.Extension as Ext
import Proto.Google.Protobuf.Timestamp (Timestamp(..), defaultTimestamp)

-- TH-generated JSON instances carry a 'Given ExtensionRegistry' constraint
-- for proto2 extensions; this bench schema has none, so satisfy it with
-- the empty registry (same orphan pattern as the test-conformance and
-- derive-test fixtures).
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry

-- A small inline schema covering the common shapes.
$(loadProto "bench/Bench.proto")

------------------------------------------------------------------------
-- Test inputs
------------------------------------------------------------------------

samplePerson :: Person
samplePerson = defaultPerson
  { personName  = T.pack "John Doe"
  , personAge   = 30
  , personEmail = T.pack "john@example.com"
  , personScore = 95.5
  , personActive = True
  }

sampleNumbers :: Numbers
sampleNumbers = defaultNumbers
  { numbersInts   = V.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  , numbersDoubles = V.fromList [1.1, 2.2, 3.3, 4.4, 5.5]
  }

sampleChoice :: Choice
sampleChoice = defaultChoice
  { choiceChoice = Just (Choice'Choice'StringValue (T.pack "hello"))
  }

sampleStatus :: Person
sampleStatus = samplePerson
  { personStatus = Status'StatusActive }

------------------------------------------------------------------------
-- Timing
------------------------------------------------------------------------

iters :: Int
iters = 200000

-- | Run @action@ @n@ times, accumulating an Int summary so the
-- compiler can't dead-code-eliminate the work. The summary is
-- 'evaluate'd to WHNF every iteration, defeating laziness.
timeNF :: NFData b => Int -> (Int -> b) -> (b -> Int) -> IO Integer
timeNF n make summarize = do
  -- Warmup
  _ <- evaluate (force (make n))
  start <- getCPUTime
  let loop !k !acc
        | k <= 0    = pure acc
        | otherwise = do
            !y <- evaluate (make k)
            let !a = acc + summarize y
            loop (k - 1) a
  !final <- loop n 0
  evaluate final
  end <- getCPUTime
  -- Print the summary once so it's not optimised away.
  printf "    [acc=%d] " final
  pure ((end - start) `div` fromIntegral n)

nsPerIter :: Integer -> Integer
nsPerIter = id

bench :: NFData b => String -> (Int -> b) -> (b -> Int) -> IO ()
bench label make summarize = do
  ns <- timeNF iters make summarize
  printf "%-36s %7d ns/iter\n" label ns
  hFlush stdout

------------------------------------------------------------------------
-- Bench drivers
------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "loadProto micro-benchmarks"
  putStrLn (replicate 60 '=')
  putStrLn ""

  let !p      = samplePerson
      !pBytes = PE.encodeMessage p
      !pJson  = Aeson.encode p
  printf "Person (4 scalars + 1 bool, encoded size = %d bytes):\n" (BS.length pBytes)
  bench "wire encode" (\_ -> PE.encodeMessage p) BS.length
  bench "wire decode"
    (\_ -> case PD.decodeMessage pBytes :: Either PD.DecodeError Person of
             Right v -> v
             Left _  -> defaultPerson)
    (\v -> fromIntegral (personAge v))
  bench "JSON encode" (\_ -> Aeson.encode p) (fromIntegral . BL.length)
  bench "JSON decode"
    (\_ -> case Aeson.eitherDecode pJson :: Either String Person of
             Right v -> v
             Left _  -> defaultPerson)
    (\v -> fromIntegral (personAge v))
  putStrLn ""

  let !n      = sampleNumbers
      !nBytes = PE.encodeMessage n
      !nJson  = Aeson.encode n
  printf "Numbers (10 int32 + 5 doubles, encoded size = %d bytes):\n" (BS.length nBytes)
  bench "wire encode" (\_ -> PE.encodeMessage n) BS.length
  bench "wire decode"
    (\_ -> case PD.decodeMessage nBytes :: Either PD.DecodeError Numbers of
             Right v -> v
             Left _  -> defaultNumbers)
    (\v -> V.length (numbersInts v))
  bench "JSON encode" (\_ -> Aeson.encode n) (fromIntegral . BL.length)
  bench "JSON decode"
    (\_ -> case Aeson.eitherDecode nJson :: Either String Numbers of
             Right v -> v
             Left _  -> defaultNumbers)
    (\v -> V.length (numbersInts v))
  putStrLn ""

  let !c      = sampleChoice
      !cBytes = PE.encodeMessage c
      !cJson  = Aeson.encode c
  printf "Choice (oneof string variant, encoded size = %d bytes):\n" (BS.length cBytes)
  bench "wire encode" (\_ -> PE.encodeMessage c) BS.length
  bench "wire decode"
    (\_ -> case PD.decodeMessage cBytes :: Either PD.DecodeError Choice of
             Right v -> v
             Left _  -> defaultChoice)
    (\v -> case choiceChoice v of
             Just (Choice'Choice'StringValue s) -> T.length s
             _ -> 0)
  bench "JSON encode" (\_ -> Aeson.encode c) (fromIntegral . BL.length)
  bench "JSON decode"
    (\_ -> case Aeson.eitherDecode cJson :: Either String Choice of
             Right v -> v
             Left _  -> defaultChoice)
    (\v -> case choiceChoice v of
             Just (Choice'Choice'StringValue s) -> T.length s
             _ -> 0)
  putStrLn ""

  putStrLn "Sanity baselines (small Aeson Object):"
  let aesonBytes = "{\"name\":\"John Doe\"}" :: BL.ByteString
  bench "Aeson decode tiny obj"
    (\_ -> case Aeson.eitherDecode aesonBytes :: Either String Aeson.Value of
             Right v -> v
             Left _  -> Aeson.Null)
    (\_ -> 0)
  putStrLn ""

  putStrLn "Status enum (open-enum representation):"
  let !sKnown   = sampleStatus
      !sBytesK  = PE.encodeMessage sKnown
      !sUnknown = sampleStatus { personStatus = Status''Unrecognized 12345 }
      !sBytesU  = PE.encodeMessage sUnknown
  bench "encode known enum"   (\_ -> PE.encodeMessage sKnown) BS.length
  bench "decode known enum"
    (\_ -> case PD.decodeMessage sBytesK :: Either PD.DecodeError Person of
             Right v -> v
             Left _  -> defaultPerson)
    (\v -> fromEnum (personStatus v))
  bench "encode unknown enum" (\_ -> PE.encodeMessage sUnknown) BS.length
  bench "decode unknown enum"
    (\_ -> case PD.decodeMessage sBytesU :: Either PD.DecodeError Person of
             Right v -> v
             Left _  -> defaultPerson)
    (\v -> fromEnum (personStatus v))
  putStrLn ""

  -- Direct Timestamp bench. The checked-in
  -- Proto.Google.Protobuf.Timestamp uses a hand-edited
  -- 'loop_dispatch + loop_after_N + withTagM' state-machine shape
  -- that this benchmark measures against the codegen-emitted
  -- shape after regen.
  let !ts      = defaultTimestamp { timestampSeconds = 1234567890, timestampNanos = 999999 }
      !tsBytes = PE.encodeMessage ts
  printf "Timestamp (2 varint scalars, encoded size = %d bytes):\n" (BS.length tsBytes)
  bench "wire encode" (\_ -> PE.encodeMessage ts) BS.length
  bench "wire decode"
    (\_ -> case PD.decodeMessage tsBytes :: Either PD.DecodeError Timestamp of
             Right v -> v
             Left _  -> defaultTimestamp)
    (\v -> fromIntegral (timestampSeconds v))
  putStrLn ""
