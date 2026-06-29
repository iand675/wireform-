{-# LANGUAGE OverloadedStrings #-}

module Test.Metadata (tests) where

import Data.ByteString (ByteString)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Connect.Metadata
import Network.GRPC.Spec (CustomMetadata (..), HeaderName (..))

tests :: Group
tests =
  Group
    "Metadata"
    [ ("ascii leading metadata round-trips", property asciiRoundtrip)
    , ("binary -bin leading metadata round-trips", property binaryRoundtrip)
    , ("trailer- prefix round-trips", property trailerRoundtrip)
    , ("reserved headers are dropped", withTests 1 (property dropsReserved))
    ]

-- ASCII header names: lowercase letters only (valid per the grammar).
genName :: Gen ByteString
genName = Gen.utf8 (Range.linear 1 8) Gen.lower

genAscii :: Gen CustomMetadata
genAscii = do
  nm <- genName
  val <- Gen.utf8 (Range.linear 0 20) Gen.alphaNum
  pure (CustomMetadata (AsciiHeader nm) val)

genBinary :: Gen CustomMetadata
genBinary = do
  nm <- genName
  val <- Gen.bytes (Range.linear 0 20)
  pure (CustomMetadata (BinaryHeader (nm <> "-bin")) val)

asciiRoundtrip :: PropertyT IO ()
asciiRoundtrip = do
  cms <- forAll (Gen.list (Range.linear 0 5) genAscii)
  headersToLeading (leadingToHeaders cms) === cms

binaryRoundtrip :: PropertyT IO ()
binaryRoundtrip = do
  cms <- forAll (Gen.list (Range.linear 0 5) genBinary)
  headersToLeading (leadingToHeaders cms) === cms

trailerRoundtrip :: PropertyT IO ()
trailerRoundtrip = do
  cms <- forAll (Gen.list (Range.linear 0 5) (Gen.choice [genAscii, genBinary]))
  prefixedHeadersToTrailing (trailingToPrefixedHeaders cms) === cms

dropsReserved :: PropertyT IO ()
dropsReserved =
  headersToLeading [("content-type", "application/json"), ("connect-timeout-ms", "5"), ("x-foo", "bar")]
    === [CustomMetadata (AsciiHeader "x-foo") "bar"]
