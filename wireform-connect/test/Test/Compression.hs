{-# LANGUAGE OverloadedStrings #-}

module Test.Compression (tests) where

import Data.ByteString (ByteString)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Connect.Compression

tests :: Group
tests =
  Group
    "Compression"
    [ ("gzip round-trips", property (roundtrip Gzip))
    , ("brotli round-trips", property (roundtrip Br))
    , ("zstd round-trips", property (roundtrip Zstd))
    , ("identity round-trips", property (roundtrip Identity))
    , ("negotiate prefers client order intersected with server", withTests 1 (property negotiation))
    ]

roundtrip :: ContentCoding -> PropertyT IO ()
roundtrip coding = do
  bs <- forAll (Gen.bytes (Range.linear 0 2000))
  out <- evalIO (decompress coding (compress coding bs))
  out === Right bs

negotiation :: PropertyT IO ()
negotiation = do
  negotiate [Identity, Gzip] [Br, Gzip, Identity] === Gzip
  negotiate [Identity, Gzip] [Br] === Identity
  negotiate [Identity, Gzip, Zstd] [Zstd, Gzip] === Zstd
  parseAcceptEncoding "gzip, br , zstd" === [Gzip, Br, Zstd]
