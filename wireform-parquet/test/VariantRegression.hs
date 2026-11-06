{-# LANGUAGE OverloadedStrings #-}

module VariantRegression (tests, values, malformed) where

import Control.Monad (forM_, unless)
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Variant


values :: V.Vector Variant
values =
  V.fromList
    [ VNull
    , VBool False
    , VBool True
    , VInt8 minBound
    , VInt8 maxBound
    , VInt16 minBound
    , VInt16 maxBound
    , VInt32 minBound
    , VInt32 maxBound
    , VInt64 minBound
    , VInt64 maxBound
    , VFloat 0
    , VFloat (-0)
    , VFloat (1 / 0)
    , VFloat (-1 / 0)
    , VFloat (castWord32ToFloat 0x7fc12345)
    , VDouble 0
    , VDouble (-0)
    , VDouble (1 / 0)
    , VDouble (-1 / 0)
    , VDouble (castWord64ToDouble 0x7ff8123456789012)
    , VDecimal4 0 0
    , VDecimal4 38 (-999999999)
    , VDecimal4 9 999999999
    , VDecimal8 38 (-999999999999999999)
    , VDecimal8 18 999999999999999999
    , VDecimal16 38 (negate (10 ^ (38 :: Int) - 1))
    , VDecimal16 0 (10 ^ (38 :: Int) - 1)
    , VDate 0
    , VDate (-40000)
    , VTimestamp (-1)
    , VTimestamp 2000000000000000
    , VTimestampNtz (-1)
    , VTimestampNtz 2000000000000000
    , VTimestampNanos minBound
    , VTimestampNanos maxBound
    , VTimestampNtzNanos minBound
    , VTimestampNtzNanos maxBound
    , VTime 0
    , VTime 86399999999
    , VBinary BS.empty
    , VBinary (BS.pack [0 .. 255])
    , VString ""
    , VString "a"
    , VString "λ🦊"
    , VString (T.replicate 63 "x")
    , VString (T.replicate 64 "x")
    , VString (T.replicate 128 "λ")
    , VUuid (BS.pack [0 .. 15])
    , VArray V.empty
    , VObject M.empty
    , VArray (V.fromList [VNull, VInt8 4, VString "five", VObject (M.singleton "nested" (VBool True))])
    , VObject (M.fromList [("a", VInt8 1), ("b", VArray (V.fromList [VNull, VString "λ"])), ("c", VObject (M.singleton "d" (VInt64 (-2))))])
    , VArray (V.replicate 300 (VString "wide"))
    , VObject (M.fromList (map (\n -> (T.replicate n "a", VInt32 (fromIntegral n))) [1 .. 300]))
    ]


-- Each case has a complete enclosing buffer. These specifically distinguish
-- validation from merely retrieving a prefix or parsing a leaf in isolation.
malformed :: [(String, BS.ByteString, BS.ByteString)]
malformed =
  map
    (\(name, metadata, value) -> (name, BS.pack metadata, BS.pack value))
    [ ("trailing-value", [1, 0, 0], [0, 255])
    , ("metadata-nonzero-start", [1, 1, 1, 2, 120, 97], [0])
    , ("metadata-unused-tail", [1, 0, 0, 120], [0])
    , ("metadata-false-sorted", [17, 2, 0, 1, 2, 98, 97], [0])
    , ("metadata-duplicate-sorted", [17, 2, 0, 1, 2, 97, 97], [0])
    , ("duplicate-object-key", [1, 1, 0, 1, 97], [2, 2, 0, 0, 0, 1, 2, 0, 0])
    , ("unordered-object-key", [1, 2, 0, 1, 2, 97, 98], [2, 2, 1, 0, 0, 1, 2, 0, 0])
    , ("shared-object-offset", [1, 2, 0, 1, 2, 97, 98], [2, 2, 0, 1, 0, 0, 1, 0])
    , ("object-offset-gap", [1, 1, 0, 1, 97], [2, 1, 0, 1, 2, 0, 0])
    , ("child-crosses-array-boundary", [1, 0, 0], [3, 1, 0, 1, 12, 127])
    , ("child-crosses-object-boundary", [1, 1, 0, 1, 97], [2, 1, 0, 0, 1, 12, 127])
    , ("array-child-trailing", [1, 0, 0], [3, 1, 0, 2, 0, 255])
    , ("descending-array-offsets", [1, 0, 0], [3, 2, 0, 2, 1, 0, 0])
    , ("decimal-bad-scale", [1, 0, 0], [32, 255, 0, 0, 0, 0])
    , ("decimal4-bad-precision", [1, 0, 0], [32, 0, 0, 202, 154, 59])
    , ("unknown-type", [1, 0, 0], [84])
    , ("invalid-short-utf8", [1, 0, 0], [5, 255])
    , ("invalid-long-utf8", [1, 0, 0], [64, 1, 0, 0, 0, 255])
    , ("huge-array-count", [1, 0, 0], [19, 255, 255, 255, 255])
    , ("huge-object-count", [1, 0, 0], [66, 255, 255, 255, 255])
    , ("huge-dictionary-count", [193, 255, 255, 255, 255], [0])
    , ("unsupported-version", [2, 0, 0], [0])
    ]


tests :: IO ()
tests = do
  let load = either fail pure
      check name condition = unless condition (fail ("Variant regression: " ++ name))
  V.forM_ values $ \value -> do
    encoded <- load (encodeVariant value)
    decoded <- load (decodeVariant (variantMetadata encoded) (variantValue encoded))
    reencoded <- load (encodeVariant decoded)
    -- Byte equality preserves NaN payloads and signed zero, unlike Eq Double.
    check "all primitive/container representations, including IEEE bits" (encoded == reencoded)
  forM_ malformed $ \(name, metadata, value) -> check name (isLeft (decodeVariant metadata value))
  let dictionary = BS.pack [1, 2, 0, 1, 2, 97, 98]
      shuffled = BS.pack [2, 2, 0, 1, 2, 0, 4, 12, 2, 12, 1]
      expected = VObject (M.fromList [("a", VInt8 1), ("b", VInt8 2)])
  check "nonmonotone object offsets remain valid" (decodeVariant dictionary shuffled == Right expected)
  check "reserved metadata and object bits ignored" (decodeVariant (BS.cons (BS.head dictionary .|. 32) (BS.tail dictionary)) (BS.cons (BS.head shuffled .|. 128) (BS.tail shuffled)) == Right expected)
  check "unsorted/nonunique dictionary permitted when not marked sorted" (decodeVariant (BS.pack [1, 3, 0, 1, 2, 3, 98, 97, 97]) (BS.pack [2, 1, 2, 0, 1, 0]) == Right (VObject (M.singleton "a" VNull)))
  forM_ [1 .. 4] $ \width -> do
    let word n = BS.pack (take width (n : repeat 0))
        metadata = BS.singleton (1 + fromIntegral ((width - 1) * 64)) <> word 1 <> word 0 <> word 1 <> "a"
        object = BS.singleton (2 + fromIntegral ((width - 1) * 4 + (width - 1) * 16)) <> BS.singleton 1 <> word 0 <> word 0 <> word 1 <> BS.singleton 0
    check "all legal ID/offset widths" (decodeVariant metadata object == Right (VObject (M.singleton "a" VNull)))
  forM_ [VUuid "short", VUuid (BS.replicate 17 0), VDecimal4 39 0, VDecimal4 0 1000000000, VDecimal8 0 1000000000000000000, VDecimal16 0 (10 ^ (38 :: Int)), VDecimal16 0 (negate (2 ^ (128 :: Int))), VTime (-1), VTime 86400000000] $ \value -> check "encoder never pads/truncates invalid values" (isLeft (encodeVariant value))
  let nested = VArray (V.singleton (VArray (V.singleton VNull)))
  encoded <- load (encodeVariant nested)
  check "depth override" (isLeft (decodeVariantWithLimits (VariantLimits 1 10 10) (variantMetadata encoded) (variantValue encoded)))
  check "aggregate value budget" (isLeft (decodeVariantWithLimits (VariantLimits 5 2 10) (variantMetadata encoded) (variantValue encoded)))
  check "exact budget accepted" (decodeVariantWithLimits (VariantLimits 2 3 0) (variantMetadata encoded) (variantValue encoded) == Right nested)
  check "encoder budget before collection" (isLeft (encodeVariantWithLimits (VariantLimits 1 10 10) nested))
  check "dictionary budget" (isLeft (decodeVariantWithLimits (VariantLimits 5 10 1) dictionary shuffled))
  shared <- load (buildVariantMetadata (V.fromList ["z", "a", "typed-only", "z"]))
  let fragment = VObject (M.singleton "z" (VInt8 42))
  fallback <- load (encodeVariantValue shared fragment)
  check "shredded fallback uses the full shared dictionary" (metadataKeys shared == V.fromList ["a", "typed-only", "z"] && decodeVariant (metadataBytes shared) fallback == Right fragment)
  check "undeclared fallback field fails instead of using a wrong field ID" (isLeft (encodeVariantValue shared (VObject (M.singleton "absent" VNull))))
  originalMetadata <- load (compileVariantMetadata (BS.pack [1, 3, 0, 1, 2, 3, 98, 97, 97]))
  preserved <- load (encodeVariantValue originalMetadata (VObject (M.singleton "a" VNull)))
  check "existing unsorted nonunique dictionary is preserved and indexed correctly" (decodeVariantValue originalMetadata preserved == Right (VObject (M.singleton "a" VNull)) && metadataBytes originalMetadata == BS.pack [1, 3, 0, 1, 2, 3, 98, 97, 97])
  let deep = foldr (\_ value -> VArray (V.singleton value)) VNull [1 .. 501 :: Int]
      raised = VariantLimits 501 502 0
  deepEncoded <- load (encodeVariantWithLimits raised deep)
  check "default depth limit is overridable for valid deep trees" (isLeft (encodeVariant deep) && isLeft (decodeVariant (variantMetadata deepEncoded) (variantValue deepEncoded)) && decodeVariantWithLimits raised (variantMetadata deepEncoded) (variantValue deepEncoded) == Right deep)
  properties <- sequence [H.check roundtripProperty, H.check mutatedProperty, H.check arbitraryBytesProperty]
  check "generated and adversarial binary properties" (and properties)
  putStrLn "Variant binary primitive/container/bounds regressions passed"


genVariant :: H.Gen Variant
genVariant =
  Gen.recursive
    Gen.choice
    [ Gen.element (take 49 (V.toList values))
    , VString <$> Gen.text (Range.linear 0 80) Gen.unicode
    , VBinary <$> Gen.bytes (Range.linear 0 80)
    , VInt64 <$> Gen.int64 Range.constantBounded
    ]
    [ VArray . V.fromList <$> Gen.list (Range.linear 0 8) genVariant
    , VObject . M.fromList <$> Gen.list (Range.linear 0 8) ((,) <$> Gen.text (Range.linear 0 20) Gen.unicode <*> genVariant)
    ]


roundtripProperty :: H.Property
roundtripProperty = H.withTests 1000 $ H.property $ do
  value <- H.forAll genVariant
  encoded <- H.evalEither (encodeVariant value)
  decoded <- H.evalEither (decodeVariant (variantMetadata encoded) (variantValue encoded))
  reencoded <- H.evalEither (encodeVariant decoded)
  reencoded H.=== encoded


mutatedProperty :: H.Property
mutatedProperty = H.withTests 3000 $ H.property $ do
  value <- H.forAll genVariant
  encoded <- H.evalEither (encodeVariant value)
  changeMetadata <- H.forAll Gen.bool
  replacement <- H.forAll (Gen.bytes (Range.linear 0 10))
  let target = if changeMetadata then variantMetadata encoded else variantValue encoded
  offset <- H.forAll (Gen.int (Range.linear 0 (BS.length target)))
  removed <- H.forAll (Gen.int (Range.linear 0 (BS.length target - offset)))
  let changed = BS.take offset target <> replacement <> BS.drop (offset + removed) target
      metadata = if changeMetadata then changed else variantMetadata encoded
      bytes = if changeMetadata then variantValue encoded else changed
  assertStable metadata bytes


arbitraryBytesProperty :: H.Property
arbitraryBytesProperty = H.withTests 3000 $ H.property $ do
  metadata <- H.forAll (Gen.bytes (Range.linear 0 128))
  bytes <- H.forAll (Gen.bytes (Range.linear 0 128))
  assertStable metadata bytes


assertStable :: BS.ByteString -> BS.ByteString -> H.PropertyT IO ()
assertStable metadata bytes = case decodeVariant metadata bytes of
  Left _ -> H.success
  Right value -> do
    encoded <- H.evalEither (encodeVariant value)
    decoded <- H.evalEither (decodeVariant (variantMetadata encoded) (variantValue encoded))
    stable <- H.evalEither (encodeVariant decoded)
    stable H.=== encoded
