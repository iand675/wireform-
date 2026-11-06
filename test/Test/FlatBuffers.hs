{-# LANGUAGE OverloadedStrings #-}

module Test.FlatBuffers (flatBuffersTests) where

import Data.Text (Text)
import Data.Vector qualified as V
import FlatBuffers.Decode (decode)
import FlatBuffers.Encode (encode)
import FlatBuffers.Parser (parseFlatBuffers)
import FlatBuffers.Schema (FlatBuffersSchema)
import FlatBuffers.Value qualified as F
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


flatBuffersTests :: Spec
flatBuffersTests = describe "FlatBuffers schema-directed codec" $ do
  it "round-trips full-width signed scalars" $ property $ do
    value <- forAll (Gen.int64 Range.constantBounded)
    let root = F.VTable (V.singleton (Just (F.VInt64 value)))
    (encode (schema "table R { value:long; } root_type R;") root >>= decode (schema "table R { value:long; } root_type R;")) === Right root
  it "round-trips UTF-8 and embedded NUL strings" $ property $ do
    value <- forAll (Gen.text (Range.linear 0 100) Gen.unicode)
    let root = F.VTable (V.singleton (Just (F.VString value)))
        fbs = schema "table R { value:string; } root_type R;"
    (encode fbs root >>= decode fbs) === Right root


schema :: Text -> FlatBuffersSchema
schema = either error id . parseFlatBuffers
