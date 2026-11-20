{-# LANGUAGE OverloadedStrings #-}

module MetadataRegression (tests) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int16)
import Data.Vector qualified as V
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Footer qualified as Footer
import Parquet.Types
import Thrift.Encode qualified as Thrift
import Thrift.Value qualified as TV
import Thrift.Wire (ThriftType (..))


-- Raw field numbers supply fixtures independently of the named field aliases.
baseFields :: V.Vector (Int16, TV.Value)
baseFields =
  V.fromList
    [ (1, TV.I32 1)
    , (2, TV.List TT_STRUCT (V.singleton (TV.Struct (V.fromList [(4, TV.String "schema"), (5, TV.I32 0)]))))
    , (3, TV.I64 0)
    , (4, TV.List TT_STRUCT V.empty)
    ]


tests :: IO ()
tests = do
  let parse fields = Footer.readFooterRaw (Thrift.encodeCompact (TV.Struct (baseFields V.++ fields)))
      check name condition = unless condition (fail ("metadata regression: " ++ name))
      entry key value = TV.Struct (V.singleton (1, TV.Binary key) V.++ maybe V.empty (V.singleton . (2,) . TV.Binary) value)
      rows = V.fromList [KeyValue "x" Nothing, KeyValue "x" (Just ""), KeyValue "x" (Just "last"), KeyValue (BS.pack [255, 0]) (Just (BS.pack [128, 0, 255]))]
      wireRows = V.map (\row -> entry (kvKey row) (kvValue row)) rows
  without <- either fail pure (parse V.empty)
  check "absent list remains absent" (fmKeyValueMetadata without == Nothing)
  empty <- either fail pure (parse (V.singleton (5, TV.List TT_STRUCT V.empty)))
  check "present empty list remains present" (fmKeyValueMetadata empty == Just V.empty)
  decoded <- either fail pure (parse (V.singleton (5, TV.List TT_STRUCT wireRows)))
  check "raw fields preserve order, duplicate keys and optional/binary values" (fmKeyValueMetadata decoded == Just rows)
  case Footer.fileMetadataToThrift decoded of
    TV.Struct fields ->
      check
        "encoder uses field 5 and KeyValue fields 1/2"
        (fmap snd (V.find ((== 5) . fst) fields) == Just (TV.List TT_STRUCT wireRows))
    _ -> fail "metadata regression: encoder did not produce a struct"
  text <- either fail pure (parse (V.singleton (5, TV.List TT_STRUCT (V.singleton (TV.Struct (V.fromList [(1, TV.String "crs"), (2, TV.String "EPSG:3857")]))))))
  check "decoded Text is retained as UTF-8 bytes" (fmKeyValueMetadata text == Just (V.singleton (KeyValue "crs" (Just "EPSG:3857"))))
  let malformed =
        [ V.singleton (5, TV.I32 1)
        , V.fromList [(5, TV.List TT_STRUCT V.empty), (5, TV.List TT_STRUCT V.empty)]
        , V.singleton (5, TV.List TT_I32 (V.singleton (TV.I32 1)))
        , V.singleton (5, TV.List TT_STRUCT (V.singleton (TV.Struct V.empty)))
        , V.singleton (5, TV.List TT_STRUCT (V.singleton (TV.Struct (V.singleton (1, TV.I32 1)))))
        , V.singleton (5, TV.List TT_STRUCT (V.singleton (TV.Struct (V.fromList [(1, TV.Binary "x"), (1, TV.Binary "x")]))))
        , V.singleton (5, TV.List TT_STRUCT (V.singleton (TV.Struct (V.fromList [(1, TV.Binary "x"), (2, TV.Bool True)]))))
        , V.singleton (5, TV.List TT_STRUCT (V.singleton (TV.Struct (V.fromList [(1, TV.Binary "x"), (2, TV.Binary "a"), (2, TV.Binary "b")]))))
        ]
  mapM_ (\fields -> check "malformed/duplicate known fields are rejected" (isLeft (parse fields))) malformed
  passed <- H.check $ H.withTests 100 $ H.property $ do
    values <- H.forAll (Gen.list (Range.linear 0 40) (KeyValue <$> Gen.bytes (Range.linear 0 40) <*> Gen.maybe (Gen.bytes (Range.linear 0 40))))
    let expected = V.fromList values
        metadata = without {fmKeyValueMetadata = Just expected}
    loaded <- H.evalEither (Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift metadata)))
    fmKeyValueMetadata loaded H.=== Just expected
  check "opaque application metadata wire roundtrip property" passed
