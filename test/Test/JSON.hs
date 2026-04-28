module Test.JSON (jsonTests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Aeson.Types as AesonT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Int (Int64)
import Data.Word (Word64)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.JSON

jsonTests :: TestTree
jsonTests = testGroup "JSON representation helpers"
  [ testGroup "Int64 / UInt64 (proto3 string-encoded)"
      [ testCase "int64FieldToJSON emits string" $ do
          let (_, v) = int64FieldToJSON "n" (9007199254740993 :: Int64)
          v @?= Aeson.String "9007199254740993"

      , testCase "uint64FieldToJSON emits string" $ do
          let (_, v) = uint64FieldToJSON "n" (18446744073709551615 :: Word64)
          v @?= Aeson.String "18446744073709551615"

      , testCase "parseInt64FieldMaybe accepts string" $ do
          let obj = mkObj [("n", Aeson.String "12345")]
          AesonT.parseEither (parseInt64FieldMaybe obj) "n"
            @?= Right (Just (12345 :: Int64))

      , testCase "parseInt64FieldMaybe accepts number for compat" $ do
          let obj = mkObj [("n", Aeson.Number 12345)]
          AesonT.parseEither (parseInt64FieldMaybe obj) "n"
            @?= Right (Just (12345 :: Int64))

      , testCase "parseUInt64FieldMaybe rejects negative number" $ do
          let obj = mkObj [("n", Aeson.String "-1")]
          case AesonT.parseEither (parseUInt64FieldMaybe obj) "n" of
            Left _  -> pure ()
            Right v -> assertFailure $
              "expected rejection of negative uint64, got " <> show v

      , testCase "int64MapFieldToJSON emits map of strings" $ do
          let (_, v) = int64MapFieldToJSON "m"
                         (Map.fromList [("a", 1), ("b", -2)])
          v @?= Aeson.Object (AesonKM.fromList
                  [ ("a", Aeson.String "1")
                  , ("b", Aeson.String "-2")
                  ])
      ]

  , testGroup "Strict ByteString (base64)"
      [ testProperty "roundtrip via Value" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let val = protoBytesToJSON bs
          parsed <- evalEither (AesonT.parseEither protoBytesFromJSON val)
          parsed === bs

      , testCase "encodes as base64 string" $ do
          let val = protoBytesToJSON (BS.pack [0x00, 0xFF, 0x42])
          case val of
            Aeson.String _ -> pure ()
            other -> assertFailure ("Expected String, got " <> show other)

      , testProperty "field helper roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let (_, val) = bytesFieldToJSON "data" bs
              obj = mkObj [("data", val)]
          result <- evalEither (AesonT.parseEither (parseBytesFieldMaybe obj) "data")
          result === Just bs
      ]

  , testGroup "Lazy ByteString (base64)"
      [ testProperty "roundtrip via Value" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let lbs = BL.fromStrict bs
              val = protoLazyBytesToJSON lbs
          parsed <- evalEither (AesonT.parseEither protoLazyBytesFromJSON val)
          parsed === lbs

      , testProperty "field helper roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let lbs = BL.fromStrict bs
              (_, val) = lazyBytesFieldToJSON "blob" lbs
              obj = mkObj [("blob", val)]
          result <- evalEither (AesonT.parseEither (parseLazyBytesFieldMaybe obj) "blob")
          result === Just lbs
      ]

  , testGroup "ShortByteString (base64)"
      [ testProperty "roundtrip via Value" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let sbs = SBS.toShort bs
              val = protoShortBytesToJSON sbs
          parsed <- evalEither (AesonT.parseEither protoShortBytesFromJSON val)
          parsed === sbs

      , testProperty "field helper roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let sbs = SBS.toShort bs
              (_, val) = shortBytesFieldToJSON "compact" sbs
              obj = mkObj [("compact", val)]
          result <- evalEither (AesonT.parseEither (parseShortBytesFieldMaybe obj) "compact")
          result === Just sbs
      ]

  , testGroup "Strict Text"
      [ testProperty ".=: / parseFieldMaybe roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let (_, val) = "name" .=: t
              obj = mkObj [("name", val)]
          result <- evalEither (AesonT.parseEither (parseFieldMaybe obj) "name")
          result === Just t
      ]

  , testGroup "Lazy Text"
      [ testProperty "field helper roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let lt = TL.fromStrict t
              (_, val) = lazyTextFieldToJSON "desc" lt
              obj = mkObj [("desc", val)]
          result <- evalEither (AesonT.parseEither (parseLazyTextFieldMaybe obj) "desc")
          result === Just lt
      ]

  , testGroup "ShortByteString as text (UTF-8)"
      [ testProperty "field helper roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let sbs = SBS.toShort (TE.encodeUtf8 t)
              (_, val) = shortTextFieldToJSON "tag" sbs
              obj = mkObj [("tag", val)]
          result <- evalEither (AesonT.parseEither (parseShortTextFieldMaybe obj) "tag")
          result === Just sbs
      ]

  , testGroup "Haskell String"
      [ testProperty "field helper roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let s = T.unpack t
              (_, val) = hsStringFieldToJSON "label" s
              obj = mkObj [("label", val)]
          result <- evalEither (AesonT.parseEither (parseHsStringFieldMaybe obj) "label")
          result === Just s
      ]

  , testGroup "Missing / null field handling"
      [ testCase "parseBytesFieldMaybe missing -> Nothing" $ do
          let obj = mkObj []
          runParserOK (parseBytesFieldMaybe obj "x") >>= (@?= Nothing)

      , testCase "parseBytesFieldMaybe null -> Nothing" $ do
          let obj = mkObj [("x", Aeson.Null)]
          runParserOK (parseBytesFieldMaybe obj "x") >>= (@?= Nothing)

      , testCase "parseLazyBytesFieldMaybe missing -> Nothing" $ do
          let obj = mkObj []
          runParserOK (parseLazyBytesFieldMaybe obj "x") >>= (@?= Nothing)

      , testCase "parseShortBytesFieldMaybe missing -> Nothing" $ do
          let obj = mkObj []
          runParserOK (parseShortBytesFieldMaybe obj "x") >>= (@?= Nothing)

      , testCase "parseLazyTextFieldMaybe missing -> Nothing" $ do
          let obj = mkObj []
          runParserOK (parseLazyTextFieldMaybe obj "x") >>= (@?= Nothing)

      , testCase "parseShortTextFieldMaybe missing -> Nothing" $ do
          let obj = mkObj []
          runParserOK (parseShortTextFieldMaybe obj "x") >>= (@?= Nothing)

      , testCase "parseHsStringFieldMaybe missing -> Nothing" $ do
          let obj = mkObj []
          runParserOK (parseHsStringFieldMaybe obj "x") >>= (@?= Nothing)
      ]

  , testGroup "Type mismatch errors"
      [ testCase "parseBytesFieldMaybe non-string -> fail" $
          assertParserFails (parseBytesFieldMaybe (mkObj [("x", Aeson.Number 42)]) "x")

      , testCase "parseLazyTextFieldMaybe non-string -> fail" $
          assertParserFails (parseLazyTextFieldMaybe (mkObj [("x", Aeson.Bool True)]) "x")

      , testCase "parseShortTextFieldMaybe non-string -> fail" $
          assertParserFails (parseShortTextFieldMaybe (mkObj [("x", Aeson.Number 1)]) "x")

      , testCase "parseHsStringFieldMaybe non-string -> fail" $
          assertParserFails (parseHsStringFieldMaybe (mkObj [("x", Aeson.Array mempty)]) "x")
      ]

  , testGroup "Cross-representation consistency"
      [ testProperty "all bytes reps produce same base64" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 200)
          let strict = protoBytesToJSON bs
              lazy   = protoLazyBytesToJSON (BL.fromStrict bs)
              short  = protoShortBytesToJSON (SBS.toShort bs)
          strict === lazy
          strict === short

      , testProperty "all text reps produce same JSON string" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let (_, strictVal)  = "k" .=: t
              (_, lazyVal)    = lazyTextFieldToJSON "k" (TL.fromStrict t)
              (_, shortVal)   = shortTextFieldToJSON "k" (SBS.toShort (TE.encodeUtf8 t))
              (_, stringVal)  = hsStringFieldToJSON "k" (T.unpack t)
          strictVal === lazyVal
          strictVal === shortVal
          strictVal === stringVal
      ]

  , testGroup "Map representations"
      [ testGroup "Ordered Map (Map.Map)"
          [ testProperty "ordMapToJSON roundtrip" $ property $ do
              keys <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.text (Range.linear 1 20) Gen.alphaNum)
              vals <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.int32 (Range.linear (-1000) 1000))
              let m = Map.fromList (zip keys vals)
                  encoded = ordMapToJSON m
              parsed <- evalEither (AesonT.parseEither parseOrdMapFromJSON encoded)
              parsed === m

          , testCase "ordMapToJSON empty" $ do
              let m = Map.empty :: Map.Map Text Int
                  encoded = ordMapToJSON m
              encoded @?= Aeson.object []

          , testCase "ordMapToJSON preserves entries" $ do
              let m = Map.fromList [("a" :: Text, 1 :: Int), ("b", 2)]
                  Aeson.Object o = ordMapToJSON m
              AesonT.parseEither (parseOrdMapFromJSON . Aeson.Object) o @?= Right m
          ]

      , testGroup "HashMap"
          [ testProperty "hashMapToJSON roundtrip" $ property $ do
              keys <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.text (Range.linear 1 20) Gen.alphaNum)
              vals <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.int32 (Range.linear (-1000) 1000))
              let m = HM.fromList (zip keys vals)
                  encoded = hashMapToJSON m
              parsed <- evalEither (AesonT.parseEither parseHashMapFromJSON encoded)
              parsed === m

          , testCase "hashMapToJSON empty" $ do
              let m = HM.empty :: HM.HashMap Text Int
                  encoded = hashMapToJSON m
              encoded @?= Aeson.object []

          , testCase "hashMapToJSON preserves entries" $ do
              let m = HM.fromList [("x" :: Text, True), ("y", False)]
                  Aeson.Object o = hashMapToJSON m
              AesonT.parseEither (parseHashMapFromJSON . Aeson.Object) o @?= Right m
          ]

      , testGroup "Cross-representation consistency"
          [ testProperty "ordMap and hashMap produce same JSON" $ property $ do
              keys <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.text (Range.linear 1 20) Gen.alphaNum)
              vals <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.int32 (Range.linear (-1000) 1000))
              let ordM = Map.fromList (zip keys vals)
                  hashM = HM.fromList (zip keys vals)
                  ordJSON = ordMapToJSON ordM
                  hashJSON = hashMapToJSON hashM
              normalizeObject ordJSON === normalizeObject hashJSON

          , testProperty "ordMap JSON parses as hashMap and vice versa" $ property $ do
              keys <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.text (Range.linear 1 20) Gen.alphaNum)
              vals <- forAll $ Gen.list (Range.linear 0 10)
                        (Gen.int32 (Range.linear (-1000) 1000))
              let ordM = Map.fromList (zip keys vals)
                  encoded = ordMapToJSON ordM
              parsedAsHash <- evalEither (AesonT.parseEither parseHashMapFromJSON encoded)
              Map.fromList (HM.toList parsedAsHash) === ordM
          ]

      , testCase "parseOrdMapFromJSON non-object -> fail" $
          assertParserFails (parseOrdMapFromJSON (Aeson.String "nope") :: AesonT.Parser (Map.Map Text Int))

      , testCase "parseHashMapFromJSON non-object -> fail" $
          assertParserFails (parseHashMapFromJSON (Aeson.Number 42) :: AesonT.Parser (HM.HashMap Text Int))
      ]
  ]

mkObj :: [(Text, Aeson.Value)] -> Aeson.Object
mkObj kvs = case Aeson.object (fmap (\(k, v) -> AesonKey.fromText k Aeson..= v) kvs) of
  Aeson.Object o -> o
  _ -> error "impossible"

runParserOK :: (Show a, Eq a) => AesonT.Parser a -> IO a
runParserOK p = case AesonT.parseEither (const p) () of
  Right a -> pure a
  Left e  -> assertFailure ("Parser failed: " <> e) >> error "unreachable"

assertParserFails :: AesonT.Parser a -> IO ()
assertParserFails p = case AesonT.parseEither (const p) () of
  Left _  -> pure ()
  Right _ -> assertFailure "Expected parser to fail"

normalizeObject :: Aeson.Value -> Map.Map Text Aeson.Value
normalizeObject (Aeson.Object o) =
  Map.fromList (fmap (\(k, v) -> (AesonKey.toText k, v)) (AesonKM.toList o))
normalizeObject _ = Map.empty
