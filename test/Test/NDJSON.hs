module Test.NDJSON (ndjsonTests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import NDJSON.Decode
import NDJSON.Encode

ndjsonTests :: TestTree
ndjsonTests = testGroup "NDJSON"
  [ parseTests
  , emptyLineTests
  , streamTests
  , typedRecordTests
  , roundtripTests
  , largeFileTests
  ]

parseTests :: TestTree
parseTests = testGroup "Basic parsing"
  [ testCase "Parse multi-line NDJSON" $ do
      let input = "{\"a\":1}\n{\"b\":2}\n{\"c\":3}"
          Right vals = decode (BSC.pack input)
      V.length vals @?= 3
      vals V.! 0 @?= Aeson.object [("a", Aeson.Number 1)]
      vals V.! 1 @?= Aeson.object [("b", Aeson.Number 2)]
      vals V.! 2 @?= Aeson.object [("c", Aeson.Number 3)]

  , testCase "Parse single line" $ do
      let input = "{\"x\":42}"
          Right vals = decode (BSC.pack input)
      V.length vals @?= 1

  , testCase "Parse arrays" $ do
      let input = "[1,2,3]\n[4,5,6]"
          Right vals = decode (BSC.pack input)
      V.length vals @?= 2

  , testCase "Parse mixed types" $ do
      let input = "\"hello\"\n42\ntrue\nnull"
          Right vals = decode (BSC.pack input)
      V.length vals @?= 4
      vals V.! 0 @?= Aeson.String "hello"
      vals V.! 1 @?= Aeson.Number 42
      vals V.! 2 @?= Aeson.Bool True
      vals V.! 3 @?= Aeson.Null
  ]

emptyLineTests :: TestTree
emptyLineTests = testGroup "Empty lines"
  [ testCase "Empty lines are skipped" $ do
      let input = "{\"a\":1}\n\n{\"b\":2}\n\n"
          Right vals = decode (BSC.pack input)
      V.length vals @?= 2

  , testCase "Only newlines yields empty" $ do
      let input = "\n\n\n"
          Right vals = decode (BSC.pack input)
      V.length vals @?= 0

  , testCase "Empty input" $ do
      let Right vals = decode BS.empty
      V.length vals @?= 0
  ]

streamTests :: TestTree
streamTests = testGroup "Streaming"
  [ testCase "Streaming decode processes all values" $ do
      let input = "{\"a\":1}\n{\"b\":2}\n{\"c\":3}"
      ref <- newIORef []
      result <- decodeStream (BSC.pack input) $ \val ->
        modifyIORef' ref (val :)
      result @?= Right ()
      vals <- reverse <$> readIORef ref
      length vals @?= 3

  , testCase "Streaming decode skips empty lines" $ do
      let input = "{\"a\":1}\n\n{\"b\":2}\n"
      ref <- newIORef []
      result <- decodeStream (BSC.pack input) $ \val ->
        modifyIORef' ref (val :)
      result @?= Right ()
      vals <- reverse <$> readIORef ref
      length vals @?= 2
  ]

data TestRecord = TestRecord
  { name :: !String
  , value :: !Int
  } deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

typedRecordTests :: TestTree
typedRecordTests = testGroup "Typed records"
  [ testCase "Parse typed records" $ do
      let input = "{\"name\":\"foo\",\"value\":1}\n{\"name\":\"bar\",\"value\":2}"
          Right records = decodeRecords (BSC.pack input) :: Either String (V.Vector TestRecord)
      V.length records @?= 2
      records V.! 0 @?= TestRecord "foo" 1
      records V.! 1 @?= TestRecord "bar" 2
  ]

roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testCase "Encode and decode objects" $ do
      let vals = V.fromList
            [ Aeson.object [("a", Aeson.Number 1)]
            , Aeson.object [("b", Aeson.String "hello")]
            ]
          encoded = encode vals
          Right decoded = decode encoded
      decoded @?= vals

  , testCase "Encode and decode records" $ do
      let records = V.fromList [TestRecord "x" 1, TestRecord "y" 2]
          encoded = encodeRecords records
          Right decoded = decodeRecords encoded :: Either String (V.Vector TestRecord)
      decoded @?= records

  , testProperty "Roundtrip with random objects" $ property $ do
      n <- forAll $ Gen.int (Range.linear 1 20)
      vals <- forAll $ V.replicateM n $ do
        k <- Gen.text (Range.linear 1 10) Gen.alpha
        v <- Gen.int (Range.linear (-1000) 1000)
        pure $ Aeson.object [(Key.fromText k, Aeson.Number (fromIntegral v))]
      let encoded = encode vals
      case decode encoded of
        Left err -> do
          annotate err
          failure
        Right decoded -> decoded === vals

  , testCase "encoder always terminates with LF (ndjson-spec)" $ do
      -- ndjson-spec: every record's trailing newline is required so
      -- consumers can append more records by byte concatenation.
      let vals = V.fromList
            [ Aeson.object [("a", Aeson.Number 1)]
            , Aeson.object [("b", Aeson.Number 2)]
            ]
          encoded = encode vals
      assertBool "trailing LF present"
        (BS.length encoded > 0 && BS.last encoded == 0x0A)
      -- Concatenating two NDJSON streams yields a valid NDJSON stream.
      let combined = encoded <> encoded
      case decode combined of
        Right decoded -> decoded @?= (vals V.++ vals)
        Left err -> assertFailure $ "decode failed: " ++ err
  ]

largeFileTests :: TestTree
largeFileTests = testGroup "Large files"
  [ testCase "10k lines" $ do
      let mkLine i = BSC.pack $ "{\"id\":" ++ show i ++ "}"
          input = BS.intercalate "\n" [mkLine i | i <- [1..10000 :: Int]]
          Right vals = decode input
      V.length vals @?= 10000
  ]
