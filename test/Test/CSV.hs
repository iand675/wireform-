module Test.CSV (csvTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import CSV.Value
import CSV.Decode
import CSV.Encode
import CSV.Class

csvTests :: TestTree
csvTests = testGroup "CSV"
  [ parseTests
  , quoteTests
  , tsvTests
  , emptyFieldTests
  , roundtripTests
  , streamTests
  , genericTests
  ]

parseTests :: TestTree
parseTests = testGroup "Basic parsing"
  [ testCase "Simple CSV with header" $ do
      let input = "name,age\nAlice,30\nBob,25"
          Right doc = decode defaultCSV (BSC.pack input)
      csvHeader doc @?= Just (V.fromList ["name", "age"])
      csvRows doc @?= V.fromList
        [ V.fromList ["Alice", "30"]
        , V.fromList ["Bob", "25"]
        ]

  , testCase "CSV without header" $ do
      let cfg = defaultCSV { csvHasHeader = False }
          input = "Alice,30\nBob,25"
          Right doc = decode cfg (BSC.pack input)
      csvHeader doc @?= Nothing
      csvRows doc @?= V.fromList
        [ V.fromList ["Alice", "30"]
        , V.fromList ["Bob", "25"]
        ]

  , testCase "Single row" $ do
      let input = "a,b,c"
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      csvRows doc @?= V.fromList [V.fromList ["a", "b", "c"]]

  , testCase "CRLF line endings" $ do
      let input = "x,y\r\n1,2\r\n3,4"
          Right doc = decode defaultCSV (BSC.pack input)
      csvHeader doc @?= Just (V.fromList ["x", "y"])
      csvRows doc @?= V.fromList
        [ V.fromList ["1", "2"]
        , V.fromList ["3", "4"]
        ]
  ]

quoteTests :: TestTree
quoteTests = testGroup "Quoted fields"
  [ testCase "Field with embedded comma" $ do
      let input = "a,\"hello, world\",c"
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      csvRows doc @?= V.fromList [V.fromList ["a", "hello, world", "c"]]

  , testCase "Escaped quotes (doubled)" $ do
      let input = "\"she said \"\"hi\"\"\""
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      V.head (csvRows doc) @?= V.fromList ["she said \"hi\""]

  , testCase "Quoted field with newline" $ do
      let input = "\"line1\nline2\",b"
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      V.head (csvRows doc) @?= V.fromList ["line1\nline2", "b"]

  , testCase "Empty quoted field" $ do
      let input = "\"\",b"
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      V.head (csvRows doc) @?= V.fromList ["", "b"]
  ]

tsvTests :: TestTree
tsvTests = testGroup "TSV"
  [ testCase "Tab-separated values" $ do
      let input = "name\tage\nAlice\t30\nBob\t25"
          Right doc = decode defaultTSV (BSC.pack input)
      csvHeader doc @?= Just (V.fromList ["name", "age"])
      csvRows doc @?= V.fromList
        [ V.fromList ["Alice", "30"]
        , V.fromList ["Bob", "25"]
        ]

  , testCase "TSV with quoted tab" $ do
      let input = "\"a\tb\"\tc"
          cfg = defaultTSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      V.head (csvRows doc) @?= V.fromList ["a\tb", "c"]
  ]

emptyFieldTests :: TestTree
emptyFieldTests = testGroup "Empty fields"
  [ testCase "Empty fields at various positions" $ do
      let input = ",b,\na,,c"
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      csvRows doc @?= V.fromList
        [ V.fromList ["", "b", ""]
        , V.fromList ["a", "", "c"]
        ]

  , testCase "All empty fields" $ do
      let input = ",,"
          cfg = defaultCSV { csvHasHeader = False }
          Right doc = decode cfg (BSC.pack input)
      csvRows doc @?= V.fromList [V.fromList ["", "", ""]]

  , testCase "decoder strips a leading UTF-8 BOM (Excel export quirk)" $ do
      -- 0xEF 0xBB 0xBF is the UTF-8 BOM, prepended by Excel and many
      -- spreadsheet exporters. Without stripping, the first header
      -- column shows up as "\xFEFFname" instead of "name".
      let bs = BS.pack [0xEF, 0xBB, 0xBF]
                <> BSC.pack "name,age\nAlice,30\n"
          Right doc = decode defaultCSV bs
      csvHeader doc @?= Just (V.fromList ["name", "age"])
      csvRows doc @?= V.fromList [V.fromList ["Alice", "30"]]
  ]

roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testProperty "CSV encode-decode roundtrip" $ property $ do
      nRows <- forAll $ Gen.int (Range.linear 1 20)
      nCols <- forAll $ Gen.int (Range.linear 1 5)
      rows <- forAll $ V.replicateM nRows $
        V.replicateM nCols (Gen.text (Range.linear 0 20) Gen.alphaNum)
      let cfg = defaultCSV { csvHasHeader = False }
          doc = CSVDocument Nothing rows
          encoded = encode cfg doc
      case decode cfg encoded of
        Left err -> do
          annotate err
          failure
        Right doc' -> csvRows doc' === csvRows doc

  , testProperty "CSV with header roundtrip" $ property $ do
      nCols <- forAll $ Gen.int (Range.linear 1 5)
      header <- forAll $ V.replicateM nCols (Gen.text (Range.linear 1 10) Gen.alpha)
      nRows <- forAll $ Gen.int (Range.linear 0 10)
      rows <- forAll $ V.replicateM nRows $
        V.replicateM nCols (Gen.text (Range.linear 0 15) Gen.alphaNum)
      let doc = CSVDocument (Just header) rows
          encoded = encode defaultCSV doc
      case decode defaultCSV encoded of
        Left err -> do
          annotate err
          failure
        Right doc' -> do
          csvHeader doc' === csvHeader doc
          csvRows doc' === csvRows doc
  ]

streamTests :: TestTree
streamTests = testGroup "Streaming"
  [ testCase "Streaming decode collects all rows" $ do
      let input = "h1,h2\na,1\nb,2\nc,3"
          ref = V.fromList
            [ V.fromList ["a", "1"]
            , V.fromList ["b", "2"]
            , V.fromList ["c", "3"]
            ]
      collected <- newIORef []
      result <- decodeStream defaultCSV (BSC.pack input) $ \row ->
        modifyIORef' collected (row :)
      result @?= Right ()
      rows <- reverse <$> readIORef collected
      V.fromList rows @?= ref
  ]

data Person = Person
  { personName :: !Text
  , personAge  :: !Int
  } deriving stock (Show, Eq, Generic)

instance ToCSV Person where
  toCSVRow p = V.fromList [personName p, T.pack (show (personAge p))]

instance FromCSV Person where
  fromCSVRow v
    | V.length v < 2 = Left "not enough fields for Person"
    | otherwise = do
        let name = v V.! 0
        age <- case reads (T.unpack (v V.! 1)) of
                 [(n, "")] -> Right n
                 _         -> Left "cannot parse age"
        Right (Person name age)

genericTests :: TestTree
genericTests = testGroup "Generic deriving"
  [ testCase "Person record from CSV" $ do
      let input = "name,age\nAlice,30\nBob,25"
          Right persons = decodeRecords defaultCSV (BSC.pack input) :: Either String (V.Vector Person)
      persons @?= V.fromList [Person "Alice" 30, Person "Bob" 25]

  , testCase "Person record roundtrip" $ do
      let cfg = defaultCSV { csvHasHeader = False }
          persons = V.fromList [Person "Alice" 30, Person "Bob" 25]
          encoded = encodeRecords cfg persons
          Right decoded = decodeRecords cfg encoded :: Either String (V.Vector Person)
      decoded @?= persons
  ]

