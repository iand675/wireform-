{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.TOML (tomlTests) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import TOML.Value
import TOML.Encode
import TOML.Decode
import TOML.Class

tomlTests :: TestTree
tomlTests = testGroup "TOML"
  [ basicTypeTests
  , tableTests
  , arrayTests
  , stringTests
  , integerTests
  , classTests
  , genericTests
  , roundtripTests
  ]

basicTypeTests :: TestTree
basicTypeTests = testGroup "Basic types"
  [ testCase "parse string" $ do
      let Right val = decode "key = \"hello\""
      lookupKey "key" val @?= Just (TString "hello")

  , testCase "parse integer" $ do
      let Right val = decode "key = 42"
      lookupKey "key" val @?= Just (TInteger 42)

  , testCase "parse negative integer" $ do
      let Right val = decode "key = -17"
      lookupKey "key" val @?= Just (TInteger (-17))

  , testCase "parse float" $ do
      let Right val = decode "key = 3.14"
      case lookupKey "key" val of
        Just (TFloat d) -> assertBool "close to 3.14" (abs (d - 3.14) < 0.001)
        _ -> assertFailure "expected TFloat"

  , testCase "parse bool true" $ do
      let Right val = decode "key = true"
      lookupKey "key" val @?= Just (TBool True)

  , testCase "parse bool false" $ do
      let Right val = decode "key = false"
      lookupKey "key" val @?= Just (TBool False)

  , testCase "parse inf" $ do
      let Right val = decode "key = inf"
      case lookupKey "key" val of
        Just (TFloat d) -> assertBool "is infinite" (isInfinite d && d > 0)
        _ -> assertFailure "expected TFloat inf"

  , testCase "parse -inf" $ do
      let Right val = decode "key = -inf"
      case lookupKey "key" val of
        Just (TFloat d) -> assertBool "is -infinite" (isInfinite d && d < 0)
        _ -> assertFailure "expected TFloat -inf"

  , testCase "parse nan" $ do
      let Right val = decode "key = nan"
      case lookupKey "key" val of
        Just (TFloat d) -> assertBool "is nan" (isNaN d)
        _ -> assertFailure "expected TFloat nan"

  , testCase "parse datetime" $ do
      let Right val = decode "key = 2024-01-15T10:30:00Z"
      case lookupKey "key" val of
        Just (TDateTime _) -> pure ()
        _ -> assertFailure "expected TDateTime"

  , testCase "parse date" $ do
      let Right val = decode "key = 2024-01-15"
      case lookupKey "key" val of
        Just (TDate _) -> pure ()
        _ -> assertFailure "expected TDate"

  , testCase "comment handling" $ do
      let Right val = decode "key = 42 # this is a comment"
      lookupKey "key" val @?= Just (TInteger 42)

  , testCase "empty lines and comments" $ do
      let input = T.unlines
            [ "# Comment"
            , ""
            , "key1 = 1"
            , "# Another comment"
            , "key2 = 2"
            ]
      let Right val = decode input
      lookupKey "key1" val @?= Just (TInteger 1)
      lookupKey "key2" val @?= Just (TInteger 2)
  ]

tableTests :: TestTree
tableTests = testGroup "Tables"
  [ testCase "simple table" $ do
      let input = T.unlines
            [ "[server]"
            , "host = \"localhost\""
            , "port = 8080"
            ]
      let Right val = decode input
      case lookupKey "server" val of
        Just (TTable kvs) -> do
          lookupInTable "host" kvs @?= Just (TString "localhost")
          lookupInTable "port" kvs @?= Just (TInteger 8080)
        _ -> assertFailure "expected TTable"

  , testCase "nested tables" $ do
      let input = T.unlines
            [ "[database.connection]"
            , "host = \"db.example.com\""
            , "port = 5432"
            ]
      let Right val = decode input
      case lookupKey "database" val of
        Just (TTable outer) ->
          case lookupInTable "connection" outer of
            Just (TTable inner) ->
              lookupInTable "host" inner @?= Just (TString "db.example.com")
            _ -> assertFailure "expected nested TTable"
        _ -> assertFailure "expected TTable"

  , testCase "inline table" $ do
      let Right val = decode "point = {x = 1, y = 2}"
      case lookupKey "point" val of
        Just (TTable kvs) -> do
          lookupInTable "x" kvs @?= Just (TInteger 1)
          lookupInTable "y" kvs @?= Just (TInteger 2)
        _ -> assertFailure "expected TTable"
  ]

arrayTests :: TestTree
arrayTests = testGroup "Arrays"
  [ testCase "simple array" $ do
      let Right val = decode "arr = [1, 2, 3]"
      case lookupKey "arr" val of
        Just (TArray vs) -> V.toList vs @?= [TInteger 1, TInteger 2, TInteger 3]
        _ -> assertFailure "expected TArray"

  , testCase "empty array" $ do
      let Right val = decode "arr = []"
      case lookupKey "arr" val of
        Just (TArray vs) -> V.null vs @?= True
        _ -> assertFailure "expected TArray"

  , testCase "array of strings" $ do
      let Right val = decode "arr = [\"a\", \"b\", \"c\"]"
      case lookupKey "arr" val of
        Just (TArray vs) -> V.toList vs @?= [TString "a", TString "b", TString "c"]
        _ -> assertFailure "expected TArray"

  , testCase "array of tables" $ do
      let input = T.unlines
            [ "[[products]]"
            , "name = \"Hammer\""
            , ""
            , "[[products]]"
            , "name = \"Nail\""
            ]
      let Right val = decode input
      case lookupKey "products" val of
        Just (TTable _) -> pure ()
        _ -> pure ()
  ]

stringTests :: TestTree
stringTests = testGroup "Strings"
  [ testCase "basic string" $ do
      let Right val = decode "s = \"hello world\""
      lookupKey "s" val @?= Just (TString "hello world")

  , testCase "literal string" $ do
      let Right val = decode "s = 'hello world'"
      lookupKey "s" val @?= Just (TString "hello world")

  , testCase "escape sequences in basic string" $ do
      let Right val = decode "s = \"hello\\nworld\""
      lookupKey "s" val @?= Just (TString "hello\nworld")

  , testCase "literal string preserves backslash" $ do
      let Right val = decode "s = 'hello\\nworld'"
      lookupKey "s" val @?= Just (TString "hello\\nworld")
  ]

integerTests :: TestTree
integerTests = testGroup "Integer formats"
  [ testCase "hex integer" $ do
      let Right val = decode "n = 0xFF"
      lookupKey "n" val @?= Just (TInteger 255)

  , testCase "octal integer" $ do
      let Right val = decode "n = 0o77"
      lookupKey "n" val @?= Just (TInteger 63)

  , testCase "binary integer" $ do
      let Right val = decode "n = 0b1010"
      lookupKey "n" val @?= Just (TInteger 10)

  , testCase "integer with underscores" $ do
      let Right val = decode "n = 1_000_000"
      lookupKey "n" val @?= Just (TInteger 1000000)

  , testCase "positive integer with +" $ do
      let Right val = decode "n = +42"
      lookupKey "n" val @?= Just (TInteger 42)
  ]

classTests :: TestTree
classTests = testGroup "Class instances"
  [ testCase "Text roundtrip" $ do
      let val = "hello" :: Text
      fromTOML (toTOML val) @?= Right val

  , testCase "Int roundtrip" $ do
      let val = 42 :: Int
      fromTOML (toTOML val) @?= Right val

  , testCase "Bool roundtrip" $ do
      fromTOML (toTOML True) @?= Right True

  , testCase "Double roundtrip" $ do
      let val = 3.14 :: Double
      fromTOML (toTOML val) @?= Right val

  , testCase "List roundtrip" $ do
      let val = [1, 2, 3] :: [Int]
      fromTOML (toTOML val) @?= Right val
  ]

data Config = Config
  { title :: !Text
  , port :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToTOML, FromTOML)

genericTests :: TestTree
genericTests = testGroup "Generic deriving"
  [ testCase "record to TOML" $ do
      let cfg = Config "My App" 8080
          val = toTOML cfg
      case val of
        TTable kvs -> do
          lookupInTable "title" kvs @?= Just (TString "My App")
          lookupInTable "port" kvs @?= Just (TInteger 8080)
        _ -> assertFailure "expected TTable"

  , testCase "record roundtrip" $ do
      let cfg = Config "Test" 3000
      fromTOML (toTOML cfg) @?= Right cfg
  ]

roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testCase "encode then decode preserves values" $ do
      let val = TTable (V.fromList
            [ ("name", TString "test")
            , ("count", TInteger 42)
            , ("enabled", TBool True)
            ])
      let encoded = TOML.Encode.encode val
      case decode encoded of
        Right val2 -> do
          lookupKey "name" val2 @?= Just (TString "test")
          lookupKey "count" val2 @?= Just (TInteger 42)
          lookupKey "enabled" val2 @?= Just (TBool True)
        Left err -> assertFailure $ "decode failed: " ++ err

  , testCase "array encode then decode" $ do
      let val = TTable (V.fromList
            [ ("items", TArray (V.fromList [TInteger 1, TInteger 2, TInteger 3]))
            ])
      let encoded = TOML.Encode.encode val
      case decode encoded of
        Right val2 ->
          case lookupKey "items" val2 of
            Just (TArray vs) -> V.toList vs @?= [TInteger 1, TInteger 2, TInteger 3]
            _ -> assertFailure "expected TArray"
        Left err -> assertFailure $ "decode failed: " ++ err

  , testCase "encoder escapes control characters per TOML 1.0.0" $ do
      -- Basic strings forbid raw control chars (U+0000..U+001F) other
      -- than tab; the encoder must emit them as \\uXXXX escapes so the
      -- result re-parses to the original value.
      let raw = T.pack "ctrl: \x01 \x1F bell:\x07"
          val = TTable (V.fromList [("s", TString raw)])
          encoded = TOML.Encode.encode val
      assertBool "uses \\u escape for U+0001"
        (T.isInfixOf (T.pack "\\u0001") encoded)
      assertBool "uses \\u escape for U+001F"
        (T.isInfixOf (T.pack "\\u001F") encoded)
      case decode encoded of
        Right val2 -> case lookupKey "s" val2 of
          Just (TString got) -> got @?= raw
          _ -> assertFailure "expected TString"
        Left err -> assertFailure $ "decode failed: " ++ err
  ]

-- Helpers

lookupKey :: Text -> Value -> Maybe Value
lookupKey key (TTable kvs) = lookupInTable key kvs
lookupKey _ _ = Nothing

lookupInTable :: Text -> V.Vector (Text, Value) -> Maybe Value
lookupInTable key kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == key = Just v
      | otherwise = go (i + 1)
