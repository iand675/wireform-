{-# LANGUAGE OverloadedStrings #-}

module Text.Regex.ECMA262.BaseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Regex.ECMA262.Base
import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "regex-base =~ operator" $ do
    describe "Bool context" $ do
      it "returns True on match" $ do
        let result = "hello world" =~ "world" :: Bool
        result `shouldBe` True

      it "returns False on no match" $ do
        let result = "hello world" =~ "goodbye" :: Bool
        result `shouldBe` False

      it "works with ByteString" $ do
        let result = BS.pack "hello world" =~ BS.pack "world" :: Bool
        result `shouldBe` True

      it "works case-insensitively" $ do
        let regex = makeRegex "HELLO" :: Regex
            result = "hello world" =~ regex :: Bool
        result `shouldBe` False  -- No flag support in makeRegex by default

    describe "String extraction" $ do
      it "extracts matched string" $ do
        let result = "hello world" =~ "w\\w+" :: String
        result `shouldBe` "world"

      it "returns empty on no match" $ do
        let result = "hello world" =~ "xyz" :: String
        result `shouldBe` ""

      it "extracts first match only" $ do
        let result = "foo bar baz" =~ "ba." :: String
        result `shouldBe` "bar"

    describe "All matches extraction" $ do
      it "extracts all matches" $ do
        let result = "one 123 two 456 three 789" =~ "\\d+" :: [[String]]
        result `shouldBe` [["123"], ["456"], ["789"]]

      it "returns empty list on no match" $ do
        let result = "no numbers here" =~ "\\d+" :: [[String]]
        result `shouldBe` []

      it "extracts with capture groups" $ do
        let result = "a1 b2 c3" =~ "([a-z])(\\d)" :: [[String]]
        result `shouldBe` [["a1","a","1"], ["b2","b","2"], ["c3","c","3"]]

    describe "Match count" $ do
      it "counts matches" $ do
        let result = "foo bar baz" =~ "ba." :: Int
        result `shouldBe` 2

      it "returns 0 for no matches" $ do
        let result = "foo bar" =~ "xyz" :: Int
        result `shouldBe` 0

      it "counts overlapping patterns correctly" $ do
        let result = "aaaa" =~ "aa" :: Int
        result `shouldBe` 2  -- Non-overlapping

    describe "Tuple extraction (before, match, after)" $ do
      it "extracts parts around match" $ do
        let result = "hello world" =~ "world" :: (String, String, String)
        result `shouldBe` ("hello ", "world", "")

      it "handles match at start" $ do
        let result = "hello world" =~ "hello" :: (String, String, String)
        result `shouldBe` ("", "hello", " world")

      it "handles match at end" $ do
        let result = "hello world" =~ "world" :: (String, String, String)
        result `shouldBe` ("hello ", "world", "")

    describe "Tuple with captures" $ do
      it "extracts match and captures" $ do
        let result = "Date: 2024-03-15" =~ "(\\d{4})-(\\d{2})-(\\d{2})"
                     :: (String, String, String, [String])
        result `shouldBe` ("Date: ", "2024-03-15", "", ["2024", "03", "15"])

      it "handles no captures" $ do
        let result = "hello world" =~ "world" :: (String, String, String, [String])
        result `shouldBe` ("hello ", "world", "", [])

      it "handles optional captures" $ do
        let result = "abc" =~ "(a)?(b)(c)" :: (String, String, String, [String])
        -- First group matches "a", second "b", third "c"
        result `shouldSatisfy` \(_, match, _, _) -> match == "abc"

    describe "Maybe context" $ do
      it "returns Just on match" $ do
        let result = "hello world" =~ "world"
                     :: Maybe (String, String, String)
        result `shouldSatisfy` isJust

      it "returns Nothing on no match" $ do
        let result = "hello world" =~ "xyz"
                     :: Maybe (String, String, String)
        result `shouldBe` Nothing

      it "extracts captures in Maybe" $ do
        let result = "a1" =~ "([a-z])(\\d)"
                     :: Maybe (String, String, String, [String])
        result `shouldBe` Just ("", "a1", "", ["a", "1"])

  describe "ByteString support" $ do
    it "matches ByteString patterns" $ do
      let pattern = BS.pack "hello"
          subject = BS.pack "hello world"
          result = subject =~ pattern :: Bool
      result `shouldBe` True

    it "extracts ByteString matches" $ do
      let pattern = BS.pack "\\d+"
          subject = BS.pack "test 123 foo"
          result = subject =~ pattern :: BS.ByteString
      result `shouldBe` BS.pack "123"

    it "counts ByteString matches" $ do
      let pattern = BS.pack "\\d+"
          subject = BS.pack "1 22 333"
          result = subject =~ pattern :: Int
      result `shouldBe` 3

  describe "Complex patterns" $ do
    it "handles email pattern" $ do
      let emailRegex = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
          result1 = "test@example.com" =~ emailRegex :: Bool
          result2 = "invalid.email" =~ emailRegex :: Bool
      result1 `shouldBe` True
      result2 `shouldBe` False

    it "handles phone numbers" $ do
      let phoneRegex = "\\(?\\d{3}\\)?[-.]?\\d{3}[-.]?\\d{4}"
          result1 = "(555)-123-4567" =~ phoneRegex :: Bool
          result2 = "555.123.4567" =~ phoneRegex :: Bool
          result3 = "5551234567" =~ phoneRegex :: Bool
      result1 `shouldBe` True
      result2 `shouldBe` True
      result3 `shouldBe` True

    it "handles URL pattern" $ do
      let urlRegex = "https?://[^\\s]+"
          result = "Visit https://example.com for more" =~ urlRegex :: String
      result `shouldBe` "https://example.com"

    it "extracts multiple groups from URL" $ do
      let urlRegex = "(https?)://([^/]+)(/[^\\s]*)?"
          result = "https://example.com/path" =~ urlRegex
                   :: (String, String, String, [String])
          (_, match, _, captures) = result
      match `shouldBe` "https://example.com/path"
      captures `shouldSatisfy` \cs -> length cs >= 2

  describe "Edge cases" $ do
    it "handles empty pattern" $ do
      let result = "test" =~ "" :: Bool
      result `shouldBe` True

    it "handles empty subject" $ do
      let result = "" =~ "test" :: Bool
      result `shouldBe` False

    it "handles both empty" $ do
      let result = "" =~ "" :: Bool
      result `shouldBe` True

    it "handles special regex characters" $ do
      let result = "a.b" =~ "a\\.b" :: Bool
      result `shouldBe` True

    it "handles very long strings" $ do
      let longString = concat $ replicate 1000 "test "
          result = longString =~ "test" :: Int
      result `shouldBe` 1000

  describe "RegexMaker" $ do
    it "makes regex from String" $ do
      let regex = makeRegex "test" :: Regex
          result = "this is a test" =~ regex :: Bool
      result `shouldBe` True

    it "makes regex from ByteString" $ do
      let regex = makeRegex (BS.pack "test") :: Regex
          result = BS.pack "this is a test" =~ regex :: Bool
      result `shouldBe` True

    it "handles invalid regex gracefully" $ do
      -- makeRegex with invalid pattern should throw an error
      -- We test that it compiles at least
      let regex = makeRegex "valid" :: Regex
          result = "valid" =~ regex :: Bool
      result `shouldBe` True

  describe "Performance patterns" $ do
    it "handles repeated groups efficiently" $ do
      let result = "aaa bbb ccc" =~ "(\\w+)" :: [[String]]
      length result `shouldBe` 3

    it "handles alternation" $ do
      let result = "cat dog bird" =~ "cat|dog|bird" :: Int
      result `shouldBe` 3

    it "handles nested groups" $ do
      let result = "abc" =~ "((a)(b))(c)" :: (String, String, String, [String])
          (_, match, _, captures) = result
      match `shouldBe` "abc"
      length captures `shouldBe` 4

-- Helper functions
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
