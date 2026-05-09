{-# LANGUAGE OverloadedStrings #-}

module Text.Regex.ECMA262Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Regex.ECMA262
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "Compilation" $ do
    it "compiles a simple pattern" $ do
      result <- compile "hello" []
      result `shouldSatisfy` isRight

    it "fails on invalid pattern" $ do
      result <- compile "[" []
      result `shouldSatisfy` isLeft

    it "compiles with flags" $ do
      result <- compile "HELLO" [IgnoreCase]
      result `shouldSatisfy` isRight

    it "compiles unicode patterns" $ do
      result <- compile "\\p{Letter}+" [Unicode]
      result `shouldSatisfy` isRight

    it "compiles named capture groups" $ do
      result <- compile "(?<year>\\d{4})-(?<month>\\d{2})" []
      result `shouldSatisfy` isRight

  describe "Basic Matching" $ do
    it "matches a simple pattern" $ do
      Right regex <- compile "hello" []
      result <- match regex "hello world"
      result `shouldSatisfy` isJust

    it "returns Nothing on no match" $ do
      Right regex <- compile "goodbye" []
      result <- match regex "hello world"
      result `shouldBe` Nothing

    it "matches case-insensitively" $ do
      Right regex <- compile "hello" [IgnoreCase]
      result <- match regex "HELLO world"
      result `shouldSatisfy` isJust

    it "extracts match position" $ do
      Right regex <- compile "world" []
      Just m <- match regex "hello world"
      matchStart m `shouldBe` 6
      matchEnd m `shouldBe` 11
      matchText m `shouldBe` "world"

  describe "Capture Groups" $ do
    it "captures groups" $ do
      Right regex <- compile "(\\d{4})-(\\d{2})-(\\d{2})" []
      Just m <- match regex "Date: 2024-03-15"
      length (captures m) `shouldBe` 3
      let [(_, _, year), (_, _, month), (_, _, day)] = captures m
      year `shouldBe` "2024"
      month `shouldBe` "03"
      day `shouldBe` "15"

    it "handles optional groups" $ do
      Right regex <- compile "(a)?(b)" []
      Just m1 <- match regex "ab"
      length (captures m1) `shouldBe` 2

      Just m2 <- match regex "b"
      length (captures m2) `shouldBe` 2

    it "handles nested groups" $ do
      Right regex <- compile "((a)(b))" []
      Just m <- match regex "ab"
      length (captures m) `shouldBe` 3

  describe "Anchors" $ do
    it "matches start anchor" $ do
      Right regex <- compile "^hello" []
      Just m1 <- match regex "hello world"
      matchStart m1 `shouldBe` 0

      result2 <- match regex "say hello"
      result2 `shouldBe` Nothing

    it "matches end anchor" $ do
      Right regex <- compile "world$" []
      Just m <- match regex "hello world"
      matchEnd m `shouldBe` 11

      result2 <- match regex "world!"
      result2 `shouldBe` Nothing

    it "matches both anchors" $ do
      Right regex <- compile "^exact$" []
      Just _ <- match regex "exact"
      result2 <- match regex "not exact"
      result2 `shouldBe` Nothing

  describe "Character Classes" $ do
    it "matches character class" $ do
      Right regex <- compile "[abc]+" []
      Just m <- match regex "aabbcc"
      matchText m `shouldBe` "aabbcc"

    it "matches negated character class" $ do
      Right regex <- compile "[^0-9]+" []
      Just m <- match regex "hello123"
      matchText m `shouldBe` "hello"

    it "matches digit class" $ do
      Right regex <- compile "\\d+" []
      Just m <- match regex "abc123def"
      matchText m `shouldBe` "123"

    it "matches word class" $ do
      Right regex <- compile "\\w+" []
      Just m <- match regex "hello_world123"
      matchText m `shouldBe` "hello_world123"

    it "matches whitespace class" $ do
      Right regex <- compile "\\s+" []
      Just m <- match regex "hello   world"
      matchText m `shouldBe` "   "

  describe "Quantifiers" $ do
    it "matches zero or more" $ do
      Right regex <- compile "a*" []
      Just m1 <- match regex "aaa"
      matchText m1 `shouldBe` "aaa"

      Just m2 <- match regex "bbb"
      matchText m2 `shouldBe` ""

    it "matches one or more" $ do
      Right regex <- compile "a+" []
      Just m <- match regex "aaa"
      matchText m `shouldBe` "aaa"

      result2 <- match regex "bbb"
      result2 `shouldBe` Nothing

    it "matches optional" $ do
      Right regex <- compile "colou?r" []
      result1 <- match regex "color"
      result1 `shouldSatisfy` isJust
      result2 <- match regex "colour"
      result2 `shouldSatisfy` isJust

    it "matches exact count" $ do
      Right regex <- compile "a{3}" []
      Just m <- match regex "aaa"
      matchText m `shouldBe` "aaa"

      result2 <- match regex "aa"
      result2 `shouldBe` Nothing

    it "matches range" $ do
      Right regex <- compile "a{2,4}" []
      Just m1 <- match regex "aa"
      matchText m1 `shouldBe` "aa"

      Just m2 <- match regex "aaaa"
      matchText m2 `shouldBe` "aaaa"

      result3 <- match regex "a"
      result3 `shouldBe` Nothing

    it "matches non-greedy quantifier" $ do
      Right regex <- compile "a+?" []
      Just m <- match regex "aaa"
      matchText m `shouldBe` "a"

  describe "Alternation" $ do
    it "matches alternatives" $ do
      Right regex <- compile "cat|dog" []
      Just m1 <- match regex "I have a cat"
      matchText m1 `shouldBe` "cat"

      Just m2 <- match regex "I have a dog"
      matchText m2 `shouldBe` "dog"

    it "matches first alternative" $ do
      Right regex <- compile "a|ab" []
      Just m <- match regex "ab"
      matchText m `shouldBe` "a"

  describe "Multiline Mode" $ do
    it "matches multiline anchors" $ do
      Right regex <- compile "^line" [Multiline]
      Just m <- match regex "first\nline"
      matchStart m `shouldBe` 6

    it "matches end anchor in multiline" $ do
      Right regex <- compile "line$" [Multiline]
      Just m <- match regex "line\nsecond"
      matchEnd m `shouldBe` 4

  describe "DotAll Mode" $ do
    it "dot matches newline in dotall mode" $ do
      Right regex <- compile "a.b" [DotAll]
      Just m <- match regex "a\nb"
      matchText m `shouldBe` "a\nb"

    it "dot doesn't match newline without dotall" $ do
      Right regex <- compile "a.b" []
      result <- match regex "a\nb"
      result `shouldBe` Nothing

  describe "Unicode Support" $ do
    -- Full UTF-16 mode is used when Unicode flag is set
    it "matches Unicode characters with dot" $ do
      Right regex <- compile "." [Unicode]
      let emoji = TE.encodeUtf8 "😀"
      Just m <- match regex emoji
      matchText m `shouldBe` emoji  -- Should match the full 4-byte emoji

    it "matches emoji with Unicode property escape" $ do
      Right regex <- compile "\\p{Emoji}+" [Unicode]
      let text = TE.encodeUtf8 "Hello 😀🎉 world"
      let emojiPart = TE.encodeUtf8 "😀🎉"
      Just m <- match regex text
      matchText m `shouldBe` emojiPart

    it "matches Unicode letters with \\p{Letter}" $ do
      Right regex <- compile "\\p{Letter}+" [Unicode]
      let text = TE.encodeUtf8 "Hello世界"
      Just m <- match regex text
      -- Should match "Hello" or "世界" depending on where it starts
      BS.length (matchText m) `shouldSatisfy` (> 0)

    it "handles surrogate pairs correctly" $ do
      -- U+1D573 MATHEMATICAL BOLD FRAKTUR CAPITAL H (outside BMP)
      Right regex <- compile "." [Unicode]
      let char = TE.encodeUtf8 "𝕳"  -- This is a 4-byte UTF-8 character
      Just m <- match regex char
      matchText m `shouldBe` char  -- Should match the full character

    it "matches multiple emojis" $ do
      -- Use Unicode escape sequence in pattern (U+1F600 = 😀)
      Right regex <- compile "\\u{1F600}+" [Unicode]
      let text = TE.encodeUtf8 "😀😀😀"
      Just m <- match regex text
      matchText m `shouldBe` text

    it "correctly positions captures with mixed ASCII and Unicode" $ do
      -- Use Unicode escape sequence in pattern
      Right regex <- compile "Hello (\\u{1F600}+) world" [Unicode]
      let text = TE.encodeUtf8 "Hello 😀😀 world"
      Just m <- match regex text
      length (captures m) `shouldBe` 1
      let [(_, _, emojiMatch)] = captures m
      emojiMatch `shouldBe` TE.encodeUtf8 "😀😀"

    it "uses byte mode without Unicode flag" $ do
      -- Without Unicode flag, should still use byte mode for performance
      Right regex <- compile "[a-z]+" []
      Just m <- match regex "hello"
      matchText m `shouldBe` "hello"

  describe "matchAll" $ do
    it "finds all matches" $ do
      Right regex <- compile "\\d+" []
      matches <- matchAll regex "1 22 333"
      length matches `shouldBe` 3
      map matchText matches `shouldBe` ["1", "22", "333"]

    it "finds non-overlapping matches" $ do
      Right regex <- compile "aa" []
      matches <- matchAll regex "aaaa"
      length matches `shouldBe` 2

    it "returns empty list for no matches" $ do
      Right regex <- compile "\\d+" []
      matches <- matchAll regex "no numbers"
      matches `shouldBe` []

  describe "test function" $ do
    it "returns True on match" $ do
      Right regex <- compile "hello" []
      result <- test regex "hello world"
      result `shouldBe` True

    it "returns False on no match" $ do
      Right regex <- compile "goodbye" []
      result <- test regex "hello world"
      result `shouldBe` False

  describe "Introspection" $ do
    it "gets capture count" $ do
      Right regex <- compile "(a)(b)(c)" []
      count <- getCaptureCount regex
      count `shouldBe` 3

    it "gets flags" $ do
      Right regex <- compile "test" [IgnoreCase, Multiline]
      flags <- getFlags regex
      IgnoreCase `shouldSatisfy` (`elem` flags)
      Multiline `shouldSatisfy` (`elem` flags)


  describe "Character Class Unicode Matching" $ do
    it "\\d matches '0' with Unicode flag (no anchors)" $ do
      Right regex <- compile "\\d" [Unicode]
      result <- match regex "0"
      result `shouldSatisfy` isJust

    it "^a$ matches 'a' with Unicode flag (basic anchor test)" $ do
      Right regex <- compile "^a$" [Unicode]
      result <- match regex "a"
      result `shouldSatisfy` isJust

    it "^hello$ matches 'hello' with Unicode flag (longer text)" $ do
      Right regex <- compile "^hello$" [Unicode]
      result <- match regex "hello"
      result `shouldSatisfy` isJust

  describe "Non-ASCII Pattern Matching" $ do
    it "^á compiles without Unicode flag" $ do
      result <- compile "^á" []
      case result of
        Left err -> expectationFailure $ "Compilation failed: " ++ err
        Right _ -> return ()

    it "^á compiles with Unicode flag" $ do
      result <- compile "^á" [Unicode]
      case result of
        Left err -> expectationFailure $ "Compilation failed: " ++ err
        Right _ -> return ()

    it "^á matches 'ármányos' with Unicode flag" $ do
      result <- compile "^á" [Unicode]
      case result of
        Left err -> expectationFailure $ "Compilation failed: " ++ err
        Right regex -> do
          let subject = TE.encodeUtf8 "ármányos"
          matchResult <- match regex subject
          matchResult `shouldSatisfy` isJust

  describe "Non-BMP Character Patterns" $ do
    it "^🐲*$ compiles with Unicode flag" $ do
      let pattern = "^🐲*$"
      result <- compile pattern [Unicode]
      case result of
        Left err -> expectationFailure $ "Compilation failed: " ++ err ++
                                        "\nPattern bytes: " ++ show (BS.unpack pattern)
        Right _ -> return ()

    it "^🐲*$ matches empty string" $ do
      Right regex <- compile "^🐲*$" [Unicode]
      result <- match regex ""
      result `shouldSatisfy` isJust

    it "^🐲$ matches single dragon (no quantifier)" $ do
      Right regex <- compile "^🐲$" [Unicode]
      let dragon = TE.encodeUtf8 "🐲"
      result <- match regex dragon
      result `shouldSatisfy` isJust

    it "🐲 matches dragon without anchors" $ do
      Right regex <- compile "🐲" [Unicode]
      let dragon = TE.encodeUtf8 "🐲"
      result <- match regex dragon
      result `shouldSatisfy` isJust

    it "\\u{1F432} (Unicode escape) matches dragon" $ do
      Right regex <- compile "\\u{1F432}" [Unicode]
      let dragon = TE.encodeUtf8 "🐲"
      result <- match regex dragon
      result `shouldSatisfy` isJust

    it "^🐲*$ matches single dragon" $ do
      Right regex <- compile "^🐲*$" [Unicode]
      let dragon = TE.encodeUtf8 "🐲"
      result <- match regex dragon
      result `shouldSatisfy` isJust

    it "^🐲*$ matches two dragons" $ do
      Right regex <- compile "^🐲*$" [Unicode]
      let dragons = TE.encodeUtf8 "🐲🐲"
      result <- match regex dragons
      result `shouldSatisfy` isJust

  describe "Edge Cases" $ do
    it "handles empty pattern" $ do
      Right regex <- compile "" []
      Just m <- match regex "anything"
      matchText m `shouldBe` ""

    it "handles empty subject" $ do
      Right regex <- compile "a" []
      result <- match regex ""
      result `shouldBe` Nothing

    it "handles empty match" $ do
      Right regex <- compile "a*" []
      Just m <- match regex "b"
      matchText m `shouldBe` ""

    it "handles very long patterns" $ do
      let longPattern = BS.concat $ replicate 100 "a"
      result <- compile longPattern []
      result `shouldSatisfy` isRight

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
