{-# LANGUAGE OverloadedStrings #-}

module Data.Text.IDNSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDN

spec :: Spec
spec = do
  describe "toASCII" $ do
    it "converts simple ASCII domains unchanged" $ do
      toASCII "example.com" `shouldBe` Right "example.com"
    
    it "converts German Unicode domain to Punycode" $ do
      case toASCII "münchen.de" of
        Right result -> result `shouldSatisfy` T.isPrefixOf "xn--"
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "converts Chinese domain to Punycode" $ do
      case toASCII "中国.com" of
        Right result -> T.isInfixOf "xn--" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "converts Arabic domain to Punycode" $ do
      case toASCII "مصر.com" of
        Right result -> T.isInfixOf "xn--" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "converts Russian domain to Punycode" $ do
      case toASCII "испытание.рф" of
        Right result -> T.isInfixOf "xn--" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "converts Japanese domain to Punycode" $ do
      case toASCII "日本.jp" of
        Right result -> T.isInfixOf "xn--" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "handles empty input" $ do
      toASCII "" `shouldSatisfy` isLeft
    
    it "rejects labels that are too long (>63 octets)" $ do
      let longLabel = T.replicate 64 "a"
      toASCII longLabel `shouldSatisfy` isLeft
    
    it "rejects domains longer than 253 octets" $ do
      let longDomain = T.intercalate "." (replicate 50 "label")
      toASCII longDomain `shouldSatisfy` isLeft
    
    it "rejects invalid hyphen positions" $ do
      toASCII "-example.com" `shouldSatisfy` isLeft
      toASCII "example-.com" `shouldSatisfy` isLeft
      toASCII "ab--cd.com" `shouldSatisfy` isLeft
    
    it "rejects xn-- prefix with pure ASCII (RFC 5891 violation)" $ do
      -- RFC 5891: Punycode should only be used for labels with non-ASCII characters
      toASCII "xn--test.com" `shouldSatisfy` isLeft
    
    it "handles multi-label domains" $ do
      toASCII "sub.example.com" `shouldBe` Right "sub.example.com"
    
    it "handles mixed ASCII and Unicode labels" $ do
      case toASCII "sub.münchen.de" of
        Right result -> do
          T.isInfixOf "sub" result `shouldBe` True
          T.isInfixOf "xn--" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
  
  describe "toUnicode" $ do
    it "converts simple ASCII domains unchanged" $ do
      toUnicode "example.com" `shouldBe` Right "example.com"
    
    it "decodes German Punycode A-label" $ do
      case toUnicode "xn--mnchen-3ya.de" of
        Right result -> T.isInfixOf "ü" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "handles empty input" $ do
      toUnicode "" `shouldSatisfy` isLeft
    
    it "handles multi-label domains" $ do
      toUnicode "sub.example.com" `shouldBe` Right "sub.example.com"
    
    it "decodes mixed Punycode and ASCII" $ do
      case toUnicode "sub.xn--mnchen-3ya.de" of
        Right result -> do
          T.isInfixOf "sub" result `shouldBe` True
          T.isInfixOf "ü" result `shouldBe` True
        Left err -> expectationFailure $ "Failed: " ++ show err
    
    it "leaves non-Punycode labels unchanged" $ do
      toUnicode "example.com" `shouldBe` Right "example.com"
  
  describe "validateLabel" $ do
    it "accepts valid ASCII labels" $ do
      validateLabel "example" `shouldBe` Right ()
      validateLabel "ex-ample" `shouldBe` Right ()
      validateLabel "test123" `shouldBe` Right ()
    
    it "rejects empty labels" $ do
      validateLabel "" `shouldSatisfy` isLeft
    
    it "rejects labels starting with hyphen" $ do
      validateLabel "-example" `shouldSatisfy` isLeft
    
    it "rejects labels ending with hyphen" $ do
      validateLabel "example-" `shouldSatisfy` isLeft
    
    it "rejects labels with hyphens at positions 3-4 (unless xn--)" $ do
      validateLabel "ab--cd" `shouldSatisfy` isLeft
      validateLabel "xn--test" `shouldBe` Right ()
      validateLabel "xn--mnchen-3ya" `shouldBe` Right ()
    
    it "rejects labels longer than 63 octets" $ do
      let longLabel = T.replicate 64 "a"
      validateLabel longLabel `shouldSatisfy` isLeft
    
    it "accepts valid German Unicode label" $ do
      validateLabel "münchen" `shouldBe` Right ()
    
    it "accepts valid Japanese Unicode label" $ do
      validateLabel "日本" `shouldBe` Right ()
    
    it "accepts valid Chinese Unicode label" $ do
      validateLabel "中国" `shouldBe` Right ()
    
    it "accepts valid Arabic Unicode label" $ do
      validateLabel "مصر" `shouldBe` Right ()
    
    it "accepts valid Russian Unicode label" $ do
      validateLabel "испытание" `shouldBe` Right ()
    
    it "accepts valid Greek Unicode label" $ do
      validateLabel "ελλάδα" `shouldBe` Right ()
    
    it "accepts valid Korean Unicode label" $ do
      validateLabel "한국" `shouldBe` Right ()
    
    it "accepts valid Thai Unicode label" $ do
      validateLabel "ไทย" `shouldBe` Right ()
    
    it "accepts valid Hindi Unicode label" $ do
      validateLabel "भारत" `shouldBe` Right ()
    
    it "rejects labels starting with combining marks" $ do
      validateLabel "\x0301test" `shouldSatisfy` isLeft  -- COMBINING ACUTE ACCENT + test

  describe "round-trip ASCII ↔ Unicode" $ do
    it "round-trips German domain" $ do
      let original = "münchen.de"
      case toASCII original of
        Right ascii -> toUnicode ascii `shouldSatisfy` \r -> case r of
          Right unicode -> T.toLower unicode == T.toLower original
          Left _ -> False
        Left err -> expectationFailure $ "toASCII failed: " ++ show err
    
    it "round-trips Chinese domain" $ do
      let original = "中国.com"
      case toASCII original of
        Right ascii -> toUnicode ascii `shouldSatisfy` \r -> case r of
          Right unicode -> unicode == original
          Left _ -> False
        Left err -> expectationFailure $ "toASCII failed: " ++ show err
    
    it "round-trips pure ASCII domain" $ do
      let original = "example.com"
      case toASCII original of
        Right ascii -> toUnicode ascii `shouldBe` Right original
        Left err -> expectationFailure $ "toASCII failed: " ++ show err

  describe "Real-world domains" $ do
    it "handles Google Japan" $ do
      toASCII "グーグル.jp" `shouldSatisfy` isRight
    
    it "handles German government domains" $ do
      toASCII "münchen.de" `shouldSatisfy` isRight
      toASCII "düsseldorf.de" `shouldSatisfy` isRight
    
    it "handles Russian domains" $ do
      toASCII "яндекс.рф" `shouldSatisfy` isRight
    
    it "handles Arabic domains" $ do
      toASCII "مصر.مصر" `shouldSatisfy` isRight
    
    it "handles Chinese domains" $ do
      toASCII "中国.中国" `shouldSatisfy` isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
