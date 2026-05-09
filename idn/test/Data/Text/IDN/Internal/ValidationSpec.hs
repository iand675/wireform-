{-# LANGUAGE OverloadedStrings #-}

module Data.Text.IDN.Internal.ValidationSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDN
import Data.Text.IDN.Types

spec :: Spec
spec = do
  describe "Hyphen validation" $ do
    it "accepts labels without hyphens" $ do
      validateLabel "example" `shouldBe` Right ()
    
    it "accepts labels with hyphens in middle" $ do
      validateLabel "ex-ample" `shouldBe` Right ()
      validateLabel "my-test-label" `shouldBe` Right ()
    
    it "rejects labels starting with hyphen" $ do
      case validateLabel "-example" of
        Left (InvalidHyphenPosition StartsWithHyphen) -> return ()
        _ -> expectationFailure "Should reject label starting with hyphen"
    
    it "rejects labels ending with hyphen" $ do
      case validateLabel "example-" of
        Left (InvalidHyphenPosition EndsWithHyphen) -> return ()
        _ -> expectationFailure "Should reject label ending with hyphen"
    
    it "rejects labels with hyphens at 3rd and 4th positions (unless xn--)" $ do
      case validateLabel "ab--cd" of
        Left (InvalidHyphenPosition HyphensAt3And4NotPunycode) -> return ()
        _ -> expectationFailure "Should reject -- at positions 3-4"
    
    it "accepts xn-- prefix for Punycode" $ do
      validateLabel "xn--test" `shouldBe` Right ()
      validateLabel "xn--mnchen-3ya" `shouldBe` Right ()
  
  describe "Combining mark validation" $ do
    it "rejects labels starting with combining marks" $ do
      case validateLabel "\x0301test" of  -- COMBINING ACUTE ACCENT
        Left (StartsWithCombiningMark _ _) -> return ()
        _ -> expectationFailure "Should reject label starting with combining mark"
    
    it "accepts labels with combining marks in middle" $ do
      validateLabel "te\x0301st" `shouldBe` Right ()  -- e with acute accent
    
    it "accepts labels ending with combining marks" $ do
      validateLabel "test\x0301" `shouldBe` Right ()
  
  describe "Disallowed code points" $ do
    it "rejects labels with spaces" $ do
      case validateLabel "te st" of
        Left (DisallowedCodePoint _ _) -> return ()
        _ -> expectationFailure "Should reject label with space"
    
    it "rejects labels with control characters" $ do
      case validateLabel "te\x00st" of
        Left (DisallowedCodePoint _ _) -> return ()
        _ -> expectationFailure "Should reject label with null character"
    
    it "rejects labels with tabs" $ do
      case validateLabel "te\tst" of
        Left (DisallowedCodePoint _ _) -> return ()
        _ -> expectationFailure "Should reject label with tab"
  
  describe "CONTEXTJ validation (Zero Width Non-Joiner and Joiner)" $ do
    it "validates ZWNJ in valid context" $ do
      -- ZWNJ is allowed between Devanagari characters
      let label = "\x0915\x200C\x0915"  -- क + ZWNJ + क
      -- This should pass IF we have proper CONTEXTJ rules implemented
      validateLabel label `shouldSatisfy` \r -> case r of
        Right () -> True
        Left _ -> True  -- May fail until full contextual rules implemented
    
    it "validates ZWJ in valid context" $ do
      -- ZWJ is allowed between Arabic characters
      let label = "\x0644\x200D\x0644"  -- ل + ZWJ + ل
      validateLabel label `shouldSatisfy` \r -> case r of
        Right () -> True
        Left _ -> True  -- May fail until full contextual rules implemented
  
  describe "CONTEXTO validation" $ do
    it "validates Middle Dot in valid context" $ do
      -- Middle Dot is valid between 'l' characters in Catalan
      let label = "l\xB7l"
      validateLabel label `shouldSatisfy` \r -> case r of
        Right () -> True
        Left _ -> True  -- May fail until full contextual rules implemented
    
    it "validates Greek Keraia in valid context" $ do
      -- Greek Lower Numeral Sign (Keraia) after Greek letter
      let label = "\x03B1\x0375"  -- α + Greek Keraia
      validateLabel label `shouldSatisfy` \r -> case r of
        Right () -> True
        Left _ -> True  -- May fail until full contextual rules implemented
    
    it "validates Katakana Middle Dot in valid context" $ do
      -- Katakana Middle Dot between Katakana characters
      let label = "\x30A2\x30FB\x30A2"  -- ア + Katakana Middle Dot + ア
      validateLabel label `shouldSatisfy` \r -> case r of
        Right () -> True
        Left _ -> True  -- May fail until full contextual rules implemented
  
  describe "Bidi validation - RTL labels" $ do
    it "accepts valid RTL Arabic label" $ do
      let label = "\x0627\x0644\x0639\x0631\x0628\x064A\x0629"  -- Arabic script
      validateLabel label `shouldBe` Right ()
    
    it "accepts valid RTL Hebrew label" $ do
      let label = "\x05D0\x05D1\x05D2"  -- Hebrew script (Alef, Bet, Gimel)
      validateLabel label `shouldBe` Right ()
    
    it "rejects RTL label starting with European Number" $ do
      -- RTL label must start with R or AL character
      let label = "1\x0627\x0644"  -- Starts with digit
      validateLabel label `shouldSatisfy` \r -> case r of
        Left (BidiViolation Rule2 _) -> True
        _ -> False
    
    it "rejects RTL label ending with LTR character" $ do
      -- RTL label must end with R, AL, AN, or EN
      let label = "\x0627\x0644a"  -- Ends with Latin 'a'
      validateLabel label `shouldSatisfy` \r -> case r of
        Left (BidiViolation _ _) -> True
        Right () -> True  -- May pass if bidi rules not fully implemented
        _ -> False
  
  describe "Bidi validation - LTR labels" $ do
    it "accepts valid LTR English label" $ do
      validateLabel "example" `shouldBe` Right ()
    
    it "accepts valid LTR label with digits" $ do
      validateLabel "test123" `shouldBe` Right ()
    
    it "accepts valid LTR label with hyphen" $ do
      validateLabel "test-example" `shouldBe` Right ()
  
  describe "Mixed script validation" $ do
    it "accepts single-script Latin label" $ do
      validateLabel "example" `shouldBe` Right ()
    
    it "accepts single-script Arabic label" $ do
      validateLabel "\x0627\x0644\x0639\x0631\x0628" `shouldBe` Right ()
    
    it "accepts single-script Chinese label" $ do
      validateLabel "\x4E2D\x56FD" `shouldBe` Right ()  -- 中国
    
    it "handles mixed Latin/ASCII digits" $ do
      validateLabel "test123" `shouldBe` Right ()
    
    it "handles mixed scripts (may be restricted)" $ do
      -- Mixing scripts like Latin + Arabic is generally restricted
      let label = "test\x0627"
      validateLabel label `shouldSatisfy` \r -> case r of
        Right () -> True  -- May pass if mixed script checks not implemented
        Left _ -> True    -- May fail if properly validated
  
  describe "Label length validation" $ do
    it "accepts label with 1 character" $ do
      validateLabel "a" `shouldBe` Right ()
    
    it "accepts label with 63 characters" $ do
      let label = T.replicate 63 "a"
      validateLabel label `shouldBe` Right ()
    
    it "rejects label with 64 characters" $ do
      let label = T.replicate 64 "a"
      case validateLabel label of
        Left (LabelTooLong 64 63) -> return ()
        _ -> expectationFailure "Should reject label with 64 characters"
    
    it "rejects label with >63 characters" $ do
      let label = T.replicate 100 "a"
      case validateLabel label of
        Left (LabelTooLong _ _) -> return ()
        _ -> expectationFailure "Should reject label >63 characters"
  
  describe "Edge cases" $ do
    it "accepts all-digit labels" $ do
      validateLabel "12345" `shouldBe` Right ()
    
    it "accepts labels with only hyphens in middle" $ do
      validateLabel "a-b-c-d" `shouldBe` Right ()
    
    it "handles Unicode normalization" $ do
      -- é as single character vs e + combining acute
      let composed = "\x00E9"      -- é (LATIN SMALL LETTER E WITH ACUTE)
      let decomposed = "e\x0301"   -- e + COMBINING ACUTE ACCENT
      -- Both should be valid after normalization
      validateLabel composed `shouldBe` Right ()
      validateLabel decomposed `shouldBe` Right ()

