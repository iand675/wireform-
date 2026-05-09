{-# LANGUAGE OverloadedStrings #-}

-- | RFC 5893 Bidi compliance test cases
module Data.Text.IDN.RFC5893Spec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDN

spec :: Spec
spec = do
  describe "RFC 5893 Section 2: Bidi Rule Validation" $ do
    describe "Examples from RFC 5893" $ do
      it "Example 1: Simple Arabic domain" $ do
        -- Arabic: السعودية (Saudi Arabia)
        let domain = "\x0627\x0644\x0633\x0639\x0648\x062F\x064A\x0629"
        validateLabel domain `shouldSatisfy` isRight
      
      it "Example 2: Hebrew domain" $ do
        -- Hebrew: ישראל (Israel)
        let domain = "\x05D9\x05E9\x05E8\x05D0\x05DC"
        validateLabel domain `shouldSatisfy` isRight
      
      it "Example 3: Arabic with European Numbers" $ do
        -- Mixed Arabic letters with ASCII digits
        let domain = "\x0645\x0635\x0631\&123"  -- Egypt + 123
        validateLabel domain `shouldSatisfy` isRight
      
      it "Example 4: Arabic with Arabic-Indic digits" $ do
        let domain = "\x0645\x0635\x0631\x0661\x0662\x0663"  -- Egypt + ١٢٣
        validateLabel domain `shouldSatisfy` isRight
    
    describe "Invalid Bidi sequences" $ do
      it "rejects Latin followed by Arabic" $ do
        let domain = "test\x0627\x0644"
        toASCII domain `shouldSatisfy` isLeft
      
      it "rejects Arabic followed by Latin" $ do
        let domain = "\x0627\x0644test"
        toASCII domain `shouldSatisfy` isLeft
      
      it "rejects RTL starting with digit" $ do
        let domain = "1\x0627\x0644\x0639"
        toASCII domain `shouldSatisfy` isLeft
      
      it "rejects RTL ending with LTR punctuation" $ do
        let domain = "\x0627\x0644\x0639."
        toASCII domain `shouldSatisfy` isLeft
  
  describe "RFC 5893 Section 3: Detailed Rules" $ do
    describe "Rule 1: RTL label detection" $ do
      it "detects Arabic letters trigger RTL rules" $ do
        let domain = "\x0627test"
        toASCII domain `shouldSatisfy` isLeft  -- Mixed script
      
      it "detects Hebrew letters trigger RTL rules" $ do
        let domain = "\x05D0test"
        toASCII domain `shouldSatisfy` isLeft  -- Mixed script
    
    describe "Rule 2: First character" $ do
      it "accepts R character (Hebrew) at start" $ do
        let domain = "\x05D0\x05D1\x05D2"
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts AL character (Arabic) at start" $ do
        let domain = "\x0627\x0644\x0639"
        validateLabel domain `shouldSatisfy` isRight
      
      it "rejects EN at start of RTL label" $ do
        let domain = "1\x0627"
        toASCII domain `shouldSatisfy` isLeft
    
    describe "Rule 3: Last character" $ do
      it "accepts R at end" $ do
        let domain = "\x05D0\x05D1\x05D0"  -- Hebrew
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts AL at end" $ do
        let domain = "\x0627\x0644\x0639"  -- Arabic
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts AN at end" $ do
        let domain = "\x0627\x0644\x0661"  -- Arabic + Arabic-Indic digit
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts EN at end" $ do
        let domain = "\x0627\&1"  -- Arabic + ASCII digit
        validateLabel domain `shouldSatisfy` isRight
    
    describe "Rule 4: NSM following allowed characters" $ do
      it "accepts NSM after R" $ do
        -- Must append valid ending char to satisfy Rule 3
        let domain = "\x05D0\x05B0\x05D0"  -- Hebrew letter + vowel point + Hebrew letter
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts NSM after AL" $ do
        let domain = "\x0627\x064B\x0627"  -- Arabic letter + fathatan + Arabic letter
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts NSM after EN" $ do
        let domain = "\x0627\&1\x0300\&1"  -- Arabic + digit + combining grave + digit
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts NSM after AN" $ do
        let domain = "\x0627\x0660\x0300\x0660"  -- Arabic + Arabic-Indic 0 + combining + digit
        validateLabel domain `shouldSatisfy` isRight
    
    describe "Rule 5: ES and CS only between numbers" $ do
      it "accepts ES (plus sign) between EN" $ do
        let domain = "\x0627\&1+2\x0628"
        validateLabel domain `shouldSatisfy` isRight
      
      it "accepts CS (comma) between EN" $ do
        let domain = "\x0627\&1,000\x0628"
        validateLabel domain `shouldSatisfy` isRight
      
      it "rejects ES between AN" $ do
        let domain = "\x0627\x0661+\x0662\x0628"
        validateLabel domain `shouldSatisfy` isLeft
    
    describe "Rule 6: ON with constraints" $ do
      it "handles ON in RTL context" $ do
        let domain = "\x0627(\x0644)\x0627"  -- Arabic + parentheses + Arabic
        validateLabel domain `shouldSatisfy` isRight
  
  describe "Real-world RTL domains" $ do
    it "validates مصر (Egypt in Arabic)" $ do
      let domain = "\x0645\x0635\x0631"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates السعودية (Saudi Arabia)" $ do
      let domain = "\x0627\x0644\x0633\x0639\x0648\x062F\x064A\x0629"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates الإمارات (UAE)" $ do
      let domain = "\x0627\x0644\x0625\x0645\x0627\x0631\x0627\x062A"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates ישראל (Israel)" $ do
      let domain = "\x05D9\x05E9\x05E8\x05D0\x05DC"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates فلسطين (Palestine)" $ do
      let domain = "\x0641\x0644\x0633\x0637\x064A\x0646"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates الأردن (Jordan)" $ do
      let domain = "\x0627\x0644\x0623\x0631\x062F\x0646"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates لبنان (Lebanon)" $ do
      let domain = "\x0644\x0628\x0646\x0627\x0646"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates سوريا (Syria)" $ do
      let domain = "\x0633\x0648\x0631\x064A\x0627"
      toASCII domain `shouldSatisfy` isRight
  
  describe "Mixed domain components" $ do
    it "accepts ASCII TLD with Unicode label" $ do
      let domain = "\x0645\x0635\x0631.com"
      toASCII domain `shouldSatisfy` isRight
    
    it "accepts Unicode TLD with ASCII label" $ do
      let domain = "test.\x0645\x0635\x0631"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates multi-label Arabic domain" $ do
      let domain = "\x0645\x0635\x0631.\x0645\x0635\x0631"
      toASCII domain `shouldSatisfy` isRight
    
    it "validates subdomain with RTL" $ do
      let domain = "sub.\x0645\x0635\x0631.com"
      toASCII domain `shouldSatisfy` isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
