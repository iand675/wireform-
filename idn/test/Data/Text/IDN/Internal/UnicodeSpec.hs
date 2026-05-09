{-# LANGUAGE OverloadedStrings #-}

module Data.Text.IDN.Internal.UnicodeSpec (spec) where

import Test.Hspec
import Data.Text.IDN.Internal.Unicode
import Data.Text.IDN.Types

spec :: Spec
spec = do
  describe "codePointStatus" $ do
    it "correctly identifies ASCII letters as PVALID" $ do
      codePointStatus 'a' `shouldBe` PVALID
      codePointStatus 'z' `shouldBe` PVALID
      codePointStatus 'A' `shouldBe` PVALID
      codePointStatus 'Z' `shouldBe` PVALID
    
    it "correctly identifies ASCII digits as PVALID" $ do
      codePointStatus '0' `shouldBe` PVALID
      codePointStatus '9' `shouldBe` PVALID
    
    it "correctly identifies hyphen as PVALID" $ do
      codePointStatus '-' `shouldBe` PVALID
    
    it "correctly identifies ZERO WIDTH NON-JOINER as CONTEXTJ" $ do
      codePointStatus '\x200C' `shouldBe` CONTEXTJ
    
    it "correctly identifies ZERO WIDTH JOINER as CONTEXTJ" $ do
      codePointStatus '\x200D' `shouldBe` CONTEXTJ
    
    it "correctly identifies MIDDLE DOT as CONTEXTO" $ do
      codePointStatus '\xB7' `shouldBe` CONTEXTO
    
    it "correctly identifies spaces as DISALLOWED" $ do
      codePointStatus ' ' `shouldBe` DISALLOWED
      codePointStatus '\t' `shouldBe` DISALLOWED
    
    it "correctly identifies control characters as DISALLOWED" $ do
      codePointStatus '\x00' `shouldBe` DISALLOWED
      codePointStatus '\x1F' `shouldBe` DISALLOWED
  
  describe "bidiClass" $ do
    it "correctly identifies ASCII letters as L (Left-to-Right)" $ do
      bidiClass 'a' `shouldBe` L
      bidiClass 'Z' `shouldBe` L
    
    it "correctly identifies Arabic letters as AL (Arabic Letter)" $ do
      bidiClass '\x0627' `shouldBe` AL  -- ARABIC LETTER ALEF
      bidiClass '\x0628' `shouldBe` AL  -- ARABIC LETTER BEH
    
    it "correctly identifies Hebrew letters as R (Right-to-Left)" $ do
      bidiClass '\x05D0' `shouldBe` R  -- HEBREW LETTER ALEF
      bidiClass '\x05D1' `shouldBe` R  -- HEBREW LETTER BET
    
    it "correctly identifies ASCII digits as EN (European Number)" $ do
      bidiClass '0' `shouldBe` EN
      bidiClass '9' `shouldBe` EN
    
    it "correctly identifies Arabic-Indic digits as AN (Arabic Number)" $ do
      bidiClass '\x0660' `shouldBe` AN  -- ARABIC-INDIC DIGIT ZERO
      bidiClass '\x0669' `shouldBe` AN  -- ARABIC-INDIC DIGIT NINE
    
    it "correctly identifies whitespace as WS" $ do
      bidiClass ' ' `shouldBe` WS
      bidiClass '\t' `shouldBe` S
    
    it "correctly identifies paragraph separator as B" $ do
      bidiClass '\x000A' `shouldBe` B  -- LINE FEED
      bidiClass '\x000D' `shouldBe` B  -- CARRIAGE RETURN
  
  describe "isCombiningMark" $ do
    it "returns False for regular ASCII letters" $ do
      isCombiningMark 'a' `shouldBe` False
      isCombiningMark 'Z' `shouldBe` False
    
    it "returns True for combining diacritical marks" $ do
      isCombiningMark '\x0301' `shouldBe` True  -- COMBINING ACUTE ACCENT
      isCombiningMark '\x0308' `shouldBe` True  -- COMBINING DIAERESIS
  
  describe "isVirama" $ do
    it "returns False for regular characters" $ do
      isVirama 'a' `shouldBe` False
      isVirama '\x0627' `shouldBe` False
    
    it "returns True for virama characters" $ do
      isVirama '\x094D' `shouldBe` True  -- DEVANAGARI SIGN VIRAMA
  
  describe "scriptOf" $ do
    it "identifies Latin script" $ do
      scriptOf 'a' `shouldBe` Just Latin
      scriptOf 'Z' `shouldBe` Just Latin
    
    it "identifies Greek script" $ do
      scriptOf '\x03B1' `shouldBe` Just Greek  -- GREEK SMALL LETTER ALPHA
      scriptOf '\x03A9' `shouldBe` Just Greek  -- GREEK CAPITAL LETTER OMEGA
    
    it "identifies Hebrew script" $ do
      scriptOf '\x05D0' `shouldBe` Just Hebrew  -- HEBREW LETTER ALEF
    
    it "identifies Arabic script" $ do
      scriptOf '\x0627' `shouldBe` Just Arabic  -- ARABIC LETTER ALEF
    
    it "identifies Hiragana script" $ do
      scriptOf '\x3042' `shouldSatisfy` \s -> s == Just Hiragana || s == Just OtherScript
    
    it "identifies Katakana script" $ do
      scriptOf '\x30A2' `shouldSatisfy` \s -> s == Just Katakana || s == Just OtherScript
  
  describe "contextRule" $ do
    it "identifies ZWNJ rule" $ do
      lookupContextRule '\x200C' `shouldBe` Just ZWNJRule
    
    it "identifies ZWJ rule" $ do
      lookupContextRule '\x200D' `shouldBe` Just ZWJRule
    
    it "identifies Middle Dot rule" $ do
      lookupContextRule '\xB7' `shouldBe` Just MiddleDotRule
    
    it "identifies Greek Keraia rule" $ do
      lookupContextRule '\x0375' `shouldBe` Just GreekKeraiaRule
    
    it "identifies Hebrew punctuation rules" $ do
      lookupContextRule '\x05F3' `shouldBe` Just HebrewGereshRule
      lookupContextRule '\x05F4' `shouldBe` Just HebrewGershayimRule
    
    it "identifies Katakana Middle Dot rule" $ do
      lookupContextRule '\x30FB' `shouldBe` Just KatakanaMiddleDotRule
    
    it "identifies Arabic-Indic Digits rule" $ do
      lookupContextRule '\x0660' `shouldBe` Just ArabicIndicDigitsRule
      lookupContextRule '\x0669' `shouldBe` Just ArabicIndicDigitsRule
    
    it "identifies Extended Arabic-Indic Digits rule" $ do
      lookupContextRule '\x06F0' `shouldBe` Just ExtendedArabicIndicDigitsRule
      lookupContextRule '\x06F9' `shouldBe` Just ExtendedArabicIndicDigitsRule
    
    it "returns Nothing for regular characters" $ do
      lookupContextRule 'a' `shouldBe` Nothing
      lookupContextRule '0' `shouldBe` Nothing

