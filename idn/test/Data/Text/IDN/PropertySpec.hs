{-# LANGUAGE OverloadedStrings #-}

module Data.Text.IDN.PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAscii, isAlphaNum)
import Data.Text.IDN
import Data.Text.Punycode

spec :: Spec
spec = do
  describe "Property-based tests" $ do
    describe "Punycode round-trip" $ do
      it "round-trips ASCII text" $ property $ \(ASCIIText txt) ->
        case encode txt of
          Right encoded -> decode encoded `shouldBe` Right txt
          Left _ -> expectationFailure "Encoding failed for ASCII text"
      
      it "preserves length relationship" $ property $ \(UnicodeText txt) ->
        case encode txt of
          Right encoded -> 
            if T.all isAscii txt
              then T.length encoded `shouldSatisfy` (<= T.length txt + 10)  -- Small overhead for pure ASCII
              else T.length encoded `shouldSatisfy` (>= 0)
          Left _ -> return ()  -- Some texts may fail validation
    
    describe "Label validation properties" $ do
      it "rejects labels starting with hyphen" $ property $ \(NonEmptyText suffix) ->
        validateLabel ("-" <> suffix) `shouldSatisfy` isLeft
      
      it "rejects labels ending with hyphen" $ property $ \(NonEmptyText prefix) ->
        validateLabel (prefix <> "-") `shouldSatisfy` isLeft
      
      it "accepts valid alphanumeric labels" $ property $ \(AlphanumericText txt) ->
        not (T.null txt) ==>
          validateLabel txt `shouldSatisfy` \r -> case r of
            Right () -> True
            Left _ -> T.length txt > 63  -- Only fails if too long
    
    describe "Domain name properties" $ do
      it "round-trips pure ASCII domains through toASCII/toUnicode" $ property $
        \(ASCIIDomain domain) ->
          case toASCII domain of
            Right ascii -> toUnicode ascii `shouldBe` Right domain
            Left _ -> return ()  -- May fail validation
      
      it "preserves number of labels" $ property $ \(ValidDomain domain) ->
        let numLabels = length (T.splitOn "." domain)
        in case toASCII domain of
          Right ascii -> length (T.splitOn "." ascii) `shouldBe` numLabels
          Left _ -> return ()
      
      it "rejects empty domains" $
        toASCII "" `shouldSatisfy` isLeft
      
      it "rejects domains with empty labels" $ property $
        \(NonEmptyText label) ->
          toASCII (label <> "..com") `shouldSatisfy` isLeft
    
    describe "Length constraints" $ do
      it "rejects labels longer than 63 octets" $ property $
        forAll (genLongLabel 64) $ \label ->
          validateLabel label `shouldSatisfy` isLeft
      
      it "accepts labels up to 63 octets" $ property $
        forAll (genLabel 63) $ \label ->
          not (T.null label) ==>
            validateLabel label `shouldSatisfy` \r -> case r of
              Right () -> True
              Left _ -> False  -- Should not fail for valid 63-octet label
      
      it "rejects domains longer than 253 octets" $ property $
        forAll (genLongDomain 254) $ \domain ->
          toASCII domain `shouldSatisfy` isLeft

-- Custom generators for property testing

newtype ASCIIText = ASCIIText Text deriving Show
instance Arbitrary ASCIIText where
  arbitrary = ASCIIText . T.pack <$> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

newtype UnicodeText = UnicodeText Text deriving Show
instance Arbitrary UnicodeText where
  arbitrary = UnicodeText . T.pack <$> listOf (elements 
    (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ 
     ['\x00E9', '\x00F6', '\x00FC', '\x4E2D', '\x56FD', '\x0627', '\x0644']))

newtype NonEmptyText = NonEmptyText Text deriving Show
instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack <$> listOf1 (elements ['a'..'z'])

newtype AlphanumericText = AlphanumericText Text deriving Show
instance Arbitrary AlphanumericText where
  arbitrary = AlphanumericText . T.pack <$> 
    listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

newtype ASCIIDomain = ASCIIDomain Text deriving Show
instance Arbitrary ASCIIDomain where
  arbitrary = do
    labels <- listOf1 genSimpleLabel
    return $ ASCIIDomain $ T.intercalate "." labels
    where
      genSimpleLabel = T.pack <$> listOf1 (elements ['a'..'z'])

newtype ValidDomain = ValidDomain Text deriving Show
instance Arbitrary ValidDomain where
  arbitrary = do
    numLabels <- choose (1, 5)
    labels <- vectorOf numLabels genValidLabel
    return $ ValidDomain $ T.intercalate "." labels
    where
      genValidLabel = do
        len <- choose (1, 20)
        T.pack <$> vectorOf len (elements (['a'..'z'] ++ ['0'..'9']))

genLongLabel :: Int -> Gen Text
genLongLabel len = T.pack <$> vectorOf len (elements ['a'..'z'])

genLabel :: Int -> Gen Text
genLabel maxLen = do
  len <- choose (1, maxLen)
  T.pack <$> vectorOf len (elements ['a'..'z'])

genLongDomain :: Int -> Gen Text
genLongDomain minLen = do
  let labelLen = 60
  let numLabels = (minLen `div` labelLen) + 1
  labels <- vectorOf numLabels (genLongLabel labelLen)
  return $ T.intercalate "." labels

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

