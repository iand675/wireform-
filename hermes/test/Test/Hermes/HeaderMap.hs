{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.HeaderMap (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers (deleteHeader, headerMapFromList, headerMapToList, insertRawHeader, lookupHeader, lookupRawHeader, setRawHeader)
import Network.HTTP.Headers.CacheControl (CacheControl (..), CacheControlDirective (..))
import Network.HTTP.Headers.HeaderFieldName (hCacheControl)
import Test.Syd


tests :: Spec
tests = describe "HeaderMap" $ do
  it "headerMapFromList preserves order on lookup" $
    lookupRawHeader hCacheControl (headerMapFromList [("cache-control", "a"), ("cache-control", "b")]) `shouldBe` Just ("a" :| ["b"])

  it "string-built name matches the built-in constant" $
    lookupRawHeader hCacheControl (headerMapFromList [("Cache-Control", "no-cache")]) `shouldBe` Just ("no-cache" :| [])

  it "two raw header inserts preserve order on lookup" $
    lookupRawHeader hCacheControl (insertRawHeader hCacheControl "b" (insertRawHeader hCacheControl "a" (headerMapFromList []))) `shouldBe` Just ("a" :| ["b"])

  it "two raw header inserts preserve order in toList" $
    headerMapToList (insertRawHeader hCacheControl "b" (insertRawHeader hCacheControl "a" (headerMapFromList [])))
      `shouldBe` [("cache-control", "a"), ("cache-control", "b")]

  it "insert after headerMapFromList appends to the end" $
    lookupRawHeader hCacheControl (insertRawHeader hCacheControl "c" (headerMapFromList [("cache-control", "a"), ("cache-control", "b")]))
      `shouldBe` Just ("a" :| ["b", "c"])

  it "setRawHeader round-trips through lookup" $
    lookupRawHeader hCacheControl (setRawHeader hCacheControl ("a" :| ["b"]) (headerMapFromList []))
      `shouldBe` Just ("a" :| ["b"])

  it "deleteHeader removes every value for the name" $
    lookupRawHeader hCacheControl (deleteHeader hCacheControl (headerMapFromList [("cache-control", "a"), ("cache-control", "b")]))
      `shouldBe` Nothing

  -- Counts rather than compares values so this checks key folding only, not ordering.
  it "case-variant names collapse into one key" $
    fmap length (lookupRawHeader hCacheControl (headerMapFromList [("cache-control", "a"), ("CACHE-CONTROL", "b")]))
      `shouldBe` Just 2

  -- Typed lookups parse the repeated lines in order, so the directives must come back in wire order.
  it "typed lookupHeader sees repeated values from headerMapFromList in order" $
    lookupHeader @CacheControl (headerMapFromList [("cache-control", "no-store"), ("cache-control", "max-age=60")])
      `shouldBe` Right (Just (CacheControl (NoStore :| [MaxAge 60])))

  it "typed lookupHeader sees repeated raw inserts in order" $
    lookupHeader @CacheControl (insertRawHeader hCacheControl "max-age=60" (insertRawHeader hCacheControl "no-store" (headerMapFromList [])))
      `shouldBe` Right (Just (CacheControl (NoStore :| [MaxAge 60])))
