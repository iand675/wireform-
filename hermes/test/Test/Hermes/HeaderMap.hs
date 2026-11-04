{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.HeaderMap (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers (headerMapFromList, headerMapToList, insertRawHeader, lookupRawHeader)
import Network.HTTP.Headers.HeaderFieldName (hCacheControl)
import Test.Syd


tests :: Spec
tests = describe "HeaderMap" $ do
  let headers = headerMapFromList [("cache-control", "a"), ("cache-control", "b")]

  it "keeps insertion order" $
    lookupRawHeader hCacheControl headers `shouldBe` Just ("a" :| ["b"])
