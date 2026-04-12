{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import GHC.Stats (getRTSStats, allocated_bytes)
import HTML.Rewriter
import HTML.Selector
import System.Mem (performGC)


main :: IO ()
main = do
  let header = "<html><body><div class=\"catalog\">\n"
      footer = "</div></body></html>\n"
      mkItem i = BS8.pack $ concat
        [ "  <div class=\"item\" id=\"i", show i, "\">\n"
        , "    <span class=\"name\">Product ", show i, "</span>\n"
        , "    <span class=\"price\">", show (fromIntegral i * 9.99 :: Double), "</span>\n"
        , "    <p class=\"description\">This is the description for product number "
        , show i, " in our catalog</p>\n"
        , "    <span class=\"category\">Category ", show (i `mod` 10), "</span>\n"
        , "    <span class=\"inStock\">", if even i then "true" else "false", "</span>\n"
        , "  </div>\n"
        ]
      mediumHTML = BS.concat $ [header] <> fmap mkItem [1..100 :: Int] <> [footer]

  let mp s = case parseSelector s of Right r -> r; Left e -> error (show e)
      selDiv = mp "div.item"
      selSpan = mp "span.name"
      selPrice = mp "span.price"
      selDesc = mp "p.description"
      selCat = mp "span.category"

  let Right rwSelector = buildRewriter $ do
        onElement selDiv $ \er -> do
          _ <- getTagName er
          pure ()
        onElement selSpan $ \er -> do
          _ <- getTagName er
          pure ()
        onElement selPrice $ \er -> do
          _ <- getTagName er
          pure ()
        onText selDesc $ \tr -> do
          _ <- getTextContent tr
          pure ()
        onElement selCat $ \er -> do
          _ <- getElemAttr er "class"
          pure ()

  let Right rwMutate = buildRewriter $ do
        onElement selDiv $ \er -> do
          setElemAttr er "data-processed" "true"
        onElement selSpan $ \er -> do
          setTagName er "strong"
        onText selDesc $ \tr -> do
          content <- getTextContent tr
          replaceTextChunk tr (T.toUpper content) AsText

  putStrLn $ "Input: " <> show (BS.length mediumHTML) <> " bytes"

  let iters = 3000 :: Int
      go _ 0 = pure ()
      go rw n = do
        !r <- rewrite rw mediumHTML
        _ <- evaluate $! BS.length r
        go rw (n - 1)

  go rwSelector 500
  performGC
  s0 <- getRTSStats
  go rwSelector iters
  performGC
  s1 <- getRTSStats
  let alloc1 = fromIntegral (allocated_bytes s1 - allocated_bytes s0) `div` fromIntegral iters :: Int
  putStrLn $ "selector matching: " <> show alloc1 <> " bytes/iter"

  go rwMutate 500
  performGC
  s2 <- getRTSStats
  go rwMutate iters
  performGC
  s3 <- getRTSStats
  let alloc2 = fromIntegral (allocated_bytes s3 - allocated_bytes s2) `div` fromIntegral iters :: Int
  putStrLn $ "mutations: " <> show alloc2 <> " bytes/iter"
