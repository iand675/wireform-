{-# LANGUAGE OverloadedStrings #-}

{- | Interning against @symbolize@ (the interner this package replaces).

* @intern-hit@: re-interning names that are already live, the steady state
  of an HTTP server interning request header names.
* @intern-fresh@: 1000 names nobody holds, so each run creates (or finds
  dead and replaces) every entry, and the table sweeps as it goes.
* @hash@: hashing live symbols, as a @HashMap@ keyed by them does.
-}
module Main (main) where

import Control.DeepSeq (NFData (..), rwhnf)
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Hashable (hash)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Symbol
import qualified Symbolize

newtype WHNF a = WHNF a

instance NFData (WHNF a) where rnf = rwhnf

headerNames :: [ByteString]
headerNames =
  [ "host", "user-agent", "accept", "accept-encoding", "accept-language", "cache-control"
  , "connection", "content-length", "content-type", "cookie", "origin", "referer"
  , "sec-fetch-dest", "sec-fetch-mode", "sec-fetch-site", "sec-fetch-user"
  , "upgrade-insecure-requests", "if-none-match", "if-modified-since", "x-forwarded-for"
  , "x-request-id", "authorization", "pragma", "priority"
  ]

sumBy :: (a -> Int) -> [a] -> Int
sumBy f = go 0
 where
  go !acc [] = acc
  go !acc (x : xs) = go (acc + f x) xs

main :: IO ()
main = do
  let texts = map TE.decodeUtf8 headerNames
      ours = map Symbol.intern headerNames
      theirs = map Symbolize.intern headerNames
      freshNames = map (\i -> BS8.pack ("x-fresh-" <> show i)) [1 .. 1000 :: Int]
  -- keep the hit sets alive for the whole run
  rnf ours `seq` rnf theirs `seq` rnf texts `seq` pure ()
  defaultMain
    [ bgroup
        "intern-hit/bytestring"
        [ bench "wireform-symbol" $ nf (sumBy (hash . Symbol.intern)) headerNames
        , bench "symbolize" $ nf (sumBy (hash . Symbolize.intern)) headerNames
        ]
    , bgroup
        "intern-hit/text"
        [ bench "wireform-symbol" $ nf (sumBy (hash . Symbol.intern)) texts
        , bench "symbolize" $ nf (sumBy (hash . Symbolize.intern)) texts
        ]
    , bgroup
        "intern-fresh/bytestring"
        [ bench "wireform-symbol" $ nf (sumBy (Symbol.symbolLength . Symbol.intern)) freshNames
        , bench "symbolize" $ nf (sumBy (T.length . Symbolize.unintern . Symbolize.intern)) freshNames
        ]
    , bgroup
        "hash"
        [ bench "wireform-symbol" $ nf (sumBy hash) ours
        , bench "symbolize" $ nf (sumBy hash) theirs
        ]
    , bgroup
        "eq"
        [ bench "wireform-symbol" $ nf (sumBy (\s -> fromEnum (s == head ours))) ours
        , bench "symbolize" $ nf (sumBy (\s -> fromEnum (s == head theirs))) theirs
        ]
    ]
  -- the hit sets must survive until here
  rnf ours `seq` rnf theirs `seq` pure ()

