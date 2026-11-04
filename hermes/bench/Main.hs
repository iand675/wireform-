{-# OPTIONS_GHC -Wno-orphans #-}

{- | Criterion benchmarks for 'HeaderMap' construction, lookup and rendering.

Fixtures cover the common shapes a server sees: a small request, a typical
browser request, and a response with many repeated @Set-Cookie@ lines.
-}
module Main (main) where

import Control.DeepSeq (NFData (..))
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.CaseInsensitive (CI)
import Network.HTTP.Headers
import Network.HTTP.Headers.CacheControl (CacheControl (..))
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName, hCacheControl, hSetCookie)


-- 'env' needs to fully force the prepared map before timing starts.
instance NFData HeaderMap where
  rnf = rnf . headerMapToMap


main :: IO ()
main =
  defaultMain
    [ fixture "small" smallRequest hCacheControl
    , fixture "typical" typicalRequest hCacheControl
    , fixture "repeated" repeatedSetCookie hSetCookie
    ]


fixture :: String -> [(CI ByteString, ByteString)] -> HeaderFieldName -> Benchmark
fixture label hs hit =
  env (pure (hs, headerMapFromList hs)) $ \ ~(list, m) ->
    bgroup
      label
      [ bench "headerMapFromList" $ nf (headerMapToMap . headerMapFromList) list
      , bench "headerMapToList" $ nf headerMapToList m
      , bench "lookupRawHeader/hit" $ nf (lookupRawHeader hit) m
      , bench "lookupRawHeader/miss" $ nf (lookupRawHeader "x-not-present") m
      , bench "lookupHeader/CacheControl" $ whnf forceCacheControl m
      ]


-- CacheControl has no NFData instance; counting the directives forces the parse.
forceCacheControl :: HeaderMap -> Int
forceCacheControl m = case lookupHeader @CacheControl m of
  Left _ -> -1
  Right Nothing -> 0
  Right (Just cc) -> length (cacheControlDirectives cc)


smallRequest :: [(CI ByteString, ByteString)]
smallRequest =
  [ ("Host", "example.com")
  , ("User-Agent", "curl/8.7.1")
  , ("Accept", "*/*")
  , ("Accept-Encoding", "gzip, br")
  , ("Cache-Control", "no-cache")
  ]


typicalRequest :: [(CI ByteString, ByteString)]
typicalRequest =
  [ ("Host", "example.com")
  , ("Connection", "keep-alive")
  , ("Cache-Control", "max-age=0")
  , ("Upgrade-Insecure-Requests", "1")
  , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36")
  , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8")
  , ("Sec-Fetch-Site", "none")
  , ("Sec-Fetch-Mode", "navigate")
  , ("Sec-Fetch-User", "?1")
  , ("Sec-Fetch-Dest", "document")
  , ("Accept-Encoding", "gzip, deflate, br, zstd")
  , ("Accept-Language", "en-US,en;q=0.9")
  , ("Cookie", "session=abc123; theme=dark")
  , ("Cookie", "tracking=xyz789")
  , ("Cache-Control", "no-transform")
  ]


repeatedSetCookie :: [(CI ByteString, ByteString)]
repeatedSetCookie =
  [ ("Content-Type", "text/html; charset=utf-8")
  , ("Cache-Control", "private, max-age=60")
  ]
    <> map setCookie [1 .. 20 :: Int]
  where
    setCookie i = ("Set-Cookie", BS.toStrict (B.toLazyByteString ("c" <> B.intDec i <> "=v; Path=/; HttpOnly")))
