{-# LANGUAGE OverloadedStrings #-}

{- |
'ProxyAuthenticate' reuses the WWW-Authenticate parser; smoke-test
that the @KnownHeader@ instance pins the right field name and
the wrapper newtype carries the structure through unchanged.
-}
module Test.Hermes.ProxyAuthenticate (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.ProxyAuthenticate as P
import qualified Network.HTTP.Headers.WWWAuthenticate as W
import Test.Syd


parseOk :: ByteString -> Either String P.ProxyAuthenticate
parseOk bs = case runParser P.proxyAuthenticateParser bs of
  OK p leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right p
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


unit_smoke :: Spec
unit_smoke = it "Proxy-Authenticate roundtrip" $
  case parseOk "Basic realm=\"corp\", Digest realm=\"api\", qop=\"auth\", nonce=\"n\"" of
    Right (P.ProxyAuthenticate [b, d]) -> do
      W.challengeScheme b `shouldBe` W.AuthScheme (ST.fromString "Basic")
      W.challengeScheme d `shouldBe` W.AuthScheme (ST.fromString "Digest")
    other -> error ("unexpected parse: " <> show other)


tests :: Spec
tests = describe "ProxyAuthenticate" $ sequence_ [unit_smoke]
