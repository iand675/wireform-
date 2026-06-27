{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteCookie (tests) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Cookie2 as C2
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.SetCookie2 as SC2
import Test.Syd
import Test.Syd.Hedgehog ()


parseCookie2 :: ByteString -> Either String C2.Cookie2
parseCookie2 bs = case runParser C2.cookie2Parser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCookie2 :: C2.Cookie2 -> ByteString
renderCookie2 = M.toStrictByteString . C2.renderCookie2


parseSetCookie2 :: ByteString -> Either String SC2.SetCookie2
parseSetCookie2 bs = case runParser SC2.setCookie2Parser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSetCookie2 :: SC2.SetCookie2 -> ByteString
renderSetCookie2 = M.toStrictByteString . SC2.renderSetCookie2


-- Cookie2 -----------------------------------------------------------------

unit_cookie2_parse :: Spec
unit_cookie2_parse = it "parses a Cookie2 version directive" $
  case parseCookie2 "$Version=\"1\"" of
    Right (C2.Cookie2 v) -> v `shouldBe` ST.fromString "$Version=\"1\""
    other -> error (show other)


unit_cookie2_render :: Spec
unit_cookie2_render =
  it "renders a Cookie2 version directive verbatim" $
    renderCookie2 (C2.Cookie2 (ST.fromString "$Version=\"1\"")) `shouldBe` "$Version=\"1\""


-- Set-Cookie2 -------------------------------------------------------------

unit_setcookie2_parse :: Spec
unit_setcookie2_parse = it "parses a Set-Cookie2 definition" $
  case parseSetCookie2 "Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\"" of
    Right (SC2.SetCookie2 v) ->
      v `shouldBe` ST.fromString "Customer=\"WILE_E_COYOTE\"; Version=\"1\"; Path=\"/acme\""
    other -> error (show other)


unit_setcookie2_render :: Spec
unit_setcookie2_render =
  it "renders a Set-Cookie2 definition verbatim" $
    renderSetCookie2 (SC2.SetCookie2 (ST.fromString "Part_Number=\"Rocket_Launcher_0001\"; Version=\"1\"; Path=\"/acme\""))
      `shouldBe` "Part_Number=\"Rocket_Launcher_0001\"; Version=\"1\"; Path=\"/acme\""


-- Round-trip properties ---------------------------------------------------

{- | Printable, non-empty ASCII values within the obsolete cookie grammar.
The raw-preserving newtypes round-trip any such value byte-for-byte.
-}
valueGen :: Gen ST.ShortText
valueGen =
  ST.fromString
    <$> Gen.string (Range.linear 1 64) (Gen.element cookieChars)
  where
    cookieChars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "=\"_-./$; "


prop_cookie2_roundtrip :: Property
prop_cookie2_roundtrip = property $ do
  v <- C2.Cookie2 <$> forAll valueGen
  let bs = renderCookie2 v
  case parseCookie2 bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


prop_setcookie2_roundtrip :: Property
prop_setcookie2_roundtrip = property $ do
  v <- SC2.SetCookie2 <$> forAll valueGen
  let bs = renderSetCookie2 v
  case parseSetCookie2 bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "ObsoleteCookie" $
    sequence_
      [ unit_cookie2_parse
      , unit_cookie2_render
      , unit_setcookie2_parse
      , unit_setcookie2_render
      , it "Cookie2 round-trip" prop_cookie2_roundtrip
      , it "Set-Cookie2 round-trip" prop_setcookie2_roundtrip
      ]
