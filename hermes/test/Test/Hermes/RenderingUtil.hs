{-# LANGUAGE OverloadedStrings #-}

{- |
Test the @rfc8941String@ escape that was previously disabled.

A round-trip through 'rfc8941String' must produce a quoted-string
on the wire that escapes any embedded DQUOTE or backslash, and
the @rfc8941String@ parser ('Network.HTTP.Headers.Parsing.Util.rfc8941String')
must accept it back as the same value.
-}
module Test.Hermes.RenderingUtil (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (
  RFC8941String,
  Result (..),
  mkRFC8941String,
  rfc8941String,
  runParser,
  unsafeToRFC8941String,
 )
import qualified Network.HTTP.Headers.Rendering.Util as R
import Test.Syd


-- Build an RFC8941String value from a 'ByteString' for tests; the
-- input is restricted to bytes the parser will accept later.
mkVal :: ByteString -> RFC8941String
mkVal bs = case mkRFC8941String (st bs) of
  Just s -> s
  Nothing -> error ("mkVal: input not RFC8941-stringable: " <> show bs)
  where
    st x = case ST.fromByteString x of
      Just s -> s
      Nothing -> error "non-UTF-8 in test input"


renderToBytes :: RFC8941String -> ByteString
renderToBytes = M.toStrictByteString . R.rfc8941String


parseValue :: ByteString -> Either String RFC8941String
parseValue bs = case runParser rfc8941String bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


unit_simple :: Spec
unit_simple =
  it "no escaping for plain ASCII" $
    let v = mkVal "hello world"
        out = renderToBytes v
    in out `shouldBe` "\"hello world\""


unit_escape_quote :: Spec
unit_escape_quote =
  it "embedded DQUOTE is escaped" $
    let v = mkVal "a\"b"
        out = renderToBytes v
    in do
        out `shouldBe` "\"a\\\"b\""
        -- Parser recovers the original payload.
        case parseValue out of
          Right v' -> ST.toByteString (unsafeToRFC8941String v') `shouldBe` ST.toByteString (unsafeToRFC8941String v)
          Left err -> error err


unit_escape_backslash :: Spec
unit_escape_backslash =
  it "embedded backslash is escaped" $
    let v = mkVal "a\\b"
        out = renderToBytes v
    in do
        out `shouldBe` "\"a\\\\b\""
        case parseValue out of
          Right v' -> ST.toByteString (unsafeToRFC8941String v') `shouldBe` ST.toByteString (unsafeToRFC8941String v)
          Left err -> error err


unit_double_escape :: Spec
unit_double_escape =
  it "both DQUOTE and backslash" $
    let v = mkVal "a\"b\\c"
        out = renderToBytes v
    in do
        (if "\\\"" `BS.isInfixOf` out then pure () else expectationFailure ("\\\" in " <> show out))
        (if "\\\\" `BS.isInfixOf` out then pure () else expectationFailure ("\\\\ in " <> show out))


tests :: Spec
tests =
  describe "RenderingUtil" $
    sequence_
      [ unit_simple
      , unit_escape_quote
      , unit_escape_backslash
      , unit_double_escape
      ]
