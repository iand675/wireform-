{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.WebSocket (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.SecWebSocketAccept as Accept
import qualified Network.HTTP.Headers.SecWebSocketExtensions as Ext
import qualified Network.HTTP.Headers.SecWebSocketKey as Key
import qualified Network.HTTP.Headers.SecWebSocketProtocol as Proto
import qualified Network.HTTP.Headers.SecWebSocketVersion as Ver
import Test.Syd
import Test.Syd.Hedgehog ()


-- Run a parser, accepting trailing optional whitespace.
runOk :: (ByteString -> Result String a) -> ByteString -> Either String a
runOk p bs = case p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


tok :: String -> ST.ShortText
tok = ST.fromString


-- ---------------------------------------------------------------------------
-- Sec-WebSocket-Key
-- ---------------------------------------------------------------------------

unit_key_parse :: Spec
unit_key_parse = it "parses the RFC 6455 example key" $
  case runOk (runParser Key.secWebSocketKeyParser) "dGhlIHNhbXBsZSBub25jZQ==" of
    Right (Key.SecWebSocketKey b) -> b `shouldBe` ("the sample nonce" :: ByteString)
    other -> error (show other)


unit_key_render :: Spec
unit_key_render =
  it "renders 16 nonce bytes as base64" $
    M.toStrictByteString (Key.renderSecWebSocketKey (Key.SecWebSocketKey "the sample nonce"))
      `shouldBe` "dGhlIHNhbXBsZSBub25jZQ=="


-- ---------------------------------------------------------------------------
-- Sec-WebSocket-Accept
-- ---------------------------------------------------------------------------

unit_accept_parse :: Spec
unit_accept_parse = it "parses the RFC 6455 example accept" $
  case runOk (runParser Accept.secWebSocketAcceptParser) "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" of
    Right v ->
      M.toStrictByteString (Accept.renderSecWebSocketAccept v)
        `shouldBe` "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    other -> error (show other)


unit_accept_render :: Spec
unit_accept_render =
  it "renders digest bytes as base64" $
    M.toStrictByteString (Accept.renderSecWebSocketAccept (Accept.SecWebSocketAccept "the sample nonce"))
      `shouldBe` "dGhlIHNhbXBsZSBub25jZQ=="


-- ---------------------------------------------------------------------------
-- Sec-WebSocket-Version
-- ---------------------------------------------------------------------------

unit_version_parse :: Spec
unit_version_parse = it "parses a comma list of versions" $
  case runOk (runParser Ver.secWebSocketVersionParser) "13, 8, 7" of
    Right (Ver.SecWebSocketVersion vs) -> NE.toList vs `shouldBe` [13, 8, 7]
    other -> error (show other)


unit_version_render :: Spec
unit_version_render =
  it "renders a single version" $
    M.toStrictByteString (Ver.renderSecWebSocketVersion (Ver.SecWebSocketVersion (13 :| [])))
      `shouldBe` "13"


-- ---------------------------------------------------------------------------
-- Sec-WebSocket-Protocol
-- ---------------------------------------------------------------------------

unit_protocol_parse :: Spec
unit_protocol_parse = it "parses subprotocol tokens" $
  case runOk (runParser Proto.secWebSocketProtocolParser) "chat, superchat" of
    Right (Proto.SecWebSocketProtocol ps) -> NE.toList ps `shouldBe` [tok "chat", tok "superchat"]
    other -> error (show other)


unit_protocol_render :: Spec
unit_protocol_render =
  it "renders subprotocol tokens comma-separated" $
    M.toStrictByteString
      (Proto.renderSecWebSocketProtocol (Proto.SecWebSocketProtocol (tok "chat" :| [tok "superchat"])))
      `shouldBe` "chat, superchat"


-- ---------------------------------------------------------------------------
-- Sec-WebSocket-Extensions
-- ---------------------------------------------------------------------------

unit_ext_parse :: Spec
unit_ext_parse = it "parses an extension with parameters" $
  case runOk
    (runParser Ext.secWebSocketExtensionsParser)
    "permessage-deflate; client_max_window_bits=15; server_no_context_takeover" of
    Right (Ext.SecWebSocketExtensions exts) -> case NE.toList exts of
      [Ext.Extension t params] -> do
        t `shouldBe` tok "permessage-deflate"
        params
          `shouldBe` [ Ext.ExtensionParam (tok "client_max_window_bits") (Just (tok "15"))
                     , Ext.ExtensionParam (tok "server_no_context_takeover") Nothing
                     ]
      other -> error (show other)
    other -> error (show other)


unit_ext_render :: Spec
unit_ext_render =
  it "renders extensions with parameters" $
    let v =
          Ext.SecWebSocketExtensions
            ( Ext.Extension
                (tok "permessage-deflate")
                [Ext.ExtensionParam (tok "client_max_window_bits") (Just (tok "15"))]
                :| [Ext.Extension (tok "x-custom") []]
            )
    in M.toStrictByteString (Ext.renderSecWebSocketExtensions v)
        `shouldBe` "permessage-deflate;client_max_window_bits=15, x-custom"


-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

bytesGen :: Gen ByteString
bytesGen = Gen.bytes (Range.linear 1 48)


tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tokenChars)
  where
    tokenChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_."


prop_key_roundtrip :: Property
prop_key_roundtrip = property $ do
  b <- forAll bytesGen
  let v = Key.SecWebSocketKey b
      bs = M.toStrictByteString (Key.renderSecWebSocketKey v)
  case runOk (runParser Key.secWebSocketKeyParser) bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


prop_accept_roundtrip :: Property
prop_accept_roundtrip = property $ do
  b <- forAll bytesGen
  let v = Accept.SecWebSocketAccept b
      bs = M.toStrictByteString (Accept.renderSecWebSocketAccept v)
  case runOk (runParser Accept.secWebSocketAcceptParser) bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


prop_version_roundtrip :: Property
prop_version_roundtrip = property $ do
  vs <- forAll (Gen.nonEmpty (Range.linear 1 5) (Gen.word (Range.linear 0 255)))
  let v = Ver.SecWebSocketVersion vs
      bs = M.toStrictByteString (Ver.renderSecWebSocketVersion v)
  case runOk (runParser Ver.secWebSocketVersionParser) bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


prop_protocol_roundtrip :: Property
prop_protocol_roundtrip = property $ do
  ps <- forAll (Gen.nonEmpty (Range.linear 1 4) tokenGen)
  let v = Proto.SecWebSocketProtocol ps
      bs = M.toStrictByteString (Proto.renderSecWebSocketProtocol v)
  case runOk (runParser Proto.secWebSocketProtocolParser) bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


paramGen :: Gen Ext.ExtensionParam
paramGen = Ext.ExtensionParam <$> tokenGen <*> Gen.maybe tokenGen


extGen :: Gen Ext.Extension
extGen = Ext.Extension <$> tokenGen <*> Gen.list (Range.linear 0 3) paramGen


prop_ext_roundtrip :: Property
prop_ext_roundtrip = property $ do
  exts <- forAll (Gen.nonEmpty (Range.linear 1 3) extGen)
  let v = Ext.SecWebSocketExtensions exts
      bs = M.toStrictByteString (Ext.renderSecWebSocketExtensions v)
  case runOk (runParser Ext.secWebSocketExtensionsParser) bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


tests :: Spec
tests =
  describe "WebSocket" $
    sequence_
      [ unit_key_parse
      , unit_key_render
      , unit_accept_parse
      , unit_accept_render
      , unit_version_parse
      , unit_version_render
      , unit_protocol_parse
      , unit_protocol_render
      , unit_ext_parse
      , unit_ext_render
      , it "key bytes round-trip" prop_key_roundtrip
      , it "accept bytes round-trip" prop_accept_roundtrip
      , it "version list round-trip" prop_version_roundtrip
      , it "protocol list round-trip" prop_protocol_roundtrip
      , it "extensions round-trip" prop_ext_roundtrip
      ]
