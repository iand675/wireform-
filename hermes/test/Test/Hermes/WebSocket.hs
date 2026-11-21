{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.WebSocket (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (HeaderMessage (..), defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, lookupHeaderWith, renderHeaderLines, setHeader, withHeaderDateReference, withHeaderMessage)
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


unit_key_field :: Spec
unit_key_field = it "requires exactly one field with 16 bytes and zero padding bits" $ do
  let wire = "AQIDBAUGBwgJCgsMDQ4PEA=="
      value = Key.SecWebSocketKey (BS.pack [1 .. 16])
      readField raw = lookupHeader @Key.SecWebSocketKey (headerMapFromList [("Sec-WebSocket-Key", raw)])
  readField (" \t" <> wire <> "\t") `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("sec-websocket-key", wire)]
  mapM_
    (\raw -> readField raw `shouldSatisfy` isLeft)
    ["", "AQID", "AQIDBAUGBwgJCgsMDQ4PEC==", "AQIDBAUGBwgJCgsMDQ4PEA", "AQIDBAUGBwgJCgsMDQ4PEAA=", "AQIDBAUGBwgJCgsMDQ4PEA==x"]
  lookupHeader @Key.SecWebSocketKey (headerMapFromList [("Sec-WebSocket-Key", wire), ("Sec-WebSocket-Key", wire)])
    `shouldSatisfy` isLeft


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
  it "renders the 20 digest bytes as base64" $
    M.toStrictByteString (Accept.renderSecWebSocketAccept sampleAccept)
      `shouldBe` "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="


sampleAccept :: Accept.SecWebSocketAccept
sampleAccept = Accept.SecWebSocketAccept (BS.pack [179, 122, 79, 44, 192, 98, 79, 22, 144, 246, 70, 6, 207, 56, 89, 69, 178, 190, 196, 234])


unit_accept_field :: Spec
unit_accept_field = it "requires exactly one field with a canonical padded 20-byte digest" $ do
  let wire = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
      readField raw = lookupHeader @Accept.SecWebSocketAccept (headerMapFromList [("Sec-WebSocket-Accept", raw)])
  readField ("\t" <> wire <> " ") `shouldBe` Right (Just sampleAccept)
  mapM_
    (\raw -> readField raw `shouldSatisfy` isLeft)
    ["dGhlIHNhbXBsZSBub25jZQ==", "s3pPLMBiTxaQ9kYGzzhZRbK+xOp=", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo", "s3pPLMBiTxaQ9kYGzzhZRbK+xO==", wire <> "X"]
  lookupHeader @Accept.SecWebSocketAccept (headerMapFromList [("Sec-WebSocket-Accept", wire), ("Sec-WebSocket-Accept", wire)])
    `shouldSatisfy` isLeft


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


unit_version_field :: Spec
unit_version_field = it "distinguishes a single request version from ordered response advertisements" $ do
  let request = withHeaderDateReference (UTCTime (fromGregorian 2020 1 1) 0) (withHeaderMessage HeaderRequest defaultHeaderSettings)
      response = withHeaderMessage HeaderResponse defaultHeaderSettings
      first wire = headerMapFromList [("Sec-WebSocket-Version", wire)]
      split = headerMapFromList [("Sec-WebSocket-Version", " 13, 8"), ("Sec-WebSocket-Version", ",7,0,255, ")]
      value = Ver.SecWebSocketVersion (13 :| [8, 7, 0, 255])
  lookupHeaderWith @Ver.SecWebSocketVersion request (first "\t13 ") `shouldBe` Right (Just (Ver.SecWebSocketVersion (13 :| [])))
  lookupHeaderWith @Ver.SecWebSocketVersion request (first "255") `shouldBe` Right (Just (Ver.SecWebSocketVersion (255 :| [])))
  lookupHeaderWith @Ver.SecWebSocketVersion request split `shouldSatisfy` isLeft
  lookupHeaderWith @Ver.SecWebSocketVersion request (first "13, 8") `shouldSatisfy` isLeft
  lookupHeaderWith @Ver.SecWebSocketVersion response (first ", \t13,, 8, 7,0,255 ,,") `shouldBe` Right (Just value)
  lookupHeaderWith @Ver.SecWebSocketVersion response split `shouldBe` Right (Just value)
  lookupHeader @Ver.SecWebSocketVersion split `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("sec-websocket-version", "13, 8, 7, 0, 255")]
  mapM_
    (\raw -> lookupHeaderWith @Ver.SecWebSocketVersion response (first raw) `shouldSatisfy` isLeft)
    ["", ", ,", "013", "00", "256", "299", "-1", "+13", "18446744073709551629", "13, 08"]


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


unit_protocol_field :: Spec
unit_protocol_field = it "preserves distinct case-sensitive offers and reads one selected token" $ do
  let request = withHeaderMessage HeaderRequest defaultHeaderSettings
      response = withHeaderMessage HeaderResponse defaultHeaderSettings
      first wire = headerMapFromList [("Sec-WebSocket-Protocol", wire)]
      split = headerMapFromList [("Sec-WebSocket-Protocol", ",Chat, "), ("Sec-WebSocket-Protocol", "chat,superchat")]
      duplicate = headerMapFromList [("Sec-WebSocket-Protocol", "Chat"), ("Sec-WebSocket-Protocol", "Chat")]
      offers = "Chat, chat"
      sliced = BS.take (BS.length offers) (BS.drop 1 ("!" <> offers <> "/"))
  lookupHeaderWith @Proto.SecWebSocketProtocol request split
    `shouldBe` Right (Just (Proto.SecWebSocketProtocol ("Chat" :| ["chat", "superchat"])))
  lookupHeaderWith @Proto.SecWebSocketProtocol request duplicate `shouldSatisfy` isLeft
  lookupHeaderWith @Proto.SecWebSocketProtocol request (first sliced)
    `shouldBe` Right (Just (Proto.SecWebSocketProtocol ("Chat" :| ["chat"])))
  lookupHeaderWith @Proto.SecWebSocketProtocol request (first ",Chat, , chat,, ")
    `shouldBe` Right (Just (Proto.SecWebSocketProtocol ("Chat" :| ["chat"])))
  lookupHeaderWith @Proto.SecWebSocketProtocol request (first ",chat,,chat,") `shouldSatisfy` isLeft
  lookupHeaderWith @Proto.SecWebSocketProtocol request (first "a,b,a") `shouldSatisfy` isLeft
  lookupHeaderWith @Proto.SecWebSocketProtocol response (first "chat")
    `shouldBe` Right (Just (Proto.SecWebSocketProtocol ("chat" :| [])))
  lookupHeaderWith @Proto.SecWebSocketProtocol response (first " chat\t")
    `shouldBe` Right (Just (Proto.SecWebSocketProtocol ("chat" :| [])))
  lookupHeaderWith @Proto.SecWebSocketProtocol response (first "chat,") `shouldSatisfy` isLeft
  lookupHeaderWith @Proto.SecWebSocketProtocol response split `shouldSatisfy` isLeft


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


unit_ext_field :: Spec
unit_ext_field = it "preserves ordered extension lines and requires unescaped parameter tokens" $ do
  let values = headerMapFromList [("Sec-WebSocket-Extensions", ",foo; mode \t= \"a\\b\""), ("Sec-WebSocket-Extensions", "bar;flag,")]
      value = Ext.SecWebSocketExtensions (Ext.Extension "foo" [Ext.ExtensionParam "mode" (Just "ab")] :| [Ext.Extension "bar" [Ext.ExtensionParam "flag" Nothing]])
      readField raw = lookupHeader @Ext.SecWebSocketExtensions (headerMapFromList [("Sec-WebSocket-Extensions", raw)])
      readOn side = lookupHeaderWith @Ext.SecWebSocketExtensions (withHeaderMessage side defaultHeaderSettings) values
  readOn HeaderRequest `shouldBe` Right (Just value)
  readOn HeaderResponse `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("sec-websocket-extensions", "foo;mode=ab, bar;flag")]
  mapM_
    (\raw -> readField raw `shouldSatisfy` isLeft)
    ["foo;mode=\"\"", "foo;mode=\"a b\"", "foo;mode=\"a,b\"", "foo;mode=\"a\\\"b\"", "foo;mode=\"\xff\"", "foo;mode=", "foo;", ", ,"]


checked_writers :: Spec
checked_writers = do
  describe "Sec-WebSocket-Key" $
    it "validates public constructors before writing field lines" $ do
      let wire = "dGhlIHNhbXBsZSBub25jZQ=="
          previous = headerMapFromList [("Sec-WebSocket-Key", wire)]
      renderHeaderLines defaultHeaderSettings (Key.SecWebSocketKey "the sample nonce") `shouldBe` Just (wire :| [])
      forM_ [0, 15, 17] $ \width -> do
        let value = Key.SecWebSocketKey (BS.replicate width 0xff)
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value previous `shouldBe` previous
  describe "Sec-WebSocket-Accept" $
    it "validates public constructors before writing field lines" $ do
      let wire = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
          previous = headerMapFromList [("Sec-WebSocket-Accept", wire)]
      renderHeaderLines defaultHeaderSettings sampleAccept `shouldBe` Just (wire :| [])
      forM_ [0, 19, 21] $ \width -> do
        let value = Accept.SecWebSocketAccept (BS.replicate width 0xff)
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value previous `shouldBe` previous
  describe "Sec-WebSocket-Version" $
    it "validates public constructors before writing field lines" $ do
      let request = withHeaderMessage HeaderRequest defaultHeaderSettings
          response = withHeaderMessage HeaderResponse defaultHeaderSettings
          previous = headerMapFromList [("Sec-WebSocket-Version", "13")]
          versions = Ver.SecWebSocketVersion (13 :| [0, 255])
      renderHeaderLines request (Ver.SecWebSocketVersion (13 :| [])) `shouldBe` Just ("13" :| [])
      renderHeaderLines response versions `shouldBe` Just ("13, 0, 255" :| [])
      renderHeaderLines request versions `shouldBe` Nothing
      forM_ [256 :| [], 13 :| [maxBound]] $ \numbers -> do
        let invalid = Ver.SecWebSocketVersion numbers
        renderHeaderLines response invalid `shouldBe` Nothing
        setHeader invalid previous `shouldBe` previous
  describe "Sec-WebSocket-Protocol" $
    it "validates public constructors before writing field lines" $ do
      let request = withHeaderMessage HeaderRequest defaultHeaderSettings
          response = withHeaderMessage HeaderResponse defaultHeaderSettings
          previous = headerMapFromList [("Sec-WebSocket-Protocol", "chat")]
          protocols = Proto.SecWebSocketProtocol ("Chat" :| ["chat"])
      renderHeaderLines response (Proto.SecWebSocketProtocol ("chat" :| [])) `shouldBe` Just ("chat" :| [])
      renderHeaderLines request protocols `shouldBe` Just ("Chat, chat" :| [])
      renderHeaderLines response protocols `shouldBe` Nothing
      forM_ ["" :| [], "chat, superchat" :| [], "chat" :| ["chat"], "one" :| ["two", "one"], "chat" :| ["💬"]] $ \tokens -> do
        let invalid = Proto.SecWebSocketProtocol tokens
        renderHeaderLines request invalid `shouldBe` Nothing
        setHeader invalid previous `shouldBe` previous
  describe "Sec-WebSocket-Extensions" $
    it "validates public constructors before writing field lines" $ do
      let previous = headerMapFromList [("Sec-WebSocket-Extensions", "foo")]
          make extension = Ext.SecWebSocketExtensions (Ext.Extension "foo" [] :| [extension])
          valid = make (Ext.Extension "bar" [Ext.ExtensionParam "flag" Nothing, Ext.ExtensionParam "mode" (Just "x")])
      renderHeaderLines defaultHeaderSettings valid `shouldBe` Just ("foo, bar;flag;mode=x" :| [])
      forM_ [Ext.Extension "bar;q" [], Ext.Extension "" [], Ext.Extension "bar" [Ext.ExtensionParam "q=x" Nothing], Ext.Extension "bar" [Ext.ExtensionParam "q" (Just "")], Ext.Extension "bar" [Ext.ExtensionParam "q" (Just "a b")]] $ \invalid -> do
        renderHeaderLines defaultHeaderSettings (make invalid) `shouldBe` Nothing
        setHeader (make invalid) previous `shouldBe` previous


-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element tokenChars)
  where
    tokenChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_."


prop_key_roundtrip :: Property
prop_key_roundtrip = property $ do
  b <- forAll (Gen.bytes (Range.singleton 16))
  let v = Key.SecWebSocketKey b
      bs = M.toStrictByteString (Key.renderSecWebSocketKey v)
  case runOk (runParser Key.secWebSocketKeyParser) bs of
    Right v' -> v === v'
    Left e -> error (e <> " on " <> show bs)


prop_accept_roundtrip :: Property
prop_accept_roundtrip = property $ do
  b <- forAll (Gen.bytes (Range.singleton 20))
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
  ps <- forAll (NE.nub <$> Gen.nonEmpty (Range.linear 1 4) tokenGen)
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
      , unit_key_field
      , unit_accept_parse
      , unit_accept_render
      , unit_accept_field
      , unit_version_parse
      , unit_version_render
      , unit_version_field
      , unit_protocol_parse
      , unit_protocol_render
      , unit_protocol_field
      , unit_ext_parse
      , unit_ext_render
      , unit_ext_field
      , checked_writers
      , it "key bytes round-trip" prop_key_roundtrip
      , it "accept bytes round-trip" prop_accept_roundtrip
      , it "version list round-trip" prop_version_roundtrip
      , it "protocol list round-trip" prop_protocol_roundtrip
      , it "extensions round-trip" prop_ext_roundtrip
      ]
