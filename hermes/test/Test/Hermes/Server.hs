{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Server (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hServer, hUserAgent)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Comment (..), Result (..), comment, eof, runParser)
import qualified Network.HTTP.Headers.Server as Server
import qualified Network.HTTP.Headers.UserAgent as UserAgent
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String Server.Server
parseOk bs = case runParser Server.serverParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: Server.Server -> ByteString
render = M.toStrictByteString . Server.renderServer


unit_bare_product :: Spec
unit_bare_product = it "parses a bare product token" $
  case parseOk "CERN" of
    Right (Server.Server (Server.Product n Nothing) []) ->
      n `shouldBe` ST.fromString "CERN"
    other -> error (show other)


unit_product_version_comment :: Spec
unit_product_version_comment = it "parses product/version with a comment" $
  case parseOk "Apache/2.4.1 (Unix)" of
    Right (Server.Server (Server.Product n v) rest) -> do
      n `shouldBe` ST.fromString "Apache"
      v `shouldBe` Just (ST.fromString "2.4.1")
      rest `shouldBe` [Left (Comment "Unix")]
    other -> error (show other)


unit_multiple_products :: Spec
unit_multiple_products = it "parses several whitespace-separated products" $
  case parseOk "nginx/1.25 OpenSSL/3" of
    Right (Server.Server (Server.Product n v) rest) -> do
      n `shouldBe` ST.fromString "nginx"
      v `shouldBe` Just (ST.fromString "1.25")
      rest `shouldBe` [Right (Server.Product (ST.fromString "OpenSSL") (Just (ST.fromString "3")))]
    other -> error (show other)


unit_render :: Spec
unit_render =
  it "renders products and a comment RWS-separated" $
    let v =
          Server.Server
            (Server.Product (ST.fromString "nginx") (Just (ST.fromString "1.25")))
            [ Left (Comment "Ubuntu")
            , Right (Server.Product (ST.fromString "OpenSSL") (Just (ST.fromString "3")))
            ]
    in render v `shouldBe` "nginx/1.25 (Ubuntu) OpenSSL/3"


unit_render_bare :: Spec
unit_render_bare =
  it "renders a bare product without a version" $
    let v = Server.Server (Server.Product (ST.fromString "CERN") Nothing) []
    in render v `shouldBe` "CERN"


unit_user_agent_products :: Spec
unit_user_agent_products = it "parses and renders spaced User-Agent products and comments" $ do
  let value =
        UserAgent.UserAgent
          (UserAgent.Product "Example" (Just "2"))
          [Left (Comment "foo"), Right (UserAgent.Product "TLS" (Just "1")), Left (Comment "")]
      headers = headerMapFromList [("User-Agent", "\tExample/2\t  (foo)  TLS/1 ()\t ")]
  lookupHeader headers `shouldBe` Right (Just value)
  lookupRawHeader hUserAgent (setHeader value headers) `shouldBe` Just ("Example/2 (foo) TLS/1 ()" :| [])


unit_nested_and_quoted :: Spec
unit_nested_and_quoted = it "decodes nested and quoted comment text and preserves obs-text bytes" $ do
  let raw = "Example/2 (a (nested) \"c\" \\) \\\\ " <> BS.pack [0x80, 0xff] <> ")"
      wire = "Example/2 (a \\(nested\\) \"c\" \\) \\\\ " <> BS.pack [0x80, 0xff] <> ")"
      value = Server.Server (Server.Product "Example" (Just "2")) [Left (Comment "a (nested) \"c\" ) \\ \x80\xff")]
      headers = headerMapFromList [("Server", raw)]
  lookupHeader headers `shouldBe` Right (Just value)
  lookupRawHeader hServer (setHeader value headers) `shouldBe` Just (wire :| [])
  lookupHeader (headerMapFromList [("Server", wire)]) `shouldBe` Right (Just value)


unit_invalid_product_fields :: Spec
unit_invalid_product_fields = it "rejects malformed and repeated singleton product fields" $ do
  mapM_
    (\wire -> lookupHeader @UserAgent.UserAgent (headerMapFromList [("User-Agent", wire)]) `shouldSatisfy` isLeft)
    [ "Example/2(foo)"
    , "Example/ TLS"
    , "Example/2 (unclosed"
    , "Example/2 (bad\\)"
    , "Example/2 (bad\rbyte)"
    , "Example/2 (bad\\\rbyte)"
    , "Example/2 (bad\x7f)"
    ]
  lookupHeader @UserAgent.UserAgent (headerMapFromList [("User-Agent", "Example"), ("User-Agent", "TLS")])
    `shouldSatisfy` isLeft
  lookupHeader @Server.Server (headerMapFromList [("Server", "Example"), ("Server", "TLS")])
    `shouldSatisfy` isLeft


unit_deep_comments :: Spec
unit_deep_comments = it "handles deep comments and ends at the matching close" $ do
  let depth = 32768
      input = BS.replicate depth 40 <> "x" <> BS.replicate depth 41 <> "rest"
  case runParser comment input :: Result String Text.Text of
    OK body "rest" -> Text.length body `shouldBe` (2 * depth - 1)
    other -> expectationFailure (show other)


unit_comment_octets :: Spec
unit_comment_octets =
  it "matches the comment and quoted-pair octet ranges" $
    mapM_ check [0 .. 255]
  where
    check byte = do
      let literalAllowed = byte == 9 || byte >= 32 && byte /= 127 && byte /= 40 && byte /= 41 && byte /= 92
          quotedAllowed = byte == 9 || byte >= 32 && byte /= 127
          expected = Text.singleton (chr (fromIntegral byte))
      decode (BS.pack [40, byte, 41]) `shouldBe` if literalAllowed then Just expected else Nothing
      decode (BS.pack [40, 92, byte, 41]) `shouldBe` if quotedAllowed then Just expected else Nothing
    decode bytes = case runParser (comment <* eof) bytes of
      OK content "" -> Just content
      _ -> Nothing


tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) Gen.alphaNum


productGen :: Gen Server.Product
productGen = Server.Product <$> tokenGen <*> Gen.maybe tokenGen


commentGen :: Gen Comment
commentGen =
  Comment . Text.pack
    <$> Gen.list (Range.linear 0 16) (Gen.element (['0' .. '9'] <> " \t()\\\"\x80\xfe\xff"))


elementGen :: Gen (Either Comment Server.Product)
elementGen = Gen.choice [Left <$> commentGen, Right <$> productGen]


serverGen :: Gen Server.Server
serverGen = Server.Server <$> productGen <*> Gen.list (Range.linear 0 4) elementGen


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll serverGen
  let bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Server" $
    sequence_
      [ unit_bare_product
      , unit_product_version_comment
      , unit_multiple_products
      , unit_render
      , unit_render_bare
      , unit_user_agent_products
      , unit_nested_and_quoted
      , unit_invalid_product_fields
      , unit_deep_comments
      , unit_comment_octets
      , it "round-trips render then parse" prop_roundtrip
      ]
