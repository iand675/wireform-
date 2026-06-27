{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Server (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Comment (..), Result (..), runParser)
import qualified Network.HTTP.Headers.Server as Server
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


tokenGen :: Gen ST.ShortText
tokenGen = ST.fromString <$> Gen.list (Range.linear 1 8) Gen.alphaNum


productGen :: Gen Server.Product
productGen = Server.Product <$> tokenGen <*> Gen.maybe tokenGen


commentGen :: Gen Comment
commentGen = Comment . Text.pack <$> Gen.list (Range.linear 0 8) Gen.alphaNum


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
      , it "round-trips render then parse" prop_roundtrip
      ]
