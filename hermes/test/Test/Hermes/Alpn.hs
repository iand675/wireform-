{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Alpn (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.ALPN as A
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.TLS.Extensions as E
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String A.ALPN
parseOk bs = case runParser A.alpnParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: A.ALPN -> ByteString
render = M.toStrictByteString . A.renderALPN


-- Parse the canonical RFC 7639 example, percent-decoding @http%2F1.1@ back to
-- the @http/1.1@ identification sequence.
unit_parse :: Spec
unit_parse = it "parses the RFC 7639 example (h2, http%2F1.1)" $
  case parseOk "h2, http%2F1.1" of
    Right (A.ALPN (a :| [b])) -> do
      a `shouldBe` E.alpnHttp2
      b `shouldBe` E.alpnHttp11
    other -> error (show other)


-- Octets that are not token characters (here @/@) must be percent-encoded with
-- uppercase hex; plain token octets pass through verbatim.
unit_render :: Spec
unit_render =
  it "renders protocol IDs percent-encoded per RFC 7639" $
    render (A.ALPN (E.alpnHttp2 :| [E.alpnHttp11]))
      `shouldBe` "h2, http%2F1.1"


-- Property: any non-empty list of arbitrary octet sequences renders to a header
-- value that parses back to the original protocol IDs.
genProtocol :: Gen E.ALPNProtocol
genProtocol =
  E.mkALPNProtocol . BS.pack
    <$> Gen.list (Range.linear 1 8) (Gen.word8 Range.constantBounded)


genAlpn :: Gen A.ALPN
genAlpn = A.ALPN <$> Gen.nonEmpty (Range.linear 1 4) genProtocol


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll genAlpn
  let bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Alpn" $
    sequence_
      [ unit_parse
      , unit_render
      , it "combines ordered lines and skips empty members through typed lookup" $ do
          let fields = headerMapFromList [("ALPN", ",h2, "), ("ALPN", "\t ,"), ("ALPN", "http%2F1.1, h2 ")]
              expected = A.ALPN (E.alpnHttp2 :| [E.alpnHttp11, E.alpnHttp2])
          lookupHeader @A.ALPN fields `shouldBe` Right (Just expected)
          lookupRawHeader "ALPN" (setHeader expected (headerMapFromList []))
            `shouldBe` Just ("h2, http%2F1.1, h2" :| [])
      , it "requires at least one member and canonical percent escapes on every line" $ do
          forM_ ["", ", ,", "h%32", "http%2f1.1", "%2B", "%", "%0", "%Q0", "%0G", "http/1.1", "h\0"] $ \wire -> do
            parseOk wire `shouldSatisfy` isLeft
            lookupHeader @A.ALPN (headerMapFromList [("ALPN", wire)]) `shouldSatisfy` isLeft
          lookupHeader @A.ALPN (headerMapFromList [("ALPN", "h2"), ("ALPN", "%68")]) `shouldSatisfy` isLeft
      , it "round-trips every opaque protocol octet in its canonical form" $
          forM_ [0 .. 255] $ \byte -> do
            let protocol = A.ALPN (E.mkALPNProtocol (BS.singleton byte) :| [])
                alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$&'*+-.^`_|~"
                hex = BS.index "0123456789ABCDEF" . fromIntegral
                (high, low) = byte `quotRem` 16
                wire = if byte `BS.elem` alphabet then BS.singleton byte else BS.pack [37, hex high, hex low]
            parseOk wire `shouldBe` Right protocol
            render protocol `shouldBe` wire
      , it "bounds protocol names by their decoded size" $ do
          let wire = BS.concat (replicate 255 "%FF")
              value = A.ALPN (E.mkALPNProtocol (BS.replicate 255 255) :| [])
          parseOk wire `shouldBe` Right value
          parseOk (wire <> "a") `shouldSatisfy` isLeft
          parseOk (BS.replicate 255 65) `shouldBe` Right (A.ALPN (E.mkALPNProtocol (BS.replicate 255 65) :| []))
          lookupHeader @A.ALPN (headerMapFromList [("ALPN", BS.replicate 256 65)]) `shouldSatisfy` isLeft
      , it "validates the decoded size of every authored protocol before replacing field lines" $ do
          let existing = headerMapFromList [("ALPN", "h2"), ("ALPN", "http%2F1.1")]
              value wire = A.ALPN (E.alpnHttp2 :| [E.mkALPNProtocol wire])
          forM_ ["", BS.replicate 256 0xff, BS.replicate 256 65] $ \wire -> do
            renderHeaderLines defaultHeaderSettings (value wire) `shouldBe` Nothing
            setHeader (value wire) existing `shouldBe` existing
          let accepted = value (BS.replicate 255 0xff)
          lookupRawHeader "ALPN" (setHeader accepted existing)
            `shouldBe` Just (("h2, " <> BS.concat (replicate 255 "%FF")) :| [])
          lookupHeader @A.ALPN (setHeader accepted existing) `shouldBe` Right (Just accepted)
      , it "round-trips arbitrary protocol-id octet sequences" prop_roundtrip
      ]
