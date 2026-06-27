{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Framing (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Close as Close
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.MaxForwards as MaxForwards
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.TE as TE
import qualified Network.HTTP.Headers.Trailer as Trailer
import qualified Network.HTTP.Headers.Upgrade as Upgrade
import qualified Network.HTTP.Headers.Via as Via
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers -----------------------------------------------------------------

finish :: Result String a -> Either String a
finish = \case
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseTE :: ByteString -> Either String TE.TE
parseTE = finish . runParser TE.teParser


parseTrailer :: ByteString -> Either String Trailer.Trailer
parseTrailer = finish . runParser Trailer.trailerParser


parseUpgrade :: ByteString -> Either String Upgrade.Upgrade
parseUpgrade = finish . runParser Upgrade.upgradeParser


parseVia :: ByteString -> Either String Via.Via
parseVia = finish . runParser Via.viaParser


parseMaxForwards :: ByteString -> Either String MaxForwards.MaxForwards
parseMaxForwards = finish . runParser MaxForwards.maxForwardsParser


parseClose :: ByteString -> Either String Close.Close
parseClose = finish . runParser Close.closeParser


roundtrip
  :: (Eq a, Show a)
  => Gen a
  -> (ByteString -> Either String a)
  -> (a -> M.Builder)
  -> Property
roundtrip gen parse render = property $ do
  x <- forAll gen
  let bs = M.toStrictByteString (render x)
  case parse bs of
    Right y -> y === x
    Left e -> error (e <> " on " <> show bs)


-- Generators --------------------------------------------------------------

genToken :: Gen ST.ShortText
genToken = ST.fromString <$> Gen.string (Range.linear 1 8) (Gen.element tokenChars)
  where
    tokenChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-._"


genNE :: Gen a -> Gen (NonEmpty a)
genNE g = (:|) <$> g <*> Gen.list (Range.linear 0 3) g


genWeight :: Gen (Maybe Double)
genWeight = Gen.maybe (Gen.element [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9])


genTE :: Gen TE.TE
genTE = TE.TE <$> genNE (TE.TECoding <$> genToken <*> genWeight)


genTrailer :: Gen Trailer.Trailer
genTrailer = Trailer.Trailer <$> genNE genToken


genUpgrade :: Gen Upgrade.Upgrade
genUpgrade = Upgrade.Upgrade <$> genNE (Upgrade.Protocol <$> genToken <*> Gen.maybe genToken)


genReceivedBy :: Gen ST.ShortText
genReceivedBy =
  Gen.choice
    [ genToken
    , do
        h <- genToken
        p <- Gen.int (Range.linear 1 65535)
        pure (h <> ST.fromString (':' : show p))
    ]


genComment :: Gen (Maybe Text)
genComment = Gen.maybe (T.pack <$> Gen.string (Range.linear 1 10) (Gen.element commentChars))
  where
    commentChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " -._"


genVia :: Gen Via.Via
genVia = Via.Via <$> genNE entry
  where
    entry =
      Via.ViaEntry
        <$> (Via.ReceivedProtocol <$> Gen.maybe genToken <*> genToken)
        <*> genReceivedBy
        <*> genComment


genMaxForwards :: Gen MaxForwards.MaxForwards
genMaxForwards = MaxForwards.MaxForwards <$> Gen.int (Range.linear 0 1_000_000)


-- TE ----------------------------------------------------------------------

unit_te_parse :: Spec
unit_te_parse = it "parses transfer-codings with weights" $
  case parseTE "gzip, trailers, deflate;q=0.5" of
    Right (TE.TE codings) ->
      map (\c -> (TE.teCodingName c, TE.teCodingWeight c)) (NE.toList codings)
        `shouldBe` [ (ST.fromString "gzip", Nothing)
                   , (ST.fromString "trailers", Nothing)
                   , (ST.fromString "deflate", Just 0.5)
                   ]
    other -> error (show other)


unit_te_render :: Spec
unit_te_render =
  it "renders TE" $
    let v = TE.TE (TE.TECoding (ST.fromString "trailers") Nothing :| [TE.TECoding (ST.fromString "gzip") (Just 0.5)])
    in M.toStrictByteString (TE.renderTE v) `shouldBe` "trailers, gzip;q=0.5"


-- Trailer -----------------------------------------------------------------

unit_trailer_parse :: Spec
unit_trailer_parse = it "parses a field-name list" $
  case parseTrailer "Expires, Content-MD5" of
    Right (Trailer.Trailer names) ->
      NE.toList names `shouldBe` [ST.fromString "Expires", ST.fromString "Content-MD5"]
    other -> error (show other)


unit_trailer_render :: Spec
unit_trailer_render =
  it "renders Trailer" $
    let v = Trailer.Trailer (ST.fromString "Expires" :| [ST.fromString "X-Checksum"])
    in M.toStrictByteString (Trailer.renderTrailer v) `shouldBe` "Expires, X-Checksum"


-- Upgrade -----------------------------------------------------------------

unit_upgrade_parse :: Spec
unit_upgrade_parse = it "parses a protocol list" $
  case parseUpgrade "HTTP/2.0, websocket" of
    Right (Upgrade.Upgrade protos) ->
      map (\p -> (Upgrade.protocolName p, Upgrade.protocolVersion p)) (NE.toList protos)
        `shouldBe` [ (ST.fromString "HTTP", Just (ST.fromString "2.0"))
                   , (ST.fromString "websocket", Nothing)
                   ]
    other -> error (show other)


unit_upgrade_render :: Spec
unit_upgrade_render =
  it "renders Upgrade" $
    let v =
          Upgrade.Upgrade
            ( Upgrade.Protocol (ST.fromString "h2c") Nothing
                :| [Upgrade.Protocol (ST.fromString "HTTP") (Just (ST.fromString "1.1"))]
            )
    in M.toStrictByteString (Upgrade.renderUpgrade v) `shouldBe` "h2c, HTTP/1.1"


-- Via ---------------------------------------------------------------------

unit_via_parse :: Spec
unit_via_parse = it "parses hops with optional comment" $
  case parseVia "1.1 vegur, HTTP/1.1 proxy.example.com:8080 (test-proxy)" of
    Right (Via.Via entries) -> case NE.toList entries of
      [e1, e2] -> do
        Via.viaReceivedProtocol e1 `shouldBe` Via.ReceivedProtocol Nothing (ST.fromString "1.1")
        Via.viaReceivedBy e1 `shouldBe` ST.fromString "vegur"
        Via.viaComment e1 `shouldBe` Nothing
        Via.viaReceivedProtocol e2 `shouldBe` Via.ReceivedProtocol (Just (ST.fromString "HTTP")) (ST.fromString "1.1")
        Via.viaReceivedBy e2 `shouldBe` ST.fromString "proxy.example.com:8080"
        Via.viaComment e2 `shouldBe` Just (T.pack "test-proxy")
      other -> error (show other)
    other -> error (show other)


unit_via_render :: Spec
unit_via_render =
  it "renders Via" $
    let v =
          Via.Via
            ( Via.ViaEntry
                (Via.ReceivedProtocol (Just (ST.fromString "HTTP")) (ST.fromString "1.1"))
                (ST.fromString "proxy.example.com")
                (Just (T.pack "fast"))
                :| []
            )
    in M.toStrictByteString (Via.renderVia v) `shouldBe` "HTTP/1.1 proxy.example.com (fast)"


-- Max-Forwards ------------------------------------------------------------

unit_maxforwards_parse :: Spec
unit_maxforwards_parse =
  it "parses an integer" $
    parseMaxForwards "10" `shouldBe` Right (MaxForwards.MaxForwards 10)


unit_maxforwards_render :: Spec
unit_maxforwards_render =
  it "renders Max-Forwards" $
    M.toStrictByteString (MaxForwards.renderMaxForwards (MaxForwards.MaxForwards 0)) `shouldBe` "0"


-- Close -------------------------------------------------------------------

unit_close_parse :: Spec
unit_close_parse = it "parses the close token case-insensitively" $ do
  parseClose "close" `shouldBe` Right Close.Close
  parseClose "Close" `shouldBe` Right Close.Close


unit_close_render :: Spec
unit_close_render =
  it "renders close" $
    M.toStrictByteString (Close.renderClose Close.Close) `shouldBe` "close"


-- Suite -------------------------------------------------------------------

tests :: Spec
tests =
  describe "Framing" $
    sequence_
      [ unit_te_parse
      , unit_te_render
      , it "TE round-trip" (roundtrip genTE parseTE TE.renderTE)
      , unit_trailer_parse
      , unit_trailer_render
      , it "Trailer round-trip" (roundtrip genTrailer parseTrailer Trailer.renderTrailer)
      , unit_upgrade_parse
      , unit_upgrade_render
      , it "Upgrade round-trip" (roundtrip genUpgrade parseUpgrade Upgrade.renderUpgrade)
      , unit_via_parse
      , unit_via_render
      , it "Via round-trip" (roundtrip genVia parseVia Via.renderVia)
      , unit_maxforwards_parse
      , unit_maxforwards_render
      , it "Max-Forwards round-trip" (roundtrip genMaxForwards parseMaxForwards MaxForwards.renderMaxForwards)
      , unit_close_parse
      , unit_close_render
      ]
