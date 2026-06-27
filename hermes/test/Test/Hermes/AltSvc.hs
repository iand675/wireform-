{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AltSvc (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AltSvc as AS
import qualified Network.HTTP.Headers.AltUsed as AU
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseAS :: ByteString -> Either String AS.AltSvc
parseAS bs = case runParser AS.altSvcParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseAU :: ByteString -> Either String AU.AltUsed
parseAU bs = case runParser AU.altUsedParser bs of
  OK v leftover
    | BS.null (dropOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderAS :: AS.AltSvc -> ByteString
renderAS = M.toStrictByteString . AS.renderAltSvc


renderAU :: AU.AltUsed -> ByteString
renderAU = M.toStrictByteString . AU.renderAltUsed


-- ---------------------------------------------------------------------------
-- Alt-Svc unit tests
-- ---------------------------------------------------------------------------

unit_clear :: Spec
unit_clear = it "parses and renders the clear sentinel" $ do
  case parseAS "clear" of
    Right AS.AltSvcClear -> pure () :: IO ()
    other -> error (show other)
  renderAS AS.AltSvcClear `shouldBe` "clear"


unit_value :: Spec
unit_value = it "parses a single alternative with an ma parameter" $
  case parseAS "h2=\":443\"; ma=3600" of
    Right (AS.AltSvcValues (v :| [])) -> do
      AS.altProtocolId v `shouldBe` ST.fromString "h2"
      AS.altAuthority v `shouldBe` AS.AltAuthority Nothing 443
      AS.altParameters v
        `shouldBe` [ AS.AltParameter
                      (ST.fromString "ma")
                      (AS.AltParamToken (ST.fromString "3600"))
                   ]
    other -> error (show other)


unit_value_list :: Spec
unit_value_list = it "parses a comma list with host and quoted value" $
  case parseAS "h3=\"alt.example.net:443\"; persist=\"1\", h2=\":8443\"" of
    Right (AS.AltSvcValues (a :| [b])) -> do
      AS.altProtocolId a `shouldBe` ST.fromString "h3"
      AS.altAuthority a `shouldBe` AS.AltAuthority (Just (ST.fromString "alt.example.net")) 443
      AS.altParameters a
        `shouldBe` [ AS.AltParameter
                      (ST.fromString "persist")
                      (AS.AltParamQuoted (ST.fromString "1"))
                   ]
      AS.altProtocolId b `shouldBe` ST.fromString "h2"
      AS.altAuthority b `shouldBe` AS.AltAuthority Nothing 8443
    other -> error (show other)


unit_render_value :: Spec
unit_render_value =
  it "renders an alternative list" $
    let v =
          AS.AltSvcValues
            ( AS.AltValue
                (ST.fromString "h3")
                (AS.AltAuthority (Just (ST.fromString "alt.example.net")) 443)
                [ AS.AltParameter
                    (ST.fromString "ma")
                    (AS.AltParamToken (ST.fromString "2592000"))
                ]
                :| [ AS.AltValue
                      (ST.fromString "h2")
                      (AS.AltAuthority Nothing 443)
                      []
                   ]
            )
    in renderAS v `shouldBe` "h3=\"alt.example.net:443\"; ma=2592000, h2=\":443\""


-- ---------------------------------------------------------------------------
-- Alt-Used unit tests
-- ---------------------------------------------------------------------------

unit_altused :: Spec
unit_altused = it "parses and renders host:port" $ do
  case parseAU "alt.example.com:443" of
    Right (AU.AltUsed h (Just 443)) -> h `shouldBe` ST.fromString "alt.example.com"
    other -> error (show other)
  renderAU (AU.AltUsed (ST.fromString "alt.example.com") (Just 443))
    `shouldBe` "alt.example.com:443"


unit_altused_noport :: Spec
unit_altused_noport = it "parses a bare host (no port)" $ do
  case parseAU "example.com" of
    Right (AU.AltUsed h Nothing) -> h `shouldBe` ST.fromString "example.com"
    other -> error (show other)
  renderAU (AU.AltUsed (ST.fromString "example.com") Nothing) `shouldBe` "example.com"


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genToken :: Gen ST.ShortText
genToken =
  ST.fromString
    <$> Gen.list (Range.linear 1 8) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))


genHost :: Gen ST.ShortText
genHost =
  ST.fromString
    <$> Gen.list (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-."))


genQuoted :: Gen ST.ShortText
genQuoted =
  ST.fromString
    <$> Gen.list (Range.linear 0 8) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> " -._/"))


genParamValue :: Gen AS.AltParamValue
genParamValue =
  Gen.choice
    [ AS.AltParamToken <$> genToken
    , AS.AltParamQuoted <$> genQuoted
    ]


genParam :: Gen AS.AltParameter
genParam = AS.AltParameter <$> genToken <*> genParamValue


genAuthority :: Gen AS.AltAuthority
genAuthority =
  AS.AltAuthority
    <$> Gen.maybe genHost
    <*> Gen.word16 (Range.linear 0 65535)


genAltValue :: Gen AS.AltValue
genAltValue =
  AS.AltValue
    <$> genToken
    <*> genAuthority
    <*> Gen.list (Range.linear 0 3) genParam


genAltSvc :: Gen AS.AltSvc
genAltSvc =
  Gen.choice
    [ pure AS.AltSvcClear
    , do
        x <- genAltValue
        xs <- Gen.list (Range.linear 0 2) genAltValue
        pure (AS.AltSvcValues (x :| xs))
    ]


genAltUsed :: Gen AU.AltUsed
genAltUsed =
  AU.AltUsed
    <$> genHost
    <*> Gen.maybe (Gen.word16 (Range.linear 0 65535))


-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

prop_altSvc_roundtrip :: Property
prop_altSvc_roundtrip = property $ do
  v <- forAll genAltSvc
  let bs = renderAS v
  case parseAS bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


prop_altUsed_roundtrip :: Property
prop_altUsed_roundtrip = property $ do
  v <- forAll genAltUsed
  let bs = renderAU v
  case parseAU bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "AltSvc" $
    sequence_
      [ unit_clear
      , unit_value
      , unit_value_list
      , unit_render_value
      , unit_altused
      , unit_altused_noport
      , it "Alt-Svc round-trips" prop_altSvc_roundtrip
      , it "Alt-Used round-trips" prop_altUsed_roundtrip
      ]
