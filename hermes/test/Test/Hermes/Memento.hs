{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Memento (tests) where

import Data.ByteString (ByteString)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AcceptDatetime as AD
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.MementoDatetime as MD
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers ------------------------------------------------------------------

mkTime :: Integer -> Int -> Int -> Integer -> UTCTime
mkTime year month day secs =
  UTCTime (fromGregorian year month day) (secondsToDiffTime secs)


parseAccept :: ByteString -> Either String AD.AcceptDatetime
parseAccept bs = case runParser AD.acceptDatetimeParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderAccept :: AD.AcceptDatetime -> ByteString
renderAccept = M.toStrictByteString . AD.renderAcceptDatetime


parseMemento :: ByteString -> Either String MD.MementoDatetime
parseMemento bs = case runParser MD.mementoDatetimeParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderMemento :: MD.MementoDatetime -> ByteString
renderMemento = M.toStrictByteString . MD.renderMementoDatetime


-- A canonical IMF-fixdate-producing instant (whole seconds, 4-digit year).
utcTimeGen :: Gen UTCTime
utcTimeGen = do
  year <- Gen.integral (Range.linear 1000 9999) :: Gen Integer
  month <- Gen.integral (Range.linear 1 12) :: Gen Int
  day <- Gen.integral (Range.linear 1 28) :: Gen Int
  secs <- Gen.integral (Range.linear 0 86399) :: Gen Integer
  pure (mkTime year month day secs)


-- Accept-Datetime ----------------------------------------------------------

unit_accept_parse :: Spec
unit_accept_parse = it "parses Accept-Datetime HTTP-date" $
  case parseAccept "Sun, 06 Nov 1994 08:49:37 GMT" of
    Right (AD.AcceptDatetime t) -> t `shouldBe` mkTime 1994 11 6 31777
    other -> error (show other)


unit_accept_render :: Spec
unit_accept_render =
  it "renders Accept-Datetime as IMF-fixdate" $
    renderAccept (AD.AcceptDatetime (mkTime 1994 11 6 31777))
      `shouldBe` "Sun, 06 Nov 1994 08:49:37 GMT"


prop_accept_roundtrip :: Property
prop_accept_roundtrip = property $ do
  t <- forAll utcTimeGen
  let v = AD.AcceptDatetime t
  case parseAccept (renderAccept v) of
    Right v' -> v === v'
    Left err -> error err


-- Memento-Datetime ---------------------------------------------------------

unit_memento_parse :: Spec
unit_memento_parse = it "parses Memento-Datetime HTTP-date" $
  case parseMemento "Tue, 15 Jun 2021 12:30:00 GMT" of
    Right (MD.MementoDatetime t) -> t `shouldBe` mkTime 2021 6 15 45000
    other -> error (show other)


unit_memento_render :: Spec
unit_memento_render =
  it "renders Memento-Datetime as IMF-fixdate" $
    renderMemento (MD.MementoDatetime (mkTime 2002 12 31 86399))
      `shouldBe` "Tue, 31 Dec 2002 23:59:59 GMT"


prop_memento_roundtrip :: Property
prop_memento_roundtrip = property $ do
  t <- forAll utcTimeGen
  let v = MD.MementoDatetime t
  case parseMemento (renderMemento v) of
    Right v' -> v === v'
    Left err -> error err


tests :: Spec
tests =
  describe "Memento" $
    sequence_
      [ unit_accept_parse
      , unit_accept_render
      , it "Accept-Datetime round-trip" prop_accept_roundtrip
      , unit_memento_parse
      , unit_memento_render
      , it "Memento-Datetime round-trip" prop_memento_roundtrip
      ]
