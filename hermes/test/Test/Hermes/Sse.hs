{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Sse (tests) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.LastEventID as LEI
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Refresh as R
import Test.Syd
import Test.Syd.Hedgehog ()


-- Last-Event-ID --------------------------------------------------------------

parseLastEventID :: ByteString -> Either String LEI.LastEventID
parseLastEventID bs = case runParser LEI.lastEventIDParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderLastEventID :: LEI.LastEventID -> ByteString
renderLastEventID = M.toStrictByteString . LEI.renderLastEventID


unit_last_event_id_parse :: Spec
unit_last_event_id_parse = it "parses an opaque event id verbatim" $
  case parseLastEventID "evt-42:checkpoint" of
    Right (LEI.LastEventID i) -> i `shouldBe` ST.fromString "evt-42:checkpoint"
    other -> error (show other)


unit_last_event_id_render :: Spec
unit_last_event_id_render =
  it "renders an event id verbatim" $
    renderLastEventID (LEI.LastEventID (ST.fromString "abc 123"))
      `shouldBe` "abc 123"


idGen :: Gen ST.ShortText
idGen =
  ST.fromString
    <$> Gen.list
      (Range.linear 0 32)
      (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_:.@ "))


prop_last_event_id_roundtrip :: Property
prop_last_event_id_roundtrip = property $ do
  i <- forAll idGen
  let v = LEI.LastEventID i
      bs = renderLastEventID v
  case parseLastEventID bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Refresh ---------------------------------------------------------------------

parseRefresh :: ByteString -> Either String R.Refresh
parseRefresh bs = case runParser R.refreshParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderRefresh :: R.Refresh -> ByteString
renderRefresh = M.toStrictByteString . R.renderRefresh


unit_refresh_seconds_only :: Spec
unit_refresh_seconds_only = it "parses a bare delta-seconds" $
  case parseRefresh "5" of
    Right (R.Refresh 5 Nothing) -> pure () :: IO ()
    other -> error (show other)


unit_refresh_with_url :: Spec
unit_refresh_with_url = it "parses delta-seconds with a url" $
  case parseRefresh "0; url=https://example.com/" of
    Right (R.Refresh 0 (Just u)) ->
      u `shouldBe` ST.fromString "https://example.com/"
    other -> error (show other)


unit_refresh_render :: Spec
unit_refresh_render =
  it "renders delta-seconds with a url" $
    renderRefresh (R.Refresh 10 (Just (ST.fromString "https://example.com/next")))
      `shouldBe` "10; url=https://example.com/next"


unit_refresh_render_seconds_only :: Spec
unit_refresh_render_seconds_only =
  it "renders a bare delta-seconds" $
    renderRefresh (R.Refresh 30 Nothing) `shouldBe` "30"


urlGen :: Gen ST.ShortText
urlGen =
  ST.fromString
    <$> Gen.list
      (Range.linear 1 48)
      (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ":/.?=&-_~"))


refreshGen :: Gen R.Refresh
refreshGen =
  R.Refresh
    <$> Gen.word (Range.linear 0 100_000)
    <*> Gen.maybe urlGen


prop_refresh_roundtrip :: Property
prop_refresh_roundtrip = property $ do
  v <- forAll refreshGen
  let bs = renderRefresh v
  case parseRefresh bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Sse" $
    sequence_
      [ unit_last_event_id_parse
      , unit_last_event_id_render
      , it "Last-Event-ID round-trip" prop_last_event_id_roundtrip
      , unit_refresh_seconds_only
      , unit_refresh_with_url
      , unit_refresh_render
      , unit_refresh_render_seconds_only
      , it "Refresh round-trip" prop_refresh_roundtrip
      ]
