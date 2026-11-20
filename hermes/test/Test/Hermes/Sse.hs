{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Sse (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
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
      (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_:.@ ,\tπ東京🌍"))


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
      , it "preserves opaque UTF-8, including commas and spaces, through typed lookup" $ do
          let wire = " caf\195\169-\240\159\140\141, next\t"
              value = LEI.LastEventID " café-🌍, next\t"
              fields = headerMapFromList [("Last-Event-ID", wire)]
          lookupHeader @LEI.LastEventID fields `shouldBe` Right (Just value)
          lookupRawHeader "Last-Event-ID" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
          parseLastEventID "" `shouldBe` Right (LEI.LastEventID "")
      , it "rejects invalid UTF-8, NUL, CR, LF, and repeated Last-Event-ID field lines" $ do
          for_ ["a\0b", "a\rb", "a\nb", "\128", "\195", "\192\128", "\237\160\128", "\244\144\128\128", "\195\169\NUL"] $ \wire -> do
            parseLastEventID wire `shouldSatisfy` isLeft
            lookupHeader @LEI.LastEventID (headerMapFromList [("Last-Event-ID", wire)]) `shouldSatisfy` isLeft
          lookupHeader @LEI.LastEventID (headerMapFromList [("Last-Event-ID", "a"), ("Last-Event-ID", "b")])
            `shouldSatisfy` isLeft
      , it "Last-Event-ID round-trip" prop_last_event_id_roundtrip
      , unit_refresh_seconds_only
      , unit_refresh_with_url
      , unit_refresh_render
      , unit_refresh_render_seconds_only
      , it "extracts HTML refresh delay and target across separator and quote forms" $
          for_
            [ ("\t5.99..15 , UrL \t= '/next,a;b' ignored", R.Refresh 5 (Just "/next,a;b"))
            , (".25; ../next", R.Refresh 0 (Just "../next"))
            , ("6 \turl.html", R.Refresh 6 (Just "url.html"))
            , ("0; URL /next", R.Refresh 0 (Just "URL /next"))
            , ("2; URL=", R.Refresh 2 (Just ""))
            , ("3 ;\t", R.Refresh 3 Nothing)
            , ("0; url=\"/unclosed", R.Refresh 0 (Just "/unclosed"))
            ]
            ( \(wire, expected) -> do
                parseRefresh wire `shouldBe` Right expected
                lookupHeader @R.Refresh (headerMapFromList [("Refresh", wire)]) `shouldBe` Right (Just expected)
                parseRefresh (renderRefresh expected) `shouldBe` Right expected
            )
      , it "keeps the first field line and preserves isomorphic target bytes" $ do
          let value = R.Refresh 0 (Just "/café")
              wire = "0; url=/caf\233"
          lookupHeader @R.Refresh (headerMapFromList [("Refresh", wire), ("Refresh", "1")])
            `shouldBe` Right (Just value)
          lookupRawHeader "Refresh" (setHeader value (headerMapFromList [])) `shouldBe` Just (wire :| [])
      , it "quotes target text that would otherwise be interpreted as refresh syntax" $ do
          parseRefresh "0;url='\"relative'" `shouldBe` Right (R.Refresh 0 (Just "\"relative"))
          renderRefresh (R.Refresh 0 (Just " /relative")) `shouldBe` "0; url=\" /relative\""
          parseRefresh (renderRefresh (R.Refresh 0 (Just "'relative"))) `shouldBe` Right (R.Refresh 0 (Just "'relative"))
      , it "bounds the whole-second delay without wrapping or accepting a missing separator" $ do
          let limit = BSC.pack (show (maxBound :: Word))
          parseRefresh (limit <> ".23") `shouldBe` Right (R.Refresh maxBound Nothing)
          for_ ["", " ", "-1", "+1", "1e1", "1.5x;url=/next", limit <> "0"] $ \wire ->
            parseRefresh wire `shouldSatisfy` isLeft
      , it "rejects control bytes anywhere in the HTTP field value" $
          for_ ["\r0", "0\n;url=/next", "0;url=/ne\0xt", "0;url='/next'\DEL"] $ \wire -> do
            parseRefresh wire `shouldSatisfy` isLeft
            lookupHeader @R.Refresh (headerMapFromList [("Refresh", wire)]) `shouldSatisfy` isLeft
      , it "Refresh round-trip" prop_refresh_roundtrip
      , it "protects target whitespace and quotes when writing a Refresh field" $ do
          let trailing = R.Refresh 0 (Just "/next \t")
              mixed = R.Refresh 0 (Just " '/both\"quotes")
              renderTyped :: R.Refresh -> Maybe (NonEmpty ByteString)
              renderTyped value = lookupRawHeader "Refresh" (setHeader value (headerMapFromList []))
          renderTyped trailing `shouldBe` Just ("0; url=\"/next \t\"" :| [])
          parseRefresh (renderRefresh trailing) `shouldBe` Right trailing
          renderTyped mixed `shouldBe` Just ("0; url=\" '/both%22quotes\"" :| [])
          parseRefresh (renderRefresh mixed) `shouldBe` Right (R.Refresh 0 (Just " '/both%22quotes"))
      ]
