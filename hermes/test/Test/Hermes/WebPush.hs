{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.WebPush (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (HeaderMessage (..), defaultHeaderSettings, headerMapFromList, lookupHeader, lookupHeaderWith, lookupRawHeader, setHeader, withHeaderMessage)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.TTL as TTL
import qualified Network.HTTP.Headers.Topic as Topic
import qualified Network.HTTP.Headers.Urgency as Urgency
import Test.Syd
import Test.Syd.Hedgehog ()


-- TTL -----------------------------------------------------------------------

parseTTL :: ByteString -> Either String TTL.TTL
parseTTL bs = case runParser TTL.ttlParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderTTL :: TTL.TTL -> ByteString
renderTTL = M.toStrictByteString . TTL.renderTTL


unit_ttl_parse :: Spec
unit_ttl_parse =
  it "parses a delta-seconds value" $
    parseTTL "3600" `shouldBe` Right (TTL.TTL 3600)


unit_ttl_render :: Spec
unit_ttl_render =
  it "renders a delta-seconds value" $
    renderTTL (TTL.TTL 0) `shouldBe` "0"


unit_ttl_fields :: Spec
unit_ttl_fields = it "reads and writes a singleton TTL in both directions and applies the overflow value" $ do
  let headers values = headerMapFromList (map (\wire -> ("TTL", wire)) values)
      ttl = TTL.TTL 86400
      largest = TTL.TTL maxBound
      tooLarge = M.toStrictByteString (M.integerDec (toInteger (maxBound :: Word) + 1))
  forM_ [HeaderRequest, HeaderResponse] $ \direction ->
    lookupHeaderWith @TTL.TTL (withHeaderMessage direction defaultHeaderSettings) (headers [" \t00086400\t "])
      `shouldBe` Right (Just ttl)
  lookupRawHeader "TTL" (setHeader ttl (headers [])) `shouldBe` Just ("86400" :| [])
  lookupHeader @TTL.TTL (headers ["0"]) `shouldBe` Right (Just (TTL.TTL 0))
  lookupHeader @TTL.TTL (headers [BS.replicate 100 48 <> renderTTL largest]) `shouldBe` Right (Just largest)
  lookupHeader @TTL.TTL (headers [tooLarge]) `shouldBe` Right (Just (TTL.TTL 2_147_483_648))
  lookupHeader @TTL.TTL (headers [BS.replicate 4096 57 <> "\t"]) `shouldBe` Right (Just (TTL.TTL 2_147_483_648))
  forM_ [[""], ["+1"], ["-1"], ["1, 2"], ["1", "2"], [tooLarge <> "x"]] $ \bad ->
    lookupHeader @TTL.TTL (headers bad) `shouldSatisfy` isLeft


ttlGen :: Gen TTL.TTL
ttlGen = TTL.TTL <$> Gen.word (Range.linear 0 maxBound)


prop_ttl_roundtrip :: Property
prop_ttl_roundtrip = property $ do
  v <- forAll ttlGen
  let bs = renderTTL v
  case parseTTL bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Topic ---------------------------------------------------------------------

parseTopic :: ByteString -> Either String Topic.Topic
parseTopic bs = case runParser Topic.topicParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderTopic :: Topic.Topic -> ByteString
renderTopic = M.toStrictByteString . Topic.renderTopic


unit_topic_parse :: Spec
unit_topic_parse =
  it "parses a url-safe base64 topic" $
    parseTopic "upd-42_v1" `shouldBe` Right (Topic.Topic (ST.fromString "upd-42_v1"))


unit_topic_render :: Spec
unit_topic_render =
  it "renders a topic verbatim" $
    renderTopic (Topic.Topic (ST.fromString "abcXYZ09-_")) `shouldBe` "abcXYZ09-_"


unit_topic_fields :: Spec
unit_topic_fields = it "requires 1–32 URL-safe characters and preserves the case of a single Topic" $ do
  let headers values = headerMapFromList (map (\wire -> ("Topic", wire)) values)
      spelling = "ABCDEFGHIJKLMNOPQRSTUVWXYZ09-ab_"
      longest = Topic.Topic "ABCDEFGHIJKLMNOPQRSTUVWXYZ09-ab_"
  lookupHeader @Topic.Topic (headers ["A"]) `shouldBe` Right (Just (Topic.Topic "A"))
  lookupHeader @Topic.Topic (headers ["_N"]) `shouldBe` Right (Just (Topic.Topic "_N"))
  lookupHeader @Topic.Topic (headers [" \t" <> spelling <> "\t "]) `shouldBe` Right (Just longest)
  lookupRawHeader "Topic" (setHeader longest (headers [])) `shouldBe` Just (spelling :| [])
  forM_ [[""], ["topic="], ["topic+"], ["topic/"], ["ab cd"], ["a,b"], ["a", "b"], [spelling <> "X"]] $ \bad ->
    lookupHeader @Topic.Topic (headers bad) `shouldSatisfy` isLeft


topicCharGen :: Gen Char
topicCharGen = Gen.element (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_")


topicGen :: Gen Topic.Topic
topicGen = Topic.Topic . ST.fromString <$> Gen.list (Range.linear 1 32) topicCharGen


prop_topic_roundtrip :: Property
prop_topic_roundtrip = property $ do
  v <- forAll topicGen
  let bs = renderTopic v
  case parseTopic bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Urgency -------------------------------------------------------------------

parseUrgency :: ByteString -> Either String Urgency.Urgency
parseUrgency bs = case runParser Urgency.urgencyParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderUrgency :: Urgency.Urgency -> ByteString
renderUrgency = M.toStrictByteString . Urgency.renderUrgency


unit_urgency_parse :: Spec
unit_urgency_parse = it "parses the urgency enum values" $ do
  parseUrgency "very-low" `shouldBe` Right Urgency.VeryLow
  parseUrgency "high" `shouldBe` Right Urgency.High


unit_urgency_render :: Spec
unit_urgency_render = it "renders the urgency enum values" $ do
  renderUrgency Urgency.VeryLow `shouldBe` "very-low"
  renderUrgency Urgency.Normal `shouldBe` "normal"


unit_urgency_fields :: Spec
unit_urgency_fields = it "folds ASCII case for a single Urgency and leaves absence to the caller" $ do
  let headers values = headerMapFromList (map (\wire -> ("Urgency", wire)) values)
  lookupHeader @Urgency.Urgency (headers []) `shouldBe` Right Nothing
  forM_ [("VeRy-LoW", Urgency.VeryLow), ("LOW", Urgency.Low), ("nOrMaL", Urgency.Normal), ("hiGH", Urgency.High)] $ \(wire, value) -> do
    lookupHeader @Urgency.Urgency (headers [" \t" <> wire <> "\t"]) `shouldBe` Right (Just value)
    lookupRawHeader "Urgency" (setHeader value (headers [])) `shouldBe` Just (renderUrgency value :| [])
  forM_ [[""], ["very-high"], ["normalish"], ["low, high"], ["low", "high"], ["low", "low"]] $ \bad ->
    lookupHeader @Urgency.Urgency (headers bad) `shouldSatisfy` isLeft


urgencyGen :: Gen Urgency.Urgency
urgencyGen = Gen.element [minBound .. maxBound]


prop_urgency_roundtrip :: Property
prop_urgency_roundtrip = property $ do
  v <- forAll urgencyGen
  let bs = renderUrgency v
  case parseUrgency bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "WebPush" $
    sequence_
      [ unit_ttl_parse
      , unit_ttl_render
      , unit_ttl_fields
      , it "TTL round-trip" prop_ttl_roundtrip
      , unit_topic_parse
      , unit_topic_render
      , unit_topic_fields
      , it "Topic round-trip" prop_topic_roundtrip
      , unit_urgency_parse
      , unit_urgency_render
      , unit_urgency_fields
      , it "Urgency round-trip" prop_urgency_roundtrip
      ]
