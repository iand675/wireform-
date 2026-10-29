{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-906 client-side record filter.
module Client.FilterSpec (tests) where

import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kafka.Client.Consumer qualified as C
import Kafka.Client.Filter qualified as F
import Test.Syd


tests :: Spec
tests =
  describe "ConsumerRecordFilter (KIP-906)" $
    sequence_
      [ it
          "identityFilter keeps everything"
          identity_keeps_all
      , it
          "byKeyEquals matches exactly"
          key_equals
      , it
          "byHeaderEquals matches a specific header value"
          header_equals
      , it
          "byTopicIn keeps only listed topics"
          topic_in
      , it
          "<&&> intersects two filters"
          and_filter
      , it
          "<||> unions two filters"
          or_filter
      , it
          "negateFilter inverts"
          negate_filter
      ]


mkRec :: T.Text -> T.Text -> Maybe T.Text -> [(T.Text, T.Text)] -> C.ConsumerRecord
mkRec t k v hdrs =
  C.ConsumerRecord
    { topic = t
    , partition = 0
    , offset = 0
    , timestamp = 0
    , key = fmap TE.encodeUtf8 (Just k)
    , value = TE.encodeUtf8 <$> v
    , headers = [(hk, TE.encodeUtf8 hv) | (hk, hv) <- hdrs]
    }


sample :: [C.ConsumerRecord]
sample =
  [ mkRec "a" "k1" (Just "v1") []
  , mkRec "a" "k2" (Just "v2") [("trace", "1")]
  , mkRec "b" "k3" (Just "v3") [("trace", "2")]
  ]


identity_keeps_all :: IO ()
identity_keeps_all = F.applyFilter F.identityFilter sample `shouldBe` sample


key_equals :: IO ()
key_equals =
  map (.key) (F.applyFilter (F.byKeyEquals "k2") sample)
    `shouldBe` [Just (TE.encodeUtf8 "k2")]


header_equals :: IO ()
header_equals =
  map (.key) (F.applyFilter (F.byHeaderEquals "trace" "1") sample)
    `shouldBe` [Just (TE.encodeUtf8 "k2")]


topic_in :: IO ()
topic_in =
  map (.topic) (F.applyFilter (F.byTopicIn (Set.singleton "b")) sample)
    `shouldBe` ["b"]


and_filter :: IO ()
and_filter =
  let f = F.byTopicIn (Set.singleton "a") F.<&&> F.byKeyEquals "k1"
  in map (.key) (F.applyFilter f sample) `shouldBe` [Just (TE.encodeUtf8 "k1")]


or_filter :: IO ()
or_filter =
  let f = F.byKeyEquals "k1" F.<||> F.byKeyEquals "k3"
  in map (.key) (F.applyFilter f sample)
       `shouldBe` [Just (TE.encodeUtf8 "k1"), Just (TE.encodeUtf8 "k3")]


negate_filter :: IO ()
negate_filter = do
  let f = F.negateFilter (F.byTopicIn (Set.singleton "a"))
  map (.topic) (F.applyFilter f sample) `shouldBe` ["b"]
