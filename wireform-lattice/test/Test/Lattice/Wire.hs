{- | Wire-format contracts (spec §9): every record constructor round-trips
through its pinned JSON shape, scope encoding uses the bare-ref shorthand
exactly for entity scopes, unknown kinds and tags decode tolerantly
(§9.4.1), page values follow §3.6, and the corpus entry-11 body decodes
verbatim.
-}
module Test.Lattice.Wire (tests) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Lattice.Types (Ref (..), SliceName (..))
import Lattice.Wire
import Test.Syd


tests :: Spec
tests =
  describe "Wire format (§9)" $ do
    describe "§9.1/§9.2 every record kind round-trips" $ do
      it "decode . encode = id for each constructor" $
        traverse_ roundTrips sampleRecords
      it "the NDJSON framing round-trips a whole stream (§9.1)" $ do
        let decoded = decodeRecords (encodeRecords sampleRecords)
        decoded `shouldBe` map Right sampleRecords

    describe "§9.4.1 scope encoding" $ do
      it "entity scopes use the bare-ref shorthand" $
        scopeJson (ScopeEntity (Ref "Human" "1002"))
          `shouldBe` A.String "Human:1002"
      it "field/edge/root/item scopes are tagged objects" $ do
        scopeJson (ScopeField (Ref "Post" "17") "title")
          `shouldBe` A.object ["$tag" A..= t "Field", "entity" A..= t "Post:17", "field" A..= t "title"]
        scopeJson (ScopeEdge (Ref "Post" "17") "comments")
          `shouldBe` A.object ["$tag" A..= t "Edge", "entity" A..= t "Post:17", "field" A..= t "comments"]
        scopeJson (ScopeRoot "feed")
          `shouldBe` A.object ["$tag" A..= t "Root", "root" A..= t "feed"]
        scopeJson (ScopeItem "ord_b")
          `shouldBe` A.object ["$tag" A..= t "Item", "item" A..= t "ord_b"]
      it "an unknown $tag decodes to ScopeUnknown and re-encodes verbatim" $ do
        let raw = "{\"kind\":\"error\",\"scope\":{\"$tag\":\"Wormhole\",\"x\":1},\"retryable\":false}"
        case A.eitherDecodeStrict raw of
          Right (RError e) -> do
            errScope e
              `shouldBe` Just (ScopeUnknown "Wormhole" (A.object ["x" A..= (1 :: Int)]))
            -- Tolerated, not destroyed: re-encoding restores the tag.
            fmap scopeJson (errScope e)
              `shouldBe` Just (A.object ["$tag" A..= t "Wormhole", "x" A..= (1 :: Int)])
          other -> expectationFailure ("expected an error record, got " <> show other)

    describe "§9.4.1 unknown record kinds are tolerated" $ do
      it "an unknown kind decodes to RUnknown keeping its payload" $
        A.eitherDecodeStrict "{\"kind\":\"hologram\",\"x\":1}"
          `shouldBe` Right (RUnknown "hologram" (KM.fromList ["x" A..= (1 :: Int)]))
      it "RUnknown re-encodes with its kind reinserted (round-trip)" $
        roundTrips (RUnknown "hologram" (KM.fromList ["x" A..= (1 :: Int)]))
      it "decodeRecords skips blank lines and Lefts undecodable ones" $
        decodeRecords "garbage\n\n{\"kind\":\"end\",\"complete\":true}\n"
          `shouldBe` [Left "garbage", Right (REnd (EndRecord True Nothing))]

    describe "§3.6 page values" $ do
      it "pageToJSON/pageFromJSON round-trip (with and without total)" $ do
        let full = PageValue [Ref "Human" "1002", Ref "Human" "1003"] (Just "cur_a91") (Just "cur_7c3") (Just 42)
            bare = PageValue [Ref "Human" "1002"] Nothing Nothing Nothing
        pageFromJSON (pageToJSON full) `shouldBe` Just full
        pageFromJSON (pageToJSON bare) `shouldBe` Just bare
      it "a plain ref array is not a page (bounded collections never wrap)" $
        pageFromJSON (A.toJSON [t "Tag:1", t "Tag:2"]) `shouldBe` Nothing
      it "a page with only items decodes (next/prev omitted on the wire)" $
        pageFromJSON (A.object ["$page" A..= A.object ["items" A..= [refValue (Ref "Human" "1000")]]])
          `shouldBe` Just (PageValue [Ref "Human" "1000"] Nothing Nothing Nothing)

    describe "§9 corpus entry 11: partial failure body decodes verbatim" $ do
      it "all six records decode to their expected values" $
        decodeRecords corpus11Body `shouldBe` map Right corpus11Records


-- ---------------------------------------------------------------------------
-- Samples: one of every constructor, exercising optional fields both ways
-- ---------------------------------------------------------------------------

sampleRecords :: [Record]
sampleRecords =
  [ RManifest
      Manifest
        { mQuery = Just "8f2c41a9"
        , mMutation = Nothing
        , mPlan = Just "pl_9dK2"
        , mSlice = Just SliceCtx
        , mRoot = Map.singleton "feed" [Ref "Post" "17", Ref "Post" "18"]
        , mEtag = "m:abc"
        , mBatch = Nothing
        }
  , RManifest
      Manifest
        { mQuery = Nothing
        , mMutation = Just "cancelOrder"
        , mPlan = Nothing
        , mSlice = Nothing
        , mRoot = Map.singleton "items" []
        , mEtag = "m:c9f1"
        , mBatch = Just (BatchInfo "best-effort" 3)
        }
  , REntity
      EntityRecord
        { erId = Ref "Post" "17"
        , erVer = "e41"
        , erFields =
            Map.fromList
              [ ("title", A.String "Hello")
              , ("author", refValue (Ref "User" "9"))
              ,
                ( "comments(first:20)"
                , pageToJSON (PageValue [Ref "Comment" "1"] (Just "cur_x") Nothing Nothing)
                )
              ]
        , erItem = Just "ord_a"
        , erSrc = Just "posts"
        }
  , RTombstone (Ref "Post" "17") "t:99" (Just "ord_b")
  , RTombstone (Ref "Post" "17") "t:99" Nothing
  , RElided (Ref "Post" "17")
  , RUnchanged (Ref "Post" "17") "e41"
  , RError
      ErrorRecord
        { errScope = Just (ScopeEntity (Ref "Human" "1002"))
        , errCode = Just "lattice:loader-timeout"
        , errDomain = Nothing
        , errRetryable = True
        , errMessage = Nothing
        }
  , RError
      ErrorRecord
        { errScope = Just (ScopeItem "ord_b")
        , errCode = Nothing
        , errDomain = Just (A.object ["$tag" A..= t "AlreadyFilled"])
        , errRetryable = False
        , errMessage = Just "order already filled"
        }
  , RError
      ErrorRecord
        { errScope = Nothing
        , errCode = Just "lattice:internal"
        , errDomain = Nothing
        , errRetryable = False
        , errMessage = Nothing
        }
  , RInvalidated ["Post:17", "feed:123"] (Just "ord_a")
  , RInvalidated ["Post:17"] Nothing
  , REnd (EndRecord True (Just "m:abc"))
  , REnd (EndRecord False Nothing)
  , RPlan
      PlanRecord
        { prQuery = "8f2c41a9"
        , prPlan = "pl_9dK2"
        , prSlices = Map.singleton SliceCtx (SliceInfo ["org"] ["feed"])
        }
  , RReauth
  ]


roundTrips :: Record -> IO ()
roundTrips r = A.eitherDecodeStrict (encodeRecord r) `shouldBe` Right r


scopeJson :: Scope -> A.Value
scopeJson = A.toJSON


t :: String -> A.Value
t = A.toJSON


-- ---------------------------------------------------------------------------
-- Corpus entry 11 (the entity record is one NDJSON line on the wire; the
-- corpus wraps it for display)
-- ---------------------------------------------------------------------------

corpus11Body :: ByteString
corpus11Body =
  BS8.unlines
    [ "{\"kind\":\"manifest\",\"query\":\"...\",\"plan\":\"...\",\"slice\":\"pub\",\"root\":{\"hero\":[\"Droid:2001\"]},\"etag\":\"m:...\"}"
    , "{\"kind\":\"entity\",\"id\":\"Droid:2001\",\"ver\":\"f10\",\"fields\":{\"name\":\"R2-D2\",\"friends(first:3)\":{\"$page\":{\"items\":[{\"$ref\":\"Human:1000\"},{\"$ref\":\"Human:1002\"},{\"$ref\":\"Human:1003\"}]}}}}"
    , "{\"kind\":\"entity\",\"id\":\"Human:1000\",\"ver\":\"a01\",\"fields\":{\"name\":\"Luke Skywalker\"}}"
    , "{\"kind\":\"error\",\"scope\":\"Human:1002\",\"code\":\"lattice:loader-timeout\",\"retryable\":true}"
    , "{\"kind\":\"entity\",\"id\":\"Human:1003\",\"ver\":\"c19\",\"fields\":{\"name\":\"Leia Organa\"}}"
    , "{\"kind\":\"end\",\"complete\":true}"
    ]


corpus11Records :: [Record]
corpus11Records =
  [ RManifest
      Manifest
        { mQuery = Just "..."
        , mMutation = Nothing
        , mPlan = Just "..."
        , mSlice = Just SlicePub
        , mRoot = Map.singleton "hero" [Ref "Droid" "2001"]
        , mEtag = "m:..."
        , mBatch = Nothing
        }
  , REntity
      EntityRecord
        { erId = Ref "Droid" "2001"
        , erVer = "f10"
        , erFields =
            Map.fromList
              [ ("name", A.String "R2-D2")
              ,
                ( "friends(first:3)"
                , A.object
                    [ "$page"
                        A..= A.object
                          [ "items"
                              A..= [ refValue (Ref "Human" "1000")
                                   , refValue (Ref "Human" "1002")
                                   , refValue (Ref "Human" "1003")
                                   ]
                          ]
                    ]
                )
              ]
        , erItem = Nothing
        , erSrc = Nothing
        }
  , REntity
      EntityRecord
        { erId = Ref "Human" "1000"
        , erVer = "a01"
        , erFields = Map.singleton "name" (A.String "Luke Skywalker")
        , erItem = Nothing
        , erSrc = Nothing
        }
  , RError
      ErrorRecord
        { errScope = Just (ScopeEntity (Ref "Human" "1002"))
        , errCode = Just "lattice:loader-timeout"
        , errDomain = Nothing
        , errRetryable = True
        , errMessage = Nothing
        }
  , REntity
      EntityRecord
        { erId = Ref "Human" "1003"
        , erVer = "c19"
        , erFields = Map.singleton "name" (A.String "Leia Organa")
        , erItem = Nothing
        , erSrc = Nothing
        }
  , REnd (EndRecord True Nothing)
  ]
