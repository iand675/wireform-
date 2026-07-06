{- | Cache digest contracts (spec §10.4): the GCS bit-level pin (no false
negatives, self-delimiting header round-trip, order/duplicate
insensitivity, a byte-exact cross-implementation golden), the enumerated
@X-Have@ form, the origin's header-selection policy, and record elision.

The transport-level half (a priv request with @X-Have@ receives
@unchanged@ markers, pub requests ignore the headers, the client's
advertise-and-gap-fill loop) lives in "Test.Lattice.DigestE2E".
-}
module Test.Lattice.Digest (tests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lattice.Digest
import Lattice.Types (Ref (..), TypeName)
import Lattice.Wire (EntityRecord (..), Record (..), hXHave, hXHaveDigest)
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Cache digests (§10.4)" $ do
    describe "GCS bit-level pin" $ do
      it "every inserted member is contained (no false negatives)" $
        H.withTests 200 $
          H.property $ do
            pairs <- H.forAll genPairs
            fp <- H.forAll (Gen.int (Range.linear 8 16))
            let d = DigestGcs (encodeGcs fp pairs)
            H.annotateShow d
            mapM_ (\(i, v) -> H.assert (digestContains d i v)) pairs

      it "the header value round-trips through render and parse" $
        H.withTests 200 $
          H.property $ do
            pairs <- H.forAll genPairs
            fp <- H.forAll (Gen.int (Range.linear 8 16))
            let g = encodeGcs fp pairs
            parseHaveDigest (renderHaveDigest g) H.=== Just (DigestGcs g)

      it "encoding is order- and duplicate-insensitive" $
        H.withTests 200 $
          H.property $ do
            pairs <- H.forAll genPairs
            shuffled <- H.forAll (Gen.shuffle (pairs <> pairs))
            encodeGcs 10 shuffled H.=== encodeGcs 10 pairs

      it "false positives at fp=10 stay near the declared 2^-10 rate" $ do
        let members = map (\n -> ("Post:" <> tshow n, "e" <> tshow n)) [1 .. 1000 :: Int]
            d = DigestGcs (encodeGcs 10 members)
            probes = map (\n -> ("Ghost:" <> tshow n, "v" <> tshow n)) [1 .. 4096 :: Int]
            falsePositives = length (filter (\(i, v) -> digestContains d i v) probes)
        -- 4096 probes at ~2^-10 expect ~4 hits; 40 is ten expectations
        -- away and cannot flake, while still catching a broken modulus
        -- or bit packer (which sends the rate toward 1).
        falsePositives `shouldSatisfy` (< 40)
        mapM_ (\(i, v) -> digestContains d i v `shouldBe` True) members

      it "an empty digest matches nothing and still round-trips" $ do
        let g = encodeGcs 10 []
        digestContains (DigestGcs g) "Post:1" "e1" `shouldBe` False
        parseHaveDigest (renderHaveDigest g) `shouldBe` Just (DigestGcs g)

      it "the origin accepts exactly 8 <= fp <= 16" $ do
        map acceptableFp [7, 8, 16, 17] `shouldBe` [False, True, True, False]

      it "golden header value (cross-implementation pin)" $
        pureGoldenTextFile "test/fixtures/golden/digest.headers.txt" goldenHeaders

    describe "enumerated X-Have form" $ do
      it "membership is exact and round-trips under optional whitespace" $
        H.withTests 200 $
          H.property $ do
            pairs <- H.forAll genPairs
            sep <- H.forAll (Gen.element [",", " , ", ",  ", " ,"])
            let rendered = T.intercalate sep (map (\(i, v) -> i <> "@" <> v) pairs)
            d <- H.evalMaybe (parseHave rendered)
            mapM_ (\(i, v) -> H.assert (digestContains d i v)) pairs
            -- Exactness: a ver the client never advertised is absent.
            mapM_ (\(i, v) -> H.assert (not (digestContains d i (v <> "zz")))) pairs

      it "an id may itself contain '@'; the last one separates" $ do
        d <- requireParse (parseHave "we@ird:1@e1")
        digestContains d "we@ird:1" "e1" `shouldBe` True
        digestContains d "we@ird:1@e1" "" `shouldBe` False

      it "malformed items reject as a whole; empty list elements are ignored (RFC 9110 §5.6.1)" $ do
        let bad = ["", "Post:17", "@e41", "Post:17@", " , ", "Post:17@e41, User:9"]
        mapM_ (\raw -> parseHave raw `shouldBe` Nothing) bad
        -- Empty elements between commas are the RFC-blessed tolerance,
        -- not a malformed item: the rest of the list still parses.
        parseHave "Post:17@e41,,User:9@b02" `shouldBe` parseHave "Post:17@e41,User:9@b02"

    describe "requestDigest header-selection policy" $ do
      it "prefers the enumerated form when both headers are present" $ do
        let hdrs =
              [ (hXHave, "Post:17@e41")
              , (hXHaveDigest, encodeUtf8 (renderHaveDigest (encodeGcs 10 [("User:9", "b02")])))
              ]
        requestDigest hdrs `shouldBe` parseHave "Post:17@e41"

      it "honors a GCS digest within the fp policy window" $ do
        let g = encodeGcs 10 [("Post:17", "e41")]
        requestDigest [(hXHaveDigest, encodeUtf8 (renderHaveDigest g))]
          `shouldBe` Just (DigestGcs g)

      it "ignores a GCS digest whose declared fp is out of policy" $ do
        let out fp = renderHaveDigest (encodeGcs fp [("Post:17", "e41")])
        requestDigest [(hXHaveDigest, encodeUtf8 (out 7))] `shouldBe` Nothing
        requestDigest [(hXHaveDigest, encodeUtf8 (out 17))] `shouldBe` Nothing

      it "falls to the digest form when X-Have is malformed" $ do
        let g = encodeGcs 10 [("Post:17", "e41")]
        requestDigest [(hXHave, "not a digest"), (hXHaveDigest, encodeUtf8 (renderHaveDigest g))]
          `shouldBe` Just (DigestGcs g)

      it "no headers, no digest" $
        requestDigest [] `shouldBe` Nothing

    describe "elideKnown" $ do
      it "replaces exactly the matching entity records with unchanged markers" $ do
        let known = parseHave "Post:17@e41"
            keep = entity "Post" "18" "e9"
            stale = entity "Post" "17" "e40"
            match = entity "Post" "17" "e41"
            tomb = RTombstone (Ref "Post" "19") "t:1" Nothing
        elideKnown known [match, keep, stale, tomb]
          `shouldBe` [RUnchanged (Ref "Post" "17") "e41", keep, stale, tomb]

      it "elides nothing without a digest" $ do
        let recs = [entity "Post" "17" "e41"]
        elideKnown Nothing recs `shouldBe` recs


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

genPairs :: H.Gen [(Text, Text)]
genPairs = do
  ids <- Gen.list (Range.linear 1 64) genIdVer
  pure (dedup ids)
  where
    -- Distinct ids: duplicate (id, ver) pairs are covered by the
    -- explicit insensitivity property, and distinctness makes the
    -- exactness assertions of the enumerated form sound.
    dedup = Map.toList . Map.fromList
    genIdVer = do
      ty <- Gen.element ["Post", "User", "Review"]
      key <- Gen.text (Range.linear 1 8) Gen.alphaNum
      ver <- Gen.text (Range.linear 1 6) Gen.alphaNum
      pure (ty <> ":" <> key, ver)


-- | The byte-exact §10.4 pin: fixed members, both header renderings.
-- The TypeScript client and the spec's worked example must reproduce
-- these strings verbatim.
goldenHeaders :: Text
goldenHeaders =
  T.unlines
    [ "X-Have: " <> renderHave members
    , "X-Have-Digest: " <> renderHaveDigest (encodeGcs 10 members)
    , "X-Have-Digest(fp=8): " <> renderHaveDigest (encodeGcs 8 members)
    ]
  where
    members =
      [ ("Post:17", "e41")
      , ("User:9", "b02")
      , ("Review:5001", "e2")
      , ("Droid:2001", "e1")
      ]


entity :: TypeName -> Text -> Text -> Record
entity ty key ver =
  REntity
    EntityRecord
      { erId = Ref ty key
      , erVer = ver
      , erFields = Map.empty
      , erItem = Nothing
      , erSrc = Nothing
      }


requireParse :: Maybe a -> IO a
requireParse = maybe (expectationFailure "header failed to parse") pure


tshow :: (Show a) => a -> Text
tshow = T.pack . show
