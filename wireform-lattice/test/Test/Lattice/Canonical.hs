{- | Canonicalization contracts (spec §5.1): the two-spellings property,
fixpoint and closure, default erasure, fragment expansion, and a
determinism/idempotence property over shuffled selection orders.
-}
module Test.Lattice.Canonical (tests) where

import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Lattice.Canonical (Compiled (..))
import Lattice.Schema (Schema)
import Test.Lattice.Fixtures
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Canonicalization (§5.1)" $ do
    describe "§5.1 two spellings, one identity (blog FeedPage)" $ do
      it "reordered fields, arguments, and variables" $
        shouldCanonEq blogSchema feedPageText $
          T.unlines
            [ "query FeedPage($limit: I32 = 20, $after: Cursor) {"
            , "  feed(limit: $limit, after: $after) {"
            , "    publishedAt"
            , "    comments(first: 3) {"
            , "      author { ...UserByline }"
            , "      createdAt"
            , "      body"
            , "    }"
            , "    author { ...UserByline }"
            , "    title"
            , "  }"
            , "}"
            ]
      it "limit: is the first: argument by another name" $
        shouldCanonEq blogSchema feedPageText $
          T.unlines
            [ "query FeedPage($after: Cursor, $limit: I32 = 20) {"
            , "  feed(after: $after, first: $limit) {"
            , "    title"
            , "    publishedAt"
            , "    author { ...UserByline }"
            , "    comments(first: 3) {"
            , "      body"
            , "      createdAt"
            , "      author { ...UserByline }"
            , "    }"
            , "  }"
            , "}"
            ]
      it "commas are never syntax" $
        shouldCanonEq blogSchema feedPageText $
          T.unlines
            [ "query FeedPage($after: Cursor, $limit: I32 = 20) {"
            , "  feed(after: $after, limit: $limit) {"
            , "    title, publishedAt, author { ...UserByline },"
            , "    comments(first: 3) { body, createdAt, author { ...UserByline } },"
            , "  }"
            , "}"
            ]
      it "Int is the I32 variable type by another name (§3.5 aliases)" $
        shouldCanonEq blogSchema feedPageText $
          T.replace "$limit: I32" "$limit: Int" feedPageText
      it "an explicit declared-default argument erases (avatarUrl(size:96))" $
        shouldCanonEq
          blogSchema
          "query { me { avatarUrl } }"
          "query { me { avatarUrl(size: 96) } }"

    describe "§5.1 default erasure (pagination defaultPage)" $ do
      it "friends(first:10) erases to bare friends under starwars" $ do
        explicit <- mustCompileWith starwarsSchema "query { hero { friends(first: 10) { name } } }"
        bare <- mustCompileWith starwarsSchema "query { hero { friends { name } } }"
        compiledText explicit `shouldBe` compiledText bare
        compiledText explicit `shouldNotSatisfy` T.isInfixOf "first"

    describe "§5.1 fixpoint and closure" $ do
      it "compiling a canonical text is the identity (FeedPage)" $ do
        c <- mustCompileWith blogSchema feedPageText
        c2 <- mustCompileWith blogSchema (compiledText c)
        compiledText c2 `shouldBe` compiledText c
        compiledHash c2 `shouldBe` compiledHash c
      it "the canonical text reparses and revalidates (§4.8 closure)" $ do
        c <- mustCompileWith starwarsSchema "query Hero { hero { name friends(first: 10) { name } } }"
        compileWith starwarsSchema (compiledText c) `shouldSatisfy` isRight

    describe "§5.1 fragment expansion" $ do
      it "local fragments leave no spread in the canonical text" $ do
        c <-
          mustCompileWith starwarsSchema $
            T.unlines
              [ "fragment Names on Character { name }"
              , "query { hero { friends { ...Names } } }"
              ]
        compiledText c `shouldNotSatisfy` T.isInfixOf "..."
      it "schema-fragment spreads survive as late-bound references (§4.5)" $ do
        c <- mustCompileWith blogSchema feedPageText
        compiledText c `shouldSatisfy` T.isInfixOf "...UserByline"

    describe "§5.1 determinism property" $ do
      it "canonicalization is deterministic and idempotent over shuffled selections" $
        H.withTests 100 $
          H.property $ do
            reference <- H.evalEither (compileWith starwarsSchema (reviewsQuery reviewFields))
            perm <- H.forAll (Gen.shuffle reviewFields)
            c <- H.evalEither (compileWith starwarsSchema (reviewsQuery perm))
            compiledText c H.=== compiledText reference
            compiledHash c H.=== compiledHash reference
            -- Idempotence: the canonical text canonicalizes to itself.
            c2 <- H.evalEither (compileWith starwarsSchema (compiledText c))
            compiledText c2 H.=== compiledText c

    describe "§5.1 canonical query goldens (cross-implementation pin)" $ do
      it "starwars Hero" $
        goldenCompiled "test/fixtures/golden/hero.canonical.txt" starwarsSchema $
          "query Hero { hero { name friends(first: 10) { name } } }"
      it "starwars Search" $
        goldenCompiled "test/fixtures/golden/search.canonical.txt" starwarsSchema $
          T.unlines
            [ "query Search($text: Text) {"
            , "  search(text: $text, first: 10) {"
            , "    ... on Human    { name homePlanet }"
            , "    ... on Droid    { name primaryFunction }"
            , "    ... on Starship { name length }"
            , "  }"
            , "}"
            ]
      it "blog FeedPage" $
        goldenCompiled "test/fixtures/golden/feedpage.canonical.txt" blogSchema feedPageText

    describe "§5.1 step 4: NFC normalization" $ do
      -- The cross-implementation vector: "café" spelled decomposed
      -- (e + U+0301) and precomposed (U+00E9) is ONE query identity —
      -- byte-identical canonical text, colliding hashes. The TypeScript
      -- canonicalizer must agree on these exact bytes.
      it "decomposed and precomposed string literals are one identity" $
        shouldCanonEq starwarsSchema (nfcQuery decomposedCafe) (nfcQuery precomposedCafe)
      it "the canonical text carries the precomposed form" $ do
        c <- mustCompileWith starwarsSchema (nfcQuery decomposedCafe)
        compiledText c `shouldSatisfy` T.isInfixOf precomposedCafe
        compiledText c `shouldNotSatisfy` T.isInfixOf decomposedCafe
      it "the pinned cross-language vector: canonical bytes and hash match lattice-ts" $ do
        -- Verified byte-identical in GHC and node/tsx by the DigestNfc
        -- wave (lattice-ts canonical.test.ts pins the same two values).
        c <- mustCompileWith starwarsSchema "query { search(text: \"cafe\x0301\") { name } }"
        compiledText c `shouldBe` "query{search(text:\"caf\x00E9\"){name}}"
        compiledHash c `shouldBe` "u_SYA1r8Jk6j5rebZd1GCw"


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Two spellings must produce byte-identical canonical text and equal hash.
shouldCanonEq :: Schema -> Text -> Text -> IO ()
shouldCanonEq schema a b = do
  ca <- mustCompileWith schema a
  cb <- mustCompileWith schema b
  compiledText cb `shouldBe` compiledText ca
  compiledHash cb `shouldBe` compiledHash ca


{- | @café@ with the accent as a combining mark (e + U+0301): the §5.1
step-4 input that must collapse to 'precomposedCafe'.
-}
decomposedCafe :: Text
decomposedCafe = "cafe\x0301"


-- | @café@ with the precomposed U+00E9 — the NFC form.
precomposedCafe :: Text
precomposedCafe = "caf\x00E9"


-- | A starwars search with the vector inside a string-literal argument.
nfcQuery :: Text -> Text
nfcQuery needle =
  "query { search(text: \"" <> needle <> "\", first: 10) { ... on Human { name } } }"


reviewFields :: [Text]
reviewFields = ["commentary", "createdAt", "episode", "id", "stars"]


reviewsQuery :: [Text] -> Text
reviewsQuery fields =
  "query { reviews(episode: Empire) { " <> T.unwords fields <> " } }"


-- | Canonical text plus its query hash, pinned as a golden file.
goldenCompiled :: FilePath -> Schema -> Text -> GoldenTest Text
goldenCompiled path schema q = goldenTextFile path $ do
  c <- mustCompileWith schema q
  pure (compiledText c <> "\n" <> compiledHash c <> "\n")
