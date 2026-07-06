{- | Query-language contracts (spec §4.8): the corpus queries compile against
the starwars fixture, the grammar has no alias/directive/null productions,
and static rules 6–8 reject with diagnostics naming the offender.
-}
module Test.Lattice.Query (tests) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.Canonical (Compiled (..), compileDocument, expandImports)
import Lattice.Query.Parser (parseDocument)
import Lattice.Query.Validate (CompileError (..))
import Lattice.Schema (Schema, defaultBudgets)
import Test.Lattice.Fixtures
import Test.Syd


tests :: Spec
tests =
  describe "Query language (§4.8)" $ do
    describe "§4.8 corpus queries compile against the starwars fixture" $ do
      it "corpus 1: Hero (nested fields, paginated edge)" $ do
        c <- mustCompileWith starwarsSchema heroQ
        compiledText c `shouldSatisfy` T.isInfixOf "hero{"
      it "corpus 2: HeroNameAndFriends (grouping-key argument via variable)" $ do
        c <- mustCompileWith starwarsSchema heroNameAndFriendsQ
        compiledText c `shouldSatisfy` T.isInfixOf "$episode"
      it "corpus 4: fragments resolved through expandImports, expanded at build time" $ do
        doc <- requireRight (parseDocument empireHeroQ)
        expanded <- requireRight (expandImports importFiles doc)
        c <- requireRight (compileDocument starwarsSchema defaultBudgets expanded)
        -- The local (imported) fragment leaves no spread behind; only the
        -- inline dispatch survives.
        compiledText c `shouldNotSatisfy` T.isInfixOf "...Comparison"
        compiledText c `shouldSatisfy` T.isInfixOf "... on Droid"
      it "corpus 6: Search (inline fragments on a union root)" $ do
        c <- mustCompileWith starwarsSchema searchQ
        compiledText c `shouldSatisfy` T.isInfixOf "... on Starship"

    describe "§4.8 grammar has no production for these (parse rejections)" $ do
      it "aliases (rule 3): 'empire: name' is rejected" $
        rejects starwarsSchema "query { hero { empire: name } }"
      it "@include (rule 4): the only directive is @depth" $
        rejects starwarsSchema "query Q($x: Bool) { hero { name @include(if: $x) } }"
      it "@skip (rule 4): the only directive is @depth" $
        rejects starwarsSchema "query Q($x: Bool) { hero { name @skip(if: $x) } }"
      it "explicit null (rule 6): omission is the only spelling of absence" $
        rejects starwarsSchema "query { hero(episode: null) { name } }"
      it "two query definitions (rule 1)" $
        rejects starwarsSchema "query A { hero { name } } query B { hero { name } }"
      it "reserved name as a variable (rule 2): $on" $
        rejects starwarsSchema "query Q($on: Text) { search(text: $on, first: 10) { name } }"
      it "reserved URL parameter as a variable (§6.1): $slice" $
        rejectsMentioning
          starwarsSchema
          "reserved URL parameter"
          "query Q($slice: Text) { search(text: $slice, first: 10) { name } }"

    describe "§4.8 rule 6: argument declaredness and pagination exclusivity" $ do
      it "first and last are mutually exclusive" $
        rejects starwarsSchema "query { reviews(episode: Empire, first: 5, last: 5) { stars } }"
      it "after requires first" $
        rejectsMentioning
          starwarsSchema
          "after"
          "query { reviews(episode: Empire, after: \"cur_abc\") { stars } }"
      it "a bounded collection takes no arguments (blog Post.tags)" $
        rejectsMentioning
          blogSchema
          "tags"
          "query { feed { tags(first: 5) { name } } }"
      it "a floor does not paginate: a min-floored bounded collection takes no arguments (§3.6)" $
        rejectsMentioning
          cardSchema
          "lineItems"
          "query Q($id: OrderId) { order(id: $id) { lineItems(first: 5) { sku } } }"
      it "an undeclared argument names the argument" $
        rejectsMentioning starwarsSchema "zap" "query { hero(zap: 3) { name } }"

    describe "§4.8 rule 7: variable use/declaration agreement" $ do
      it "an unused variable is rejected, not ignored" $
        rejectsMentioning starwarsSchema "x" "query Q($x: Text) { hero { name } }"
      it "an undeclared variable is rejected" $
        rejectsMentioning starwarsSchema "ep" "query { hero(episode: $ep) { name } }"
      it "a Text variable bound to first is type-incompatible" $
        rejectsMentioning
          starwarsSchema
          "x"
          "query Q($x: Text) { reviews(episode: Empire, first: $x) { stars } }"

    describe "§4.8 rule 8: spread resolution, acyclicity, and shadowing" $ do
      it "an unresolved spread names the fragment" $
        rejectsMentioning starwarsSchema "Nope" "query { hero { ...Nope } }"
      it "a spread cycle is rejected" $
        rejectsMentioning starwarsSchema "cycle" $
          T.unlines
            [ "fragment A on Character { name ...B }"
            , "fragment B on Character { ...A }"
            , "query { hero { friends { ...A } } }"
            ]
      it "a local fragment shadowing a schema fragment is lattice:fragment-shadow" $
        case compileWith blogSchema shadowQ of
          Right _ -> expectationFailure "fragment shadow unexpectedly compiled"
          Left ce -> ceCode ce `shouldBe` "lattice:fragment-shadow"


-- ---------------------------------------------------------------------------
-- Corpus query texts (corpus.md entries 1, 2, 4, 6; enum casing follows the
-- fixture's canonical `Empire`, corpus prose uses GraphQL's `EMPIRE`)
-- ---------------------------------------------------------------------------

heroQ :: Text
heroQ = "query Hero { hero { name friends(first: 10) { name } } }"


heroNameAndFriendsQ :: Text
heroNameAndFriendsQ =
  T.unlines
    [ "query HeroNameAndFriends($episode: Episode) {"
    , "  hero(episode: $episode) {"
    , "    name"
    , "    friends(first: 10) { name }"
    , "  }"
    , "}"
    ]


{- | Corpus entry 4 verbatim: the fixture's @Character@ interface declares
@name@, @appearsIn@, and the @friends@ collection, so the corpus's shared
comparison fragment is legal as written (§4.4, rule 5); an inline fragment
is added to keep per-type dispatch covered too.
-}
importFiles :: Map.Map Text Text
importFiles =
  Map.singleton "fragments/character.lq" $
    T.unlines
      [ "fragment Comparison on Character {"
      , "  name"
      , "  appearsIn"
      , "  friends(first: 10) { name }"
      , "  ... on Droid { primaryFunction }"
      , "}"
      ]


empireHeroQ :: Text
empireHeroQ =
  T.unlines
    [ "import \"fragments/character.lq\""
    , "query EmpireHero { hero(episode: Empire) { ...Comparison } }"
    ]


searchQ :: Text
searchQ =
  T.unlines
    [ "query Search($text: Text) {"
    , "  search(text: $text, first: 10) {"
    , "    ... on Human    { name homePlanet }"
    , "    ... on Droid    { name primaryFunction }"
    , "    ... on Starship { name length }"
    , "  }"
    , "}"
    ]


shadowQ :: Text
shadowQ =
  T.unlines
    [ "fragment UserByline on User { name }"
    , "query { me { ...UserByline } }"
    ]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

rejects :: Schema -> Text -> IO ()
rejects schema q = compileWith schema q `shouldSatisfy` isLeft


-- | Compilation must fail with a diagnostic mentioning the offender.
rejectsMentioning :: Schema -> Text -> Text -> IO ()
rejectsMentioning schema needle q = case compileWith schema q of
  Right c ->
    expectationFailure
      ("query unexpectedly compiled to " <> T.unpack (compiledText c))
  Left ce -> ceDiagnostics ce `shouldSatisfy` any (T.isInfixOf needle)
