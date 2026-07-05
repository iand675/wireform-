{- | IDL front-end contracts (spec §3.4): the fixtures elaborate, canonical
IDL round-trips and is a fixpoint, and elaboration rejections name the
offending declaration.
-}
module Test.Lattice.IDL (tests) where

import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Test.Lattice.Fixtures
import Test.Syd


tests :: Spec
tests =
  describe "IDL (§3.4)" $ do
    describe "§3.4 fixture schemas elaborate" $ do
      it "starwars.lattice parses and elaborates" $
        parseSchema starwarsText `shouldSatisfy` isRight
      it "blog.lattice parses and elaborates" $
        parseSchema blogText `shouldSatisfy` isRight

    describe "§3.4/§7.1 canonical IDL round-trip" $ do
      it "parseSchema . canonicalIdl is the identity on the starwars model" $
        parseSchema (canonicalIdl starwarsSchema) `shouldBe` Right starwarsSchema
      it "parseSchema . canonicalIdl is the identity on the blog model" $
        parseSchema (canonicalIdl blogSchema) `shouldBe` Right blogSchema
      it "canonical IDL is a fixpoint (starwars)" $
        fmap canonicalIdl (parseSchema (canonicalIdl starwarsSchema))
          `shouldBe` Right (canonicalIdl starwarsSchema)
      it "canonical IDL is a fixpoint (blog)" $
        fmap canonicalIdl (parseSchema (canonicalIdl blogSchema))
          `shouldBe` Right (canonicalIdl blogSchema)

    describe "§7.1 canonical IDL golden (cross-implementation pin)" $ do
      it "starwars canonical IDL" $
        pureGoldenTextFile
          "test/fixtures/golden/starwars.canonical.lattice"
          (canonicalIdl starwarsSchema)
      it "blog canonical IDL" $
        pureGoldenTextFile
          "test/fixtures/golden/blog.canonical.lattice"
          (canonicalIdl blogSchema)

    describe "§3.4/§3.1 elaboration rejections name the offender" $ do
      it "a policy referencing an undeclared claim names the claim" $
        rejectsMentioning "zorg" $
          entitySchema
            [ "entity Buddy by id {"
            , "  visible to all by default"
            , "  id: Text"
            , "  secret: Text visible when caller.zorg = id"
            , "}"
            ]
      it "an entity missing its default visibility names the entity" $ do
        let src =
              entitySchema
                [ "entity Buddy by id {"
                , "  id: Text"
                , "}"
                ]
        rejectsMentioning "Buddy" src
        rejectsMentioning "default visibility" src
      it "a has-many link field missing on a single-entity target names the field" $
        rejectsMentioning "ownerId" $
          entitySchema
            [ "entity Buddy by id {"
            , "  visible to all by default"
            , "  id: Text"
            , "  name: Text"
            , "}"
            , ""
            , "entity Home by id {"
            , "  visible to all by default"
            , "  id: Text"
            , "  has many pals: Buddy by ownerId"
            , "                 ordered by name asc"
            , "                 page 5 max 10"
            , "}"
            ]
      it "a keyset column missing on the target names the column" $
        rejectsMentioning "zapf" $
          entitySchema
            [ "entity Buddy by id {"
            , "  visible to all by default"
            , "  id: Text"
            , "  homeId: Text"
            , "}"
            , ""
            , "entity Home by id {"
            , "  visible to all by default"
            , "  id: Text"
            , "  has many pals: Buddy by homeId"
            , "                 ordered by zapf asc"
            , "                 page 5 max 10"
            , "}"
            ]
      it "a mutation writing an unknown collection names the collection" $
        rejectsMentioning "ghosts" $
          entitySchema
            [ "entity Buddy by id {"
            , "  visible to all by default"
            , "  id: Text"
            , "}"
            , ""
            , "mutation touch(b: Text) returns Buddy {"
            , "  allow       public"
            , "  writes      Buddy(b), ghosts(b)"
            , "  invalidates writes"
            , "  effect      transactional"
            , "}"
            ]
      it "a duplicate field declaration names the field" $ do
        let src =
              entitySchema
                [ "entity Buddy by id {"
                , "  visible to all by default"
                , "  id: Text"
                , "  name: Text"
                , "  name: Text"
                , "}"
                ]
        rejectsMentioning "duplicate field" src
        rejectsMentioning "name" src


-- | A minimal schema document around the given declaration lines.
entitySchema :: [Text] -> Text
entitySchema decls = T.unlines ("schema t.example" : "" : decls)


{- | Elaboration must fail, and at least one 'SchemaError' must mention the
offending name so the author can find the declaration.
-}
rejectsMentioning :: Text -> Text -> IO ()
rejectsMentioning offender src = case parseSchema src of
  Right _ ->
    expectationFailure
      ("elaboration unexpectedly succeeded; wanted an error mentioning " <> T.unpack offender)
  Left errs -> map seMessage errs `shouldSatisfy` any (T.isInfixOf offender)
