{- | IDL front-end contracts (spec §3.4, §3.8): the fixtures elaborate,
canonical IDL round-trips and is a fixpoint, co-keyed entities inherit
their base's key, and elaboration rejections name the offending
declaration.
-}
module Test.Lattice.IDL (tests) where

import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Schema (
  CoKey (..),
  CoKeyMode (..),
  EntityDef (..),
  FieldDef (..),
  Schema,
  lookupEntity,
  lookupEntityField,
  sharedTruthFamily,
 )
import Lattice.Types (FieldName, FieldType, TypeName, unTypeName)
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

    describe "§3.8 co-keyed entities" $ do
      it "cokey.lattice parses and elaborates" $
        parseSchema cokeyText `shouldSatisfy` isRight

      it "joins elaborates to entityCoKey, key inherited from the base" $ do
        base <- entityOf cokeySchema "User"
        prof <- entityOf cokeySchema "UserProfile"
        entityCoKey prof `shouldBe` Just (CoKey "User" JoinsBase)
        entityKey prof `shouldBe` entityKey base
        keyFieldType prof "id" `shouldBe` keyFieldType base "id"

      it "refines elaborates to entityCoKey, key inherited from the base" $ do
        base <- entityOf cokeySchema "User"
        admin <- entityOf cokeySchema "AdminUser"
        entityCoKey admin `shouldBe` Just (CoKey "User" RefinesBase)
        entityKey admin `shouldBe` entityKey base
        keyFieldType admin "id" `shouldBe` keyFieldType base "id"

      it "an ordinary entity has no co-key" $ do
        base <- entityOf cokeySchema "User"
        entityCoKey base `shouldBe` Nothing

      it "a composite key is inherited whole (names and types)" $ do
        schema <- requireRight (parseSchema compositeSrc)
        base <- entityOf schema "Ledger"
        note <- entityOf schema "LedgerNote"
        entityKey base `shouldBe` "orgId" :| ["seq"]
        entityKey note `shouldBe` entityKey base
        keyFieldType note "orgId" `shouldBe` keyFieldType base "orgId"
        keyFieldType note "seq" `shouldBe` keyFieldType base "seq"

      describe "sharedTruthFamily (invalidation coupling)" $ do
        it "a base's family is itself plus every refinement, never joins companions" $
          NE.toList (sharedTruthFamily cokeySchema "User")
            `shouldBe` ["User", "AdminUser"]
        it "a refinement resolves to its base's family" $
          NE.toList (sharedTruthFamily cokeySchema "AdminUser")
            `shouldBe` ["User", "AdminUser"]
        it "a joins companion is a singleton family" $
          NE.toList (sharedTruthFamily cokeySchema "UserProfile")
            `shouldBe` ["UserProfile"]
        it "an uncoupled entity is a singleton family" $ do
          schema <- requireRight (parseSchema compositeSrc)
          NE.toList (sharedTruthFamily schema "Ledger") `shouldBe` ["Ledger"]

      describe "canonical IDL (§7.1)" $ do
        it "cokey canonical IDL golden" $
          pureGoldenTextFile
            "test/fixtures/golden/cokey.canonical.lattice"
            (canonicalIdl cokeySchema)
        it "parseSchema . canonicalIdl is the identity on the cokey model" $
          parseSchema (canonicalIdl cokeySchema) `shouldBe` Right cokeySchema
        it "canonical IDL is a fixpoint (cokey)" $
          fmap canonicalIdl (parseSchema (canonicalIdl cokeySchema))
            `shouldBe` Right (canonicalIdl cokeySchema)

      describe "rejections name the offender" $ do
        it "an unknown base names the base" $
          rejectsMentioning "Ghost" $
            entitySchema
              [ "entity Orphan joins Ghost {"
              , "  visible to all by default"
              , "  note: Text"
              , "}"
              ]
        it "chained co-keying names the offending declaration" $
          rejectsMentioning "Deep" $
            entitySchema
              [ "entity Base by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              , ""
              , "entity Mid refines Base {"
              , "  visible to all by default"
              , "  level: Text"
              , "}"
              , ""
              , "entity Deep refines Mid {"
              , "  visible to all by default"
              , "  depth: Text"
              , "}"
              ]
        it "re-declaring an inherited key field names the offender" $ do
          let src =
                entitySchema
                  [ "entity Base by id {"
                  , "  visible to all by default"
                  , "  id: Text"
                  , "}"
                  , ""
                  , "entity Shadow joins Base {"
                  , "  visible to all by default"
                  , "  id: Text"
                  , "}"
                  ]
          rejectsMentioning "Shadow" src
          rejectsMentioning "id" src
        it "a co-keyed declaration with a `by` clause is rejected naming the entity" $
          rejectsMentioning "Doubled" $
            entitySchema
              [ "entity Base by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              , ""
              , "entity Doubled joins Base by id {"
              , "  visible to all by default"
              , "  note: Text"
              , "}"
              ]


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


-- | The entity must exist in the schema (fixture invariant).
entityOf :: Schema -> TypeName -> IO EntityDef
entityOf s t =
  maybe
    (expectationFailure ("schema is missing entity " <> T.unpack (unTypeName t)))
    pure
    (lookupEntity s t)


-- | The declared (or, on a co-keyed entity, inherited) type of a field.
keyFieldType :: EntityDef -> FieldName -> Maybe FieldType
keyFieldType e f = fieldType <$> lookupEntityField e f


{- | Spec §3.8's composite-key case: a @joins@ companion of a base keyed
@by (orgId, seq)@ inherits the whole composite key.
-}
compositeSrc :: Text
compositeSrc =
  entitySchema
    [ "entity Ledger by (orgId, seq) {"
    , "  visible to all by default"
    , "  orgId: Text"
    , "  seq:   I32"
    , "  total: F64"
    , "}"
    , ""
    , "entity LedgerNote joins Ledger {"
    , "  visible to all by default"
    , "  note: Text"
    , "}"
    ]
