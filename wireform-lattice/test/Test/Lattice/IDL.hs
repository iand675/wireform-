{- | IDL front-end contracts (spec §3.4, §3.5, §3.6, §3.8): the fixtures
elaborate, canonical IDL round-trips and is a fixpoint, co-keyed entities
inherit their base's key, cardinality declarations elaborate into the
pinned model shapes, and elaboration rejections name the offending
declaration.
-}
module Test.Lattice.IDL (tests) where

import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Lattice.IDL.Parser (SchemaError (..), parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Schema (
  ArgDef (..),
  CoKey (..),
  CoKeyMode (..),
  CollectionDef (..),
  DeclPath (..),
  DirLocation (..),
  DirectiveApp (..),
  DirectiveDef (..),
  EntityDef (..),
  FieldDef (..),
  OverflowPolicy (..),
  RelationshipDef (..),
  Schema,
  Windowing (..),
  lookupEntity,
  lookupEntityField,
  lookupEntityRel,
  schemaDescriptions,
  schemaDirectiveDecls,
  schemaDirectives,
  sharedTruthFamily,
 )
import Lattice.Types (FieldName, FieldType (..), Prim (..), TypeName, unTypeName)
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

    describe "§3.4–§3.6 cardinality declarations" $ do
      it "the card fixture parses and elaborates" $
        parseSchema cardText `shouldSatisfy` isRight

      describe "elaborated model shapes" $ do
        it "bare `has one` is the exactly-one contract (relOptional = False)" $ do
          rel <- relOf cardSchema "Order" "customer"
          case rel of
            ToOne {relByField = by, relOptional = opt} -> do
              by `shouldBe` "customerId"
              opt `shouldBe` False
            other -> expectationFailure ("expected a to-one, got: " <> show other)
        it "`has one?` declares absence legal (relOptional = True)" $ do
          rel <- relOf cardSchema "Order" "reviewer"
          case rel of
            ToOne {relByField = by, relOptional = opt} -> do
              by `shouldBe` "reviewerId"
              opt `shouldBe` True
            other -> expectationFailure ("expected a to-one, got: " <> show other)
        it "`min 1 max 200` elaborates the floored bounded window" $ do
          w <- windowOf cardSchema "Order" "lineItems"
          w `shouldBe` Bounded 1 200 Overflow
        it "an omitted min defaults to 0 (blog Post.tags)" $ do
          w <- windowOf blogSchema "Post" "tags"
          w `shouldBe` Bounded 0 50 Overflow
        it "min composes with the truncate overflow policy" $ do
          schema <-
            requireRight . parseSchema $
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
                , "  has many pals: Buddy by homeId min 1 max 5 truncate"
                , "}"
                ]
          w <- windowOf schema "Home" "pals"
          w `shouldBe` Bounded 1 5 Truncate
        it "`[Text]+` elaborates to the nonempty list type" $ do
          order <- entityOf cardSchema "Order"
          keyFieldType order "tags" `shouldBe` Just (TList1 (TPrim PText))
        it "`[Text]+?` is absent-or-nonempty (option over nonempty list)" $ do
          order <- entityOf cardSchema "Order"
          keyFieldType order "memo" `shouldBe` Just (TOptional (TList1 (TPrim PText)))

      describe "canonical IDL (§7.1)" $ do
        it "parseSchema . canonicalIdl is the identity on the card model" $
          parseSchema (canonicalIdl cardSchema) `shouldBe` Right cardSchema
        it "canonical IDL is a fixpoint (card)" $
          fmap canonicalIdl (parseSchema (canonicalIdl cardSchema))
            `shouldBe` Right (canonicalIdl cardSchema)
        it "the new surface prints canonically: has one?, min before max, [t]+, [t]+?" $ do
          let canon = canonicalIdl cardSchema
          canon `shouldSatisfy` T.isInfixOf "has one? reviewer: Customer by reviewerId"
          canon `shouldSatisfy` T.isInfixOf "has many lineItems: LineItem by orderId min 1 max 200"
          canon `shouldSatisfy` T.isInfixOf "tags: [Text]+"
          canon `shouldSatisfy` T.isInfixOf "memo: [Text]+?"
          canon `shouldSatisfy` T.isInfixOf "newtype Tags = [Text]+"
        it "bare `has one` and an unfloored `max` print exactly as before" $ do
          canonicalIdl cardSchema
            `shouldSatisfy` T.isInfixOf "has one customer: Customer by customerId"
          canonicalIdl cardSchema
            `shouldNotSatisfy` T.isInfixOf "has one? customer"
          -- The blog model uses only the old surface: its canonical text
          -- carries no optional-edge marker and no floor (its golden pins
          -- the exact bytes; this pins the absence of the new syntax).
          canonicalIdl blogSchema `shouldNotSatisfy` T.isInfixOf "has one?"
          canonicalIdl blogSchema `shouldNotSatisfy` T.isInfixOf "min "

      describe "rejections name the offender" $ do
        it "element optionality [t?] in field position names the field" $
          rejectsMentioning "chaos" $
            entitySchema
              [ "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "  chaos: [Text?]"
              , "}"
              ]
        it "element optionality [t?] in argument position names the argument" $
          rejectsMentioning "xs" $
            entitySchema
              [ "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              , ""
              , "mutation touch(b: Text, xs: [Text?]) returns Buddy {"
              , "  allow       public"
              , "  writes      Buddy(b)"
              , "  invalidates writes"
              , "  effect      transactional"
              , "}"
              ]
        it "a floor above the cap (min 5 max 2) names the relationship" $
          rejectsMentioning "pals" $
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
              , "  has many pals: Buddy by homeId min 5 max 2"
              , "}"
              ]
        it "a floor on a paginated collection names the relationship" $
          rejectsMentioning "pals" $
            entitySchema
              [ "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "  homeId: Text"
              , "  name: Text"
              , "}"
              , ""
              , "entity Home by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "  has many pals: Buddy by homeId min 1 max 10"
              , "                 ordered by name asc page 5"
              , "}"
              ]
        it "a required `has one` over an optional link column names the edge" $ do
          let doc byLine =
                entitySchema
                  [ "entity Buddy by id {"
                  , "  visible to all by default"
                  , "  id: Text"
                  , "}"
                  , ""
                  , "entity Doc by id {"
                  , "  visible to all by default"
                  , "  id: Text"
                  , "  editorId: Text?"
                  , byLine
                  , "}"
                  ]
          rejectsMentioning "editor" (doc "  has one editor: Buddy by editorId")
          -- The declared-optional sibling over the same column is the fix.
          parseSchema (doc "  has one? editor: Buddy by editorId")
            `shouldSatisfy` isRight

    describe "§3.9 directives + descriptions" $ do
      it "directives.lattice parses and elaborates" $
        parseSchema directivesText `shouldSatisfy` isRight

      describe "canonical IDL (§7.1)" $ do
        it "directives canonical IDL golden" $
          pureGoldenTextFile
            "test/fixtures/golden/directives.canonical.lattice"
            (canonicalIdl directivesSchema)
        it "parseSchema . canonicalIdl is the identity on the directives model" $
          parseSchema (canonicalIdl directivesSchema) `shouldBe` Right directivesSchema
        it "canonical IDL is a fixpoint (directives)" $
          fmap canonicalIdl (parseSchema (canonicalIdl directivesSchema))
            `shouldBe` Right (canonicalIdl directivesSchema)

      describe "the elaborated model carries the registry, applications, and docs" $ do
        it "the five declared directives register in the closed registry" $
          Map.keys (schemaDirectiveDecls directivesSchema)
            `shouldBe` ["audit", "internal", "label", "pii", "rateLimit"]
        it "a declaration's repeatable/locations/args match its `directive` line (@audit)" $
          case Map.lookup "audit" (schemaDirectiveDecls directivesSchema) of
            Nothing -> expectationFailure "the fixture declares no @audit directive"
            Just def -> do
              dirRepeatable def `shouldBe` True
              dirLocations def `shouldBe` Set.fromList [DLEntity, DLField, DLMutation]
              map adName (dirArgs def) `shouldBe` ["level"]
              map (isJust . adDefault) (dirArgs def) `shouldBe` [True]
        it "both @audit applications on the repeated field are stored, args-sorted" $
          case Map.lookup (OnEntityItem "Post" "createdAt") (schemaDirectives directivesSchema) of
            Nothing -> expectationFailure "no directives stored on Post.createdAt"
            Just apps -> do
              map daName apps `shouldBe` ["audit", "audit"]
              map (map fst . daArgs) apps `shouldBe` [[], ["level"]]
        it "application arguments are stored sorted by name (@rateLimit on `list posts`)" $
          case Map.lookup (OnRoot "posts") (schemaDirectives directivesSchema) of
            Nothing -> expectationFailure "no directives stored on the posts root"
            Just apps ->
              map (\a -> (daName a, map fst (daArgs a))) apps
                `shouldBe` [("rateLimit", ["burst", "perMinute"])]
        it "descriptions attach to their declaration site with exact text" $ do
          Map.lookup (OnType "UserId") (schemaDescriptions directivesSchema)
            `shouldBe` Just "A stable, opaque user identifier."
          Map.lookup (OnEntity "User") (schemaDescriptions directivesSchema)
            `shouldBe` Just "A registered account holder."
          Map.lookup (OnEntityItem "User" "id") (schemaDescriptions directivesSchema)
            `shouldBe` Just "The user's stable key."
          Map.lookup (OnDirective "audit") (schemaDescriptions directivesSchema)
            `shouldBe` Just "Emit an audit-log entry when this element is read or written."

      describe "elaboration rejections name the offender" $ do
        it "applying an undeclared directive names it" $
          rejectsMentioning "phantom" $
            entitySchema
              [ "@phantom"
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "applying a directive at a disallowed location names it (FIELD on an ENTITY)" $
          rejectsMentioning "onField" $
            entitySchema
              [ "directive @onField on FIELD"
              , ""
              , "@onField"
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "an unknown argument names the argument" $
          rejectsMentioning "bogus" $
            entitySchema
              [ "directive @tagme(label: Text = \"x\") on ENTITY"
              , ""
              , "@tagme(bogus: \"y\")"
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "a missing required (no-default) argument names it" $
          rejectsMentioning "ticket" $
            entitySchema
              [ "directive @needs(ticket: Text) on ENTITY"
              , ""
              , "@needs"
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "a duplicate argument in one application names it" $
          rejectsMentioning "tone" $
            entitySchema
              [ "directive @dupe(tone: Text = \"soft\") on ENTITY"
              , ""
              , "@dupe(tone: \"a\", tone: \"b\")"
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "a second application of a non-repeatable directive names it" $
          rejectsMentioning "once" $
            entitySchema
              [ "directive @once on ENTITY"
              , ""
              , "@once"
              , "@once"
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "a directive declaration reusing a reserved name names it" $
          rejectsMentioning "break" $
            entitySchema
              [ "directive @break on FIELD"
              , ""
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "two directive declarations of one name name it" $
          rejectsMentioning "twice" $
            entitySchema
              [ "directive @twice on FIELD"
              , "directive @twice on ENTITY"
              , ""
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "}"
              ]
        it "a FIELD directive applied to a `has many` edge is rejected (FIELD /= RELATIONSHIP)" $
          rejectsMentioning "fieldOnly" $
            entitySchema
              [ "directive @fieldOnly on FIELD"
              , ""
              , "entity Buddy by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "  homeId: Text"
              , "  name: Text"
              , "}"
              , ""
              , "entity Home by id {"
              , "  visible to all by default"
              , "  id: Text"
              , "  @fieldOnly"
              , "  has many pals: Buddy by homeId"
              , "                 ordered by name asc"
              , "                 page 20 max 100"
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


-- | The relationship must exist on the entity (fixture invariant).
relOf :: Schema -> TypeName -> FieldName -> IO RelationshipDef
relOf s t f = do
  e <- entityOf s t
  maybe
    (expectationFailure ("entity " <> T.unpack (unTypeName t) <> " is missing relationship"))
    pure
    (lookupEntityRel e f)


-- | The windowing of a @has many@ relationship (fixture invariant).
windowOf :: Schema -> TypeName -> FieldName -> IO Windowing
windowOf s t f =
  relOf s t f >>= \case
    ToMany {relCollection = col} -> pure (colWindow col)
    other -> expectationFailure ("expected a to-many, got: " <> show other)


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
