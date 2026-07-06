{- | The corpus dataset: the Star Wars schema from the Lattice docs, seeded
with the classic characters, wired into the in-memory backend.

The schema text is parsed at runtime: the checked-in fixture is preferred
(so edits to it take effect without a rebuild), with an embedded copy of
the same text as fallback so the binary runs from any working directory.

Hook choices mirror the corpus:

* @hero(episode:)@ — R2-D2 by default, Luke for @Empire@.
* @search(text:)@ — case-insensitive substring on @name@ across all three
  tables, ordered by name, single-window (cursor-less: @next@ is always
  null).
* @friends@ — each parent row stores a @friendIds@ field (list of ref
  strings); the children override resolves them to live targets and pages
  them ordered by target name through the shared pagination machinery.
* @createReview@ — inserts a Review under the next sequential id and
  reports the entity + @reviews:\<episode\>@ collection write facts.
* @Saga@ (appended demo entity, §3.7) — one row per episode with an
  @on read@ aggregate (@reviewCount@) and a @maintained@ aggregate
  (@starTotal@, seeded stale at 0 so relay convergence is observable).
  @addSagaReview@ is @createReview@ plus the @Saga.sagaReviews@ collection
  write fact that triggers the maintained relay.
-}
module StarWars (
  loadStarWarsSchema,
  newStarWarsDb,
  starWarsHooks,
) where

import Control.Concurrent.STM (STM, atomically)
import Control.Exception (IOException, try)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Lattice.Backend
import Lattice.Backend.Memory
import Lattice.IDL.Parser (parseSchema)
import Lattice.Schema
import Lattice.Types
import Lattice.Value (renderScalarKey)
import Text.Read (readMaybe)


-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

{- | Parse the corpus schema, preferring the checked-in fixture, falling
back to 'starWarsSource'. Demo-only declarations ('starWarsExtensions')
are appended to whichever text loads, so the fixture (and its canonical
golden) stays byte-identical.
-}
loadStarWarsSchema :: IO Schema
loadStarWarsSchema = do
  src <- readFirst fixturePaths
  case parseSchema (fromMaybe starWarsSource src <> starWarsExtensions <> starWarsDerivedExtensions) of
    Right schema -> pure schema
    Left errs -> fail ("starwars.lattice failed to parse: " <> show errs)
  where
    fixturePaths =
      [ "wireform-lattice/test/fixtures/starwars.lattice"
      , "test/fixtures/starwars.lattice"
      ]
    readFirst [] = pure Nothing
    readFirst (p : ps) =
      try (TIO.readFile p) >>= \case
        Right t -> pure (Just t)
        Left (_ :: IOException) -> readFirst ps


-- | Embedded copy of @test/fixtures/starwars.lattice@.
starWarsSource :: Text
starWarsSource =
  T.unlines
    [ "schema starwars.example.com"
    , ""
    , "-- The corpus schema (Lattice for GraphQL Developers, §0), adjusted where the"
    , "-- corpus was loose; each adjustment is a spec clarification:"
    , "--"
    , "--   * interfaces are declared, and entities opt in with `implements`"
    , "--     (the corpus said Character \"never appears as a declared interface"
    , "--     type\", but `has many friends: Character` and `fragment ... on"
    , "--     Character` both need the name resolvable);"
    , "--   * `list search` is a parameter-backed list root (its `text` is a query"
    , "--     parameter, not a field of the targets), ordered by a real field;"
    , "--   * newtypes for the id types are declared."
    , ""
    , "newtype HumanId    = Text"
    , "newtype DroidId    = Text"
    , "newtype StarshipId = Text"
    , "newtype ReviewId   = Text"
    , ""
    , "enum Episode closed = NewHope | Empire | Jedi"
    , ""
    , "interface Character {"
    , "  name: Text"
    , "}"
    , ""
    , "entity Human by id implements Character {"
    , "  visible to all by default"
    , ""
    , "  id:         HumanId"
    , "  name:       Text"
    , "  homePlanet: Text?"
    , "  appearsIn:  [Episode]"
    , ""
    , "  has many friends: Character by ownerId"
    , "                    ordered by name asc"
    , "                    page 10 max 50"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Droid by id implements Character {"
    , "  visible to all by default"
    , ""
    , "  id:              DroidId"
    , "  name:            Text"
    , "  primaryFunction: Text?"
    , "  appearsIn:       [Episode]"
    , ""
    , "  has many friends: Character by ownerId"
    , "                    ordered by name asc"
    , "                    page 10 max 50"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Starship by id {"
    , "  visible to all by default"
    , ""
    , "  id:     StarshipId"
    , "  name:   Text"
    , "  length: F64?"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "entity Review by id {"
    , "  visible to all by default"
    , ""
    , "  id:         ReviewId"
    , "  episode:    Episode"
    , "  stars:      W8"
    , "  commentary: Text?"
    , "  createdAt:  Timestamp"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "fragment CharacterName on Character { name }"
    , ""
    , "get hero(episode: Episode?) of (Human | Droid) public"
    , ""
    , "list reviews of Review by episode"
    , "     ordered by createdAt desc"
    , "     page 10 max 50"
    , "     public"
    , ""
    , "list search(text: Text) of (Human | Droid | Starship)"
    , "     ordered by name asc"
    , "     page 10 max 50"
    , "     public"
    , ""
    , "mutation createReview(episode: Episode, stars: W8, commentary: Text?) returns Review {"
    , "  allow       public"
    , "  writes      Review(new), reviews(episode)"
    , "  invalidates writes"
    , "  effect      transactional"
    , "}"
    ]


{- | Demo-only declarations appended to the parsed schema: the verb-binding
surface (spec §11.7\/§11.8). Kept out of @starwars.lattice@ so the
fixture's canonical golden stays byte-identical; appending is limited to
top-level declarations by construction.
-}
starWarsExtensions :: Text
starWarsExtensions =
  T.unlines
    [ ""
    , "-- Verb-binding demo surface (spec §11.7/§11.8), appended by the example"
    , "-- server so the checked-in fixture stays untouched."
    , ""
    , "data ReviewInput { episode: Episode, stars: W8, commentary: Text? }"
    , "data ReviewPatch { stars: W8?, commentary: Text? }"
    , ""
    , "mutation replaceReview(review: ReviewId, content: ReviewInput) returns Review {"
    , "  allow       public"
    , "  writes      Review(review), reviews(Review.episode)"
    , "  invalidates writes"
    , "  effect      natural \"PUT is set-to-state: replaying a replacement is a no-op\""
    , "  as          PUT /e/Review/{review}"
    , "}"
    , ""
    , "mutation editReview(review: ReviewId, patch: ReviewPatch) returns Review {"
    , "  allow       public"
    , "  writes      Review(review)"
    , "  invalidates writes"
    , "  effect      natural \"merge-patch: reapplying the same patch is a no-op\""
    , "  as          PATCH /e/Review/{review}"
    , "  batch       best-effort max 100 as PATCH /e/Review"
    , "}"
    , ""
    , "mutation deleteReview(review: ReviewId) returns Review {"
    , "  allow       public"
    , "  writes      Review(review), reviews(Review.episode)"
    , "  invalidates writes"
    , "  effect      natural \"deleting a named entity twice deletes it once\""
    , "  as          DELETE /e/Review/{review}"
    , "  batch       best-effort max 100 as DELETE /e/Review"
    , "}"
    , ""
    , "mutation submitReview(episode: Episode, stars: W8, commentary: Text?) returns Review {"
    , "  allow       public"
    , "  writes      Review(new), reviews(episode)"
    , "  invalidates writes"
    , "  effect      transactional"
    , "  batch       best-effort max 100"
    , "}"
    ]


{- | Demo-only derived-field surface (spec §3.7), appended like
'starWarsExtensions' so the fixture golden stays byte-identical. @Saga@ is
keyed by the episode name; its collection groups Reviews by @episode@, so
the collection tag @Saga.sagaReviews:\<episode\>@ names the owning row —
which is what lets the maintained relay recover the owner from a purge
key, and lets @addSagaReview@'s write set purge both aggregates.
-}
starWarsDerivedExtensions :: Text
starWarsDerivedExtensions =
  T.unlines
    [ ""
    , "-- Derived-field demo surface (spec §3.7), appended by the example server."
    , ""
    , "entity Saga by id {"
    , "  visible to all by default"
    , ""
    , "  id:    Text"
    , "  title: Text"
    , ""
    , "  has many sagaReviews: Review by episode"
    , ""
    , "  reviewCount: W32 derived reads sagaReviews count on read"
    , "  starTotal:   I32 derived reads sagaReviews sum(stars) maintained"
    , ""
    , "  fetch by id: public"
    , "}"
    , ""
    , "mutation addSagaReview(episode: Episode, stars: W8, commentary: Text?) returns Review {"
    , "  allow       public"
    , "  writes      Review(new), reviews(episode), Saga.sagaReviews(episode)"
    , "  invalidates writes"
    , "  effect      transactional"
    , "}"
    ]


-- ---------------------------------------------------------------------------
-- Seed data
-- ---------------------------------------------------------------------------

-- | A fresh database seeded with the classic characters and a few reviews.
newStarWarsDb :: Schema -> IO MemoryDb
newStarWarsDb schema = do
  db <- newMemoryDb
  atomically (mapM_ (seedEntity schema db) seedRows)
  pure db


seedEntity :: Schema -> MemoryDb -> (TypeName, Map FieldName A.Value) -> STM ()
seedEntity schema db (ty, fields) =
  case entityRowKey schema ty fields of
    Just key -> putRow db ty key fields
    Nothing -> error ("starwars seed row for " <> T.unpack (unTypeName ty) <> " lacks its key field")


seedRows :: [(TypeName, Map FieldName A.Value)]
seedRows = humans <> droids <> starships <> reviews <> sagas
  where
    allEpisodes = A.toJSON (["NewHope", "Empire", "Jedi"] :: [Text])
    friendIds fs = A.toJSON (fs :: [Text])

    humans =
      [ human "1000" "Luke Skywalker" (Just "Tatooine") ["Human:1002", "Human:1003", "Droid:2000", "Droid:2001"]
      , human "1001" "Darth Vader" (Just "Tatooine") []
      , human "1002" "Han Solo" Nothing ["Human:1000", "Human:1003", "Droid:2001"]
      , human "1003" "Leia Organa" (Just "Alderaan") ["Human:1000", "Human:1002", "Droid:2000", "Droid:2001"]
      ]
    human key name homePlanet friends =
      ( "Human"
      , Map.fromList $
          [ ("id", A.String key)
          , ("name", A.String name)
          , ("appearsIn", allEpisodes)
          , ("friendIds", friendIds friends)
          ]
            <> maybe [] (\hp -> [("homePlanet", A.String hp)]) homePlanet
      )

    droids =
      [ droid "2000" "C-3PO" "Protocol" ["Human:1000", "Human:1002", "Human:1003", "Droid:2001"]
      , droid "2001" "R2-D2" "Astromech" ["Human:1000", "Human:1002", "Human:1003"]
      ]
    droid key name fn friends =
      ( "Droid"
      , Map.fromList
          [ ("id", A.String key)
          , ("name", A.String name)
          , ("primaryFunction", A.String fn)
          , ("appearsIn", allEpisodes)
          , ("friendIds", friendIds friends)
          ]
      )

    starships =
      [ starship "3000" "Millennium Falcon" 34.37
      , starship "3001" "X-Wing" 12.5
      , starship "3002" "Star Destroyer" 1600
      ]
    starship key name len =
      ( "Starship"
      , Map.fromList
          [ ("id", A.String key)
          , ("name", A.String name)
          , ("length", A.Number len)
          ]
      )

    reviews =
      [ review "5001" "NewHope" 5 (Just "A classic.") "2024-05-01T10:00:00Z"
      , review "5002" "NewHope" 4 (Just "Great pacing.") "2024-05-02T11:30:00Z"
      , review "5003" "NewHope" 5 Nothing "2024-05-03T09:15:00Z"
      , review "5004" "Empire" 5 (Just "The best one.") "2024-05-04T14:00:00Z"
      , review "5005" "Empire" 5 (Just "Dark and perfect.") "2024-05-05T16:45:00Z"
      , review "5006" "Empire" 4 Nothing "2024-05-06T08:20:00Z"
      , review "5007" "Jedi" 4 (Just "Ewoks divide opinion.") "2024-05-07T12:00:00Z"
      , review "5008" "Jedi" 3 (Just "A solid ending.") "2024-05-08T18:30:00Z"
      ]
    review key episode stars commentary createdAt =
      ( "Review"
      , Map.fromList $
          [ ("id", A.String key)
          , ("episode", A.String episode)
          , ("stars", A.Number stars)
          , ("createdAt", A.String createdAt)
          ]
            <> maybe [] (\c -> [("commentary", A.String c)]) commentary
      )

    -- One Saga row per episode. @starTotal@ (maintained, §3.7) is seeded
    -- deliberately stale at 0: the first triggering write converges it to
    -- the full recomputed aggregate, which the demo observes.
    sagas =
      [ saga "NewHope" "A New Hope"
      , saga "Empire" "The Empire Strikes Back"
      , saga "Jedi" "Return of the Jedi"
      ]
    saga key title =
      ( "Saga"
      , Map.fromList
          [ ("id", A.String key)
          , ("title", A.String title)
          , ("starTotal", A.Number 0)
          ]
      )


-- ---------------------------------------------------------------------------
-- Hooks
-- ---------------------------------------------------------------------------

starWarsHooks :: Schema -> MemoryHooks
starWarsHooks schema =
  defaultHooks
    { mhGetRoots = Map.singleton "hero" heroRoot
    , mhListOverrides = Map.singleton "search" searchRoot
    , mhChildrenOverrides =
        Map.fromList
          [ (("Human", "friends"), friendsChildren schema)
          , (("Droid", "friends"), friendsChildren schema)
          ]
    , mhMutations =
        Map.fromList
          [ ("createReview", createReview)
          , ("submitReview", createReview)
          , ("replaceReview", replaceReview)
          , ("editReview", editReview)
          , ("deleteReview", deleteReview)
          , ("addSagaReview", addSagaReview)
          ]
    }


-- | @hero(episode:)@: R2-D2 unless asked about @Empire@, whose hero is Luke.
heroRoot :: MemoryDb -> Map ArgName A.Value -> IO (Maybe Ref)
heroRoot _db args = pure $ case Map.lookup "episode" args of
  Just (A.String "Empire") -> Just (Ref "Human" "1000")
  _ -> Just (Ref "Droid" "2001")


{- | @search(text:)@: case-insensitive substring on @name@ across the three
tables, ordered by name. Paginated shape but cursor-less: @next@\/@prev@
are always null.
-}
searchRoot :: MemoryDb -> Map ArgName A.Value -> Window -> IO (Either BackendFailure Page)
searchRoot db args window = do
  let needle = T.toLower $ case Map.lookup "text" args of
        Just (A.String t) -> t
        _ -> ""
  hits <- atomically $ do
    found <- traverse (searchTable needle) ["Human", "Droid", "Starship"]
    pure (concat found)
  let cap = fromIntegral $ case window of
        WWhole n -> n
        WPage {wCount} -> wCount
  pure . Right $
    Page
      { pageRefs = take cap (map snd (sortOn fst hits))
      , pageNext = Nothing
      , pagePrev = Nothing
      , pageTotal = Nothing
      , pageOverflow = False
      }
  where
    searchTable needle ty = do
      rows <- tableRows db ty
      let hit (key, row) = case Map.lookup "name" (rowFields row) of
            Just (A.String n)
              | needle `T.isInfixOf` T.toLower n ->
                  Just (n, Ref ty key)
            _ -> Nothing
      pure (mapMaybe hit (Map.toList rows))


{- | The @friends@ edge: resolve each parent's @friendIds@ (ref strings) to
live targets and page them by target name via 'pageFromRows', so cursors
behave exactly like the generic machinery's.
-}
friendsChildren :: Schema -> MemoryDb -> [(Ref, EntityRow)] -> Window -> IO (Map Ref (Either BackendFailure Page))
friendsChildren schema db parents window = do
  tables <- atomically $ do
    humans <- tableRows db "Human"
    droids <- tableRows db "Droid"
    pure (Map.fromList [("Human" :: TypeName, humans), ("Droid", droids)])
  let nameOf ref = do
        table <- Map.lookup (refType ref) tables
        row <- Map.lookup (refKey ref) table
        A.String n <- Map.lookup "name" (rowFields row)
        pure n
      resolve v = do
        A.String refText <- Just v
        ref <- parseRef refText
        n <- nameOf ref
        pure (ref, Map.singleton "name" (A.String n))
      friendRows parentRow = case Map.lookup "friendIds" (rowFields parentRow) of
        Just (A.Array ids) -> mapMaybe resolve (toList ids)
        _ -> []
      pageFor (_, parentRow) =
        pageFromRows schema ["Human", "Droid"] (friendsWindowing schema) window (friendRows parentRow)
  pure (Map.fromList (map (\p -> (fst p, Right (pageFor p))) parents))


-- | The declared windowing of the @friends@ collection (both entities share it).
friendsWindowing :: Schema -> Windowing
friendsWindowing schema =
  case lookupEntity schema "Human" >>= (`lookupEntityRel` "friends") of
    Just ToMany {relCollection} -> colWindow relCollection
    _ -> error "starwars fixture: Human.friends collection missing"


{- | @createReview(episode, stars, commentary?)@: insert a Review under the
next sequential id.
-}
createReview :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
createReview db _claims args =
  case (Map.lookup "episode" args, Map.lookup "stars" args) of
    (Just episode@(A.String episodeText), Just stars@(A.Number _)) -> do
      now <- getCurrentTime
      let createdAt = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
      atomically $ do
        existing <- tableRows db "Review"
        let keyNum k = fromMaybe 0 (readMaybe (T.unpack k)) :: Integer
            nextId = T.pack (show (1 + Map.foldlWithKey' (\acc k _ -> max acc (keyNum k)) 5000 existing))
            ref = Ref "Review" nextId
            fields =
              Map.fromList $
                [ ("id", A.String nextId)
                , ("episode", episode)
                , ("stars", stars)
                , ("createdAt", A.String createdAt)
                ]
                  <> commentaryField
            commentaryField = case Map.lookup "commentary" args of
              Just c@(A.String _) -> [("commentary", c)]
              _ -> []
        putRow db "Review" nextId fields
        tok <- snapshotToken db
        pure . MutationCommitted $
          CommitResult
            { crResult = [ref]
            , crWrites = [WroteEntity ref, WroteCollection "reviews" [episodeText]]
            , crSnapshot = tok
            }
    _ -> pure (MutationFailed (internalError (Just "createReview: episode and stars arguments required")))


{- | @addSagaReview(episode, stars, commentary?)@: 'createReview' plus the
@Saga.sagaReviews@ collection write fact, so the invalidation event
carries the tag the maintained @Saga.starTotal@ derivation (§3.7) is
keyed on — the relay recovers the owning Saga row from it.
-}
addSagaReview :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
addSagaReview db claims args = do
  out <- createReview db claims args
  pure $ case out of
    MutationCommitted cr ->
      MutationCommitted cr {crWrites = crWrites cr <> concatMap sagaFact (crWrites cr)}
    other -> other
  where
    sagaFact = \case
      WroteCollection "reviews" gs -> [WroteCollection "Saga.sagaReviews" gs]
      _ -> []


-- ---------------------------------------------------------------------------
-- Verb-bound demo mutations (§11.7)
-- ---------------------------------------------------------------------------

-- | The @review@ key argument (URL segment or inline batch key) as
-- canonical key text.
reviewKeyArg :: Map ArgName A.Value -> Maybe Text
reviewKeyArg args = renderScalarKey <$> Map.lookup "review" args


episodeOf :: Maybe A.Value -> Maybe Text
episodeOf = \case
  Just (A.String t) -> Just t
  _ -> Nothing


nowText :: IO Text
nowText = do
  now <- getCurrentTime
  pure (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))


{- | @replaceReview(review, content)@ — @as PUT \/e\/Review\/{review}@: full
replacement (set-to-state). @createdAt@ is not part of the representation:
it survives from the prior row, or is minted on a create-if-absent PUT.
Membership facts cover both the prior and the new episode.
-}
replaceReview :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
replaceReview db _claims args =
  case (reviewKeyArg args, Map.lookup "content" args) of
    (Just key, Just (A.Object content))
      | Just episode <- KM.lookup "episode" content
      , Just stars <- KM.lookup "stars" content -> do
          now <- nowText
          atomically $ do
            prior <- readRow db "Review" key
            let priorFields = case prior of
                  RowFound r -> rowFields r
                  _ -> Map.empty
                createdAt = fromMaybe (A.String now) (Map.lookup "createdAt" priorFields)
                fields =
                  Map.fromList $
                    [ ("id", A.String key)
                    , ("episode", episode)
                    , ("stars", stars)
                    , ("createdAt", createdAt)
                    ]
                      <> (case KM.lookup "commentary" content of
                            Just c@(A.String _) -> [("commentary", c)]
                            _ -> [])
                ref = Ref "Review" key
                episodes =
                  dedup (mapMaybe episodeOf [Map.lookup "episode" priorFields, Just episode])
            putRow db "Review" key fields
            tok <- snapshotToken db
            pure . MutationCommitted $
              CommitResult
                { crResult = [ref]
                , crWrites = WroteEntity ref : map (\ep -> WroteCollection "reviews" [ep]) episodes
                , crSnapshot = tok
                }
    _ -> pure (MutationFailed (internalError (Just "replaceReview: review and content (episode, stars) required")))
  where
    dedup = foldr (\x acc -> if elem x acc then acc else x : acc) []


{- | @editReview(review, patch)@ — @as PATCH \/e\/Review\/{review}@:
merge-patch semantics — present fields replace, @null@ clears optional
fields, absent fields are untouched.
-}
editReview :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
editReview db _claims args =
  case (reviewKeyArg args, Map.lookup "patch" args) of
    (Just key, Just (A.Object patch)) ->
      atomically $ do
        prior <- readRow db "Review" key
        case prior of
          RowFound r -> do
            let apply fields (k, v) = case v of
                  A.Null -> Map.delete (FieldName (AK.toText k)) fields
                  _ -> Map.insert (FieldName (AK.toText k)) v fields
                merged = foldl' apply (rowFields r) (KM.toList patch)
                ref = Ref "Review" key
            putRow db "Review" key merged
            tok <- snapshotToken db
            pure . MutationCommitted $
              CommitResult
                { crResult = [ref]
                , crWrites = [WroteEntity ref]
                , crSnapshot = tok
                }
          _ -> pure (MutationFailed (internalError (Just "editReview: no such review")))
    _ -> pure (MutationFailed (internalError (Just "editReview: review and patch arguments required")))


{- | @deleteReview(review)@ — @as DELETE \/e\/Review\/{review}@: tombstone
the row (idempotent; deleting an absent review still records the
tombstone). The response carries the tombstone via the @DeletedEntity@
fact.
-}
deleteReview :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
deleteReview db _claims args =
  case reviewKeyArg args of
    Just key ->
      atomically $ do
        prior <- readRow db "Review" key
        let oldEp = case prior of
              RowFound r -> episodeOf (Map.lookup "episode" (rowFields r))
              _ -> Nothing
            ref = Ref "Review" key
        deleteRow db "Review" key
        after <- readRow db "Review" key
        let tv = case after of
              RowTombstone v -> v
              _ -> "t:1"
        tok <- snapshotToken db
        pure . MutationCommitted $
          CommitResult
            { crResult = [ref]
            , crWrites =
                DeletedEntity ref tv
                  : maybe [] (\ep -> [WroteCollection "reviews" [ep]]) oldEp
            , crSnapshot = tok
            }
    Nothing -> pure (MutationFailed (internalError (Just "deleteReview: review argument required")))
