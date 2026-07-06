{- | The pure compatibility checker (spec §17): 'diffSchemas'
classification along the four §17.2 axes, the §17.3 direction modes and
transitive windows, the §17.4 corpus-weighted report, @\@break@
overrides, and the §17.5 deprecation lifecycle.

Every classification test edits exactly one declaration of one shared
baseline ('compatBase') and names its subject, so a regression points at
the axis rule that moved, not at a fixture diff.
-}
module Test.Lattice.Compat (tests) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Lattice.Canonical (Compiled (..))
import Lattice.Compat
import Lattice.IDL.Parser (parseSchema)
import Lattice.IDL.Print (canonicalIdl)
import Lattice.Registry (CorpusEntry (..))
import Lattice.Schema (Deprecation (..), Schema, defaultBudgets, schemaDeprecations)
import Test.Lattice.Fixtures (mustCompileWith, mustParseSchema)
import Test.Syd


tests :: Spec
tests =
  describe "Schema compatibility (§17)" $ do
    describe "§17.2 change taxonomy: compile axis" $ do
      it "removing a field is a compile break naming Thing.label" $ do
        c <- soleChangeFor "Thing.label" (diffBase (without "label:  Text"))
        (chAxis c, chBreaking c) `shouldBe` (AxisCompile, True)

      it "changing a field's type is a compile break" $ do
        c <- soleChangeFor "Thing.label" (diffBase (edited "label:  Text" ["  label:  I32"]))
        (chAxis c, chBreaking c) `shouldBe` (AxisCompile, True)

      it "removing a root is a compile break naming thing" $ do
        c <- soleChangeFor "thing" (diffBase (without "get thing(id: ThingId) of Thing public"))
        (chAxis c, chBreaking c) `shouldBe` (AxisCompile, True)

      it "removing a fragment is a compile break naming ThingCard" $ do
        c <- soleChangeFor "ThingCard" (diffBase (without "fragment ThingCard on Thing { label }"))
        (chAxis c, chBreaking c) `shouldBe` (AxisCompile, True)

      it "removing a mutation is a compile break naming editThing" $ do
        cs <- pure (diffBase candidateNoMutation)
        c <- soleChangeFor "editThing" cs
        (chAxis c, chBreaking c) `shouldBe` (AxisCompile, True)

      it "an argument-default change compiles everywhere and is STILL a break (identity, §5.1)" $ do
        c <- soleChangeFor "Thing.score" (diffBase (edited "score(scale: I32 = 10): I32" ["  score(scale: I32 = 12): I32"]))
        (chAxis c, chBreaking c) `shouldBe` (AxisCompile, True)

      it "additive changes are reported non-breaking: new field, new optional arg with default, new root, new fragment" $ do
        let cs = diffBase candidateAdditive
        cs `shouldSatisfy` (not . null)
        map chBreaking cs `shouldBe` map (const False) cs

    describe "§17.2 plan and semantic axes: policy changes" $ do
      it "widening a policy (private -> public) moves plans, breaks nothing" $ do
        let cs = diffBase (edited "note:   Text?  private" ["  note:   Text?"])
        map chAxis cs `shouldSatisfy` elem AxisPlan
        map chBreaking cs `shouldBe` map (const False) cs

      it "narrowing a policy (public -> private) is plan-moving AND a semantic break" $ do
        let cs = changesFor "Thing.label" (diffBase (edited "label:  Text" ["  label:  Text  private"]))
        length cs `shouldBe` 2
        map chAxis cs `shouldSatisfy` elem AxisPlan
        map chAxis cs `shouldSatisfy` elem AxisSemantic
        [chBreaking c | c <- cs, chAxis c == AxisSemantic] `shouldBe` [True]
        [chBreaking c | c <- cs, chAxis c == AxisPlan] `shouldBe` [False]

    describe "§17.2 cursor axis" $ do
      it "flipping an index ordering is a cursor break naming Thing.items" $ do
        let cs = changesFor "Thing.items" (diffBase (edited "ordered by rank asc" ["                  ordered by rank desc"]))
        cs `shouldSatisfy` any (\c -> chAxis c == AxisCursor && chBreaking c)

      it "a non-append open-enum edit is a cursor break AND a semantic break (Status)" $ do
        let cs = changesFor "Status" (diffBase (edited "enum Status open = Draft | Live" ["enum Status open = Draft | Frozen | Live"]))
        let shapes = map (\c -> (chAxis c, chBreaking c)) cs
        length shapes `shouldBe` 2
        shapes `shouldSatisfy` elem (AxisCursor, True)
        shapes `shouldSatisfy` elem (AxisSemantic, True)

      it "appending to an open enum is non-breaking" $ do
        let cs = diffBase (edited "enum Status open = Draft | Live" ["enum Status open = Draft | Live | Archived"])
        map chBreaking cs `shouldBe` map (const False) cs

    describe "§17.2 semantic axis: batch shape" $ do
      it "lowering maxItems is a semantic break naming editThing" $ do
        let cs = changesFor "editThing" (diffBase (edited "batch       best-effort max 50" ["  batch       best-effort max 20"]))
        cs `shouldSatisfy` any (\c -> chAxis c == AxisSemantic && chBreaking c)

      it "raising maxItems is additive" $ do
        let cs = diffBase (edited "batch       best-effort max 50" ["  batch       best-effort max 100"])
        map chBreaking cs `shouldBe` map (const False) cs

      it "flipping atomicity is a semantic break (partial failure changes meaning)" $ do
        let cs = changesFor "editThing" (diffBase (edited "batch       best-effort max 50" ["  batch       all-or-nothing max 50"]))
        cs `shouldSatisfy` any (\c -> chAxis c == AxisSemantic && chBreaking c)

    describe "@break overrides (§17.3)" $ do
      it "@break(approved: ...) on the surviving enclosing declaration clears the removal" $ do
        c <- soleChangeFor "Thing.label" (diffSchemas compatBase candidateOverridden)
        chOverride c `shouldBe` Just "TICKET-123"
        let rep = runCheck ClientBackward now2026 [logged t2026 compatBase] [] candidateOverridden
        repPass rep `shouldBe` True

      it "the same removal without the annotation fails the check" $ do
        let rep = runCheck ClientBackward now2026 [logged t2026 compatBase] [] (without "label:  Text")
        repPass rep `shouldBe` False

    describe "@deprecated (§17.5)" $ do
      it "parses, prints canonically (fixpoint), and surfaces in the schema model" $ do
        parseSchema (canonicalIdl deprecatedBase) `shouldBe` Right deprecatedBase
        fmap canonicalIdl (parseSchema (canonicalIdl deprecatedBase))
          `shouldBe` Right (canonicalIdl deprecatedBase)
        case map (\d -> (depSunset d, depNote d)) (elemsOfDeprecations deprecatedBase) of
          [(s, n)] -> do
            s `shouldBe` fromGregorian 2027 1 1
            n `shouldBe` "use headline"
          other -> expectationFailure ("expected exactly one deprecation, got: " <> show other)

      it "removal of a deprecated element fails before sunset and passes at it (checker takes now)" $ do
        let candidate = withoutFrom deprecatedLines "@deprecated(sunset: \"2027-01-01\", note: \"use headline\") label:  Text"
            before = runCheck ClientBackward (utc 2026 7 1) [logged (utc 2026 1 1) deprecatedBase] [] candidate
            atSunset = runCheck ClientBackward (utc 2027 1 1) [logged (utc 2026 1 1) deprecatedBase] [] candidate
        repPass before `shouldBe` False
        repPass atSunset `shouldBe` True

    describe "§17.3 directions and windows" $ do
      it "parseCheckMode covers the mode vocabulary" $ do
        parseCheckMode "client-backward" `shouldBe` Just (ClientBackward, False)
        parseCheckMode "client-backward-transitive" `shouldBe` Just (ClientBackward, True)
        parseCheckMode "server-forward" `shouldBe` Just (ServerForward, False)
        parseCheckMode "server-forward-transitive" `shouldBe` Just (ServerForward, True)
        parseCheckMode "full" `shouldBe` Just (Full, False)
        parseCheckMode "sideways" `shouldBe` Nothing

      it "parseWindow parses the P<n>D/M/Y subset and windowStart subtracts it" $ do
        fmap (windowStart (utc 2026 7 1)) (parseWindow "P18M") `shouldBe` Just (utc 2025 1 1)
        fmap (windowStart (utc 2026 7 31)) (parseWindow "P30D") `shouldBe` Just (utc 2026 7 1)
        fmap (windowStart (utc 2026 7 1)) (parseWindow "P2Y") `shouldBe` Just (utc 2024 7 1)
        parseWindow "18M" `shouldBe` Nothing
        parseWindow "P18" `shouldBe` Nothing
        parseWindow "P18W" `shouldBe` Nothing

      it "SERVER_FORWARD is the reversed diff: a candidate-only field breaks it but not CLIENT_BACKWARD" $ do
        let candidate = mustParseSchema (T.unlines (editLines baseLines [("label:  Text", ["  label:  Text", "  extra:  Text"])]))
            fwd = runCheck ServerForward now2026 [logged t2026 compatBase] [] candidate
            back = runCheck ClientBackward now2026 [logged t2026 compatBase] [] candidate
        repPass back `shouldBe` True
        repPass fwd `shouldBe` False

      it "a transitive check runs against every logged schema inside the window only (3-entry log)" $ do
        -- The 2023 deployment is the only baseline that still carries
        -- `old`; the candidate (= current base) removed it since.
        let logThree =
              [ logged (utc 2026 6 1) compatBase
              , logged (utc 2026 1 1) compatBase
              , logged (utc 2023 1 1) oldSchema
              ]
            narrow = runTransitive (parseWindowOrBust "P18M") now2026 logThree compatBase
            wide = runTransitive (parseWindowOrBust "P4Y") now2026 logThree compatBase
        repPass narrow `shouldBe` True
        length (repBaselines narrow) `shouldBe` 2
        repPass wide `shouldBe` False
        length (repBaselines wide) `shouldBe` 3

    describe "§17.4 corpus-aware report" $ do
      it "a breaking removal carries the affected texts, aggregate hits, and newest client" $ do
        labelQ <- compiledText <$> mustCompileWith compatBase "query Q { thing(id: \"t1\") { label } }"
        otherQ <- compiledText <$> mustCompileWith compatBase "query R { thing(id: \"t1\") { note } }"
        let corpus =
              [ CorpusEntry {ceText = labelQ, ceHits = 7, ceClients = ["app-ios/3.19.2"]}
              , CorpusEntry {ceText = otherQ, ceHits = 100, ceClients = ["app-android/4.0.0"]}
              ]
            rep = runCheck ClientBackward now2026 [logged t2026 compatBase] corpus (without "label:  Text")
        repPass rep `shouldBe` False
        case [rc | rc <- repChanges rep, chSubject (rcChange rc) == "Thing.label"] of
          [rc] -> do
            ciTexts (rcCorpus rc) `shouldBe` [labelQ]
            ciAggregateHits (rcCorpus rc) `shouldBe` 7
            ciNewestClient (rcCorpus rc) `shouldBe` Just "app-ios/3.19.2"
          other -> expectationFailure ("expected one reported Thing.label change, got: " <> show (length other))

      it "the report JSON carries the pinned keys and axis vocabulary" $ do
        let rep = runCheck ClientBackward now2026 [logged t2026 compatBase] [] (without "label:  Text")
        case A.toJSON rep of
          A.Object o -> do
            KM.lookup "pass" o `shouldBe` Just (A.Bool False)
            KM.lookup "mode" o `shouldSatisfy` (/= Nothing)
            KM.lookup "transitive" o `shouldBe` Just (A.Bool False)
            KM.lookup "baselines" o `shouldSatisfy` (/= Nothing)
            case KM.lookup "changes" o of
              Just (A.Array cs) -> do
                let axes = [ax | A.Object c <- foldr (:) [] cs, Just (A.String ax) <- [KM.lookup "axis" c]]
                axes `shouldSatisfy` elem "compile"
              other -> expectationFailure ("expected a changes array, got: " <> show other)
          other -> expectationFailure ("expected a report object, got: " <> show other)


-- ---------------------------------------------------------------------------
-- The shared baseline and its one-line variants
-- ---------------------------------------------------------------------------

{- | One small schema exercising every §17.2 axis site: an open enum, a
defaulted field argument, a policy site, a cursor spec, a fragment, a
root, and a batched mutation.
-}
baseLines :: [Text]
baseLines =
  [ "schema compat.example.com"
  , ""
  , "newtype ThingId = Text"
  , ""
  , "enum Status open = Draft | Live"
  , ""
  , "entity Thing by id {"
  , "  visible to all by default"
  , ""
  , "  id:     ThingId"
  , "  label:  Text"
  , "  status: Status"
  , "  score(scale: I32 = 10): I32"
  , "  note:   Text?  private"
  , ""
  , "  has many items: Item by thingId"
  , "                  ordered by rank asc"
  , "                  page 10 max 100"
  , ""
  , "  fetch by id: public"
  , "}"
  , ""
  , "entity Item by id {"
  , "  visible to all by default"
  , ""
  , "  id:      Text"
  , "  thingId: ThingId"
  , "  rank:    I32"
  , ""
  , "  fetch by id: public"
  , "}"
  , ""
  , "fragment ThingCard on Thing { label }"
  , ""
  , "get thing(id: ThingId) of Thing public"
  , ""
  , "mutation editThing(thing: ThingId, label: Text) returns Thing {"
  , "  allow       public"
  , "  writes      Thing(thing)"
  , "  invalidates writes"
  , "  effect      transactional"
  , "  batch       best-effort max 50"
  , "}"
  ]


compatBase :: Schema
compatBase = mustParseSchema (T.unlines baseLines)
{-# NOINLINE compatBase #-}


{- | Apply exact-line edits (matched on the stripped line) to a line set;
every edit must hit exactly once, so a fixture drift fails loudly here
rather than silently testing nothing.
-}
editLines :: [Text] -> [(Text, [Text])] -> [Text]
editLines src edits = go src edits
  where
    go ls [] = ls
    go ls ((from, to) : rest) = case break ((== T.strip from) . T.strip) ls of
      (pre, _hit : post) -> go (pre <> to <> post) rest
      (_, []) -> error ("editLines: no line matches " <> T.unpack from)


-- | The base with one line replaced.
edited :: Text -> [Text] -> Schema
edited from to = mustParseSchema (T.unlines (editLines baseLines [(from, to)]))


-- | The base with one line removed.
without :: Text -> Schema
without from = mustParseSchema (T.unlines (editLines baseLines [(from, [])]))


withoutFrom :: [Text] -> Text -> Schema
withoutFrom src from = mustParseSchema (T.unlines (editLines src [(from, [])]))


-- | The base minus the whole mutation block.
candidateNoMutation :: Schema
candidateNoMutation =
  mustParseSchema . T.unlines $
    takeWhile (/= "mutation editThing(thing: ThingId, label: Text) returns Thing {") baseLines


-- | Purely additive candidate: a new field, a new defaulted argument,
-- a new root, and a new fragment.
candidateAdditive :: Schema
candidateAdditive =
  mustParseSchema . T.unlines $
    editLines
      baseLines
      [ ("label:  Text", ["  label:  Text", "  blurb:  Text"])
      , ("score(scale: I32 = 10): I32", ["  score(scale: I32 = 10, pad: I32 = 0): I32"])
      ,
        ( "get thing(id: ThingId) of Thing public"
        ,
          [ "get thing(id: ThingId) of Thing public"
          , ""
          , "get item(id: Text) of Item public"
          , ""
          , "fragment ThingBlurb on Thing { blurb }"
          ]
        )
      ]


-- | Removes Thing.label AND carries the approval on the surviving
-- enclosing entity (the pinned override site for field removals).
candidateOverridden :: Schema
candidateOverridden =
  mustParseSchema . T.unlines $
    editLines
      baseLines
      [ ("label:  Text", [])
      , ("entity Thing by id {", ["@break(approved: \"TICKET-123\")", "entity Thing by id {"])
      ]


-- | The base with @label@ deprecated (sunset 2027-01-01).
deprecatedLines :: [Text]
deprecatedLines =
  editLines
    baseLines
    [("label:  Text", ["  @deprecated(sunset: \"2027-01-01\", note: \"use headline\") label:  Text"])]


deprecatedBase :: Schema
deprecatedBase = mustParseSchema (T.unlines deprecatedLines)
{-# NOINLINE deprecatedBase #-}


-- | What the 2023 deployment looked like: it still had `old`.
oldSchema :: Schema
oldSchema = edited "label:  Text" ["  label:  Text", "  old:    Text"]


-- ---------------------------------------------------------------------------
-- Runners and assertions
-- ---------------------------------------------------------------------------

diffBase :: Schema -> [Change]
diffBase = diffSchemas compatBase


changesFor :: Text -> [Change] -> [Change]
changesFor subj = filter ((== subj) . chSubject)


-- | Exactly one change names the subject.
soleChangeFor :: Text -> [Change] -> IO Change
soleChangeFor subj cs = case changesFor subj cs of
  [c] -> pure c
  other ->
    expectationFailure
      ("expected exactly one change for " <> T.unpack subj <> ", got " <> show (length other) <> " (of " <> show (map chSubject cs) <> ")")


runCheck :: CheckMode -> UTCTime -> [LoggedSchema] -> [CorpusEntry] -> Schema -> Report
runCheck mode now logd corpus candidate =
  checkSchemas
    CheckConfig
      { ccMode = mode
      , ccTransitive = False
      , ccWindow = Nothing
      , ccNow = now
      , ccBudgets = defaultBudgets
      }
    logd
    corpus
    candidate


runTransitive :: Window -> UTCTime -> [LoggedSchema] -> Schema -> Report
runTransitive win now logd candidate =
  checkSchemas
    CheckConfig
      { ccMode = ClientBackward
      , ccTransitive = True
      , ccWindow = Just win
      , ccNow = now
      , ccBudgets = defaultBudgets
      }
    logd
    []
    candidate


logged :: UTCTime -> Schema -> LoggedSchema
logged t s = LoggedSchema {lsDeployedAt = t, lsHash = "h:" <> T.pack (show t), lsSchema = s}


utc :: Integer -> Int -> Int -> UTCTime
utc y m d = UTCTime (fromGregorian y m d) 0


now2026 :: UTCTime
now2026 = utc 2026 7 1


t2026 :: UTCTime
t2026 = utc 2026 1 1


parseWindowOrBust :: Text -> Window
parseWindowOrBust t = maybe (error ("bad test window: " <> T.unpack t)) id (parseWindow t)


elemsOfDeprecations :: Schema -> [Deprecation]
elemsOfDeprecations = foldr (:) [] . schemaDeprecations
