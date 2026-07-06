{- | Verb bindings over loopback HTTP (spec §11.7/§11.8): routing,
conditional requests, merge-patch semantics, creation, bound batches, and
the named form's continued availability.

Everything speaks raw HTTP ('httpRaw'): "Lattice.Client" has no verb
surface, and the point of these tests is the wire contract — status
lines, precondition headers, entity-stream bodies, @Location@.
-}
module Test.Lattice.VerbsE2E (tests) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Lattice.Backend
import Lattice.Backend.Memory
import Lattice.Client (mutate)
import Lattice.Types
import Lattice.Wire (EntityRecord (..), Record (..), hIdempotencyReplayed)
import Network.HTTP.Types.Header (HeaderName, hCacheControl, hIfMatch, hIfNoneMatch, hLocation)
import Network.HTTP.Types.Method (Method (..))
import Test.Lattice.Fixtures (verbsSchema)
import Test.Lattice.Loop
import Test.Syd


tests :: Spec
tests =
  describe "Verb bindings over loopback HTTP (§11.7/§11.8)" $ do
    describe "§11.7 conditional requests are the concurrency control" $ do
      it "a bare non-lww PUT is 428 Precondition Required; nothing written" $
        withVerbs $ \loop -> do
          r <- putNote loop "n1" [] (A.object [("title", A.String "Blind")])
          rawStatus r `shouldBe` 428
          fetchTitle loop "n1" >>= (`shouldBe` Just "Title n1")

      it "PUT with a matching If-Match replaces and bumps ver" $
        withVerbs $ \loop -> do
          r <- putNote loop "n1" [(hIfMatch, "\"e1\"")] (A.object [("title", A.String "Replaced")])
          rawStatus r `shouldBe` 200
          case entityRecordsOf r of
            [er] -> do
              erId er `shouldBe` Ref "Note" "n1"
              erVer er `shouldBe` "e2"
            other -> expectationFailure ("expected exactly one entity record, got: " <> show other)
          fetchTitle loop "n1" >>= (`shouldBe` Just "Replaced")

      it "a stale If-Match is 412 whose body carries CURRENT state, no-store" $
        withVerbs $ \loop -> do
          r <- putNote loop "n1" [(hIfMatch, "\"e9\"")] (A.object [("title", A.String "Lost race")])
          rawStatus r `shouldBe` 412
          rawHeader hCacheControl r `shouldSatisfy` maybe False ("no-store" `BS8.isInfixOf`)
          -- §11.7: the 412 body is an entity stream with current state,
          -- so the racing client rebases with no follow-up fetch.
          case entityRecordsOf r of
            [er] -> do
              erId er `shouldBe` Ref "Note" "n1"
              erVer er `shouldBe` "e1"
              Map.lookup "title" (erFields er) `shouldBe` Just (A.String "Title n1")
            other -> expectationFailure ("expected current state in the 412, got: " <> show other)
          fetchTitle loop "n1" >>= (`shouldBe` Just "Title n1")

      it "If-None-Match: * on an absent key creates (201); on an existing key 412s" $
        withVerbs $ \loop -> do
          r <- putNote loop "n9" [(hIfNoneMatch, "*")] (A.object [("title", A.String "Fresh")])
          rawStatus r `shouldBe` 201
          fetchTitle loop "n9" >>= (`shouldBe` Just "Fresh")
          r2 <- putNote loop "n9" [(hIfNoneMatch, "*")] (A.object [("title", A.String "Clobber")])
          rawStatus r2 `shouldBe` 412
          fetchTitle loop "n9" >>= (`shouldBe` Just "Fresh")

      it "a deleted row fails If-Match (a tombstone is not any live version)" $
        withVerbs $ \loop -> do
          _ <- deleteNote loop "n2" [(hIfMatch, "\"e1\"")]
          r <- putNote loop "n2" [(hIfMatch, "\"e1\"")] (A.object [("title", A.String "Zombie")])
          rawStatus r `shouldBe` 412

    describe "§11.7 PATCH is merge-patch" $ do
      it "present fields replace; absent fields survive" $
        withVerbs $ \loop -> do
          r <- patchNote loop "n1" [(hIfMatch, "\"e1\"")] (A.object [("title", A.String "Edited")])
          rawStatus r `shouldBe` 200
          fetchTitle loop "n1" >>= (`shouldBe` Just "Edited")
          fetchField loop "n1" "body" >>= (`shouldBe` Just (A.String "Body n1"))

      it "null clears an optional field" $
        withVerbs $ \loop -> do
          r <- patchNote loop "n1" [(hIfMatch, "\"e1\"")] (A.object [("body", A.Null)])
          rawStatus r `shouldBe` 200
          fetchField loop "n1" "body" >>= (`shouldBe` Nothing)
          fetchTitle loop "n1" >>= (`shouldBe` Just "Title n1")

      it "the wrong content type is 415" $
        withVerbs $ \loop -> do
          r <-
            httpRaw
              loop
              PATCH
              "/e/Note/n1"
              [("Content-Type", "application/json"), (hIfMatch, "\"e1\"")]
              (Just (bodyOf (A.object [("title", A.String "x")])))
          rawProblem 415 "unsupported-media" r

      it "an undeclared merge-patch field is 400 naming the field" $
        withVerbs $ \loop -> do
          r <- patchNote loop "n1" [(hIfMatch, "\"e1\"")] (A.object [("zap", A.String "?")])
          rawStatus r `shouldBe` 400
          rawBody r `shouldSatisfy` ("zap" `BS8.isInfixOf`)

      it "a bare non-lww PATCH is 428" $
        withVerbs $ \loop -> do
          r <- patchNote loop "n1" [] (A.object [("title", A.String "x")])
          rawStatus r `shouldBe` 428

      it "last-writer-wins suppresses the precondition demand" $
        withVerbs $ \loop -> do
          r <-
            httpRaw
              loop
              PATCH
              "/e/Flag/f1"
              [("Content-Type", mergePatchType)]
              (Just (bodyOf (A.object [("raised", A.Bool True)])))
          rawStatus r `shouldBe` 200
          fetchField' loop "Flag" "f1" "raised" >>= (`shouldBe` Just (A.Bool True))

    describe "§11.7 DELETE and creation POST" $ do
      it "DELETE with If-Match tombstones; the response carries the tombstone; the row 410s" $
        withVerbs $ \loop -> do
          r <- deleteNote loop "n1" [(hIfMatch, "\"e1\"")]
          rawStatus r `shouldBe` 200
          tombstonesOf r `shouldBe` [Ref "Note" "n1"]
          gone <- httpRaw loop GET "/e/Note/n1" [] Nothing
          rawStatus gone `shouldBe` 410

      it "a bare DELETE is 428" $
        withVerbs $ \loop -> do
          r <- deleteNote loop "n1" []
          rawStatus r `shouldBe` 428
          fetchTitle loop "n1" >>= (`shouldBe` Just "Title n1")

      it "creation POST is 201 with Location: /e/Note/{key}" $
        withVerbs $ \loop -> do
          r <- postNotes loop [] (A.object [("title", A.String "Newborn")])
          rawStatus r `shouldBe` 201
          loc <- maybe (expectationFailure "no Location header") pure (rawHeader hLocation r)
          loc `shouldSatisfy` ("/e/Note/" `BS8.isPrefixOf`)
          fresh <- httpRaw loop GET loc [] Nothing
          rawStatus fresh `shouldBe` 200
          case entityRecordsOf fresh of
            [er] -> Map.lookup "title" (erFields er) `shouldBe` Just (A.String "Newborn")
            other -> expectationFailure ("expected the created entity, got: " <> show other)

      it "the Idempotency-Key envelope applies to creation POST unchanged (§11.2)" $
        withVerbs $ \loop -> do
          let hdrs = [("Idempotency-Key", "create-once")]
          r1 <- postNotes loop hdrs (A.object [("title", A.String "Once")])
          rawStatus r1 `shouldBe` 201
          r2 <- postNotes loop hdrs (A.object [("title", A.String "Once")])
          rawHeader hIdempotencyReplayed r2 `shouldBe` Just "true"
          rawHeader hLocation r2 `shouldBe` rawHeader hLocation r1

    describe "§11.8 bound batches" $ do
      it "a collection PATCH applies an array of merge-patches, correlated by item key" $
        withVerbs $ \loop -> do
          r <-
            httpRaw
              loop
              PATCH
              "/e/Note"
              [("Content-Type", mergePatchType)]
              ( Just . bodyOf . A.toJSON $
                  [ A.object [("id", A.String "n1"), ("key", A.String "op_1"), ("title", A.String "B1")]
                  , A.object [("id", A.String "n2"), ("key", A.String "op_2"), ("title", A.String "B2")]
                  ]
              )
          rawStatus r `shouldBe` 200
          let items = mapMaybe itemOf (entityRecordsOf r)
          items `shouldBe` ["op_1", "op_2"]
          fetchTitle loop "n1" >>= (`shouldBe` Just "B1")
          fetchTitle loop "n2" >>= (`shouldBe` Just "B2")

      it "a collection POST with an array body batch-creates under the array-vs-object rule" $
        withVerbs $ \loop -> do
          r <-
            postNotes loop [] . A.toJSON $
              [ A.object [("key", A.String "a"), ("input", A.object [("title", A.String "BatchA")])]
              , A.object [("key", A.String "b"), ("input", A.object [("title", A.String "BatchB")])]
              ]
          rawStatus r `shouldSatisfy` (`elem` [200, 207 :: Int])
          let ers = entityRecordsOf r
          mapMaybe itemOf ers `shouldBe` ["a", "b"]
          titles <- mapM (\er -> pure (Map.lookup "title" (erFields er))) ers
          titles `shouldBe` [Just (A.String "BatchA"), Just (A.String "BatchB")]

      it "a collection DELETE pairs repeated id/key query parameters by position" $
        withVerbs $ \loop -> do
          r <- httpRaw loop DELETE "/e/Note?id=n5&id=n6&key=op_5&key=op_6" [] Nothing
          rawStatus r `shouldBe` 200
          let tombs = mapMaybe tombItemOf (rawRecords r)
          tombs `shouldBe` [(Ref "Note" "n5", Just "op_5"), (Ref "Note" "n6", Just "op_6")]
          gone <- httpRaw loop GET "/e/Note/n5" [] Nothing
          rawStatus gone `shouldBe` 410

      it "more key parameters than ids is 400" $
        withVerbs $ \loop -> do
          r <- httpRaw loop DELETE "/e/Note?id=n5&key=a&key=b" [] Nothing
          rawStatus r `shouldBe` 400

      it "a batch beyond the mutation's max is 400 batch-too-large" $
        withVerbs $ \loop -> do
          let overMax = map (\i -> A.object [("id", A.String ("n" <> tshow i)), ("title", A.String "x")]) [1 .. 101 :: Int]
          r <- httpRaw loop PATCH "/e/Note" [("Content-Type", mergePatchType)] (Just (bodyOf (A.toJSON overMax)))
          rawProblem 400 "batch-too-large" r

      it "a collection PATCH with an object body is 400 (batch bodies are arrays)" $
        withVerbs $ \loop -> do
          r <- httpRaw loop PATCH "/e/Note" [("Content-Type", mergePatchType)] (Just (bodyOf (A.object [("id", A.String "n1")])))
          rawStatus r `shouldBe` 400

    describe "§11.7 the binding chooses the wire spelling only" $ do
      it "a bound mutation stays invocable via POST /m/{name}" $
        withVerbs $ \loop ->
          clientFor loop id $ \lc -> do
            void $
              io "named replaceNote" (mutate lc "replaceNote" (A.object [("note", A.String "n3"), ("input", A.object [("title", A.String "Named")])]) Nothing)
                >>= either (expectationFailure . show) pure
            fetchTitle loop "n3" >>= (`shouldBe` Just "Named")

      it "an unbound verb/URL falls back to 404/405, never dispatches" $
        withVerbs $ \loop -> do
          -- Flag declares only PATCH: PUT and DELETE have no binding.
          r1 <- httpRaw loop PUT "/e/Flag/f1" [(hIfMatch, "\"e1\"")] (Just (bodyOf (A.object [])))
          rawStatus r1 `shouldSatisfy` (`elem` [404, 405 :: Int])
          -- PUT never binds a collection URL (§11.8).
          r2 <- httpRaw loop PUT "/e/Note" [] (Just (bodyOf (A.object [])))
          rawStatus r2 `shouldSatisfy` (`elem` [404, 405 :: Int])
          fetchField' loop "Flag" "f1" "raised" >>= (`shouldBe` Just (A.Bool False))


-- ---------------------------------------------------------------------------
-- Fixture: the verbs origin
-- ---------------------------------------------------------------------------

withVerbs :: (Loop -> IO a) -> IO a
withVerbs action = do
  counter <- newTVarIO (0 :: Int)
  withLoop
    (loopSpec verbsSchema)
      { lsHooks = verbsHooks counter
      , lsRows = verbRows
      }
    action


verbRows :: [(TypeName, Map FieldName A.Value)]
verbRows = map noteRow ["n1", "n2", "n3", "n4", "n5", "n6"] <> [flagRow]
  where
    noteRow k =
      ( "Note"
      , Map.fromList
          [ ("id", A.String k)
          , ("title", A.String ("Title " <> k))
          , ("body", A.String ("Body " <> k))
          ]
      )
    flagRow = ("Flag", Map.fromList [("id", A.String "f1"), ("raised", A.Bool False)])


verbsHooks :: TVar Int -> MemoryHooks
verbsHooks counter =
  defaultHooks
    { mhGetRoots =
        Map.fromList
          [ ("note", byIdRoot "Note")
          , ("flag", byIdRoot "Flag")
          ]
    , mhMutations =
        Map.fromList
          [ ("replaceNote", vReplace)
          , ("editNote", vMerge "Note" "note")
          , ("deleteNote", vDelete)
          , ("createNote", vCreate counter)
          , ("setFlag", vMerge "Flag" "flag")
          ]
    }
  where
    byIdRoot ty _db args = pure $ case Map.lookup "id" args of
      Just (A.String k) -> Just (Ref ty k)
      _ -> Nothing


committed :: MemoryDb -> [WriteFact] -> [Ref] -> STM MutationOutcome
committed db writes refs = do
  tok <- snapshotToken db
  pure . MutationCommitted $
    CommitResult {crResult = refs, crWrites = writes, crSnapshot = tok}


-- | @replaceNote(note, input)@: PUT semantics — the row becomes exactly
-- the given representation (plus its key).
vReplace :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
vReplace db _claims args =
  case (Map.lookup "note" args, Map.lookup "input" args) of
    (Just (A.String k), Just (A.Object input)) -> atomically $ do
      putRow db "Note" k (Map.insert "id" (A.String k) (fieldsOf input))
      committed db [WroteEntity (Ref "Note" k)] [Ref "Note" k]
    _ -> pure (MutationFailed (internalError (Just "replaceNote: note and input arguments required")))


-- | Merge-patch effect shared by @editNote@ and @setFlag@: present
-- fields replace, @null@ clears, absent fields survive.
vMerge :: TypeName -> ArgName -> MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
vMerge ty keyArg db _claims args =
  case (Map.lookup keyArg args, Map.lookup "patch" args) of
    (Just (A.String k), Just (A.Object patch)) -> atomically $ do
      existing <-
        readRow db ty k >>= \case
          RowFound row -> pure (rowFields row)
          _ -> pure (Map.singleton "id" (A.String k))
      putRow db ty k (applyMergePatch existing patch)
      committed db [WroteEntity (Ref ty k)] [Ref ty k]
    _ -> pure (MutationFailed (internalError (Just "merge mutation: key and patch arguments required")))


vDelete :: MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
vDelete db _claims args =
  case Map.lookup "note" args of
    Just (A.String k) -> atomically $ do
      deleteRow db "Note" k
      tomb <- readRow db "Note" k
      let ver = case tomb of
            RowTombstone v -> v
            _ -> "t:1"
      committed db [DeletedEntity (Ref "Note" k) ver] [Ref "Note" k]
    _ -> pure (MutationFailed (internalError (Just "deleteNote: note argument required")))


vCreate :: TVar Int -> MemoryDb -> Claims -> Map ArgName A.Value -> IO MutationOutcome
vCreate counter db _claims args =
  case Map.lookup "input" args of
    Just (A.Object input) -> atomically $ do
      n <- stateTVar counter (\i -> (i + 1, i + 1))
      let k = "c" <> tshow n
      putRow db "Note" k (Map.insert "id" (A.String k) (fieldsOf input))
      committed db [WroteEntity (Ref "Note" k)] [Ref "Note" k]
    _ -> pure (MutationFailed (internalError (Just "createNote: input argument required")))


applyMergePatch :: Map FieldName A.Value -> A.Object -> Map FieldName A.Value
applyMergePatch row patch = foldl' step row (KM.toList patch)
  where
    step acc (k, v) = case v of
      A.Null -> Map.delete (FieldName (AK.toText k)) acc
      _ -> Map.insert (FieldName (AK.toText k)) v acc


fieldsOf :: A.Object -> Map FieldName A.Value
fieldsOf obj = Map.fromList (map (\(k, v) -> (FieldName (AK.toText k), v)) (KM.toList obj))


-- ---------------------------------------------------------------------------
-- Request and record helpers
-- ---------------------------------------------------------------------------

mergePatchType :: ByteString
mergePatchType = "application/x-lattice-merge-patch"


bodyOf :: A.Value -> ByteString
bodyOf = BS8.toStrict . A.encode


putNote :: Loop -> Text -> [(HeaderName, ByteString)] -> A.Value -> IO RawResp
putNote loop key hdrs v =
  httpRaw
    loop
    PUT
    ("/e/Note/" <> encodeUtf8 key)
    (("Content-Type", "application/json") : hdrs)
    (Just (bodyOf v))


patchNote :: Loop -> Text -> [(HeaderName, ByteString)] -> A.Value -> IO RawResp
patchNote loop key hdrs v =
  httpRaw
    loop
    PATCH
    ("/e/Note/" <> encodeUtf8 key)
    (("Content-Type", mergePatchType) : hdrs)
    (Just (bodyOf v))


deleteNote :: Loop -> Text -> [(HeaderName, ByteString)] -> IO RawResp
deleteNote loop key hdrs =
  httpRaw loop DELETE ("/e/Note/" <> encodeUtf8 key) hdrs Nothing


postNotes :: Loop -> [(HeaderName, ByteString)] -> A.Value -> IO RawResp
postNotes loop hdrs v =
  httpRaw loop POST "/e/Note" (("Content-Type", "application/json") : hdrs) (Just (bodyOf v))


entityRecordsOf :: RawResp -> [EntityRecord]
entityRecordsOf r = mapMaybe entityOf (rawRecords r)
  where
    entityOf = \case
      REntity er -> Just er
      _ -> Nothing


tombstonesOf :: RawResp -> [Ref]
tombstonesOf r = mapMaybe tombOf (rawRecords r)
  where
    tombOf = \case
      RTombstone ref _ _ -> Just ref
      _ -> Nothing


tombItemOf :: Record -> Maybe (Ref, Maybe Text)
tombItemOf = \case
  RTombstone ref _ item -> Just (ref, item)
  _ -> Nothing


itemOf :: EntityRecord -> Maybe Text
itemOf = erItem


-- | The named field of a live row, read through the point-fetch wire path.
fetchField' :: Loop -> Text -> Text -> Text -> IO (Maybe A.Value)
fetchField' loop ty key field = do
  r <- httpRaw loop GET ("/e/" <> encodeUtf8 ty <> "/" <> encodeUtf8 key) [] Nothing
  rawStatus r `shouldBe` 200
  case entityRecordsOf r of
    [er] -> pure (Map.lookup field (erFields er))
    other -> expectationFailure ("expected exactly one entity record, got: " <> show other)


fetchField :: Loop -> Text -> Text -> IO (Maybe A.Value)
fetchField loop = fetchField' loop "Note"


fetchTitle :: Loop -> Text -> IO (Maybe Text)
fetchTitle loop key =
  fetchField loop key "title" >>= \case
    Just (A.String t) -> pure (Just t)
    _ -> pure Nothing


tshow :: (Show a) => a -> Text
tshow = T.pack . show
