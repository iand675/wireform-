{- | The Lattice wire format (spec §9): NDJSON entity-stream records, the
manifest, scoped errors, and the page value convention, plus the protocol
header names shared by server and client.

JSON shapes are pinned here as the cross-language contract (the TypeScript
client mirrors them):

@
{"kind":"manifest","query":"8f2c…","plan":"pl_…","slice":"ctx","root":{"feed":["Post:17"]},"etag":"m:…"}
{"kind":"entity","id":"Post:17","ver":"e41","fields":{"title":"…","author":{"$ref":"User:9"}}}
{"kind":"tombstone","id":"Post:17","ver":"t:99"}
{"kind":"elided","id":"Post:17"}
{"kind":"unchanged","id":"Post:17","ver":"e41"}
{"kind":"error","scope":"Human:1002","code":"lattice:loader-timeout","retryable":true}
{"kind":"invalidated","keys":["Post:17","feed:123"]}
{"kind":"end","complete":true,"etag":"m:…"}
{"kind":"plan","query":"8f2c…","plan":"pl_…","slices":{"pub":false,"ctx":{"claims":["org"],"roots":["feed"]},"priv":false}}
@

Unknown record kinds and unknown scope tags decode to their tolerant
constructors ('RUnknown', 'ScopeUnknown'); clients MUST NOT fail on them
(§9.4.1).
-}
module Lattice.Wire (
  -- * Records
  Record (..),
  Manifest (..),
  BatchInfo (..),
  EntityRecord (..),
  ErrorRecord (..),
  Scope (..),
  EndRecord (..),
  PlanRecord (..),
  SliceInfo (..),

  -- * Page values
  PageValue (..),
  pageToJSON,
  pageFromJSON,
  refValue,

  -- * NDJSON framing
  encodeRecords,
  encodeRecord,
  decodeRecords,

  -- * Surrogate keys
  SurrogateKey,
  entityKeyOf,
  collectionKey,
  planKey,

  -- * Protocol header names
  hLatticePlan,
  hLatticeSchema,
  hLatticeSnapshot,
  hLatticeOutcome,
  hLatticeQueryName,
  hSurrogateKey,
  hIdempotencyKey,
  hIdempotencyReplayed,
  hVcAuth,
  hXHave,
  hXHaveDigest,
  hLatticeQuerySig,
  hLatticeClient,

  -- * Media types
  queryMediaType,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (CI)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Lattice.Types


-- ---------------------------------------------------------------------------
-- Records
-- ---------------------------------------------------------------------------

data Record
  = RManifest Manifest
  | REntity EntityRecord
  | RTombstone Ref Text (Maybe Text)
  -- ^ id, ver, item key.
  | RElided Ref
  | RUnchanged Ref Text
  | RError ErrorRecord
  | RInvalidated [SurrogateKey] (Maybe Text)
  -- ^ keys, item key.
  | REnd EndRecord
  | RPlan PlanRecord
  | RReauth
  | -- | Tolerated unknown kind: kept verbatim for forward compatibility.
    RUnknown Text A.Object
  deriving stock (Eq, Show)


data Manifest = Manifest
  { mQuery :: Maybe Text
  -- ^ Query hash; present on query responses.
  , mMutation :: Maybe MutationName
  -- ^ Present on mutation responses.
  , mPlan :: Maybe Text
  , mSlice :: Maybe SliceName
  , mRoot :: Map Text [Ref]
  -- ^ Result order per root name (or @result@ / @items@ for mutations).
  , mEtag :: Text
  , mBatch :: Maybe BatchInfo
  }
  deriving stock (Eq, Show)


data BatchInfo = BatchInfo
  { biAtomicity :: Text
  -- ^ @"all-or-nothing"@ | @"best-effort"@.
  , biCount :: Int
  }
  deriving stock (Eq, Show)


data EntityRecord = EntityRecord
  { erId :: Ref
  , erVer :: Text
  , erFields :: Map Text A.Value
  -- ^ Keyed by canonical field form, e.g. @avatarUrl(size:48)@.
  , erItem :: Maybe Text
  -- ^ Batch item correlation key.
  , erSrc :: Maybe Text
  -- ^ Federation source tag (§18.4).
  }
  deriving stock (Eq, Show)


-- | The protocol-defined open Scope sum (§9.4.1).
data Scope
  = ScopeEntity Ref
  | ScopeField Ref FieldName
  | ScopeEdge Ref FieldName
  | ScopeRoot RootName
  | ScopeItem Text
  | -- | Unrecognized @$tag@, kept verbatim; treat as unscoped for display.
    ScopeUnknown Text A.Value
  deriving stock (Eq, Show)


data ErrorRecord = ErrorRecord
  { errScope :: Maybe Scope
  -- ^ 'Nothing' for a fatal, unscoped stream error.
  , errCode :: Maybe Text
  -- ^ Protocol vocabulary, @lattice:*@.
  , errDomain :: Maybe A.Value
  -- ^ Declared domain error sum value, @{"$tag":…}@.
  , errRetryable :: Bool
  , errMessage :: Maybe Text
  }
  deriving stock (Eq, Show)


data EndRecord = EndRecord
  { endComplete :: Bool
  , endEtag :: Maybe Text
  }
  deriving stock (Eq, Show)


-- | The @slice=plan@ response record (§9.2).
data PlanRecord = PlanRecord
  { prQuery :: Text
  , prPlan :: Text
  , prSlices :: Map SliceName SliceInfo
  -- ^ Only data slices appear; a missing key means the slice is empty.
  }
  deriving stock (Eq, Show)


data SliceInfo = SliceInfo
  { siClaims :: [ClaimName]
  , siRoots :: [RootName]
  }
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

instance ToJSON Record where
  toJSON = \case
    RManifest m -> obj ("manifest" :: Text) (manifestPairs m)
    REntity e ->
      obj
        "entity"
        ( [ "id" .= renderRef (erId e)
          , "ver" .= erVer e
          , "fields" .= A.object [AK.fromText k .= v | (k, v) <- Map.toList (erFields e)]
          ]
            <> catMaybes
              [ ("item" .=) <$> erItem e
              , ("src" .=) <$> erSrc e
              ]
        )
    RTombstone r v item ->
      obj "tombstone" (["id" .= renderRef r, "ver" .= v] <> catMaybes [("item" .=) <$> item])
    RElided r -> obj "elided" ["id" .= renderRef r]
    RUnchanged r v -> obj "unchanged" ["id" .= renderRef r, "ver" .= v]
    RError e ->
      obj
        "error"
        ( catMaybes
            [ ("scope" .=) <$> errScope e
            , ("code" .=) <$> errCode e
            , ("error" .=) <$> errDomain e
            , Just ("retryable" .= errRetryable e)
            , ("message" .=) <$> errMessage e
            ]
        )
    RInvalidated keys item ->
      obj "invalidated" (["keys" .= keys] <> catMaybes [("item" .=) <$> item])
    REnd e ->
      obj "end" (["complete" .= endComplete e] <> catMaybes [("etag" .=) <$> endEtag e])
    RPlan p ->
      obj
        "plan"
        [ "query" .= prQuery p
        , "plan" .= prPlan p
        , "slices"
            .= A.object
              [ AK.fromText (renderSlice s)
                  .= maybe (A.Bool False) sliceInfoJSON (Map.lookup s (prSlices p))
              | s <- [SlicePub, SliceCtx, SlicePriv]
              ]
        ]
    RReauth -> obj "reauth" []
    RUnknown k o -> A.Object (KM.insert "kind" (A.String k) o)
    where
      obj :: Text -> [A.Pair] -> A.Value
      obj k ps = A.object (("kind" .= k) : ps)
      sliceInfoJSON (SliceInfo cs rs) =
        A.object
          [ "claims" .= map unClaimName cs
          , "roots" .= map unRootName rs
          ]
      manifestPairs m =
        catMaybes
          [ ("query" .=) <$> mQuery m
          , ("mutation" .=) . unMutationName <$> mMutation m
          , ("plan" .=) <$> mPlan m
          , ("slice" .=) . renderSlice <$> mSlice m
          , ("batch" .=) <$> mBatch m
          ]
          <> [ "root" .= A.object [AK.fromText k .= map renderRef rs | (k, rs) <- Map.toList (mRoot m)]
             , "etag" .= mEtag m
             ]


instance FromJSON Record where
  parseJSON = A.withObject "Record" $ \o -> do
    kind <- o .: "kind"
    case kind :: Text of
      "manifest" -> do
        mQuery <- o .:? "query"
        mMutationT <- o .:? "mutation"
        mPlan <- o .:? "plan"
        mSliceT <- o .:? "slice"
        rootObj <- o .:? "root" A..!= KM.empty
        mEtag <- o .: "etag"
        mBatch <- o .:? "batch"
        mRoot <-
          Map.fromList
            <$> traverse
              (\(k, v) -> (AK.toText k,) <$> parseRefList v)
              (KM.toList rootObj)
        let mMutation = MutationName <$> mMutationT
            mSlice = mSliceT >>= parseSlice
        pure (RManifest Manifest {..})
      "entity" -> do
        r <- refField o
        erVer <- o .: "ver"
        fieldsObj <- o .: "fields"
        erItem <- o .:? "item"
        erSrc <- o .:? "src"
        let erFields = Map.fromList [(AK.toText k, v) | (k, v) <- KM.toList fieldsObj]
        pure (REntity EntityRecord {erId = r, ..})
      "tombstone" -> RTombstone <$> refField o <*> o .: "ver" <*> o .:? "item"
      "elided" -> RElided <$> refField o
      "unchanged" -> RUnchanged <$> refField o <*> o .: "ver"
      "error" ->
        fmap RError $
          ErrorRecord
            <$> o .:? "scope"
            <*> o .:? "code"
            <*> o .:? "error"
            <*> o .:? "retryable" A..!= False
            <*> o .:? "message"
      "invalidated" -> RInvalidated <$> o .: "keys" <*> o .:? "item"
      "end" -> fmap REnd $ EndRecord <$> o .: "complete" <*> o .:? "etag"
      "plan" -> do
        prQuery <- o .: "query"
        prPlan <- o .: "plan"
        slicesObj <- o .: "slices"
        prSlices <-
          Map.fromList . catMaybes
            <$> traverse
              ( \(k, v) -> case parseSlice (AK.toText k) of
                  Nothing -> pure Nothing
                  Just s -> case v of
                    A.Bool False -> pure Nothing
                    _ -> do
                      info <- parseSliceInfo v
                      pure (Just (s, info))
              )
              (KM.toList slicesObj)
        pure (RPlan PlanRecord {..})
      "reauth" -> pure RReauth
      other -> pure (RUnknown other (KM.delete "kind" o))
    where
      refField o = do
        t <- o .: "id"
        maybe (fail "bad ref") pure (parseRef t)
      parseRefList = A.withArray "refs" $ \v ->
        traverse (A.withText "ref" (maybe (fail "bad ref") pure . parseRef)) (foldr (:) [] v)
      parseSliceInfo = A.withObject "SliceInfo" $ \s ->
        SliceInfo
          <$> (map ClaimName <$> s .:? "claims" A..!= [])
          <*> (map RootName <$> s .:? "roots" A..!= [])


instance ToJSON BatchInfo where
  toJSON (BatchInfo a c) = A.object ["atomicity" .= a, "count" .= c]


instance FromJSON BatchInfo where
  parseJSON = A.withObject "BatchInfo" $ \o ->
    BatchInfo <$> o .: "atomicity" <*> o .: "count"


{- | 'ScopeEntity' uses the bare-ref shorthand; every other constructor is a
tagged object (§9.4.1).
-}
instance ToJSON Scope where
  toJSON = \case
    ScopeEntity r -> A.String (renderRef r)
    ScopeField r f ->
      A.object ["$tag" .= ("Field" :: Text), "entity" .= renderRef r, "field" .= unFieldName f]
    ScopeEdge r f ->
      A.object ["$tag" .= ("Edge" :: Text), "entity" .= renderRef r, "field" .= unFieldName f]
    ScopeRoot n -> A.object ["$tag" .= ("Root" :: Text), "root" .= unRootName n]
    ScopeItem k -> A.object ["$tag" .= ("Item" :: Text), "item" .= k]
    ScopeUnknown tag v -> case v of
      A.Object o -> A.Object (KM.insert "$tag" (A.String tag) o)
      _ -> A.object ["$tag" .= tag, "value" .= v]


instance FromJSON Scope where
  parseJSON v = case v of
    A.String t -> maybe (fail "bad scope ref") (pure . ScopeEntity) (parseRef t)
    A.Object o -> do
      tag <- o .: "$tag"
      case tag :: Text of
        "Entity" -> ScopeEntity <$> refOf o "entity"
        "Field" -> ScopeField <$> refOf o "entity" <*> (FieldName <$> o .: "field")
        "Edge" -> ScopeEdge <$> refOf o "entity" <*> (FieldName <$> o .: "field")
        "Root" -> ScopeRoot . RootName <$> o .: "root"
        "Item" -> ScopeItem <$> o .: "item"
        other -> pure (ScopeUnknown other (A.Object (KM.delete "$tag" o)))
    _ -> fail "scope must be a string or object"
    where
      refOf o k = do
        t <- o .: k
        maybe (fail "bad ref") pure (parseRef t)


-- ---------------------------------------------------------------------------
-- Page values
-- ---------------------------------------------------------------------------

{- | The wire form of one paginated edge occurrence (§3.6):
@{"$page":{"items":[…],"next":…,"prev":…,"total":…}}@. Bounded collections
are plain ref-string arrays and never use this wrapper.
-}
data PageValue = PageValue
  { pvItems :: [Ref]
  , pvNext :: Maybe Text
  , pvPrev :: Maybe Text
  , pvTotal :: Maybe Int
  }
  deriving stock (Eq, Show)


pageToJSON :: PageValue -> A.Value
pageToJSON PageValue {..} =
  A.object
    [ "$page"
        .= A.object
          ( [ "items" .= map (\r -> A.object ["$ref" .= renderRef r]) pvItems
            , "next" .= pvNext
            , "prev" .= pvPrev
            ]
              <> maybe [] (\t -> ["total" .= t]) pvTotal
          )
    ]


pageFromJSON :: A.Value -> Maybe PageValue
pageFromJSON v = A.parseMaybe parser v
  where
    parser = A.withObject "$page" $ \o -> do
      p <- o .: "$page"
      flip (A.withObject "page body") p $ \b -> do
        items <- b .: "items"
        refs <- traverse itemRef items
        PageValue refs <$> b .:? "next" <*> b .:? "prev" <*> b .:? "total"
    itemRef = A.withObject "$ref" $ \o -> do
      t <- o .: "$ref"
      maybe (fail "bad ref") pure (parseRef t)


-- | The wire form of a to-one edge occurrence: @{"$ref":"Type:key"}@.
refValue :: Ref -> A.Value
refValue r = A.object ["$ref" .= renderRef r]


-- ---------------------------------------------------------------------------
-- NDJSON framing
-- ---------------------------------------------------------------------------

encodeRecord :: Record -> ByteString
encodeRecord = BL.toStrict . A.encode


-- | One record per line, each line newline-terminated.
encodeRecords :: [Record] -> ByteString
encodeRecords rs = BS8.concat [encodeRecord r <> "\n" | r <- rs]


{- | Decode an NDJSON body. Blank lines are skipped; undecodable lines are
returned as 'Left' with the offending line so callers can choose severity.
-}
decodeRecords :: ByteString -> [Either ByteString Record]
decodeRecords body =
  mapMaybe decodeLine (BS8.lines body)
  where
    decodeLine ln
      | BS8.all (== ' ') ln = Nothing
      | otherwise = Just $ case A.eitherDecodeStrict ln of
          Left _ -> Left ln
          Right r -> Right r


-- ---------------------------------------------------------------------------
-- Surrogate keys
-- ---------------------------------------------------------------------------

-- | A cache tag (§10.5): @Type:id@, @{collection}:{grouping}@, or @plan:{planId}@.
type SurrogateKey = Text


entityKeyOf :: Ref -> SurrogateKey
entityKeyOf = renderRef


collectionKey :: CollectionName -> [Text] -> SurrogateKey
collectionKey (CollectionName n) groupVals = n <> ":" <> commaJoin groupVals
  where
    commaJoin [] = ""
    commaJoin [x] = x
    commaJoin (x : xs) = x <> "," <> commaJoin xs


planKey :: Text -> SurrogateKey
planKey planId = "plan:" <> planId


-- ---------------------------------------------------------------------------
-- Headers and media types
-- ---------------------------------------------------------------------------

hLatticePlan, hLatticeSchema, hLatticeSnapshot, hLatticeOutcome :: CI ByteString
hLatticePlan = "Lattice-Plan"
hLatticeSchema = "Lattice-Schema"
hLatticeSnapshot = "Lattice-Snapshot"
hLatticeOutcome = "Lattice-Outcome"


hLatticeQueryName, hSurrogateKey, hIdempotencyKey, hIdempotencyReplayed :: CI ByteString
hLatticeQueryName = "Lattice-Query-Name"
hSurrogateKey = "Surrogate-Key"
hIdempotencyKey = "Idempotency-Key"
hIdempotencyReplayed = "Idempotency-Replayed"


hVcAuth, hXHave, hXHaveDigest, hLatticeQuerySig, hLatticeClient :: CI ByteString
hVcAuth = "X-Vc-Auth"
hXHave = "X-Have"
hXHaveDigest = "X-Have-Digest"
hLatticeQuerySig = "X-Lattice-Query-Sig"
hLatticeClient = "Lattice-Client"


queryMediaType :: ByteString
queryMediaType = "application/x-lattice-query"
