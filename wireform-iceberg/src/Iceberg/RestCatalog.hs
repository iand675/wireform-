{-# LANGUAGE BangPatterns #-}
-- | Apache Iceberg REST catalog protocol.
--
-- Defines the wire-level request and response types for the Iceberg
-- REST catalog API, plus their JSON codecs and an abstract
-- 'CatalogClient' interface that callers can implement against any
-- HTTP transport (http-client, http2, etc.).  This mirrors how the
-- official Java and Python clients separate the protocol from the
-- transport.
--
-- Reference: <https://github.com/apache/iceberg/blob/main/open-api/rest-catalog-open-api.yaml>
--
-- == Typical use
--
-- @
-- import qualified Iceberg.RestCatalog as Rest
--
-- -- 1. Implement a 'CatalogClient' over your HTTP transport:
-- myClient :: Rest.CatalogClient IO
-- myClient = Rest.CatalogClient
--   { Rest.ccBaseUrl = \"https:\/\/my-catalog.example.com\"
--   , Rest.ccAuth    = Rest.AuthBearer \"my-token\"
--   , Rest.ccPerform = \\req -> ...  -- run the HTTP request, return a Response
--   }
--
-- -- 2. Use the protocol functions to talk to the catalog:
-- Rest.listNamespaces myClient (Rest.ListNamespacesReq Nothing Nothing)
-- @
module Iceberg.RestCatalog
  ( -- * Client interface
    CatalogClient (..)
  , AuthMethod (..)
  , Request (..)
  , Response (..)
  , HttpMethod (..)
    -- * Endpoints
  , listNamespaces
  , createNamespace
  , dropNamespace
  , loadNamespaceMetadata
  , listTables
  , createTable
  , loadTable
  , dropTable
  , renameTable
  , commitTable
  , configEndpoint
    -- * Request types
  , ListNamespacesReq (..)
  , CreateNamespaceReq (..)
  , ListTablesReq (..)
  , CreateTableReq (..)
  , RenameTableReq (..)
  , CommitTableReq (..)
  , TableUpdate (..)
  , TableRequirement (..)
  , Namespace (..)
  , TableIdentifier (..)
    -- * Response types
  , ConfigResponse (..)
  , ListNamespacesResp (..)
  , GetNamespaceResp (..)
  , ListTablesResp (..)
  , LoadTableResp (..)
  , CommitTableResp (..)
  , ErrorResponse (..)
    -- * Path builders (useful for custom transports)
  , namespacesPath
  , namespacePath
  , tablesPath
  , tablePath
  , renamePath
  ) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Iceberg.JSON as IJ
import Iceberg.Types
  ( PartitionSpec
  , Schema
  , SortOrder
  , Snapshot
  , TableMetadata
  )

-- Orphan instances for the schema-fragment types.  'Iceberg.JSON'
-- exposes these as plain functions; we lift them to typeclass
-- instances so this module can use 'A.encode' / 'A.eitherDecodeStrict'
-- uniformly.

instance A.ToJSON TableMetadata where
  toJSON = IJ.metadataToJSON

instance A.FromJSON TableMetadata where
  parseJSON v = case IJ.metadataFromJSON v of
    Right tm -> pure tm
    Left  e  -> fail e

instance A.ToJSON Schema where
  toJSON = IJ.schemaToJSON

instance A.FromJSON Schema where
  parseJSON v = case IJ.schemaFromJSON v of
    Right s -> pure s
    Left  e -> fail e

instance A.ToJSON Snapshot where
  toJSON = IJ.snapshotToJSON

instance A.FromJSON Snapshot where
  parseJSON v = case IJ.snapshotFromJSON v of
    Right s -> pure s
    Left  e -> fail e

instance A.ToJSON PartitionSpec where
  toJSON = IJ.partitionSpecToJSON

instance A.FromJSON PartitionSpec where
  parseJSON v = case IJ.partitionSpecFromJSON v of
    Right p -> pure p
    Left  e -> fail e

instance A.ToJSON SortOrder where
  toJSON = IJ.sortOrderToJSON

instance A.FromJSON SortOrder where
  parseJSON v = case IJ.sortOrderFromJSON v of
    Right s -> pure s
    Left  e -> fail e

------------------------------------------------------------------------
-- Client interface
------------------------------------------------------------------------

-- | An abstract HTTP transport.  Provide 'ccPerform' to bridge to any
-- HTTP library.
data CatalogClient m = CatalogClient
  { ccBaseUrl :: !Text
  , ccAuth    :: !AuthMethod
  , ccPerform :: !(Request -> m Response)
  }

data AuthMethod
  = AuthNone
  | AuthBearer !Text
    -- ^ Adds @Authorization: Bearer \<token\>@.
  | AuthCustom !(Map Text Text)
    -- ^ Arbitrary header set.
  deriving stock (Show, Eq)

data HttpMethod = GET | POST | PUT | DELETE | HEAD
  deriving stock (Show, Eq)

data Request = Request
  { reqMethod  :: !HttpMethod
  , reqPath    :: !Text
  , reqQuery   :: ![(Text, Text)]
  , reqHeaders :: ![(Text, Text)]
  , reqBody    :: !(Maybe ByteString)
    -- ^ JSON-encoded request body, if any.
  } deriving stock (Show, Eq)

data Response = Response
  { respStatus :: !Int
  , respBody   :: !ByteString
  } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Path builders
------------------------------------------------------------------------

-- | @\/v1\/namespaces@
namespacesPath :: Text
namespacesPath = "/v1/namespaces"

-- | @\/v1\/namespaces\/\<ns\>@ where @\<ns\>@ uses the dot-encoded
-- namespace per the spec.
namespacePath :: Namespace -> Text
namespacePath (Namespace levels) =
  namespacesPath <> "/" <> T.intercalate "%1F" (V.toList levels)

-- | @\/v1\/namespaces\/\<ns\>\/tables@
tablesPath :: Namespace -> Text
tablesPath ns = namespacePath ns <> "/tables"

-- | @\/v1\/namespaces\/\<ns\>\/tables\/\<table\>@
tablePath :: TableIdentifier -> Text
tablePath (TableIdentifier ns name) = tablesPath ns <> "/" <> name

-- | @\/v1\/tables\/rename@
renamePath :: Text
renamePath = "/v1/tables/rename"

------------------------------------------------------------------------
-- Common types
------------------------------------------------------------------------

-- | A namespace is a list of \"levels\" (e.g. @[\"db\", \"schema\"]@).
newtype Namespace = Namespace { nsLevels :: Vector Text }
  deriving stock (Show, Eq)

instance A.ToJSON Namespace where
  toJSON (Namespace ls) = A.toJSON ls

instance A.FromJSON Namespace where
  parseJSON v = Namespace <$> A.parseJSON v

data TableIdentifier = TableIdentifier
  { tiNamespace :: !Namespace
  , tiName      :: !Text
  } deriving stock (Show, Eq)

instance A.ToJSON TableIdentifier where
  toJSON ti = A.object
    [ "namespace" .= tiNamespace ti
    , "name"      .= tiName ti
    ]

instance A.FromJSON TableIdentifier where
  parseJSON = A.withObject "TableIdentifier" $ \o ->
    TableIdentifier <$> o .: "namespace" <*> o .: "name"

------------------------------------------------------------------------
-- /v1/config
------------------------------------------------------------------------

data ConfigResponse = ConfigResponse
  { crDefaults  :: !(Map Text Text)
  , crOverrides :: !(Map Text Text)
  } deriving stock (Show, Eq)

instance A.FromJSON ConfigResponse where
  parseJSON = A.withObject "ConfigResponse" $ \o ->
    ConfigResponse
      <$> (o .:? "defaults"  A..!= Map.empty)
      <*> (o .:? "overrides" A..!= Map.empty)

-- | @GET \/v1\/config@.
configEndpoint :: Monad m => CatalogClient m -> m (Either ErrorResponse ConfigResponse)
configEndpoint cc = runGet cc "/v1/config" []

------------------------------------------------------------------------
-- Namespaces
------------------------------------------------------------------------

data ListNamespacesReq = ListNamespacesReq
  { lnrParent      :: !(Maybe Namespace)
    -- ^ Filter to direct children of the given namespace.
  , lnrPageToken   :: !(Maybe Text)
  } deriving stock (Show, Eq)

data ListNamespacesResp = ListNamespacesResp
  { lnpNamespaces  :: !(Vector Namespace)
  , lnpNextToken   :: !(Maybe Text)
  } deriving stock (Show, Eq)

instance A.FromJSON ListNamespacesResp where
  parseJSON = A.withObject "ListNamespacesResp" $ \o ->
    ListNamespacesResp
      <$> o .:  "namespaces"
      <*> o .:? "next-page-token"

-- | @GET \/v1\/namespaces@.
listNamespaces :: Monad m => CatalogClient m -> ListNamespacesReq -> m (Either ErrorResponse ListNamespacesResp)
listNamespaces cc req =
  let q = catMaybes2
            [ ("parent",) . encodeNamespaceParam <$> lnrParent req
            , ("pageToken",) <$> lnrPageToken req
            ]
  in runGet cc namespacesPath q

encodeNamespaceParam :: Namespace -> Text
encodeNamespaceParam (Namespace ls) = T.intercalate "\x1F" (V.toList ls)

data CreateNamespaceReq = CreateNamespaceReq
  { cnrNamespace  :: !Namespace
  , cnrProperties :: !(Map Text Text)
  } deriving stock (Show, Eq)

instance A.ToJSON CreateNamespaceReq where
  toJSON r = A.object
    [ "namespace"  .= cnrNamespace r
    , "properties" .= cnrProperties r
    ]

data GetNamespaceResp = GetNamespaceResp
  { gnrNamespace  :: !Namespace
  , gnrProperties :: !(Map Text Text)
  } deriving stock (Show, Eq)

instance A.FromJSON GetNamespaceResp where
  parseJSON = A.withObject "GetNamespaceResp" $ \o ->
    GetNamespaceResp
      <$> o .: "namespace"
      <*> (o .:? "properties" A..!= Map.empty)

-- | @POST \/v1\/namespaces@.
createNamespace
  :: Monad m
  => CatalogClient m
  -> CreateNamespaceReq
  -> m (Either ErrorResponse GetNamespaceResp)
createNamespace cc req = runPost cc namespacesPath req

-- | @DELETE \/v1\/namespaces\/\<ns\>@.
dropNamespace
  :: Monad m
  => CatalogClient m
  -> Namespace
  -> m (Either ErrorResponse ())
dropNamespace cc ns = runDelete cc (namespacePath ns)

-- | @GET \/v1\/namespaces\/\<ns\>@.
loadNamespaceMetadata
  :: Monad m
  => CatalogClient m
  -> Namespace
  -> m (Either ErrorResponse GetNamespaceResp)
loadNamespaceMetadata cc ns = runGet cc (namespacePath ns) []

------------------------------------------------------------------------
-- Tables
------------------------------------------------------------------------

data ListTablesReq = ListTablesReq
  { ltrNamespace :: !Namespace
  , ltrPageToken :: !(Maybe Text)
  } deriving stock (Show, Eq)

data ListTablesResp = ListTablesResp
  { ltpIdentifiers :: !(Vector TableIdentifier)
  , ltpNextToken   :: !(Maybe Text)
  } deriving stock (Show, Eq)

instance A.FromJSON ListTablesResp where
  parseJSON = A.withObject "ListTablesResp" $ \o ->
    ListTablesResp
      <$> o .:  "identifiers"
      <*> o .:? "next-page-token"

-- | @GET \/v1\/namespaces\/\<ns\>\/tables@.
listTables
  :: Monad m
  => CatalogClient m
  -> ListTablesReq
  -> m (Either ErrorResponse ListTablesResp)
listTables cc req =
  let q = catMaybes2 [ ("pageToken",) <$> ltrPageToken req ]
  in runGet cc (tablesPath (ltrNamespace req)) q

data CreateTableReq = CreateTableReq
  { ctrName          :: !Text
  , ctrSchema        :: !Schema
  , ctrLocation      :: !(Maybe Text)
  , ctrPartitionSpec :: !(Maybe PartitionSpec)
  , ctrWriteOrder    :: !(Maybe SortOrder)
  , ctrStageCreate   :: !(Maybe Bool)
  , ctrProperties    :: !(Map Text Text)
  } deriving stock (Show, Eq)

instance A.ToJSON CreateTableReq where
  toJSON r = A.object $
    [ "name"      .= ctrName r
    , "schema"    .= ctrSchema r
    , "properties".= ctrProperties r
    ] ++ optionalField "location"       (ctrLocation r)
      ++ optionalField "partition-spec" (ctrPartitionSpec r)
      ++ optionalField "write-order"    (ctrWriteOrder r)
      ++ optionalField "stage-create"   (ctrStageCreate r)

data LoadTableResp = LoadTableResp
  { ltMetadataLocation :: !(Maybe Text)
  , ltMetadata         :: !TableMetadata
  , ltConfig           :: !(Map Text Text)
  } deriving stock (Show, Eq)

instance A.FromJSON LoadTableResp where
  parseJSON = A.withObject "LoadTableResp" $ \o ->
    LoadTableResp
      <$> o .:? "metadata-location"
      <*> o .:  "metadata"
      <*> (o .:? "config" A..!= Map.empty)

-- | @POST \/v1\/namespaces\/\<ns\>\/tables@.
createTable
  :: Monad m
  => CatalogClient m
  -> Namespace
  -> CreateTableReq
  -> m (Either ErrorResponse LoadTableResp)
createTable cc ns req = runPost cc (tablesPath ns) req

-- | @GET \/v1\/namespaces\/\<ns\>\/tables\/\<table\>@.
loadTable
  :: Monad m
  => CatalogClient m
  -> TableIdentifier
  -> m (Either ErrorResponse LoadTableResp)
loadTable cc ti = runGet cc (tablePath ti) []

-- | @DELETE \/v1\/namespaces\/\<ns\>\/tables\/\<table\>@.
dropTable
  :: Monad m
  => CatalogClient m
  -> TableIdentifier
  -> m (Either ErrorResponse ())
dropTable cc ti = runDelete cc (tablePath ti)

data RenameTableReq = RenameTableReq
  { rtrSource      :: !TableIdentifier
  , rtrDestination :: !TableIdentifier
  } deriving stock (Show, Eq)

instance A.ToJSON RenameTableReq where
  toJSON r = A.object
    [ "source"      .= rtrSource r
    , "destination" .= rtrDestination r
    ]

-- | @POST \/v1\/tables\/rename@.
renameTable
  :: Monad m
  => CatalogClient m
  -> RenameTableReq
  -> m (Either ErrorResponse ())
renameTable cc req = runPostUnit cc renamePath req

------------------------------------------------------------------------
-- Commit
------------------------------------------------------------------------

-- | A single update to a table.  Iceberg's commit protocol applies a
-- list of these atomically against a list of preconditions
-- ('TableRequirement').
data TableUpdate
  = AssignUUID            !Text
  | UpgradeFormatVersion  !Int
  | AddSchema             !Schema !(Maybe Int)  -- last-column-id
  | SetCurrentSchema      !Int
  | AddPartitionSpec      !PartitionSpec
  | SetDefaultSpec        !Int
  | AddSortOrder          !SortOrder
  | SetDefaultSortOrder   !Int
  | AddSnapshot           !Snapshot
  | SetSnapshotRef        !Text !SnapshotRefUpdate
  | RemoveSnapshots       !(Vector Int64)
  | RemoveSnapshotRef     !Text
  | SetLocation           !Text
  | SetProperties         !(Map Text Text)
  | RemoveProperties      !(Vector Text)
  deriving stock (Show, Eq)

data SnapshotRefUpdate = SnapshotRefUpdate
  { sruSnapshotId         :: !Int64
  , sruRefType            :: !Text  -- "branch" or "tag"
  , sruMinSnapshotsToKeep :: !(Maybe Int)
  , sruMaxSnapshotAgeMs   :: !(Maybe Int64)
  , sruMaxRefAgeMs        :: !(Maybe Int64)
  } deriving stock (Show, Eq)

instance A.ToJSON SnapshotRefUpdate where
  toJSON r = A.object $
    [ "snapshot-id" .= sruSnapshotId r
    , "type"        .= sruRefType r
    ] ++ optionalField "min-snapshots-to-keep"  (sruMinSnapshotsToKeep r)
      ++ optionalField "max-snapshot-age-ms"    (sruMaxSnapshotAgeMs r)
      ++ optionalField "max-ref-age-ms"         (sruMaxRefAgeMs r)

instance A.ToJSON TableUpdate where
  toJSON = \case
    AssignUUID u            -> tag "assign-uuid"             [ "uuid" .= u ]
    UpgradeFormatVersion v  -> tag "upgrade-format-version"  [ "format-version" .= v ]
    AddSchema s mLcid       -> tag "add-schema"              ([ "schema" .= s ] ++ optionalField "last-column-id" mLcid)
    SetCurrentSchema sid    -> tag "set-current-schema"      [ "schema-id" .= sid ]
    AddPartitionSpec p      -> tag "add-spec"                [ "spec" .= p ]
    SetDefaultSpec sid      -> tag "set-default-spec"        [ "spec-id" .= sid ]
    AddSortOrder so         -> tag "add-sort-order"          [ "sort-order" .= so ]
    SetDefaultSortOrder soid -> tag "set-default-sort-order" [ "sort-order-id" .= soid ]
    AddSnapshot s           -> tag "add-snapshot"            [ "snapshot" .= s ]
    SetSnapshotRef nm su    -> tag "set-snapshot-ref"        ([ "ref-name" .= nm ] ++ snapshotRefFields su)
    RemoveSnapshots ids     -> tag "remove-snapshots"        [ "snapshot-ids" .= ids ]
    RemoveSnapshotRef nm    -> tag "remove-snapshot-ref"     [ "ref-name" .= nm ]
    SetLocation l           -> tag "set-location"            [ "location" .= l ]
    SetProperties props     -> tag "set-properties"          [ "updates" .= props ]
    RemoveProperties keys   -> tag "remove-properties"       [ "removals" .= keys ]
    where
      tag :: Text -> [A.Pair] -> A.Value
      tag t pairs = A.object (("action" .= t) : pairs)
      snapshotRefFields :: SnapshotRefUpdate -> [A.Pair]
      snapshotRefFields = onSrfu
        where
          onSrfu r =
            [ "snapshot-id" .= sruSnapshotId r
            , "type"        .= sruRefType r
            ] ++ optionalField "min-snapshots-to-keep" (sruMinSnapshotsToKeep r)
              ++ optionalField "max-snapshot-age-ms"   (sruMaxSnapshotAgeMs r)
              ++ optionalField "max-ref-age-ms"        (sruMaxRefAgeMs r)

-- | Optimistic-concurrency precondition.
data TableRequirement
  = AssertCreate
    -- ^ The table must not already exist (used with stage-create commits).
  | AssertTableUUID         !Text
  | AssertRefSnapshotId     !Text !(Maybe Int64)
  | AssertLastAssignedFieldId !Int
  | AssertCurrentSchemaId   !Int
  | AssertLastAssignedPartitionId !Int
  | AssertDefaultSpecId     !Int
  | AssertDefaultSortOrderId !Int
  deriving stock (Show, Eq)

instance A.ToJSON TableRequirement where
  toJSON = \case
    AssertCreate                    -> tag "assert-create" []
    AssertTableUUID u               -> tag "assert-table-uuid"            [ "uuid" .= u ]
    AssertRefSnapshotId nm mSid     -> tag "assert-ref-snapshot-id"
                                          ([ "ref" .= nm ] ++ optionalField "snapshot-id" mSid)
    AssertLastAssignedFieldId fid   -> tag "assert-last-assigned-field-id"     [ "last-assigned-field-id" .= fid ]
    AssertCurrentSchemaId sid       -> tag "assert-current-schema-id"          [ "current-schema-id" .= sid ]
    AssertLastAssignedPartitionId i -> tag "assert-last-assigned-partition-id" [ "last-assigned-partition-id" .= i ]
    AssertDefaultSpecId sid         -> tag "assert-default-spec-id"            [ "default-spec-id" .= sid ]
    AssertDefaultSortOrderId soid   -> tag "assert-default-sort-order-id"      [ "default-sort-order-id" .= soid ]
    where
      tag :: Text -> [A.Pair] -> A.Value
      tag t pairs = A.object (("type" .= t) : pairs)

data CommitTableReq = CommitTableReq
  { cmtIdentifier   :: !TableIdentifier
  , cmtRequirements :: !(Vector TableRequirement)
  , cmtUpdates      :: !(Vector TableUpdate)
  } deriving stock (Show, Eq)

instance A.ToJSON CommitTableReq where
  toJSON r = A.object
    [ "identifier"   .= cmtIdentifier r
    , "requirements" .= cmtRequirements r
    , "updates"      .= cmtUpdates r
    ]

data CommitTableResp = CommitTableResp
  { ctrMetadataLocation :: !Text
  , ctrMetadata         :: !TableMetadata
  } deriving stock (Show, Eq)

instance A.FromJSON CommitTableResp where
  parseJSON = A.withObject "CommitTableResp" $ \o ->
    CommitTableResp
      <$> o .: "metadata-location"
      <*> o .: "metadata"

-- | @POST \/v1\/namespaces\/\<ns\>\/tables\/\<table\>@.
commitTable
  :: Monad m
  => CatalogClient m
  -> TableIdentifier
  -> CommitTableReq
  -> m (Either ErrorResponse CommitTableResp)
commitTable cc ti req = runPost cc (tablePath ti) req

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data ErrorResponse = ErrorResponse
  { erMessage :: !Text
  , erType    :: !Text
  , erCode    :: !Int
  , erRaw     :: !ByteString
  } deriving stock (Show, Eq)

instance A.FromJSON ErrorResponse where
  parseJSON = A.withObject "ErrorResponse" $ \o -> do
    inner <- o .: "error"
    A.withObject "Error" (\e -> do
      msg <- e .: "message"
      ty  <- e .: "type"
      cod <- e .: "code"
      pure (ErrorResponse msg ty cod mempty)) inner

------------------------------------------------------------------------
-- HTTP helpers
------------------------------------------------------------------------

runGet
  :: (A.FromJSON a, Monad m)
  => CatalogClient m -> Text -> [(Text, Text)]
  -> m (Either ErrorResponse a)
runGet cc path query = do
  resp <- ccPerform cc (Request GET path query (authHeaders cc) Nothing)
  pure (parseRespOk resp)

runPost
  :: (A.ToJSON req, A.FromJSON resp, Monad m)
  => CatalogClient m -> Text -> req
  -> m (Either ErrorResponse resp)
runPost cc path body = do
  resp <- ccPerform cc (Request POST path [] (jsonHeaders <> authHeaders cc) (Just (encodeBody body)))
  pure (parseRespOk resp)

runPostUnit
  :: (A.ToJSON req, Monad m)
  => CatalogClient m -> Text -> req
  -> m (Either ErrorResponse ())
runPostUnit cc path body = do
  resp <- ccPerform cc (Request POST path [] (jsonHeaders <> authHeaders cc) (Just (encodeBody body)))
  pure (parseUnit resp)

runDelete
  :: Monad m
  => CatalogClient m -> Text
  -> m (Either ErrorResponse ())
runDelete cc path = do
  resp <- ccPerform cc (Request DELETE path [] (authHeaders cc) Nothing)
  pure (parseUnit resp)

authHeaders :: CatalogClient m -> [(Text, Text)]
authHeaders cc = case ccAuth cc of
  AuthNone        -> []
  AuthBearer tok  -> [("Authorization", "Bearer " <> tok)]
  AuthCustom hs   -> Map.toList hs

jsonHeaders :: [(Text, Text)]
jsonHeaders = [("Content-Type", "application/json")]

encodeBody :: A.ToJSON a => a -> ByteString
encodeBody = BL.toStrict . A.encode

parseRespOk :: A.FromJSON a => Response -> Either ErrorResponse a
parseRespOk r
  | respStatus r >= 200 && respStatus r < 300 =
      case A.eitherDecodeStrict (respBody r) of
        Right v -> Right v
        Left e  -> Left (ErrorResponse (T.pack ("response decode error: " ++ e))
                                       "DecodeError"
                                       (respStatus r)
                                       (respBody r))
  | otherwise = Left (parseError r)

parseUnit :: Response -> Either ErrorResponse ()
parseUnit r
  | respStatus r >= 200 && respStatus r < 300 = Right ()
  | otherwise = Left (parseError r)

parseError :: Response -> ErrorResponse
parseError r = case A.eitherDecodeStrict (respBody r) of
  Right e -> e { erRaw = respBody r }
  Left  _ -> ErrorResponse
    (T.pack ("HTTP " ++ show (respStatus r)))
    "UnknownError"
    (respStatus r)
    (respBody r)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

optionalField :: A.ToJSON a => AK.Key -> Maybe a -> [A.Pair]
optionalField _ Nothing  = []
optionalField k (Just v) = [k .= v]

catMaybes2 :: [Maybe a] -> [a]
catMaybes2 = foldr step []
  where
    step Nothing  acc = acc
    step (Just x) acc = x : acc