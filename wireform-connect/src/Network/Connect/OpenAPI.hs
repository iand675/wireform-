{-# LANGUAGE OverloadedStrings #-}

{- | Generate an [OpenAPI 3.1](https://spec.openapis.org/oas/v3.1.0) document
describing a set of Connect RPC services.

This is the Connect-specific HTTP shaping half of schema-derived doc
generation. The transport-agnostic JSON Schema walk lives in
"Proto.JSONSchema" (@wireform-proto@); this module wraps those schemas in the
Connect wire conventions:

* one path @\/pkg.Service\/Method@ per method, with a @POST@ operation;
* unary bodies are bare JSON (@application\/json@); streaming bodies are the
  enveloped Connect stream (@application\/connect+json@), flagged with a
  @x-connect-streaming@ vendor extension (@client@ \/ @server@ \/ @bidi@)
  because a stream is not one JSON document;
* side-effect-free unary methods (@option idempotency_level = NO_SIDE_EFFECTS@)
  additionally get a cacheable @GET@ operation with Connect's
  @?message=&encoding=&base64=&compression=&connect=@ query parameters;
* every operation carries a @default@ response referencing the shared
  Connect error envelope (@code@ \/ @message@ \/ @details@), whose @code@ enum
  is the sixteen Connect error codes from "Network.Connect.Error".

OpenAPI describes the JSON codec, so the referenced schemas match the
proto3-canonical-JSON bytes exactly (see "Proto.JSONSchema").

The output is rendered with sorted keys ('renderOpenApi') so it is
byte-deterministic — suitable for golden tests and for checking into a repo.
-}
module Network.Connect.OpenAPI (
  -- * Options
  OpenApiOptions (..),
  defaultOpenApiOptions,

  -- * Generation
  connectOpenApi,
  connectOpenApiWith,
  connectOpenApiAnnotated,
  renderOpenApi,

  -- * Composable annotators
  Annotators (..),
  noAnnotators,
  schemaAnnotators,
  deprecationAnnotators,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), NumberFormat (..), encodePretty')
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Network.Connect.Error (allConnectCodes, connectCodeName)
import Proto.IDL.Annotations (lookupSimpleOption, optionAsIdent)
import Proto.IDL.AST
import Proto.IDL.Options (isDeprecated)
import Proto.JSONSchema
  ( SchemaEnv
  , SchemaOptions
  , buildSchemaEnv
  , componentSchemasWith
  , deprecationSchemaOptions
  , refForFqn
  , resolveTypeFqn
  , wellKnownSchema
  )


-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | Document-level metadata for the generated OpenAPI spec.
data OpenApiOptions = OpenApiOptions
  { ooTitle :: !Text
  -- ^ @info.title@.
  , ooVersion :: !Text
  -- ^ @info.version@.
  , ooServers :: ![Text]
  -- ^ @servers[].url@ base URLs; empty omits the @servers@ key.
  }
  deriving stock (Show, Eq)


-- | Sensible defaults: generic title, version @0.0.0@, no servers.
defaultOpenApiOptions :: OpenApiOptions
defaultOpenApiOptions =
  OpenApiOptions
    { ooTitle = "Connect API"
    , ooVersion = "0.0.0"
    , ooServers = []
    }


-- ---------------------------------------------------------------------------
-- Document
-- ---------------------------------------------------------------------------

{- | Build the OpenAPI 3.1 document.

@svcFiles@ are the files whose services become paths; @schemaFiles@ is the
full set (targets + transitive imports) used to build the schema components
and resolve type references. For a single self-contained file, pass it as
both.
-}
connectOpenApi :: OpenApiOptions -> [ProtoFile] -> [ProtoFile] -> Value
connectOpenApi = connectOpenApiAnnotated noAnnotators


{- | 'connectOpenApi' with external schema annotations (e.g. protovalidate
rules → JSON Schema validation keywords + @x-@ extensions) applied to the
component schemas. (Operation-level annotations are left at their default; use
'connectOpenApiAnnotated' for those.)
-}
connectOpenApiWith :: SchemaOptions -> OpenApiOptions -> [ProtoFile] -> [ProtoFile] -> Value
connectOpenApiWith sopts = connectOpenApiAnnotated (schemaAnnotators sopts)


{- | A bundle of composable annotators: schema-level ('SchemaOptions', for
components) plus operation-level (extra keywords merged into each Connect
operation object). Both compose — 'Annotators' is a 'Monoid' — so independent
concerns (protovalidate, deprecation, custom @x-@ tags, auth, …) stack with
@<>@.
-}
data Annotators = Annotators
  { anSchema :: SchemaOptions
  -- ^ Component-schema annotations (fields / messages / enums).
  , anOperation :: Text -> Text -> [Pair]
  -- ^ @serviceFqn -> methodName -> extra operation keywords@ (e.g. @deprecated@, @tags@).
  }


instance Semigroup Annotators where
  Annotators s1 o1 <> Annotators s2 o2 =
    Annotators (s1 <> s2) (\svc meth -> o1 svc meth <> o2 svc meth)


instance Monoid Annotators where
  mempty = Annotators mempty (\_ _ -> [])


-- | No annotations.
noAnnotators :: Annotators
noAnnotators = mempty


-- | Lift a schema-only 'SchemaOptions' into an 'Annotators' bundle.
schemaAnnotators :: SchemaOptions -> Annotators
schemaAnnotators sopts = mempty {anSchema = sopts}


{- | 'connectOpenApi' with a full 'Annotators' bundle: schema annotations feed
the component schemas, operation annotations merge into each method's
operation object(s).
-}
connectOpenApiAnnotated :: Annotators -> OpenApiOptions -> [ProtoFile] -> [ProtoFile] -> Value
connectOpenApiAnnotated ann opts svcFiles schemaFiles =
  object $
    [ "openapi" .= ("3.1.0" :: Text)
    , "info" .= object ["title" .= ooTitle opts, "version" .= ooVersion opts]
    ]
      <> serversField
      <> [ "paths" .= object (concatMap (fileMethods (anOperation ann) env) svcFiles)
         , "components" .= object ["schemas" .= object schemas]
         ]
  where
    env = buildSchemaEnv schemaFiles
    schemas =
      fmap (\(k, v) -> AKey.fromText k .= v) (componentSchemasWith (anSchema ann) env)
        <> errorComponents
    serversField = case ooServers opts of
      [] -> []
      urls -> ["servers" .= Array (V.fromList (fmap (\u -> object ["url" .= u]) urls))]


{- | A built-in 'Annotators' bundle carrying the standard proto @deprecated@
option through: on component schemas (messages / fields / enums) and on
operations (deprecated RPCs, and every RPC of a deprecated service) as
OpenAPI @deprecated: true@. Pass the files used to build the doc; compose with
others via @<>@.
-}
deprecationAnnotators :: [ProtoFile] -> Annotators
deprecationAnnotators files =
  Annotators
    { anSchema = deprecationSchemaOptions files
    , anOperation = \svc meth ->
        if Set.member (svc, meth) depMethods then ["deprecated" .= True] else []
    }
  where
    depMethods = collectDeprecatedMethods files


-- | @(serviceFqn, methodName)@ of every deprecated RPC (a deprecated service
-- deprecates all its methods).
collectDeprecatedMethods :: [ProtoFile] -> Set (Text, Text)
collectDeprecatedMethods files = Set.fromList (concatMap fileMs files)
  where
    fileMs pf =
      let pkg = fromMaybe "" (protoPackage pf)
      in concatMap (svcMs pkg) (mapMaybe justService (protoTopLevels pf))
    justService (TLService s) = Just s
    justService _ = Nothing
    svcMs pkg svc =
      let fqn = if T.null pkg then svcName svc else pkg <> "." <> svcName svc
          svcDep = isDeprecated (svcOptionDefs svc)
       in mapMaybe (methodM fqn svcDep) (svcRpcs svc)
    methodM fqn svcDep rpc =
      if svcDep || isDeprecated (rpcOptions rpc) then Just (fqn, rpcName rpc) else Nothing


-- | The option defs declared directly on a service.
svcOptionDefs :: ServiceDef -> [OptionDef]
svcOptionDefs = svcOptions


-- | Render a document with sorted keys and 2-space indent (deterministic).
renderOpenApi :: Value -> ByteString
renderOpenApi =
  encodePretty'
    Config
      { confIndent = Spaces 2
      , confCompare = compare
      , confNumFormat = Generic
      , confTrailingNewline = True
      }


-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | An operation-level annotator: @serviceFqn -> methodName -> extra keywords@.
type OperationAnnotator = Text -> Text -> [Pair]


-- | The @(path, item)@ pairs contributed by every service in one file.
fileMethods :: OperationAnnotator -> SchemaEnv -> ProtoFile -> [(Aeson.Key, Value)]
fileMethods opAnn env pf =
  concatMap serviceMethods (fileServices pf)
  where
    pkg = fromMaybe "" (protoPackage pf)
    serviceMethods svc =
      fmap (methodPath opAnn env pkg svc) (svcRpcs svc)


fileServices :: ProtoFile -> [ServiceDef]
fileServices pf = mapMaybe justService (protoTopLevels pf)
  where
    justService (TLService s) = Just s
    justService _ = Nothing


-- | One path item: @\/pkg.Service\/Method@ → @{post, [get]}@.
methodPath :: OperationAnnotator -> SchemaEnv -> Text -> ServiceDef -> RpcDef -> (Aeson.Key, Value)
methodPath opAnn env pkg svc rpc =
  (AKey.fromText path, object operations)
  where
    fqService = if T.null pkg then svcName svc else pkg <> "." <> svcName svc
    path = "/" <> fqService <> "/" <> rpcName rpc
    opKws = opAnn fqService (rpcName rpc)
    operations = ["post" .= postOp] <> getOps
    postOp = mergeOpKeywords opKws (operationObject env pkg svc rpc)
    getOps =
      if isUnary rpc && isNoSideEffects rpc
        then ["get" .= mergeOpKeywords opKws (getOperationObject env pkg svc rpc)]
        else []


-- | Merge operation-level keywords into an operation object (existing keys win).
mergeOpKeywords :: [Pair] -> Value -> Value
mergeOpKeywords [] v = v
mergeOpKeywords kws (Object o) = Object (AKM.union o (AKM.fromList kws))
mergeOpKeywords _ v = v


-- | The POST operation for a method.
operationObject :: SchemaEnv -> Text -> ServiceDef -> RpcDef -> Value
operationObject env pkg svc rpc =
  withDoc (rpcDoc rpc) $
    object
      [ "operationId" .= (svcName svc <> "_" <> rpcName rpc)
      , "tags" .= Array (V.fromList [String (svcName svc)])
      , "requestBody" .= requestBody
      , "responses" .= responsesObject env pkg rpc
      ]
  where
    requestBody =
      object
        [ "required" .= True
        , "content" .= object [contentTypeFor rpc .= object ["schema" .= inputSchema]]
        ]
    inputSchema = decorateStreaming rpc (typeSchema env pkg (rpcInput rpc))


-- | The GET operation for a side-effect-free unary method (Connect's
-- cacheable GET: the message rides the query string).
getOperationObject :: SchemaEnv -> Text -> ServiceDef -> RpcDef -> Value
getOperationObject env pkg svc rpc =
  withDoc (rpcDoc rpc) $
    object
      [ "operationId" .= (svcName svc <> "_" <> rpcName rpc <> "_GET")
      , "tags" .= Array (V.fromList [String (svcName svc)])
      , "parameters" .= Array (V.fromList getParameters)
      , "responses" .= responsesObject env pkg rpc
      ]


-- | Connect GET query parameters (spec: /Get Requests/).
getParameters :: [Value]
getParameters =
  [ queryParam "message" True (object ["type" .= ("string" :: Text)]) "The base64url- or JSON-encoded request message."
  , queryParam "encoding" True (object ["type" .= ("string" :: Text), "enum" .= arr ["proto", "json"]]) "The message codec."
  , queryParam "base64" False (object ["type" .= ("string" :: Text), "enum" .= arr ["0", "1"]]) "1 if `message` is base64url-encoded."
  , queryParam "compression" False (object ["type" .= ("string" :: Text)]) "The message compression (e.g. gzip)."
  , queryParam "connect" False (object ["type" .= ("string" :: Text), "enum" .= arr ["v1"]]) "The Connect protocol version."
  ]
  where
    arr = Array . V.fromList . fmap String


queryParam :: Text -> Bool -> Value -> Text -> Value
queryParam name req schema desc =
  object
    [ "name" .= name
    , "in" .= ("query" :: Text)
    , "required" .= req
    , "description" .= desc
    , "schema" .= schema
    ]


-- | @responses@ map: @200@ (the output message) + @default@ (Connect error).
responsesObject :: SchemaEnv -> Text -> RpcDef -> Value
responsesObject env pkg rpc =
  object
    [ "200"
        .= object
          [ "description" .= ("Success" :: Text)
          , "content" .= object [contentTypeFor rpc .= object ["schema" .= outputSchema]]
          ]
    , "default"
        .= object
          [ "description" .= ("Error" :: Text)
          , "content" .= object ["application/json" .= object ["schema" .= refForFqn "connect.Error"]]
          ]
    ]
  where
    outputSchema = decorateStreaming rpc (typeSchema env pkg (rpcOutput rpc))


-- ---------------------------------------------------------------------------
-- Streaming / content-type shaping
-- ---------------------------------------------------------------------------

-- | Is this a unary (non-streaming) method?
isUnary :: RpcDef -> Bool
isUnary rpc = rpcInputStr rpc == NoStream && rpcOutputStr rpc == NoStream


-- | The streaming label per Connect's kinds, or 'Nothing' for unary.
streamingKind :: RpcDef -> Maybe Text
streamingKind rpc = case (rpcInputStr rpc, rpcOutputStr rpc) of
  (NoStream, NoStream) -> Nothing
  (Streaming, NoStream) -> Just "client"
  (NoStream, Streaming) -> Just "server"
  (Streaming, Streaming) -> Just "bidi"


-- | Body content-type: bare JSON for unary, enveloped Connect stream otherwise.
contentTypeFor :: RpcDef -> Aeson.Key
contentTypeFor rpc
  | isUnary rpc = "application/json"
  | otherwise = "application/connect+json"


{- | Tag a streaming method's message schema with @x-connect-streaming@ so a
reader knows the body is a sequence of enveloped frames of this schema, not a
single document.
-}
decorateStreaming :: RpcDef -> Value -> Value
decorateStreaming rpc schema = case (streamingKind rpc, schema) of
  (Just kind, Object o) ->
    Object (AKM.insert "x-connect-streaming" (String kind) o)
  _ -> schema


-- | @option idempotency_level = NO_SIDE_EFFECTS@ ?
isNoSideEffects :: RpcDef -> Bool
isNoSideEffects rpc =
  (lookupSimpleOption "idempotency_level" (rpcOptions rpc) >>= optionAsIdent)
    == Just "NO_SIDE_EFFECTS"


-- ---------------------------------------------------------------------------
-- Type references
-- ---------------------------------------------------------------------------

{- | Schema for a message type named in an RPC signature: inline the WKT shape
if it is a well-known type, otherwise a @$ref@ to its component. Falls back to
a permissive schema for an unresolved name.
-}
typeSchema :: SchemaEnv -> Text -> Text -> Value
typeSchema env pkg name =
  case wellKnownSchema (stripLeadingDot name) of
    Just wkt -> wkt
    Nothing -> case resolveTypeFqn env pkg [] name of
      Just fqn -> fromMaybe (refForFqn fqn) (wellKnownSchema fqn)
      Nothing -> object ["description" .= ("unresolved type: " <> name)]


-- | Drop a single leading @.@ from a fully-qualified proto name.
stripLeadingDot :: Text -> Text
stripLeadingDot t = fromMaybe t (T.stripPrefix "." t)


-- ---------------------------------------------------------------------------
-- Error envelope components
-- ---------------------------------------------------------------------------

-- | The @connect.Error@ + @connect.Error.Detail@ schema components.
errorComponents :: [Pair]
errorComponents =
  [ "connect.Error"
      .= object
        [ "type" .= ("object" :: Text)
        , "description" .= ("The Connect error envelope." :: Text)
        , "properties"
            .= object
              [ "code" .= object ["type" .= ("string" :: Text), "enum" .= codeEnum]
              , "message" .= object ["type" .= ("string" :: Text)]
              , "details"
                  .= object
                    [ "type" .= ("array" :: Text)
                    , "items" .= refForFqn "connect.Error.Detail"
                    ]
              ]
        , "required" .= Array (V.fromList [String "code"])
        ]
  , "connect.Error.Detail"
      .= object
        [ "type" .= ("object" :: Text)
        , "description" .= ("A base64-encoded error detail (a protobuf Any)." :: Text)
        , "properties"
            .= object
              [ "type" .= object ["type" .= ("string" :: Text)]
              , "value" .= object ["type" .= ("string" :: Text), "format" .= ("byte" :: Text)]
              , "debug" .= object []
              ]
        ]
  ]
  where
    codeEnum = Array (V.fromList (fmap (String . connectCodeName) allConnectCodes))


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Attach a @summary@ to an operation object from a proto doc-comment.
withDoc :: Maybe Text -> Value -> Value
withDoc mdoc v = case (fmap T.strip mdoc, v) of
  (Just d, Object o) | not (T.null d) -> Object (AKM.insert "summary" (String d) o)
  _ -> v
