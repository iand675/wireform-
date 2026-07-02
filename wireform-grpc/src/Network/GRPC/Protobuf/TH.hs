{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell generation of gRPC /service/ bindings.

@wireform-proto@'s 'Proto.TH.loadProto' generates the Haskell /message/
types from a @.proto@ file, but it deliberately stops short of the gRPC
service glue (the @Protobuf@-tagged RPC types and their @IsRPC@ /
@SupportsClientRpc@ / @SupportsServerRpc@ / @HasStreamingType@
instances). Historically that glue was hand-written, one stanza per
method (see the vendored @grapesy@ examples @Proto.API.Helloworld@,
@Proto.API.RouteGuide@, …). This module removes the boilerplate.

== Usage

@
{\-\# LANGUAGE TemplateHaskell \#-\}
{\-\# LANGUAGE DataKinds \#-\}
{\-\# LANGUAGE FlexibleInstances \#-\}
{\-\# LANGUAGE MultiParamTypeClasses \#-\}
{\-\# LANGUAGE OverloadedStrings \#-\}
{\-\# LANGUAGE TypeFamilies \#-\}
{\-\# LANGUAGE UndecidableInstances \#-\}

module Proto.API.RouteGuide (
    module Proto.API.RouteGuide
  , module Proto.RouteGuide
  ) where

import Network.GRPC.Protobuf.TH (loadProtoServices)
import Proto.RouteGuide          -- the message types, from loadProto

\$(loadProtoServices "proto/route_guide.proto")
@

For a service @routeguide.RouteGuide@ with a method
@rpc GetFeature(Point) returns (Feature)@ the splice produces:

@
data RouteGuide

type GetFeature = Protobuf RouteGuide "getFeature"

type instance Input  GetFeature = Proto Point
type instance Output GetFeature = Proto Feature

instance IsRPC GetFeature where
  rpcContentType _ = "application\/grpc+proto"
  rpcServiceName _ = "routeguide.RouteGuide"
  rpcMethodName  _ = "GetFeature"
  rpcMessageType _ = Just "routeguide.Point"

instance SupportsClientRpc GetFeature where
  rpcSerializeInput    _ = buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . parseLazy

instance SupportsServerRpc GetFeature where
  rpcDeserializeInput _ = fmap Proto . parseLazy
  rpcSerializeOutput  _ = buildLazy . getProto

instance SupportsStreamingType GetFeature 'NonStreaming
instance HasStreamingType GetFeature where
  type RpcStreamingType GetFeature = 'NonStreaming
@

plus, once per service:

@
type instance RequestMetadata          (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf RouteGuide meth) = NoMetadata
type instance ServiceMethods RouteGuide = '["getFeature", ...]
@

The message types referenced by @Input@ \/ @Output@ (here @Point@ and
@Feature@) must be in scope at the splice site — bring them in by
importing the @loadProto@-generated module for the same @.proto@ file.

== Custom metadata

The three @*Metadata@ families default to 'NoMetadata'. When a service
needs typed metadata, set 'sgoGenerateMetadata' to 'False' in
'loadProtoServicesWith' and hand-write the families (and their
'Network.GRPC.Spec.BuildMetadata' \/ 'Network.GRPC.Spec.ParseMetadata'
instances) yourself; everything else is still generated.
-}
module Network.GRPC.Protobuf.TH (
    -- * Generators
    loadProtoServices
  , loadProtoServicesWith
    -- * Options
  , ServiceGenOpts (..)
  , defaultServiceGenOpts
  ) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)

import Proto.CodeGen (hsTypeName, lowerFirst)
import Proto.IDL.AST
import Proto.IDL.Parser (parseProtoFile, renderParseError)

import Network.GRPC.Common.Protobuf (Proto (..), Protobuf, getProto)
import Network.GRPC.Spec (
  HasStreamingType (..),
  ServiceMethods,
  Input,
  IsRPC (..),
  NoMetadata,
  Output,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
  StreamingType (..),
  SupportsClientRpc (..),
  SupportsServerRpc (..),
  SupportsStreamingType,
 )
import Network.GRPC.Spec.Util.Protobuf (buildLazy, parseLazy)


-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

{- | Knobs controlling service generation.

Use 'defaultServiceGenOpts' and override the fields you need.
-}
newtype ServiceGenOpts = ServiceGenOpts
  { sgoGenerateMetadata :: Bool
  -- ^ When 'True' (the default) emit
  -- @type instance RequestMetadata (Protobuf Svc meth) = NoMetadata@
  -- (and the response-initial \/ response-trailing variants) for every
  -- service. Set to 'False' to hand-write typed metadata families.
  }


-- | Generate metadata families, defaulting them to 'NoMetadata'.
defaultServiceGenOpts :: ServiceGenOpts
defaultServiceGenOpts = ServiceGenOpts {sgoGenerateMetadata = True}


-- ---------------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------------

{- | Parse a @.proto@ file and splice the gRPC service bindings for every
@service@ it declares, using 'defaultServiceGenOpts'.
-}
loadProtoServices :: FilePath -> Q [Dec]
loadProtoServices = loadProtoServicesWith defaultServiceGenOpts


-- | Like 'loadProtoServices' but with explicit 'ServiceGenOpts'.
loadProtoServicesWith :: ServiceGenOpts -> FilePath -> Q [Dec]
loadProtoServicesWith opts path = do
  addDependentFile path
  contents <- runIO (TIO.readFile path)
  case parseProtoFile path contents of
    Left err -> fail (renderParseError err)
    Right pf -> do
      let pkg = fromMaybe T.empty (protoPackage pf)
          services = collectServices (protoTopLevels pf)
      concat <$> mapM (serviceToDecls opts pf pkg) services


collectServices :: [TopLevel] -> [ServiceDef]
collectServices = foldr step []
  where
    step (TLService s) acc = s : acc
    step _ acc = acc


-- ---------------------------------------------------------------------------
-- Per-service generation
-- ---------------------------------------------------------------------------

serviceToDecls :: ServiceGenOpts -> ProtoFile -> Text -> ServiceDef -> Q [Dec]
serviceToDecls opts pf pkg svc = do
  let servHsName = mkName (T.unpack (pascalName (svcName svc)))
      fqServiceName = qualify pkg (svcName svc)
  servData <- dataD (pure []) servHsName [] Nothing [] []
  rpcDecls <-
    concat
      <$> traverse (rpcToDecls pf pkg servHsName fqServiceName) (svcRpcs svc)
  metaDecls <-
    if sgoGenerateMetadata opts
      then serviceMetadataDecls servHsName
      else pure []
  methodsDecl <- serviceMethodsDecl servHsName (svcRpcs svc)
  pure (servData : rpcDecls <> metaDecls <> [methodsDecl])


-- | All declarations for a single RPC method.
rpcToDecls :: ProtoFile -> Text -> Name -> Text -> RpcDef -> Q [Dec]
rpcToDecls pf pkg servHsName fqServiceName rpc = do
  let methHsName = mkName (T.unpack (pascalName (rpcName rpc)))
      methSym = methodSymbol (rpcName rpc)
      inHs = resolveHsType pf (rpcInput rpc)
      outHs = resolveHsType pf (rpcOutput rpc)
      inFq = fqMessageName pkg (rpcInput rpc)
      streamCon = streamingCon (rpcInputStr rpc) (rpcOutputStr rpc)
      protobufHead =
        appT (appT (conT ''Protobuf) (conT servHsName)) (strLitT methSym)
      protoOf hs = appT (conT ''Proto) (conT (mkName (T.unpack hs)))

  synDec <- tySynD methHsName [] protobufHead

  inputInst <-
    tySynInstD
      (tySynEqn Nothing (appT (conT ''Input) (conT methHsName)) (protoOf inHs))
  outputInst <-
    tySynInstD
      (tySynEqn Nothing (appT (conT ''Output) (conT methHsName)) (protoOf outHs))

  isRpcInst <- mkIsRpcInstance methHsName fqServiceName (rpcName rpc) inFq
  clientInst <- mkClientInstance methHsName
  serverInst <- mkServerInstance methHsName

  supportsInst <-
    instanceD
      (pure [])
      (appT (appT (conT ''SupportsStreamingType) (conT methHsName)) (promotedT streamCon))
      []
  hasStreamingInst <-
    instanceD
      (pure [])
      (appT (conT ''HasStreamingType) (conT methHsName))
      [ tySynInstD
          ( tySynEqn
              Nothing
              (appT (conT ''RpcStreamingType) (conT methHsName))
              (promotedT streamCon)
          )
      ]

  pure
    [ synDec
    , inputInst
    , outputInst
    , isRpcInst
    , clientInst
    , serverInst
    , supportsInst
    , hasStreamingInst
    ]


mkIsRpcInstance :: Name -> Text -> Text -> Text -> Q Dec
mkIsRpcInstance methHsName fqService methodName inFq =
  instanceD
    (pure [])
    (appT (conT ''IsRPC) (conT methHsName))
    [ funD 'rpcContentType [clause [wildP] (normalB (bsLit "application/grpc+proto")) []]
    , funD 'rpcServiceName [clause [wildP] (normalB (bsLit fqService)) []]
    , funD 'rpcMethodName [clause [wildP] (normalB (bsLit methodName)) []]
    , funD 'rpcMessageType [clause [wildP] (normalB (appE (conE 'Just) (bsLit inFq))) []]
    ]


mkClientInstance :: Name -> Q Dec
mkClientInstance methHsName =
  instanceD
    (pure [])
    (appT (conT ''SupportsClientRpc) (conT methHsName))
    [ funD 'rpcSerializeInput [clause [wildP] (normalB [|buildLazy . getProto|]) []]
    , funD 'rpcDeserializeOutput [clause [wildP] (normalB [|fmap Proto . parseLazy|]) []]
    ]


mkServerInstance :: Name -> Q Dec
mkServerInstance methHsName =
  instanceD
    (pure [])
    (appT (conT ''SupportsServerRpc) (conT methHsName))
    [ funD 'rpcDeserializeInput [clause [wildP] (normalB [|fmap Proto . parseLazy|]) []]
    , funD 'rpcSerializeOutput [clause [wildP] (normalB [|buildLazy . getProto|]) []]
    ]


-- | The three response\/request metadata families, defaulted to 'NoMetadata'.
serviceMetadataDecls :: Name -> Q [Dec]
serviceMetadataDecls servHsName =
  traverse mkMeta [''RequestMetadata, ''ResponseInitialMetadata, ''ResponseTrailingMetadata]
  where
    methV = mkName "meth"
    protobufApplied =
      appT (appT (conT ''Protobuf) (conT servHsName)) (varT methV)
    mkMeta fam =
      tySynInstD
        (tySynEqn Nothing (appT (conT fam) protobufApplied) (conT ''NoMetadata))


-- | @type instance ServiceMethods Svc = '[ ...method symbols... ]@.
serviceMethodsDecl :: Name -> [RpcDef] -> Q Dec
serviceMethodsDecl servHsName rpcs =
  tySynInstD
    ( tySynEqn
        Nothing
        (appT (conT ''ServiceMethods) (conT servHsName))
        (promotedSymbolList syms)
    )
  where
    syms = fmap (methodSymbol . rpcName) rpcs


-- ---------------------------------------------------------------------------
-- Small TH builders
-- ---------------------------------------------------------------------------

-- | A type-level string literal, e.g. @"sayHello"@.
strLitT :: Text -> Q Type
strLitT t = litT (strTyLit (T.unpack t))


-- | A promoted list of 'Symbol' literals: @'[ "a", "b" ]@.
promotedSymbolList :: [Text] -> Q Type
promotedSymbolList =
  foldr (\s acc -> appT (appT promotedConsT (strLitT s)) acc) promotedNilT


{- | A string literal lifted through 'fromString' so the generated code
type-checks against any @IsString@ target (here the strict 'ByteString'
results of the 'IsRPC' methods) without relying on @OverloadedStrings@
being enabled at the splice site.
-}
bsLit :: Text -> Q Exp
bsLit t = appE (varE 'fromString) (litE (stringL (T.unpack t)))


-- | The promoted 'StreamingType' constructor implied by the in\/out qualifiers.
streamingCon :: StreamQualifier -> StreamQualifier -> Name
streamingCon inS outS = case (inS, outS) of
  (NoStream, NoStream) -> 'NonStreaming
  (NoStream, Streaming) -> 'ServerStreaming
  (Streaming, NoStream) -> 'ClientStreaming
  (Streaming, Streaming) -> 'BiDiStreaming


{- | Pascal-case a proto identifier while /preserving/ interior capitals:
@RouteGuide@ stays @RouteGuide@, @get_feature@ becomes @GetFeature@. Unlike
'Proto.CodeGen.snakeToPascal' (which lower-cases each segment's tail) this is
safe for the already-PascalCase service and method names gRPC uses. -}
pascalName :: Text -> Text
pascalName t = T.concat (fmap upperFirst (T.splitOn (T.singleton '_') t))
  where
    upperFirst x = case T.uncons x of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing -> x


-- | The method symbol used in @Protobuf serv "<sym>"@: 'pascalName' with a
-- lower-cased leading character (@GetFeature@ / @get_feature@ -> @getFeature@).
methodSymbol :: Text -> Text
methodSymbol = lowerFirst . pascalName


-- ---------------------------------------------------------------------------
-- Proto name resolution
-- ---------------------------------------------------------------------------

-- | @qualify "pkg" "Name" = "pkg.Name"@; the package may be empty.
qualify :: Text -> Text -> Text
qualify pkg name
  | T.null pkg = name
  | otherwise = pkg <> T.singleton '.' <> name


{- | Fully-qualified proto message name for the @rpcMessageType@ slot.
A reference that is already package-qualified (contains a @.@, after any
leading dot) is used verbatim; an unqualified leaf is prefixed with the
enclosing file's package.
-}
fqMessageName :: Text -> Text -> Text
fqMessageName pkg t =
  let t' = T.dropWhile (== '.') t
  in if T.isInfixOf (T.singleton '.') t'
      then t'
      else qualify pkg t'


{- | Resolve a proto type reference to the Haskell type name
@loadProto@ would have generated for it. Nested types are scope-prefixed
with @'@ to match @wireform-proto@'s naming; anything not found among
this file's declarations falls back to the bare leaf name (correct for
top-level types defined in imported files).
-}
resolveHsType :: ProtoFile -> Text -> Text
resolveHsType pf t =
  let lf = leafName t
  in case findTypeScope (protoTopLevels pf) lf of
      Just parents -> scopedHsName parents lf
      Nothing -> hsTypeName lf


scopedHsName :: [Text] -> Text -> Text
scopedHsName parents nm = case parents of
  [] -> hsTypeName nm
  _ -> T.intercalate (T.singleton '\'') (fmap hsTypeName (parents <> [nm]))


-- | Last @.@-separated segment, ignoring any leading dot.
leafName :: Text -> Text
leafName t = case T.splitOn (T.singleton '.') (T.dropWhile (== '.') t) of
  [] -> t
  parts -> last parts


{- | Find the parent-message chain of a (leaf) type name within a file's
declarations, searching top-level and nested messages\/enums. 'Nothing'
when the name is not declared in this file.
-}
findTypeScope :: [TopLevel] -> Text -> Maybe [Text]
findTypeScope tls needle = firstJust (fmap (topLevel []) tls)
  where
    topLevel parents (TLMessage m) = searchMessage parents m
    topLevel parents (TLEnum e)
      | enumName e == needle = Just parents
      | otherwise = Nothing
    topLevel _ _ = Nothing

    searchMessage parents m
      | msgName m == needle = Just parents
      | otherwise =
          let parents' = parents <> [msgName m]
          in firstJust (fmap (element parents') (msgElements m))

    element parents (MEMessage inner) = searchMessage parents inner
    element parents (MEEnum e)
      | enumName e == needle = Just parents
      | otherwise = Nothing
    element _ _ = Nothing


firstJust :: [Maybe a] -> Maybe a
firstJust = foldr step Nothing
  where
    step (Just x) _ = Just x
    step Nothing acc = acc
