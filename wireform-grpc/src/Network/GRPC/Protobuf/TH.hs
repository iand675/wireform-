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

import Data.Char (isUpper, toUpper)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Traversable (mapAccumM)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)

import Proto.CodeGen (hsTypeName, lowerFirst, scopedTypeName)
import Proto.IDL.AST
import Proto.IDL.Descriptor (descriptorTypeDefinitions, resolveDescriptorName)
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import Proto.Internal.CodeGen.Combinators (chooseGeneratedName)

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
      (reserved, named) <- mapAccumM nameService Set.empty services
      (_, declarations) <- mapAccumM (serviceToDecls opts (rpcTypeResolver pf pkg) pkg) reserved named
      pure (concat declarations)
  where
    nameService used svc = do
      let protoName = svcName svc
      (occupied, name) <- allocateTypeName used (pascalName protoName) protoName
      pure (occupied, (svc, name))


collectServices :: [TopLevel] -> [ServiceDef]
collectServices = foldr step []
  where
    step (TLService s) acc = s : acc
    step _ acc = acc


-- ---------------------------------------------------------------------------
-- Per-service generation
-- ---------------------------------------------------------------------------

serviceToDecls
  :: ServiceGenOpts -> (Text -> (Text, Text)) -> Text
  -> Set Text -> (ServiceDef, Name) -> Q (Set Text, [Dec])
serviceToDecls opts resolveType pkg used (svc, servHsName) = do
  let fqServiceName = qualify pkg (svcName svc)
      (_, methods) = mapAccumL nameMethod Set.empty (svcRpcs svc)
  servData <- dataD (pure []) servHsName [] Nothing [] []
  (occupied, rpcDecls) <- mapAccumM (rpcToDecls resolveType servHsName fqServiceName) used methods
  metaDecls <-
    if sgoGenerateMetadata opts
      then serviceMetadataDecls servHsName
      else pure []
  methodsDecl <- serviceMethodsDecl servHsName (map snd methods)
  pure (occupied, servData : concat rpcDecls <> metaDecls <> [methodsDecl])
  where
    nameMethod symbols rpc =
      let protoName = rpcName rpc
          symbol = chooseGeneratedName symbols (methodSymbol protoName) protoName
      in (Set.insert symbol symbols, (rpc, symbol))


-- | All declarations for a single RPC method.
rpcToDecls
  :: (Text -> (Text, Text)) -> Name -> Text
  -> Set Text -> (RpcDef, Text) -> Q (Set Text, [Dec])
rpcToDecls resolveType servHsName fqServiceName used (rpc, methSym) = do
  let protoName = rpcName rpc
      suffix = T.pack (nameBase servHsName) <> "'" <> protoName
  (occupied, methHsName) <- allocateTypeName used (pascalName protoName) suffix
  let (inHs, inFq) = resolveType (rpcInput rpc)
      (outHs, _) = resolveType (rpcOutput rpc)
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

  pure (occupied,
    [ synDec
    , inputInst
    , outputInst
    , isRpcInst
    , clientInst
    , serverInst
    , supportsInst
    , hasStreamingInst
    ])


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
serviceMethodsDecl :: Name -> [Text] -> Q Dec
serviceMethodsDecl servHsName symbols =
  tySynInstD
    ( tySynEqn
        Nothing
        (appT (conT ''ServiceMethods) (conT servHsName))
        (promotedSymbolList symbols)
    )


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
pascalName t =
  let name = T.concat (fmap upperFirst (T.splitOn (T.singleton '_') t))
  in case T.uncons name of
       Just (first, _) | isUpper first -> name
       _ -> "Rpc" <> t
  where
    upperFirst x = case T.uncons x of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing -> x


-- | The method symbol used in @Protobuf serv "<sym>"@: 'pascalName' with a
-- lower-cased leading character (@GetFeature@ / @get_feature@ -> @getFeature@).
methodSymbol :: Text -> Text
methodSymbol = lowerFirst . pascalName


-- | Reserve local names and respect types already in scope at the splice.
allocateTypeName :: Set Text -> Text -> Text -> Q (Set Text, Name)
allocateTypeName used base suffix = do
  let candidate = chooseGeneratedName used base suffix
      occupied = Set.insert candidate used
  existing <- lookupTypeName (T.unpack candidate)
  case existing of
    Just _ -> allocateTypeName occupied base suffix
    Nothing -> pure (occupied, mkName (T.unpack candidate))


-- ---------------------------------------------------------------------------
-- Proto name resolution
-- ---------------------------------------------------------------------------

-- | @qualify "pkg" "Name" = "pkg.Name"@; the package may be empty.
qualify :: Text -> Text -> Text
qualify pkg name
  | T.null pkg = name
  | otherwise = pkg <> T.singleton '.' <> name


-- | Resolve the Haskell type and the fully qualified protobuf message name.
rpcTypeResolver :: ProtoFile -> Text -> Text -> (Text, Text)
rpcTypeResolver pf pkg = resolve
  where
    definitions = descriptorTypeDefinitions pf
    resolve name = case resolveDescriptorName definitions pkg name of
      Just (qualified, 11) ->
        let local = fromMaybe qualified (T.stripPrefix (pkg <> ".") qualified)
        in (scopedTypeName (T.splitOn "." local), qualified)
      _ ->
        let absolute = T.dropWhile (== '.') name
            leaf = T.takeWhileEnd (/= '.') absolute
            qualified
              | T.isPrefixOf "." name || T.isInfixOf "." absolute = absolute
              | otherwise = qualify pkg absolute
        in (hsTypeName leaf, qualified)
