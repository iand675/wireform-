{- | Code generation for gRPC service definitions.

Generates Haskell service interfaces from proto service definitions:

* A record type for the service with one field per RPC method
* A client record with functions for calling each method
* Type aliases for request/response types
* Method metadata (method names, streaming modes)

The generated code is transport-agnostic: it defines the interface
but does not depend on any specific HTTP/2 or gRPC library.
Users wire up the transport layer by providing implementations.
-}
module Proto.CodeGen.Service (
  genServiceDecls,
  genServiceDeclsQualified,
  genServiceDeclsNamed,
  genServiceModule,
  serviceValueNames,
  ServiceBindings,
  serviceBindings,
  emptyServicePragmas,
) where

import Data.Char (toLower, toUpper)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Proto.IDL.AST
import Proto.Internal.CodeGen.Combinators (braceBlock, chooseGeneratedName, txt)


data ServiceBindings = ServiceBindings
  { serverTypeName :: !Text
  , clientTypeName :: !Text
  , methodTypeName :: !Text
  , methodNameBinding :: !Text
  , serviceNameBinding :: !Text
  , methodPathsBinding :: !Text
  , rpcBindings :: !(Map Text RpcBindings)
  }


data RpcBindings = RpcBindings
  { clientSelector :: !Text
  , serverSelector :: !Text
  , methodConstructor :: !Text
  }


-- | Allocate fixed service bindings before RPC selectors and constructors.
-- The sets contain occupied type, data constructor, and value names.
serviceBindings :: [Text] -> Set Text -> Set Text -> Set Text -> [ServiceDef] -> Map Text ServiceBindings
serviceBindings scope types constructors values services =
  Map.fromList (snd (mapAccumL bindMethods (reservedConstructors, reservedValues) entries))
  where
    ((_, reservedConstructors, reservedValues), entries) =
      mapAccumL bindService (types, constructors, values) services

    bindService (usedTypes, usedConstructors, usedValues) svc =
      let base = svcTypeName scope svc
          prefix = T.toLower base
          occupied = Set.union usedTypes usedConstructors
          server = chooseGeneratedName occupied (base <> "Server") base
          client = chooseGeneratedName (Set.insert server occupied) (base <> "Client") base
          recordTypes = Set.insert client (Set.insert server usedTypes)
          recordConstructors = Set.insert client (Set.insert server usedConstructors)
          method = chooseGeneratedName recordTypes (base <> "Method") base
          methodName = chooseGeneratedName usedValues (prefix <> "MethodName") base
          serviceName = chooseGeneratedName usedValues (prefix <> "ServiceName") base
          paths = chooseGeneratedName usedValues (prefix <> "MethodPaths") base
          occupiedValues = Set.insert paths (Set.insert serviceName (Set.insert methodName usedValues))
          names = ServiceBindings server client method methodName serviceName paths Map.empty
      in ((Set.insert method recordTypes, recordConstructors, occupiedValues), (svc, names))

    bindMethods reserved (svc, names) =
      let (occupied, methods) = mapAccumL (bindMethod (svcTypeName scope svc)) reserved (svcRpcs svc)
      in (occupied, (svcName svc, names {rpcBindings = Map.fromList methods}))

    bindMethod service (usedConstructors, usedValues) rpc =
      let protoName = rpcName rpc
          suffix = service <> "'" <> protoName
          client = chooseGeneratedName usedValues (clientFieldName rpc) suffix
          withClient = Set.insert client usedValues
          server = chooseGeneratedName withClient (serverFieldName rpc) suffix
          con = chooseGeneratedName usedConstructors (methodConstructorBase protoName) suffix
      in ((Set.insert con usedConstructors, Set.insert server withClient), (protoName, RpcBindings client server con))


serviceValueNames :: [Text] -> ServiceDef -> [Text]
serviceValueNames scope svc =
  fmap (T.toLower (svcTypeName scope svc) <>) ["MethodName", "ServiceName", "MethodPaths"]
    <> concatMap (\rpc -> [clientFieldName rpc, serverFieldName rpc]) (svcRpcs svc)


clientFieldName :: RpcDef -> Text
clientFieldName = escapeReserved' . methodFieldBase


serverFieldName :: RpcDef -> Text
serverFieldName rpc = escapeReserved' (methodFieldBase rpc <> "Handler")


methodFieldBase :: RpcDef -> Text
methodFieldBase rpc =
  let name = lowerFirst' (snakeToCamel' (rpcName rpc))
  in case T.uncons name of
       Just (first, _) | first >= 'a' && first <= 'z' -> name
       _ -> "_" <> rpcName rpc <> "Method"


methodConstructorBase :: Text -> Text
methodConstructorBase name = case T.uncons (hsTypeName' name) of
  Just (first, rest) | first >= 'A' && first <= 'Z' -> T.cons first rest <> "Method"
  _ -> "Method" <> name


-- | Generate service declarations with a type qualifier function.
genServiceDeclsQualified :: Maybe Text -> [Text] -> (Text -> Text) -> ServiceDef -> [Doc ann]
genServiceDeclsQualified pkg scope qualify svc =
  genServiceDeclsNamed pkg scope qualify (serviceBindings scope Set.empty Set.empty Set.empty [svc] Map.! svcName svc) svc


-- | Render a service using names reserved across the whole generated module.
genServiceDeclsNamed :: Maybe Text -> [Text] -> (Text -> Text) -> ServiceBindings -> ServiceDef -> [Doc ann]
genServiceDeclsNamed pkg scope qualify names svc =
  [ mempty
  , genServiceDoc pkg svc
  , genServerTypeQ names qualify svc
  , mempty
  , genClientTypeQ names qualify svc
  , mempty
  , genMethodInfos names svc
  , mempty
  , genServiceMeta pkg scope names svc
  ]


-- | Generate all declarations for a service definition.
genServiceDecls :: Maybe Text -> [Text] -> ServiceDef -> [Doc ann]
genServiceDecls pkg scope = genServiceDeclsQualified pkg scope hsTypeName'


emptyServicePragmas :: [ServiceDef] -> [Doc ann]
emptyServicePragmas services
  | any (null . svcRpcs) services =
      [txt "{-# LANGUAGE EmptyCase #-}", txt "{-# LANGUAGE EmptyDataDecls #-}"]
  | otherwise = []


-- | Generate a complete module for a service.
genServiceModule :: Maybe Text -> Text -> ServiceDef -> Doc ann
genServiceModule pkg prefix svc =
  let modName = T.intercalate "." (prefix : maybe [] (pure . hsModuleName') pkg <> [svcTypeName [] svc])
  in vsep $
       [txt "{-# LANGUAGE OverloadedStrings #-}", txt "{-# LANGUAGE DerivingStrategies #-}"]
       <> emptyServicePragmas [svc]
       <> [ txt "-- | Generated gRPC service interface for " <> pretty (svcName svc)
       , txt "module " <> pretty modName <> txt " where"
       , mempty
       , txt "import Data.ByteString (ByteString)"
       , txt "import Data.Text (Text)"
       , txt "import Proto.Encode (MessageEncode, encodeMessage)"
       , txt "import Proto.Decode (MessageDecode, decodeMessage)"
       , mempty
       , vsep (genServiceDecls pkg [] svc)
       ]


genServiceDoc :: Maybe Text -> ServiceDef -> Doc ann
genServiceDoc pkg svc =
  let qualified = qualifiedServiceName pkg [] svc
      protoDoc = case svcDoc svc of
        Nothing -> []
        Just doc ->
          let ls = T.lines doc
          in case ls of
               [] -> []
               (first_ : rest) ->
                 [txt "-- | " <> pretty first_]
                   <> fmap (\l -> txt "-- " <> pretty l) rest
                   <> [txt "--"]
      header = case protoDoc of
        [] -> [txt "-- | gRPC service @" <> pretty qualified <> txt "@."]
        _ -> protoDoc <> [txt "-- gRPC service @" <> pretty qualified <> txt "@."]
  in vsep $
       header
         <> [ txt "--"
            , txt "-- Methods:"
            , vsep
                ( fmap
                    ( \r ->
                        txt "-- * "
                          <> pretty (rpcName r)
                          <> txt " ("
                          <> streamLabel (rpcInputStr r)
                          <> pretty (rpcInput r)
                          <> txt " -> "
                          <> streamLabel (rpcOutputStr r)
                          <> pretty (rpcOutput r)
                          <> txt ")"
                    )
                    (svcRpcs svc)
                )
            ]


streamLabel :: StreamQualifier -> Doc ann
streamLabel NoStream = mempty
streamLabel Streaming = txt "stream "


-- | Generate method metadata.
genMethodInfos :: ServiceBindings -> ServiceDef -> Doc ann
genMethodInfos names svc =
  vsep $
    [ txt "-- | Method metadata for @" <> pretty (svcName svc) <> txt "@."
    , txt "data " <> pretty tyN
    ]
      <> constructors
      <> [mempty, pretty binding <> txt " :: " <> pretty tyN <> txt " -> Text"]
      <> equations
  where
    tyN = methodTypeName names
    binding = methodNameBinding names
    methods = svcRpcs svc
    con rpc = pretty (methodConstructor (rpcBindings names Map.! rpcName rpc))
    constructors = case methods of
      [] -> []
      _ ->
        [ indent 2 (vsep (zipWith (\separator rpc -> separator <+> con rpc) (txt "=" : repeat (txt "|")) methods))
        , indent 2 (txt "deriving stock (Show, Eq, Ord, Enum, Bounded)")
        ]
    equations = case methods of
      [] -> [pretty binding <> txt " method = case method of {}"]
      _ -> map (\rpc -> pretty binding <+> con rpc <+> txt "= \"" <> pretty (rpcName rpc) <> txt "\"") methods


qualifiedServiceName :: Maybe Text -> [Text] -> ServiceDef -> Text
qualifiedServiceName pkg scope svc =
  T.intercalate "." (maybe scope (: scope) pkg <> [svcName svc])


-- | Generate the service name and its gRPC method paths.
genServiceMeta :: Maybe Text -> [Text] -> ServiceBindings -> ServiceDef -> Doc ann
genServiceMeta pkg scope names svc =
  let fullName = qualifiedServiceName pkg scope svc
  in vsep
       [ txt "-- | Fully-qualified service name: @" <> pretty fullName <> txt "@"
       , pretty (serviceNameBinding names) <> txt " :: Text"
       , pretty (serviceNameBinding names) <> txt " = \"" <> pretty fullName <> pretty ("\"" :: Text)
       , mempty
       , txt "-- | All method paths for @" <> pretty (svcName svc) <> txt "@."
       , pretty (methodPathsBinding names) <> txt " :: [Text]"
       , pretty (methodPathsBinding names) <> txt " = "
           <> pretty (T.pack (show (fmap (\r -> "/" <> fullName <> "/" <> rpcName r) (svcRpcs svc))))
       ]


genServerTypeQ :: ServiceBindings -> (Text -> Text) -> ServiceDef -> Doc ann
genServerTypeQ names qualify svc =
  let tyN = serverTypeName names
  in vsep
       [ txt "-- | Server handler record for @" <> pretty (svcName svc) <> txt "@."
       , txt "-- Each field is a handler function for one RPC method."
       , txt "-- Implement all fields to create a server."
       , txt "data " <> pretty tyN <> txt " m = " <> pretty tyN
       , indent 2 (braceBlock (fmap (genRpcField names serverSelector qualify) (svcRpcs svc)))
       ]


genRpcField :: ServiceBindings -> (RpcBindings -> Text) -> (Text -> Text) -> RpcDef -> Doc ann
genRpcField names selector qualify rpc =
  let fieldLine = pretty (selector (rpcBindings names Map.! rpcName rpc)) <+> txt "::" <+> genRpcTypeQ qualify rpc
  in case rpcDoc rpc of
       Nothing -> fieldLine
       Just doc ->
         case T.lines doc of
           [] -> fieldLine
           first : rest ->
             vsep $
               [fieldLine, indent 2 (txt "-- ^ " <> pretty first)]
                 <> fmap (\docLine -> indent 2 (txt "-- " <> pretty docLine)) rest


genRpcTypeQ :: (Text -> Text) -> RpcDef -> Doc ann
genRpcTypeQ qualify rpc = case (rpcInputStr rpc, rpcOutputStr rpc) of
  (NoStream, NoStream) ->
    pretty (qualify (rpcInput rpc)) <+> txt "-> m" <+> pretty (qualify (rpcOutput rpc))
  (Streaming, NoStream) ->
    txt "m "
      <> pretty (qualify (rpcInput rpc))
      <+> txt "-> m"
      <+> pretty (qualify (rpcOutput rpc))
  (NoStream, Streaming) ->
    pretty (qualify (rpcInput rpc))
      <+> txt "-> ("
      <> pretty (qualify (rpcOutput rpc))
      <+> txt "-> m ()) -> m ()"
  (Streaming, Streaming) ->
    txt "m "
      <> pretty (qualify (rpcInput rpc))
      <+> txt "-> ("
      <> pretty (qualify (rpcOutput rpc))
      <+> txt "-> m ()) -> m ()"


genClientTypeQ :: ServiceBindings -> (Text -> Text) -> ServiceDef -> Doc ann
genClientTypeQ names qualify svc =
  let tyN = clientTypeName names
  in vsep
       [ txt "-- | Client stub record for @" <> pretty (svcName svc) <> txt "@."
       , txt "-- Each field is a function for calling one RPC method."
       , txt "data " <> pretty tyN <> txt " m = " <> pretty tyN
       , indent 2 (braceBlock (fmap (genRpcField names clientSelector qualify) (svcRpcs svc)))
       ]


svcTypeName :: [Text] -> ServiceDef -> Text
svcTypeName scope svc =
  let name = T.intercalate "'" (fmap hsTypeName' (scope <> [svcName svc]))
  in case T.uncons name of
       Just (first, _) | first >= 'A' && first <= 'Z' -> name
       _ -> "Service" <> name


hsTypeName' :: Text -> Text
hsTypeName' t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t


hsModuleName' :: Text -> Text
hsModuleName' = T.intercalate (T.singleton '.') . fmap capitalize . T.splitOn (T.singleton '.')
  where
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing -> t


snakeToCamel' :: Text -> Text
snakeToCamel' t =
  let parts = T.splitOn (T.singleton '_') t
  in case parts of
       [] -> t
       (p : ps) -> T.concat (lowerFirst' p : fmap titleCase ps)


lowerFirst' :: Text -> Text
lowerFirst' s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) (T.toLower rest)
  Nothing -> s


titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s


escapeReserved' :: Text -> Text
escapeReserved' t
  | t `elem` reserved = t <> "'"
  | otherwise = t
  where
    reserved :: [Text]
    reserved =
      [ "type"
      , "class"
      , "data"
      , "default"
      , "deriving"
      , "do"
      , "else"
      , "if"
      , "import"
      , "in"
      , "infix"
      , "infixl"
      , "infixr"
      , "instance"
      , "let"
      , "module"
      , "newtype"
      , "of"
      , "then"
      , "where"
      , "case"
      , "foreign"
      , "forall"
      , "mdo"
      , "qualified"
      , "hiding"
      ]
