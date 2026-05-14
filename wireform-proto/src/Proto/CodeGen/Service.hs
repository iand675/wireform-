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
  genServiceModule,
) where

import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Proto.IDL.AST
import Proto.Internal.CodeGen.Combinators (txt)


-- | Generate service declarations with a type qualifier function.
genServiceDeclsQualified :: Maybe Text -> [Text] -> (Text -> Text) -> ServiceDef -> [Doc ann]
genServiceDeclsQualified pkg scope qualify svc =
  [ mempty
  , genServiceDoc pkg svc
  , genServerTypeQ scope qualify svc
  , mempty
  , genClientTypeQ scope qualify svc
  , mempty
  , genMethodInfos scope svc
  , mempty
  , genServiceMeta scope svc
  ]


-- | Generate all declarations for a service definition.
genServiceDecls :: Maybe Text -> [Text] -> ServiceDef -> [Doc ann]
genServiceDecls pkg scope svc =
  [ mempty
  , genServiceDoc pkg svc
  , genServerType scope svc
  , mempty
  , genClientType scope svc
  , mempty
  , genMethodInfos scope svc
  , mempty
  , genServiceMeta scope svc
  ]


-- | Generate a complete module for a service.
genServiceModule :: Maybe Text -> Text -> ServiceDef -> Doc ann
genServiceModule pkg prefix svc =
  let modName = prefix <> "." <> hsModuleName' (fromMaybe "" pkg) <> "." <> hsTypeName' (svcName svc)
  in vsep
      [ txt "{-# LANGUAGE OverloadedStrings #-}"
      , txt "-- | Generated gRPC service interface for " <> pretty (svcName svc)
      , txt "module " <> pretty modName <> txt " where"
      , mempty
      , txt "import Data.ByteString (ByteString)"
      , txt "import Data.Text (Text)"
      , txt "import Proto (MessageEncode, MessageDecode, encodeMessage, decodeMessage)"
      , mempty
      , vsep (genServiceDecls pkg [] svc)
      ]


genServiceDoc :: Maybe Text -> ServiceDef -> Doc ann
genServiceDoc pkg svc =
  let qualified = maybe (svcName svc) (\p -> p <> "." <> svcName svc) pkg
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


-- | Generate the server handler record type.
genServerType :: [Text] -> ServiceDef -> Doc ann
genServerType scope svc =
  let tyN = svcTypeName scope svc <> "Server"
  in vsep
      [ txt "-- | Server handler record for @" <> pretty (svcName svc) <> txt "@."
      , txt "-- Each field is a handler function for one RPC method."
      , txt "-- Implement all fields to create a server."
      , txt "data " <> pretty tyN <> txt " m = " <> pretty tyN
      , indent 2 (braceFields (fmap (genServerField scope) (svcRpcs svc)))
      ]


genServerField :: [Text] -> RpcDef -> Doc ann
genServerField scope rpc =
  let fname = lowerFirst' (snakeToCamel' (rpcName rpc)) <> "Handler"
      fieldLine = pretty (escapeReserved' fname) <+> txt "::" <+> genRpcServerType scope rpc
  in case rpcDoc rpc of
      Nothing -> fieldLine
      Just doc ->
        let ls = T.lines doc
        in case ls of
            [] -> fieldLine
            (first_ : rest) ->
              vsep $
                [fieldLine]
                  <> [indent 2 (txt "-- ^ " <> pretty first_)]
                  <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


genRpcServerType :: [Text] -> RpcDef -> Doc ann
genRpcServerType _ rpc = case (rpcInputStr rpc, rpcOutputStr rpc) of
  (NoStream, NoStream) ->
    pretty (hsTypeName' (rpcInput rpc)) <+> txt "-> m" <+> pretty (hsTypeName' (rpcOutput rpc))
  (Streaming, NoStream) ->
    txt "m "
      <> pretty (hsTypeName' (rpcInput rpc))
      <+> txt "-> m"
      <+> pretty (hsTypeName' (rpcOutput rpc))
  (NoStream, Streaming) ->
    pretty (hsTypeName' (rpcInput rpc))
      <+> txt "-> ("
      <> pretty (hsTypeName' (rpcOutput rpc))
      <+> txt "-> m ()) -> m ()"
  (Streaming, Streaming) ->
    txt "m "
      <> pretty (hsTypeName' (rpcInput rpc))
      <+> txt "-> ("
      <> pretty (hsTypeName' (rpcOutput rpc))
      <+> txt "-> m ()) -> m ()"


-- | Generate the client record type.
genClientType :: [Text] -> ServiceDef -> Doc ann
genClientType scope svc =
  let tyN = svcTypeName scope svc <> "Client"
  in vsep
      [ txt "-- | Client stub record for @" <> pretty (svcName svc) <> txt "@."
      , txt "-- Each field is a function for calling one RPC method."
      , txt "data " <> pretty tyN <> txt " m = " <> pretty tyN
      , indent 2 (braceFields (fmap (genClientField scope) (svcRpcs svc)))
      ]


genClientField :: [Text] -> RpcDef -> Doc ann
genClientField scope rpc =
  let fname = lowerFirst' (snakeToCamel' (rpcName rpc))
      fieldLine = pretty (escapeReserved' fname) <+> txt "::" <+> genRpcClientType scope rpc
  in case rpcDoc rpc of
      Nothing -> fieldLine
      Just doc ->
        let ls = T.lines doc
        in case ls of
            [] -> fieldLine
            (first_ : rest) ->
              vsep $
                [fieldLine]
                  <> [indent 2 (txt "-- ^ " <> pretty first_)]
                  <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


genRpcClientType :: [Text] -> RpcDef -> Doc ann
genRpcClientType = genRpcServerType


-- | Generate method metadata.
genMethodInfos :: [Text] -> ServiceDef -> Doc ann
genMethodInfos scope svc =
  let tyN = svcTypeName scope svc
  in vsep
      [ txt "-- | Method metadata for @" <> pretty (svcName svc) <> txt "@."
      , txt "data " <> pretty tyN <> txt "Method"
      , indent
          2
          ( vsep
              ( zipWith
                  ( \pfx r ->
                      pfx
                        <+> pretty (hsTypeName' (rpcName r) <> "Method")
                  )
                  seps
                  (svcRpcs svc)
              )
          )
      , indent 2 (txt "deriving stock (Show, Eq, Ord, Enum, Bounded)")
      , mempty
      , pretty (T.toLower tyN <> "MethodName :: " :: Text) <> pretty tyN <> txt "Method -> Text"
      , vsep
          ( fmap
              ( \r ->
                  pretty (T.toLower tyN <> "MethodName " :: Text)
                    <> pretty (hsTypeName' (rpcName r) <> "Method" :: Text)
                    <+> pretty ("= \"" :: Text)
                    <> pretty (rpcName r)
                    <> pretty ("\"" :: Text)
              )
              (svcRpcs svc)
          )
      ]
  where
    seps = txt "=" : repeat (txt "|")


-- | Generate ProtoService metadata instance.
genServiceMeta :: [Text] -> ServiceDef -> Doc ann
genServiceMeta scope svc =
  let tyN = svcTypeName scope svc
      fullName = T.intercalate "." (scope <> [svcName svc])
  in vsep
      [ txt "-- | Fully-qualified service name: @" <> pretty fullName <> txt "@"
      , pretty (T.toLower tyN <> "ServiceName :: Text" :: Text)
      , pretty (T.toLower tyN <> "ServiceName = \"" :: Text) <> pretty fullName <> pretty ("\"" :: Text)
      , mempty
      , txt "-- | All method paths for @" <> pretty (svcName svc) <> txt "@."
      , pretty (T.toLower tyN <> "MethodPaths :: [Text]" :: Text)
      , pretty (T.toLower tyN <> "MethodPaths = " :: Text)
          <> pretty (T.pack (show (fmap (\r -> "/" <> fullName <> "/" <> rpcName r) (svcRpcs svc))))
      ]


genServerTypeQ :: [Text] -> (Text -> Text) -> ServiceDef -> Doc ann
genServerTypeQ scope qualify svc =
  let tyN = svcTypeName scope svc <> "Server"
  in vsep
      [ txt "-- | Server handler record for @" <> pretty (svcName svc) <> txt "@."
      , txt "-- Each field is a handler function for one RPC method."
      , txt "-- Implement all fields to create a server."
      , txt "data " <> pretty tyN <> txt " m = " <> pretty tyN
      , indent 2 (braceFields (fmap (genServerFieldQ scope qualify) (svcRpcs svc)))
      ]


genServerFieldQ :: [Text] -> (Text -> Text) -> RpcDef -> Doc ann
genServerFieldQ _scope qualify rpc =
  let fname = lowerFirst' (snakeToCamel' (rpcName rpc)) <> "Handler"
      fieldLine = pretty (escapeReserved' fname) <+> txt "::" <+> genRpcTypeQ qualify rpc
  in case rpcDoc rpc of
      Nothing -> fieldLine
      Just doc ->
        let ls = T.lines doc
        in case ls of
            [] -> fieldLine
            (first_ : rest) ->
              vsep $
                [fieldLine]
                  <> [indent 2 (txt "-- ^ " <> pretty first_)]
                  <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


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


genClientTypeQ :: [Text] -> (Text -> Text) -> ServiceDef -> Doc ann
genClientTypeQ scope qualify svc =
  let tyN = svcTypeName scope svc <> "Client"
  in vsep
      [ txt "-- | Client stub record for @" <> pretty (svcName svc) <> txt "@."
      , txt "-- Each field is a function for calling one RPC method."
      , txt "data " <> pretty tyN <> txt " m = " <> pretty tyN
      , indent 2 (braceFields (fmap (genClientFieldQ scope qualify) (svcRpcs svc)))
      ]


genClientFieldQ :: [Text] -> (Text -> Text) -> RpcDef -> Doc ann
genClientFieldQ _scope qualify rpc =
  let fname = lowerFirst' (snakeToCamel' (rpcName rpc))
      fieldLine = pretty (escapeReserved' fname) <+> txt "::" <+> genRpcTypeQ qualify rpc
  in case rpcDoc rpc of
      Nothing -> fieldLine
      Just doc ->
        let ls = T.lines doc
        in case ls of
            [] -> fieldLine
            (first_ : rest) ->
              vsep $
                [fieldLine]
                  <> [indent 2 (txt "-- ^ " <> pretty first_)]
                  <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


svcTypeName :: [Text] -> ServiceDef -> Text
svcTypeName scope svc = T.intercalate "'" (fmap hsTypeName' (scope <> [svcName svc]))


braceFields :: [Doc ann] -> Doc ann
braceFields [] = txt "{ }"
braceFields (f : fs) =
  vsep (txt "{ " <> f : fmap (\x -> txt ", " <> x) fs)
    <> line
    <> txt "}"


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
      ]
