{-# LANGUAGE GADTs #-}
-- | Convert Haskell types with 'ProtoMessage', 'ProtoEnum', or 'ProtoService'
-- instances into protobuf AST definitions and @.proto@ text.
--
-- This enables generating @.proto@ files from Haskell type metadata,
-- useful for:
--
-- * Publishing proto schemas derived from Haskell-first type definitions
-- * Interop: exporting schemas so other languages can generate their own bindings
-- * Documentation and tooling
--
-- Usage:
--
-- @
-- {-\# LANGUAGE TypeApplications \#-}
-- import Data.Proxy (Proxy(..))
-- import Proto.ToProto
-- import Proto.AST (Syntax(..))
--
-- let protoText = toProtoFileText Proto3 (Just "my.package")
--       [ messageTopLevel (Proxy \@MyMessage)
--       , enumTopLevel (Proxy \@MyEnum)
--       ]
-- @
module Proto.ToProto
  ( -- * Converting individual types to AST
    messageToAST
  , enumToAST
  , serviceToAST

    -- * Wrapping as top-level definitions
  , messageTopLevel
  , enumTopLevel
  , serviceTopLevel

    -- * Building complete proto files
  , toProtoFile

    -- * Rendering directly to @.proto@ text
  , messageToProtoText
  , enumToProtoText
  , serviceToProtoText
  , toProtoFileText

    -- * Utilities
  , messagePackage
  ) where

import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T

import Proto.AST
import Proto.Print (printProtoFile, printMessage, printEnum, printService)
import Proto.Schema

-- | Extract the short (unqualified) name from a fully-qualified proto name.
--
-- >>> shortName "example.api.Person"
-- "Person"
-- >>> shortName "Person"
-- "Person"
shortName :: Text -> Text
shortName t = case T.breakOnEnd "." t of
  ("", name) -> name
  (_, name)  -> name

-- | Convert a Schema 'ScalarFieldType' to an AST 'ScalarType'.
schemaScalarToAST :: ScalarFieldType -> ScalarType
schemaScalarToAST = \case
  DoubleField   -> SDouble
  FloatField    -> SFloat
  Int32Field    -> SInt32
  Int64Field    -> SInt64
  UInt32Field   -> SUInt32
  UInt64Field   -> SUInt64
  SInt32Field   -> SSInt32
  SInt64Field   -> SSInt64
  Fixed32Field  -> SFixed32
  Fixed64Field  -> SFixed64
  SFixed32Field -> SSFixed32
  SFixed64Field -> SSFixed64
  BoolField     -> SBool
  StringField   -> SString
  BytesField    -> SBytes

-- | Convert a Schema 'FieldTypeDescriptor' to an AST 'FieldType'.
--
-- Returns 'Nothing' for 'MapType', which requires special handling
-- as a 'MapField' element rather than a regular field.
schemaTypeToFieldType :: FieldTypeDescriptor -> Maybe FieldType
schemaTypeToFieldType = \case
  ScalarType s  -> Just (FTScalar (schemaScalarToAST s))
  MessageType t -> Just (FTNamed t)
  EnumType t    -> Just (FTNamed t)
  MapType _ _   -> Nothing

-- | Convert a Schema 'FieldLabel'' to an AST 'Maybe FieldLabel'.
--
-- 'LabelOptional' maps to 'Nothing' (no explicit keyword), which is the
-- standard proto3 singular field representation. 'LabelRequired' and
-- 'LabelRepeated' are preserved as explicit labels.
schemaLabelToAST :: FieldLabel' -> Maybe FieldLabel
schemaLabelToAST = \case
  LabelOptional -> Nothing
  LabelRequired -> Just Required
  LabelRepeated -> Just Repeated

-- | Convert a 'SomeFieldDescriptor' to a 'MessageElement'.
--
-- Map fields ('MapType') become 'MEMapField'; all others become 'MEField'.
fieldDescToElement :: SomeFieldDescriptor a -> MessageElement
fieldDescToElement (SomeField fd) = case fdTypeDesc fd of
  MapType keyScalar valTypeDesc ->
    MEMapField MapField
      { mapKeyType   = schemaScalarToAST keyScalar
      , mapValueType = case schemaTypeToFieldType valTypeDesc of
          Just ft -> ft
          Nothing -> FTNamed "<nested-map>"
      , mapFieldName = fdName fd
      , mapFieldNum  = FieldNumber (fdNumber fd)
      , mapOptions   = []
      }
  _ ->
    MEField FieldDef
      { fieldLabel   = schemaLabelToAST (fdLabel fd)
      , fieldType    = case schemaTypeToFieldType (fdTypeDesc fd) of
          Just ft -> ft
          Nothing -> FTNamed "<unknown>"
      , fieldName    = fdName fd
      , fieldNumber  = FieldNumber (fdNumber fd)
      , fieldOptions = []
      }

-- | Convert a 'ProtoMessage' type to an AST 'MessageDef'.
--
-- Uses the short (unqualified) portion of 'protoMessageName' as the
-- message name. Fields are ordered by field number.
messageToAST :: ProtoMessage a => Proxy a -> MessageDef
messageToAST p = MessageDef
  { msgName     = shortName (protoMessageName p)
  , msgElements = fmap fieldDescToElement (Map.elems (protoFieldDescriptors p))
  }

-- | Convert a 'ProtoEnum' type to an AST 'EnumDef'.
enumToAST :: ProtoEnum a => Proxy a -> EnumDef
enumToAST p = EnumDef
  { enumName    = shortName (protoEnumName p)
  , enumValues  = fmap toEnumValue (protoEnumValues p)
  , enumOptions = []
  }
  where
    toEnumValue (name, num) = EnumValue
      { evName    = name
      , evNumber  = num
      , evOptions = []
      }

-- | Convert a 'ProtoService' type to an AST 'ServiceDef'.
serviceToAST :: ProtoService a => Proxy a -> ServiceDef
serviceToAST p = ServiceDef
  { svcName    = shortName (protoServiceName p)
  , svcRpcs    = fmap methodToRpc (protoServiceMethods p)
  , svcOptions = []
  }
  where
    methodToRpc md = RpcDef
      { rpcName      = mdName md
      , rpcInput     = mdInputType md
      , rpcInputStr  = if mdClientStreaming md then Streaming else NoStream
      , rpcOutput    = mdOutputType md
      , rpcOutputStr = if mdServerStreaming md then Streaming else NoStream
      , rpcOptions   = []
      }

-- | Wrap a 'ProtoMessage' as a 'TopLevel' definition.
messageTopLevel :: ProtoMessage a => Proxy a -> TopLevel
messageTopLevel = TLMessage . messageToAST

-- | Wrap a 'ProtoEnum' as a 'TopLevel' definition.
enumTopLevel :: ProtoEnum a => Proxy a -> TopLevel
enumTopLevel = TLEnum . enumToAST

-- | Wrap a 'ProtoService' as a 'TopLevel' definition.
serviceTopLevel :: ProtoService a => Proxy a -> TopLevel
serviceTopLevel = TLService . serviceToAST

-- | Build a complete 'ProtoFile' from syntax, package, and definitions.
--
-- For imports and file-level options, construct a 'ProtoFile' directly.
toProtoFile :: Syntax -> Maybe Text -> [TopLevel] -> ProtoFile
toProtoFile syntax pkg topLevels = ProtoFile
  { protoSyntax    = syntax
  , protoPackage   = pkg
  , protoImports   = []
  , protoOptions   = []
  , protoTopLevels = topLevels
  }

-- | Render a 'ProtoMessage' type directly to a @.proto@ message definition.
messageToProtoText :: ProtoMessage a => Proxy a -> Text
messageToProtoText = printMessage 0 . messageToAST

-- | Render a 'ProtoEnum' type directly to a @.proto@ enum definition.
enumToProtoText :: ProtoEnum a => Proxy a -> Text
enumToProtoText = printEnum 0 . enumToAST

-- | Render a 'ProtoService' type directly to a @.proto@ service definition.
serviceToProtoText :: ProtoService a => Proxy a -> Text
serviceToProtoText = printService . serviceToAST

-- | Build a 'ProtoFile' and render it to @.proto@ text in one step.
toProtoFileText :: Syntax -> Maybe Text -> [TopLevel] -> Text
toProtoFileText syntax pkg = printProtoFile . toProtoFile syntax pkg

-- | Extract the package name from a 'ProtoMessage' type, if non-empty.
messagePackage :: ProtoMessage a => Proxy a -> Maybe Text
messagePackage p = case protoPackageName p of
  "" -> Nothing
  pkg -> Just pkg
