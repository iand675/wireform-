{- | Convert wireform's AST types to descriptor.proto types and back.

This enables bundling schema metadata with generated types:
the parsed 'ProtoFile' is converted to a 'R.FileDescriptorProto',
serialized, and embedded in the generated code.

The reverse direction ('fileDescriptorToAST') is used by the protoc
plugin, which receives 'R.FileDescriptorProto' from protoc in
'CodeGeneratorRequest.proto_file' and needs wireform's 'ProtoFile' to
drive code generation.

All descriptor shapes here are the generated reflection types from
'Proto.Google.Protobuf.Reflection.Descriptor' (full @descriptor.proto@),
so the plugin path matches what @protoc@ emits without a decode/encode
narrowing step.
-}
module Proto.IDL.Descriptor (
  astToFileDescriptor,
  astToDescriptor,
  astToFieldDescriptor,
  astToEnumDescriptor,
  astToServiceDescriptor,
  serializeFileDescriptor,
  fileDescriptorToAST,
  descriptorToMessage,
  fieldDescriptorToField,
  enumDescriptorToEnum,
  serviceDescriptorToService,
) where

import Data.ByteString (ByteString)
import Data.List (find, partition)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Proto (encodeMessage)
import Proto.Google.Protobuf.Reflection.Descriptor as R
import Proto.IDL.AST


-- | Convert a parsed ProtoFile to a FileDescriptorProto.
astToFileDescriptor :: FilePath -> ProtoFile -> R.FileDescriptorProto
astToFileDescriptor path pf =
  R.defaultFileDescriptorProto
    { R.fileDescriptorProtoName = Just (T.pack path)
    , R.fileDescriptorProtoPackage = protoPackage pf
    , R.fileDescriptorProtoDependency = V.fromList (fmap importPath (protoImports pf))
    , R.fileDescriptorProtoMessageType = V.fromList (concatMap topMessages (protoTopLevels pf))
    , R.fileDescriptorProtoEnumType = V.fromList (concatMap topEnums (protoTopLevels pf))
    , R.fileDescriptorProtoService = V.fromList (concatMap topServices (protoTopLevels pf))
    , R.fileDescriptorProtoSyntax = Just (syntaxStr (protoSyntax pf))
    , R.fileDescriptorProtoEdition = astEditionToReflect (protoSyntax pf)
    }
  where
    topMessages (TLMessage msg) = [astToDescriptor msg]
    topMessages _ = []
    topEnums (TLEnum ed) = [astToEnumDescriptor ed]
    topEnums _ = []
    topServices (TLService svc) = [astToServiceDescriptor svc]
    topServices _ = []
    syntaxStr Proto2 = "proto2"
    syntaxStr Proto3 = "proto3"
    syntaxStr (Editions _) = "editions"


-- | Convert a MessageDef to a DescriptorProto.
astToDescriptor :: MessageDef -> R.DescriptorProto
astToDescriptor msg =
  R.defaultDescriptorProto
    { R.descriptorProtoName = Just (msgName msg)
    , R.descriptorProtoField = V.fromList (concatMap extractField (msgElements msg))
    , R.descriptorProtoNestedType = V.fromList (concatMap extractNested (msgElements msg))
    , R.descriptorProtoEnumType = V.fromList (concatMap extractEnum (msgElements msg))
    , R.descriptorProtoOneofDecl = V.fromList (concatMap extractOneof (msgElements msg))
    }
  where
    extractField (MEField fd) = [astToFieldDescriptor fd]
    extractField (MEMapField mf) = [mapToFieldDescriptor mf]
    extractField (MEOneof od) = fmap oneofFieldToFDP (oneofFields od)
    extractField _ = []

    extractNested (MEMessage inner) = [astToDescriptor inner]
    -- Emit the synthetic map-entry DescriptorProto so the round-trip
    -- through FileDescriptorProto preserves the map field type info.
    extractNested (MEMapField mf) = [mapEntryDescriptor mf]
    extractNested _ = []

    extractEnum (MEEnum ed) = [astToEnumDescriptor ed]
    extractEnum _ = []

    extractOneof (MEOneof od) =
      [ R.defaultOneofDescriptorProto{R.oneofDescriptorProtoName = Just (oneofName od)}
      ]
    extractOneof _ = []


-- | Convert a FieldDef to a FieldDescriptorProto.
astToFieldDescriptor :: FieldDef -> R.FieldDescriptorProto
astToFieldDescriptor fd =
  let (ty, mTn) = fieldTypeToReflect (fieldType fd)
   in R.defaultFieldDescriptorProto
        { R.fieldDescriptorProtoName = Just (fieldName fd)
        , R.fieldDescriptorProtoNumber = Just (fromIntegral (unFieldNumber (fieldNumber fd)))
        , R.fieldDescriptorProtoLabel = labelToReflect (fieldLabel fd)
        , R.fieldDescriptorProtoType = Just ty
        , R.fieldDescriptorProtoTypeName = mTn
        }


mapToFieldDescriptor :: MapField -> R.FieldDescriptorProto
mapToFieldDescriptor mf =
  R.defaultFieldDescriptorProto
    { R.fieldDescriptorProtoName = Just (mapFieldName mf)
    , R.fieldDescriptorProtoNumber = Just (fromIntegral (unFieldNumber (mapFieldNum mf)))
    , R.fieldDescriptorProtoLabel = Just R.FieldDescriptorProto'Label'LabelRepeated
    , R.fieldDescriptorProtoType = Just R.FieldDescriptorProto'Type'TypeMessage
    , R.fieldDescriptorProtoTypeName = Just (mapEntryName (mapFieldName mf))
    }


-- | Build the synthetic map-entry DescriptorProto that encodes a map
-- field's key/value types. The entry name matches 'mapToFieldDescriptor'
-- so the round-trip through 'FileDescriptorProto' is lossless.
mapEntryDescriptor :: MapField -> R.DescriptorProto
mapEntryDescriptor mf =
  R.defaultDescriptorProto
    { R.descriptorProtoName = Just (mapEntryName (mapFieldName mf))
    , R.descriptorProtoField = V.fromList [keyField, valField]
    , R.descriptorProtoOptions =
        Just R.defaultMessageOptions{R.messageOptionsMapEntry = Just True}
    }
  where
    keyField =
      R.defaultFieldDescriptorProto
        { R.fieldDescriptorProtoName = Just "key"
        , R.fieldDescriptorProtoNumber = Just 1
        , R.fieldDescriptorProtoLabel = Just R.FieldDescriptorProto'Label'LabelOptional
        , R.fieldDescriptorProtoType = Just (scalarToFieldType (mapKeyType mf))
        }
    valField = case mapValueType mf of
      FTScalar st ->
        R.defaultFieldDescriptorProto
          { R.fieldDescriptorProtoName = Just "value"
          , R.fieldDescriptorProtoNumber = Just 2
          , R.fieldDescriptorProtoLabel = Just R.FieldDescriptorProto'Label'LabelOptional
          , R.fieldDescriptorProtoType = Just (scalarToFieldType st)
          }
      FTNamed n ->
        R.defaultFieldDescriptorProto
          { R.fieldDescriptorProtoName = Just "value"
          , R.fieldDescriptorProtoNumber = Just 2
          , R.fieldDescriptorProtoLabel = Just R.FieldDescriptorProto'Label'LabelOptional
          , R.fieldDescriptorProtoType = Just R.FieldDescriptorProto'Type'TypeMessage
          , R.fieldDescriptorProtoTypeName = Just n
          }


mapEntryName :: Text -> Text
mapEntryName fieldName = fieldName <> "Entry"


oneofFieldToFDP :: OneofField -> R.FieldDescriptorProto
oneofFieldToFDP of' =
  let (ty, mTn) = fieldTypeToReflect (oneofFieldType of')
   in R.defaultFieldDescriptorProto
        { R.fieldDescriptorProtoName = Just (oneofFieldName of')
        , R.fieldDescriptorProtoNumber = Just (fromIntegral (unFieldNumber (oneofFieldNumber of')))
        , R.fieldDescriptorProtoLabel = Just R.FieldDescriptorProto'Label'LabelOptional
        , R.fieldDescriptorProtoType = Just ty
        , R.fieldDescriptorProtoTypeName = mTn
        }


-- | Convert an EnumDef to an EnumDescriptorProto.
astToEnumDescriptor :: EnumDef -> R.EnumDescriptorProto
astToEnumDescriptor ed =
  R.defaultEnumDescriptorProto
    { R.enumDescriptorProtoName = Just (enumName ed)
    , R.enumDescriptorProtoValue = V.fromList (fmap toEVDP (enumValues ed))
    }
  where
    toEVDP ev =
      R.defaultEnumValueDescriptorProto
        { R.enumValueDescriptorProtoName = Just (evName ev)
        , R.enumValueDescriptorProtoNumber = Just (fromIntegral (evNumber ev))
        }


-- | Convert a ServiceDef to a ServiceDescriptorProto.
astToServiceDescriptor :: ServiceDef -> R.ServiceDescriptorProto
astToServiceDescriptor svc =
  R.defaultServiceDescriptorProto
    { R.serviceDescriptorProtoName = Just (svcName svc)
    , R.serviceDescriptorProtoMethod = V.fromList (fmap toMDP (svcRpcs svc))
    }
  where
    toMDP rpc =
      R.defaultMethodDescriptorProto
        { R.methodDescriptorProtoName = Just (rpcName rpc)
        , R.methodDescriptorProtoInputType = Just (rpcInput rpc)
        , R.methodDescriptorProtoOutputType = Just (rpcOutput rpc)
        , R.methodDescriptorProtoClientStreaming = Just (rpcInputStr rpc == Streaming)
        , R.methodDescriptorProtoServerStreaming = Just (rpcOutputStr rpc == Streaming)
        }


-- | Serialize a ProtoFile's schema to bytes (as a FileDescriptorProto).
serializeFileDescriptor :: FilePath -> ProtoFile -> ByteString
serializeFileDescriptor path pf = encodeMessage (astToFileDescriptor path pf)


labelToReflect :: Maybe FieldLabel -> Maybe R.FieldDescriptorProto'Label
labelToReflect Nothing = Just R.FieldDescriptorProto'Label'LabelOptional
labelToReflect (Just Optional) = Just R.FieldDescriptorProto'Label'LabelOptional
labelToReflect (Just Required) = Just R.FieldDescriptorProto'Label'LabelRequired
labelToReflect (Just Repeated) = Just R.FieldDescriptorProto'Label'LabelRepeated


fieldTypeToReflect :: FieldType -> (R.FieldDescriptorProto'Type, Maybe Text)
fieldTypeToReflect (FTScalar s) = (scalarToFieldType s, Nothing)
fieldTypeToReflect (FTNamed n) = (R.FieldDescriptorProto'Type'TypeMessage, Just n)


scalarToFieldType :: ScalarType -> R.FieldDescriptorProto'Type
scalarToFieldType = \case
  SDouble -> R.FieldDescriptorProto'Type'TypeDouble
  SFloat -> R.FieldDescriptorProto'Type'TypeFloat
  SInt64 -> R.FieldDescriptorProto'Type'TypeInt64
  SUInt64 -> R.FieldDescriptorProto'Type'TypeUint64
  SInt32 -> R.FieldDescriptorProto'Type'TypeInt32
  SFixed64 -> R.FieldDescriptorProto'Type'TypeFixed64
  SFixed32 -> R.FieldDescriptorProto'Type'TypeFixed32
  SBool -> R.FieldDescriptorProto'Type'TypeBool
  SString -> R.FieldDescriptorProto'Type'TypeString
  SBytes -> R.FieldDescriptorProto'Type'TypeBytes
  SUInt32 -> R.FieldDescriptorProto'Type'TypeUint32
  SSFixed32 -> R.FieldDescriptorProto'Type'TypeSfixed32
  SSFixed64 -> R.FieldDescriptorProto'Type'TypeSfixed64
  SSInt32 -> R.FieldDescriptorProto'Type'TypeSint32
  SSInt64 -> R.FieldDescriptorProto'Type'TypeSint64


astEditionToReflect :: Syntax -> Maybe R.Edition
astEditionToReflect (Editions (Edition t))
  | Right (n, rest) <- TR.decimal t, T.null rest =
      case fromProtoEnumEdition (fromIntegral (n :: Integer)) of
        Just ed -> Just ed
        Nothing
          | t == "2023" -> Just R.Edition'Edition2023
          | t == "2024" -> Just R.Edition'Edition2024
          | otherwise -> Nothing
  | t == "2023" = Just R.Edition'Edition2023
  | t == "2024" = Just R.Edition'Edition2024
  | otherwise = Nothing
astEditionToReflect _ = Nothing


-- ---------------------------------------------------------------------------
-- Reverse conversion: FileDescriptorProto -> ProtoFile
-- ---------------------------------------------------------------------------

-- | Convert a 'FileDescriptorProto' (as received from protoc) to a 'ProtoFile'.
fileDescriptorToAST :: R.FileDescriptorProto -> ProtoFile
fileDescriptorToAST fdp =
  ProtoFile
    { protoSyntax = parseSyntax (R.fileDescriptorProtoSyntax fdp) (R.fileDescriptorProtoEdition fdp)
    , protoPackage =
        case R.fileDescriptorProtoPackage fdp of
          Nothing -> Nothing
          Just p | T.null p -> Nothing
          Just p -> Just p
    , protoImports = fmap (ImportDef () Nothing) (V.toList (R.fileDescriptorProtoDependency fdp))
    , protoOptions = []
    , protoTopLevels =
        fmap (TLMessage . descriptorToMessage) (V.toList (R.fileDescriptorProtoMessageType fdp))
          <> fmap (TLEnum . enumDescriptorToEnum) (V.toList (R.fileDescriptorProtoEnumType fdp))
          <> fmap (TLService . serviceDescriptorToService) (V.toList (R.fileDescriptorProtoService fdp))
    , protoSource = Nothing
    }


parseSyntax :: Maybe Text -> Maybe R.Edition -> Syntax
parseSyntax mSyn mEd
  | Just "proto2" <- mSyn = Proto2
  | Just "editions" <- mSyn, Just ed <- mEd = Editions (Edition (reflectEditionToAstText ed))
  | Just "editions" <- mSyn = Editions (Edition "")
  | otherwise = Proto3


reflectEditionToAstText :: R.Edition -> Text
reflectEditionToAstText = \case
  R.Edition'Edition2023 -> "2023"
  R.Edition'Edition2024 -> "2024"
  R.Edition'EditionProto2 -> "proto2"
  R.Edition'EditionProto3 -> "proto3"
  e -> T.pack (show (toProtoEnumEdition e))


-- | Convert a 'DescriptorProto' to a 'MessageDef'.
--
-- Nested descriptors with @options.map_entry = true@ are treated as
-- map-entry stubs emitted by 'astToDescriptor'. They are paired back
-- with the corresponding repeated-message field to reconstruct the
-- original 'MEMapField' element.
descriptorToMessage :: R.DescriptorProto -> MessageDef
descriptorToMessage dp =
  MessageDef
    { msgExt = ()
    , msgDoc = Nothing
    , msgName = fromMaybe "" (R.descriptorProtoName dp)
    , msgElements = fieldElems <> nestedElems <> enumElems
    }
  where
    oneofNames =
      V.toList
        (fmap (fromMaybe "" . R.oneofDescriptorProtoName) (R.descriptorProtoOneofDecl dp))
    allFields = V.toList (R.descriptorProtoField dp)
    nestedTypes = V.toList (R.descriptorProtoNestedType dp)

    -- Separate map-entry stubs from real nested messages.
    (mapEntries, realNested) = partition isMapEntry nestedTypes
    isMapEntry n =
      maybe False (fromMaybe False . R.messageOptionsMapEntry) (R.descriptorProtoOptions n)

    -- Map entry names → MapField (if key/value fields are decodable).
    mapEntryMap :: [(Text, MessageElement)]
    mapEntryMap = concatMap entryToMapElem mapEntries
    entryToMapElem n =
      let entryName = fromMaybe "" (R.descriptorProtoName n)
          fields = V.toList (R.descriptorProtoField n)
          mkeyF = find (\f -> R.fieldDescriptorProtoNumber f == Just 1) fields
          mvalF = find (\f -> R.fieldDescriptorProtoNumber f == Just 2) fields
      in case (mkeyF, mvalF) of
           (Just kf, Just vf) ->
             case R.fieldDescriptorProtoType kf of
               Just kt
                 | Just ks <- fieldTypeToScalar kt ->
                     -- Find the repeated field whose typeName matches this entry.
                     let vft = typeEnumToFieldType
                                 (R.fieldDescriptorProtoType vf)
                                 (fromMaybe "" (R.fieldDescriptorProtoTypeName vf))
                         matchingField = find
                           (\f -> R.fieldDescriptorProtoType f
                                    == Just R.FieldDescriptorProto'Type'TypeMessage
                                  && maybe False (\tn -> tn == entryName || T.isSuffixOf ("." <> entryName) tn)
                                       (R.fieldDescriptorProtoTypeName f))
                           allFields
                     in case matchingField of
                          Just mf ->
                            [ ( fromMaybe "" (R.fieldDescriptorProtoName mf)
                              , MEMapField MapField
                                  { mapExt = ()
                                  , mapDoc = Nothing
                                  , mapKeyType = ks
                                  , mapValueType = vft
                                  , mapFieldName = fromMaybe "" (R.fieldDescriptorProtoName mf)
                                  , mapFieldNum = FieldNumber (fromIntegral (fromMaybe 0 (R.fieldDescriptorProtoNumber mf)))
                                  , mapOptions = []
                                  }
                              )
                            ]
                          Nothing -> []
               _ -> []
           _ -> []

    mapFieldNames :: [Text]
    mapFieldNames = fmap fst mapEntryMap

    -- Fields that are NOT the repeated-message backing a map entry.
    (oneofFields', regularFields) = partitionOneofFields oneofNames
      (filter (\f -> fromMaybe "" (R.fieldDescriptorProtoName f) `notElem` mapFieldNames) allFields)

    fieldElems =
      fmap (MEField . fieldDescriptorToField) regularFields
        <> fmap snd mapEntryMap
    nestedElems = fmap (MEMessage . descriptorToMessage) realNested
    enumElems =
      fmap (MEEnum . enumDescriptorToEnum) (V.toList (R.descriptorProtoEnumType dp))
        <> buildOneofDefs oneofNames oneofFields'


partitionOneofFields ::
  [Text] ->
  [R.FieldDescriptorProto] ->
  ([[R.FieldDescriptorProto]], [R.FieldDescriptorProto])
partitionOneofFields oneofNames fields =
  let regular = filter (isNothing . R.fieldDescriptorProtoOneofIndex) fields
      grouped =
        fmap
          ( \(i, _name) ->
              filter
                (\f -> R.fieldDescriptorProtoOneofIndex f == Just (fromIntegral (i :: Int)))
                fields
          )
          (zip [(0 :: Int) ..] oneofNames)
  in (grouped, regular)


buildOneofDefs :: [Text] -> [[R.FieldDescriptorProto]] -> [MessageElement]
buildOneofDefs = zipWith mkOneof
  where
    mkOneof name flds =
      MEOneof
        ( OneofDef
            { oneofExt = ()
            , oneofDoc = Nothing
            , oneofName = name
            , oneofFields = fmap mkOneofField flds
            , oneofOptions = []
            }
        )
    mkOneofField f =
      OneofField
        { oneofFieldExt = ()
        , oneofFieldDoc = Nothing
        , oneofFieldType =
            typeEnumToFieldType
              (R.fieldDescriptorProtoType f)
              (fromMaybe "" (R.fieldDescriptorProtoTypeName f))
        , oneofFieldName = fromMaybe "" (R.fieldDescriptorProtoName f)
        , oneofFieldNumber =
            FieldNumber (fromIntegral (fromMaybe 0 (R.fieldDescriptorProtoNumber f)))
        , oneofFieldOptions = []
        }


-- | Convert a 'FieldDescriptorProto' to a 'FieldDef'.
fieldDescriptorToField :: R.FieldDescriptorProto -> FieldDef
fieldDescriptorToField f =
  FieldDef
    { fieldExt = ()
    , fieldDoc = Nothing
    , fieldLabel = labelEnumToMaybeLabel (R.fieldDescriptorProtoLabel f)
    , fieldType =
        typeEnumToFieldType
          (R.fieldDescriptorProtoType f)
          (fromMaybe "" (R.fieldDescriptorProtoTypeName f))
    , fieldName = fromMaybe "" (R.fieldDescriptorProtoName f)
    , fieldNumber =
        FieldNumber (fromIntegral (fromMaybe 0 (R.fieldDescriptorProtoNumber f)))
    , fieldOptions = jsonNameOptMaybe (R.fieldDescriptorProtoJsonName f)
    }


jsonNameOptMaybe :: Maybe Text -> [OptionDef]
jsonNameOptMaybe Nothing = []
jsonNameOptMaybe (Just t)
  | T.null t = []
  | otherwise = [OptionDef () (OptionName [SimpleOption "json_name"]) (CString t)]


-- | Convert an 'EnumDescriptorProto' to an 'EnumDef'.
enumDescriptorToEnum :: R.EnumDescriptorProto -> EnumDef
enumDescriptorToEnum e =
  EnumDef
    { enumExt = ()
    , enumDoc = Nothing
    , enumName = fromMaybe "" (R.enumDescriptorProtoName e)
    , enumValues =
        fmap
          ( \v ->
              EnumValue
                ()
                Nothing
                (fromMaybe "" (R.enumValueDescriptorProtoName v))
                (fromIntegral (fromMaybe 0 (R.enumValueDescriptorProtoNumber v)))
                []
          )
          (V.toList (R.enumDescriptorProtoValue e))
    , enumOptions = []
    }


-- | Convert a 'ServiceDescriptorProto' to a 'ServiceDef'.
serviceDescriptorToService :: R.ServiceDescriptorProto -> ServiceDef
serviceDescriptorToService s =
  ServiceDef
    { svcExt = ()
    , svcDoc = Nothing
    , svcName = fromMaybe "" (R.serviceDescriptorProtoName s)
    , svcRpcs = fmap methodToRpc (V.toList (R.serviceDescriptorProtoMethod s))
    , svcOptions = []
    }
  where
    methodToRpc m =
      RpcDef
        { rpcExt = ()
        , rpcDoc = Nothing
        , rpcName = fromMaybe "" (R.methodDescriptorProtoName m)
        , rpcInput = fromMaybe "" (R.methodDescriptorProtoInputType m)
        , rpcInputStr =
            if fromMaybe False (R.methodDescriptorProtoClientStreaming m)
              then Streaming
              else NoStream
        , rpcOutput = fromMaybe "" (R.methodDescriptorProtoOutputType m)
        , rpcOutputStr =
            if fromMaybe False (R.methodDescriptorProtoServerStreaming m)
              then Streaming
              else NoStream
        , rpcOptions = []
        }


labelEnumToMaybeLabel :: Maybe R.FieldDescriptorProto'Label -> Maybe FieldLabel
labelEnumToMaybeLabel Nothing = Nothing
labelEnumToMaybeLabel (Just R.FieldDescriptorProto'Label'LabelOptional) = Just Optional
labelEnumToMaybeLabel (Just R.FieldDescriptorProto'Label'LabelRequired) = Just Required
labelEnumToMaybeLabel (Just R.FieldDescriptorProto'Label'LabelRepeated) = Just Repeated


typeEnumToFieldType :: Maybe R.FieldDescriptorProto'Type -> Text -> FieldType
typeEnumToFieldType mTy typeName = case mTy of
  Just R.FieldDescriptorProto'Type'TypeDouble -> FTScalar SDouble
  Just R.FieldDescriptorProto'Type'TypeFloat -> FTScalar SFloat
  Just R.FieldDescriptorProto'Type'TypeInt64 -> FTScalar SInt64
  Just R.FieldDescriptorProto'Type'TypeUint64 -> FTScalar SUInt64
  Just R.FieldDescriptorProto'Type'TypeInt32 -> FTScalar SInt32
  Just R.FieldDescriptorProto'Type'TypeFixed64 -> FTScalar SFixed64
  Just R.FieldDescriptorProto'Type'TypeFixed32 -> FTScalar SFixed32
  Just R.FieldDescriptorProto'Type'TypeBool -> FTScalar SBool
  Just R.FieldDescriptorProto'Type'TypeString -> FTScalar SString
  Just R.FieldDescriptorProto'Type'TypeGroup -> FTNamed (stripLeadingDot typeName)
  Just R.FieldDescriptorProto'Type'TypeMessage -> FTNamed (stripLeadingDot typeName)
  Just R.FieldDescriptorProto'Type'TypeBytes -> FTScalar SBytes
  Just R.FieldDescriptorProto'Type'TypeUint32 -> FTScalar SUInt32
  Just R.FieldDescriptorProto'Type'TypeEnum -> FTNamed (stripLeadingDot typeName)
  Just R.FieldDescriptorProto'Type'TypeSfixed32 -> FTScalar SSFixed32
  Just R.FieldDescriptorProto'Type'TypeSfixed64 -> FTScalar SSFixed64
  Just R.FieldDescriptorProto'Type'TypeSint32 -> FTScalar SSInt32
  Just R.FieldDescriptorProto'Type'TypeSint64 -> FTScalar SSInt64
  Nothing -> FTNamed (stripLeadingDot typeName)


stripLeadingDot :: Text -> Text
stripLeadingDot t = fromMaybe t (T.stripPrefix "." t)


-- | Invert 'scalarToFieldType': map a 'FieldDescriptorProto'Type' back to a
-- 'ScalarType'. Returns 'Nothing' for TYPE_MESSAGE, TYPE_ENUM, and TYPE_GROUP
-- (which are not scalar).
fieldTypeToScalar :: R.FieldDescriptorProto'Type -> Maybe ScalarType
fieldTypeToScalar = \case
  R.FieldDescriptorProto'Type'TypeDouble   -> Just SDouble
  R.FieldDescriptorProto'Type'TypeFloat    -> Just SFloat
  R.FieldDescriptorProto'Type'TypeInt64    -> Just SInt64
  R.FieldDescriptorProto'Type'TypeUint64   -> Just SUInt64
  R.FieldDescriptorProto'Type'TypeInt32    -> Just SInt32
  R.FieldDescriptorProto'Type'TypeFixed64  -> Just SFixed64
  R.FieldDescriptorProto'Type'TypeFixed32  -> Just SFixed32
  R.FieldDescriptorProto'Type'TypeBool     -> Just SBool
  R.FieldDescriptorProto'Type'TypeString   -> Just SString
  R.FieldDescriptorProto'Type'TypeBytes    -> Just SBytes
  R.FieldDescriptorProto'Type'TypeUint32   -> Just SUInt32
  R.FieldDescriptorProto'Type'TypeSfixed32 -> Just SSFixed32
  R.FieldDescriptorProto'Type'TypeSfixed64 -> Just SSFixed64
  R.FieldDescriptorProto'Type'TypeSint32   -> Just SSInt32
  R.FieldDescriptorProto'Type'TypeSint64   -> Just SSInt64
  _                                        -> Nothing
