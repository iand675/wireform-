{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell types for the full @google/protobuf/descriptor.proto@.
--
-- These are the core descriptor types used by protoc and other protobuf tools:
-- @FileDescriptorProto@ and friends, the per-element @*Options@ messages, the
-- @SourceCodeInfo@ / @GeneratedCodeInfo@ side tables, and @UninterpretedOption@.
--
-- This module is deliberately hand-written rather than produced by the
-- @regen-wkt@ code generator: it is the bootstrap type that the generator
-- itself uses to embed a serialized @FileDescriptorProto@ in every generated
-- module, so it cannot depend on generated output.
--
-- Every message preserves __unknown fields__ on decode and re-emits them on
-- encode (the @*UnknownFields@ accessor + 'Proto.Decode.captureUnknownField' /
-- 'Proto.Decode.encodeUnknownFields'). This is what lets custom options —
-- most importantly protobuf option /extensions/ such as @buf.validate@'s
-- extension #1159 on 'FieldOptions' / 'MessageOptions' — survive a
-- decode/encode round trip even though they are not modeled as typed fields.
-- See "Proto.IDL.Descriptor" for AST conversion and (downstream)
-- @Protovalidate.Descriptor@ for reading @buf.validate@ rules out of a
-- 'FileDescriptorSet'.
--
-- Field numbers and types follow @descriptor.proto@. The classic
-- packed-repeated @int32@ side fields (@public_dependency@, @weak_dependency@,
-- @SourceCodeInfo.Location.path/span@) are not given typed accessors; they are
-- preserved losslessly as unknown fields.
module Proto.Google.Protobuf.Descriptor where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)

import Proto.Decode
import Proto.Encode
import Proto.Internal.Wire (Tag (..), WireType (..))

----------------------------------------------------------------------
-- FileDescriptorSet
----------------------------------------------------------------------

data FileDescriptorSet = FileDescriptorSet
  { fdsFile :: !(V.Vector FileDescriptorProto)
  , fdsUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultFileDescriptorSet :: FileDescriptorSet
defaultFileDescriptorSet = FileDescriptorSet V.empty []

instance MessageEncode FileDescriptorSet where
  buildMessage m =
    V.foldl' (\a f -> a <> encodeFieldMessage 1 f) mempty (fdsFile m)
      <> encodeUnknownFields (fdsUnknownFields m)

instance MessageDecode FileDescriptorSet where
  messageDecoder = loop defaultFileDescriptorSet
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {fdsUnknownFields = reverse (fdsUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldMessage; loop m {fdsFile = V.snoc (fdsFile m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {fdsUnknownFields = uf : fdsUnknownFields m}

----------------------------------------------------------------------
-- FileDescriptorProto
----------------------------------------------------------------------

data FileDescriptorProto = FileDescriptorProto
  { fdpName :: !Text
  , fdpPackage :: !Text
  , fdpDependency :: !(V.Vector Text)
  , fdpMessageType :: !(V.Vector DescriptorProto)
  , fdpEnumType :: !(V.Vector EnumDescriptorProto)
  , fdpService :: !(V.Vector ServiceDescriptorProto)
  , fdpExtension :: !(V.Vector FieldDescriptorProto)
  , fdpOptions :: !(Maybe FileOptions)
  , fdpSourceCodeInfo :: !(Maybe SourceCodeInfo)
  , fdpSyntax :: !Text
  , fdpEdition :: !Text
  , fdpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultFileDescriptorProto :: FileDescriptorProto
defaultFileDescriptorProto =
  FileDescriptorProto "" "" V.empty V.empty V.empty V.empty V.empty Nothing Nothing "" "" []

instance MessageEncode FileDescriptorProto where
  buildMessage m =
    encStr 1 (fdpName m)
      <> encStr 2 (fdpPackage m)
      <> V.foldl' (\a d -> a <> encodeFieldString 3 d) mempty (fdpDependency m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 4 x) mempty (fdpMessageType m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 5 x) mempty (fdpEnumType m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 6 x) mempty (fdpService m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 7 x) mempty (fdpExtension m)
      <> encMsg 8 (fdpOptions m)
      <> encMsg 9 (fdpSourceCodeInfo m)
      <> encStr 12 (fdpSyntax m)
      <> maybe (encStr 14 (fdpEdition m)) (encodeFieldVarint 14) (editionWireNumber (fdpEdition m))
      <> encodeUnknownFields (fdpUnknownFields m)

instance MessageDecode FileDescriptorProto where
  messageDecoder = loop defaultFileDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {fdpUnknownFields = reverse (fdpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {fdpName = v}
            2 -> do v <- decodeFieldString; loop m {fdpPackage = v}
            3 -> do v <- decodeFieldString; loop m {fdpDependency = V.snoc (fdpDependency m) v}
            4 -> do v <- decodeFieldMessage; loop m {fdpMessageType = V.snoc (fdpMessageType m) v}
            5 -> do v <- decodeFieldMessage; loop m {fdpEnumType = V.snoc (fdpEnumType m) v}
            6 -> do v <- decodeFieldMessage; loop m {fdpService = V.snoc (fdpService m) v}
            7 -> do v <- decodeFieldMessage; loop m {fdpExtension = V.snoc (fdpExtension m) v}
            8 -> do v <- decodeFieldMessage; loop m {fdpOptions = Just v}
            9 -> do v <- decodeFieldMessage; loop m {fdpSourceCodeInfo = Just v}
            12 -> do v <- decodeFieldString; loop m {fdpSyntax = v}
            14 | wt == WireVarint -> do
              v <- decodeFieldVarint
              case fst <$> find ((== v) . snd) editionNumbers of
                Just name -> loop m {fdpEdition = name}
                Nothing -> loop m {fdpUnknownFields = UnknownVarint 14 v : fdpUnknownFields m}
            -- Older Wireform-generated descriptors used the edition name.
            14 | wt == WireLengthDelimited -> do v <- decodeFieldString; loop m {fdpEdition = v}
            _ -> do uf <- captureUnknownField n wt; loop m {fdpUnknownFields = uf : fdpUnknownFields m}


editionWireNumber :: Text -> Maybe Word64
editionWireNumber name = lookup name editionNumbers


editionNumbers :: [(Text, Word64)]
editionNumbers = [("2023", edition2023WireNumber), ("2024", edition2024WireNumber)]


edition2023WireNumber, edition2024WireNumber :: Word64
edition2023WireNumber = 1000
edition2024WireNumber = 1001

----------------------------------------------------------------------
-- DescriptorProto (+ ExtensionRange, ReservedRange)
----------------------------------------------------------------------

data DescriptorProto = DescriptorProto
  { dpName :: !Text
  , dpField :: !(V.Vector FieldDescriptorProto)
  , dpNestedType :: !(V.Vector DescriptorProto)
  , dpEnumType :: !(V.Vector EnumDescriptorProto)
  , dpExtensionRange :: !(V.Vector DescriptorProtoExtensionRange)
  , dpExtension :: !(V.Vector FieldDescriptorProto)
  , dpOptions :: !(Maybe MessageOptions)
  , dpOneofDecl :: !(V.Vector OneofDescriptorProto)
  , dpReservedRange :: !(V.Vector DescriptorProtoReservedRange)
  , dpReservedName :: !(V.Vector Text)
  , dpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultDescriptorProto :: DescriptorProto
defaultDescriptorProto =
  DescriptorProto "" V.empty V.empty V.empty V.empty V.empty Nothing V.empty V.empty V.empty []

instance MessageEncode DescriptorProto where
  buildMessage m =
    encStr 1 (dpName m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 2 x) mempty (dpField m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 3 x) mempty (dpNestedType m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 4 x) mempty (dpEnumType m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 5 x) mempty (dpExtensionRange m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 6 x) mempty (dpExtension m)
      <> encMsg 7 (dpOptions m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 8 x) mempty (dpOneofDecl m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 9 x) mempty (dpReservedRange m)
      <> V.foldl' (\a x -> a <> encodeFieldString 10 x) mempty (dpReservedName m)
      <> encodeUnknownFields (dpUnknownFields m)

instance MessageDecode DescriptorProto where
  messageDecoder = loop defaultDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {dpUnknownFields = reverse (dpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {dpName = v}
            2 -> do v <- decodeFieldMessage; loop m {dpField = V.snoc (dpField m) v}
            3 -> do v <- decodeFieldMessage; loop m {dpNestedType = V.snoc (dpNestedType m) v}
            4 -> do v <- decodeFieldMessage; loop m {dpEnumType = V.snoc (dpEnumType m) v}
            5 -> do v <- decodeFieldMessage; loop m {dpExtensionRange = V.snoc (dpExtensionRange m) v}
            6 -> do v <- decodeFieldMessage; loop m {dpExtension = V.snoc (dpExtension m) v}
            7 -> do v <- decodeFieldMessage; loop m {dpOptions = Just v}
            8 -> do v <- decodeFieldMessage; loop m {dpOneofDecl = V.snoc (dpOneofDecl m) v}
            9 -> do v <- decodeFieldMessage; loop m {dpReservedRange = V.snoc (dpReservedRange m) v}
            10 -> do v <- decodeFieldString; loop m {dpReservedName = V.snoc (dpReservedName m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {dpUnknownFields = uf : dpUnknownFields m}

data DescriptorProtoExtensionRange = DescriptorProtoExtensionRange
  { dperStart :: !Int32
  , dperEnd :: !Int32
  , dperOptions :: !(Maybe ExtensionRangeOptions)
  , dperUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultDescriptorProtoExtensionRange :: DescriptorProtoExtensionRange
defaultDescriptorProtoExtensionRange = DescriptorProtoExtensionRange 0 0 Nothing []

instance MessageEncode DescriptorProtoExtensionRange where
  buildMessage m =
    encI32 1 (dperStart m) <> encI32 2 (dperEnd m) <> encMsg 3 (dperOptions m)
      <> encodeUnknownFields (dperUnknownFields m)

instance MessageDecode DescriptorProtoExtensionRange where
  messageDecoder = loop defaultDescriptorProtoExtensionRange
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {dperUnknownFields = reverse (dperUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldVarint; loop m {dperStart = fromIntegral v}
            2 -> do v <- decodeFieldVarint; loop m {dperEnd = fromIntegral v}
            3 -> do v <- decodeFieldMessage; loop m {dperOptions = Just v}
            _ -> do uf <- captureUnknownField n wt; loop m {dperUnknownFields = uf : dperUnknownFields m}

data DescriptorProtoReservedRange = DescriptorProtoReservedRange
  { dprrStart :: !Int32
  , dprrEnd :: !Int32
  , dprrUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultDescriptorProtoReservedRange :: DescriptorProtoReservedRange
defaultDescriptorProtoReservedRange = DescriptorProtoReservedRange 0 0 []

instance MessageEncode DescriptorProtoReservedRange where
  buildMessage m =
    encI32 1 (dprrStart m) <> encI32 2 (dprrEnd m) <> encodeUnknownFields (dprrUnknownFields m)

instance MessageDecode DescriptorProtoReservedRange where
  messageDecoder = loop defaultDescriptorProtoReservedRange
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {dprrUnknownFields = reverse (dprrUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldVarint; loop m {dprrStart = fromIntegral v}
            2 -> do v <- decodeFieldVarint; loop m {dprrEnd = fromIntegral v}
            _ -> do uf <- captureUnknownField n wt; loop m {dprrUnknownFields = uf : dprrUnknownFields m}

----------------------------------------------------------------------
-- FieldDescriptorProto
----------------------------------------------------------------------

-- | The wire @type@ codes (field 5 of 'FieldDescriptorProto').
data FieldDescriptorType
  = TYPE_DOUBLE | TYPE_FLOAT | TYPE_INT64 | TYPE_UINT64 | TYPE_INT32
  | TYPE_FIXED64 | TYPE_FIXED32 | TYPE_BOOL | TYPE_STRING | TYPE_GROUP
  | TYPE_MESSAGE | TYPE_BYTES | TYPE_UINT32 | TYPE_ENUM | TYPE_SFIXED32
  | TYPE_SFIXED64 | TYPE_SINT32 | TYPE_SINT64
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The @label@ codes (field 4 of 'FieldDescriptorProto').
data FieldDescriptorLabel
  = LABEL_OPTIONAL | LABEL_REQUIRED | LABEL_REPEATED
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data FieldDescriptorProto = FieldDescriptorProto
  { fdpFieldName :: !Text
  , fdpFieldExtendee :: !Text
  , fdpFieldNumber :: !Int32
  , fdpFieldLabel :: !Int32
  , fdpFieldType :: !Int32
  , fdpFieldTypeName :: !Text
  , fdpFieldDefault :: !Text
  , fdpFieldOptions :: !(Maybe FieldOptions)
  , fdpFieldOneofIdx :: !Int32
  , fdpFieldJsonName :: !Text
  , fdpFieldProto3Optional :: !Bool
  , fdpFieldUnknownFields :: ![UnknownField]
  , fdpFieldHasEmptyDefault :: !Bool
  , fdpFieldHasEmptyJsonName :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultFieldDescriptorProto :: FieldDescriptorProto
defaultFieldDescriptorProto =
  FieldDescriptorProto "" "" 0 0 0 "" "" Nothing (-1) "" False [] False False

instance MessageEncode FieldDescriptorProto where
  buildMessage m =
    encStr 1 (fdpFieldName m)
      <> encStr 2 (fdpFieldExtendee m)
      <> encI32 3 (fdpFieldNumber m)
      <> encI32 4 (fdpFieldLabel m)
      <> encI32 5 (fdpFieldType m)
      <> encStr 6 (fdpFieldTypeName m)
      <> (if fdpFieldHasEmptyDefault m then encodeFieldString 7 else encStr 7) (fdpFieldDefault m)
      <> encMsg 8 (fdpFieldOptions m)
      <> (if fdpFieldOneofIdx m < 0 then mempty else encodeFieldVarint 9 (fromIntegral (fdpFieldOneofIdx m)))
      <> (if fdpFieldHasEmptyJsonName m then encodeFieldString 10 else encStr 10) (fdpFieldJsonName m)
      <> (if fdpFieldProto3Optional m then encodeFieldBool 17 True else mempty)
      <> encodeUnknownFields (fdpFieldUnknownFields m)

instance MessageDecode FieldDescriptorProto where
  messageDecoder = loop defaultFieldDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {fdpFieldUnknownFields = reverse (fdpFieldUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {fdpFieldName = v}
            2 -> do v <- decodeFieldString; loop m {fdpFieldExtendee = v}
            3 -> do v <- decodeFieldVarint; loop m {fdpFieldNumber = fromIntegral v}
            4 -> do v <- decodeFieldVarint; loop m {fdpFieldLabel = fromIntegral v}
            5 -> do v <- decodeFieldVarint; loop m {fdpFieldType = fromIntegral v}
            6 -> do v <- decodeFieldString; loop m {fdpFieldTypeName = v}
            7 -> do v <- decodeFieldString; loop m {fdpFieldDefault = v, fdpFieldHasEmptyDefault = v == ""}
            8 -> do v <- decodeFieldMessage; loop m {fdpFieldOptions = Just v}
            9 -> do v <- decodeFieldVarint; loop m {fdpFieldOneofIdx = fromIntegral v}
            10 -> do v <- decodeFieldString; loop m {fdpFieldJsonName = v, fdpFieldHasEmptyJsonName = v == ""}
            17 -> do v <- decodeFieldBool; loop m {fdpFieldProto3Optional = v}
            _ -> do uf <- captureUnknownField n wt; loop m {fdpFieldUnknownFields = uf : fdpFieldUnknownFields m}

----------------------------------------------------------------------
-- OneofDescriptorProto
----------------------------------------------------------------------

data OneofDescriptorProto = OneofDescriptorProto
  { odpName :: !Text
  , odpOptions :: !(Maybe OneofOptions)
  , odpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultOneofDescriptorProto :: OneofDescriptorProto
defaultOneofDescriptorProto = OneofDescriptorProto "" Nothing []

instance MessageEncode OneofDescriptorProto where
  buildMessage m =
    encStr 1 (odpName m) <> encMsg 2 (odpOptions m) <> encodeUnknownFields (odpUnknownFields m)

instance MessageDecode OneofDescriptorProto where
  messageDecoder = loop defaultOneofDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {odpUnknownFields = reverse (odpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {odpName = v}
            2 -> do v <- decodeFieldMessage; loop m {odpOptions = Just v}
            _ -> do uf <- captureUnknownField n wt; loop m {odpUnknownFields = uf : odpUnknownFields m}

----------------------------------------------------------------------
-- EnumDescriptorProto (+ EnumReservedRange) / EnumValueDescriptorProto
----------------------------------------------------------------------

data EnumDescriptorProto = EnumDescriptorProto
  { edpName :: !Text
  , edpValue :: !(V.Vector EnumValueDescriptorProto)
  , edpOptions :: !(Maybe EnumOptions)
  , edpReservedRange :: !(V.Vector EnumDescriptorProtoEnumReservedRange)
  , edpReservedName :: !(V.Vector Text)
  , edpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultEnumDescriptorProto :: EnumDescriptorProto
defaultEnumDescriptorProto = EnumDescriptorProto "" V.empty Nothing V.empty V.empty []

instance MessageEncode EnumDescriptorProto where
  buildMessage m =
    encStr 1 (edpName m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 2 x) mempty (edpValue m)
      <> encMsg 3 (edpOptions m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 4 x) mempty (edpReservedRange m)
      <> V.foldl' (\a x -> a <> encodeFieldString 5 x) mempty (edpReservedName m)
      <> encodeUnknownFields (edpUnknownFields m)

instance MessageDecode EnumDescriptorProto where
  messageDecoder = loop defaultEnumDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {edpUnknownFields = reverse (edpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {edpName = v}
            2 -> do v <- decodeFieldMessage; loop m {edpValue = V.snoc (edpValue m) v}
            3 -> do v <- decodeFieldMessage; loop m {edpOptions = Just v}
            4 -> do v <- decodeFieldMessage; loop m {edpReservedRange = V.snoc (edpReservedRange m) v}
            5 -> do v <- decodeFieldString; loop m {edpReservedName = V.snoc (edpReservedName m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {edpUnknownFields = uf : edpUnknownFields m}

data EnumDescriptorProtoEnumReservedRange = EnumDescriptorProtoEnumReservedRange
  { edprrStart :: !Int32
  , edprrEnd :: !Int32
  , edprrUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultEnumDescriptorProtoEnumReservedRange :: EnumDescriptorProtoEnumReservedRange
defaultEnumDescriptorProtoEnumReservedRange = EnumDescriptorProtoEnumReservedRange 0 0 []

instance MessageEncode EnumDescriptorProtoEnumReservedRange where
  buildMessage m =
    encI32 1 (edprrStart m) <> encI32 2 (edprrEnd m) <> encodeUnknownFields (edprrUnknownFields m)

instance MessageDecode EnumDescriptorProtoEnumReservedRange where
  messageDecoder = loop defaultEnumDescriptorProtoEnumReservedRange
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {edprrUnknownFields = reverse (edprrUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldVarint; loop m {edprrStart = fromIntegral v}
            2 -> do v <- decodeFieldVarint; loop m {edprrEnd = fromIntegral v}
            _ -> do uf <- captureUnknownField n wt; loop m {edprrUnknownFields = uf : edprrUnknownFields m}

data EnumValueDescriptorProto = EnumValueDescriptorProto
  { evdpName :: !Text
  , evdpNumber :: !Int32
  , evdpOptions :: !(Maybe EnumValueOptions)
  , evdpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultEnumValueDescriptorProto :: EnumValueDescriptorProto
defaultEnumValueDescriptorProto = EnumValueDescriptorProto "" 0 Nothing []

instance MessageEncode EnumValueDescriptorProto where
  buildMessage m =
    encStr 1 (evdpName m)
      <> encI32 2 (evdpNumber m)
      <> encMsg 3 (evdpOptions m)
      <> encodeUnknownFields (evdpUnknownFields m)

instance MessageDecode EnumValueDescriptorProto where
  messageDecoder = loop defaultEnumValueDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {evdpUnknownFields = reverse (evdpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {evdpName = v}
            2 -> do v <- decodeFieldVarint; loop m {evdpNumber = fromIntegral v}
            3 -> do v <- decodeFieldMessage; loop m {evdpOptions = Just v}
            _ -> do uf <- captureUnknownField n wt; loop m {evdpUnknownFields = uf : evdpUnknownFields m}

----------------------------------------------------------------------
-- ServiceDescriptorProto / MethodDescriptorProto
----------------------------------------------------------------------

data ServiceDescriptorProto = ServiceDescriptorProto
  { sdpName :: !Text
  , sdpMethod :: !(V.Vector MethodDescriptorProto)
  , sdpOptions :: !(Maybe ServiceOptions)
  , sdpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultServiceDescriptorProto :: ServiceDescriptorProto
defaultServiceDescriptorProto = ServiceDescriptorProto "" V.empty Nothing []

instance MessageEncode ServiceDescriptorProto where
  buildMessage m =
    encStr 1 (sdpName m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 2 x) mempty (sdpMethod m)
      <> encMsg 3 (sdpOptions m)
      <> encodeUnknownFields (sdpUnknownFields m)

instance MessageDecode ServiceDescriptorProto where
  messageDecoder = loop defaultServiceDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {sdpUnknownFields = reverse (sdpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {sdpName = v}
            2 -> do v <- decodeFieldMessage; loop m {sdpMethod = V.snoc (sdpMethod m) v}
            3 -> do v <- decodeFieldMessage; loop m {sdpOptions = Just v}
            _ -> do uf <- captureUnknownField n wt; loop m {sdpUnknownFields = uf : sdpUnknownFields m}

data MethodDescriptorProto = MethodDescriptorProto
  { mdpName :: !Text
  , mdpInputType :: !Text
  , mdpOutputType :: !Text
  , mdpOptions :: !(Maybe MethodOptions)
  , mdpClientStreaming :: !Bool
  , mdpServerStreaming :: !Bool
  , mdpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultMethodDescriptorProto :: MethodDescriptorProto
defaultMethodDescriptorProto = MethodDescriptorProto "" "" "" Nothing False False []

instance MessageEncode MethodDescriptorProto where
  buildMessage m =
    encStr 1 (mdpName m)
      <> encStr 2 (mdpInputType m)
      <> encStr 3 (mdpOutputType m)
      <> encMsg 4 (mdpOptions m)
      <> (if mdpClientStreaming m then encodeFieldBool 5 True else mempty)
      <> (if mdpServerStreaming m then encodeFieldBool 6 True else mempty)
      <> encodeUnknownFields (mdpUnknownFields m)

instance MessageDecode MethodDescriptorProto where
  messageDecoder = loop defaultMethodDescriptorProto
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {mdpUnknownFields = reverse (mdpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {mdpName = v}
            2 -> do v <- decodeFieldString; loop m {mdpInputType = v}
            3 -> do v <- decodeFieldString; loop m {mdpOutputType = v}
            4 -> do v <- decodeFieldMessage; loop m {mdpOptions = Just v}
            5 -> do v <- decodeFieldBool; loop m {mdpClientStreaming = v}
            6 -> do v <- decodeFieldBool; loop m {mdpServerStreaming = v}
            _ -> do uf <- captureUnknownField n wt; loop m {mdpUnknownFields = uf : mdpUnknownFields m}

----------------------------------------------------------------------
-- Options messages
--
-- The common standard fields are modeled; every other set field (including
-- option /extensions/ such as buf.validate) is preserved as an unknown field.
----------------------------------------------------------------------

data FileOptions = FileOptions
  { foJavaPackage :: !Text
  , foGoPackage :: !Text
  , foDeprecated :: !Bool
  , foUninterpretedOption :: !(V.Vector UninterpretedOption)
  , foUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultFileOptions :: FileOptions
defaultFileOptions = FileOptions "" "" False V.empty []

instance MessageEncode FileOptions where
  buildMessage m =
    encStr 1 (foJavaPackage m)
      <> encStr 11 (foGoPackage m)
      <> (if foDeprecated m then encodeFieldBool 23 True else mempty)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (foUninterpretedOption m)
      <> encodeUnknownFields (foUnknownFields m)

instance MessageDecode FileOptions where
  messageDecoder = loop defaultFileOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {foUnknownFields = reverse (foUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {foJavaPackage = v}
            11 -> do v <- decodeFieldString; loop m {foGoPackage = v}
            23 -> do v <- decodeFieldBool; loop m {foDeprecated = v}
            999 -> do v <- decodeFieldMessage; loop m {foUninterpretedOption = V.snoc (foUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {foUnknownFields = uf : foUnknownFields m}

data MessageOptions = MessageOptions
  { moMessageSetWireFormat :: !Bool
  , moNoStandardDescriptorAccessor :: !Bool
  , moDeprecated :: !Bool
  , moMapEntry :: !Bool
  , moUninterpretedOption :: !(V.Vector UninterpretedOption)
  , moUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultMessageOptions :: MessageOptions
defaultMessageOptions = MessageOptions False False False False V.empty []

instance MessageEncode MessageOptions where
  buildMessage m =
    (if moMessageSetWireFormat m then encodeFieldBool 1 True else mempty)
      <> (if moNoStandardDescriptorAccessor m then encodeFieldBool 2 True else mempty)
      <> (if moDeprecated m then encodeFieldBool 3 True else mempty)
      <> (if moMapEntry m then encodeFieldBool 7 True else mempty)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (moUninterpretedOption m)
      <> encodeUnknownFields (moUnknownFields m)

instance MessageDecode MessageOptions where
  messageDecoder = loop defaultMessageOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {moUnknownFields = reverse (moUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldBool; loop m {moMessageSetWireFormat = v}
            2 -> do v <- decodeFieldBool; loop m {moNoStandardDescriptorAccessor = v}
            3 -> do v <- decodeFieldBool; loop m {moDeprecated = v}
            7 -> do v <- decodeFieldBool; loop m {moMapEntry = v}
            999 -> do v <- decodeFieldMessage; loop m {moUninterpretedOption = V.snoc (moUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {moUnknownFields = uf : moUnknownFields m}

data FieldOptions = FieldOptions
  { fldoCtype :: !Int32
  , fldoPacked :: !Bool
  , fldoDeprecated :: !Bool
  , fldoJstype :: !Int32
  , fldoUninterpretedOption :: !(V.Vector UninterpretedOption)
  , fldoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultFieldOptions :: FieldOptions
defaultFieldOptions = FieldOptions 0 False False 0 V.empty []

instance MessageEncode FieldOptions where
  buildMessage m =
    encI32 1 (fldoCtype m)
      <> (if fldoPacked m then encodeFieldBool 2 True else mempty)
      <> (if fldoDeprecated m then encodeFieldBool 3 True else mempty)
      <> encI32 6 (fldoJstype m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (fldoUninterpretedOption m)
      <> encodeUnknownFields (fldoUnknownFields m)

instance MessageDecode FieldOptions where
  messageDecoder = loop defaultFieldOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {fldoUnknownFields = reverse (fldoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldVarint; loop m {fldoCtype = fromIntegral v}
            2 -> do v <- decodeFieldBool; loop m {fldoPacked = v}
            3 -> do v <- decodeFieldBool; loop m {fldoDeprecated = v}
            6 -> do v <- decodeFieldVarint; loop m {fldoJstype = fromIntegral v}
            999 -> do v <- decodeFieldMessage; loop m {fldoUninterpretedOption = V.snoc (fldoUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {fldoUnknownFields = uf : fldoUnknownFields m}

data OneofOptions = OneofOptions
  { oneofoUninterpretedOption :: !(V.Vector UninterpretedOption)
  , oneofoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultOneofOptions :: OneofOptions
defaultOneofOptions = OneofOptions V.empty []

instance MessageEncode OneofOptions where
  buildMessage m =
    V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (oneofoUninterpretedOption m)
      <> encodeUnknownFields (oneofoUnknownFields m)

instance MessageDecode OneofOptions where
  messageDecoder = loop defaultOneofOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {oneofoUnknownFields = reverse (oneofoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            999 -> do v <- decodeFieldMessage; loop m {oneofoUninterpretedOption = V.snoc (oneofoUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {oneofoUnknownFields = uf : oneofoUnknownFields m}

data EnumOptions = EnumOptions
  { enoAllowAlias :: !Bool
  , enoDeprecated :: !Bool
  , enoUninterpretedOption :: !(V.Vector UninterpretedOption)
  , enoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultEnumOptions :: EnumOptions
defaultEnumOptions = EnumOptions False False V.empty []

instance MessageEncode EnumOptions where
  buildMessage m =
    (if enoAllowAlias m then encodeFieldBool 2 True else mempty)
      <> (if enoDeprecated m then encodeFieldBool 3 True else mempty)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (enoUninterpretedOption m)
      <> encodeUnknownFields (enoUnknownFields m)

instance MessageDecode EnumOptions where
  messageDecoder = loop defaultEnumOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {enoUnknownFields = reverse (enoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            2 -> do v <- decodeFieldBool; loop m {enoAllowAlias = v}
            3 -> do v <- decodeFieldBool; loop m {enoDeprecated = v}
            999 -> do v <- decodeFieldMessage; loop m {enoUninterpretedOption = V.snoc (enoUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {enoUnknownFields = uf : enoUnknownFields m}

data EnumValueOptions = EnumValueOptions
  { evoDeprecated :: !Bool
  , evoUninterpretedOption :: !(V.Vector UninterpretedOption)
  , evoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultEnumValueOptions :: EnumValueOptions
defaultEnumValueOptions = EnumValueOptions False V.empty []

instance MessageEncode EnumValueOptions where
  buildMessage m =
    (if evoDeprecated m then encodeFieldBool 1 True else mempty)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (evoUninterpretedOption m)
      <> encodeUnknownFields (evoUnknownFields m)

instance MessageDecode EnumValueOptions where
  messageDecoder = loop defaultEnumValueOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {evoUnknownFields = reverse (evoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldBool; loop m {evoDeprecated = v}
            999 -> do v <- decodeFieldMessage; loop m {evoUninterpretedOption = V.snoc (evoUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {evoUnknownFields = uf : evoUnknownFields m}

data ServiceOptions = ServiceOptions
  { svoDeprecated :: !Bool
  , svoUninterpretedOption :: !(V.Vector UninterpretedOption)
  , svoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultServiceOptions :: ServiceOptions
defaultServiceOptions = ServiceOptions False V.empty []

instance MessageEncode ServiceOptions where
  buildMessage m =
    (if svoDeprecated m then encodeFieldBool 33 True else mempty)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (svoUninterpretedOption m)
      <> encodeUnknownFields (svoUnknownFields m)

instance MessageDecode ServiceOptions where
  messageDecoder = loop defaultServiceOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {svoUnknownFields = reverse (svoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            33 -> do v <- decodeFieldBool; loop m {svoDeprecated = v}
            999 -> do v <- decodeFieldMessage; loop m {svoUninterpretedOption = V.snoc (svoUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {svoUnknownFields = uf : svoUnknownFields m}

data MethodOptions = MethodOptions
  { mtoDeprecated :: !Bool
  , mtoIdempotencyLevel :: !Int32
  , mtoUninterpretedOption :: !(V.Vector UninterpretedOption)
  , mtoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultMethodOptions :: MethodOptions
defaultMethodOptions = MethodOptions False 0 V.empty []

instance MessageEncode MethodOptions where
  buildMessage m =
    (if mtoDeprecated m then encodeFieldBool 33 True else mempty)
      <> encI32 34 (mtoIdempotencyLevel m)
      <> V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (mtoUninterpretedOption m)
      <> encodeUnknownFields (mtoUnknownFields m)

instance MessageDecode MethodOptions where
  messageDecoder = loop defaultMethodOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {mtoUnknownFields = reverse (mtoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            33 -> do v <- decodeFieldBool; loop m {mtoDeprecated = v}
            34 -> do v <- decodeFieldVarint; loop m {mtoIdempotencyLevel = fromIntegral v}
            999 -> do v <- decodeFieldMessage; loop m {mtoUninterpretedOption = V.snoc (mtoUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {mtoUnknownFields = uf : mtoUnknownFields m}

data ExtensionRangeOptions = ExtensionRangeOptions
  { eroUninterpretedOption :: !(V.Vector UninterpretedOption)
  , eroUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultExtensionRangeOptions :: ExtensionRangeOptions
defaultExtensionRangeOptions = ExtensionRangeOptions V.empty []

instance MessageEncode ExtensionRangeOptions where
  buildMessage m =
    V.foldl' (\a x -> a <> encodeFieldMessage 999 x) mempty (eroUninterpretedOption m)
      <> encodeUnknownFields (eroUnknownFields m)

instance MessageDecode ExtensionRangeOptions where
  messageDecoder = loop defaultExtensionRangeOptions
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {eroUnknownFields = reverse (eroUnknownFields m)}
          UJust (Tag n wt) -> case n of
            999 -> do v <- decodeFieldMessage; loop m {eroUninterpretedOption = V.snoc (eroUninterpretedOption m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {eroUnknownFields = uf : eroUnknownFields m}

----------------------------------------------------------------------
-- UninterpretedOption (+ NamePart)
----------------------------------------------------------------------

data UninterpretedOption = UninterpretedOption
  { uoName :: !(V.Vector UninterpretedOptionNamePart)
  , uoIdentifierValue :: !Text
  , uoPositiveIntValue :: !Word64
  , uoNegativeIntValue :: !Int64
  , uoDoubleValue :: !Double
  , uoStringValue :: !ByteString
  , uoAggregateValue :: !Text
  , uoUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultUninterpretedOption :: UninterpretedOption
defaultUninterpretedOption = UninterpretedOption V.empty "" 0 0 0 "" "" []

instance MessageEncode UninterpretedOption where
  buildMessage m =
    V.foldl' (\a x -> a <> encodeFieldMessage 2 x) mempty (uoName m)
      <> encStr 3 (uoIdentifierValue m)
      <> (if uoPositiveIntValue m == 0 then mempty else encodeFieldVarint 4 (uoPositiveIntValue m))
      <> (if uoNegativeIntValue m == 0 then mempty else encodeFieldVarint 5 (fromIntegral (uoNegativeIntValue m)))
      <> (if uoDoubleValue m == 0 then mempty else encodeFieldDouble 6 (uoDoubleValue m))
      <> encBytes 7 (uoStringValue m)
      <> encStr 8 (uoAggregateValue m)
      <> encodeUnknownFields (uoUnknownFields m)

instance MessageDecode UninterpretedOption where
  messageDecoder = loop defaultUninterpretedOption
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {uoUnknownFields = reverse (uoUnknownFields m)}
          UJust (Tag n wt) -> case n of
            2 -> do v <- decodeFieldMessage; loop m {uoName = V.snoc (uoName m) v}
            3 -> do v <- decodeFieldString; loop m {uoIdentifierValue = v}
            4 -> do v <- decodeFieldVarint; loop m {uoPositiveIntValue = v}
            5 -> do v <- decodeFieldVarint; loop m {uoNegativeIntValue = fromIntegral v}
            6 -> do v <- decodeFieldDouble; loop m {uoDoubleValue = v}
            7 -> do v <- decodeFieldBytes; loop m {uoStringValue = v}
            8 -> do v <- decodeFieldString; loop m {uoAggregateValue = v}
            _ -> do uf <- captureUnknownField n wt; loop m {uoUnknownFields = uf : uoUnknownFields m}

data UninterpretedOptionNamePart = UninterpretedOptionNamePart
  { uonpNamePart :: !Text
  , uonpIsExtension :: !Bool
  , uonpUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultUninterpretedOptionNamePart :: UninterpretedOptionNamePart
defaultUninterpretedOptionNamePart = UninterpretedOptionNamePart "" False []

instance MessageEncode UninterpretedOptionNamePart where
  buildMessage m =
    encStr 1 (uonpNamePart m)
      <> encodeFieldBool 2 (uonpIsExtension m)
      <> encodeUnknownFields (uonpUnknownFields m)

instance MessageDecode UninterpretedOptionNamePart where
  messageDecoder = loop defaultUninterpretedOptionNamePart
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {uonpUnknownFields = reverse (uonpUnknownFields m)}
          UJust (Tag n wt) -> case n of
            1 -> do v <- decodeFieldString; loop m {uonpNamePart = v}
            2 -> do v <- decodeFieldBool; loop m {uonpIsExtension = v}
            _ -> do uf <- captureUnknownField n wt; loop m {uonpUnknownFields = uf : uonpUnknownFields m}

----------------------------------------------------------------------
-- SourceCodeInfo / GeneratedCodeInfo
--
-- The packed-repeated int32 @path@ / @span@ side fields are preserved as
-- unknown fields rather than given typed accessors.
----------------------------------------------------------------------

newtype SourceCodeInfo = SourceCodeInfo
  { sciLocation :: V.Vector SourceCodeInfoLocation
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultSourceCodeInfo :: SourceCodeInfo
defaultSourceCodeInfo = SourceCodeInfo V.empty

instance MessageEncode SourceCodeInfo where
  buildMessage m = V.foldl' (\a x -> a <> encodeFieldMessage 1 x) mempty (sciLocation m)

instance MessageDecode SourceCodeInfo where
  messageDecoder = loop defaultSourceCodeInfo
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m
          UJust (Tag 1 _) -> do v <- decodeFieldMessage; loop m {sciLocation = V.snoc (sciLocation m) v}
          UJust (Tag _ wt) -> skipField wt >> loop m

data SourceCodeInfoLocation = SourceCodeInfoLocation
  { scilLeadingComments :: !Text
  , scilTrailingComments :: !Text
  , scilLeadingDetachedComments :: !(V.Vector Text)
  , scilUnknownFields :: ![UnknownField]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

defaultSourceCodeInfoLocation :: SourceCodeInfoLocation
defaultSourceCodeInfoLocation = SourceCodeInfoLocation "" "" V.empty []

instance MessageEncode SourceCodeInfoLocation where
  buildMessage m =
    encStr 3 (scilLeadingComments m)
      <> encStr 4 (scilTrailingComments m)
      <> V.foldl' (\a x -> a <> encodeFieldString 6 x) mempty (scilLeadingDetachedComments m)
      <> encodeUnknownFields (scilUnknownFields m)

instance MessageDecode SourceCodeInfoLocation where
  messageDecoder = loop defaultSourceCodeInfoLocation
    where
      loop !m = do
        mt <- getTagOrU
        case mt of
          UNothing -> pure m {scilUnknownFields = reverse (scilUnknownFields m)}
          UJust (Tag n wt) -> case n of
            3 -> do v <- decodeFieldString; loop m {scilLeadingComments = v}
            4 -> do v <- decodeFieldString; loop m {scilTrailingComments = v}
            6 -> do v <- decodeFieldString; loop m {scilLeadingDetachedComments = V.snoc (scilLeadingDetachedComments m) v}
            _ -> do uf <- captureUnknownField n wt; loop m {scilUnknownFields = uf : scilUnknownFields m}

----------------------------------------------------------------------
-- Encode helpers (omit zero/empty scalars, like proto3 implicit presence)
----------------------------------------------------------------------

encStr :: Int -> Text -> Builder
encStr n t = if t == "" then mempty else encodeFieldString n t
{-# INLINE encStr #-}

encBytes :: Int -> ByteString -> Builder
encBytes n b = if b == "" then mempty else encodeFieldBytes n b
{-# INLINE encBytes #-}

encI32 :: Int -> Int32 -> Builder
encI32 n v = if v == 0 then mempty else encodeFieldVarint n (fromIntegral v)
{-# INLINE encI32 #-}

encMsg :: MessageEncode a => Int -> Maybe a -> Builder
encMsg n = maybe mempty (encodeFieldMessage n)
{-# INLINE encMsg #-}
