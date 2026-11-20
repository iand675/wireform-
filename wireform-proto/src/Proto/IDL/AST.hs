{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Proto file abstract syntax tree.

AST types use the /trees that grow/ pattern: each node is
parameterised by a phase @p@ that determines the annotation
carried at each node via the 'XNode' type family.

Two phases are provided:

* 'Semantic' — no source location info; used by codegen, analysis,
  and all existing consumer code via backward-compatible type aliases
  (@MessageDef = MessageDef' Semantic@, etc.).

* 'Parsed' — carries 'Span' for byte-accurate source reconstruction
  via 'Proto.ExactPrint'.

Supports proto2, proto3, and Editions (2023+) syntax with full coverage of
messages, enums, services, oneofs, maps, extensions, and custom options.
-}
module Proto.IDL.AST (
  -- * Phase types and spans (re-exported from "Proto.IDL.AST.Span")
  Parsed,
  Semantic,
  Span (..),
  SrcSpan (..),
  XNode,
  noSpan,
  mkSpan,

  -- * Top-level
  ProtoFile' (..),
  ProtoFile,
  Syntax (..),
  Edition (..),
  TopLevel' (..),
  TopLevel,
  ImportDef' (..),
  ImportDef,
  ImportModifier (..),

  -- * Messages
  MessageDef' (..),
  MessageDef,
  MessageElement' (..),
  MessageElement,
  FieldDef' (..),
  FieldDef,
  isLegacyGroupField,
  FieldLabel (..),
  FieldType (..),
  ScalarType (..),
  MapField' (..),
  MapField,
  OneofDef' (..),
  OneofDef,
  OneofField' (..),
  OneofField,
  ReservedDef (..),
  ReservedName (..),
  ReservedRange (..),

  -- * Enums
  EnumDef' (..),
  EnumDef,
  EnumValue' (..),
  EnumValue,

  -- * Services
  ServiceDef' (..),
  ServiceDef,
  RpcDef' (..),
  RpcDef,
  StreamQualifier (..),

  -- * Options and annotations
  OptionDef' (..),
  OptionDef,
  OptionName (..),
  OptionNamePart (..),
  Constant (CIdent, CInt, CSpelledInt, CFloat, CString, CBytes, CBool, CAggregate),

  -- * Comments
  Comment (..),

  -- * Extensions
  ExtensionRange (..),
  ExtensionRangeBound (..),

  -- * Field numbers
  FieldNumber (..),

  -- * Edition features
  FeatureSet (..),
  FieldPresenceFeature (..),
  EnumTypeFeature (..),
  RepeatedFieldEncodingFeature (..),
  Utf8ValidationFeature (..),
  MessageEncodingFeature (..),
  JsonFormatFeature (..),
  defaultFeatureSet,
  featuresForEdition,

  -- * Phase conversion
  stripSpans,
  stripTopLevel,
  stripMessage,
  stripEnum,
  stripService,
  stripField,
  stripOption,
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Proto.IDL.AST.Span


-- -----------------------------------------------------------------------
-- Backward-compatible type aliases (Semantic phase)
-- -----------------------------------------------------------------------

-- | A proto file in the 'Semantic' phase (no source location info).
type ProtoFile = ProtoFile' Semantic


-- | A top-level declaration in the 'Semantic' phase.
type TopLevel = TopLevel' Semantic


-- | An import declaration in the 'Semantic' phase.
type ImportDef = ImportDef' Semantic


-- | A message definition in the 'Semantic' phase.
type MessageDef = MessageDef' Semantic


-- | A message body element in the 'Semantic' phase.
type MessageElement = MessageElement' Semantic


-- | A field definition in the 'Semantic' phase.
type FieldDef = FieldDef' Semantic


-- | A map field definition in the 'Semantic' phase.
type MapField = MapField' Semantic


-- | A oneof group definition in the 'Semantic' phase.
type OneofDef = OneofDef' Semantic


-- | A single field within a oneof group, in the 'Semantic' phase.
type OneofField = OneofField' Semantic


-- | An enum definition in the 'Semantic' phase.
type EnumDef = EnumDef' Semantic


-- | An enum value in the 'Semantic' phase.
type EnumValue = EnumValue' Semantic


-- | A service definition in the 'Semantic' phase.
type ServiceDef = ServiceDef' Semantic


-- | An RPC method definition in the 'Semantic' phase.
type RpcDef = RpcDef' Semantic


-- | An option definition in the 'Semantic' phase.
type OptionDef = OptionDef' Semantic


-- -----------------------------------------------------------------------
-- Top-level
-- -----------------------------------------------------------------------

-- | A complete .proto file.
data ProtoFile' p = ProtoFile
  { protoSyntax :: !Syntax
  -- ^ The syntax or edition declaration.
  , protoPackage :: !(Maybe Text)
  -- ^ The package name, if declared.
  , protoImports :: ![ImportDef' p]
  -- ^ Import declarations.
  , protoOptions :: ![OptionDef' p]
  -- ^ File-level options.
  , protoTopLevels :: ![TopLevel' p]
  -- ^ Top-level definitions (messages, enums, services, etc.).
  , protoSource :: !(Maybe Text)
  -- ^ Original source text (metadata, not used in equality).
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (ProtoFile' p)


instance NFData (XNode p) => NFData (ProtoFile' p)


-- | Equality ignores 'protoSource' (it's metadata, not semantics).
instance Eq (XNode p) => Eq (ProtoFile' p) where
  a == b =
    protoSyntax a == protoSyntax b
      && protoPackage a == protoPackage b
      && protoImports a == protoImports b
      && protoOptions a == protoOptions b
      && protoTopLevels a == protoTopLevels b


-- | The syntax version or edition of a proto file.
data Syntax
  = -- | Proto2 syntax.
    Proto2
  | -- | Proto3 syntax.
    Proto3
  | -- | Editions syntax with a specific edition identifier.
    Editions !Edition
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Protobuf edition identifier (e.g. "2023", "2024").
newtype Edition = Edition {editionName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | An import declaration in a proto file.
data ImportDef' p = ImportDef
  { importExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , importModifier :: !(Maybe ImportModifier)
  -- ^ Optional import modifier (public or weak).
  , importPath :: !Text
  -- ^ The imported file path.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (ImportDef' p)


deriving stock instance Eq (XNode p) => Eq (ImportDef' p)


instance NFData (XNode p) => NFData (ImportDef' p)


-- | Modifier on an import declaration.
data ImportModifier
  = -- | @import public@ — re-exports the imported definitions.
    ImportPublic
  | -- | @import weak@ — the import is optional.
    ImportWeak
  | -- | @import option@ — makes custom options available without importing types.
    ImportOption
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A comment in proto source.
data Comment
  = -- | @// content@ (without the @//@ prefix)
    LineComment !Text
  | -- | @/* content */@ (without delimiters)
    BlockComment !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A top-level declaration in a proto file.
data TopLevel' p
  = -- | A message definition.
    TLMessage !(MessageDef' p)
  | -- | An enum definition.
    TLEnum !(EnumDef' p)
  | -- | A service definition.
    TLService !(ServiceDef' p)
  | -- | An @extend@ block with the extended type name and additional fields.
    TLExtend !Text ![FieldDef' p]
  | -- | A file-level option.
    TLOption !(OptionDef' p)
  | -- | Standalone comment block between definitions.
    TLComment ![Comment]
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (TopLevel' p)


deriving stock instance Eq (XNode p) => Eq (TopLevel' p)


instance NFData (XNode p) => NFData (TopLevel' p)


-- -----------------------------------------------------------------------
-- Messages
-- -----------------------------------------------------------------------

-- | A protobuf message definition.
data MessageDef' p = MessageDef
  { msgExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , msgDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the message.
  , msgName :: !Text
  -- ^ The message name.
  , msgElements :: ![MessageElement' p]
  -- ^ The body elements (fields, nested types, options, etc.).
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (MessageDef' p)


deriving stock instance Eq (XNode p) => Eq (MessageDef' p)


instance NFData (XNode p) => NFData (MessageDef' p)


-- | An element inside a message body.
data MessageElement' p
  = -- | A regular field.
    MEField !(FieldDef' p)
  | -- | A nested enum definition.
    MEEnum !(EnumDef' p)
  | -- | A nested message definition.
    MEMessage !(MessageDef' p)
  | -- | A oneof group.
    MEOneof !(OneofDef' p)
  | -- | A map field.
    MEMapField !(MapField' p)
  | -- | A reserved declaration.
    MEReserved !ReservedDef
  | {- | An extensions range declaration, with any trailing options
    (e.g. @extensions 4 to 8 [verification = UNVERIFIED];@ or an
    editions @[declaration = {…}]@ list).
    -}
    MEExtensions ![ExtensionRange] ![OptionDef' p]
  | {- | A nested @extend@ block (proto2) declaring extension fields on
    another type, with the extended type name and the additional fields.
    -}
    MEExtend !Text ![FieldDef' p]
  | -- | A message-level option.
    MEOption !(OptionDef' p)
  | -- | Standalone comment inside a message body.
    MEComment ![Comment]
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (MessageElement' p)


deriving stock instance Eq (XNode p) => Eq (MessageElement' p)


instance NFData (XNode p) => NFData (MessageElement' p)


-- | A field definition within a message.
data FieldDef' p = FieldDef
  { fieldExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , fieldDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the field.
  , fieldLabel :: !(Maybe FieldLabel)
  -- ^ The field label (optional, required, or repeated).
  , fieldType :: !FieldType
  -- ^ The field's type.
  , fieldName :: !Text
  -- ^ The field name.
  , fieldNumber :: !FieldNumber
  -- ^ The field number.
  , fieldOptions :: ![OptionDef' p]
  -- ^ Inline field options (e.g. @[packed = true]@).
  }
  | GroupFieldDef
      { fieldExt :: !(XNode p)
      , fieldDoc :: !(Maybe Text)
      , fieldLabel :: !(Maybe FieldLabel)
      , fieldType :: !FieldType
      , fieldName :: !Text
      , fieldNumber :: !FieldNumber
      , fieldOptions :: ![OptionDef' p]
      }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (FieldDef' p)


deriving stock instance Eq (XNode p) => Eq (FieldDef' p)


instance NFData (XNode p) => NFData (FieldDef' p)


-- | A proto2 group names a hoisted message but has delimited wire encoding.
isLegacyGroupField :: FieldDef' p -> Bool
isLegacyGroupField GroupFieldDef {} = True
isLegacyGroupField FieldDef {} = False


-- | A protobuf field number (1-536870911).
newtype FieldNumber = FieldNumber
  { unFieldNumber :: Int
  -- ^ Unwrap the field number.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | The label on a proto field.
data FieldLabel
  = -- | An optional field (proto2) or singular field (proto3).
    Optional
  | -- | A required field (proto2 only).
    Required
  | -- | A repeated (list) field.
    Repeated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | The type of a proto field.
data FieldType
  = -- | A built-in scalar type.
    FTScalar !ScalarType
  | -- | A named message or enum type (may be fully qualified).
    FTNamed !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Built-in protobuf scalar types.
data ScalarType
  = SDouble
  | SFloat
  | SInt32
  | SInt64
  | SUInt32
  | SUInt64
  | SSInt32
  | SSInt64
  | SFixed32
  | SFixed64
  | SSFixed32
  | SSFixed64
  | SBool
  | SString
  | SBytes
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)


-- | A map field definition (@map\<K, V\>@).
data MapField' p = MapField
  { mapExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , mapDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the map field.
  , mapKeyType :: !ScalarType
  -- ^ The map key type (must be a scalar).
  , mapValueType :: !FieldType
  -- ^ The map value type.
  , mapFieldName :: !Text
  -- ^ The field name.
  , mapFieldNum :: !FieldNumber
  -- ^ The field number.
  , mapOptions :: ![OptionDef' p]
  -- ^ Inline field options.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (MapField' p)


deriving stock instance Eq (XNode p) => Eq (MapField' p)


instance NFData (XNode p) => NFData (MapField' p)


-- | A oneof group definition.
data OneofDef' p = OneofDef
  { oneofExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , oneofDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the oneof.
  , oneofName :: !Text
  -- ^ The oneof group name.
  , oneofFields :: ![OneofField' p]
  -- ^ The fields belonging to this oneof.
  , oneofOptions :: ![OptionDef' p]
  -- ^ Options declared inside the oneof block.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (OneofDef' p)


deriving stock instance Eq (XNode p) => Eq (OneofDef' p)


instance NFData (XNode p) => NFData (OneofDef' p)


-- | A single field within a oneof group.
data OneofField' p = OneofField
  { oneofFieldExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , oneofFieldDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the field.
  , oneofFieldType :: !FieldType
  -- ^ The field's type.
  , oneofFieldName :: !Text
  -- ^ The field name.
  , oneofFieldNumber :: !FieldNumber
  -- ^ The field number.
  , oneofFieldOptions :: ![OptionDef' p]
  -- ^ Inline field options.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (OneofField' p)


deriving stock instance Eq (XNode p) => Eq (OneofField' p)


instance NFData (XNode p) => NFData (OneofField' p)


-- | A @reserved@ declaration inside a message or enum.
data ReservedDef
  = -- | Reserved field number ranges.
    ReservedNumbers ![ReservedRange]
  | -- | Reserved field names.
    ReservedNames ![ReservedName]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | A single name in a @reserved@ declaration.

proto2/proto3 spell reserved names as quoted string literals; editions
(2023+) spell them as bare identifiers. The spelling is retained so that
printing reproduces the correct token for the file's syntax.
-}
data ReservedName
  = -- | A quoted reserved name, e.g. @"foo"@ (proto2/proto3).
    QuotedReservedName !Text
  | -- | A bare-identifier reserved name, e.g. @foo@ (editions 2023+).
    IdentReservedName !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A range within a @reserved@ declaration.
data ReservedRange
  = -- | A single reserved field number.
    ReservedSingle !Int
  | -- | An inclusive range of reserved field numbers.
    ReservedRange !Int !Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Enums
-- -----------------------------------------------------------------------

-- | A protobuf enum definition.
data EnumDef' p = EnumDef
  { enumExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , enumDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the enum.
  , enumName :: !Text
  -- ^ The enum name.
  , enumValues :: ![EnumValue' p]
  -- ^ The enum value entries.
  , enumOptions :: ![OptionDef' p]
  -- ^ Enum-level options (e.g. @allow_alias@).
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (EnumDef' p)


deriving stock instance Eq (XNode p) => Eq (EnumDef' p)


instance NFData (XNode p) => NFData (EnumDef' p)


-- | A single value within an enum definition.
data EnumValue' p = EnumValue
  { evExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , evDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the value.
  , evName :: !Text
  -- ^ The enum value name.
  , evNumber :: !Int
  -- ^ The numeric value.
  , evOptions :: ![OptionDef' p]
  -- ^ Inline options on the enum value.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (EnumValue' p)


deriving stock instance Eq (XNode p) => Eq (EnumValue' p)


instance NFData (XNode p) => NFData (EnumValue' p)


-- -----------------------------------------------------------------------
-- Services
-- -----------------------------------------------------------------------

-- | A protobuf service definition.
data ServiceDef' p = ServiceDef
  { svcExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , svcDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the service.
  , svcName :: !Text
  -- ^ The service name.
  , svcRpcs :: ![RpcDef' p]
  -- ^ The RPC method definitions.
  , svcOptions :: ![OptionDef' p]
  -- ^ Service-level options.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (ServiceDef' p)


deriving stock instance Eq (XNode p) => Eq (ServiceDef' p)


instance NFData (XNode p) => NFData (ServiceDef' p)


-- | An RPC method definition within a service.
data RpcDef' p = RpcDef
  { rpcExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , rpcDoc :: !(Maybe Text)
  -- ^ Documentation comment attached to the RPC.
  , rpcName :: !Text
  -- ^ The method name.
  , rpcInput :: !Text
  -- ^ The request message type name.
  , rpcInputStr :: !StreamQualifier
  -- ^ Whether the request is streaming.
  , rpcOutput :: !Text
  -- ^ The response message type name.
  , rpcOutputStr :: !StreamQualifier
  -- ^ Whether the response is streaming.
  , rpcOptions :: ![OptionDef' p]
  -- ^ Method-level options.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (RpcDef' p)


deriving stock instance Eq (XNode p) => Eq (RpcDef' p)


instance NFData (XNode p) => NFData (RpcDef' p)


-- | Whether an RPC input or output is streaming.
data StreamQualifier
  = -- | Unary (non-streaming).
    NoStream
  | -- | Server-side or client-side streaming.
    Streaming
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Options
-- -----------------------------------------------------------------------

-- | An option (including custom options with extension names).
data OptionDef' p = OptionDef
  { optExt :: !(XNode p)
  -- ^ Phase-specific annotation (e.g. source span).
  , optName :: !OptionName
  -- ^ The option name (simple or extension).
  , optValue :: !Constant
  -- ^ The option value.
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (OptionDef' p)


deriving stock instance Eq (XNode p) => Eq (OptionDef' p)


instance NFData (XNode p) => NFData (OptionDef' p)


{- | An option name can be a simple identifier or a parenthesized extension name,
optionally followed by dotted sub-field access.
-}
newtype OptionName = OptionName
  { optNameParts :: [OptionNamePart]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A single component of an option name path.
data OptionNamePart
  = -- | A simple option name (e.g. @deprecated@).
    SimpleOption !Text
  | -- | A parenthesized extension option name (e.g. @(my_option)@).
    ExtensionOption !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A constant value used in option assignments and default values.
data Constant
  = -- | An identifier constant (e.g. an enum value name).
    CIdent !Text
  | CInteger !Integer
  | -- | An integer whose spelling matters in TextFormat, such as @-0@ or @0xff@.
    CSpelledInt !Text !Integer
  | -- | A floating-point constant.
    CFloat !Double
  | -- | A string constant.
    CString !Text
  | -- | A bytes default or a quoted option containing non-UTF-8 bytes.
    CBytes !ByteString
  | -- | A boolean constant.
    CBool !Bool
  | -- | An aggregate (braced key-value) constant.
    CAggregate ![(Text, Constant)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | An integer constant, regardless of its original spelling.
pattern CInt :: Integer -> Constant
pattern CInt value <- (integerValue -> Just value)
  where CInt value = CInteger value


{-# COMPLETE CIdent, CInt, CFloat, CString, CBytes, CBool, CAggregate #-}


integerValue :: Constant -> Maybe Integer
integerValue (CInteger value) = Just value
integerValue (CSpelledInt _ value) = Just value
integerValue _ = Nothing


-- -----------------------------------------------------------------------
-- Extensions
-- -----------------------------------------------------------------------

-- | A range of field numbers reserved for extensions.
data ExtensionRange = ExtensionRange
  { extStart :: !Int
  -- ^ The start of the extension range (inclusive).
  , extEnd :: !ExtensionRangeBound
  -- ^ The end of the extension range.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | The upper bound of an extension range.
data ExtensionRangeBound
  = -- | A specific field number upper bound (inclusive).
    ExtBoundNum !Int
  | -- | The @max@ keyword, meaning the maximum valid field number.
    ExtBoundMax
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Edition features
-- -----------------------------------------------------------------------

-- | The set of edition features controlling proto semantics.
data FeatureSet = FeatureSet
  { featureFieldPresence :: !FieldPresenceFeature
  -- ^ How field presence is tracked.
  , featureEnumType :: !EnumTypeFeature
  -- ^ Whether enums are open or closed.
  , featureRepeatedFieldEncoding :: !RepeatedFieldEncodingFeature
  -- ^ Whether repeated scalars use packed encoding.
  , featureUtf8Validation :: !Utf8ValidationFeature
  -- ^ Whether string fields are validated as UTF-8.
  , featureMessageEncoding :: !MessageEncodingFeature
  -- ^ How submessages are encoded on the wire.
  , featureJsonFormat :: !JsonFormatFeature
  -- ^ JSON format handling for this edition.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Controls how field presence is tracked for singular fields.
data FieldPresenceFeature
  = -- | Field tracks presence explicitly (has-bit or optional wrapper).
    ExplicitPresence
  | -- | Field uses implicit presence (zero value means absent).
    ImplicitPresence
  | -- | Legacy required semantics (proto2 compatibility).
    LegacyRequired
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Controls whether an enum is open or closed.
data EnumTypeFeature
  = -- | Open enum: unknown values are preserved.
    OpenEnum
  | -- | Closed enum: unknown values are rejected.
    ClosedEnum
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Controls wire encoding of repeated scalar fields.
data RepeatedFieldEncodingFeature
  = -- | Packed encoding (all values in a single length-delimited chunk).
    PackedEncoding
  | -- | Expanded encoding (one tag-value pair per element).
    ExpandedEncoding
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Controls UTF-8 validation of string fields.
data Utf8ValidationFeature
  = -- | Verify that string fields contain valid UTF-8.
    Utf8Verify
  | -- | Skip UTF-8 validation.
    Utf8None
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Controls submessage wire encoding.
data MessageEncodingFeature
  = -- | Standard length-prefixed encoding.
    LengthPrefixedEncoding
  | -- | Group-style delimited encoding.
    DelimitedEncoding
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Controls JSON format handling.
data JsonFormatFeature
  = -- | Full canonical JSON support.
    JsonAllow
  | -- | Legacy best-effort JSON (for older proto2 compatibility).
    JsonLegacyBestEffort
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Default feature set (matches proto3 defaults, which is edition 2023 default).
defaultFeatureSet :: FeatureSet
defaultFeatureSet =
  FeatureSet
    { featureFieldPresence = ExplicitPresence
    , featureEnumType = OpenEnum
    , featureRepeatedFieldEncoding = PackedEncoding
    , featureUtf8Validation = Utf8Verify
    , featureMessageEncoding = LengthPrefixedEncoding
    , featureJsonFormat = JsonAllow
    }


-- | Get the default feature set for a given edition.
featuresForEdition :: Edition -> FeatureSet
featuresForEdition (Edition "2023") = defaultFeatureSet
featuresForEdition (Edition "2024") =
  defaultFeatureSet
    { featureFieldPresence = ExplicitPresence
    }
featuresForEdition _ = defaultFeatureSet


-- -----------------------------------------------------------------------
-- Phase conversion
-- -----------------------------------------------------------------------

-- | Strip all span annotations, converting any phase to 'Semantic'.
stripSpans :: ProtoFile' p -> ProtoFile
stripSpans pf =
  ProtoFile
    { protoSyntax = protoSyntax pf
    , protoPackage = protoPackage pf
    , protoImports = fmap stripImport (protoImports pf)
    , protoOptions = fmap stripOption (protoOptions pf)
    , protoTopLevels = fmap stripTopLevel (protoTopLevels pf)
    , protoSource = protoSource pf
    }


-- | Strip span annotations from a single top-level declaration.
stripTopLevel :: TopLevel' p -> TopLevel
stripTopLevel = \case
  TLMessage m -> TLMessage (stripMessage m)
  TLEnum e -> TLEnum (stripEnum e)
  TLService s -> TLService (stripService s)
  TLExtend n fs -> TLExtend n (fmap stripField fs)
  TLOption o -> TLOption (stripOption o)
  TLComment cs -> TLComment cs


stripImport :: ImportDef' p -> ImportDef
stripImport i = ImportDef {importExt = (), importModifier = importModifier i, importPath = importPath i}


-- | Strip span annotations from a message definition.
stripMessage :: MessageDef' p -> MessageDef
stripMessage m =
  MessageDef
    { msgExt = ()
    , msgDoc = msgDoc m
    , msgName = msgName m
    , msgElements = fmap stripMsgElem (msgElements m)
    }


stripMsgElem :: MessageElement' p -> MessageElement
stripMsgElem = \case
  MEField f -> MEField (stripField f)
  MEEnum e -> MEEnum (stripEnum e)
  MEMessage m -> MEMessage (stripMessage m)
  MEOneof o -> MEOneof (stripOneof o)
  MEMapField mf -> MEMapField (stripMapField mf)
  MEReserved r -> MEReserved r
  MEExtensions e opts -> MEExtensions e (fmap stripOption opts)
  MEExtend n fs -> MEExtend n (fmap stripField fs)
  MEOption o -> MEOption (stripOption o)
  MEComment cs -> MEComment cs


-- | Strip span annotations from a field definition.
stripField :: FieldDef' p -> FieldDef
stripField f =
  constructor () (fieldDoc f) (fieldLabel f) (fieldType f) (fieldName f)
    (fieldNumber f) (fmap stripOption (fieldOptions f))
  where
    constructor = if isLegacyGroupField f then GroupFieldDef else FieldDef


-- | Strip span annotations from an enum definition.
stripEnum :: EnumDef' p -> EnumDef
stripEnum e =
  EnumDef
    { enumExt = ()
    , enumDoc = enumDoc e
    , enumName = enumName e
    , enumValues = fmap stripEnumValue (enumValues e)
    , enumOptions = fmap stripOption (enumOptions e)
    }


stripEnumValue :: EnumValue' p -> EnumValue
stripEnumValue v =
  EnumValue
    { evExt = ()
    , evDoc = evDoc v
    , evName = evName v
    , evNumber = evNumber v
    , evOptions = fmap stripOption (evOptions v)
    }


-- | Strip span annotations from a service definition.
stripService :: ServiceDef' p -> ServiceDef
stripService s =
  ServiceDef
    { svcExt = ()
    , svcDoc = svcDoc s
    , svcName = svcName s
    , svcRpcs = fmap stripRpc (svcRpcs s)
    , svcOptions = fmap stripOption (svcOptions s)
    }


stripRpc :: RpcDef' p -> RpcDef
stripRpc r =
  RpcDef
    { rpcExt = ()
    , rpcDoc = rpcDoc r
    , rpcName = rpcName r
    , rpcInput = rpcInput r
    , rpcInputStr = rpcInputStr r
    , rpcOutput = rpcOutput r
    , rpcOutputStr = rpcOutputStr r
    , rpcOptions = fmap stripOption (rpcOptions r)
    }


stripOneof :: OneofDef' p -> OneofDef
stripOneof o =
  OneofDef
    { oneofExt = ()
    , oneofDoc = oneofDoc o
    , oneofName = oneofName o
    , oneofFields = fmap stripOneofField (oneofFields o)
    , oneofOptions = fmap stripOption (oneofOptions o)
    }


stripOneofField :: OneofField' p -> OneofField
stripOneofField f =
  OneofField
    { oneofFieldExt = ()
    , oneofFieldDoc = oneofFieldDoc f
    , oneofFieldType = oneofFieldType f
    , oneofFieldName = oneofFieldName f
    , oneofFieldNumber = oneofFieldNumber f
    , oneofFieldOptions = fmap stripOption (oneofFieldOptions f)
    }


stripMapField :: MapField' p -> MapField
stripMapField m =
  MapField
    { mapExt = ()
    , mapDoc = mapDoc m
    , mapKeyType = mapKeyType m
    , mapValueType = mapValueType m
    , mapFieldName = mapFieldName m
    , mapFieldNum = mapFieldNum m
    , mapOptions = fmap stripOption (mapOptions m)
    }


-- | Strip span annotations from an option definition.
stripOption :: OptionDef' p -> OptionDef
stripOption o = OptionDef {optExt = (), optName = optName o, optValue = optValue o}
