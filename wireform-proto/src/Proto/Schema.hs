{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Schema metadata system.

Retains full proto schema information at the type level and term level.

Each generated message carries:

* Its fully-qualified proto name
* Its package name
* A list of field descriptors (name, number, type, label)
* The raw file descriptor bytes (for interop with other proto tools)
* A default value

Each field is accessible via the 'HasField' typeclass, which provides
a getter, setter, and field descriptor.
-}
module Proto.Schema (
  -- * Message metadata
  ProtoMessage (..),
  SomeProtoMessage (..),

  -- * Field access
  HasField (..),

  -- * Field descriptors
  FieldDescriptor (..),
  FieldTypeDescriptor (..),
  ScalarFieldType (..),
  FieldLabel' (..),
  FieldAccessor (..),
  SomeFieldDescriptor (..),
  OneofDescriptor (..),
  OneofFieldDescriptor (..),

  -- * Enum metadata
  ProtoEnum (..),

  -- * Service metadata
  ProtoService (..),
  MethodDescriptor (..),

  -- * Querying
  lookupFieldDescriptor,
  fieldDescriptorByNumber,
  messageFieldNames,
  messageFieldNumbers,
) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Proto.Internal.Wire.Decode (Decoder)


{- | Typeclass carrying full proto schema metadata for a message type.

Generated code provides instances that make all schema information
available at runtime without parsing .proto files.
-}
class ProtoMessage a where
  -- | Fully-qualified proto message name (e.g. @"example.Person"@).
  protoMessageName :: Proxy a -> Text


  -- | The proto package this message belongs to.
  protoPackageName :: Proxy a -> Text
  protoPackageName _ = ""


  -- | All field descriptors, keyed by field number.
  protoFieldDescriptors :: Proxy a -> Map Int (SomeFieldDescriptor a)


  -- | Every wire member of each oneof, including its declared wire type.
  protoOneofDescriptors :: Proxy a -> [OneofDescriptor]
  protoOneofDescriptors _ = []

  -- | The recognized numbers for ordinary closed-enum fields, including
  -- enum-valued maps. Open enums and oneof members are omitted.
  protoClosedEnumFields :: Proxy a -> Map Int [Int]
  protoClosedEnumFields _ = Map.empty

  -- | Schemas for message and group fields, keyed by wire field number.
  protoSubmessageSchemas :: Proxy a -> Map Int SomeProtoMessage
  protoSubmessageSchemas _ = Map.empty

  -- | Validators for the message payloads in map values, keyed by map field
  -- number. An entry's keys and value framing are handled separately.
  protoMapValueDecoders :: Proxy a -> Map Int (Decoder ())
  protoMapValueDecoders _ = Map.empty


  {- | The raw serialized FileDescriptorProto bytes.
  Can be fed to other proto tools for interop.
  -}
  protoFileDescriptorBytes :: Proxy a -> ByteString
  protoFileDescriptorBytes _ = ""


  -- | Default value with all fields at their proto default.
  protoDefaultValue :: a


  -- | Prepare typed ProtoJSON for parsing in ignore-unknown-enum-names mode.
  protoJSONIgnoreUnknownEnumNames :: Proxy a -> Value -> Value
  protoJSONIgnoreUnknownEnumNames _ = id


-- | A message schema whose Haskell type is selected at runtime.
data SomeProtoMessage where
  SomeProtoMessage :: ProtoMessage a => Proxy a -> SomeProtoMessage


{- | Type-safe field access via plain get\/set functions.

@HasField msg "fieldName" fieldType@ means the message @msg@ has a
field named @"fieldName"@ with Haskell type @fieldType@. For lens-style
access, see "Proto.Lens".
-}
class HasField (msg :: Type) (name :: Symbol) (a :: Type) | msg name -> a where
  -- | Get the field value.
  getField :: msg -> a


  -- | Set the field value, returning a new message.
  setField :: a -> msg -> msg


  -- | The field descriptor for this field.
  fieldDescriptor :: Proxy msg -> Proxy name -> FieldDescriptor msg a


-- | A field descriptor carrying the proto metadata for one field.
data FieldDescriptor msg a = FieldDescriptor
  { fdName :: !Text
  -- ^ Proto field name (snake_case)
  , fdNumber :: !Int
  -- ^ Proto field number
  , fdTypeDesc :: !FieldTypeDescriptor
  , fdLabel :: !FieldLabel'
  , fdGet :: msg -> a
  -- ^ Accessor function
  , fdSet :: a -> msg -> msg
  -- ^ Setter function
  }


-- | Existentially-wrapped field descriptor for heterogeneous collections.
data SomeFieldDescriptor msg where
  SomeField :: FieldDescriptor msg a -> SomeFieldDescriptor msg


data OneofDescriptor = OneofDescriptor
  { odName :: !Text
  , odFields :: ![OneofFieldDescriptor]
  }
  deriving stock (Show, Eq)


data OneofFieldDescriptor = OneofFieldDescriptor
  { ofdName :: !Text
  , ofdNumber :: {-# UNPACK #-} !Int
  , ofdType :: !FieldTypeDescriptor
  , ofdEnumValues :: !(Maybe [Int])
  -- ^ Allowed values for a closed enum. Open enums use 'Nothing'.
  }
  deriving stock (Show, Eq)


-- | Proto field type descriptor.
data FieldTypeDescriptor
  = ScalarType !ScalarFieldType
  | -- | Fully-qualified message type name
    MessageType !Text
  | -- | Message encoded between matching group tags.
    GroupType !Text
  | -- | Fully-qualified enum type name
    EnumType !Text
  | -- | Map<key, value>
    MapType !ScalarFieldType !FieldTypeDescriptor
  deriving stock (Show, Eq)


-- | Scalar field types matching the proto spec.
data ScalarFieldType
  = DoubleField
  | FloatField
  | Int32Field
  | Int64Field
  | UInt32Field
  | UInt64Field
  | SInt32Field
  | SInt64Field
  | Fixed32Field
  | Fixed64Field
  | SFixed32Field
  | SFixed64Field
  | BoolField
  | StringField
  | BytesField
  deriving stock (Show, Eq, Ord, Enum, Bounded)


-- | Field label (cardinality).
data FieldLabel'
  = LabelOptional
  | LabelRequired
  | LabelRepeated
  deriving stock (Show, Eq, Ord)


-- | How a field is accessed in the record.
data FieldAccessor
  = -- | Direct record field
    PlainField
  | -- | Wrapped in Maybe
    OptionalField
  | -- | Wrapped in Vector
    RepeatedField
  | -- | Wrapped in Map
    MapField'
  deriving stock (Show, Eq, Ord)


-- | Full metadata for a proto enum type.
class ProtoEnum a where
  -- | Fully-qualified proto enum name.
  protoEnumName :: Proxy a -> Text


  -- | All enum value names and their numeric values.
  protoEnumValues :: Proxy a -> [(Text, Int)]


  -- | Convert from proto numeric value.
  fromProtoEnumValue :: Int -> Maybe a


  -- | Whether the enum accepts unrecognized wire values.
  protoEnumIsOpen :: Proxy a -> Bool
  protoEnumIsOpen _ = True


  -- | Convert to proto numeric value.
  toProtoEnumValue :: a -> Int


-- | Metadata for a proto service.
class ProtoService a where
  protoServiceName :: Proxy a -> Text
  protoServiceMethods :: Proxy a -> [MethodDescriptor]


-- | Metadata for a single RPC method.
data MethodDescriptor = MethodDescriptor
  { mdName :: !Text
  , mdInputType :: !Text
  , mdOutputType :: !Text
  , mdClientStreaming :: !Bool
  , mdServerStreaming :: !Bool
  }
  deriving stock (Show, Eq)


-- | Look up a field descriptor by name.
lookupFieldDescriptor :: ProtoMessage a => Text -> Proxy a -> Maybe (SomeFieldDescriptor a)
lookupFieldDescriptor name p =
  let descs = Map.elems (protoFieldDescriptors p)
  in case filter (\(SomeField fd) -> fdName fd == name) descs of
       (d : _) -> Just d
       [] -> Nothing


-- | Look up a field descriptor by field number.
fieldDescriptorByNumber :: ProtoMessage a => Int -> Proxy a -> Maybe (SomeFieldDescriptor a)
fieldDescriptorByNumber num p = Map.lookup num (protoFieldDescriptors p)


-- | All field names in a message.
messageFieldNames :: ProtoMessage a => Proxy a -> [Text]
messageFieldNames p = fmap (\(SomeField fd) -> fdName fd) (Map.elems (protoFieldDescriptors p))


-- | All field numbers in a message.
messageFieldNumbers :: ProtoMessage a => Proxy a -> [Int]
messageFieldNumbers p = Map.keys (protoFieldDescriptors p)
