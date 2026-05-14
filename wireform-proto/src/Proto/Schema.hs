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

  -- * Field access
  HasField (..),

  -- * Field descriptors
  FieldDescriptor (..),
  FieldTypeDescriptor (..),
  ScalarFieldType (..),
  FieldLabel' (..),
  FieldAccessor (..),
  SomeFieldDescriptor (..),

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

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (Symbol)


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
  protoFieldDescriptors :: Proxy a -> IntMap (SomeFieldDescriptor a)


  -- | The raw serialized FileDescriptorProto bytes.
  -- Can be fed to other proto tools for interop.
  protoFileDescriptorBytes :: Proxy a -> ByteString
  protoFileDescriptorBytes _ = ""


  -- | Default value with all fields at their proto default.
  protoDefaultValue :: a


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


-- | Proto field type descriptor.
data FieldTypeDescriptor
  = ScalarType !ScalarFieldType
  | -- | Fully-qualified message type name
    MessageType !Text
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
  let descs = IntMap.elems (protoFieldDescriptors p)
  in case filter (\(SomeField fd) -> fdName fd == name) descs of
      (d : _) -> Just d
      [] -> Nothing


-- | Look up a field descriptor by field number.
fieldDescriptorByNumber :: ProtoMessage a => Int -> Proxy a -> Maybe (SomeFieldDescriptor a)
fieldDescriptorByNumber num p = IntMap.lookup num (protoFieldDescriptors p)


-- | All field names in a message.
messageFieldNames :: ProtoMessage a => Proxy a -> [Text]
messageFieldNames p = fmap (\(SomeField fd) -> fdName fd) (IntMap.elems (protoFieldDescriptors p))


-- | All field numbers in a message.
messageFieldNumbers :: ProtoMessage a => Proxy a -> [Int]
messageFieldNumbers p = IntMap.keys (protoFieldDescriptors p)
