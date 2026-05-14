{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Registry for proto2 extension JSON helpers.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

Proto2 lets messages declare extension ranges:

@
message TestAllTypesProto2 \{
  extensions 120 to 200;
}
extend TestAllTypesProto2 \{
  optional int32 extension_int32 = 120;
}
@

The proto3 canonical JSON for an extension targets it via
a bracket-quoted fully-qualified name:

@
{\"[protobuf_test_messages.proto2.extension_int32]\": 1}
@

Generated code builds an 'ExtensionRegistry' purely via
'registerExtensionJson' and combines registries with @(\<\>)@.
JSON encode\/decode functions accept the registry explicitly.
-}
module Proto.Internal.JSON.Extension (
  ExtJsonCodec (..),
  ExtensionRegistry,
  emptyExtensionRegistry,
  registerExtensionJson,
  lookupExtensionByFqn,
  lookupExtensionByNumber,
  parentHasExtensions,
  extensionEntriesForJson,
  parseExtensionEntry,

  -- * Per-extension codec primitives (used by the

  -- 'loadProto'-generated registration code)
  parseExtValueViaConstructor,
  encodeExtValueViaConstructor,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.Types qualified as AesonT
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Proto.Internal.Decode (UnknownField (..))
import Proto.Extension (
  ExtensionType (..),
  decodeExtensionValue,
  encodeExtensionValue,
  unknownFieldNumber,
 )
import Proto.Internal.JSON qualified as PJ


{- | Describes how to bridge a single proto2 extension between
its JSON value form and the wire 'UnknownField' representation
the message stores under its unknown-fields slot.
-}
data ExtJsonCodec = ExtJsonCodec
  { ejcExtensionFqn :: !Text
  -- ^ Fully-qualified proto name of the extension (the
  --   bracket-quoted JSON key without the brackets).
  , ejcFieldNumber :: !Int
  , ejcParseValue :: Aeson.Value -> Either String UnknownField
  , ejcEncodeValue :: UnknownField -> Either String Aeson.Value
  }


{- | One per parent message: maps both extension FQN (for parse)
and field number (for output) to the codec.
-}
data ExtRegistryEntry = ExtRegistryEntry
  { byFqn :: !(Map Text ExtJsonCodec)
  , byNum :: !(Map Int ExtJsonCodec)
  }


{- | Registry for proto2 extension JSON codecs.
Built at module load time by generated code, passed explicitly
to JSON encode/decode functions.
-}
newtype ExtensionRegistry = ExtensionRegistry
  { erEntries :: Map Text ExtRegistryEntry
  }


mergeEntry :: ExtRegistryEntry -> ExtRegistryEntry -> ExtRegistryEntry
mergeEntry a b =
  ExtRegistryEntry
    { byFqn = Map.union (byFqn a) (byFqn b)
    , byNum = Map.union (byNum a) (byNum b)
    }


instance Semigroup ExtensionRegistry where
  a <> b = ExtensionRegistry (Map.unionWith mergeEntry (erEntries a) (erEntries b))


instance Monoid ExtensionRegistry where
  mempty = emptyExtensionRegistry


-- | An empty extension registry with no registered codecs.
emptyExtensionRegistry :: ExtensionRegistry
emptyExtensionRegistry = ExtensionRegistry Map.empty


{- | Register a JSON codec for an extension targeting the named
parent message. Pure function -- builds a single-entry registry
that can be combined with @(\<\>)@.
-}
registerExtensionJson
  :: Text
  -- ^ Parent message FQN.
  -> ExtJsonCodec
  -> ExtensionRegistry
registerExtensionJson parentFqn codec =
  let entry =
        ExtRegistryEntry
          { byFqn = Map.singleton (ejcExtensionFqn codec) codec
          , byNum = Map.singleton (ejcFieldNumber codec) codec
          }
  in ExtensionRegistry (Map.singleton parentFqn entry)


-- | Look up an extension codec by its fully-qualified proto name under a parent message.
lookupExtensionByFqn :: ExtensionRegistry -> Text -> Text -> Maybe ExtJsonCodec
lookupExtensionByFqn reg parentFqn fqn =
  case Map.lookup parentFqn (erEntries reg) of
    Nothing -> Nothing
    Just e -> Map.lookup fqn (byFqn e)


-- | Look up an extension codec by field number under a parent message.
lookupExtensionByNumber :: ExtensionRegistry -> Text -> Int -> Maybe ExtJsonCodec
lookupExtensionByNumber reg parentFqn n =
  case Map.lookup parentFqn (erEntries reg) of
    Nothing -> Nothing
    Just e -> Map.lookup n (byNum e)


{- | Cheap registry membership check. Returns 'True' when the
parent message has at least one registered extension codec.
-}
parentHasExtensions :: ExtensionRegistry -> Text -> Bool
parentHasExtensions reg parentFqn =
  Map.member parentFqn (erEntries reg)
{-# INLINE parentHasExtensions #-}


{- | Translate every registered extension that's present in the
supplied unknown-fields slot into its bracket-quoted JSON
@(key, value)@ pair. Unknown fields not matching a registered
extension stay invisible to JSON output (no schema to bind
them to).

Fast-paths: empty unknown-fields list AND missing-from-
registry both bypass the per-uf walk and allocate nothing.
-}
extensionEntriesForJson
  :: ExtensionRegistry
  -> Text
  -- ^ Parent message FQN.
  -> [UnknownField]
  -> [(Text, Aeson.Value)]
extensionEntriesForJson _ _ [] = []
extensionEntriesForJson reg parentFqn ufs =
  case Map.lookup parentFqn (erEntries reg) of
    Nothing -> []
    Just entry ->
      let go uf = case Map.lookup (unknownFieldNumber uf) (byNum entry) of
            Nothing -> Nothing
            Just codec -> case ejcEncodeValue codec uf of
              Left _ -> Nothing
              Right v ->
                Just
                  ( T.cons '[' (ejcExtensionFqn codec <> T.singleton ']')
                  , v
                  )
      in mapMaybe go ufs
{-# INLINE extensionEntriesForJson #-}


{- | If @key@ has the bracket-quoted form @\"[FQN]\"@, look up
the FQN in the parent's registry and parse @value@ through
the matched codec. Returns 'Just (Right uf)' on success,
'Just (Left e)' when the bracket key is recognised but parsing
fails (so the FromJSON instance can propagate the error), and
'Nothing' when the key isn't bracket-quoted (so the caller
treats it as an ordinary field).
-}
parseExtensionEntry
  :: ExtensionRegistry
  -> Text
  -- ^ Parent message FQN.
  -> AesonKey.Key
  -> Aeson.Value
  -> Maybe (Either String UnknownField)
parseExtensionEntry reg parentFqn key val =
  let t = AesonKey.toText key
  in case T.uncons t of
      Just ('[', rest) -> case T.unsnoc rest of
        Just (fqn, ']') -> case lookupExtensionByFqn reg parentFqn fqn of
          Just codec -> Just (ejcParseValue codec val)
          Nothing -> Nothing -- unrecognised extension; ignore
        _ -> Nothing
      _ -> Nothing
{-# INLINE parseExtensionEntry #-}


-- ---------------------------------------------------------------------------
-- Splice-driven codec primitives
-- ---------------------------------------------------------------------------

{- | Parse a JSON value into the wire 'UnknownField' that
'Proto.Extension.encodeExtensionValue' would have produced
for the same payload, dispatched on the extension's static
'ExtensionType' constructor. This lets the
'loadProto'-generated registration call stay completely
ADT-free at the splice site.
-}
parseExtValueViaConstructor
  :: forall a
   . ExtensionType a
  -> Int
  -> Aeson.Value
  -> Either String UnknownField
parseExtValueViaConstructor ty fn v = case ty of
  ExtBool -> encodeExtensionValue fn ty <$> parseBool v
  ExtInt32 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int32)
  ExtInt64 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int64)
  ExtUInt32 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word32)
  ExtUInt64 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word64)
  ExtSInt32 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int32)
  ExtSInt64 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int64)
  ExtFixed32 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word32)
  ExtFixed64 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Word64)
  ExtSFixed32 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int32)
  ExtSFixed64 -> encodeExtensionValue fn ty <$> (parseBoundedInt v :: Either String Int64)
  ExtFloat -> encodeExtensionValue fn ty <$> parseFloating v
  ExtDouble -> encodeExtensionValue fn ty <$> parseFloatingD v
  ExtString -> encodeExtensionValue fn ty <$> parseStringT v
  ExtBytes -> encodeExtensionValue fn ty <$> parseBytesB v
  ExtMessage ->
    -- Embedded sub-messages aren't exercised by the
    -- conformance suite's bracket-syntax tests; if a user
    -- needs them they can register a custom codec.
    Left "JSON serialisation of message-typed extensions is not yet supported"


parseBool :: Aeson.Value -> Either String Bool
parseBool (Aeson.Bool b) = Right b
parseBool _ = Left "Expected JSON Bool"


parseBoundedInt
  :: forall a
   . (Integral a, Bounded a)
  => Aeson.Value
  -> Either String a
parseBoundedInt v = case v of
  Aeson.Number n -> coerce n
  Aeson.String s -> case reads (T.unpack s) :: [(Sci.Scientific, String)] of
    [(sci, "")] -> coerce sci
    _ -> Left ("Invalid integer string: " <> show s)
  _ -> Left "Expected JSON Number or String for integer extension"
  where
    coerce sci = case Sci.toBoundedInteger sci of
      Just n -> Right n
      Nothing -> Left "Extension integer value out of range or non-integer"


parseFloating :: Aeson.Value -> Either String Float
parseFloating v = case AesonT.parseEither PJ.protoFloatFromJSON v of
  Right d -> Right d
  Left e -> Left e


parseFloatingD :: Aeson.Value -> Either String Double
parseFloatingD v = case AesonT.parseEither PJ.protoDoubleFromJSON v of
  Right d -> Right d
  Left e -> Left e


parseStringT :: Aeson.Value -> Either String Text
parseStringT (Aeson.String s) = Right s
parseStringT _ = Left "Expected JSON String for string extension"


parseBytesB :: Aeson.Value -> Either String BS.ByteString
parseBytesB v = case AesonT.parseEither PJ.protoBytesFromJSON v of
  Right b -> Right b
  Left e -> Left e


{- | Encode a stored 'UnknownField' back into the JSON form for
the matching extension type. Routes through the existing
'decodeExtensionValue' so the payload-decoding rules stay in
one place.
-}
encodeExtValueViaConstructor
  :: ExtensionType a -> UnknownField -> Either String Aeson.Value
encodeExtValueViaConstructor ty uf = case decodeExtensionValue ty uf of
  Nothing -> Left "extension JSON encode: wire-type/extension-type mismatch"
  Just a -> Right (encodeOne ty a)
  where
    encodeOne :: ExtensionType b -> b -> Aeson.Value
    encodeOne ExtBool b = Aeson.Bool b
    encodeOne ExtInt32 n = Aeson.Number (fromIntegral n)
    encodeOne ExtInt64 n = PJ.protoInt64ToJSON n
    encodeOne ExtUInt32 n = Aeson.Number (fromIntegral n)
    encodeOne ExtUInt64 n = PJ.protoWord64ToJSON n
    encodeOne ExtSInt32 n = Aeson.Number (fromIntegral n)
    encodeOne ExtSInt64 n = PJ.protoInt64ToJSON n
    encodeOne ExtFixed32 n = Aeson.Number (fromIntegral n)
    encodeOne ExtFixed64 n = PJ.protoWord64ToJSON n
    encodeOne ExtSFixed32 n = Aeson.Number (fromIntegral n)
    encodeOne ExtSFixed64 n = PJ.protoInt64ToJSON n
    encodeOne ExtFloat f = PJ.protoFloatToJSON f
    encodeOne ExtDouble d = PJ.protoDoubleToJSON d
    encodeOne ExtString s = Aeson.String s
    encodeOne ExtBytes b = PJ.protoBytesToJSON b
    encodeOne ExtMessage b = PJ.protoBytesToJSON b
