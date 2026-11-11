{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Registry for proto2 extension JSON helpers.

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
  registerTypedExtensionJson,
  lookupExtensionByFqn,
  lookupExtensionByNumber,
  parentHasExtensions,
  extensionEntriesForJson,
  parseExtensionEntry,
  parseExtensionsFromJson,

  -- * Per-extension codec primitives (used by the

  -- 'loadProto'-generated registration code)
  parseExtValueViaConstructor,
  encodeExtValueViaConstructor,
) where

import Control.Monad (foldM)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Aeson.Types qualified as AesonT
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Proto.Decode (UnknownField (..))
import Proto.Extension (
  Extension (..),
  ExtensionType (..),
  decodeExtensionValue,
  encodeExtensionValue,
  unknownFieldNumber,
 )
import Proto.Internal.JSON qualified as PJ
import Proto.Schema (ProtoEnum (..))


{- | Describes how to bridge a single proto2 extension between
its JSON value form and the wire 'UnknownField' representation
the message stores under its unknown-fields slot.
-}
data ExtJsonCodec = ExtJsonCodec
  { ejcExtensionFqn :: !Text
  {- ^ Fully-qualified proto name of the extension (the
  bracket-quoted JSON key without the brackets).
  -}
  , ejcFieldNumber :: !Int
  , ejcParseValue :: Aeson.Value -> Either String UnknownField
  , ejcEncodeValue :: UnknownField -> Either String Aeson.Value
  }


data RegisteredExtension = RegisteredExtension
  { registeredCodec :: !ExtJsonCodec
  , omitNull :: !Bool
  }


{- | One per parent message: maps both extension FQN (for parse)
and field number (for output) to the codec.
-}
data ExtRegistryEntry = ExtRegistryEntry
  { byFqn :: !(Map Text RegisteredExtension)
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
registerExtensionJson = registerExtensionWithNull False


registerExtensionWithNull :: Bool -> Text -> ExtJsonCodec -> ExtensionRegistry
registerExtensionWithNull omit parentFqn codec =
  let entry =
        ExtRegistryEntry
          { byFqn = Map.singleton (ejcExtensionFqn codec) (RegisteredExtension codec omit)
          , byNum = Map.singleton (ejcFieldNumber codec) codec
          }
  in ExtensionRegistry (Map.singleton parentFqn entry)


-- | Build the JSON registry entry for a generated singular extension.
registerTypedExtensionJson :: forall msg a. Text -> Text -> Extension msg a -> ExtensionRegistry
registerTypedExtensionJson parentFqn fqn ext =
  registerExtensionWithNull omit parentFqn $
    ExtJsonCodec
      { ejcExtensionFqn = fqn
      , ejcFieldNumber = extNumber ext
      , ejcParseValue = parseExtValueViaConstructor (extType ext) (extNumber ext)
      , ejcEncodeValue = encodeExtValueViaConstructor (extType ext)
      }
  where
    omit = case extType ext of
      ExtEnum -> not (isNullValue (Proxy :: Proxy a))
      _ -> True


-- | Look up an extension codec by its fully-qualified proto name under a parent message.
lookupExtensionByFqn :: ExtensionRegistry -> Text -> Text -> Maybe ExtJsonCodec
lookupExtensionByFqn reg parentFqn fqn = registeredCodec <$> lookupRegistered reg parentFqn fqn


lookupRegistered :: ExtensionRegistry -> Text -> Text -> Maybe RegisteredExtension
lookupRegistered reg parentFqn fqn = Map.lookup parentFqn (erEntries reg) >>= Map.lookup fqn . byFqn


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
'Nothing' when the key does not select a registered extension or
a typed extension treats null as unset.
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
         Just (fqn, ']') -> do
           registered <- lookupRegistered reg parentFqn fqn
           case val of
             Aeson.Null | omitNull registered -> Nothing
             _ -> Just (ejcParseValue (registeredCodec registered) val)
         _ -> Nothing
       _ -> Nothing
{-# INLINE parseExtensionEntry #-}


-- | Extract the registered extension entries of a message JSON object.
parseExtensionsFromJson :: ExtensionRegistry -> Text -> Aeson.Object -> Either String [UnknownField]
parseExtensionsFromJson reg parentFqn obj
  | not (parentHasExtensions reg parentFqn) = Right []
  | otherwise = reverse <$> foldM step [] (AesonKM.toList obj)
  where
    step acc (key, value) = case parseExtensionEntry reg parentFqn key value of
      Nothing -> Right acc
      Just result -> (: acc) <$> result
{-# INLINE parseExtensionsFromJson #-}


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
  ExtEnum -> do
    value <- case v of
      Aeson.Null
        | isNullValue (Proxy :: Proxy a) ->
            maybe (Left "NullValue enum has no zero member") Right (fromProtoEnumValue 0)
      _ -> AesonT.parseEither Aeson.parseJSON v
    let uf = encodeExtensionValue fn ty value
    case decodeExtensionValue ty uf of
      Just _ -> Right uf
      Nothing -> Left "Unrecognized value for a closed enum extension"
  ExtInt32 -> parseInteger PJ.protoInt32FromJSON
  ExtInt64 -> parseInteger PJ.protoInt64FromJSON
  ExtUInt32 -> parseInteger PJ.protoWord32FromJSON
  ExtUInt64 -> parseInteger PJ.protoWord64FromJSON
  ExtSInt32 -> parseInteger PJ.protoInt32FromJSON
  ExtSInt64 -> parseInteger PJ.protoInt64FromJSON
  ExtFixed32 -> parseInteger PJ.protoWord32FromJSON
  ExtFixed64 -> parseInteger PJ.protoWord64FromJSON
  ExtSFixed32 -> parseInteger PJ.protoInt32FromJSON
  ExtSFixed64 -> parseInteger PJ.protoInt64FromJSON
  ExtFloat -> encodeExtensionValue fn ty <$> parseFloating v
  ExtDouble -> encodeExtensionValue fn ty <$> parseFloatingD v
  ExtString -> encodeExtensionValue fn ty <$> parseStringT v
  ExtBytes -> encodeExtensionValue fn ty <$> parseBytesB v
  ExtMessage ->
    -- Embedded sub-messages aren't exercised by the
    -- conformance suite's bracket-syntax tests; if a user
    -- needs them they can register a custom codec.
    Left "JSON serialisation of message-typed extensions is not yet supported"
  ExtGroup -> Left "JSON serialisation of group-typed extensions is not yet supported"
  where
    parseInteger parser = encodeExtensionValue fn ty <$> AesonT.parseEither parser v


parseBool :: Aeson.Value -> Either String Bool
parseBool (Aeson.Bool b) = Right b
parseBool _ = Left "Expected JSON Bool"


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
    encodeOne :: forall b. ExtensionType b -> b -> Aeson.Value
    encodeOne ExtBool b = Aeson.Bool b
    encodeOne ExtEnum e
      | isNullValue (Proxy :: Proxy b) = Aeson.Null
      | otherwise = Aeson.toJSON e
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
    encodeOne ExtGroup b = PJ.protoBytesToJSON b


isNullValue :: ProtoEnum a => Proxy a -> Bool
isNullValue proxy = protoEnumName proxy == "google.protobuf.NullValue"
