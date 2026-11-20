{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Adapt the upstream test messages from their descriptors.
module FixtureProto (
  Registry,
  emptyRegistry,
  loadRegistry,
  fixtureEnv,
  resolveAny,
  decodeMessageFields,
  PtVal (..),
  Fields,
) where

import CEL
import CEL.Error (conversion, invalidArg, noOverload, noSuchField, overflow)
import CEL.Stdlib (protobufJsonList, protobufJsonStruct, protobufJsonValue, selectField)
import CEL.Value (celFieldPresent, selectCelField)
import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, unless)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64, Word8)
import GHC.Exts (Int (I#))
import GHC.Float (double2Float, float2Double)
import Proto.Decode qualified as Proto
import Proto.Encode (messageToByteString)
import Proto.Extension qualified as Ext
import Proto.Google.Protobuf.Any qualified as Any
import Proto.Google.Protobuf.Any.Util (typeNameFromUrl)
import Proto.Google.Protobuf.Descriptor
import Proto.IDL.AST (Constant (..), enumOptions, protoOptions, protoSyntax, pattern CInt)
import Proto.IDL.Descriptor
import Proto.IDL.Parser.Resolver
import Proto.Internal.Wire.Decode qualified as Wire
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)


data PtVal = PStr [Word8] | PAtom String | PMsg Fields
  deriving stock (Show)


type Fields = [(String, PtVal)]


data Registry = Registry
  { messages :: !(Map Text (Map Text Field))
  , fieldsByNumber :: !(Map Text (Map Int (Text, Field)))
  , enumBindings :: ![(Text, Value)]
  }


data Field
  = Singular !Presence !Int32 !Atom
  | Repeated !Atom
  | MapField !Atom !Atom


data Presence = Implicit | Explicit | Required
  deriving stock (Eq)


data Atom
  = Scalar !Int32 !(Maybe Constant)
  | Enumeration !Text !EnumDescription !Text
  | Message !Int32 !Text


data EnumDescription = EnumDescription
  { enumValues :: !(V.Vector EnumValueDescriptorProto)
  , closedEnumValues :: !(Maybe IntSet)
  }


emptyRegistry :: Registry
emptyRegistry = Registry Map.empty Map.empty []


loadRegistry :: FilePath -> IO (Maybe Registry)
loadRegistry root = do
  let protoRoot = root </> "proto"
      base = "cel/expr/conformance"
      paths = map (base </>)
        ["proto2/test_all_types.proto", "proto3/test_all_types.proto", "proto2/test_all_types_extensions.proto"]
      config = defaultResolveConfig {rcIncludeDirs = [protoRoot]}
  exists <- traverse (doesFileExist . (protoRoot </>)) paths
  if not (and exists)
    then pure Nothing
    else do
      files <- forM paths $ \path -> do
        resolved <- either (fail . show) pure =<< resolveProtoFile config (protoRoot </> path)
        pure (Map.insert (T.pack path) (rpFile resolved) (resolvedImportFiles resolved))
      sources <- either (fail . T.unpack) pure (traverse resolveFieldPresence (Map.unions files))
      let definitions = Map.unions (map descriptorTypeDefinitions (Map.elems sources))
      descriptors <- forM (Map.toList sources) $ \(path, source) ->
        either (fail . show) pure
          (Proto.decodeMessage (serializeFileDescriptorWithTypes definitions (T.unpack path) source))
      Just <$> either (fail . T.unpack) pure (buildRegistry descriptors)


buildRegistry :: [FileDescriptorProto] -> Either Text Registry
buildRegistry files = do
  enumsByName <- traverse describeEnum enumDescriptors
  let extensionsByName = extensions enumsByName
      definitions = Map.mapMaybeWithKey (ordinary enumsByName extensionsByName) allMessages
      bindings = concatMap enumBindingsFor (Map.toList enumsByName)
  pure (Registry (fmap (fmap snd) definitions) (fmap byNumber definitions) bindings)
  where
    byNumber = Map.fromList . map (\(name, (number, shape)) -> (number, (name, shape))) . Map.toList
    allMessages = Map.fromList (concatMap fileMessages files)
    fileMessages file = collect (fdpPackage file) (fdpMessageType file)
      where
        collect parent = concatMap (visit parent) . V.toList
        visit parent message =
          let name = qualify parent (dpName message)
          in (name, (file, message)) : collect name (dpNestedType message)
    enumDescriptors = Map.fromList $
      concatMap (\file -> enums file (fdpPackage file) (fdpEnumType file)) files
        <> concatMap (\(name, (file, message)) -> enums file name (dpEnumType message)) (Map.toList allMessages)
    enums file parent = map (\entry -> (qualify parent (edpName entry), (file, entry))) . V.toList
    describeEnum (file, entry) = do
      let source = fileDescriptorToAST file
          values = edpValue entry
      closed <- enumIsClosed (protoSyntax source) (protoOptions source) (enumOptions (enumDescriptorToEnum entry))
      pure (EnumDescription values (if closed then Just (V.foldl' insertEnumValue IntSet.empty values) else Nothing))
    insertEnumValue numbers value = IntSet.insert (fromIntegral (evdpNumber value)) numbers
    extensions enumsByName = Map.fromListWith Map.union $
      concatMap (\file -> scopedExtensions enumsByName file (fdpPackage file) (fdpExtension file)) files
        <> concatMap (\(name, (file, message)) -> scopedExtensions enumsByName file name (dpExtension message)) (Map.toList allMessages)
    scopedExtensions enumsByName file parent = map (extension enumsByName file parent) . V.toList
    extension enumsByName file parent f =
      (T.dropWhile (== '.') (fdpFieldExtendee f), Map.singleton (qualify parent (fdpFieldName f)) (numberedField enumsByName file f))
    enumBindingsFor (name, definition) =
      concatMap (\value ->
        let v = VInt (fromIntegral (evdpNumber value))
            parent = T.dropEnd 1 (fst (T.breakOnEnd "." name))
        in [(qualify name (evdpName value), v), (qualify parent (evdpName value), v)]) (V.toList (enumValues definition))
    ordinary enumsByName extensionsByName name (file, message)
      | maybe False moMapEntry (dpOptions message) = Nothing
      | otherwise =
          let ordinaryFields = Map.fromList (map (\f -> (fdpFieldName f, numberedField enumsByName file f)) (V.toList (dpField message)))
          in Just (Map.union ordinaryFields (Map.findWithDefault Map.empty name extensionsByName))
    numberedField enumsByName file f = (fromIntegral (fdpFieldNumber f), field enumsByName file f)
    field enumsByName file f
      | fdpFieldLabel f /= 3 = Singular (fieldPresence file f) (fdpFieldOneofIdx f) (atom enumsByName f)
      | Just (_, entry) <- Map.lookup (typeNameOf f) allMessages
      , maybe False moMapEntry (dpOptions entry)
      , Just key <- V.find ((== 1) . fdpFieldNumber) (dpField entry)
      , Just value <- V.find ((== 2) . fdpFieldNumber) (dpField entry) = MapField (atom enumsByName key) (atom enumsByName value)
      | otherwise = Repeated (atom enumsByName f)
    fieldPresence file f
      | fdpFieldLabel f == 2 = Required
      | descriptorFieldHasPresence file f = Explicit
      | otherwise = Implicit
    atom enumsByName f = case fdpFieldType f of
      10 -> Message 10 (typeNameOf f)
      11 -> Message 11 (typeNameOf f)
      14 -> Enumeration (typeNameOf f) (Map.findWithDefault (EnumDescription V.empty Nothing) (typeNameOf f) enumsByName) (fdpFieldDefault f)
      code -> Scalar code (descriptorDefaultConstant f)


qualify :: Text -> Text -> Text
qualify parent name
  | T.null parent = name
  | otherwise = parent <> "." <> name


typeNameOf :: FieldDescriptorProto -> Text
typeNameOf = T.dropWhile (== '.') . fdpFieldTypeName


fixtureEnv :: Registry -> Env
fixtureEnv registry =
  Map.foldrWithKey add initial (messages registry)
  where
    initial
      | Map.null (messages registry) = emptyEnv
      | otherwise = addAnyResolver (resolveAny registry) (bindAll (enumBindings registry) emptyEnv)
    add name fields env
      | name == "google.protobuf.Any" = env
      | otherwise = addMessageType name (construct registry normalizeLiteral fields) env
    normalizeLiteral name atom value = unpackValue value >>= normalize registry name atom


construct
  :: Registry
  -> (Text -> Atom -> Value -> Either CelError Value)
  -> Map Text Field
  -> MessageConstructor
construct registry normalizeField fields inputs = do
  supplied <- Map.traverseWithKey initialize inputs
  let groups = Map.mapMaybeWithKey (\name field -> case Map.lookup name fields of
        Just (Singular _ group _) | celFieldPresent field && group >= 0 -> Just group
        _ -> Nothing) supplied
  unless (Set.size (Set.fromList (Map.elems groups)) == Map.size groups) $
    Left (invalidArg "multiple members of a protobuf oneof")
  pure (Map.union supplied (Map.map (CelField False . defaultField registry) fields))
  where
    initialize name value = case Map.lookup name fields of
      Just field -> initializeField registry (normalizeField name) name field value
      Nothing -> Left (noSuchField name)


initializeField
  :: Registry -> (Atom -> Value -> Either CelError Value) -> Text -> Field -> Value -> Either CelError CelField
initializeField registry normalizeField name field value = case (field, value) of
  (Singular _ _ atom, VNull) | discardNull atom ->
    Right (CelField False (defaultAtom registry atom))
  (Singular presence _ atom, _) -> do
    normalized <- normalizeField atom value
    let implicitZero = case normalized of
          VDouble n -> n == 0 && not (isNegativeZero n)
          _ -> normalized == defaultAtom registry atom
    pure (CelField (presence /= Implicit || not implicitZero) normalized)
  (Repeated atom, VList values) -> do
    let retained = if discardNull atom then V.filter (/= VNull) values else values
    xs <- V.mapM (normalizeField atom) retained
    pure (CelField (not (V.null xs)) (VList xs))
  (MapField key val, VMap values) -> do
    let convert (k, v) = (,) <$> normalizeField key k <*> normalizeField val v
        entries = if discardNull val then filter ((/= VNull) . snd) else id
    normalized <- traverse convert (entries (celMapEntries values))
    result <- first invalidArg (celMap normalized)
    pure (CelField (not (null normalized)) (VMap result))
  _ -> Left (noOverload name)


discardNull :: Atom -> Bool
discardNull (Message _ name) = name `notElem`
  ["google.protobuf.Any", "google.protobuf.Value", "google.protobuf.Struct", "google.protobuf.ListValue"]
discardNull _ = False


normalize :: Registry -> Text -> Atom -> Value -> Either CelError Value
normalize _ field (Message _ "google.protobuf.Any") (VType _) = Left (noOverload field)
normalize _ _ (Message _ "google.protobuf.Any") value@VList {} = protobufJsonList value
normalize _ _ (Message _ "google.protobuf.Any") value@VMap {} = protobufJsonStruct value
normalize _ _ (Message _ "google.protobuf.Any") value = Right value
normalize registry field atom value@VAny {} = unpackValue value >>= normalize registry field atom
normalize registry field atom value = case atom of
  Scalar code _ -> normalizeScalar field code value
  Enumeration "google.protobuf.NullValue" _ _
    | VInt 0 <- value -> Right VNull
    | VNull <- value -> Right VNull
    | otherwise -> Left (noOverload field)
  Enumeration {} -> normalizeScalar field 5 value
  Message _ name -> case name of
    "google.protobuf.Value" -> protobufJsonValue value
    "google.protobuf.ListValue" -> protobufJsonList value
    "google.protobuf.Struct" -> protobufJsonStruct value
    "google.protobuf.Timestamp" -> require TyTimestamp
    "google.protobuf.Duration" -> require TyDuration
    _ | Just wrapper <- wrapperAtom registry name -> normalize registry field wrapper value
      | otherwise -> require (TyMessage name)
  where
    require expected
      | typeOf value == expected = Right value
      | otherwise = Left (noOverload field)


normalizeScalar :: Text -> Int32 -> Value -> Either CelError Value
normalizeScalar field code value
  | typeOf value /= typeOf (scalarDefault code Nothing) = Left (noOverload field)
  | VInt n <- value, code `elem` [5, 15, 17]
  , n < fromIntegral (minBound :: Int32) || n > fromIntegral (maxBound :: Int32) = Left (overflow field)
  | VUInt n <- value, code `elem` [7, 13], n > fromIntegral (maxBound :: Word32) = Left (overflow field)
  | VDouble n <- value, code == 2 = Right (VDouble (float2Double (double2Float n)))
  | otherwise = Right value


defaultField :: Registry -> Field -> Value
defaultField registry = \case
  Singular _ _ atom -> defaultAtom registry atom
  Repeated _ -> VList V.empty
  MapField _ _ -> VMap (celMapFromList [])


defaultAtom :: Registry -> Atom -> Value
defaultAtom registry = \case
  Scalar code value -> scalarDefault code value
  Enumeration "google.protobuf.NullValue" _ _ -> VNull
  Enumeration _ definition explicit ->
    let values = enumValues definition
        value = V.find ((== explicit) . evdpName) values <|> (values V.!? 0)
    in VInt (maybe 0 (fromIntegral . evdpNumber) value)
  Message _ name -> case name of
    "google.protobuf.Any" -> VNull
    "google.protobuf.Value" -> VNull
    "google.protobuf.ListValue" -> VList V.empty
    "google.protobuf.Struct" -> VMap (celMapFromList [])
    "google.protobuf.Timestamp" -> VTimestamp (Timestamp 0 0)
    "google.protobuf.Duration" -> VDuration (Duration 0 0)
    _ | Just _ <- wrapperAtom registry name -> VNull
      | otherwise -> VMessage (CelMessage name (Map.map (CelField False . defaultField registry) fields))
      where fields = Map.findWithDefault Map.empty name (messages registry)


wrapperAtom :: Registry -> Text -> Maybe Atom
wrapperAtom registry name = do
  suffix <- T.stripPrefix "google.protobuf." name
  if suffix `elem` ["DoubleValue", "FloatValue", "Int64Value", "UInt64Value", "Int32Value", "UInt32Value", "BoolValue", "StringValue", "BytesValue"]
    then do
      Singular _ _ atom <- Map.lookup name (messages registry) >>= Map.lookup "value"
      pure atom
    else Nothing


scalarDefault :: Int32 -> Maybe Constant -> Value
scalarDefault code declared = case code of
  1 -> VDouble double
  2 -> VDouble (float2Double (double2Float double))
  4 -> VUInt unsigned
  6 -> VUInt unsigned
  7 -> VUInt unsigned
  8 -> VBool (case declared of Just (CBool b) -> b; _ -> False)
  9 -> VString (case declared of Just (CString s) -> s; _ -> "")
  12 -> VBytes (case declared of Just (CBytes b) -> b; _ -> BS.empty)
  13 -> VUInt unsigned
  _ -> VInt (fromInteger integer)
  where
    double = case declared of Just (CFloat d) -> d; _ -> 0
    integer = case declared of Just (CInt n) -> n; _ -> 0
    unsigned = fromInteger integer


-- | Parse fixture text fields against the schema. Well-known types use the
-- runner's existing value adapters.
decodeMessageFields
  :: Registry -> (Text -> PtVal -> Either String Value) -> Text -> Fields -> Either String Value
decodeMessageFields registry decodeObject name fields = do
  definitions <- maybe (Left ("unknown protobuf fixture type " <> T.unpack name)) Right $
    Map.lookup name (messages registry)
  let grouped = Map.fromListWith (flip (<>)) (map (\(key, value) -> (T.pack key, [value])) fields)
  inputs <- Map.traverseWithKey (decodeField definitions) grouped
  VMessage . CelMessage name <$> first (T.unpack . errMsg) (construct registry (normalize registry) definitions inputs)
  where
    decodeField definitions field values = case (Map.lookup field definitions, values) of
      (Just (Singular _ _ atom), [value]) -> decodeAtom atom value
      (Just (Repeated atom), _) -> VList . V.fromList <$> traverse (decodeAtom atom) values
      (Just (MapField key val), _) -> VMap . celMapFromList <$> traverse (mapEntry key val) values
      _ -> Left ("invalid protobuf fixture field " <> T.unpack (qualify name field))
    mapEntry key val (PMsg entry) =
      (,) <$> item "key" key <*> item "value" val
      where item label atom = maybe (Right (defaultAtom registry atom)) (decodeAtom atom) (lookup label entry)
    mapEntry _ _ _ = Left "expected protobuf map entry"
    decodeAtom atom value = case (atom, value) of
      (Message _ ty, PMsg _) -> decodeObject ty value
      (Enumeration _ definition _, PAtom token)
        | Just entry <- V.find ((== T.pack token) . evdpName) (enumValues definition) -> Right (VInt (fromIntegral (evdpNumber entry)))
        | Just n <- readMaybe token -> Right (VInt n)
      (Scalar code _, _) -> decodeScalar (scalarDefault code Nothing) value
      _ -> Left "invalid protobuf fixture value"


decodeScalar :: Value -> PtVal -> Either String Value
decodeScalar initial value = case (initial, value) of
  (VBool _, PAtom "true") -> Right (VBool True)
  (VBool _, PAtom "false") -> Right (VBool False)
  (VInt _, PAtom token) -> readNumber VInt (readMaybe token :: Maybe Int64)
  (VUInt _, PAtom token) -> readNumber VUInt (readMaybe token :: Maybe Word64)
  (VDouble _, PAtom "inf") -> Right (VDouble (1 / 0))
  (VDouble _, PAtom "-inf") -> Right (VDouble (-1 / 0))
  (VDouble _, PAtom "nan") -> Right (VDouble (0 / 0))
  (VDouble _, PAtom token) -> readNumber VDouble (readMaybe token)
  (VString _, PStr bytes) -> VString <$> first show (TE.decodeUtf8' (BS.pack bytes))
  (VBytes _, PStr bytes) -> Right (VBytes (BS.pack bytes))
  _ -> Left "invalid scalar protobuf fixture value"
  where
    readNumber make = maybe (Left "invalid numeric protobuf fixture value") (Right . make)


-- Decode the binary payloads embedded in CEL's Any fixtures using the same
-- field metadata as the TextFormat fixtures.
resolveAny :: Registry -> AnyResolver
resolveAny registry url bytes =
  decodeWireMessage registry (typeNameFromUrl url) bytes >>= anyCelValue >>= unpackValue


decodeWireMessage :: Registry -> Text -> ByteString -> Either CelError AnyPayload
decodeWireMessage registry name bytes
  | name == "google.protobuf.Any" = do
      value <- decodeNestedAny registry bytes
      pure (AnyPayload value (Right value))
  | otherwise = do
      fields <- maybe (Left (conversion ("unknown protobuf message type " <> name))) Right (Map.lookup name (messages registry))
      numbered <- maybe (Left (conversion ("missing wire schema for " <> name))) Right (Map.lookup name (fieldsByNumber registry))
      records <- first (conversion . T.pack . show) (decodeRecords bytes)
      wire <- foldM (collectRecord numbered) (WireFields Map.empty Map.empty []) records
      supplied <- Map.traverseWithKey (decodeField fields) (knownRecords wire)
      forM_ (Map.toList fields) $ \(field, shape) -> case shape of
        Singular Required _ _ -> unless (Map.member field supplied) $
          Left (conversion ("missing required protobuf field " <> qualify name field))
        _ -> pure ()
      let decoded = Map.union supplied (Map.map (CelField False . defaultField registry) fields)
          unknown = BS.concat (reverse (unknownRecords wire))
          message = VMessage (celMessageWithUnknownFields name decoded unknown)
      pure (AnyPayload message (adaptMessage decoded message))
  where
    decodeField fields field records = case Map.lookup field fields of
      Just (Singular presence _ atom) -> do
        payload <- decodeSingular registry atom records
        let stored = anyProtobufValue payload
            isDefault = case (atom, stored) of
              (_, VDouble n) -> n == 0 && not (isNegativeZero n)
              (Enumeration "google.protobuf.NullValue" _ _, VInt n) -> n == 0
              _ -> stored == defaultAtom registry atom
        pure (CelFieldWithSelection (presence /= Implicit || not isDefault) stored (anyCelValue payload))
      Just (Repeated atom) -> do
        items <- decodeRepeated registry atom (reverse records)
        let stored = VList (V.fromList (map anyProtobufValue items))
            selected = VList . V.fromList <$> traverse anyCelValue items
        pure (CelFieldWithSelection (not (null items)) stored selected)
      Just (MapField key val) -> do
        entries <- traverse (decodeEntry registry key val) (reverse records)
        let stored = VMap (celMapFromList (map (fmap anyProtobufValue) entries))
            selectEntry (k, v) = (k,) <$> anyCelValue v
            selected = VMap . celMapFromList <$> traverse selectEntry entries
        pure (CelFieldWithSelection (not (null entries)) stored selected)
      Nothing -> Left (noSuchField field)
    adaptMessage fields message
      | Just _ <- wrapperAtom registry name = select "value"
      | otherwise = case name of
          "google.protobuf.Timestamp" -> temporal (\seconds nanos -> VTimestamp (Timestamp seconds nanos))
          "google.protobuf.Duration" -> temporal (\seconds nanos -> VDuration (Duration seconds nanos))
          "google.protobuf.ListValue" -> select "values"
          "google.protobuf.Struct" -> select "fields"
          "google.protobuf.Value" -> case Map.lookupMin (Map.filter celFieldPresent fields) of
            Nothing -> Right VNull
            Just (_, field) -> selectCelField field
          _ -> Right message
      where
        select = selectField message
        temporal make = do
          seconds <- select "seconds"
          nanos <- select "nanos"
          case (seconds, nanos) of
            (VInt s, VInt n) -> Right (make s (fromIntegral n))
            _ -> Left (conversion ("invalid " <> name))


decodeBytes :: Proto.MessageDecode a => ByteString -> Either CelError a
decodeBytes = first (conversion . T.pack . show) . Proto.decodeMessage


data WireRecord = WireRecord !Proto.UnknownField !ByteString


decodeRecords :: ByteString -> Either Proto.DecodeError [WireRecord]
decodeRecords bytes = Proto.runDecoder (go 0 []) bytes
  where
    go start records = Proto.withTagM (pure (reverse records)) $ \number wire -> do
      field <- Proto.captureUnknownField number (toEnum wire)
      end <- Wire.Decoder (\_ offset -> (# (# I# offset, offset #) | #))
      let encoded = BS.take (end - start) (BS.drop start bytes)
      go end (WireRecord field encoded : records)


data WireFields = WireFields
  { knownRecords :: !(Map Text [Proto.UnknownField])
  , activeOneofs :: !(Map Int32 Text)
  , unknownRecords :: ![ByteString]
  }


-- Wire records arrive in order. Changing a oneof arm discards earlier
-- occurrences of the old arm, while repeated occurrences of the same
-- message arm merge.
collectRecord
  :: Map Int (Text, Field)
  -> WireFields
  -> WireRecord
  -> Either CelError WireFields
collectRecord fields state (WireRecord record encoded) =
  case Map.lookup (Ext.unknownFieldNumber record) fields of
    Just (name, field) | acceptsField field record -> collectKnown name field
    _ -> Right (keepUnknown state encoded)
  where
    collectKnown name field = case (field, record) of
      (Singular _ _ atom, Proto.UnknownVarint _ raw) ->
        Right (keepEnum name field atom raw)
      (Repeated atom, Proto.UnknownVarint _ raw) ->
        Right (keepEnum name field atom raw)
      (Repeated atom, Proto.UnknownLenDelim number bytes)
        | Just known <- closedValues atom -> do
            values <- first (conversion . T.pack . show) (Proto.decodeAllVarints bytes)
            -- Unknown packed enum values become separate varint records.
            let addValue accumulated raw =
                  let expanded = Proto.UnknownVarint number raw
                  in if knownEnumValue known raw
                       then keepKnown name field accumulated expanded
                       else keepUnknown accumulated (messageToByteString (Proto.encodeUnknownFields [expanded]))
            pure (VU.foldl' addValue state values)
      (MapField _ atom, Proto.UnknownLenDelim _ bytes)
        | Just known <- closedValues atom -> do
            entries <- first (conversion . T.pack . show) (Proto.decodeUnknownFields bytes)
            let lastValue _ (Proto.UnknownVarint 2 raw) = Just raw
                lastValue previous _ = previous
            pure $ case foldl' lastValue Nothing entries of
              Just raw | not (knownEnumValue known raw) -> keepUnknown state encoded
              _ -> keepKnown name field state record
      _ -> Right (keepKnown name field state record)
    keepEnum name field atom raw
      | Just known <- closedValues atom
      , not (knownEnumValue known raw) = keepUnknown state encoded
      | otherwise = keepKnown name field state record


closedValues :: Atom -> Maybe IntSet
closedValues (Enumeration _ definition _) = closedEnumValues definition
closedValues _ = Nothing


knownEnumValue :: IntSet -> Word64 -> Bool
knownEnumValue known raw = IntSet.member (fromIntegral (fromIntegral raw :: Int32)) known


keepUnknown :: WireFields -> ByteString -> WireFields
keepUnknown state encoded = state {unknownRecords = encoded : unknownRecords state}


keepKnown :: Text -> Field -> WireFields -> Proto.UnknownField -> WireFields
keepKnown name field state record =
  retained {knownRecords = Map.insertWith (<>) name [record] (knownRecords retained)}
  where
    retained = case field of
      Singular _ group _ | group >= 0 ->
        case Map.lookup group (activeOneofs state) of
          Just previous | previous == name -> state
          previous -> state
            { knownRecords = maybe (knownRecords state) (`Map.delete` knownRecords state) previous
            , activeOneofs = Map.insert group name (activeOneofs state)
            }
      _ -> state


acceptsField :: Field -> Proto.UnknownField -> Bool
acceptsField field record = case field of
  Singular _ _ atom -> acceptsAtom atom record
  Repeated atom -> acceptsAtom atom record || (wireType record == 2 && atomWireType atom `elem` [0, 1, 5])
  MapField {} -> wireType record == 2


acceptsAtom :: Atom -> Proto.UnknownField -> Bool
acceptsAtom atom record = atomWireType atom == wireType record


wireType :: Proto.UnknownField -> Int
wireType = \case
  Proto.UnknownVarint {} -> 0
  Proto.UnknownFixed64 {} -> 1
  Proto.UnknownLenDelim {} -> 2
  Proto.UnknownGroup {} -> 3
  Proto.UnknownFixed32 {} -> 5


atomWireType :: Atom -> Int
atomWireType = \case
  Message 10 _ -> 3
  Message {} -> 2
  Scalar code _
    | code `elem` [1, 6, 16] -> 1
    | code `elem` [2, 7, 15] -> 5
    | code `elem` [9, 12] -> 2
  _ -> 0


decodeSingular :: Registry -> Atom -> [Proto.UnknownField] -> Either CelError AnyPayload
decodeSingular registry atom records = case (atom, records) of
  (Message kind name, _ : _) -> do
    let decoder = if kind == 10 then Ext.ExtGroup else Ext.ExtMessage
    payloads <- traverse (readRecord decoder) (reverse records)
    -- A truncated occurrence must not read through into the next occurrence.
    merged <- case payloads of
      [payload] -> Right payload
      _ -> do
        mapM_ (first (conversion . T.pack . show) . Proto.decodeUnknownFields) payloads
        Right (BS.concat payloads)
    decodeWireMessage registry name merged
  (_, record : _) -> case scalarCodec atom of
    ScalarCodec decoder toValue -> scalarPayload atom . toValue <$> readRecord decoder record
  (Message _ name, []) -> decodeWireMessage registry name BS.empty
  (_, []) ->
    let value = defaultAtom registry atom
    in Right (AnyPayload value (Right value))


decodeNestedAny :: Registry -> ByteString -> Either CelError Value
decodeNestedAny registry bytes = do
  message <- decodeBytes bytes
  let url = Any.anyTypeUrl message
  pure (VAnyPayload url bytes (decodeWireMessage registry (typeNameFromUrl url) (Any.anyValue message)))


readRecord :: Ext.ExtensionType a -> Proto.UnknownField -> Either CelError a
readRecord decoder = maybe (Left (conversion "invalid protobuf wire field")) Right . Ext.decodeExtensionValue decoder


decodeRepeated :: Registry -> Atom -> [Proto.UnknownField] -> Either CelError [AnyPayload]
decodeRepeated registry atom records = case (atom, records) of
  (_, []) -> Right []
  (Message {}, _) -> traverse (decodeSingular registry atom . pure) records
  (_, record : _) -> case scalarCodec atom of
    ScalarCodec decoder toValue ->
      let extension = Ext.RepeatedExtension (Ext.unknownFieldNumber record) decoder False
      in map (scalarPayload atom . toValue) <$> first (conversion . T.pack . show) (Ext.getRepeatedExtensionChecked extension (WireRecords records))


scalarPayload :: Atom -> Value -> AnyPayload
scalarPayload (Enumeration "google.protobuf.NullValue" _ _) value = AnyPayload value (Right VNull)
scalarPayload _ value = AnyPayload value (Right value)


decodeEntry :: Registry -> Atom -> Atom -> Proto.UnknownField -> Either CelError (Value, AnyPayload)
decodeEntry registry key val record = do
  bytes <- readRecord Ext.ExtMessage record
  fields <- reverse <$> first (conversion . T.pack . show) (Proto.decodeUnknownFields bytes)
  let readEntry number atom =
        decodeSingular registry atom (filter (\f -> Ext.unknownFieldNumber f == number && acceptsAtom atom f) fields)
  (,) <$> (readEntry 1 key >>= anyCelValue) <*> readEntry 2 val


newtype WireRecords = WireRecords [Proto.UnknownField]


instance Ext.HasExtensions WireRecords where
  messageUnknownFields (WireRecords records) = records
  setMessageUnknownFields records _ = WireRecords records


data ScalarCodec where
  ScalarCodec :: Ext.ExtensionType a -> (a -> Value) -> ScalarCodec


scalarCodec :: Atom -> ScalarCodec
scalarCodec atom = case atom of
  Enumeration {} -> ScalarCodec Ext.ExtInt32 (VInt . fromIntegral)
  Scalar code _ -> case code of
    1 -> ScalarCodec Ext.ExtDouble VDouble
    2 -> ScalarCodec Ext.ExtFloat (VDouble . float2Double)
    3 -> ScalarCodec Ext.ExtInt64 VInt
    4 -> ScalarCodec Ext.ExtUInt64 VUInt
    5 -> ScalarCodec Ext.ExtInt32 (VInt . fromIntegral)
    6 -> ScalarCodec Ext.ExtFixed64 VUInt
    7 -> ScalarCodec Ext.ExtFixed32 (VUInt . fromIntegral)
    8 -> ScalarCodec Ext.ExtBool VBool
    9 -> ScalarCodec Ext.ExtString VString
    12 -> ScalarCodec Ext.ExtBytes VBytes
    13 -> ScalarCodec Ext.ExtUInt32 (VUInt . fromIntegral)
    15 -> ScalarCodec Ext.ExtSFixed32 (VInt . fromIntegral)
    16 -> ScalarCodec Ext.ExtSFixed64 VInt
    17 -> ScalarCodec Ext.ExtSInt32 (VInt . fromIntegral)
    18 -> ScalarCodec Ext.ExtSInt64 VInt
    _ -> ScalarCodec Ext.ExtBytes VBytes
  Message {} -> ScalarCodec Ext.ExtBytes VBytes
