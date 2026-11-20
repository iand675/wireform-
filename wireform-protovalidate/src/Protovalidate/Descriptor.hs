{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Read @buf.validate@ rules out of a /compiled/ protobuf descriptor
(@FileDescriptorProto@ / @DescriptorProto@), as produced by @protoc@ and
carried in a @FileDescriptorSet@.

protovalidate stores its rules as option /extensions/: extension #1159 on
@google.protobuf.FieldOptions@ (a @buf.validate.FieldConstraints@) and on
@google.protobuf.MessageOptions@ (a @buf.validate.MessageConstraints@).
Because "Proto.Google.Protobuf.Descriptor" now preserves unknown fields,
those extension bytes survive decoding; this module pulls them out and maps
them onto the validation 'MessageRules' model.

The decoder reads custom CEL constraints (@cel@), @required@, and @ignore@.
Supported standard rules use their @buf.validate@ v1 field numbers and wire
types, including packed scalar lists and timestamp and duration messages.
-}
module Protovalidate.Descriptor (
  fileRulesFromDescriptor,
  messageRulesFromDescriptor,
  fieldConstraintExtension,
  messageConstraintExtension,
) where

import CEL.Value (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.List (find, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble, double2Float, float2Double)
import Proto.Decode (
  DecodeError (..),
  Decoder,
  UnknownField (..),
  decodeAllVarints,
  decodeFail,
  decodeMessage,
  getFixed32,
  getFixed64,
  getLengthDelimited,
  getVarint,
  runDecoder,
  withTagM,
 )
import Proto.Google.Protobuf.Descriptor
import Proto.Google.Protobuf.Duration qualified as ProtoDuration
import Proto.Google.Protobuf.Timestamp qualified as ProtoTimestamp
import Proto.IDL.AST (Constant (..))
import Proto.IDL.Descriptor (descriptorDefaultConstant, descriptorFieldHasPresence)
import Protovalidate.Constraint (Constraint, mkAnnotationConstraint)
import Protovalidate.Rules


-- buf.validate extension number on FieldOptions / MessageOptions / OneofOptions.
bufValidateExtension :: Int
bufValidateExtension = 1159


----------------------------------------------------------------------
-- Extension byte extraction
----------------------------------------------------------------------

{- | The raw @buf.validate.FieldConstraints@ bytes attached to a field's
options, if present.
-}
fieldConstraintExtension :: FieldOptions -> Maybe BS.ByteString
fieldConstraintExtension = lenDelimUnknown bufValidateExtension . fldoUnknownFields


{- | The raw @buf.validate.MessageConstraints@ bytes attached to a message's
options, if present.
-}
messageConstraintExtension :: MessageOptions -> Maybe BS.ByteString
messageConstraintExtension = lenDelimUnknown bufValidateExtension . moUnknownFields


lenDelimUnknown :: Int -> [UnknownField] -> Maybe BS.ByteString
lenDelimUnknown number = mergeSubmessages . foldr collect []
  where
    collect (UnknownLenDelim n bytes) rest | n == number = bytes : rest
    collect _ rest = rest


mergeSubmessages :: [BS.ByteString] -> Maybe BS.ByteString
mergeSubmessages [] = Nothing
mergeSubmessages chunks = Just (BS.concat chunks)


----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

-- | Extract validation rules for every message defined in a file descriptor.
fileRulesFromDescriptor :: FileDescriptorProto -> Either Text [(Text, MessageRules)]
fileRulesFromDescriptor fdp =
  traverse extract messages
  where
    messages = descriptorMessages fdp
    enums = descriptorEnums fdp messages
    shapes = descriptorShapes fdp messages enums
    extract entry@(_, msg) =
      (,) (dpName msg) <$> extractMessage fdp messages enums shapes Set.empty entry


-- | Extract rules for a message by its qualified or simple name.
messageRulesFromDescriptor :: FileDescriptorProto -> Text -> Either Text MessageRules
messageRulesFromDescriptor fdp name =
  case find ((== T.dropWhile (== '.') name) . fst) messages of
    Just entry -> extract entry
    Nothing -> case find ((== name) . dpName . snd) messages of
      Just entry -> extract entry
      Nothing -> Left ("no such message: " <> name)
  where
    messages = descriptorMessages fdp
    enums = descriptorEnums fdp messages
    extract = extractMessage fdp messages enums (descriptorShapes fdp messages enums) Set.empty


type MessageDescriptors = [(Text, DescriptorProto)]


descriptorMessages :: FileDescriptorProto -> MessageDescriptors
descriptorMessages fdp = collect (fdpPackage fdp) (fdpMessageType fdp)
  where
    collect scope = concatMap (go scope) . V.toList
    go scope msg =
      let name = qualifiedName scope (dpName msg)
      in (name, msg) : collect name (dpNestedType msg)


type EnumDefinitions = Map.Map Text [EnumValueDescriptorProto]


descriptorEnums :: FileDescriptorProto -> MessageDescriptors -> EnumDefinitions
descriptorEnums fdp messages =
  Map.fromList (entries (fdpPackage fdp) (fdpEnumType fdp) ++ concatMap nested messages)
  where
    entries scope = map (\ed -> (qualifiedName scope (edpName ed), V.toList (edpValue ed))) . V.toList
    nested (scope, msg) = entries scope (dpEnumType msg)


qualifiedName :: Text -> Text -> Text
qualifiedName scope name
  | T.null scope = name
  | otherwise = scope <> "." <> name


descriptorShapes :: FileDescriptorProto -> MessageDescriptors -> EnumDefinitions -> ProtoSchema
descriptorShapes file messages enums = Map.fromList (map messageShape messages)
  where
    byName = Map.fromList messages
    messageShape (name, msg) = (name, Map.fromList (map fieldShape (V.toList (dpField msg))))
    fieldShape field = (fdpFieldName field, shape)
      where
        shape
          | fdpFieldLabel field /= 3 = descriptorValueShape file enums field
          | Just entry <- Map.lookup (descriptorTypeName field) byName
          , maybe False moMapEntry (dpOptions entry) =
              MapShape (maybe (ExplicitDefault VNull) (descriptorValueShape file enums) (V.find ((== 2) . fdpFieldNumber) (dpField entry)))
          | otherwise = ListShape (descriptorValueShape file enums field)


descriptorTypeName :: FieldDescriptorProto -> Text
descriptorTypeName = T.dropWhile (== '.') . fdpFieldTypeName


descriptorValueShape :: FileDescriptorProto -> EnumDefinitions -> FieldDescriptorProto -> ProtoFieldShape
descriptorValueShape file enums field = case fdpFieldType field of
  10 -> namedMessageShape name
  11 -> namedMessageShape name
  14 -> withPresence (VInt (fromIntegral enumDefault))
  code -> withPresence (fromMaybe fallback (descriptorScalarDefault code fallback <$> descriptorDefaultConstant field))
    where fallback = fromMaybe VNull (scalarDefaultValue code)
  where
    name = descriptorTypeName field
    supplied = fdpFieldDefault field
    withPresence = if descriptorFieldHasPresence file field then ExplicitDefault else ImplicitDefault
    enumDefault = case Map.findWithDefault [] name enums of
      [] -> 0
      first : rest -> evdpNumber (fromMaybe first (find ((== supplied) . evdpName) (first : rest)))


descriptorScalarDefault :: Int32 -> Value -> Constant -> Value
descriptorScalarDefault code fallback value = case (fallback, value) of
  (VInt _, CInt n) -> VInt (fromInteger n)
  (VUInt _, CInt n) -> VUInt (fromInteger n)
  (VBool _, CBool b) -> VBool b
  (VString _, CString text) -> VString text
  (VBytes _, CBytes bytes) -> VBytes bytes
  (VDouble _, CFloat n) -> VDouble (if code == 2 then float2Double (double2Float n) else n)
  _ -> fallback


----------------------------------------------------------------------
-- Message / field extraction
----------------------------------------------------------------------

-- Resolve nested rules only when the corresponding field rules are active.
type NestedRules = Either Text (Maybe NestedMessage)

data ValueTarget = ValueTarget ~NestedRules ~(Maybe [Integer])

data FieldTarget
  = SingleTarget !(Maybe Value) !ValueTarget
  | RepeatedTarget !ValueTarget
  | MapTarget !ValueTarget
  | ElementTarget !ValueTarget


extractMessage :: FileDescriptorProto -> MessageDescriptors -> EnumDefinitions -> ProtoSchema -> Set Text -> (Text, DescriptorProto) -> Either Text MessageRules
extractMessage file messages enums shapes visiting (name, dp) = do
  let fields = map (\fd -> (fd, fieldTarget fd)) (V.toList (dpField dp))
      (maps, ordinary) = partition (isMap . snd) fields
      known = Set.fromList (map (fdpFieldName . fst) fields)
  options <- maybe (Right []) scanMessage (dpOptions dp >>= messageConstraintExtension)
  groups <- traverse (messageOneofFromBytes name known) (submessageChunks 4 options)
  let members = Set.fromList (concatMap messageOneofFields groups)
  rules <- traverse (uncurry (extractField members)) (ordinary ++ maps)
  msgCustoms <- decodeCelRules 5 3 options
  oneofs <- catMaybes <$> traverse requiredOneof (zip [0 ..] (V.toList (dpOneofDecl dp)))
  Right (messageRules rules (msgCustoms ++ oneofs))
    { mrProtoSchema = Just (name, shapes)
    , mrMessageOneofs = groups
    }
  where
    requiredOneof (index, oneof) =
      case odpOptions oneof >>= lenDelimUnknown bufValidateExtension . oneofoUnknownFields of
        Nothing -> Right Nothing
        Just bytes -> do
          fields <- scanMessage bytes
          let members = V.toList (V.map fdpFieldName (V.filter ((== index) . fdpFieldOneofIdx) (dpField dp)))
          case lastVarint 1 fields of
            Just value | value /= 0 -> Right (Just (oneofRequired (odpName oneof) members))
            _ -> Right Nothing

    isMap (MapTarget _) = True
    isMap _ = False

    findMessage fd
      | fdpFieldType fd == 11 || fdpFieldType fd == 10 = find ((== descriptorTypeName fd) . fst) messages
      | otherwise = Nothing

    fieldTarget fd
      | fdpFieldLabel fd /= 3 = SingleTarget (fieldDefault fd) target
      | Just (_, entry) <- referenced
      , maybe False moMapEntry (dpOptions entry) =
          MapTarget (maybe emptyTarget valueTarget (V.find ((== 2) . fdpFieldNumber) (dpField entry)))
      | otherwise = RepeatedTarget target
      where
        referenced = findMessage fd
        target = ValueTarget (resolve referenced) (enumNumbers fd)

    valueTarget fd = ValueTarget (resolve (findMessage fd)) (enumNumbers fd)
    enumNumbers :: FieldDescriptorProto -> Maybe [Integer]
    enumNumbers fd
      | fdpFieldType fd == 14 = map (fromIntegral . evdpNumber) <$> Map.lookup (descriptorTypeName fd) enums
      | otherwise = Nothing

    fieldDefault fd
      | descriptorFieldHasPresence file fd = Nothing
      | Just (first : _) <- enumNumbers fd = Just (VInt (fromIntegral first))
      | otherwise = scalarDefaultValue (fdpFieldType fd)

    resolve (Just entry@(childName, _))
      | childName == name || Set.member childName visiting = Right (Just (RecursiveMessage childName))
      | otherwise =
          Just . InlineMessage <$> extractMessage file messages enums shapes (Set.insert name visiting) entry
    resolve _ = Right Nothing


emptyTarget :: ValueTarget
emptyTarget = ValueTarget (Right Nothing) Nothing


extractField :: Set Text -> FieldDescriptorProto -> FieldTarget -> Either Text (Text, FieldRules)
extractField members fd target = do
  fields <- maybe (Right []) scanMessage (fdpFieldOptions fd >>= fieldConstraintExtension)
  let name = fdpFieldName fd
      effective
        | Set.member name members, not (isJust (lastVarint 27 fields)) = (27, WVarint 1) : fields
        | otherwise = fields
  rules <- decodeFieldRules target effective
  Right (name, rules)


----------------------------------------------------------------------
-- buf.validate message decoding
----------------------------------------------------------------------

fieldConstraints :: FieldTarget -> BS.ByteString -> Either Text FieldRules
fieldConstraints target bytes = scanMessage bytes >>= decodeFieldRules target


decodeFieldRules :: FieldTarget -> [(Int, WV)] -> Either Text FieldRules
decodeFieldRules target fields
  | ignoreMode == Just 3 = Right emptyFieldRules
  | otherwise = do
      customs <- decodeCelRules 29 23 fields
      (kind, ruleFields) <- case selectedRule fields of
        Just (n, bytes) | Just k <- ruleKindOf n -> (Just k,) <$> scanMessage bytes
        _ -> Right (Nothing, [])
      let base =
            emptyFieldRules
              { frRequired = maybe False (/= 0) (lastVarint 25 fields)
              , frImplicitDefault = implicitDefault
              , frIgnoreEmpty = allowIgnoreZero && ignoreMode == Just 1
              , frCustom = customs
              }
      case (target, kind) of
        (MapTarget nested, _) ->
          mapRules base nested (if kind == Just KMap then ruleFields else [])
        (_, Just KMap) -> mapRules base emptyTarget ruleFields
        (RepeatedTarget nested, Just KRepeated) -> repeatedRules base nested ruleFields
        (_, Just KRepeated) -> repeatedRules base emptyTarget ruleFields
        (RepeatedTarget nested, _) -> do
          item <- scalarRules kind ruleFields nested emptyFieldRules
          let hasItems = isJust kind || isJust (frMessage item) || isJust (frMessageReference item)
          Right base {frKind = Just KRepeated, frItems = if hasItems then Just item else Nothing}
        (SingleTarget _ nested, _) -> scalarRules kind ruleFields nested base
        (ElementTarget nested, _) -> scalarRules kind ruleFields nested base
  where
    ignoreMode = lastVarint 27 fields
    implicitDefault = case target of
      SingleTarget value _ -> value
      RepeatedTarget _ -> Just (VList V.empty)
      MapTarget _ -> Just (VMap (celMapFromList []))
      ElementTarget _ -> Nothing
    allowIgnoreZero = case target of
      ElementTarget _ -> True
      _ -> isJust implicitDefault

    mapRules base nested ruleFields = do
      values <- decodeRuleMessage KMap ruleFields
      keys <- subRules 4 emptyTarget ruleFields
      items <- subRules 5 nested ruleFields
      Right base {frKind = Just KMap, frRules = values, frMapKeys = keys, frMapValues = items}

    repeatedRules base nested ruleFields = do
      values <- decodeRuleMessage KRepeated ruleFields
      items <- subRules 4 nested ruleFields
      Right base {frKind = Just KRepeated, frRules = values, frItems = items}

    subRules number target'@(ValueTarget nested _) ruleFields =
      case mergeSubmessages (submessageChunks number ruleFields) of
        Just bytes -> Just <$> fieldConstraints (ElementTarget target') bytes
        Nothing -> fmap (\message -> withNestedMessage (Just message) emptyFieldRules) <$> nested

    scalarRules kind ruleFields (ValueTarget nested enumNumbers) base = do
      values <- maybe (Right []) (\k -> decodeRuleMessage k ruleFields) kind
      context <- scalarContext kind ruleFields enumNumbers
      message <- nested
      Right $ withNestedMessage message base
        { frKind = kind
        , frRules = values
        , frCustom = frCustom base ++ context
        }


scalarContext :: Maybe RuleKind -> [(Int, WV)] -> Maybe [Integer] -> Either Text [Constraint]
scalarContext kind fields numbers = case kind of
  Just KEnum | maybe False (/= 0) (lastVarint 2 fields) ->
    maybe (Left "buf.validate enum.defined_only needs the enum definition") (Right . pure . definedOnly) numbers
  Just KString | Just code <- lastVarint 24 fields, code == 1 || code == 2 ->
    Right [wellKnownRegex (fromIntegral code) (lastVarint 25 fields /= Just 0)]
  _ -> Right []


submessageChunks :: Int -> [(Int, WV)] -> [BS.ByteString]
submessageChunks number = foldr collect []
  where
    collect (n, WLen bytes) rest | n == number = bytes : rest
    collect _ rest = rest


lastVarint :: Int -> [(Int, WV)] -> Maybe Word64
lastVarint number = foldl' select Nothing
  where
    select _ (n, WVarint word) | n == number = Just word
    select current _ = current


-- FieldRules.type is a oneof. The last member wins, and fragments of that
-- member merge until a different member is encountered.
selectedRule :: [(Int, WV)] -> Maybe (Int, BS.ByteString)
selectedRule fields = fmap (fmap (BS.concat . reverse)) (foldl' select Nothing fields)
  where
    select current (n, WLen bytes)
      | isJust (ruleKindOf n) || n == 20 || n == 28 =
          case current of
            Just (prev, chunks) | n == prev -> Just (n, bytes : chunks)
            _ -> Just (n, [bytes])
    select current _ = current


messageOneofFromBytes :: Text -> Set Text -> BS.ByteString -> Either Text MessageOneof
messageOneofFromBytes name known bytes = do
  fields <- scanMessage bytes
  members <- traverse ruleText (submessageChunks 1 fields)
  makeMessageOneof name known members (maybe False (/= 0) (lastVarint 2 fields))


decodeCelRules :: Int -> Int -> [(Int, WV)] -> Either Text [Constraint]
decodeCelRules expressionField ruleField fields = do
  expressions <- traverse fromExpression (submessageChunks expressionField fields)
  rules <- traverse constraintFromBytes (submessageChunks ruleField fields)
  Right (expressions ++ rules)
  where
    fromExpression bytes = do
      expression <- ruleText bytes
      mkAnnotationConstraint expression "" expression


-- Constraint { id=1 string, message=2 string, expression=3 string }
constraintFromBytes :: BS.ByteString -> Either Text Constraint
constraintFromBytes bytes = do
  fields <- scanMessage bytes
  cid <- lastText 1 fields
  msg <- lastText 2 fields
  expr <- lastText 3 fields
  mkAnnotationConstraint cid msg expr


lastText :: Int -> [(Int, WV)] -> Either Text Text
lastText number fields = case reverse (submessageChunks number fields) of
  bytes : _ -> ruleText bytes
  [] -> Right ""


ruleText :: BS.ByteString -> Either Text Text
ruleText = either (const (Left "invalid UTF-8 in buf.validate string rule")) Right . TE.decodeUtf8'


-- Map the FieldConstraints oneof field number to a RuleKind.
ruleKindOf :: Int -> Maybe RuleKind
ruleKindOf = \case
  1 -> Just KFloat
  2 -> Just KDouble
  3 -> Just KInt32
  4 -> Just KInt64
  5 -> Just KUint32
  6 -> Just KUint64
  7 -> Just KSint32
  8 -> Just KSint64
  9 -> Just KFixed32
  10 -> Just KFixed64
  11 -> Just KSfixed32
  12 -> Just KSfixed64
  13 -> Just KBool
  14 -> Just KString
  15 -> Just KBytes
  16 -> Just KEnum
  18 -> Just KRepeated
  19 -> Just KMap
  21 -> Just KDuration
  22 -> Just KTimestamp
  _ -> Nothing


-- Decode a rule submessage (e.g. StringRules) into (ruleName, value) pairs.
decodeRuleMessage :: RuleKind -> [(Int, WV)] -> Either Text [(Text, Value)]
decodeRuleMessage kind fields = do
  entries <- concat <$> traverse decodeEntry fields
  pure (mergeIn entries)
  where
    decodeEntry (n, wire) = case ruleName kind n of
      Just name -> map (name,) <$> decodeRuleValues kind name wire
      Nothing -> Right []

    -- Merge repeated in/not_in entries into a single list value.
    mergeIn entries =
      let names = nubOrd (map fst entries)
      in [(nm, combine nm [v | (k, v) <- entries, k == nm]) | nm <- names]
    combine nm vs
      | nm == "in" || nm == "not_in" = VList (V.fromList vs)
      | otherwise = last vs
    nubOrd = go Set.empty
      where
        go _ [] = []
        go seen (x : xs) | Set.member x seen = go seen xs | otherwise = x : go (Set.insert x seen) xs


-- Field-number -> rule-name tables per kind (buf.validate v1).
ruleName :: RuleKind -> Int -> Maybe Text
ruleName kind n = case kind of
  KString -> stringRuleName n
  KBytes -> bytesRuleName n
  KBool -> if n == 1 then Just "const" else Nothing
  KRepeated -> case n of 1 -> Just "min_items"; 2 -> Just "max_items"; 3 -> Just "unique"; _ -> Nothing
  KMap -> case n of 1 -> Just "min_pairs"; 2 -> Just "max_pairs"; _ -> Nothing
  KEnum -> case n of
    1 -> Just "const"
    3 -> Just "in"
    4 -> Just "not_in"
    _ -> Nothing
  KDuration -> case n of
    7 -> Just "in"
    8 -> Just "not_in"
    _ -> timeBoundName n
  KTimestamp -> case n of
    7 -> Just "lt_now"
    8 -> Just "gt_now"
    9 -> Just "within"
    _ -> timeBoundName n
  KFloat | n == 8 -> Just "finite"
  KDouble | n == 8 -> Just "finite"
  _ -> numericRuleName n


timeBoundName :: Int -> Maybe Text
timeBoundName n
  | n >= 2 && n <= 6 = numericRuleName (n - 1)
  | otherwise = Nothing


numericRuleName :: Int -> Maybe Text
numericRuleName = \case
  1 -> Just "const"
  2 -> Just "lt"
  3 -> Just "lte"
  4 -> Just "gt"
  5 -> Just "gte"
  6 -> Just "in"
  7 -> Just "not_in"
  _ -> Nothing


stringRuleName :: Int -> Maybe Text
stringRuleName = \case
  1 -> Just "const"
  2 -> Just "min_len"
  3 -> Just "max_len"
  4 -> Just "min_bytes"
  5 -> Just "max_bytes"
  6 -> Just "pattern"
  7 -> Just "prefix"
  8 -> Just "suffix"
  9 -> Just "contains"
  10 -> Just "in"
  11 -> Just "not_in"
  12 -> Just "email"
  13 -> Just "hostname"
  14 -> Just "ip"
  15 -> Just "ipv4"
  16 -> Just "ipv6"
  17 -> Just "uri"
  18 -> Just "uri_ref"
  19 -> Just "len"
  20 -> Just "len_bytes"
  21 -> Just "address"
  22 -> Just "uuid"
  23 -> Just "not_contains"
  26 -> Just "ip_with_prefixlen"
  27 -> Just "ipv4_with_prefixlen"
  28 -> Just "ipv6_with_prefixlen"
  29 -> Just "ip_prefix"
  30 -> Just "ipv4_prefix"
  31 -> Just "ipv6_prefix"
  32 -> Just "host_and_port"
  33 -> Just "tuuid"
  _ -> Nothing


bytesRuleName :: Int -> Maybe Text
bytesRuleName = \case
  1 -> Just "const"
  2 -> Just "min_len"
  3 -> Just "max_len"
  5 -> Just "prefix"
  6 -> Just "suffix"
  7 -> Just "contains"
  8 -> Just "in"
  9 -> Just "not_in"
  10 -> Just "ip"
  11 -> Just "ipv4"
  12 -> Just "ipv6"
  13 -> Just "len"
  _ -> Nothing


countRuleNames :: [Text]
countRuleNames =
  ["min_len", "max_len", "len", "min_bytes", "max_bytes", "len_bytes", "min_items", "max_items", "min_pairs", "max_pairs"]


flagRules :: [Text]
flagRules =
  [ "email", "hostname", "ip", "ipv4", "ipv6", "uri", "uri_ref", "address", "uuid", "tuuid", "unique"
  , "ip_prefix", "ipv4_prefix", "ipv6_prefix", "ip_with_prefixlen", "ipv4_with_prefixlen", "ipv6_with_prefixlen"
  , "host_and_port", "finite", "lt_now", "gt_now"
  ]


-- A field with a different wire type is unknown to the schema. Length-delimited
-- numeric lists also have a valid packed representation, even in proto2.
decodeRuleValues :: RuleKind -> Text -> WV -> Either Text [Value]
decodeRuleValues kind name wire
  | name `elem` countRuleNames = Right (fromVarint VUInt wire)
  | name `elem` flagRules || kind == KBool = Right (fromVarint (VBool . (/= 0)) wire)
  | kind == KString, WLen bytes <- wire = do
      value <- ruleText bytes
      Right [VString value]
  | kind == KBytes, WLen bytes <- wire = Right [VBytes bytes]
  | kind == KDuration || (kind == KTimestamp && name == "within") = case wire of
      WLen bytes -> do
        duration <- mapDecodeError (decodeMessage bytes)
        Right [VDuration (Duration (ProtoDuration.durationSeconds duration) (ProtoDuration.durationNanos duration))]
      _ -> Right []
  | kind == KTimestamp = case wire of
      WLen bytes -> do
        timestamp <- mapDecodeError (decodeMessage bytes)
        Right [VTimestamp (Timestamp (ProtoTimestamp.timestampSeconds timestamp) (ProtoTimestamp.timestampNanos timestamp))]
      _ -> Right []
  | name == "in" || name == "not_in"
  , WLen bytes <- wire = concatMap (numericValues kind) <$> unpackNumeric kind bytes
  | otherwise = Right (numericValues kind wire)


fromVarint :: (Word64 -> Value) -> WV -> [Value]
fromVarint f (WVarint word) = [f word]
fromVarint _ _ = []


numericValues :: RuleKind -> WV -> [Value]
numericValues kind wire = case (kind, wire) of
  (KFloat, WI32 word) -> [VDouble (float2Double (castWord32ToFloat word))]
  (KDouble, WI64 word) -> [VDouble (castWord64ToDouble word)]
  (KFixed32, WI32 word) -> [VUInt (fromIntegral word)]
  (KFixed64, WI64 word) -> [VUInt word]
  (KSfixed32, WI32 word) -> [signed32 word]
  (KSfixed64, WI64 word) -> [VInt (fromIntegral word)]
  (KInt32, WVarint word) -> [signed32 word]
  (KEnum, WVarint word) -> [signed32 word]
  (KInt64, WVarint word) -> [VInt (fromIntegral word)]
  (KUint32, WVarint word) -> [VUInt (fromIntegral (fromIntegral word :: Word32))]
  (KUint64, WVarint word) -> [VUInt word]
  (KSint32, WVarint word) -> [VInt (zigzag (fromIntegral (fromIntegral word :: Word32)))]
  (KSint64, WVarint word) -> [VInt (zigzag word)]
  _ -> []
  where
    signed32 :: Integral a => a -> Value
    signed32 word = VInt (fromIntegral (fromIntegral word :: Int32))


unpackNumeric :: RuleKind -> BS.ByteString -> Either Text [WV]
unpackNumeric kind bytes
  | kind `elem` [KFloat, KFixed32, KSfixed32] = fixed 4 (WI32 <$> getFixed32)
  | kind `elem` [KDouble, KFixed64, KSfixed64] = fixed 8 (WI64 <$> getFixed64)
  | otherwise = map WVarint . VU.toList <$> mapDecodeError (decodeAllVarints bytes)
  where
    fixed width readWord
      | remainder == 0 = mapDecodeError (runDecoder (replicateM count readWord) bytes)
      | otherwise = Left "invalid length for packed buf.validate fixed-width rule"
      where
        (count, remainder) = BS.length bytes `quotRem` width


----------------------------------------------------------------------
-- Protobuf wire scanning via the wireform-proto decoder
----------------------------------------------------------------------

data WV = WVarint !Word64 | WI64 !Word64 | WI32 !Word32 | WLen !BS.ByteString
  deriving stock (Show)


zigzag :: Word64 -> Int64
zigzag w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))


{- | Scan a protobuf message into @(fieldNumber, value)@ pairs, preserving
repeated fields, using the @wireform-proto@ wire decoder. Unlike a hand-rolled
scanner, malformed wire data surfaces as a 'DecodeError' rather than a
silently truncated field list, and group wire types are rejected.
-}
scanMessage :: BS.ByteString -> Either Text [(Int, WV)]
scanMessage = mapDecodeError . runDecoder scanLoop


mapDecodeError :: Either DecodeError a -> Either Text a
mapDecodeError = \case
  Right fs -> Right fs
  Left e -> Left ("malformed buf.validate wire data: " <> T.pack (show e))


-- A field loop built on the decoder's tag dispatch.
scanLoop :: Decoder [(Int, WV)]
scanLoop = go []
  where
    go acc =
      withTagM (pure (reverse acc)) $ \fn wt -> do
        wv <- readWireValue wt
        go ((fn, wv) : acc)


readWireValue :: Int -> Decoder WV
readWireValue = \case
  0 -> WVarint <$> getVarint
  1 -> WI64 <$> getFixed64
  5 -> WI32 <$> getFixed32
  2 -> WLen <$> getLengthDelimited
  wt -> decodeFail (InvalidWireType wt)
