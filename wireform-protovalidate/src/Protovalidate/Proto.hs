{-# LANGUAGE OverloadedStrings #-}

{- | Bridge from @wireform-proto@ dynamic messages to the CEL value model used
by the validator.

A 'Proto.Dynamic.DynamicMessage' stores fields by number and (being
schemaless) cannot by itself tell @int64@ from @uint64@ or name its fields.
Callers therefore supply a 'MessageSchema' describing each field's number,
name, and shape; this module walks the message and produces a CEL 'VMap'
suitable for "Protovalidate.Eval.validate".
-}
module Protovalidate.Proto (
  MessageSchema,
  FieldSchema (..),
  FieldShape (..),
  dynamicMessageToCel,
  dynamicValueToCel,
) where

import CEL.Value (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble, float2Double)
import Proto.Decode (DecodeError, MessageDecode, decodeAllVarints, decodeMessage, decodePackedFixed32, decodePackedFixed64, runDecoder)
import Proto.Dynamic (DynamicMessage (..), DynamicValue (..), decodeDynamic, encodeDynamic)
import Proto.Encode (messageToByteString)
import Proto.Google.Protobuf.Duration qualified as ProtoDuration
import Proto.Google.Protobuf.Duration.Util (isValidDuration)
import Proto.Google.Protobuf.Timestamp qualified as ProtoTimestamp
import Proto.Google.Protobuf.Timestamp.Util (isValidTimestamp)
import Proto.Internal.Wire (WireType (..))
import Proto.Internal.Wire.Decode (getGroupBody)
import Proto.Internal.Wire.Encode (putLengthDelimited)
import Protovalidate.Rules (RuleKind (..))


{- | A description of a message's fields, used to name and type-interpret a
dynamic message.
-}
type MessageSchema = [FieldSchema]


-- | One field's number, name, and shape.
data FieldSchema = FieldSchema
  { fsNumber :: !Int
  , fsName :: !Text
  , fsShape :: !FieldShape
  }
  deriving stock (Show)


-- | The shape of a field's value.
data FieldShape
  = -- | A CEL scalar. 'KTimestamp' and 'KDuration' use embedded messages on the wire.
    ScalarField !RuleKind
  | -- | A nested message with its own schema.
    MessageField !MessageSchema
  | -- | A nested message delimited by matching group tags.
    GroupField !MessageSchema
  | -- | A repeated field of the given element shape.
    RepeatedField !FieldShape
  deriving stock (Show)


{- | Convert a dynamic message into a CEL 'VMap' keyed by field name, using the
supplied schema. Fields outside the schema and records with a mismatched wire
type are ignored. A malformed payload with the declared wire type keeps its
raw value.
-}
dynamicMessageToCel :: MessageSchema -> DynamicMessage -> Value
dynamicMessageToCel schema msg =
  VMap (celMapFromList (mapMaybe convertField schema))
  where
    convertField fsch =
      let number = fsNumber fsch
          shape = fsShape fsch
          value = case shape of
            GroupField _ -> lookupGroup number
            RepeatedField (GroupField _) -> lookupGroup number
            _ -> Map.lookup number (dynFields msg) >>= matchingOccurrences (acceptsOccurrence shape)
      in fmap (\dv -> (VString (fsName fsch), convertShaped shape dv)) value

    lookupGroup number = case Map.lookup number (dynFields msg) >>= matchingOccurrences isGroupValue of
      Just value -> Just value
      _ -> fmap (occurrenceValue . map (decodeGroup number) . reverse) (Map.lookup number groupPayloads)

    groupPayloads = foldl' collectGroup Map.empty (dynUnknownFields msg)
    collectGroup groups (number, WireStartGroup, payload) = Map.insertWith (<>) number [payload] groups
    collectGroup groups _ = groups
    occurrenceValue [value] = value
    occurrenceValue values = DynRepeated values

    -- Schemaless decoding validates group framing and retains the original
    -- payload, including the closing tag. Interpret it only for group fields.
    decodeGroup number payload =
      case runDecoder (getGroupBody number) payload >>= decodeDynamic of
        Left _ -> DynBytes payload
        Right child -> DynGroup child


convertShaped :: FieldShape -> DynamicValue -> Value
convertShaped shape dv = case shape of
  ScalarField KTimestamp ->
    temporalValue isValidTimestamp
      (\ts -> VTimestamp (Timestamp (ProtoTimestamp.timestampSeconds ts) (ProtoTimestamp.timestampNanos ts))) dv
  ScalarField KDuration ->
    temporalValue isValidDuration
      (\dur -> VDuration (Duration (ProtoDuration.durationSeconds dur) (ProtoDuration.durationNanos dur))) dv
  ScalarField k | DynRepeated (first : rest) <- dv, Just _ <- scalarKindWire k ->
    coerceScalar k (foldl' (\_ next -> next) first rest)
  ScalarField k -> coerceScalar k dv
  MessageField sub ->
    maybe (dynamicValueToCel dv) (dynamicMessageToCel sub) (messagePayload dv)
  GroupField sub
    | isGroupValue dv -> convertShaped (MessageField sub) dv
    | otherwise -> dynamicValueToCel dv
  RepeatedField elemShape ->
    VList (V.fromList (foldr (prependElement elemShape) [] occurrences))
  where
    occurrences = case dv of
      DynRepeated xs -> xs
      _ -> [dv]


matchingOccurrences :: (DynamicValue -> Bool) -> DynamicValue -> Maybe DynamicValue
matchingOccurrences accepts (DynRepeated values) = case filter accepts values of
  [] -> Nothing
  matches -> Just (DynRepeated matches)
matchingOccurrences accepts value
  | accepts value = Just value
  | otherwise = Nothing


acceptsOccurrence :: FieldShape -> DynamicValue -> Bool
acceptsOccurrence shape value = case shape of
  ScalarField KTimestamp -> isMessageValue value
  ScalarField KDuration -> isMessageValue value
  ScalarField kind -> case scalarKindWire kind of
    Nothing -> True
    Just wire -> scalarValueWire value == Just wire
  MessageField _ -> isMessageValue value
  GroupField _ -> isGroupValue value
  RepeatedField (ScalarField _) | DynBytes _ <- value -> True
  RepeatedField element -> acceptsOccurrence element value


isMessageValue :: DynamicValue -> Bool
isMessageValue (DynBytes _) = True
isMessageValue (DynMessage _) = True
isMessageValue (DynGroup _) = True
isMessageValue _ = False


isGroupValue :: DynamicValue -> Bool
isGroupValue (DynGroup _) = True
isGroupValue (DynRepeated values) = all isGroupValue values
isGroupValue _ = False


messagePayload :: DynamicValue -> Maybe DynamicMessage
messagePayload = \case
  DynMessage message -> Just message
  DynGroup message -> Just message
  DynBytes bytes -> parsed bytes
  DynRepeated [] -> Nothing
  DynRepeated [occurrence] -> messagePayload occurrence
  DynRepeated occurrences -> traverse fragment occurrences >>= parsed . BS.concat
  _ -> Nothing
  where
    parsed = either (const Nothing) Just . decodeDynamic
    fragment = \case
      DynBytes bytes -> bytes <$ parsed bytes
      DynMessage message -> Just (encodeDynamic message)
      DynGroup message -> Just (encodeDynamic message)
      _ -> Nothing


temporalValue :: MessageDecode a => (a -> Bool) -> (a -> Value) -> DynamicValue -> Value
temporalValue valid project value =
  case payload >>= either (const Nothing) Just . decodeMessage of
    Just time | valid time -> project time
    _ -> dynamicValueToCel value
  where
    payload = case value of
      DynBytes bytes -> Just bytes
      _ -> encodeDynamic <$> messagePayload value


prependElement :: FieldShape -> DynamicValue -> [Value] -> [Value]
prependElement (ScalarField kind) (DynBytes bytes) rest
  | Just packed <- case scalarKindWire kind of
      Just WireVarint -> values DynVarint (decodeAllVarints bytes)
      Just Wire32Bit -> values DynFixed32 (runDecoder decodePackedFixed32 framed)
      Just Wire64Bit -> values DynFixed64 (runDecoder decodePackedFixed64 framed)
      _ -> Nothing =
    packed
  where
    framed = messageToByteString (putLengthDelimited bytes)
    values :: VU.Unbox a => (a -> DynamicValue) -> Either DecodeError (VU.Vector a) -> Maybe [Value]
    values wrap = either (const Nothing) (Just . VU.foldr (\bits tailValues -> coerceScalar kind (wrap bits) : tailValues) rest)
prependElement shape dv rest = convertShaped shape dv : rest


scalarKindWire :: RuleKind -> Maybe WireType
scalarKindWire = \case
  KBool -> Just WireVarint
  KEnum -> Just WireVarint
  KInt32 -> Just WireVarint
  KInt64 -> Just WireVarint
  KUint32 -> Just WireVarint
  KUint64 -> Just WireVarint
  KSint32 -> Just WireVarint
  KSint64 -> Just WireVarint
  KFixed32 -> Just Wire32Bit
  KSfixed32 -> Just Wire32Bit
  KFloat -> Just Wire32Bit
  KFixed64 -> Just Wire64Bit
  KSfixed64 -> Just Wire64Bit
  KDouble -> Just Wire64Bit
  KString -> Just WireLengthDelimited
  KBytes -> Just WireLengthDelimited
  _ -> Nothing


scalarValueWire :: DynamicValue -> Maybe WireType
scalarValueWire = \case
  DynBool _ -> Just WireVarint
  DynEnum _ -> Just WireVarint
  DynVarint _ -> Just WireVarint
  DynSVarint _ -> Just WireVarint
  DynFixed32 _ -> Just Wire32Bit
  DynFloat _ -> Just Wire32Bit
  DynFixed64 _ -> Just Wire64Bit
  DynDouble _ -> Just Wire64Bit
  DynString _ -> Just WireLengthDelimited
  DynBytes _ -> Just WireLengthDelimited
  _ -> Nothing


-- | Interpret a scalar dynamic value according to a 'RuleKind'.
coerceScalar :: RuleKind -> DynamicValue -> Value
coerceScalar k dv = case dv of
  DynBool b -> VBool b
  DynString s -> VString s
  DynBytes b
    | k == KString, Right text <- TE.decodeUtf8' b -> VString text
    | otherwise -> VBytes b
  DynFloat f -> VDouble (float2Double f)
  DynDouble d -> VDouble d
  DynEnum e -> VInt (fromIntegral e)
  DynSVarint i -> VInt i
  DynVarint w -> fromVarint k w
  DynFixed32 w -> fromFixed k (fromIntegral w)
  DynFixed64 w -> fromFixed k w
  DynRepeated xs -> VList (V.fromList (map (coerceScalar k) xs))
  DynMessage m -> dynamicValueToCel (DynMessage m)
  DynGroup m -> dynamicValueToCel (DynMessage m)
  DynMap m -> dynamicValueToCel (DynMap m)
  where
    fromVarint kind w = case kind of
      KBool -> VBool (w /= 0)
      KInt32 -> VInt (signed32 w)
      KEnum -> VInt (signed32 w)
      KUint32 -> VUInt (unsigned32 w)
      KUint64 -> VUInt w
      KSint32 -> VInt (zigzag (unsigned32 w))
      KSint64 -> VInt (zigzag w)
      _ -> VInt (fromIntegral w :: Int64)
    fromFixed kind w = case kind of
      KUint32 -> VUInt w
      KUint64 -> VUInt w
      KFixed32 -> VUInt w
      KFixed64 -> VUInt w
      KFloat -> VDouble (float2Double (wordToFloat w))
      KDouble -> VDouble (wordToDouble w)
      KSfixed32 -> VInt (signed32 w)
      _ -> VInt (fromIntegral w :: Int64)

    signed32 :: Word64 -> Int64
    signed32 w = fromIntegral (fromIntegral w :: Int32)

    unsigned32 :: Word64 -> Word64
    unsigned32 w = fromIntegral (fromIntegral w :: Word32)


{- | A best-effort, schema-less conversion of a dynamic value. Varints become
unsigned, nested messages become field-number-keyed maps. Prefer
'dynamicMessageToCel' with a schema where field names / signedness matter.
-}
dynamicValueToCel :: DynamicValue -> Value
dynamicValueToCel = \case
  DynVarint w -> VUInt w
  DynSVarint i -> VInt i
  DynFixed32 w -> VUInt (fromIntegral w)
  DynFixed64 w -> VUInt w
  DynFloat f -> VDouble (float2Double f)
  DynDouble d -> VDouble d
  DynBool b -> VBool b
  DynString s -> VString s
  DynBytes b -> VBytes b
  DynEnum e -> VInt (fromIntegral e)
  DynRepeated xs -> VList (V.fromList (map dynamicValueToCel xs))
  DynMessage m ->
    VMap (celMapFromList [(VInt (fromIntegral n), dynamicValueToCel v) | (n, v) <- Map.toList (dynFields m)])
  DynGroup m -> dynamicValueToCel (DynMessage m)
  DynMap m ->
    VMap (celMapFromList [(dynamicValueToCel kk, dynamicValueToCel vv) | (kk, vv) <- Map.toList m])


zigzag :: Word64 -> Int64
zigzag w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))


-- Reinterpret the bits of a 32/64-bit word as IEEE-754 float/double.
wordToFloat :: Word64 -> Float
wordToFloat w = castWord32ToFloat (fromIntegral w)


wordToDouble :: Word64 -> Double
wordToDouble = castWord64ToDouble
