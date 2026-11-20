{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | Runtime value model for the Common Expression Language (CEL).

This module defines 'Value', the dynamically-typed value that a CEL
expression evaluates to, the 'CelType' tag returned by the @type()@
function, and the equality / ordering semantics mandated by the CEL
language definition.

The semantics implemented here follow
<https://github.com/google/cel-spec/blob/master/doc/langdef.md>:

  * Numeric values (@int@, @uint@, @double@) are compared as though they
    lie on a single continuous number line, so @1 == 1u == 1.0@ all hold
    and ordering works across the numeric types
    ('compareValues' / 'valueEq').
  * @NaN@ never compares equal to anything (including itself) and is
    unordered with respect to every value.
  * Equality is heterogeneous: comparing two values of different,
    non-numeric types yields @false@ rather than an error.
  * @timestamp@ and @duration@ are the built-in abstract types backed by
    @google.protobuf.Timestamp@ / @google.protobuf.Duration@.
-}
module CEL.Value (
  Value
    ( VNull
    , VBool
    , VInt
    , VUInt
    , VDouble
    , VString
    , VBytes
    , VList
    , VMap
    , VMessage
    , VAny
    , VAnyPayload
    , VType
    , VTimestamp
    , VDuration
    ),
  AnyPayload (..),
  CelType (..),
  Timestamp (..),
  Duration (..),
  CelMap,
  celMap,
  celMapFromList,
  celMapEntries,
  traverseCelMapValues,
  celMapLookup,
  celMapSize,
  CelMessage (CelMessage),
  celMessageWithUnknownFields,
  celMessageUnknownFields,
  equalUnknownFields,
  CelField (..),
  celFieldPresent,
  celFieldStoredValue,
  selectCelField,
  unpackValue,
  typeOf,
  typeName,
  typeNameText,

  -- * Equality and ordering
  valueEq,
  compareValues,
  isNumeric,

  -- * Numeric helpers
  durationNanos,
  timestampNanos,
) where

import CEL.Error (CelError)
import CEL.Internal.UnknownFields (equalUnknownFields)
import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.List (partition)
import Data.Map.Lazy qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)


{- | A @google.protobuf.Duration@: a signed span of seconds plus nanoseconds.
The nanosecond component carries the same sign as 'durSeconds' for non-zero
values (matching the protobuf normalization rules).
-}
data Duration = Duration
  { durSeconds :: {-# UNPACK #-} !Int64
  , durNanos :: {-# UNPACK #-} !Int32
  }
  deriving stock (Show, Generic)


instance NFData Duration


{- | A @google.protobuf.Timestamp@: seconds since the Unix epoch plus a
non-negative nanosecond component in @[0, 1e9)@.
-}
data Timestamp = Timestamp
  { tsSeconds :: {-# UNPACK #-} !Int64
  , tsNanos :: {-# UNPACK #-} !Int32
  }
  deriving stock (Show, Generic)


instance NFData Timestamp


-- | Total nanoseconds represented by a duration.
durationNanos :: Duration -> Integer
durationNanos (Duration s n) = fromIntegral s * 1000000000 + fromIntegral n


-- | Total nanoseconds since the Unix epoch represented by a timestamp.
timestampNanos :: Timestamp -> Integer
timestampNanos (Timestamp s n) = fromIntegral s * 1000000000 + fromIntegral n


instance Eq Duration where
  a == b = durationNanos a == durationNanos b


instance Ord Duration where
  compare a b = compare (durationNanos a) (durationNanos b)


instance Eq Timestamp where
  a == b = timestampNanos a == timestampNanos b


instance Ord Timestamp where
  compare a b = compare (timestampNanos a) (timestampNanos b)


{- | The runtime type of a CEL value, as returned by the @type()@ function.
'TyMessage' carries the fully-qualified name of a protobuf or abstract type.
-}
data CelType
  = TyNull
  | TyBool
  | TyInt
  | TyUInt
  | TyDouble
  | TyString
  | TyBytes
  | TyList
  | TyMap
  | TyType
  | TyTimestamp
  | TyDuration
  | TyMessage !Text
  deriving stock (Eq, Show, Generic, Lift)


instance NFData CelType


{- | The canonical CEL name of a type, e.g. @"int"@, @"null_type"@,
@"google.protobuf.Timestamp"@.
-}
typeNameText :: CelType -> Text
typeNameText = \case
  TyNull -> "null_type"
  TyBool -> "bool"
  TyInt -> "int"
  TyUInt -> "uint"
  TyDouble -> "double"
  TyString -> "string"
  TyBytes -> "bytes"
  TyList -> "list"
  TyMap -> "map"
  TyType -> "type"
  TyTimestamp -> "google.protobuf.Timestamp"
  TyDuration -> "google.protobuf.Duration"
  TyMessage n -> n


-- | Alias for 'typeNameText'.
typeName :: CelType -> Text
typeName = typeNameText


{- | A CEL value.

Maps preserve insertion order and reject duplicate keys at construction
time (see 'celMap'). Map keys are restricted by the language to @int@,
@uint@, @bool@, and @string@, but the representation does not enforce that
invariant; the evaluator does when constructing map literals.
-}
data Value
  = VNull
  | VBool !Bool
  | VInt {-# UNPACK #-} !Int64
  | VUInt {-# UNPACK #-} !Word64
  | VDouble {-# UNPACK #-} !Double
  | VString !Text
  | VBytes !ByteString
  | VList !(Vector Value)
  | VMap !CelMap
  | VMessage !CelMessage
  -- | The type URL and bytes are available for equality when decoding fails.
  | VAnyPayload !Text !ByteString ~(Either CelError AnyPayload)
  | VType !CelType
  | VTimestamp !Timestamp
  | VDuration !Duration
  deriving stock (Show, Generic)


instance NFData Value


-- | The decoded protobuf value used for equality and its CEL projection.
data AnyPayload = AnyPayload
  { anyProtobufValue :: ~Value
  , anyCelValue :: ~(Either CelError Value)
  }
  deriving stock (Show, Generic)


instance NFData AnyPayload


-- | An Any supplied as a CEL value. A wire decoder can use 'VAnyPayload' to
-- retain the full protobuf message when projection removes fields.
pattern VAny :: Text -> ByteString -> Either CelError Value -> Value
pattern VAny url bytes adapted <- VAnyPayload url bytes ((>>= anyCelValue) -> adapted)
  where VAny url bytes adapted = VAnyPayload url bytes (fmap (\value -> AnyPayload value (Right value)) adapted)


{-# COMPLETE VNull, VBool, VInt, VUInt, VDouble, VString, VBytes, VList, VMap, VMessage, VAny, VType, VTimestamp, VDuration #-}


-- | Read the CEL value represented by an Any, leaving collection members
-- deferred until they are used.
unpackValue :: Value -> Either CelError Value
unpackValue (VAnyPayload _ _ decoded) = decoded >>= anyCelValue >>= unpackValue
unpackValue value = Right value


-- | A protobuf message as seen by CEL. The value of an absent field is its
-- selection default, which can itself be a lazily constructed message.
data CelMessage = Message !Text !(Map.Map Text CelField) !ByteString
  deriving stock (Generic)


-- | A message's type and declared fields. Constructing one this way starts
-- with no unknown fields.
pattern CelMessage :: Text -> Map.Map Text CelField -> CelMessage
pattern CelMessage name fields <- Message name fields _
  where CelMessage name fields = Message name fields BS.empty


{-# COMPLETE CelMessage #-}


-- | Construct a message with the original wire bytes of its unknown fields.
celMessageWithUnknownFields :: Text -> Map.Map Text CelField -> ByteString -> CelMessage
celMessageWithUnknownFields = Message


celMessageUnknownFields :: CelMessage -> ByteString
celMessageUnknownFields (Message _ _ unknown) = unknown


data CelField
  = CelField !Bool ~Value
  -- | The stored value participates in protobuf message equality. Reading
  -- the field performs CEL adaptation, which can fail for an unresolved Any.
  | CelFieldWithSelection !Bool ~Value ~(Either CelError Value)
  deriving stock (Show, Generic)


celFieldPresent :: CelField -> Bool
celFieldPresent (CelField present _) = present
celFieldPresent (CelFieldWithSelection present _ _) = present


celFieldStoredValue :: CelField -> Value
celFieldStoredValue (CelField _ value) = value
celFieldStoredValue (CelFieldWithSelection _ value _) = value


selectCelField :: CelField -> Either CelError Value
selectCelField (CelField _ value) = Right value
selectCelField (CelFieldWithSelection _ _ selected) = selected


instance Show CelMessage where
  showsPrec p (Message name fields unknown) =
    showParen (p > 10) (showString "CelMessage " . showsPrec 11 name . showChar ' ' . showsPrec 11 (presentFields fields) . showUnknown)
    where
      showUnknown
        | BS.null unknown = id
        | otherwise = showString " unknown=" . shows unknown


instance NFData CelMessage where
  rnf (Message name fields unknown) = rnf name `seq` rnf (presentFields fields) `seq` rnf unknown


presentFields :: Map.Map Text CelField -> Map.Map Text Value
presentFields = Map.mapMaybe (\field -> if celFieldPresent field then Just (celFieldStoredValue field) else Nothing)


{- | An insertion-ordered association of CEL keys to CEL values with unique
keys, using the same equivalence as 'celMapLookup'. For example, @1@ and @1u@
are the same key.
-}
newtype CelMap = CelMap {celMapEntries :: [(Value, Value)]}
  deriving stock (Show, Generic)


instance NFData CelMap


-- | Transform each value with its key, preserving the keys and their order.
traverseCelMapValues :: Applicative f => (Value -> Value -> f Value) -> CelMap -> f CelMap
traverseCelMapValues f (CelMap entries) =
  CelMap <$> traverse (\(key, value) -> (key,) <$> f key value) entries


-- | Number of entries in a map.
celMapSize :: CelMap -> Int
celMapSize (CelMap es) = length es


{- | Build a map from insertion-ordered entries, returning 'Left' on a
duplicate key (per CEL semantics, duplicate map keys are an error).
-}
celMap :: [(Value, Value)] -> Either Text CelMap
celMap = go []
  where
    go acc [] = Right (CelMap (reverse acc))
    go acc ((k, v) : rest)
      | any (\(k', _) -> mapKeyEq k k') acc =
          Left ("duplicate key in map: " <> T.pack (show k))
      | otherwise = go ((k, v) : acc) rest


{- | Build a map from entries, keeping the last value for duplicate keys
in the position and key representation of their first occurrence.
-}
celMapFromList :: [(Value, Value)] -> CelMap
celMapFromList entries
  | ascendingIndices entries = CelMap entries
  | otherwise = CelMap $ case traverse indexEntry entries of
      Just indexed -> inInsertionOrder (Map.fromList indexed) indexed
      Nothing -> deduplicateOtherKeys entries
  where
    indexEntry entry@(k, _) = (,entry) <$> indexKey k

    inInsertionOrder _ [] = []
    inInsertionOrder values ((indexedKey, (k, _)) : rest) =
      case Map.lookup indexedKey values of
        Just (_, v) -> (k, v) : inInsertionOrder (Map.delete indexedKey values) rest
        Nothing -> inInsertionOrder values rest

    -- Host callers can supply keys outside the CEL language's key types.
    deduplicateOtherKeys [] = []
    deduplicateOtherKeys ((k, v) : rest) =
      let (duplicates, others) = partition (mapKeyEq k . fst) rest
          final = foldl' (\_ entry -> entry) (k, v) duplicates
      in (k, snd final) : deduplicateOtherKeys others


-- List transformMap produces an increasing subsequence of the input indices.
ascendingIndices :: [(Value, Value)] -> Bool
ascendingIndices [] = True
ascendingIndices ((VInt i, _) : rest) = go i rest
  where
    go _ [] = True
    go prev ((VInt next, _) : xs) = prev < next && go next xs
    go _ _ = False
ascendingIndices _ = False


data MapKey = IntegerKey !Integer | BoolKey !Bool | StringKey !Text
  deriving stock (Eq, Ord)


indexKey :: Value -> Maybe MapKey
indexKey = \case
  VInt i -> Just (IntegerKey (toInteger i))
  VUInt u -> Just (IntegerKey (toInteger u))
  VBool b -> Just (BoolKey b)
  VString s -> Just (StringKey s)
  _ -> Nothing


-- | Look up a key, allowing only lossless numeric conversions between key
-- types.
celMapLookup :: Value -> CelMap -> Maybe Value
celMapLookup k (CelMap es) = go es
  where
    go [] = Nothing
    go ((k', v) : rest)
      | mapKeyEq k k' = Just v
      | otherwise = go rest


mapKeyEq :: Value -> Value -> Bool
mapKeyEq a b = case (a, b) of
  (VDouble d, VInt i) -> exactDoubleKey d (toInteger i)
  (VInt i, VDouble d) -> exactDoubleKey d (toInteger i)
  (VDouble d, VUInt u) -> exactDoubleKey d (toInteger u)
  (VUInt u, VDouble d) -> exactDoubleKey d (toInteger u)
  _ -> valueEq a b


exactDoubleKey :: Double -> Integer -> Bool
exactDoubleKey d expected
  | isNaN d || isInfinite d = False
  | otherwise =
      let whole = truncate d :: Integer
      in whole == expected && fromInteger whole == d


-- | The runtime 'CelType' of a value.
typeOf :: Value -> CelType
typeOf = \case
  VNull -> TyNull
  VBool _ -> TyBool
  VInt _ -> TyInt
  VUInt _ -> TyUInt
  VDouble _ -> TyDouble
  VString _ -> TyString
  VBytes _ -> TyBytes
  VList _ -> TyList
  VMap _ -> TyMap
  VMessage (CelMessage name _) -> TyMessage name
  value@(VAny _ _ _) -> either (const (TyMessage "google.protobuf.Any")) typeOf (unpackValue value)
  VType _ -> TyType
  VTimestamp _ -> TyTimestamp
  VDuration _ -> TyDuration


instance Eq Value where
  (==) = valueEq


-- | Is this value one of the numeric types (@int@, @uint@, @double@)?
isNumeric :: Value -> Bool
isNumeric = \case
  VInt _ -> True
  VUInt _ -> True
  VDouble _ -> True
  value@(VAny _ _ _) -> either (const False) isNumeric (unpackValue value)
  _ -> False


-- Internal three-way numeric representation for cross-type comparison.
data Num3 = NI !Int64 | NU !Word64 | ND !Double


toNum :: Value -> Maybe Num3
toNum = \case
  VInt i -> Just (NI i)
  VUInt u -> Just (NU u)
  VDouble d -> Just (ND d)
  _ -> Nothing


invert :: Ordering -> Ordering
invert LT = GT
invert GT = LT
invert EQ = EQ


{- | Compare two numbers on the shared number line. 'Nothing' indicates the
comparison is undefined, which only happens when a @NaN@ is involved.
-}
cmpNum :: Num3 -> Num3 -> Maybe Ordering
cmpNum a b = case (a, b) of
  (NI x, NI y) -> Just (compare x y)
  (NU x, NU y) -> Just (compare x y)
  (ND x, ND y)
    | isNaN x || isNaN y -> Nothing
    | otherwise -> Just (compare x y)
  (NI x, NU y) -> Just (cmpIntUint x y)
  (NU x, NI y) -> Just (invert (cmpIntUint y x))
  (NI x, ND y) -> cmpIntDouble x y
  (ND x, NI y) -> invert <$> cmpIntDouble y x
  (NU x, ND y) -> cmpUintDouble x y
  (ND x, NU y) -> invert <$> cmpUintDouble y x


cmpIntUint :: Int64 -> Word64 -> Ordering
cmpIntUint x y
  | x < 0 = LT
  | otherwise = compare (fromIntegral x :: Word64) y


-- Cross-type comparison follows reference CEL (cel-go): the value with the
-- larger magnitude type is bounds-checked first, then the integer is converted
-- to 'Double' and compared. This is intentionally lossy at the extreme ends of
-- the @int64@ / @uint64@ range (e.g. @9223372036854775807@ compares equal to
-- @9223372036854775808.0@), matching every conformant runtime.
cmpIntDouble :: Int64 -> Double -> Maybe Ordering
cmpIntDouble x y
  | isNaN y = Nothing
  | y < fromIntegral (minBound :: Int64) = Just GT
  | y > fromIntegral (maxBound :: Int64) = Just LT
  | otherwise = Just (compare (fromIntegral x) y)


cmpUintDouble :: Word64 -> Double -> Maybe Ordering
cmpUintDouble x y
  | isNaN y = Nothing
  | y < 0 = Just GT
  | y > fromIntegral (maxBound :: Word64) = Just LT
  | otherwise = Just (compare (fromIntegral x) y)


{- | Heterogeneous CEL equality. Always total: differing non-numeric types
compare unequal rather than erroring. @NaN@ is unequal to everything.
-}
valueEq :: Value -> Value -> Bool
valueEq a@(VAny _ _ _) b = either (const False) (\value -> valueEq value b) (unpackValue a)
valueEq a b@(VAny _ _ _) = either (const False) (valueEq a) (unpackValue b)
valueEq a b
  | Just x <- toNum a, Just y <- toNum b = cmpNum x y == Just EQ
valueEq a b = case (a, b) of
  (VNull, VNull) -> True
  (VBool x, VBool y) -> x == y
  (VString x, VString y) -> x == y
  (VBytes x, VBytes y) -> x == y
  (VTimestamp x, VTimestamp y) -> x == y
  (VDuration x, VDuration y) -> x == y
  (VType x, VType y) -> typeNameText x == typeNameText y
  (VList x, VList y) ->
    V.length x == V.length y && V.and (V.zipWith valueEq x y)
  (VMap x, VMap y) -> mapEq x y
  (VMessage (Message name x unknown), VMessage (Message name' y unknown')) ->
    let px = presentFields x
        py = presentFields y
    in name == name' && equalUnknownFields unknown unknown'
         && Map.size px == Map.size py && Map.isSubmapOfBy protobufValueEq px py
  _ -> False


mapEq :: CelMap -> CelMap -> Bool
mapEq = mapEqWith valueEq


mapEqWith :: (Value -> Value -> Bool) -> CelMap -> CelMap -> Bool
mapEqWith equal (CelMap xs) my@(CelMap ys) =
  length xs == length ys && all entryMatches xs
  where
    entryMatches (k, v) = case celMapLookup k my of
      Just v' -> equal v v'
      Nothing -> False


-- Protobuf fields have homogeneous types even when their CEL projections
-- support heterogeneous equality. Any adds its packed type to the comparison.
protobufValueEq :: Value -> Value -> Bool
protobufValueEq (VAnyPayload leftUrl leftBytes left) (VAnyPayload rightUrl rightBytes right) =
  case (left, right) of
    (Right x, Right y) ->
      let name = anyType leftUrl
      in name == anyType rightUrl && equalAnyPayload name (anyProtobufValue x) (anyProtobufValue y)
    _ -> leftUrl == rightUrl && leftBytes == rightBytes
protobufValueEq (VAnyPayload url _ decoded) y = case decoded of
  Right x -> anyType url == packedType y && equalAnyPayload (anyType url) (anyProtobufValue x) y
  Left _ -> False
protobufValueEq x (VAnyPayload url _ decoded) = case decoded of
  Right y -> packedType x == anyType url && equalAnyPayload (anyType url) x (anyProtobufValue y)
  Left _ -> False
protobufValueEq (VList x) (VList y) =
  V.length x == V.length y && V.and (V.zipWith protobufValueEq x y)
protobufValueEq (VMap x) (VMap y) = mapEqWith protobufValueEq x y
protobufValueEq x y = typeOf x == typeOf y && valueEq x y


equalAnyPayload :: Text -> Value -> Value -> Bool
equalAnyPayload name x y = case (x, y) of
  (VMessage _, VMessage _) -> protobufValueEq x y
  (VMessage _, _) -> maybe False (protobufValueEq x) (projectedMessage name y)
  (_, VMessage _) -> maybe False (\value -> protobufValueEq value y) (projectedMessage name x)
  _ -> protobufValueEq x y


-- Compare a wire message to the canonical protobuf form of a CEL value
-- assigned to an Any field.
projectedMessage :: Text -> Value -> Maybe Value
projectedMessage name value = case (name, value) of
  ("google.protobuf.BoolValue", VBool b) -> wrapper (not b)
  ("google.protobuf.BytesValue", VBytes b) -> wrapper (BS.null b)
  ("google.protobuf.StringValue", VString t) -> wrapper (T.null t)
  ("google.protobuf.Int32Value", VInt n) -> wrapper (n == 0)
  ("google.protobuf.Int64Value", VInt n) -> wrapper (n == 0)
  ("google.protobuf.UInt32Value", VUInt n) -> wrapper (n == 0)
  ("google.protobuf.UInt64Value", VUInt n) -> wrapper (n == 0)
  ("google.protobuf.FloatValue", VDouble n) -> wrapper (n == 0 && not (isNegativeZero n))
  ("google.protobuf.DoubleValue", VDouble n) -> wrapper (n == 0 && not (isNegativeZero n))
  ("google.protobuf.Timestamp", VTimestamp (Timestamp seconds nanos)) -> temporal seconds nanos
  ("google.protobuf.Duration", VDuration (Duration seconds nanos)) -> temporal seconds nanos
  ("google.protobuf.ListValue", VList values) -> do
    items <- traverse jsonValueMessage values
    oneField "values" (V.null items) (VList items)
  ("google.protobuf.Struct", VMap values) -> do
    items <- traverseCelMapValues (\_ -> jsonValueMessage) values
    oneField "fields" (celMapSize items == 0) (VMap items)
  ("google.protobuf.Value", _) -> jsonValueMessage value
  _ -> Nothing
  where
    wrapper isDefault = oneField "value" isDefault value
    oneField field isDefault fieldValue =
      Just (VMessage (CelMessage name (Map.singleton field (CelField (not isDefault) fieldValue))))
    temporal seconds nanos = Just $ VMessage $ CelMessage name $ Map.fromList
      [ ("seconds", CelField (seconds /= 0) (VInt seconds))
      , ("nanos", CelField (nanos /= 0) (VInt (fromIntegral nanos)))
      ]


jsonValueMessage :: Value -> Maybe Value
jsonValueMessage value = case value of
  VNull -> field "null_value" (VInt 0)
  VBool _ -> field "bool_value" value
  VString _ -> field "string_value" value
  VDouble _ -> field "number_value" value
  VList _ -> projectedMessage "google.protobuf.ListValue" value >>= field "list_value"
  VMap _ -> projectedMessage "google.protobuf.Struct" value >>= field "struct_value"
  _ -> Nothing
  where
    field name v = Just (VMessage (CelMessage "google.protobuf.Value" (Map.singleton name (CelField True v))))


anyType :: Text -> Text
anyType = snd . T.breakOnEnd "/"


-- CEL values assigned to Any are packed as the corresponding protobuf type.
packedType :: Value -> Text
packedType value = case typeOf value of
  TyNull -> "google.protobuf.Value"
  TyBool -> "google.protobuf.BoolValue"
  TyInt -> "google.protobuf.Int64Value"
  TyUInt -> "google.protobuf.UInt64Value"
  TyDouble -> "google.protobuf.DoubleValue"
  TyString -> "google.protobuf.StringValue"
  TyBytes -> "google.protobuf.BytesValue"
  TyList -> "google.protobuf.ListValue"
  TyMap -> "google.protobuf.Struct"
  ty -> typeNameText ty


{- | Compare two values for ordering. 'Nothing' is returned when the values
are not comparable: either they have incompatible (non-numeric, different)
types, or a @NaN@ is involved. Callers distinguish "no overload" from
"unordered @NaN@" using 'isNumeric' and type checks.
-}
compareValues :: Value -> Value -> Maybe Ordering
compareValues a b
  | Just x <- toNum a, Just y <- toNum b = cmpNum x y
compareValues a b = case (a, b) of
  (VBool x, VBool y) -> Just (compare x y)
  (VString x, VString y) -> Just (compare x y)
  (VBytes x, VBytes y) -> Just (compare x y)
  (VTimestamp x, VTimestamp y) -> Just (compare x y)
  (VDuration x, VDuration y) -> Just (compare x y)
  _ -> Nothing
