{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Proto3 canonical JSON mapping for well-known types.

These functions provide the canonical conversions specified by the
proto3 JSON specification.
-}
module Proto.Internal.JSON.WellKnown (
  timestampToJSON,
  timestampFromJSON,
  durationToJSON,
  durationFromJSON,
  fieldMaskToJSON,
  fieldMaskFromJSON,
  structToJSON,
  structFromJSON,
  valueToJSON,
  valueFromJSON,
  formatRfc3339,
  parseRfc3339,

  -- * Wrapper types

  {- | Per the proto3 JSON spec, every @google.protobuf.XValue@
  wrapper serialises as just its inner value (rather than the
  generic @{"value": ...}@ shape the pre-generated @ToJSON@
  instances produce). The 'wrap*' helpers go in the encode
  direction; the 'unwrap*' helpers parse a bare JSON value
  and construct the wrapper. 64-bit integer wrappers
  additionally string-encode their inner value.
  -}
  wrapBoolValue,
  wrapInt32Value,
  wrapInt64Value,
  wrapUInt32Value,
  wrapUInt64Value,
  wrapFloatValue,
  wrapDoubleValue,
  wrapStringValue,
  wrapBytesValue,
  unwrapBoolValue,
  unwrapInt32Value,
  unwrapInt64Value,
  unwrapUInt32Value,
  unwrapUInt64Value,
  unwrapFloatValue,
  unwrapDoubleValue,
  unwrapStringValue,
  unwrapBytesValue,

  -- * Empty / NullValue
  emptyToJSON,
  emptyFromJSON,
  nullValueToJSON,
  nullValueFromJSON,

  -- * Any
  anyToJSON,
  anyFromJSON,
  standardWktRegistry,
  anyJsonRegistry,

  -- * Re-exports from "Proto.Registry"
  AnyCodec (..),
  TypeRegistry,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import Proto.Google.Protobuf.Any qualified as Any
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty qualified as Empty
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.Struct
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Wrappers qualified as W
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Proto.Registry (AnyCodec (..), TypeRegistry, emptyRegistry, globalAnyRegistry, lookupCodec, registerCodec)


{- | Encode a 'Timestamp' as canonical RFC 3339. Throws on out-
of-range input — the conformance suite requires that
serialisation fail when the wire-format value is outside
@[0001-01-01T00:00:00Z, 9999-12-31T23:59:59Z]@. The runner
catches the exception and turns it into a serialize-error
response.
-}
timestampToJSON :: Timestamp -> Aeson.Value
timestampToJSON ts
  | s < timestampMinSecs || s > timestampMaxSecs =
      error ("Timestamp out of range: " <> show s)
  | otherwise = Aeson.String (formatRfc3339 s n)
  where
    !s = timestampSeconds ts
    !n = timestampNanos ts


-- | Parse a 'Timestamp' from an RFC 3339 JSON string.
timestampFromJSON :: Aeson.Value -> Either String Timestamp
timestampFromJSON (Aeson.String t) = parseRfc3339 t
timestampFromJSON _ = Left "Expected RFC 3339 string for Timestamp"


-- | Format seconds and nanos as an RFC 3339 timestamp string.
formatRfc3339 :: Int64 -> Int32 -> Text
formatRfc3339 secs nanos =
  let !civil = unixToCivil secs
      dateStr = padInt 4 (cvYear civil) <> "-" <> padInt 2 (cvMonth civil) <> "-" <> padInt 2 (cvDay civil)
      timeStr = padInt 2 (cvHour civil) <> ":" <> padInt 2 (cvMinute civil) <> ":" <> padInt 2 (cvSecond civil)
  in dateStr <> "T" <> timeStr <> canonicalNanoSuffix nanos <> "Z"


{- | Per the proto3 JSON spec, Timestamp/Duration nanos are
formatted with EXACTLY 0, 3, 6, or 9 fractional digits. A
value with fewer significant digits is padded out to the
next bucket; zero suppresses the entire @\".nnn\"@ suffix.

This is the rule the conformance suite's Validator tests
(Timestamp/Duration {Has3, Has6, Has9}FractionalDigits and
TimestampZeroNormalized) check.
-}
canonicalNanoSuffix :: Int32 -> Text
canonicalNanoSuffix n
  | n == 0 = T.empty
  | otherwise =
      let !digits = padInt 9 (fromIntegral (abs n))
          -- Trim trailing zeros, then round /up/ the kept count
          -- to the next multiple of 3 (3, 6, or 9).
          !trimmed = T.dropWhileEnd (== '0') digits
          !keep = case T.length trimmed of
            k
              | k <= 3 -> 3
              | k <= 6 -> 6
              | otherwise -> 9
      in T.cons '.' (T.take keep digits)


data CivilTime = CivilTime
  { cvYear :: {-# UNPACK #-} !Int
  , cvMonth :: {-# UNPACK #-} !Int
  , cvDay :: {-# UNPACK #-} !Int
  , cvHour :: {-# UNPACK #-} !Int
  , cvMinute :: {-# UNPACK #-} !Int
  , cvSecond :: {-# UNPACK #-} !Int
  }


unixToCivil :: Int64 -> CivilTime
unixToCivil totalSecs =
  let !s' = fromIntegral totalSecs :: Int
      (!days, !dayRem) = s' `quotRem` 86400
      (!h, !hmRem) = dayRem `quotRem` 3600
      (!mi, !sec) = hmRem `quotRem` 60
      !date = civilFromDays (days + 719468)
  in CivilTime (cdYear date) (cdMonth date) (cdDay date) h mi sec


data CivilDate = CivilDate
  { cdYear :: {-# UNPACK #-} !Int
  , cdMonth :: {-# UNPACK #-} !Int
  , cdDay :: {-# UNPACK #-} !Int
  }


civilFromDays :: Int -> CivilDate
civilFromDays z =
  let !era = (if z >= 0 then z else z - 146096) `quot` 146097
      !doe = z - era * 146097
      !yoe = (doe - doe `quot` 1460 + doe `quot` 36524 - doe `quot` 146096) `quot` 365
      !y = yoe + era * 400
      !doy = doe - (365 * yoe + yoe `quot` 4 - yoe `quot` 100)
      !mp = (5 * doy + 2) `quot` 153
      !d = doy - (153 * mp + 2) `quot` 5 + 1
      !m = mp + (if mp < 10 then 3 else -9)
      !y' = y + (if m <= 2 then 1 else 0)
  in CivilDate y' m d


daysFromCivil :: Int -> Int -> Int -> Int
daysFromCivil y m d =
  let !y' = y - (if m <= 2 then 1 else 0)
      !era = (if y' >= 0 then y' else y' - 399) `quot` 400
      !yoe = y' - era * 400
      !doy = (153 * (m + (if m > 2 then -3 else 9)) + 2) `quot` 5 + d - 1
      !doe = yoe * 365 + yoe `quot` 4 - yoe `quot` 100 + doy
  in era * 146097 + doe


padInt :: Int -> Int -> Text
padInt width n =
  let !raw = intToText n
      !pad = width - T.length raw
  in if pad <= 0 then raw else T.replicate pad "0" <> raw


intToText :: Int -> Text
intToText n
  | n < 0 = "-" <> intToText (negate n)
  | n < 10 = T.singleton (digit n)
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (digit r) acc) q
    digit i = toEnum (i + 48)


{- | Proto3 spec range for Timestamp: @[0001-01-01T00:00:00Z,
9999-12-31T23:59:59.999999999Z]@. Outside that range
timestamps are not representable in canonical JSON.
-}
timestampMinSecs, timestampMaxSecs :: Int64
timestampMinSecs = -62135596800 -- 0001-01-01T00:00:00Z
timestampMaxSecs = 253402300799 -- 9999-12-31T23:59:59Z


-- | Parse an RFC 3339 string into a 'Timestamp'.
parseRfc3339 :: Text -> Either String Timestamp
parseRfc3339 t = do
  let stripped = T.strip t
  case T.breakOn "T" stripped of
    (datePart, rest)
      | T.null rest -> Left "Invalid RFC 3339 timestamp: missing T separator"
      | otherwise -> do
          let timePart' = T.drop 1 rest
          (timePart, !offsetSecs) <- splitOffset timePart'
          date <- parseDate datePart
          time <- parseTime timePart
          let !days = daysFromCivil (pdYear date) (pdMonth date) (pdDay date) - 719468
              !rawSecs =
                fromIntegral days * 86400
                  + fromIntegral (ptHour time) * 3600
                  + fromIntegral (ptMinute time) * 60
                  + fromIntegral (ptSecond time)
              -- If the input came with a +HH:MM offset, normalise
              -- to UTC by subtracting the offset (so an East-of-
              -- UTC wall clock yields a smaller unix epoch).
              !totalSecs = rawSecs - offsetSecs
          if totalSecs < timestampMinSecs || totalSecs > timestampMaxSecs
            then Left "Timestamp out of range [0001-01-01, 9999-12-31]"
            else
              Right
                Timestamp
                  { timestampSeconds = totalSecs
                  , timestampNanos = ptNanos time
                  , timestampUnknownFields = []
                  }


{- | Strip the trailing zone designator from an RFC 3339 time
and return the remaining time component plus the offset in
seconds (positive for east-of-UTC, negative for west).

The proto3 spec requires the timestamp to end with @Z@ (the
conformance suite explicitly rejects lowercase @z@ and
missing zone). Numeric offsets @+HH:MM@ and @-HH:MM@ are
accepted and normalised to UTC.
-}
splitOffset :: Text -> Either String (Text, Int64)
splitOffset t
  | T.null t = Left "Empty time component"
  | last' == 'Z' = Right (T.init t, 0)
  | last' == 'z' = Left "Lowercase 'z' zone designator not allowed"
  | otherwise =
      -- Try to peel a +HH:MM / -HH:MM suffix. We scan back from
      -- the end for the first '+' or '-' that's part of the zone
      -- (the seconds field can also have a leading '-' but we
      -- look for an offset signature: ±DD:DD).
      case parseOffsetSuffix t of
        Just (timePart, offsetSecs) -> Right (timePart, offsetSecs)
        Nothing -> Left "Timestamp must end with 'Z' or numeric offset"
  where
    last' = T.last t


{- | Recognise a trailing @+HH:MM@ / @-HH:MM@ (and the rare
@+HHMM@ shorthand) suffix on an RFC 3339 time string.
-}
parseOffsetSuffix :: Text -> Maybe (Text, Int64)
parseOffsetSuffix t
  -- ±HH:MM (6 chars, e.g. "+08:00")
  | T.length t >= 6
  , Just (rest, suf) <- splitAtRev 6 t
  , Just (sign, hh, mm) <- parseHhMmSuffix suf =
      Just (rest, sign * (hh * 3600 + mm * 60))
  | otherwise = Nothing
  where
    splitAtRev n s
      | T.length s < n = Nothing
      | otherwise = Just (T.dropEnd n s, T.takeEnd n s)

    parseHhMmSuffix s = case T.unpack s of
      [c1, h1, h2, ':', m1, m2]
        | c1 == '+' || c1 == '-'
        , isDigit h1
        , isDigit h2
        , isDigit m1
        , isDigit m2 ->
            Just
              ( if c1 == '+' then 1 else -1
              , fromIntegral (d h1 * 10 + d h2)
              , fromIntegral (d m1 * 10 + d m2)
              )
      _ -> Nothing

    d c = fromEnum c - fromEnum '0'


data ParsedDate = ParsedDate
  { pdYear :: {-# UNPACK #-} !Int
  , pdMonth :: {-# UNPACK #-} !Int
  , pdDay :: {-# UNPACK #-} !Int
  }


data ParsedTime = ParsedTime
  { ptHour :: {-# UNPACK #-} !Int
  , ptMinute :: {-# UNPACK #-} !Int
  , ptSecond :: {-# UNPACK #-} !Int
  , ptNanos :: {-# UNPACK #-} !Int32
  }


parseDate :: Text -> Either String ParsedDate
parseDate t = case T.splitOn "-" t of
  [ys, ms, ds] -> do
    y <- readInt ys
    m <- readInt ms
    d <- readInt ds
    Right (ParsedDate y m d)
  _ -> Left "Invalid date format"


parseTime :: Text -> Either String ParsedTime
parseTime t =
  let (wholePart, fracPart) = T.breakOn "." t
  in case T.splitOn ":" wholePart of
       [hs, ms, ss] -> do
         h <- readInt hs
         m <- readInt ms
         s <- readInt ss
         let !nanos = parseFracNanos fracPart
         Right (ParsedTime h m s nanos)
       _ -> Left "Invalid time format"


parseFracNanos :: Text -> Int32
parseFracNanos t
  | T.null t = 0
  | T.head t == '.' =
      let digits = T.takeWhile isDigit (T.tail t)
          padded = T.take 9 (digits <> T.replicate (9 - T.length digits) "0")
      in case readInt padded of
           Right n -> fromIntegral n
           Left _ -> 0
  | otherwise = 0


readInt :: Text -> Either String Int
readInt t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Right n
  Right (_, rest) -> Left ("Trailing chars: " <> T.unpack rest)
  Left e -> Left e


-- Duration: "3.5s" format
--
-- Proto3 spec range: seconds ∈ [-315576000000, 315576000000]
-- (about ±10000 years). Nanos must have the same sign as
-- seconds (or be zero), and lie in (-1e9, 1e9).

-- | Inclusive bounds on @Duration.seconds@ per the proto3 spec.
durationMinSecs, durationMaxSecs :: Int64
durationMinSecs = -315576000000
durationMaxSecs = 315576000000


-- | Encode a 'Duration' as a canonical JSON string (e.g. @\"3.5s\"@).
durationToJSON :: Duration -> Aeson.Value
durationToJSON dur
  | s < durationMinSecs || s > durationMaxSecs =
      error ("Duration out of range: " <> show s)
  | otherwise = Aeson.String (secStr <> canonicalNanoSuffix (abs n) <> "s")
  where
    !s = durationSeconds dur
    !n = durationNanos dur
    -- Sign comes from EITHER field. Always render the
    -- whole-seconds magnitude with an explicit leading '-'
    -- when the duration is negative (covers "-0.5s" and
    -- "-1.5s" alike).
    negative = s < 0 || n < 0
    signStr = if negative then T.singleton '-' else T.empty
    secStr = signStr <> intToText (fromIntegral (abs s))


-- | Parse a 'Duration' from a canonical JSON string (e.g. @\"3.5s\"@).
durationFromJSON :: Aeson.Value -> Either String Duration
durationFromJSON (Aeson.String t) = parseDuration t
durationFromJSON _ = Left "Expected duration string"


parseDuration :: Text -> Either String Duration
parseDuration t = do
  let stripped = T.strip t
  case T.stripSuffix "s" stripped of
    Nothing -> Left "Duration must end with 's'"
    Just numPart ->
      let !negative = case T.uncons numPart of
            Just ('-', _) -> True
            _ -> False
      in case T.breakOn "." numPart of
           (wholePart, fracPart) -> do
             secs <- readInt wholePart
             let !rawNanos = parseFracNanos fracPart
                 -- Proto3 spec: nanos carry the same sign as seconds.
                 -- For "-0.5s" the wholePart parses to 0 but the
                 -- sign comes from the leading '-' in the input.
                 !nanos = if negative then negate rawNanos else rawNanos
                 !secs64 = fromIntegral secs :: Int64
             if secs64 < durationMinSecs || secs64 > durationMaxSecs
               then Left "Duration out of range [-315576000000, 315576000000]"
               else
                 Right
                   Duration
                     { durationSeconds = secs64
                     , durationNanos = nanos
                     , durationUnknownFields = []
                     }


-- FieldMask: comma-separated paths
--
-- Proto3 spec: each path component is stored in
-- lower_snake_case on the wire, but rendered in lowerCamelCase
-- in JSON. The conversion is round-trip-required, so any path
-- whose snake form can't be unambiguously recovered from the
-- camel form (uppercase chars, embedded digits, repeated
-- underscores) MUST be rejected on serialise.

-- | Encode a 'FieldMask' as a comma-separated lowerCamelCase JSON string.
fieldMaskToJSON :: FieldMask -> Aeson.Value
fieldMaskToJSON fm =
  case traverse snakeToFieldMaskCamel (V.toList (fieldMaskPaths fm)) of
    Right cs -> Aeson.String (T.intercalate "," cs)
    Left e -> error ("FieldMask path: " <> e)


-- | Parse a 'FieldMask' from a comma-separated lowerCamelCase JSON string.
fieldMaskFromJSON :: Aeson.Value -> Either String FieldMask
fieldMaskFromJSON (Aeson.String t)
  | T.null t =
      Right FieldMask {fieldMaskPaths = V.empty, fieldMaskUnknownFields = []}
  | otherwise = do
      paths <- traverse camelToFieldMaskSnake (T.splitOn "," t)
      Right
        FieldMask
          { fieldMaskPaths = V.fromList paths
          , fieldMaskUnknownFields = []
          }
fieldMaskFromJSON _ = Left "Expected string for FieldMask"


{- | snake_case -> lowerCamelCase for one FieldMask path
component, refusing inputs that wouldn't round-trip back to
the original snake form (uppercase chars in source, embedded
digits surrounded by underscores, multiple consecutive
underscores).
-}
snakeToFieldMaskCamel :: Text -> Either String Text
snakeToFieldMaskCamel t = T.pack <$> go False (T.unpack t)
  where
    go _ [] = Right []
    go cap (c : cs)
      | c == '_' =
          if cap
            then Left ("repeated underscores in path: " <> show t)
            else case cs of
              (next : _)
                | not (isLowerAscii next) ->
                    Left
                      ( "'_' must precede lowercase ASCII in path: "
                          <> show t
                      )
              _ -> go True cs
      | c == '.' =
          if cap
            then Left ("'.' immediately after '_' in path: " <> show t)
            else (c :) <$> go False cs
      | isUpperAscii c =
          Left ("uppercase character not allowed in source path: " <> show t)
      | cap = (upperOf c :) <$> go False cs
      | otherwise = (c :) <$> go False cs


{- | lowerCamelCase -> snake_case for one FieldMask path
component (the input form on JSON parse). Each uppercase
letter introduces a leading @_@; @.@ separators stay verbatim
so nested paths like @foo.barBaz@ become @foo.bar_baz@.
Rejects inputs containing @_@ (FieldMaskInvalidCharacter) —
the JSON form is required to be lowerCamelCase, which never
has bare underscores.
-}
camelToFieldMaskSnake :: Text -> Either String Text
camelToFieldMaskSnake t = T.pack <$> go (T.unpack t)
  where
    go [] = Right []
    go (c : cs)
      | c == '_' =
          Left ("'_' not allowed in JSON FieldMask path: " <> show t)
      | isUpperAscii c =
          ('_' :) . (lowerOf c :) <$> go cs
      | otherwise = (c :) <$> go cs


isLowerAscii, isUpperAscii :: Char -> Bool
isLowerAscii = isAsciiLower
isUpperAscii = isAsciiUpper


upperOf, lowerOf :: Char -> Char
upperOf c
  | isLowerAscii c = toEnum (fromEnum c - 32)
  | otherwise = c
lowerOf c
  | isUpperAscii c = toEnum (fromEnum c + 32)
  | otherwise = c


-- Struct/Value: native JSON

-- | Encode a 'Struct' as a native JSON object.
structToJSON :: Struct -> Aeson.Value
structToJSON s =
  Aeson.Object
    ( AesonKM.fromList
        (fmap (bimap AesonKey.fromText valueToJSON) (Map.toList (structFields s)))
    )


-- | Parse a 'Struct' from a native JSON object.
structFromJSON :: Aeson.Value -> Either String Struct
structFromJSON (Aeson.Object o) =
  Right
    defaultStruct
      { structFields =
          Map.fromList
            (fmap (bimap AesonKey.toText jsonToValue) (AesonKM.toList o))
      }
structFromJSON _ = Left "Expected object for Struct"


-- | Encode a protobuf 'Value' as a native JSON value.
valueToJSON :: Value -> Aeson.Value
valueToJSON v = case valueKind v of
  Nothing -> Aeson.Null
  Just vk -> case vk of
    Value'Kind'NullValue _ -> Aeson.Null
    Value'Kind'NumberValue d
      | isNaN d || isInfinite d ->
          -- Proto3 spec: google.protobuf.Value rejects
          -- non-finite numbers on serialise (the Reject{Inf,
          -- Nan}NumberValue conformance tests). Surface this
          -- as a serialise failure via 'error', caught by
          -- the conformance handler.
          error ("Value: non-finite number not allowed: " <> show d)
      | otherwise -> Aeson.Number (fromFloatDigits d)
    Value'Kind'StringValue s -> Aeson.String s
    Value'Kind'BoolValue b -> Aeson.Bool b
    Value'Kind'StructValue s -> structToJSON s
    Value'Kind'ListValue l -> Aeson.Array (fmap valueToJSON (listValueValues l))


-- | Parse a protobuf 'Value' from a native JSON value.
valueFromJSON :: Aeson.Value -> Either String Value
valueFromJSON jv = Right (jsonToValue jv)


jsonToValue :: Aeson.Value -> Value
jsonToValue Aeson.Null = defaultValue {valueKind = Just (Value'Kind'NullValue NullValue'NullValue)}
jsonToValue (Aeson.Bool b) = defaultValue {valueKind = Just (Value'Kind'BoolValue b)}
jsonToValue (Aeson.Number n) = defaultValue {valueKind = Just (Value'Kind'NumberValue (toRealFloat n))}
jsonToValue (Aeson.String s) = defaultValue {valueKind = Just (Value'Kind'StringValue s)}
jsonToValue (Aeson.Array vs) = defaultValue {valueKind = Just (Value'Kind'ListValue (defaultListValue {listValueValues = fmap jsonToValue vs}))}
jsonToValue (Aeson.Object o) = defaultValue {valueKind = Just (Value'Kind'StructValue (defaultStruct {structFields = Map.fromList (fmap (bimap AesonKey.toText jsonToValue) (AesonKM.toList o))}))}


-- ---------------------------------------------------------------------------
-- Wrappers (proto3 spec: emit just the inner value, not @{"value": ...}@)
-- ---------------------------------------------------------------------------

-- Encoders unwrap to the inner value, applying proto3-canonical
-- per-scalar conversions where needed (string-form 64-bit ints,
-- NaN/Infinity floats, base64 bytes).

-- | Encode a 'W.BoolValue' as its bare inner JSON boolean.
wrapBoolValue :: W.BoolValue -> Aeson.Value
wrapBoolValue = Aeson.Bool . W.boolValueValue


-- | Encode an 'W.Int32Value' as its bare inner JSON number.
wrapInt32Value :: W.Int32Value -> Aeson.Value
wrapInt32Value = Aeson.toJSON . W.int32ValueValue


-- | Encode an 'W.Int64Value' as its bare inner JSON string (64-bit canonical).
wrapInt64Value :: W.Int64Value -> Aeson.Value
wrapInt64Value = Aeson.String . T.pack . show . W.int64ValueValue


-- | Encode a 'W.UInt32Value' as its bare inner JSON number.
wrapUInt32Value :: W.UInt32Value -> Aeson.Value
wrapUInt32Value = Aeson.toJSON . W.uInt32ValueValue


-- | Encode a 'W.UInt64Value' as its bare inner JSON string (64-bit canonical).
wrapUInt64Value :: W.UInt64Value -> Aeson.Value
wrapUInt64Value = Aeson.String . T.pack . show . W.uInt64ValueValue


-- | Encode a 'W.FloatValue' as its bare inner JSON number (with NaN\/Infinity sentinels).
wrapFloatValue :: W.FloatValue -> Aeson.Value
wrapFloatValue = floatLikeToJSON . realToFrac . W.floatValueValue


-- | Encode a 'W.DoubleValue' as its bare inner JSON number (with NaN\/Infinity sentinels).
wrapDoubleValue :: W.DoubleValue -> Aeson.Value
wrapDoubleValue = floatLikeToJSON . W.doubleValueValue


-- | Encode a 'W.StringValue' as its bare inner JSON string.
wrapStringValue :: W.StringValue -> Aeson.Value
wrapStringValue = Aeson.String . W.stringValueValue


-- | Encode a 'W.BytesValue' as its bare inner base64 JSON string.
wrapBytesValue :: W.BytesValue -> Aeson.Value
wrapBytesValue = Aeson.String . TE.decodeUtf8 . Base64.encode . W.bytesValueValue


floatLikeToJSON :: Double -> Aeson.Value
floatLikeToJSON d
  | isNaN d = Aeson.String "NaN"
  | isInfinite d = Aeson.String (if d > 0 then "Infinity" else "-Infinity")
  | otherwise = Aeson.Number (fromFloatDigits d)


-- Decoders parse a bare JSON value and construct the wrapper.

-- | Parse a bare JSON boolean into a 'W.BoolValue'.
unwrapBoolValue :: Aeson.Value -> Either String W.BoolValue
unwrapBoolValue (Aeson.Bool b) = Right W.defaultBoolValue {W.boolValueValue = b}
unwrapBoolValue _ = Left "Expected JSON Bool for BoolValue"


-- | Parse a bare JSON number or string into an 'W.Int32Value'.
unwrapInt32Value :: Aeson.Value -> Either String W.Int32Value
unwrapInt32Value v = case parseIntegral v of
  Right n -> Right W.defaultInt32Value {W.int32ValueValue = fromIntegral (n :: Int64)}
  Left e -> Left e


-- | Parse a bare JSON number or string into an 'W.Int64Value'.
unwrapInt64Value :: Aeson.Value -> Either String W.Int64Value
unwrapInt64Value v = case parseIntegral v of
  Right n -> Right W.defaultInt64Value {W.int64ValueValue = n}
  Left e -> Left e


-- | Parse a bare JSON number or string into a 'W.UInt32Value'.
unwrapUInt32Value :: Aeson.Value -> Either String W.UInt32Value
unwrapUInt32Value v = case parseIntegral v of
  Right n ->
    Right
      W.defaultUInt32Value
        { W.uInt32ValueValue = fromIntegral (n :: Int64)
        }
  Left e -> Left e


-- | Parse a bare JSON number or string into a 'W.UInt64Value'.
unwrapUInt64Value :: Aeson.Value -> Either String W.UInt64Value
unwrapUInt64Value v = case parseIntegral v of
  Right n ->
    Right
      W.defaultUInt64Value
        { W.uInt64ValueValue = fromIntegral (n :: Int64)
        }
  Left e -> Left e


-- | Parse a bare JSON number (or NaN\/Infinity string) into a 'W.FloatValue'.
unwrapFloatValue :: Aeson.Value -> Either String W.FloatValue
unwrapFloatValue v = case parseFloating v of
  Right d -> Right W.defaultFloatValue {W.floatValueValue = realToFrac d}
  Left e -> Left e


-- | Parse a bare JSON number (or NaN\/Infinity string) into a 'W.DoubleValue'.
unwrapDoubleValue :: Aeson.Value -> Either String W.DoubleValue
unwrapDoubleValue v = case parseFloating v of
  Right d -> Right W.defaultDoubleValue {W.doubleValueValue = d}
  Left e -> Left e


-- | Parse a bare JSON string into a 'W.StringValue'.
unwrapStringValue :: Aeson.Value -> Either String W.StringValue
unwrapStringValue (Aeson.String s) =
  Right W.defaultStringValue {W.stringValueValue = s}
unwrapStringValue _ = Left "Expected JSON String for StringValue"


-- | Parse a bare base64 JSON string into a 'W.BytesValue'.
unwrapBytesValue :: Aeson.Value -> Either String W.BytesValue
unwrapBytesValue (Aeson.String s) =
  case Base64.decode (TE.encodeUtf8 s) of
    Right bs -> Right W.defaultBytesValue {W.bytesValueValue = bs}
    Left e -> Left ("invalid base64 for BytesValue: " <> e)
unwrapBytesValue _ = Left "Expected JSON String for BytesValue"


parseIntegral :: Aeson.Value -> Either String Int64
parseIntegral (Aeson.String s) = case TR.signed TR.decimal s of
  Right (n, rest) | T.null rest -> Right n
  _ -> Left "Invalid integer string"
parseIntegral (Aeson.Number n) = Right (round n)
parseIntegral _ = Left "Expected JSON String or Number"


parseFloating :: Aeson.Value -> Either String Double
parseFloating (Aeson.Number n) = Right (toRealFloat n)
parseFloating (Aeson.String "NaN") = Right (0 / 0)
parseFloating (Aeson.String "Infinity") = Right (1 / 0)
parseFloating (Aeson.String "-Infinity") = Right (negate (1 / 0))
parseFloating _ = Left "Expected JSON Number or {NaN,Infinity}"


-- ---------------------------------------------------------------------------
-- Empty / NullValue / Any
-- ---------------------------------------------------------------------------

-- | Encode 'Empty.Empty' as an empty JSON object.
emptyToJSON :: Empty.Empty -> Aeson.Value
emptyToJSON _ = Aeson.Object AesonKM.empty


-- | Parse 'Empty.Empty' from a JSON object.
emptyFromJSON :: Aeson.Value -> Either String Empty.Empty
emptyFromJSON (Aeson.Object _) = Right Empty.defaultEmpty
emptyFromJSON _ = Left "Expected JSON Object for Empty"


{- | 'NullValue' is the proto3 enum @NULL_VALUE = 0@; it serialises
as JSON @null@. We import it from "Proto.Google.Protobuf.Struct"
(since that's where the codegen put it).
-}
nullValueToJSON :: NullValue -> Aeson.Value
nullValueToJSON _ = Aeson.Null


nullValueFromJSON :: Aeson.Value -> Either String NullValue
nullValueFromJSON Aeson.Null = Right NullValue'NullValue
nullValueFromJSON _ = Left "Expected JSON null for NullValue"


{- | Strip the canonical @type.googleapis.com/@ prefix so the
registry key is the bare proto fully-qualified type name.
-}
typeFromUrl :: Text -> Text
typeFromUrl t =
  let prefix = T.pack "type.googleapis.com/"
  in case T.stripPrefix prefix t of
       Just rest -> rest
       Nothing -> case T.breakOnEnd (T.pack "/") t of
         (_, suffix) | not (T.null suffix) -> suffix
         _ -> t


{- | @google.protobuf.Any@ JSON shape:
@{"@type": "type.googleapis.com/...", ...other fields embedded...}@
for a regular message, or
@{"@type": "...", "value": <canonical>}@ for a WKT.

Falls back to the degenerate shape (@@type + base64 value@)
when the type isn't in the codec registry.
-}
anyToJSON :: TypeRegistry -> Any.Any -> Aeson.Value
anyToJSON registry a =
  let url = Any.anyTypeUrl a
      ty = typeFromUrl url
      typeKey = AesonKey.fromText (T.pack "@type")
      typeVal = (typeKey, Aeson.String url)
      fallback =
        Aeson.Object
          ( AesonKM.fromList
              [ typeVal
              ,
                ( AesonKey.fromText (T.pack "value")
                , Aeson.String (TE.decodeUtf8 (Base64.encode (Any.anyValue a)))
                )
              ]
          )
  in case lookupCodec ty registry of
       Just codec ->
         case acToJSON codec (Any.anyValue a) of
           Left _ -> fallback
           Right v
             | acIsWkt codec ->
                 Aeson.Object
                   ( AesonKM.fromList
                       [ typeVal
                       , (AesonKey.fromText (T.pack "value"), v)
                       ]
                   )
             | Aeson.Object obj <- v ->
                 Aeson.Object (AesonKM.insert typeKey (Aeson.String url) obj)
             | otherwise ->
                 -- Codec for a non-WKT returned a non-object;
                 -- shouldn't happen for our registered types, but
                 -- we degrade gracefully by switching back to the
                 -- "value": <encoded> form rather than crash.
                 Aeson.Object
                   ( AesonKM.fromList
                       [typeVal, (AesonKey.fromText (T.pack "value"), v)]
                   )
       Nothing -> fallback


anyFromJSON :: TypeRegistry -> Aeson.Value -> Either String Any.Any
anyFromJSON registry (Aeson.Object o) = do
  let typeKey = AesonKey.fromText (T.pack "@type")
  url <- case AesonKM.lookup typeKey o of
    Just (Aeson.String s) -> Right s
    _ -> Left "Any: missing or non-string @type"
  let ty = typeFromUrl url
  case lookupCodec ty registry of
    Just codec
      | acIsWkt codec ->
          case AesonKM.lookup (AesonKey.fromText (T.pack "value")) o of
            Nothing ->
              Right
                Any.defaultAny
                  { Any.anyTypeUrl = url
                  , Any.anyValue = BS.empty
                  }
            Just v -> do
              bs <- acFromJSON codec v
              Right Any.defaultAny {Any.anyTypeUrl = url, Any.anyValue = bs}
      | otherwise ->
          let inner = Aeson.Object (AesonKM.delete typeKey o)
          in do
               bs <- acFromJSON codec inner
               Right Any.defaultAny {Any.anyTypeUrl = url, Any.anyValue = bs}
    Nothing -> do
      -- Unregistered type: fall back to the degenerate
      -- "value": base64 form. Tests like AnyWithFieldMask
      -- which we've registered hit the codec path instead.
      bs <- case AesonKM.lookup (AesonKey.fromText (T.pack "value")) o of
        Nothing -> Right BS.empty
        Just (Aeson.String s) -> case Base64.decode (TE.encodeUtf8 s) of
          Right bs' -> Right bs'
          Left e -> Left ("Any: invalid base64 value: " <> e)
        _ -> Left "Any: non-string value"
      Right Any.defaultAny {Any.anyTypeUrl = url, Any.anyValue = bs}
anyFromJSON _ _ = Left "Expected JSON Object for Any"


{- | The 'TypeRegistry' generated code uses to (de)serialise @Any@-typed
fields: 'standardWktRegistry' unioned with the process-global registry
('Proto.Registry.globalAnyRegistry'), so applications can register their
own message types at runtime (via
'Proto.Registry.registerGlobalAnyCodecs') and have @Any@ values wrapping
them produce the canonical inlined @{"\@type": …, …}@ JSON instead of the
degenerate @{"value": <base64>}@ fallback.

This is a CAF: it is evaluated once, on first @Any@ (de)serialisation,
capturing the global registry as it stands at that moment. Register all
application types /before/ any @Any@ value crosses the wire (i.e. during
process startup). When nothing is registered it is exactly
'standardWktRegistry', so WKT-only users see no change.
-}
{-# NOINLINE anyJsonRegistry #-}
anyJsonRegistry :: TypeRegistry
anyJsonRegistry =
  standardWktRegistry <> unsafePerformIO (readIORef globalAnyRegistry)


{- | A pure 'TypeRegistry' containing all 17 standard well-known
type codecs (Timestamp, Duration, FieldMask, Struct, Value,
ListValue, NullValue, Empty, Any, BoolValue, Int32Value,
Int64Value, UInt32Value, UInt64Value, FloatValue, DoubleValue,
StringValue, BytesValue).

Use this as the base registry and extend it with
'registerCodec' for application-specific message types.
-}
standardWktRegistry :: TypeRegistry
standardWktRegistry =
  foldr
    (uncurry registerCodec)
    emptyRegistry
    [
      ( "google.protobuf.Timestamp"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          timestampToJSON
          timestampFromJSON
      )
    ,
      ( "google.protobuf.Duration"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          durationToJSON
          durationFromJSON
      )
    ,
      ( "google.protobuf.FieldMask"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          fieldMaskToJSON
          fieldMaskFromJSON
      )
    ,
      ( "google.protobuf.Struct"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          structToJSON
          structFromJSON
      )
    ,
      ( "google.protobuf.Value"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          valueToJSON
          valueFromJSON
      )
    ,
      ( "google.protobuf.ListValue"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          (Aeson.Array . V.map valueToJSON . listValueValues)
          ( \case
              Aeson.Array vs ->
                Right (defaultListValue {listValueValues = V.map jsonToValue vs})
              _ -> Left "Expected JSON array for ListValue"
          )
      )
    ,
      ( "google.protobuf.NullValue"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          nullValueToJSON
          nullValueFromJSON
      )
    ,
      ( "google.protobuf.Empty"
      , wktCodecVia PD.decodeMessage PE.encodeMessage emptyToJSON emptyFromJSON
      )
    , -- google.protobuf.Any nested inside another Any: per
      -- proto3 spec, Any IS a WKT, so the envelope is "value"
      -- and the value of "value" is itself the recursive
      -- @{"@type":...,...}@ object.

      ( "google.protobuf.Any"
      , AnyCodec
          { acToJSON = \bs -> case PD.decodeMessage bs of
              Left _ -> Left "Any: nested Any decode failed"
              Right (a :: Any.Any) -> Right (anyToJSON standardWktRegistry a)
          , acFromJSON = fmap PE.encodeMessage . anyFromJSON standardWktRegistry
          , acIsWkt = True
          }
      )
    , -- All 9 wrapper types: each round-trips through its bare
      -- inner value rather than the generic @{"value": ...}@ shape.
      -- The Any envelope still uses @"value"@, but that's the
      -- envelope wrapping the wrapper's own bare value.

      ( "google.protobuf.BoolValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapBoolValue unwrapBoolValue
      )
    ,
      ( "google.protobuf.Int32Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapInt32Value unwrapInt32Value
      )
    ,
      ( "google.protobuf.Int64Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapInt64Value unwrapInt64Value
      )
    ,
      ( "google.protobuf.UInt32Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapUInt32Value unwrapUInt32Value
      )
    ,
      ( "google.protobuf.UInt64Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapUInt64Value unwrapUInt64Value
      )
    ,
      ( "google.protobuf.FloatValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapFloatValue unwrapFloatValue
      )
    ,
      ( "google.protobuf.DoubleValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapDoubleValue unwrapDoubleValue
      )
    ,
      ( "google.protobuf.StringValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapStringValue unwrapStringValue
      )
    ,
      ( "google.protobuf.BytesValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapBytesValue unwrapBytesValue
      )
    ]


-- | Build a WKT-style 'AnyCodec' (envelope = @"value"@).
wktCodecVia
  :: (ByteString -> Either e a)
  -- ^ wire decoder
  -> (a -> ByteString)
  -- ^ wire encoder
  -> (a -> Aeson.Value)
  -- ^ JSON encoder
  -> (Aeson.Value -> Either String a)
  -- ^ JSON decoder
  -> AnyCodec
wktCodecVia decBytes encBytes encJson decJson =
  AnyCodec
    { acToJSON = \bs -> case decBytes bs of
        Left _ -> Left "Any: failed to decode embedded value"
        Right a -> Right (encJson a)
    , acFromJSON = fmap encBytes . decJson
    , acIsWkt = True
    }
