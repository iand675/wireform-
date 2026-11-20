{-# LANGUAGE BangPatterns #-}

{- | The CEL standard library: arithmetic and comparison operators, the
membership / indexing operators, type conversions, and the built-in
functions (size, string functions, regex matching, and the timestamp /
duration accessors).

Semantics follow the "Standard Definitions" section of the CEL language
definition: numeric operators check for overflow, division / modulus by
zero are errors, ordering and equality treat the numeric types as a single
number line, and @NaN@ is unordered and never equal.

Timestamp accessors support named IANA timezones, @UTC@, and fixed
@±HH:MM@ offsets.
-}
module CEL.Stdlib (
  arith,
  negateValue,
  notValue,
  ordCompare,
  inOp,
  indexValue,
  selectField,
  callFunction,
  callGlobalFunction,
  callMemberFunction,
  sizeOf,
  mkTimestampNanos,
  mkDurationNanos,
  protobufJsonValue,
  protobufJsonList,
  protobufJsonStruct,
) where

import CEL.Error
import CEL.Internal.Number (parseDecimalDouble)
import CEL.Syntax (ArithOp (..), RelOp (..))
import CEL.Value
import Data.Bits ((.&.), countTrailingZeros, shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (isAsciiLower, isDigit, toUpper)
import Data.Int (Int64)
import Data.Map.Lazy qualified as Map
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Text.Read qualified as TR
import Data.Time.Calendar (dayOfWeek, toGregorian)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Zones (diffForPOSIX)
import Data.Time.Zones.All (fromTZName, tzByLabel)
import Data.Vector qualified as V
import Data.Word (Word64)
import Foreign.C.Types (CChar, CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import GHC.Float (castDoubleToWord64)
import Numeric (floatToDigits)
import System.IO.Unsafe (unsafePerformIO)
import Wireform.Base64 (encodeBase64)


----------------------------------------------------------------------
-- Integer helpers
----------------------------------------------------------------------

i64min, i64max, u64max :: Integer
i64min = toInteger (minBound :: Int64)
i64max = toInteger (maxBound :: Int64)
u64max = toInteger (maxBound :: Word64)


mkInt :: Integer -> Either CelError Value
mkInt r
  | r < i64min || r > i64max = Left (overflow "integer")
  | otherwise = Right (VInt (fromInteger r))


mkUInt :: Integer -> Either CelError Value
mkUInt r
  | r < 0 || r > u64max = Left (overflow "unsigned integer")
  | otherwise = Right (VUInt (fromInteger r))


----------------------------------------------------------------------
-- Arithmetic
----------------------------------------------------------------------

-- | Evaluate a binary arithmetic operator.
arith :: ArithOp -> Value -> Value -> Either CelError Value
arith op a b = case op of
  Add -> add a b
  Sub -> sub a b
  Mul -> mul a b
  Div -> divide a b
  Mod -> modulo a b


add :: Value -> Value -> Either CelError Value
add (VInt x) (VInt y) = mkInt (toInteger x + toInteger y)
add (VUInt x) (VUInt y) = mkUInt (toInteger x + toInteger y)
add (VDouble x) (VDouble y) = Right (VDouble (x + y))
add (VString x) (VString y) = Right (VString (x <> y))
add (VBytes x) (VBytes y) = Right (VBytes (x <> y))
add (VList x) (VList y) = Right (VList (x <> y))
add (VTimestamp t) (VDuration d) = addTsDur t d
add (VDuration d) (VTimestamp t) = addTsDur t d
add (VDuration x) (VDuration y) = mkDurationNanos (durationNanos x + durationNanos y)
add _ _ = Left (noOverload "_+_")


sub :: Value -> Value -> Either CelError Value
sub (VInt x) (VInt y) = mkInt (toInteger x - toInteger y)
sub (VUInt x) (VUInt y) = mkUInt (toInteger x - toInteger y)
sub (VDouble x) (VDouble y) = Right (VDouble (x - y))
sub (VTimestamp x) (VTimestamp y) = mkDurationNanos (timestampNanos x - timestampNanos y)
sub (VTimestamp t) (VDuration d) = mkTimestampNanos (timestampNanos t - durationNanos d)
sub (VDuration x) (VDuration y) = mkDurationNanos (durationNanos x - durationNanos y)
sub _ _ = Left (noOverload "_-_")


mul :: Value -> Value -> Either CelError Value
mul (VInt x) (VInt y) = mkInt (toInteger x * toInteger y)
mul (VUInt x) (VUInt y) = mkUInt (toInteger x * toInteger y)
mul (VDouble x) (VDouble y) = Right (VDouble (x * y))
mul _ _ = Left (noOverload "_*_")


divide :: Value -> Value -> Either CelError Value
divide (VInt _) (VInt 0) = Left (divByZero "divide by zero")
divide (VInt x) (VInt y)
  | x == minBound && y == (-1) = Left (overflow "integer")
  | otherwise = Right (VInt (x `quot` y))
divide (VUInt _) (VUInt 0) = Left (divByZero "divide by zero")
divide (VUInt x) (VUInt y) = Right (VUInt (x `quot` y))
divide (VDouble x) (VDouble y) = Right (VDouble (x / y))
divide _ _ = Left (noOverload "_/_")


modulo :: Value -> Value -> Either CelError Value
modulo (VInt _) (VInt 0) = Left (divByZero "modulus by zero")
modulo (VInt x) (VInt y)
  | x == minBound && y == (-1) = Right (VInt 0)
  | otherwise = Right (VInt (x `rem` y))
modulo (VUInt _) (VUInt 0) = Left (divByZero "modulus by zero")
modulo (VUInt x) (VUInt y) = Right (VUInt (x `rem` y))
modulo _ _ = Left (noOverload "_%_")


-- | Unary negation.
negateValue :: Value -> Either CelError Value
negateValue (VInt x)
  | x == minBound = Left (overflow "integer")
  | otherwise = Right (VInt (negate x))
negateValue (VDouble x) = Right (VDouble (negate x))
negateValue _ = Left (noOverload "-_")


-- | Logical NOT.
notValue :: Value -> Either CelError Value
notValue (VBool b) = Right (VBool (not b))
notValue _ = Left (noOverload "!_")


----------------------------------------------------------------------
-- Timestamp / duration range construction
----------------------------------------------------------------------

-- Valid timestamp seconds range for [0001-01-01T00:00:00Z,
-- 9999-12-31T23:59:59.999999999Z].
tsSecMin, tsSecMax :: Integer
tsSecMin = -62135596800
tsSecMax = 253402300799


mkTimestampNanos :: Integer -> Either CelError Value
mkTimestampNanos total =
  let (s, n) = total `divMod` 1000000000
  in if s < tsSecMin || s > tsSecMax
       then Left (overflow "timestamp")
       else Right (VTimestamp (Timestamp (fromInteger s) (fromInteger n)))


mkDurationNanos :: Integer -> Either CelError Value
mkDurationNanos total =
  let (s, n) = total `quotRem` 1000000000
  in if abs s > 315576000000
       then Left (overflow "duration")
       else Right (VDuration (Duration (fromInteger s) (fromInteger n)))


addTsDur :: Timestamp -> Duration -> Either CelError Value
addTsDur t d = mkTimestampNanos (timestampNanos t + durationNanos d)


----------------------------------------------------------------------
-- Comparison / membership / indexing
----------------------------------------------------------------------

{- | Ordering comparison (@<@, @<=@, @>@, @>=@). Cross-type numeric ordering is
supported; @NaN@ comparisons are @false@; incompatible types are a
no-overload error.
-}
ordCompare :: RelOp -> Value -> Value -> Either CelError Value
ordCompare op a b
  | isNumeric a && isNumeric b = decide
  | sameOrderable a b = decide
  | otherwise = Left (noOverload (relName op))
  where
    decide = case compareValues a b of
      Just o -> Right (VBool (applyOrd op o))
      Nothing -> Right (VBool False)


applyOrd :: RelOp -> Ordering -> Bool
applyOrd Lt o = o == LT
applyOrd Le o = o /= GT
applyOrd Gt o = o == GT
applyOrd Ge o = o /= LT
applyOrd _ _ = False


relName :: RelOp -> Text
relName = \case
  Lt -> "_<_"
  Le -> "_<=_"
  Gt -> "_>_"
  Ge -> "_>=_"
  Eq -> "_==_"
  Ne -> "_!=_"
  In -> "@in"


sameOrderable :: Value -> Value -> Bool
sameOrderable a b = case (a, b) of
  (VBool _, VBool _) -> True
  (VString _, VString _) -> True
  (VBytes _, VBytes _) -> True
  (VTimestamp _, VTimestamp _) -> True
  (VDuration _, VDuration _) -> True
  _ -> False


-- | The @in@ operator: membership in a list, or key membership in a map.
inOp :: Value -> Value -> Either CelError Value
inOp x (VList v) = Right (VBool (V.any (valueEq x) v))
inOp x (VMap m) = Right (VBool (maybe False (const True) (celMapLookup x m)))
inOp _ _ = Left (noOverload "@in")


-- | Indexing: @list[int]@ and @map[key]@.
indexValue :: Value -> Value -> Either CelError Value
indexValue (VList v) i = asListIndex (V.length v) i >>= unpackValue . V.unsafeIndex v
indexValue (VMap m) k = case celMapLookup k m of
  Just val -> unpackValue val
  Nothing -> Left (noSuchKey (renderKey k))
indexValue _ _ = Left (noOverload "_[_]")


-- Check bounds before narrowing to the machine Int used by Vector.
asListIndex :: Int -> Value -> Either CelError Int
asListIndex len = \case
  VInt n
    | n >= 0 && n < fromIntegral len -> Right (fromIntegral n)
    | otherwise -> outOfBounds
  VUInt n
    | n < fromIntegral len -> Right (fromIntegral n)
    | otherwise -> outOfBounds
  VDouble d
    | not (isNaN d || isInfinite d)
    , let n = truncate d :: Integer
    , fromInteger n == d ->
        if n >= 0 && n < toInteger len then Right (fromInteger n) else outOfBounds
  _ -> Left (noOverload "_[_]")
  where
    outOfBounds = Left (CelError ErrNoSuchKey "index out of bounds")


renderKey :: Value -> Text
renderKey = \case
  VString s -> s
  VInt i -> intToText i
  VUInt u -> intToText u
  VBool b -> if b then "true" else "false"
  v -> T.pack (show v)


-- | Field selection @e.f@. An unset protobuf field yields its default.
selectField :: Value -> Text -> Either CelError Value
selectField (VMap m) f = case celMapLookup (VString f) m of
  Just v -> unpackValue v
  Nothing -> Left (noSuchKey f)
selectField (VMessage (CelMessage _ fields)) f = case Map.lookup f fields of
  Just field -> selectCelField field >>= unpackValue
  Nothing -> Left (noSuchField f)
selectField _ f = Left (noSuchField f)


----------------------------------------------------------------------
-- size
----------------------------------------------------------------------

-- | The @size@ of a string (code points), bytes (octets), list, or map.
sizeOf :: Value -> Either CelError Value
sizeOf = \case
  VString s -> Right (VInt (fromIntegral (T.length s)))
  VBytes b -> Right (VInt (fromIntegral (BS.length b)))
  VList v -> Right (VInt (fromIntegral (V.length v)))
  VMap m -> Right (VInt (fromIntegral (celMapSize m)))
  _ -> Left (noOverload "size")


----------------------------------------------------------------------
-- Standard functions
----------------------------------------------------------------------

{- | Dispatch a built-in whose arguments have already been flattened.
Expression evaluation uses the form-specific dispatchers below.
-}
callFunction :: Text -> [Value] -> Either CelError Value
callFunction name args = case callGlobalFunction name args of
  Left e | errKind e == ErrNoOverload -> callMemberFunction name args
  result -> result


-- | Dispatch a global call without a receiver.
callGlobalFunction :: Text -> [Value] -> Either CelError Value
callGlobalFunction name args = case (name, args) of
  ("size", [v]) -> sizeOf v
  ("type", [v]) -> Right (VType (typeOf v))
  ("dyn", [v]) -> Right v
  -- Boolean
  ("bool", [VBool b]) -> Right (VBool b)
  ("bool", [VString s]) -> parseBoolText s
  -- Bytes
  ("bytes", [VBytes b]) -> Right (VBytes b)
  ("bytes", [VString s]) -> Right (VBytes (TE.encodeUtf8 s))
  -- Double
  ("double", [VDouble d]) -> Right (VDouble d)
  ("double", [VInt i]) -> Right (VDouble (fromIntegral i))
  ("double", [VUInt u]) -> Right (VDouble (fromIntegral u))
  ("double", [VString s]) -> parseDoubleText s
  -- Int
  ("int", [VInt i]) -> Right (VInt i)
  ("int", [VUInt u]) -> if toInteger u > i64max then Left (overflow "integer") else Right (VInt (fromIntegral u))
  ("int", [VDouble d]) -> doubleToInt d
  ("int", [VString s]) -> parseIntText s
  ("int", [VTimestamp t]) -> Right (VInt (tsSeconds t))
  -- Uint
  ("uint", [VUInt u]) -> Right (VUInt u)
  ("uint", [VInt i]) -> if i < 0 then Left (overflow "unsigned integer") else Right (VUInt (fromIntegral i))
  ("uint", [VDouble d]) -> doubleToUint d
  ("uint", [VString s]) -> parseUintText s
  -- String
  ("string", [VString s]) -> Right (VString s)
  ("string", [VBool b]) -> Right (VString (if b then "true" else "false"))
  ("string", [VInt i]) -> Right (VString (intToText i))
  ("string", [VUInt u]) -> Right (VString (intToText u))
  ("string", [VDouble d]) -> Right (VString (doubleToText d))
  ("string", [VBytes b]) -> bytesToString b
  ("string", [VTimestamp t]) -> Right (VString (formatTimestamp t))
  ("string", [VDuration d]) -> Right (VString (formatDuration (durationNanos d)))
  -- Duration / timestamp
  ("duration", [VDuration d]) -> Right (VDuration d)
  ("duration", [VString s]) -> parseDurationText s
  ("timestamp", [VTimestamp t]) -> Right (VTimestamp t)
  ("timestamp", [VString s]) -> parseTimestampText s
  ("timestamp", [VInt i]) -> mkTimestampNanos (toInteger i * 1000000000)
  ("matches", [VString s, VString p]) -> matchesRegex s p
  _ -> Left (noOverload name)


-- | Dispatch a member call with the receiver as its first argument.
callMemberFunction :: Text -> [Value] -> Either CelError Value
callMemberFunction name args = case (name, args) of
  ("size", _) -> callGlobalFunction name args
  ("matches", _) -> callGlobalFunction name args
  -- String functions
  ("contains", [VString s, VString needle]) -> Right (VBool (needle `T.isInfixOf` s))
  ("startsWith", [VString s, VString p]) -> Right (VBool (p `T.isPrefixOf` s))
  ("endsWith", [VString s, VString p]) -> Right (VBool (p `T.isSuffixOf` s))
  ("contains", [VBytes s, VBytes needle]) -> Right (VBool (needle `BS.isInfixOf` s))
  ("startsWith", [VBytes s, VBytes p]) -> Right (VBool (p `BS.isPrefixOf` s))
  ("endsWith", [VBytes s, VBytes p]) -> Right (VBool (p `BS.isSuffixOf` s))
  -- Timestamp accessors
  ("getFullYear", _) -> tsAccessor args (\(y, _, _, _, _, _, _, _) -> y)
  ("getMonth", _) -> tsAccessor args (\(_, mo, _, _, _, _, _, _) -> mo - 1)
  ("getDate", _) -> tsAccessor args (\(_, _, d, _, _, _, _, _) -> d)
  ("getDayOfMonth", _) -> tsAccessor args (\(_, _, d, _, _, _, _, _) -> d - 1)
  ("getDayOfYear", _) -> tsAccessor args (\(_, _, _, _, _, _, doy, _) -> doy)
  ("getDayOfWeek", _) -> tsAccessor args (\(_, _, _, _, _, _, _, dow) -> dow)
  ("getHours", [VDuration d]) -> Right (VInt (fromInteger (durationNanos d `quot` 3600000000000)))
  ("getMinutes", [VDuration d]) -> Right (VInt (fromInteger (durationNanos d `quot` 60000000000)))
  ("getSeconds", [VDuration d]) -> Right (VInt (fromInteger (durationNanos d `quot` 1000000000)))
  ("getMilliseconds", [VDuration d]) ->
    Right (VInt (fromInteger ((durationNanos d `quot` 1000000) `rem` 1000)))
  ("getHours", _) -> tsAccessor args (\(_, _, _, h, _, _, _, _) -> h)
  ("getMinutes", _) -> tsAccessor args (\(_, _, _, _, mi, _, _, _) -> mi)
  ("getSeconds", _) -> tsAccessor args (\(_, _, _, _, _, se, _, _) -> se)
  ("getMilliseconds", [VTimestamp t]) -> Right (timestampMillis t)
  ("getMilliseconds", [VTimestamp t, VString tz]) -> timestampMillis t <$ resolveOffset tz t
  _ -> Left (noOverload name)
  where
    timestampMillis t = VInt (fromIntegral (tsNanos t `div` 1000000))


----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

parseBoolText :: Text -> Either CelError Value
parseBoolText s
  | s `elem` ["true", "True", "TRUE", "t", "T", "1"] = Right (VBool True)
  | s `elem` ["false", "False", "FALSE", "f", "F", "0"] = Right (VBool False)
  | otherwise = Left (conversion ("cannot convert string to bool: " <> s))


parseDoubleText :: Text -> Either CelError Value
parseDoubleText s0 =
  let s = T.strip s0
      invalid = Left (conversion ("cannot convert string to double: " <> s0))
  in case parseDecimalDouble s of
       Just d | isInfinite d -> Left (conversion ("double out of range: " <> s0))
       Just d -> Right (VDouble d)
       Nothing
         | T.compareLength s 9 == GT -> invalid
         | otherwise -> case T.toLower s of
             "inf" -> Right (VDouble (1 / 0))
             "+inf" -> Right (VDouble (1 / 0))
             "infinity" -> Right (VDouble (1 / 0))
             "+infinity" -> Right (VDouble (1 / 0))
             "-inf" -> Right (VDouble (-1 / 0))
             "-infinity" -> Right (VDouble (-1 / 0))
             "nan" -> Right (VDouble (0 / 0))
             _ -> invalid


parseIntText :: Text -> Either CelError Value
parseIntText input = do
  let text = T.strip input
      (negative, digits) = case T.uncons text of
        Just ('-', rest) -> (True, rest)
        Just ('+', rest) -> (False, rest)
        _ -> (False, text)
      invalid = conversion ("cannot convert string to int: " <> input)
  n <- decimalMagnitude invalid (overflow "integer") digits
  mkInt (if negative then negate n else n)


parseUintText :: Text -> Either CelError Value
parseUintText input =
  let invalid = conversion ("cannot convert string to uint: " <> input)
  in decimalMagnitude invalid (overflow "unsigned integer") (T.strip input) >>= mkUInt


-- Check the input before accumulating. Any 64-bit integer has at most
-- twenty significant decimal digits, regardless of the number of zeros.
decimalMagnitude :: CelError -> CelError -> Text -> Either CelError Integer
decimalMagnitude invalid tooLarge digits
  | T.null digits = Left invalid
  | T.null significant = Right 0
  | T.compareLength significant 20 == GT =
      Left (if T.all isDigit significant then tooLarge else invalid)
  | otherwise = case TR.decimal significant of
      Right (n, rest) | T.null rest -> Right n
      _ -> Left invalid
  where
    significant = T.dropWhile (== '0') digits


doubleToInt :: Double -> Either CelError Value
doubleToInt d
  | isNaN d || isInfinite d = Left (conversion "cannot convert double to int")
  -- The valid range is (minInt, maxInt) non-inclusive (the conservative
  -- round-trip bound from the spec), so exactly +-2^63 is also out of range.
  | d >= 9223372036854775808.0 = Left (overflow "integer")
  | d <= -9223372036854775808.0 = Left (overflow "integer")
  | otherwise = Right (VInt (truncate d))


doubleToUint :: Double -> Either CelError Value
doubleToUint d
  | isNaN d || isInfinite d = Left (conversion "cannot convert double to uint")
  | d < 0 = Left (overflow "unsigned integer")
  | d >= 18446744073709551616.0 = Left (overflow "unsigned integer")
  | otherwise = Right (VUInt (truncate d))


bytesToString :: BS.ByteString -> Either CelError Value
bytesToString b = case TE.decodeUtf8' b of
  Right t -> Right (VString t)
  Left _ -> Left (conversion "invalid UTF-8 in bytes to string conversion")


matchesRegex :: Text -> Text -> Either CelError Value
matchesRegex subject patternText = unsafePerformIO $
  BSU.unsafeUseAsCStringLen (TE.encodeUtf8 subject) $ \(subjectPtr, subjectLen) ->
    BSU.unsafeUseAsCStringLen (TE.encodeUtf8 patternText) $ \(patternPtr, patternLen) -> do
      result <- re2Match subjectPtr (fromIntegral subjectLen) patternPtr (fromIntegral patternLen)
      pure $ case result of
        0 -> Right (VBool False)
        1 -> Right (VBool True)
        -1 -> Left (invalidArg ("invalid regular expression: " <> patternText))
        _ -> Left (invalidArg "regular expression evaluation failed")
{-# NOINLINE matchesRegex #-}


foreign import ccall safe "wireform_cel_re2_match"
  re2Match :: Ptr CChar -> CSize -> Ptr CChar -> CSize -> IO CInt


----------------------------------------------------------------------
-- Text formatting helpers
----------------------------------------------------------------------

intToText :: Integral a => a -> Text
intToText = TL.toStrict . TB.toLazyText . TBI.decimal


-- Conversion through google.protobuf.Value preserves doubles as numbers,
-- including IEEE special values. JSON serialization is a separate step.
protobufJsonValue :: Value -> Either CelError Value
protobufJsonValue value = case value of
  VNull -> Right value
  VBool _ -> Right value
  VString _ -> Right value
  VDouble _ -> Right value
  VInt n -> Right (jsonInteger n)
  VUInt n -> Right (jsonInteger n)
  VBytes bytes -> Right (VString (base64Text bytes))
  VDuration d -> Right (VString (formatDuration (durationNanos d)))
  VTimestamp t -> Right (VString (formatTimestamp t))
  VList _ -> protobufJsonList value
  VMap _ -> protobufJsonStruct value
  VMessage (CelMessage "google.protobuf.Empty" _) -> Right (VMap (celMapFromList []))
  VMessage (CelMessage "google.protobuf.FieldMask" fields) ->
    case Map.lookup "paths" fields of
      Nothing -> Right (VString "")
      Just field -> do
        paths <- selectCelField field
        case paths of
          VList values -> VString . T.intercalate "," <$> traverse renderPath (V.toList values)
          _ -> Left (noOverload "FieldMask.paths")
  VMessage _ -> Left (noOverload "google.protobuf.Value")
  VAny {} -> unpackValue value >>= protobufJsonValue
  VType _ -> Left (noOverload "google.protobuf.Value")
  where
    jsonInteger :: Integral a => a -> Value
    jsonInteger n
      | integer >= -9007199254740991 && integer <= 9007199254740991 = VDouble (fromIntegral n)
      | otherwise = VString (intToText n)
      where
        integer = toInteger n

    renderPath (VString path)
      | T.all (\c -> isAsciiLower c || ('0' <= c && c <= '9') || c == '.' || c == '_') path =
          case T.splitOn "_" path of
            first : rest -> (first <>) . T.concat <$> traverse capitalize rest
            [] -> Right ""
      | otherwise = Left (conversion "irreversible FieldMask path")
    renderPath _ = Left (noOverload "FieldMask.paths")
    capitalize part = case T.uncons part of
      Just (c, rest) | isAsciiLower c -> Right (T.cons (toUpper c) rest)
      _ -> Left (conversion "irreversible FieldMask path")

    -- Keep each call below the C encoder's length limit. Full chunks
    -- contain complete triplets, so padding occurs only in the last one.
    base64Text = T.concat . encodeChunks
    encodeChunks bytes
      | BS.null bytes = []
      | otherwise =
          let (chunk, rest) = BS.splitAt (3 * 1024 * 1024) bytes
          in TE.decodeUtf8 (encodeBase64 chunk) : encodeChunks rest


protobufJsonList :: Value -> Either CelError Value
protobufJsonList (VList values) = VList <$> V.mapM protobufJsonValue values
protobufJsonList _ = Left (noOverload "google.protobuf.ListValue")


protobufJsonStruct :: Value -> Either CelError Value
protobufJsonStruct (VMap values) = VMap <$> traverseCelMapValues convertEntry values
  where
    convertEntry (VString _) value = protobufJsonValue value
    convertEntry _ _ = Left (noOverload "google.protobuf.Struct requires string keys")
protobufJsonStruct _ = Left (noOverload "google.protobuf.Struct")


{- | Format a double the way reference CEL (Go's @strconv.FormatFloat(_, 'g',
-1, 64)@) does: shortest round-tripping decimal, switching to scientific
notation when the decimal exponent is @< -4@ or @>= 6@.
-}
doubleToText :: Double -> Text
doubleToText d
  | isNaN d = "NaN"
  | isInfinite d = if d > 0 then "+Inf" else "-Inf"
  | d == 0 = if isNegativeZero d then "-0" else "0"
  | otherwise =
      let sign = if d < 0 then "-" else ""
          (openDigits, point) = floatToDigits 10 (abs d)
          scale = point - length openDigits
          coefficient = foldl' (\n digit -> n * 10 + fromIntegral digit) 0 openDigits
          decimal = shortestDecimal (abs d) coefficient scale
          raw = intToText (decimalCoefficient decimal)
          digits = T.dropWhileEnd (== '0') raw
          n = decimalScale decimal + T.length raw
          expo = n - 1
      in sign <> if expo < -4 || expo >= 6 then sci digits expo else fixed digits n
  where
    sci digits expo =
      let (first, rest) = T.splitAt 1 digits
          mantissa = if T.null rest then first else first <> "." <> rest
          expSign = if expo < 0 then "-" else "+"
      in mantissa <> "e" <> expSign <> T.justifyRight 2 '0' (intToText (abs expo))
    fixed digits n
      | n <= 0 = "0." <> T.replicate (negate n) "0" <> digits
      | n >= T.length digits = digits <> T.replicate (n - T.length digits) "0"
      | otherwise = let (a, b) = T.splitAt n digits in a <> "." <> b


data Decimal = Decimal
  { decimalCoefficient :: {-# UNPACK #-} !Word64
  , decimalScale :: {-# UNPACK #-} !Int
  }


-- GHC supplies the shortest decimal strictly inside the binary rounding
-- interval. An even significand can also round-trip from either endpoint.
-- Only the two neighboring points on the next coarser decimal grid can
-- produce a shorter representation. Within the same grid, ties go to even.
shortestDecimal :: Double -> Word64 -> Int -> Decimal
shortestDecimal d coefficient scale
  | even significand && lower /= 0 && onBoundary lower = Decimal lower coarser
  | even significand && coefficient >= 10 && onBoundary (lower + 1) =
      Decimal (lower + 1) coarser
  | coefficient > 1 && odd coefficient
  , sameDecimal significand (binaryScale + 1) (2 * coefficient - 1) scale
  , not closerLower || lowerTieRoundsBack =
      Decimal (coefficient - 1) scale
  | otherwise = Decimal coefficient scale
  where
    bits = castDoubleToWord64 d
    encodedExponent = fromIntegral (bits `shiftR` 52) :: Int
    leadingBit = 0x10000000000000
    fraction = bits .&. (leadingBit - 1)
    significand = if encodedExponent == 0 then fraction else leadingBit + fraction
    binaryScale = max 1 encodedExponent - 1075
    -- A normal power of two above the smallest normal has half the
    -- spacing on the lower side.
    closerLower = fraction == 0 && encodedExponent > 1
    lower = coefficient `quot` 10
    coarser = scale + 1
    onBoundary candidate =
      let lowerMantissa = (if closerLower then 4 else 2) * significand - 1
          lowerScale = binaryScale - if closerLower then 2 else 1
      in sameDecimal lowerMantissa lowerScale candidate coarser
           || sameDecimal (2 * significand + 1) (binaryScale - 1) candidate coarser
    -- At an asymmetric interval, the equally near lower decimal can fall
    -- outside the interval even though the upper decimal round-trips.
    lowerTieRoundsBack =
      fromRational ((toInteger (coefficient - 1) % 1) * 10 ^^ scale) == d


-- Compare m * 2^b to c * 10^e exactly. Removing factors of two rules out
-- most candidates before any arbitrary-precision arithmetic is needed.
sameDecimal :: Word64 -> Int -> Word64 -> Int -> Bool
sameDecimal m b c e
  | b + binaryTwos /= e + decimalTwos = False
  | e == 0 = binaryOdd == decimalOdd
  | e > 0 = toInteger binaryOdd == toInteger decimalOdd * 5 ^ e
  | otherwise = toInteger binaryOdd * 5 ^ negate e == toInteger decimalOdd
  where
    binaryTwos = countTrailingZeros m
    decimalTwos = countTrailingZeros c
    binaryOdd = m `shiftR` binaryTwos
    decimalOdd = c `shiftR` decimalTwos


----------------------------------------------------------------------
-- Duration parsing / formatting
----------------------------------------------------------------------

{- | Format a duration (given as total nanoseconds) as seconds with an @s@
suffix, e.g. @"60.001s"@.
-}
formatDuration :: Integer -> Text
formatDuration total =
  let neg = total < 0
      a = abs total
      (secs, nanos) = a `divMod` 1000000000
      sign = if neg then "-" else ""
      secsT = intToText secs
      fracT = if nanos == 0 then "" else "." <> trimTrailingZeros (padLeft 9 (intToText nanos))
  in sign <> secsT <> fracT <> "s"


padLeft :: Int -> Text -> Text
padLeft n t = T.replicate (max 0 (n - T.length t)) "0" <> t


trimTrailingZeros :: Text -> Text
trimTrailingZeros = T.dropWhileEnd (== '0')


{- | Parse a CEL duration string such as @"1h30m"@, @"-1.5h"@, @"1h34us"@, or
@"0"@.
-}
parseDurationText :: Text -> Either CelError Value
parseDurationText input =
  let (neg, rest) = case T.uncons input of
        Just ('-', r) -> (True, r)
        Just ('+', r) -> (False, r)
        _ -> (False, input)
  in if rest == "0"
       then Right (VDuration (Duration 0 0))
       else
         if T.null rest
           then Left (conversion ("invalid duration string: " <> input))
           else do
             nanos <- parseUnits rest 0
             let signed = if neg then negate nanos else nanos
             mkDurationNanos signed


-- Parse one-or-more (number, unit) segments accumulating nanoseconds.
parseUnits :: Text -> Integer -> Either CelError Integer
parseUnits t acc
  | T.null t = Right acc
  | otherwise =
      let isNumberChar c = isDigit c || c == '.'
          (numText, t1) = T.span isNumberChar t
          (unitText, t2) = T.break isNumberChar t1
      in if T.null numText
           then dErr
           else case unitNanos unitText of
             Nothing -> dErr
             Just per -> case parseNumberRational numText of
               Nothing -> dErr
               Just q -> parseUnits t2 (acc + truncate (q * fromInteger per))
  where
    dErr = Left (conversion "invalid duration string")


unitNanos :: Text -> Maybe Integer
unitNanos u = case u of
  "h" -> Just 3600000000000
  "m" -> Just 60000000000
  "s" -> Just 1000000000
  "ms" -> Just 1000000
  "us" -> Just 1000
  "µs" -> Just 1000
  "μs" -> Just 1000
  "ns" -> Just 1
  _ -> Nothing


parseNumberRational :: Text -> Maybe Rational
parseNumberRational t
  | t == "." = Nothing
  | T.isPrefixOf "." t = readNumber ("0" <> t)
  | Just whole <- T.stripSuffix "." t = readNumber whole
  | otherwise = readNumber t
  where
    readNumber input = case TR.rational input of
      Right (q, rest) | T.null rest -> Just q
      _ -> Nothing


----------------------------------------------------------------------
-- Timestamp parsing / formatting / accessors
----------------------------------------------------------------------

formatTimestamp :: Timestamp -> Text
formatTimestamp (Timestamp secs nanos) =
  let utct = posixSecondsToUTCTime (fromIntegral secs)
      base = formatTime defaultTimeLocale "%04Y-%m-%dT%H:%M:%S" utct
      frac =
        if nanos == 0
          then ""
          else "." ++ T.unpack (trimTrailingZeros (padLeft 9 (intToText (toInteger nanos))))
  in T.pack (base ++ frac ++ "Z")


parseTimestampText :: Text -> Either CelError Value
parseTimestampText input =
  case parseTimestampUTC input of
    Nothing -> Left (conversion ("cannot parse timestamp: " <> input))
    Just posix -> mkTimestampNanos (round (posix * 1000000000))


parseTimestampUTC :: Text -> Maybe POSIXTime
parseTimestampUTC input = do
  let (base, suffix) = T.splitAt 19 (T.toUpper input)
      (fraction, zone) = case T.stripPrefix "." suffix of
        Just rest ->
          let (digits, z) = T.span isDigit rest
          in ("." <> T.take 9 digits, z)
        Nothing -> ("", suffix)
  offset <- case T.uncons zone of
    Just ('Z', "") -> Just 0
    Just (sign, _) | sign == '+' || sign == '-' -> fixedOffset zone
    _ -> Nothing
  if validTimestampBase base && fraction /= "."
    then do
      local <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack (base <> fraction <> "Z"))
      pure (utcTimeToPOSIXSeconds local - fromIntegral offset)
    else Nothing


validTimestampBase :: Text -> Bool
validTimestampBase base =
  T.length base == 19
    && T.index base 4 == '-'
    && T.index base 7 == '-'
    && T.index base 10 == 'T'
    && T.index base 13 == ':'
    && T.index base 16 == ':'
    && T.length (T.filter isDigit base) == 14
    && T.index base 17 <= '5'


-- (year, month, day, hour, minute, second, dayOfYear0, dayOfWeek0)
type TsParts = (Int, Int, Int, Int, Int, Int, Int, Int)


tsAccessor :: [Value] -> (TsParts -> Int) -> Either CelError Value
tsAccessor args sel = case args of
  [VTimestamp t] -> withParts t 0
  [VTimestamp t, VString tz] -> do
    off <- resolveOffset tz t
    withParts t off
  _ -> Left (noOverload "timestamp accessor")
  where
    withParts t off = Right (VInt (fromIntegral (sel (timestampParts t off))))


timestampParts :: Timestamp -> Int -> TsParts
timestampParts (Timestamp secs _) off =
  let utct = posixSecondsToUTCTime (fromIntegral (toInteger secs + toInteger off))
      day = utctDay utct
      (y, mo, d) = toGregorian day
      secsOfDay = fromInteger (diffTimeToPicoseconds (utctDayTime utct) `div` 1000000000000) :: Int
      hh = secsOfDay `div` 3600
      mm = (secsOfDay `div` 60) `mod` 60
      ss = secsOfDay `mod` 60
      (_, ord) = toOrdinalDate day
      dow = fromEnum (dayOfWeek day) `mod` 7
  in (fromInteger y, mo, d, hh, mm, ss, ord - 1, dow)


{- | Resolve a timezone to its offset in seconds /at the given instant/.
Supports @UTC@, the empty string (UTC), fixed @±HH:MM@ offsets (the sign is
optional), and named IANA/Joda zones (with their DST rules applied at the
instant in question) via the bundled zone database.
-}
resolveOffset :: Text -> Timestamp -> Either CelError Int
resolveOffset tz ts
  | tz == "" || tz == "UTC" = Right 0
  | Just off <- fixedOffset tz = Right off
  | otherwise = case fromTZName (TE.encodeUtf8 tz) of
      Just label -> Right (diffForPOSIX (tzByLabel label) (tsSeconds ts))
      Nothing -> Left (invalidArg ("unsupported timezone: " <> tz))


fixedOffset :: Text -> Maybe Int
fixedOffset t0 =
  let (neg, t) = case T.uncons t0 of
        Just ('+', r) -> (False, r)
        Just ('-', r) -> (True, r)
        _ -> (False, t0)
  in case T.splitOn ":" t of
       [hh, mm]
         | T.length hh == 2
         , T.length mm == 2
         , Right (h, hr) <- TR.decimal hh
         , T.null hr
         , h < 24
         , Right (m, mr) <- TR.decimal mm
         , T.null mr
         , m < 60 ->
             let off = h * 3600 + m * 60
             in Just (if neg then negate off else off)
       _ -> Nothing
