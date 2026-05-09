{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Format keyword validation
--
-- Validates format constraints for string values. Supports RFC-compliant validators
-- for email, URI, IP addresses, date-time formats, hostnames (including IDN),
-- UUID, JSON Pointer, regex patterns, and URI templates.
--
-- Format validation can operate in two modes:
-- - Annotation mode (default): Format validation failures are collected as annotations
-- - Assertion mode: Format validation failures cause schema validation to fail
module JsonSchema.Keywords.Format
  ( validateFormatConstraints
  , validateFormatValue
  ) where

import JsonSchema.Types
  ( ValidationContext(..), SchemaObject(..), ValidationResult
  , schemaValidation, SchemaValidation(..), ValidationConfig(..)
  , Format(..), FormatBehavior(..), validationFormat
  , pattern ValidationSuccess, pattern ValidationFailure
  , validationFailure, ValidationAnnotations(..)
  )
import qualified JsonSchema.Regex as Regex
import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Regex.ECMA262 as R
import qualified Data.UUID as UUID
import qualified Data.Time.Format.ISO8601 as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.RFC3339 as RFC3339
import qualified Text.Email.Validate as Email
import qualified Network.URI as URI
import qualified Data.IP as IP
import qualified Data.Text.IDN as IDN
import qualified Text.Read as Read
import qualified Network.URI.Template.Parser as URITemplate

-- | Validate format constraints
--
-- __Format Behavior__:
--
-- The behavior is determined by the validation configuration:
--
-- 1. If 'validationDialectFormatBehavior' is set (from dialect configuration):
--    * 'FormatAssertion': Format failures cause validation to fail
--    * 'FormatAnnotation': Format failures are annotations only
--
-- 2. Otherwise, falls back to 'validationFormatAssertion':
--    * 'True': Format failures cause validation to fail
--    * 'False': Format failures are annotations only (default)
--
-- __JSON Schema Specification__:
--
-- * JSON Schema 2019-09+: Format is annotation-only by default
-- * Format-assertion vocabulary: Optional vocabulary to enable format assertions
-- * Draft-07 and earlier: Spec was ambiguous
--
-- __Implementation Notes__:
--
-- This function respects dialect-specific format behavior when schemas are parsed
-- with 'parseSchemaWithDialectRegistry'. When no dialect is specified, it uses
-- the global 'validationFormatAssertion' flag for backward compatibility.
--
-- @since 0.1.0.0
validateFormatConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateFormatConstraints ctx obj (String txt) =
  let validation = schemaValidation obj
  in case validationFormat validation of
    Nothing -> ValidationSuccess mempty
    Just format ->
      let config = contextConfig ctx
          -- Check dialect-specific format behavior first, fall back to boolean flag
          shouldAssert = case validationDialectFormatBehavior config of
            Just FormatAssertion -> True
            Just FormatAnnotation -> False
            Nothing -> validationFormatAssertion config
          result = validateFormatValue format txt
      in if shouldAssert
         then result  -- Format validation failures cause schema failure
         else ValidationSuccess mempty  -- Format as annotation only
validateFormatConstraints _ _ _ = ValidationSuccess mempty

-- | Validate format values
validateFormatValue :: Format -> Text -> ValidationResult
validateFormatValue Email text
  | isValidEmail text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid email format"
validateFormatValue IDNEmail text
  | isValidIDNEmail text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IDN email format"
validateFormatValue URI text
  | isValidURI text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid URI format"
validateFormatValue URIRef text
  | isValidURIReference text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid URI reference"
validateFormatValue IRI text
  | isValidIRI text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IRI format"
validateFormatValue IRIRef text
  | isValidIRIReference text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IRI reference"
validateFormatValue IPv4 text
  | isValidIPv4 text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IPv4 address"
validateFormatValue IPv6 text
  | isValidIPv6 text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IPv6 address"
validateFormatValue UUID text
  | isValidUUID text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid UUID format"
validateFormatValue DateTime text
  | isValidDateTime text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid date-time format"
validateFormatValue Date text
  | isValidDate text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid date format"
validateFormatValue Time text
  | isValidTime text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid time format"
validateFormatValue Duration text
  | isValidDuration text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid ISO 8601 duration"
validateFormatValue Hostname text
  | isValidHostname text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid hostname"
validateFormatValue IDNHostname text
  | isValidIDNHostname text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IDN hostname"
validateFormatValue JSONPointerFormat text
  | isValidJSONPointer text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid JSON Pointer"
validateFormatValue RelativeJSONPointerFormat text
  | isValidRelativeJSONPointer text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid relative JSON Pointer"
validateFormatValue RegexFormat text
  | isValidRegex text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid ECMA-262 regex"
validateFormatValue URITemplate text
  | isValidURITemplate text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid URI template"
validateFormatValue _ _ = ValidationSuccess mempty  -- Other formats: annotation only or not implemented

-- Format validators using proper libraries
-- Validates email addresses per RFC 5321 and RFC 5322
isValidEmail :: Text -> Bool
isValidEmail text =
  -- First check with email-validate library
  if not (Email.isValid (TE.encodeUtf8 text))
    then False
    else
      -- Additional validation for IP address literals in square brackets
      -- email-validate might accept invalid IP addresses like [127.0.0.300]
      case T.splitOn "@" text of
        [_local, domain] | T.isPrefixOf "[" domain && T.isSuffixOf "]" domain ->
          -- Domain is an IP address literal [xxx.xxx.xxx.xxx] or [IPv6:...]
          let ipLiteral = T.drop 1 $ T.dropEnd 1 domain
          in if T.isPrefixOf "IPv6:" ipLiteral || T.isPrefixOf "ipv6:" ipLiteral
             then isValidIPv6 (T.drop 5 ipLiteral)  -- IPv6 literal
             else isValidIPv4 ipLiteral  -- IPv4 literal
        _ -> True  -- Not an IP literal, email-validate check is sufficient

-- IDN Email validator - validates internationalized email addresses
-- Per RFC 6531, the domain part must be a valid IDN hostname
isValidIDNEmail :: Text -> Bool
isValidIDNEmail text =
  case T.splitOn "@" text of
    [local, domain] | not (T.null local) && not (T.null domain) ->
      -- Basic structure check: must have exactly one @ with non-empty parts
      -- Validate local part: basic checks (no leading/trailing dots, valid chars)
      let validLocal = not (T.isPrefixOf "." local) &&
                      not (T.isSuffixOf "." local) &&
                      not (T.isInfixOf ".." local) &&
                      T.length local <= 64
          -- Validate domain part using IDN hostname validation
          validDomain = isValidIDNHostname domain
      in validLocal && validDomain
    _ -> False  -- Must have exactly one @

isValidURI :: Text -> Bool
isValidURI text = case URI.parseURI (T.unpack text) of
  Just _ -> True
  Nothing -> False

isValidIPv4 :: Text -> Bool
isValidIPv4 text =
  -- Reject strings with leading or trailing whitespace
  if T.any (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r') text
    then False
    else
      let parts = T.splitOn "." text
          isValidOctet part =
            -- Reject leading zeroes (except for "0" itself) to avoid octal ambiguity
            if T.length part > 1 && T.head part == '0'
              then False
              else case reads (T.unpack part) :: [(Int, String)] of
                [(n, "")] -> n >= 0 && n <= 255
                _ -> False
      in length parts == 4 && all isValidOctet parts

isValidIPv6 :: Text -> Bool
isValidIPv6 text =
  -- Reject strings with leading or trailing whitespace
  if T.any (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r') text
    then False
    else
      -- Additional pre-validation checks for RFC 4291 compliance
      -- These catch cases where the iproute parser is too lenient
      let validChars = T.all (\c -> (c >= '0' && c <= '9') ||
                                     (c >= 'a' && c <= 'f') ||
                                     (c >= 'A' && c <= 'F') ||
                                     c == ':' || c == '.') text
          -- Must not start or end with single colon (:: is ok)
          invalidColons = (T.isPrefixOf ":" text && not (T.isPrefixOf "::" text)) ||
                          (T.isSuffixOf ":" text && not (T.isSuffixOf "::" text))
          -- Check for groups with more than 4 hex digits (but allow IPv4 addresses in last group)
          groups = T.splitOn ":" text
          -- The last group might be an IPv4 address (contains dots)
          ipv6Groups = case reverse groups of
            (lastGroup:rest) | T.any (== '.') lastGroup -> reverse rest
            _ -> groups
          -- IPv6 groups must be <= 4 hex digits
          validGroupLengths = all (\g -> T.null g || T.length g <= 4) ipv6Groups
          -- Each non-empty IPv6 group must have only hex digits
          validGroupContent = all (\g -> T.null g ||
                                         T.all (\c -> (c >= '0' && c <= '9') ||
                                                     (c >= 'a' && c <= 'f') ||
                                                     (c >= 'A' && c <= 'F')) g) ipv6Groups
      in if not validChars || invalidColons || not validGroupLengths || not validGroupContent
        then False
        else
          let str = T.unpack text
          in case reads str :: [(IP.IPv6, String)] of
            [(_, "")] -> True
            _ -> False

isValidUUID :: Text -> Bool
isValidUUID text = case UUID.fromText text of
  Just _ -> True
  Nothing -> False

-- RFC3339 date-time validator
-- Validates according to RFC3339 as required by JSON Schema
-- Uses the timerep library for strict RFC3339 parsing with additional checks
isValidDateTime :: Text -> Bool
isValidDateTime text =
  -- First check with RFC3339 parser
  case RFC3339.parseTimeRFC3339 (T.unpack text) of
    Nothing -> False
    Just _ ->
      -- Additional validation for edge cases not caught by timerep:
      -- 1. Leap seconds must be at XX:59:60 (not other minutes like :58:60)
      -- 2. Leap seconds in UTC must be at 23:59:60
      -- 3. Timezone offsets must be -23:59 to +23:59 (not -24:00 or +24:00)
      let normalized = T.map (\c -> if c == 't' then 'T' else if c == 'z' then 'Z' else c) text
          hasLeapSecond = T.isInfixOf ":60" normalized || T.isInfixOf ":60." normalized
      in if hasLeapSecond
        then
          -- Check that leap second is at :59:60
          let hasValidMinute = T.isInfixOf ":59:60" normalized
              parts = T.splitOn "T" normalized
          in if length parts /= 2 || not hasValidMinute
            then False
            else
              let timePart = parts !! 1
                  isUTC = T.isSuffixOf "Z" timePart
                  timeStr = if isUTC
                           then T.dropEnd 1 timePart
                           else case T.breakOnEnd "+" timePart of
                                  (before, after) | not (T.null after) ->
                                    T.dropEnd (T.length after + 1) before
                                  _ -> case T.breakOnEnd "-" timePart of
                                         (before, after) | T.length after == 5 && T.elem ':' after ->
                                           T.dropEnd 6 before
                                         _ -> timePart
                  hour = case T.splitOn ":" timeStr of
                           (h:_) -> Read.readMaybe (T.unpack h) :: Maybe Int
                           _ -> Nothing
              in case (isUTC, hour) of
                   (True, Just h) -> h == 23  -- UTC leap seconds must be at 23:59:60
                   (False, _) -> True         -- Non-UTC can be at any XX:59:60
                   _ -> False
        else
          -- Check for invalid timezone offsets (hour must be < 24)
          let hasOffset = T.isInfixOf "+" normalized || (T.count "-" normalized > 2)
          in if hasOffset && not (T.isSuffixOf "Z" normalized)
            then
              let offsetStr = T.takeEnd 6 normalized  -- e.g., "+24:00" or "-24:00"
                  offsetHour = Read.readMaybe (T.unpack $ T.take 2 $ T.drop 1 offsetStr) :: Maybe Int
              in case offsetHour of
                   Just h -> h < 24
                   Nothing -> True  -- Couldn't parse, let RFC3339 parser decision stand
            else True

-- RFC3339 date validator
isValidDate :: Text -> Bool
isValidDate text =
  case Time.iso8601ParseM (T.unpack text) :: Maybe Time.Day of
    Just _ -> True
    Nothing -> False

-- Hostname validator (RFC 1123 + RFC 5890-5893 for A-labels)
-- Validates both regular ASCII hostnames and Punycode A-labels
isValidHostname :: Text -> Bool
isValidHostname text =
  let labels = T.splitOn "." text
      -- Check if any label is a Punycode A-label or has "--" in positions 3-4
      -- These require IDNA validation per RFC 5890
      needsIDNValidation lbl =
        T.isPrefixOf "xn--" (T.toLower lbl) ||
        (T.length lbl >= 4 && T.take 2 (T.drop 2 lbl) == "--")
  in if any needsIDNValidation labels
       then case IDN.toASCII text of
              Right _ -> True   -- IDNA validation passed
              Left _ -> False   -- IDNA validation failed
       else
         -- Pure ASCII hostname, use RFC 1123 rules
         let validLabel lbl =
               not (T.null lbl) &&
               T.length lbl <= 63 &&
               not (T.isPrefixOf "-" lbl) &&
               not (T.isSuffixOf "-" lbl) &&
               T.all (\c -> c == '-' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) lbl
         in not (null labels) &&
            T.length text <= 253 &&
            all validLabel labels

-- IDN Hostname validator (RFC 5890-5893)
isValidIDNHostname :: Text -> Bool
isValidIDNHostname text =
  -- Try to convert to ASCII using IDNA2008
  case IDN.toASCII text of
    Right ascii -> isValidHostname ascii  -- Validate the ASCII form
    Left _ -> False  -- IDN conversion failed

-- URI Reference validator (allows relative references)
isValidURIReference :: Text -> Bool
isValidURIReference text = case URI.parseURIReference (T.unpack text) of
  Just _ -> True
  Nothing -> False

-- IRI validator (RFC 3987 - Internationalized Resource Identifiers)
-- IRIs extend URIs to support Unicode characters
isValidIRI :: Text -> Bool
isValidIRI text =
  -- Try to convert to ASCII URI using IDNA for domain parts
  -- For now, we accept any valid URI or any text with Unicode chars that
  -- matches URI structure
  if T.all (\c -> fromEnum c < 128) text
    then isValidURI text  -- Pure ASCII, validate as URI
    else
      -- Contains Unicode, do basic IRI validation
      -- Must contain a scheme (before first colon)
      case T.breakOn ":" text of
        (scheme, rest) | not (T.null scheme) && not (T.null rest) ->
          -- Has a scheme, check it's valid (alphanumeric + - .)
          let validScheme = not (T.null scheme) &&
                           T.all (\c -> (c >= 'a' && c <= 'z') ||
                                       (c >= 'A' && c <= 'Z') ||
                                       (c >= '0' && c <= '9') ||
                                       c == '+' || c == '-' || c == '.') scheme
              -- Rest should not contain invalid characters (space, <>\"{}|\\^`)
              validRest = not $ T.any (\c -> c `elem` [' ', '<', '>', '"', '{', '}', '|', '\\', '^', '`']) rest
          in validScheme && validRest
        _ -> False

-- IRI Reference validator (allows relative IRIs)
isValidIRIReference :: Text -> Bool
isValidIRIReference text =
  -- If pure ASCII, validate as URI reference
  if T.all (\c -> fromEnum c < 128) text
    then isValidURIReference text
    else
      -- Contains Unicode, do basic IRI reference validation
      -- Can be relative (no scheme) or absolute (with scheme)
      -- Just check for invalid characters
      not $ T.any (\c -> c `elem` [' ', '<', '>', '"', '{', '}', '|', '\\', '^', '`']) text

-- Time validator (RFC3339 time format)
-- Per RFC 3339, time MUST include a timezone offset (Z or +/-HH:MM)
isValidTime :: Text -> Bool
isValidTime text =
  -- Pattern: HH:MM:SS[.sss](Z|z|+HH:MM|-HH:MM)
  -- Hour: 00-23, Minute: 00-59, Second: 00-60 (60 for leap seconds)
  -- Timezone offset hour: 00-23, minute: 00-59
  -- Timezone is REQUIRED per RFC 3339
  let timePattern = "^([01][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]|60)(\\.[0-9]+)?([Zz]|[+-]([01][0-9]|2[0-3]):[0-5][0-9])$"
      matches = case Regex.compileText timePattern [] of
                  Left _ -> False
                  Right regex -> Regex.test regex (TE.encodeUtf8 text)
  in if not matches
    then False
    else validateLeapSecond text
  where
    -- Validate leap second rules:
    -- 1. Must be at XX:59:60 (not other minutes)
    -- 2. When converted to UTC, must be at 23:59:60
    validateLeapSecond t =
      if T.isInfixOf ":60" t || T.isInfixOf ":60." t
        then
          -- Parse time format: HH:MM:SS[.sss](Z|z|+HH:MM|-HH:MM)
          -- Extract hour and minute from HH:MM at the start
          case T.splitOn ":" t of
            (hourStr:minStr:_) ->
              case (Read.readMaybe (T.unpack hourStr) :: Maybe Int,
                    Read.readMaybe (T.unpack minStr) :: Maybe Int) of
                (Just hour, Just minute) ->
                  -- Extract timezone and convert to UTC
                  -- The UTC time (not local time) must be 23:59:60
                  let tz = extractTimezone t
                  in case parseOffset tz of
                       Just offsetMinutes ->
                         let totalLocalMinutes = hour * 60 + minute
                             -- Subtract offset to get UTC (if offset is +05:30, UTC is 5.5 hours earlier)
                             totalUtcMinutes = totalLocalMinutes - offsetMinutes
                             -- Normalize to 0-1439 range (0-23:59)
                             normalizedUtcMinutes = ((totalUtcMinutes `mod` 1440) + 1440) `mod` 1440
                             utcHour = normalizedUtcMinutes `div` 60
                             utcMinute = normalizedUtcMinutes `mod` 60
                         in utcHour == 23 && utcMinute == 59
                       Nothing -> False
                _ -> False
            _ -> False
        else True  -- Not a leap second, basic pattern match is sufficient

    -- Extract timezone from time string (Z, z, +HH:MM, or -HH:MM)
    extractTimezone t
      | T.isSuffixOf "Z" t || T.isSuffixOf "z" t = T.takeEnd 1 t
      | T.length t >= 6 =
          -- Look for +HH:MM or -HH:MM at the end
          let lastSix = T.takeEnd 6 t
          in if T.isPrefixOf "+" lastSix || T.isPrefixOf "-" lastSix
            then lastSix
            else ""
      | otherwise = ""

    -- Parse timezone offset into minutes
    parseOffset tz
      | T.toUpper tz == "Z" = Just 0
      | T.length tz == 6 =
          case T.uncons tz of
            Just (sign, rest) ->
              let offsetParts = T.splitOn ":" rest
              in case offsetParts of
                [hourStr, minStr] ->
                  case (Read.readMaybe (T.unpack hourStr) :: Maybe Int,
                        Read.readMaybe (T.unpack minStr) :: Maybe Int) of
                    (Just h, Just m) ->
                      let totalMinutes = h * 60 + m
                      in Just $ if sign == '+' then totalMinutes else -totalMinutes
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
      | otherwise = Nothing

-- ISO 8601 Duration validator
isValidDuration :: Text -> Bool
isValidDuration text =
  -- ISO 8601 duration: P[nY][nM][nW][nD][T[nH][nM][nS]]
  -- Must start with P and have at least one date or time element
  -- Note: Weeks (W) cannot be combined with other date units
  if not (T.isPrefixOf "P" text)
    then False
    else
      let body = T.drop 1 text
      in case T.uncons body of
           Nothing -> False  -- Just "P" with no elements
           Just (c, _) ->
             -- Check for week-only format (e.g., P3W)
             if c >= '0' && c <= '9' && T.isSuffixOf "W" body
               then validateWeekDuration body
             -- Check for date/time format
             else validateDateTimeDuration body
  where
    -- Week-only duration: must be just digits followed by W
    validateWeekDuration t =
      case T.unsnoc t of
        Just (digits, 'W') -> T.all (\c -> c >= '0' && c <= '9') digits && not (T.null digits)
        _ -> False

    -- Date/time duration: [nY][nM][nD][T[nH][nM][nS]]
    -- At least one date or time element must be present
    validateDateTimeDuration t =
      let (datePart, timePart) = T.breakOn "T" t
          hasDateElements = hasDatePart datePart
          hasTimeElements = if T.null timePart
                            then False
                            else hasTimePart (T.drop 1 timePart)
      in case T.null timePart of
           True -> hasDateElements  -- No T, must have date elements
           False -> if T.length timePart == 1
                    then False  -- Just "T" with no time elements
                    else hasDateElements || hasTimeElements  -- Either date or time elements present

    -- Check if date part has valid elements (Y, M, D)
    hasDatePart t =
      let hasY = T.isInfixOf "Y" t
          hasM = T.isInfixOf "M" t
          hasD = T.isInfixOf "D" t
          regexPattern = "^(?:[0-9]+Y)?(?:[0-9]+M)?(?:[0-9]+D)?$"
      in (hasY || hasM || hasD) &&
         case Regex.compileText regexPattern [] of
           Left _ -> False
           Right regex -> Regex.test regex (TE.encodeUtf8 t)

    -- Check if time part has valid elements (H, M, S)
    hasTimePart t =
      let hasH = T.isInfixOf "H" t
          hasM = T.isInfixOf "M" t
          hasS = T.isInfixOf "S" t
          -- Allow decimal seconds
          regexPattern = "^(?:[0-9]+H)?(?:[0-9]+M)?(?:[0-9]+(?:\\.[0-9]+)?S)?$"
      in (hasH || hasM || hasS) &&
         case Regex.compileText regexPattern [] of
           Left _ -> False
           Right regex -> Regex.test regex (TE.encodeUtf8 t)

-- JSON Pointer validator (RFC 6901)
isValidJSONPointer :: Text -> Bool
isValidJSONPointer text =
  -- Either empty string or starts with /
  (T.null text || T.isPrefixOf "/" text) && hasValidEscaping text
  where
    -- Check that all ~ are properly escaped as ~0 or ~1
    hasValidEscaping t =
      let checkEscapes "" = True
          checkEscapes s
            | Just ('~', rest) <- T.uncons s =
                case T.uncons rest of
                  Just ('0', remainder) -> checkEscapes remainder  -- ~0 is valid (represents ~)
                  Just ('1', remainder) -> checkEscapes remainder  -- ~1 is valid (represents /)
                  _ -> False  -- ~ must be followed by 0 or 1
            | otherwise =
                case T.uncons s of
                  Just (_, remainder) -> checkEscapes remainder
                  Nothing -> True
      in checkEscapes t

-- Relative JSON Pointer validator
-- Format: <non-negative-integer>[#|<json-pointer>]
-- Where:
-- - non-negative-integer cannot have leading zeros (except "0" itself)
-- - # gets the member name/index
-- - json-pointer starts with /
isValidRelativeJSONPointer :: Text -> Bool
isValidRelativeJSONPointer text =
  case T.uncons text of
    Just (c, _) | c >= '0' && c <= '9' ->
      -- Starts with a digit, followed by optional # or JSON pointer
      let (digits, remainder) = T.span (\x -> x >= '0' && x <= '9') text
          -- Check for leading zero followed by other digits (invalid)
          hasLeadingZero = T.isPrefixOf "0" digits && T.length digits > 1
      in not (T.null digits) && not hasLeadingZero &&
         (T.null remainder ||  -- Just the number
          remainder == "#" ||  -- Number followed by # (get member name)
          T.isPrefixOf "/" remainder)  -- Number followed by JSON pointer
    _ -> False

-- ECMA-262 Regex validator
-- Validates that the pattern is a valid ECMAScript regex
isValidRegex :: Text -> Bool
isValidRegex text =
  -- First check for invalid escape sequences (like \a)
  -- These are not valid in ECMA-262 but might be accepted by lenient parsers
  if hasInvalidEscapes text
    then False
    else case Regex.compileText text [] of
      Right _ -> True
      Left _ -> False
  where
    -- Check for invalid single-character escapes
    -- Valid escapes: \b \f \n \r \t \v (control chars)
    --               \d \D \w \W \s \S (char classes)
    --               \c[A-Za-z] (control escapes)
    --               \x[0-9A-Fa-f]{2} (hex escapes)
    --               \u[0-9A-Fa-f]{4} (unicode escapes)
    --               \u{...} (unicode code point escapes)
    --               \p{...} \P{...} (unicode property escapes)
    --               \0-9 (backreferences and octal)
    --               Any special regex char: \. \* \+ \? \^ \$ \| \\ \/ \[ \] \{ \} \( \)
    -- Invalid: \a \e \g \h \i \j \k \l \m \o \q \y \z and uppercase variants
    hasInvalidEscapes :: Text -> Bool
    hasInvalidEscapes t = checkEscapes (T.unpack t)
      where
        checkEscapes [] = False
        checkEscapes ('\\':c:rest)
          -- Valid single-character escapes
          | c `elem` ['b', 'f', 'n', 'r', 't', 'v', 'B'] = checkEscapes rest
          -- Valid character class escapes
          | c `elem` ['d', 'D', 'w', 'W', 's', 'S'] = checkEscapes rest
          -- Control escape \cX (where X is A-Z or a-z)
          | c == 'c' = case rest of
              (x:xs) | x >= 'A' && x <= 'Z' || x >= 'a' && x <= 'z' -> checkEscapes xs
              _ -> True  -- Invalid: \c must be followed by letter
          -- Hex escape \xHH
          | c == 'x' = case rest of
              (h1:h2:xs) | isHexDigit h1 && isHexDigit h2 -> checkEscapes xs
              _ -> checkEscapes rest  -- Let compiler handle invalid \x
          -- Unicode escapes \uHHHH or \u{H+}
          | c == 'u' = case rest of
              ('{':xs) -> checkEscapes (dropWhile (/= '}') xs)  -- \u{...}
              (h1:h2:h3:h4:xs) | all isHexDigit [h1,h2,h3,h4] -> checkEscapes xs
              _ -> checkEscapes rest  -- Let compiler handle invalid \u
          -- Unicode property escapes \p{...} \P{...}
          | c `elem` ['p', 'P'] = case rest of
              ('{':xs) -> checkEscapes (dropWhile (/= '}') xs)
              _ -> True  -- Invalid: \p must be followed by {
          -- Octal/backreferences \0-9
          | c >= '0' && c <= '9' = checkEscapes rest
          -- Special regex characters (always valid to escape)
          | c `elem` ['.', '*', '+', '?', '^', '$', '|', '\\', '/', '[', ']', '{', '}', '(', ')', '-', ',', '=', '!', ':', '<', '>'] = checkEscapes rest
          -- Invalid escapes (not in ECMA-262 spec)
          | c `elem` ['a', 'e', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'o', 'q', 'y', 'z',
                      'A', 'C', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'Q', 'R', 'T', 'U', 'V', 'X', 'Y', 'Z'] = True
          | otherwise = checkEscapes rest
        checkEscapes (_:rest) = checkEscapes rest

        isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- URI Template validator (RFC 6570)
-- Uses uri-templater library (>= 1.0.0.1) for proper RFC 6570 compliance
isValidURITemplate :: Text -> Bool
isValidURITemplate text =
  case URITemplate.parseTemplate (T.unpack text) of
    Right _ -> True
    Left _ -> False
