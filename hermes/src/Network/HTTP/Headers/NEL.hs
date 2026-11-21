{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
@NEL@ (Network Error Logging) supplies a reporting policy for the origin.
The field is a comma-separated sequence of JSON values. The first usable
policy is available through 'nelPolicyDetails'. Use 'nelFromPolicy' to
author a registration or removal. Decoding retains the original spelling
of the combined field, including extension members.

<https://www.w3.org/TR/network-error-logging/#nel-response-header Network Error Logging §4.1>
-}
module Network.HTTP.Headers.NEL (
  NEL (NEL, nelPolicy),
  NELPolicy (..),
  NELRegistration (..),
  nelPolicyDetails,
  nelFromPolicy,
  nelParser,
  renderNEL,
) where

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as JSONTypes
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific (Scientific, base10Exponent, coefficient, isInteger, normalize)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import qualified Data.Vector as V
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hNEL)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (renderSeparated, shortText)
import Wireform.Builder.Internal.Prim (word16HexFixed)
import Wireform.Parser.Driver (parseByteString)


data NEL
  = NELValue !ST.ShortText (Either String NELPolicy)
  | AuthoredNEL !ByteString !NELPolicy


{- | Construct directly from JSON spelling, or extract the spelling that was
received. Use 'nelPolicyDetails' to validate a directly constructed value.
-}
pattern NEL :: ST.ShortText -> NEL
pattern NEL {nelPolicy} <- (nelSpelling -> nelPolicy)
  where
    NEL wire = NELValue wire (decodePolicy (ST.toByteString wire))


{-# COMPLETE NEL #-}


nelSpelling :: NEL -> ST.ShortText
nelSpelling (NELValue wire _) = wire
nelSpelling (AuthoredNEL wire _) = STU.fromByteStringUnsafe wire


instance Eq NEL where
  NEL left == NEL right = left == right


instance Show NEL where
  showsPrec prec (NEL wire) =
    showParen (prec > 10) (showString "NEL {nelPolicy = " . shows wire . showChar '}')


-- | A zero lifetime removes the policy. A positive lifetime registers it.
data NELPolicy
  = NELRemove
  | NELRegister !NELRegistration
  deriving stock (Eq, Show)


data NELRegistration = NELRegistration
  { nelMaxAge :: !Scientific
  -- ^ Integer seconds, represented without expanding a decimal exponent.
  , nelReportTo :: !ST.ShortText
  , nelIncludeSubdomains :: !Bool
  , nelSuccessFraction :: !Scientific
  , nelFailureFraction :: !Scientific
  , nelRequestHeaders :: ![ST.ShortText]
  , nelResponseHeaders :: ![ST.ShortText]
  }
  deriving stock (Eq, Show)


instance KnownHeader NEL where
  type ParseFailure NEL = String
  type Cardinality NEL = 'ZeroOrOne
  type Direction NEL = 'Response


  parseFromHeaders _ (wire :| []) = decodeNEL wire
  parseFromHeaders _ (first :| rest) = decodeNEL (BS.intercalate ", " (first : rest))


  renderToHeaders _ (NELValue wire _) = ST.toByteString wire
  renderToHeaders _ (AuthoredNEL wire _) = wire


  renderToHeadersChecked _ (NELValue wire details) = case details of
    Right _ -> Just (ST.toByteString wire)
    Left _ -> Nothing
  renderToHeadersChecked _ (AuthoredNEL wire _) = Just wire


  headerName _ = hNEL


nelPolicyDetails :: NEL -> Either String NELPolicy
nelPolicyDetails (NELValue _ policy) = policy
nelPolicyDetails (AuthoredNEL _ policy) = Right policy


nelParser :: ParserT st String NEL
nelParser = takeRest >>= either err pure . decodeNEL


decodeNEL :: ByteString -> Either String NEL
decodeNEL wire = do
  policy <- decodePolicy wire
  case ST.fromByteString wire of
    Just compact -> Right (NELValue compact (Right policy))
    Nothing -> Left "NEL field is not valid UTF-8"


decodePolicy :: ByteString -> Either String NELPolicy
decodePolicy wire
  | BS.elem 10 wire || BS.elem 13 wire = Left "Unexpected line ending in NEL field"
  | Left _ <- parseByteString (checkJSONExponents (fromIntegral (jsonExponentLimit wire))) wire =
      Left "Invalid JSON string or numeric exponent in NEL"
  | otherwise = case JSON.eitherDecodeStrict' wire of
      Right item -> JSONTypes.parseEither parsePolicy item
      Left _ -> do
        -- JSON Field Values puts the array delimiters around the combined field.
        items <- JSON.eitherDecode (BL.fromChunks ["[", wire, "]"])
        V.foldr firstValid (Left "No valid NEL policy") items
  where
    firstValid item rest = case JSONTypes.parseEither parsePolicy item of
      Right policy -> Right policy
      Left _ -> rest


jsonExponentLimit :: ByteString -> Int
jsonExponentLimit wire = maxBound - BS.length wire
{-# INLINE jsonExponentLimit #-}


-- Aeson's decimal exponent uses Int. Check its bounds before decoding, with
-- room for subtracting the length of the fractional part. Quoted strings
-- and their escapes are skipped as units.
checkJSONExponents :: Word -> ParserT st String ()
checkJSONExponents limit = skipMany next <* eof
  where
    next =
      branch (skipSatisfyAscii (== '"')) quoted $
        branch (skipSatisfyAscii isExponent) exponentDigits anyWord8_
    quoted = skipMany stringByte *> skipSatisfyAscii (== '"')
    stringByte = branch (skipSatisfyAscii (== '\\')) anyWord8_ $
      withAnyWord8 $
        \byte -> if byte < 0x20 || byte == 0x22 then failed else pure ()
    exponentDigits =
      optional_ (skipSatisfyAscii (\c -> c == '+' || c == '-'))
        *> optional_ (boundedDecimalWord limit (err "JSON numeric exponent overflows Int"))
    isExponent c = c == 'e' || c == 'E'


parsePolicy :: JSON.Value -> JSONTypes.Parser NELPolicy
parsePolicy = JSON.withObject "NEL policy" $ \obj -> do
  age <- obj .: "max_age"
  case compare (coefficient age) 0 of
    EQ -> pure NELRemove
    GT | isInteger age -> NELRegister <$> parseRegistration age obj
    _ -> fail "NEL max_age must be a non-negative integer"


parseRegistration :: Scientific -> JSON.Object -> JSONTypes.Parser NELRegistration
parseRegistration age obj = do
  group <- ST.fromText <$> obj .: "report_to"
  success <- optionalMember obj "success_fraction" 0
  failure <- optionalMember obj "failure_fraction" 1
  if inFractionRange success && inFractionRange failure
    then do
      requests <- map ST.fromText <$> optionalMember obj "request_headers" []
      responses <- map ST.fromText <$> optionalMember obj "response_headers" []
      let subdomains = KeyMap.lookup "include_subdomains" obj == Just (JSON.Bool True)
      pure (NELRegistration age group subdomains success failure requests responses)
    else fail "NEL sampling fractions must be between zero and one"


optionalMember :: JSON.FromJSON a => JSON.Object -> JSON.Key -> a -> JSONTypes.Parser a
optionalMember obj key fallback = case KeyMap.lookup key obj of
  Nothing -> pure fallback
  Just _ -> obj .: key
{-# INLINE optionalMember #-}


inFractionRange :: Scientific -> Bool
inFractionRange value =
  c == 0 || (c > 0 && base10Exponent value <= 0 && value <= 1)
  where
    c = coefficient value


-- | Serialize a policy as one ASCII JSON object, omitting default settings.
nelFromPolicy :: NELPolicy -> Either String NEL
nelFromPolicy NELRemove = Right (AuthoredNEL "{\"max_age\":0}" NELRemove)
nelFromPolicy policy@(NELRegister registration)
  | coefficient age <= 0 || not (isInteger age) = Left "NEL registration requires a positive integer lifetime"
  | not (inFractionRange success && inFractionRange failure) = Left "NEL sampling fractions must be between zero and one"
  | not (exponentFits age && exponentFits success && exponentFits failure) = Left "NEL policy exponent exceeds the wire range"
  | otherwise = Right (AuthoredNEL wire policy)
  where
    wire = M.toStrictByteString members
    limit = jsonExponentLimit wire
    exponentFits value = let e = base10Exponent value in e >= negate limit && e <= limit
    age = nelMaxAge registration
    success = nelSuccessFraction registration
    failure = nelFailureFraction registration
    requests = nelRequestHeaders registration
    responses = nelResponseHeaders registration
    members =
      "{\"report_to\":"
        <> nelString (nelReportTo registration)
        <> ",\"max_age\":"
        <> nelNumber (if base10Exponent age < 0 then normalize age else age)
        <> include (nelIncludeSubdomains registration) ",\"include_subdomains\":true"
        <> include (success /= 0) (",\"success_fraction\":" <> nelNumber success)
        <> include (failure /= 1) (",\"failure_fraction\":" <> nelNumber failure)
        <> include (not (null requests)) (",\"request_headers\":" <> stringList requests)
        <> include (not (null responses)) (",\"response_headers\":" <> stringList responses)
        <> "}"
    include condition value = if condition then value else mempty
    stringList values = "[" <> renderSeparated "," nelString values <> "]"


nelString :: ST.ShortText -> M.Builder
nelString value
  | ST.all (\c -> c >= ' ' && c < '\DEL' && c /= '"' && c /= '\\') value = M.char7 '"' <> shortText value <> M.char7 '"'
  | otherwise = M.byteString (asciiJSON (BL.toStrict (Encoding.encodingToLazyByteString (Encoding.text (ST.toText value)))))


-- Keep large magnitudes compact and an ordinary positive lifetime integral
-- on the wire for user agents that require a plain JSON integer.
nelNumber :: Scientific -> M.Builder
nelNumber value
  | e == 0 = M.integerDec c
  | e > 0 && e <= 18 = M.integerDec (c * 10 ^ e)
  | otherwise = M.integerDec c <> M.char7 'e' <> M.intDec e
  where
    e = base10Exponent value
    c = coefficient value


-- Aeson has already escaped JSON punctuation and controls. Non-ASCII code
-- points in its output can only appear inside strings.
asciiJSON :: ByteString -> ByteString
asciiJSON wire
  | BS.all (< 0x7f) wire = wire
  | otherwise = M.toStrictByteString (T.foldl' write mempty (Text.decodeUtf8 wire))
  where
    write builder c
      | code < 0x7f = builder <> M.char7 c
      | code <= 0xffff = builder <> escape code
      | otherwise =
          let supplementary = code - 0x10000
          in builder <> escape (0xd800 + (supplementary `shiftR` 10)) <> escape (0xdc00 + (supplementary .&. 0x3ff))
      where
        code = ord c
    escape code = "\\u" <> word16HexFixed (fromIntegral code)


renderNEL :: NEL -> M.Builder
renderNEL (NELValue wire _) = shortText wire
renderNEL (AuthoredNEL wire _) = M.byteString wire
