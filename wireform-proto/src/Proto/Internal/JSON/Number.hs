-- | Shared ProtoJSON numeric parsing for runtime helpers and generated codecs.
module Proto.Internal.JSON.Number (numericString, floating, decimalMagnitude, checkJSONNumberExponents) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Scientific (Scientific, scientific, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)


{- | Use JSON's full numeric grammar, including signed e/E exponents, with
complete consumption. The resulting decimal preserves the conversion to any
protobuf integer or floating type.
-}
numericString :: Text -> Aeson.Parser Scientific
numericString = maybe (fail "Invalid numeric string") pure . parse
  where
    parse input = do
      let (negative, unsigned) = case T.stripPrefix "-" input of
            Just rest -> (True, rest)
            Nothing -> (False, input)
          (whole, afterWhole) = T.span isDigit unsigned
      case T.uncons whole of
        Nothing -> Nothing
        Just ('0', rest) | not (T.null rest) -> Nothing
        _ -> do
          (fraction, exponentSuffix) <- case T.stripPrefix "." afterWhole of
            Nothing -> Just (T.empty, afterWhole)
            Just afterDot -> case T.span isDigit afterDot of
              (digits, rest) | not (T.null digits) -> Just (digits, rest)
              _ -> Nothing
          magnitude <- decimalMagnitude whole fraction exponentSuffix
          pure (if negative then negate magnitude else magnitude)


-- | Convert already lexed ASCII mantissa digits and an optional exponent.
-- Orders outside the binary64 range only need a nonzero representative to
-- distinguish overflow, underflow and a nonintegral integer input.
decimalMagnitude :: Text -> Text -> Text -> Maybe Scientific
decimalMagnitude whole fraction suffix = do
  decimalExponent <- exponentWithin limit suffix
  let digits = T.dropWhile (== '0') (whole <> fraction)
      order = decimalExponent - toInteger (T.length fraction) + toInteger (T.length digits) - 1
      magnitude
        | T.null digits = 0
        | order > 308 = scientific 1 309
        | order < -324 = scientific 1 (-325)
        | otherwise = boundedSignificand order digits
  pure magnitude
  where
    limit = toInteger (T.length whole) + toInteger (T.length fraction) + 400


-- A rounding midpoint of adjacent binary64 values has at most 768
-- significant decimal digits (at worst a 54-bit integer times 2^-1075).
-- One extra sticky digit preserves which side of every midpoint the
-- input lies on, and whether it has a fractional part for integer fields.
boundedSignificand :: Integer -> Text -> Scientific
boundedSignificand order digits =
  let (prefix, rest) = T.splitAt 768 digits
      coefficient = T.foldl' decimalStep 0 prefix
      power = fromInteger order + 1 - T.length prefix
  in if T.any (/= '0') rest
       then scientific (coefficient * 10 + 1) (power - 1)
       else scientific coefficient power


exponentWithin :: Integer -> Text -> Maybe Integer
exponentWithin limit suffix = case T.uncons suffix of
  Nothing -> Just 0
  Just (e, afterE) | e == 'e' || e == 'E' ->
    let (sign, digits) = case T.uncons afterE of
          Just ('-', rest) -> (-1, rest)
          Just ('+', rest) -> (1, rest)
          _ -> (1, afterE)
        significant = T.dropWhile (== '0') digits
        -- The limit is bounded by the input's length plus 400.
        magnitude
          | T.compareLength significant 19 == GT = limit
          | otherwise = min limit (T.foldl' decimalStep 0 significant)
    in if T.null digits || not (T.all isDigit digits)
         then Nothing
         else Just (sign * magnitude)
  _ -> Nothing


decimalStep :: Integer -> Char -> Integer
decimalStep n digit = n * 10 + toInteger (fromEnum digit - fromEnum '0')


-- | Guard raw JSON numbers before Aeson's lexer stores their decimal
-- exponent in an Int. Leave JSON syntax validation to Aeson.
checkJSONNumberExponents :: ByteString -> Either String ()
checkJSONNumberExponents input = scan input
  where
    -- Allow for the lexer subtracting the fraction length.
    limit = fromIntegral (maxBound - BS.length input) :: Word64
    scan bytes = case BS.uncons (BS.dropWhile ordinary bytes) of
      Nothing -> Right ()
      Just (34, rest) -> skipString rest
      Just (_, rest) ->
        let afterMantissa = BS.dropWhile (\w -> w == 46 || digit w) rest
        in case BS.uncons afterMantissa of
             Just (e, afterE) | e == 101 || e == 69 -> checkExponent afterE
             _ -> scan afterMantissa

    skipString bytes = case BS.uncons (BS.dropWhile (\w -> w /= 34 && w /= 92) bytes) of
      Nothing -> Right ()
      Just (34, rest) -> scan rest
      Just (_, rest) -> skipString (BS.drop 1 rest)

    checkExponent bytes =
      let afterSign = case BS.uncons bytes of
            Just (w, rest) | w == 43 || w == 45 -> rest
            _ -> bytes
          (digits, rest) = BS.span digit afterSign
          significant = BS.dropWhile (== 48) digits
      in if BS.length significant > 19 || BS.foldl' step 0 significant > limit
           then Left "JSON number exponent out of range"
           else scan rest

    ordinary w = w /= 34 && w /= 45 && not (digit w)
    digit w = w >= 48 && w <= 57
    step n w = n * 10 + fromIntegral (w - 48)


floating :: forall a. RealFloat a => Aeson.Value -> Aeson.Parser a
floating = \case
  Aeson.Number n -> finite n
  Aeson.String "NaN" -> pure (0 / 0)
  Aeson.String "Infinity" -> pure (1 / 0)
  Aeson.String "-Infinity" -> pure (negate (1 / 0))
  Aeson.String t -> do
    value <- numericString t >>= finite
    pure (if value == 0 && T.isPrefixOf "-" t then negate 0 else value)
  _ -> fail "Expected JSON number or numeric string"
  where
    finite n =
      let value = toRealFloat n :: a
      in if isInfinite value
           then fail "float/double overflow"
           else pure value
