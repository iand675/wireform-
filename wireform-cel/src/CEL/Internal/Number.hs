-- | Decimal-to-binary64 conversion shared by source literals and double(string).
module CEL.Internal.Number (parseDecimalDouble) where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T


-- Check the decimal magnitude before building an exact rational. An
-- attacker-controlled exponent alone must never size an Integer allocation.
parseDecimalDouble :: Text -> Maybe Double
parseDecimalDouble t0 =
  let (negative, t1) = case T.uncons t0 of
        Just ('-', r) -> (True, r)
        Just ('+', r) -> (False, r)
        _ -> (False, t0)
      (intPart, t2) = T.span isDigit t1
      (fracPart, t3) = case T.uncons t2 of
        Just ('.', r) -> T.span isDigit r
        _ -> (T.empty, t2)
      (expVal, t4) = parseExp t3
  in if not (T.null intPart && T.null fracPart) && T.null t4
       then
         let digits = T.dropWhile (== '0') (intPart <> fracPart)
             power = expVal - toInteger (T.length fracPart)
             order = power + toInteger (T.length digits) - 1
             magnitude
               | T.null digits || order < -324 = 0
               | order > 308 = 1 / 0
               | otherwise = roundedMagnitude order digits
         in Just (if negative then negate magnitude else magnitude)
       else Nothing
  where
    parseExp t = case T.uncons t of
      Just (e, r)
        | e == 'e' || e == 'E' ->
            let (s', r') = case T.uncons r of
                  Just ('-', rr) -> (-1, rr)
                  Just ('+', rr) -> (1, rr)
                  _ -> (1, r)
                (ds, r'') = T.span isDigit r'
                significant = T.dropWhile (== '0') ds
                magnitude
                  | T.compareLength significant 19 == GT = exponentLimit
                  | otherwise = min exponentLimit (readIntegerText significant)
            in if T.null ds then (0, t) else (s' * magnitude, r'')
      _ -> (0, t)

    -- The mantissa can offset the exponent by at most the input length.
    exponentLimit = toInteger (T.length t0) + 400


-- Adjacent binary64 values have a rounding midpoint with at most 768
-- significant decimal digits. A sticky digit preserves the rounding
-- decision when the input extends past that prefix.
roundedMagnitude :: Integer -> Text -> Double
roundedMagnitude order digits =
  let (prefix, rest) = T.splitAt 768 digits
      coefficient = readIntegerText prefix
      power = order + 1 - toInteger (T.length prefix)
  in if T.any (/= '0') rest
       then fromRational (fromInteger (coefficient * 10 + 1) * 10 ^^ (power - 1))
       else fromRational (fromInteger coefficient * 10 ^^ power)


readIntegerText :: Text -> Integer
readIntegerText t = T.foldl' (\acc c -> acc * 10 + toInteger (fromEnum c - fromEnum '0')) 0 t
