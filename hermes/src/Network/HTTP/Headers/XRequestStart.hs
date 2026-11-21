{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | @X-Request-Start@ records the edge-arrival Unix timestamp. Heroku
sends integer milliseconds, Nginx typically sends fractional seconds
with a @t=@ prefix, and New Relic also supports microseconds.

The codec preserves the received bytes and exposes an exact decimal
value. The sender's choice of unit is external to the field syntax.

Spec: <https://docs.newrelic.com/docs/apm/applications-menu/features/configure-request-queue-reporting/>
and <https://devcenter.heroku.com/articles/http-routing>.

See also: "Network.HTTP.Headers.ServerTiming", "Network.HTTP.Headers.XRequestID".
-}
module Network.HTTP.Headers.XRequestStart (
  XRequestStart (XRequestStart, xRequestStartTPrefixed, xRequestStartTimestamp),
  xRequestStartBytes,
  xRequestStartEpoch,
  xRequestStartFromBytes,
  xRequestStartFromEpoch,
  xRequestStartParser,
  renderXRequestStart,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Scientific as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific (FPFormat (Fixed), Scientific, base10Exponent, coefficient, scientific)
import qualified Data.Text.Short as ST
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts (Int (I#), eqAddr#, eqWord#, gtWord#, indexWord8OffAddr#, isTrue#, minusWord#, plusAddr#, plusWord64#, timesWord64#, word8ToWord#, wordToWord64#, (+#), (-#), (==#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.Word (Word64 (W64#))
import Network.HTTP.Grammar.Parser (ParserT, err, isOWS, takeRest)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXRequestStart)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.OpaqueField (opaqueFieldText)


data XRequestStart = XRequestStartWire !Bool !ByteString !TimestampSource


data TimestampSource = LegacyTimestamp | ParsedTimestamp !ByteString !Scientific


-- | Legacy view of the optional prefix and original timestamp spelling.
pattern XRequestStart :: Bool -> ST.ShortText -> XRequestStart
pattern XRequestStart {xRequestStartTPrefixed, xRequestStartTimestamp} <-
  XRequestStartWire xRequestStartTPrefixed (opaqueFieldText -> xRequestStartTimestamp) _
  where
    XRequestStart prefixed text = XRequestStartWire prefixed (ST.toByteString text) LegacyTimestamp


{-# COMPLETE XRequestStart #-}


instance Eq XRequestStart where
  XRequestStartWire p a _ == XRequestStartWire q b _ = p == q && a == b


instance Show XRequestStart where
  showsPrec precedence (XRequestStart prefixed stamp) =
    showParen (precedence > 10) $
      showString "XRequestStart {xRequestStartTPrefixed = "
        . shows prefixed
        . showString ", xRequestStartTimestamp = "
        . shows stamp
        . showChar '}'


-- | Exact source bytes, including received field whitespace.
xRequestStartBytes :: XRequestStart -> ByteString
xRequestStartBytes (XRequestStartWire _ _ (ParsedTimestamp wire _)) = wire
xRequestStartBytes (XRequestStartWire prefixed stamp LegacyTimestamp) =
  if prefixed then "t=" <> stamp else stamp


-- | An exact decimal in the sender's unit.
xRequestStartEpoch :: XRequestStart -> Either String Scientific
xRequestStartEpoch (XRequestStartWire _ _ (ParsedTimestamp _ value)) = Right value
xRequestStartEpoch (XRequestStartWire _ stamp LegacyTimestamp) = parseEpoch stamp


{- | Decode an HTTP field with an optional @t=@ marker and decimal timestamp.
Scientific notation is accepted for floating-point timestamps.
-}
xRequestStartFromBytes :: ByteString -> Either String XRequestStart
xRequestStartFromBytes wire =
  case BS.stripPrefix "t=" stripped of
    Just stamp -> checked True stamp
    Nothing -> checked False stripped
  where
    stripped = BC.dropWhileEnd isOWS (BC.dropWhile isOWS wire)
    checked prefixed stamp = do
      value <- parseEpoch stamp
      pure (XRequestStartWire prefixed stamp (ParsedTimestamp wire value))


-- | Author a timestamp in the caller's chosen unit.
xRequestStartFromEpoch :: Bool -> Scientific -> XRequestStart
xRequestStartFromEpoch prefixed value =
  let stamp = M.toStrictByteString (renderEpoch value)
      wire = if prefixed then "t=" <> stamp else stamp
  in XRequestStartWire prefixed stamp (ParsedTimestamp wire value)


renderEpoch :: Scientific -> M.Builder
renderEpoch value
  | power >= 0 && power <= 18 = M.integerDec (coeff * 10 ^ power)
  | power >= -18 && power < 0 =
      M.byteString (BL.toStrict (BB.toLazyByteString (BB.formatScientificBuilder Fixed Nothing value)))
  | otherwise = M.integerDec coeff <> M.char7 'e' <> M.intDec power
  where
    power = base10Exponent value
    coeff = coefficient value


parseEpoch :: ByteString -> Either String Scientific
parseEpoch raw@(BI.BS fp@(ForeignPtr start _) (I# size)) =
  BI.accursedUnutterablePerformIO $ withForeignPtr fp $ \_ ->
    let end = plusAddr# start size
        finish amount scale = Right $! scientific (toInteger (W64# amount)) (I# scale)
        append amount digit = plusWord64# (timesWord64# amount (wordToWord64# 10##)) (wordToWord64# digit)
        slow = parseGeneralEpoch raw
        -- Eighteen digits fit in the accumulator. Signs, exponents and
        -- larger coefficients use the arbitrary-precision path.
        scanWhole cur amount left
          | isTrue# (eqAddr# cur end) =
              if isTrue# (eqAddr# cur start) then slow else finish amount 0#
          | otherwise =
              let byte = word8ToWord# (indexWord8OffAddr# cur 0#)
                  digit = minusWord# byte 48##
                  next = plusAddr# cur 1#
              in if isTrue# (gtWord# digit 9##)
                   then
                     if isTrue# (eqWord# byte 46##) && not (isTrue# (eqAddr# cur start)) && not (isTrue# (eqAddr# next end))
                       then scanFraction next amount left 0#
                       else slow
                   else if isTrue# (left ==# 0#) then slow else scanWhole next (append amount digit) (left -# 1#)
        scanFraction cur amount left places
          | isTrue# (eqAddr# cur end) = finish amount (0# -# places)
          | otherwise =
              let digit = minusWord# (word8ToWord# (indexWord8OffAddr# cur 0#)) 48##
              in if isTrue# (gtWord# digit 9##) || isTrue# (left ==# 0#)
                   then slow
                   else scanFraction (plusAddr# cur 1#) (append amount digit) (left -# 1#) (places +# 1#)
    in pure $! scanWhole start (wordToWord64# 0##) 18#
{-# NOINLINE parseEpoch #-}


parseGeneralEpoch :: ByteString -> Either String Scientific
parseGeneralEpoch raw = case BC.readInteger digits of
  Just (whole, rest)
    | not (startsWithDigit digits) -> invalid
    | BS.null rest -> Right (makeValue whole 0)
    | BSU.unsafeHead rest == 46 ->
        let fractionBytes = BSU.unsafeDrop 1 rest
        in case BC.readInteger fractionBytes of
             Just (fraction, end)
               | startsWithDigit fractionBytes ->
                   let places = BS.length fractionBytes - BS.length end
                       amount = if whole == 0 then fraction else whole * 10 ^ places + fraction
                   in readScale amount places end
             _ -> invalid
    | otherwise -> readScale whole 0 rest
  _ -> invalid
  where
    negative = not (BS.null raw) && BSU.unsafeHead raw == 45
    digits
      | negative || (not (BS.null raw) && BSU.unsafeHead raw == 43) = BSU.unsafeDrop 1 raw
      | otherwise = raw
    makeValue amount power = scientific (if negative then negate amount else amount) power
    readScale amount places rest
      | BS.null rest = Right (makeValue amount (negate places))
      | BSU.unsafeHead rest == 69 || BSU.unsafeHead rest == 101 =
          case BC.readInteger (BSU.unsafeDrop 1 rest) of
            Just (power, end)
              | BS.null end
              , let scale = power - toInteger places
              , scale >= toInteger (minBound :: Int)
              , scale <= toInteger (maxBound :: Int) ->
                  Right (makeValue amount (fromInteger scale))
            _ -> invalid
      | otherwise = invalid
    invalid = Left "Invalid X-Request-Start timestamp"
{-# NOINLINE parseGeneralEpoch #-}


startsWithDigit :: ByteString -> Bool
startsWithDigit bytes = not (BS.null bytes) && BSU.unsafeHead bytes - 48 <= 9
{-# INLINE startsWithDigit #-}


instance KnownHeader XRequestStart where
  type ParseFailure XRequestStart = String
  type Cardinality XRequestStart = 'ZeroOrOne
  type Direction XRequestStart = 'Request


  parseFromHeaders _ (wire :| []) = xRequestStartFromBytes wire
  parseFromHeaders _ _ = Left "Multiple X-Request-Start field lines"


  renderToHeaders _ = xRequestStartBytes


  renderToHeadersChecked _ (XRequestStartWire _ _ (ParsedTimestamp wire _)) = Just wire
  renderToHeadersChecked _ value =
    case xRequestStartEpoch value of
      Right _ -> Just (xRequestStartBytes value)
      Left _ -> Nothing


  headerName _ = hXRequestStart


xRequestStartParser :: ParserT st String XRequestStart
xRequestStartParser = takeRest >>= either err pure . xRequestStartFromBytes


renderXRequestStart :: XRequestStart -> M.Builder
renderXRequestStart = M.byteString . xRequestStartBytes
