{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
IP address literals as used throughout IETF protocols.

This module provides faithful parsers and canonical renderers for the
three textual address forms an @authority@/@host@ component may carry:

* __IPv4__ — dotted-quad, four decimal octets (RFC 791,
  <https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2 RFC 3986 §3.2.2>).
* __IPv6__ — the full RFC 4291 grammar including @::@ zero-compression and an
  optional IPv4-mapped tail such as @::ffff:192.0.2.1@
  (<https://www.rfc-editor.org/rfc/rfc4291#section-2.2 RFC 4291 §2.2>).
* __IPvFuture__ — the version-tagged escape hatch @vX.payload@
  (<https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2 RFC 3986 §3.2.2>),
  preserved verbatim.

'renderIPv6' emits the canonical textual representation mandated by
<https://www.rfc-editor.org/rfc/rfc5952 RFC 5952>: lower-case hex, no leading
zeros, and the longest run of all-zero hextets (leftmost on ties, length ≥ 2)
collapsed to @::@.  Consequently @renderIPv6 . ipv6Parser@ round-trips on any
RFC 5952 canonical input.
-}
module Network.IPAddress (
  IPv4 (..),
  IPv6 (..),
  IPAddress (..),
  ipv4Parser,
  ipv6Parser,
  ipLiteralParser,
  skipIPv6Parser,
  skipIPLiteralParser,
  ipAddressParser,
  renderIPv4,
  renderIPv6,
  renderIPAddress,
  parseIPv4,
  parseBareIPv4,
  parseIPv6,
  parseBareIPv6,
  parseIPAddress,
) where

import Data.Bifunctor (first)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Char (isAsciiLower, isAsciiUpper)
import qualified Data.Text.Short as ST
import Data.Word (Word16, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts (Addr#, Int (I#), RealWorld, State#, Word (W#), Word#, eqAddr#, gtWord#, indexWord8OffAddr#, isTrue#, minusWord#, or#, plusAddr#, plusWord#, timesWord#, uncheckedShiftL#, uncheckedShiftRL#, word8ToWord#, wordToWord8#, (-#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr))
import GHC.IO (IO (..))
import GHC.Word (Word8 (W8#))
import Network.HTTP.Grammar (WireGrammar (..), grammarParseErrorToString, parseGrammar)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Numeric (showHex)
import Wireform.Parser.Internal (Parser (..), StRes#, pattern Fail#, pattern OK#)


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | An IPv4 address as four octets, most-significant first.
data IPv4 = IPv4 {-# UNPACK #-} !Word8 !Word8 !Word8 !Word8
  deriving stock (Eq, Ord, Show)


-- | An IPv6 address as eight 16-bit hextets, most-significant first.
data IPv6 = IPv6 {-# UNPACK #-} !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
  deriving stock (Eq, Ord, Show)


{- | Any of the textual IP-literal forms permitted by RFC 3986.

The 'IPvFuture' payload retains the raw @vX.payload@ text (including the
leading @v@) so it can be re-emitted exactly as received.
-}
data IPAddress
  = IPv4Address !IPv4
  | IPv6Address !IPv6
  | IPvFuture !ST.ShortText
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- | Parse a dotted-quad IPv4 address: four decimal octets @0-255@ joined by @.@.
ipv4Parser :: ParserT st String IPv4
ipv4Parser = Parser $ \_ end start st ->
  case scanIPv4 start end of
    (# (# packed, stop #) | #) -> (# st, OK# (unpackIPv4 packed) stop #)
    (# | (# #) #) -> (# st, Fail# #)
{-# INLINE ipv4Parser #-}


-- Scan four decimal octets. The returned cursor lets parsers embed
-- the same grammar in a larger value without copying its bytes.
scanIPv4 :: Addr# -> Addr# -> (# (# Word#, Addr# #) | (# #) #)
scanIPv4 start end = octet start 0## 3#
  where
    byteAt cur = word8ToWord# (indexWord8OffAddr# cur 0#)
    digitAt cur = minusWord# (byteAt cur) 48##
    octet cur packed octetsLeft
      | isTrue# (eqAddr# cur end) = (# | (# #) #)
      | otherwise =
          let next = plusAddr# cur 1#
              digit = digitAt cur
          in case digit of
               0##
                 | isTrue# (eqAddr# next end) || isTrue# (gtWord# (digitAt next) 9##) ->
                     finish next packed octetsLeft 0##
                 | otherwise -> (# | (# #) #)
               _
                 | isTrue# (gtWord# digit 9##) -> (# | (# #) #)
                 | otherwise -> moreDigits next packed octetsLeft digit
    moreDigits cur packed octetsLeft octetValue
      | isTrue# (eqAddr# cur end) = finish cur packed octetsLeft octetValue
      | otherwise =
          let digit = digitAt cur
              total = plusWord# (timesWord# octetValue 10##) digit
          in if isTrue# (gtWord# digit 9##)
               then finish cur packed octetsLeft octetValue
               else
                 if isTrue# (gtWord# total 255##)
                   then (# | (# #) #)
                   else moreDigits (plusAddr# cur 1#) packed octetsLeft total
    finish cur packed octetsLeft octetValue =
      let next = or# (uncheckedShiftL# packed 8#) octetValue
      in case octetsLeft of
           0# -> (# (# next, cur #) | #)
           _
             | not (isTrue# (eqAddr# cur end))
             , 46## <- byteAt cur ->
                 octet (plusAddr# cur 1#) next (octetsLeft -# 1#)
             | otherwise -> (# | (# #) #)
{-# INLINE scanIPv4 #-}


unpackIPv4 :: Word# -> IPv4
unpackIPv4 packed =
  IPv4
    (W8# (wordToWord8# (uncheckedShiftRL# packed 24#)))
    (W8# (wordToWord8# (uncheckedShiftRL# packed 16#)))
    (W8# (wordToWord8# (uncheckedShiftRL# packed 8#)))
    (W8# (wordToWord8# packed))
{-# INLINE unpackIPv4 #-}


{- | Parse an IPv6 address per <https://www.rfc-editor.org/rfc/rfc4291#section-2.2 RFC 4291 §2.2>:
hextet groups joined by @:@, at most one @::@ zero-compression marker, and an
optional embedded IPv4 address occupying the final 32 bits.
-}
ipv6Parser :: ParserT st String IPv6
ipv6Parser = Parser $ \_ end start -> scanIPv6Value start end

scanIPv6Value :: Addr# -> Addr# -> State# RealWorld -> StRes# String IPv6
scanIPv6Value start end st =
  scanIPv6 True start end
    (\stop count compression upper lower ->
      (# st, OK# (unpackIPv6 count compression upper lower) stop #))
    st
{-# NOINLINE scanIPv6Value #-}

-- | Validate an IPv6 address without constructing its numeric value.
skipIPv6Parser :: ParserT st String ()
skipIPv6Parser = Parser $ \_ end start st ->
  scanIPv6 False start end
    (\stop _ _ _ _ -> (# st, OK# () stop #))
    st
{-# NOINLINE skipIPv6Parser #-}


{- | Parse any IP-literal form, trying IPv6 first, then IPv4, then IPvFuture.

IPv6 is attempted first because its character set (hex digits, @:@ and @.@) is a
superset of IPv4's; the v6 attempt backtracks cleanly when the bytes are not a
valid v6 address.
-}
ipAddressParser :: ParserT st String IPAddress
ipAddressParser =
  (IPv6Address <$> ipv6Parser)
    <|> (IPv4Address <$> ipv4Parser)
    <|> (IPvFuture <$> ipvFutureParser)


-- | A bracketed URI IP literal. IPv4 addresses use the unbracketed form.
ipLiteralParser :: ParserT st String IPAddress
ipLiteralParser =
  satisfyAscii_ (== '[')
    *> ((IPv6Address <$> ipv6Parser) <|> (IPvFuture <$> ipvFutureParser))
    <* satisfyAscii_ (== ']')
{-# INLINE ipLiteralParser #-}

-- | Validate a bracketed URI literal while retaining its original spelling
-- at the call site.
skipIPLiteralParser :: ParserT st String ()
skipIPLiteralParser =
  satisfyAscii_ (== '[')
    *> (skipIPv6Parser <|> ipvFutureBytes)
    <* satisfyAscii_ (== ']')
{-# INLINE skipIPLiteralParser #-}


-- | @IPvFuture = "v" 1*HEXDIG "." 1*( unreserved \/ sub-delims \/ ":" )@.
ipvFutureParser :: ParserT st e ST.ShortText
ipvFutureParser = shortASCIIFromParser_ ipvFutureBytes

ipvFutureBytes :: ParserT st e ()
ipvFutureBytes = do
  satisfyAscii_ (\c -> c == 'v' || c == 'V')
  skipSome (skipSatisfyAscii isHexDigitChar)
  satisfyAscii_ (== '.')
  skipSome (skipSatisfyAscii isIPvFutureChar)


-- ---------------------------------------------------------------------------
-- IPv6 assembly (pure)
-- ---------------------------------------------------------------------------

-- Scan groups directly in the parser input. The validation caller
-- specializes away the packed values and retains only group and digit counts.
scanIPv6
  :: Bool
  -> Addr#
  -> Addr#
  -> (Addr# -> Int -> Int -> Word64 -> Word64 -> StRes# e a)
  -> State# RealWorld
  -> StRes# e a
scanIPv6 save start end success st
  | doubleColonAt start = afterCompression (plusAddr# start 2#) 0 0 0 0
  | otherwise = group start 0 (-1) 0 0
  where
    peek cur
      | isTrue# (eqAddr# cur end) = 256##
      | otherwise = word8ToWord# (indexWord8OffAddr# cur 0#)

    doubleColonAt cur = case peek cur of
      58## -> case peek (plusAddr# cur 1#) of
        58## -> True
        _ -> False
      _ -> False

    afterCompression cur !count !compression !upper !lower =
      case peek cur of
        46## -> (# st, Fail# #)
        58## -> (# st, Fail# #)
        byte
          | isTrue# (gtWord# (hexNibble byte) 15##) ->
              finish cur count compression upper lower
          | otherwise -> group cur count compression upper lower

    group cur !count !compression !upper !lower
      | count == 8 = (# st, Fail# #)
      | otherwise = digits cur (0 :: Int) (0 :: Word64)
      where
        digits next !width !value = case peek next of
          46## -> mixed cur count compression upper lower
          byte
            | let nibble = hexNibble byte
            , not (isTrue# (gtWord# nibble 15##)) ->
                if width == 4
                  then (# st, Fail# #)
                  else
                    let !nextValue = if save then value * 16 + fromIntegral (W# nibble) else 0
                    in digits (plusAddr# next 1#) (width + 1) nextValue
            | width == 0 -> (# st, Fail# #)
            | otherwise ->
                let !count' = count + 1
                    !upper' = if save && count < 4 then (upper `unsafeShiftL` 16) .|. value else upper
                    !lower' = if save && count >= 4 then (lower `unsafeShiftL` 16) .|. value else lower
                in case byte of
                    58##
                      | doubleColonAt next ->
                          if compression < 0 && count' < 8
                            then afterCompression (plusAddr# next 2#) count' count' upper' lower'
                            else (# st, Fail# #)
                      | otherwise -> group (plusAddr# next 1#) count' compression upper' lower'
                    _ -> finish next count' compression upper' lower'

    -- A dotted quad occupies the last two encoded groups. Reuse the
    -- bounded IPv4 reader from the start of this group.
    mixed cur count compression upper lower
      | count > 6 = (# st, Fail# #)
      | otherwise = case scanIPv4 cur end of
          (# (# packed, stop #) | #) -> case peek stop of
            46## -> (# st, Fail# #)
            58## -> (# st, Fail# #)
            byte | isTrue# (gtWord# (hexNibble byte) 15##) ->
              let value = fromIntegral (W# packed)
                  upper'
                    | not save = upper
                    | count < 3 = (upper `unsafeShiftL` 32) .|. value
                    | count == 3 = (upper `unsafeShiftL` 16) .|. (value `unsafeShiftR` 16)
                    | otherwise = upper
                  lower'
                    | not save = lower
                    | count == 3 = value .&. 0xffff
                    | count >= 4 = (lower `unsafeShiftL` 32) .|. value
                    | otherwise = lower
              in finish stop (count + 2) compression upper' lower'
            _ -> (# st, Fail# #)
          (# | (# #) #) -> (# st, Fail# #)

    finish stop count compression upper lower
      | compression < 0 && count /= 8 = (# st, Fail# #)
      | compression >= 0 && count == 8 = (# st, Fail# #)
      | otherwise = success stop count compression upper lower
{-# INLINE scanIPv6 #-}

-- 16 marks a byte outside HEXDIG.
hexNibble :: Word# -> Word#
hexNibble byte =
  let digit = minusWord# byte 48##
      alpha = minusWord# (or# byte 32##) 97##
  in if not (isTrue# (gtWord# digit 9##))
       then digit
       else if not (isTrue# (gtWord# alpha 5##)) then plusWord# alpha 10## else 16##
{-# INLINE hexNibble #-}

unpackIPv6 :: Int -> Int -> Word64 -> Word64 -> IPv6
unpackIPv6 count compression upper lower =
  if compression < 0
    then IPv6 (packed 0) (packed 1) (packed 2) (packed 3) (packed 4) (packed 5) (packed 6) (packed 7)
    else IPv6 (field 0) (field 1) (field 2) (field 3) (field 4) (field 5) (field 6) (field 7)
  where
    -- Align once after appending the encoded groups to each word.
    !high = if count > 0 && count < 4 then upper `unsafeShiftL` (16 * (4 - count)) else upper
    !low = if count > 4 && count < 8 then lower `unsafeShiftL` (16 * (8 - count)) else lower
    missing = 8 - count
    field position
      | position < compression = packed position
      | position < compression + missing = 0
      | otherwise = packed (position - missing)
    packed position
      | position < 4 = fromIntegral (high `unsafeShiftR` (48 - 16 * position))
      | otherwise = fromIntegral (low `unsafeShiftR` (112 - 16 * position))


-- ---------------------------------------------------------------------------
-- Renderers
-- ---------------------------------------------------------------------------

-- | Render an IPv4 address as dotted-quad decimal.
renderIPv4 :: IPv4 -> M.Builder
renderIPv4 (IPv4 a b c d) =
  d8 a <> dot <> d8 b <> dot <> d8 c <> dot <> d8 d
  where
    d8 w = M.wordDec (fromIntegral w)
    dot = M.char7 '.'


{- | Render an IPv6 address in the canonical form of
<https://www.rfc-editor.org/rfc/rfc5952 RFC 5952>: lower-case hex hextets with
no leading zeros, and the longest run of zero hextets (leftmost on ties, only
when its length is ≥ 2) collapsed to @::@.
-}
renderIPv6 :: IPv6 -> M.Builder
renderIPv6 (IPv6 a b c d e f g h) =
  let hs = [a, b, c, d, e, f, g, h]
      (start, len) = bestZeroRun hs
  in if len >= 2
       then
         joinColon (map hx (take start hs))
           <> M.string8 "::"
           <> joinColon (map hx (drop (start + len) hs))
       else joinColon (map hx hs)
  where
    hx w = M.string8 (showHex w "")
    joinColon = M.intersperse (M.char7 ':')


-- | Render any IP-literal form. 'IPvFuture' is emitted verbatim.
renderIPAddress :: IPAddress -> M.Builder
renderIPAddress (IPv4Address v4) = renderIPv4 v4
renderIPAddress (IPv6Address v6) = renderIPv6 v6
renderIPAddress (IPvFuture t) = M.shortByteString (ST.toShortByteString t)


-- | The all-zero hextet runs of an address, as @(startIndex, length)@ pairs.
zeroRuns :: [Word16] -> [(Int, Int)]
zeroRuns = goRuns 0
  where
    goRuns _ [] = []
    goRuns i ys@(x : xs)
      | x == 0 =
          let (zs, rest) = span (== 0) ys
              l = length zs
          in (i, l) : goRuns (i + l) rest
      | otherwise = goRuns (i + 1) xs


{- | The leftmost longest run of zero hextets, as @(startIndex, length)@.
@length@ is @0@ when there are no zero hextets at all.
-}
bestZeroRun :: [Word16] -> (Int, Int)
bestZeroRun = foldl' better (0, 0) . zeroRuns
  where
    better acc@(_, bl) cur@(_, l) = if l > bl then cur else acc


-- ---------------------------------------------------------------------------
-- WireGrammar instances
-- ---------------------------------------------------------------------------

instance WireGrammar IPv4 where
  type GrammarErr IPv4 = String
  grammarParser = ipv4Parser
  grammarRender = renderIPv4


instance WireGrammar IPv6 where
  type GrammarErr IPv6 = String
  grammarParser = ipv6Parser
  grammarRender = renderIPv6


instance WireGrammar IPAddress where
  type GrammarErr IPAddress = String
  grammarParser = ipAddressParser
  grammarRender = renderIPAddress


-- ---------------------------------------------------------------------------
-- Convenience parsers (full consumption)
-- ---------------------------------------------------------------------------

-- | Parse a complete IPv4 address from a 'ByteString'.
parseIPv4 :: ByteString -> Either String IPv4
parseIPv4 bytes = case parseBareIPv4 bytes of
  Just address -> Right address
  Nothing -> first (grammarParseErrorToString "IPv4 address") (parseGrammar bytes)


-- | Parse a complete dotted quad directly from a bare byte slice.
parseBareIPv4 :: ByteString -> Maybe IPv4
parseBareIPv4 (BSI.BS fp@(ForeignPtr start _) (I# size)) =
  BSI.accursedUnutterablePerformIO $ withForeignPtr fp $ \_ ->
    let end = plusAddr# start size
        result = case scanIPv4 start end of
          (# (# packed, stop #) | #)
            | isTrue# (eqAddr# stop end) -> Just $! unpackIPv4 packed
          _ -> Nothing
    in pure $! result
{-# INLINE parseBareIPv4 #-}


-- | Parse a complete IPv6 address from a 'ByteString'.
parseIPv6 :: ByteString -> Either String IPv6
parseIPv6 wire = withBareIPv6 wire Right $
  first (grammarParseErrorToString "IPv6 address") (parseGrammar wire)

-- | Parse a complete IPv6 literal directly from a bare byte slice.
parseBareIPv6 :: ByteString -> Maybe IPv6
parseBareIPv6 wire = withBareIPv6 wire Just Nothing
{-# INLINE parseBareIPv6 #-}

withBareIPv6 :: ByteString -> (IPv6 -> a) -> a -> a
withBareIPv6 (BSI.BS fp@(ForeignPtr start _) (I# size)) success fallback =
  BSI.accursedUnutterablePerformIO $ withForeignPtr fp $ \_ -> IO $ \st ->
    let end = plusAddr# start size
    in case scanIPv6Value start end st of
      (# st', OK# address stop #) | isTrue# (eqAddr# stop end) ->
        (# st', success address #)
      (# st', _ #) -> (# st', fallback #)
{-# INLINE withBareIPv6 #-}


-- | Parse a complete IP-literal of any form from a 'ByteString'.
parseIPAddress :: ByteString -> Either String IPAddress
parseIPAddress = first (grammarParseErrorToString "IP address") . parseGrammar


-- ---------------------------------------------------------------------------
-- Character classes
-- ---------------------------------------------------------------------------

isHexDigitChar :: Char -> Bool
isHexDigitChar c =
  isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')


-- | @unreserved \/ sub-delims \/ ":"@ — the IPvFuture payload character set.
isIPvFutureChar :: Char -> Bool
isIPvFutureChar c = isUnreservedChar c || isSubDelimChar c || c == ':'


isUnreservedChar :: Char -> Bool
isUnreservedChar c =
  isAsciiUpper c
    || isAsciiLower c
    || isDigit c
    || c == '-'
    || c == '.'
    || c == '_'
    || c == '~'


isSubDelimChar :: Char -> Bool
isSubDelimChar c = c `elem` ("!$&'()*+,;=" :: String)
